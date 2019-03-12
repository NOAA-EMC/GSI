#!/usr/bin/python3
# the first line does not matter, it will be replaced by running "initmpmc"
###################################################
# This python script will generate GSI/EnKF compiling job scripts, 
#     case running job scripts and rocoto flow xml file
#
# by Guoqing Ge, 2018/8/28, guoqing.ge@noaa.gov
#
#
from MPMC_config import MPMC_root, ProdGSI_root, build_root, module_pre, q_directives, \
     cmake_version, build_options, comp_post, hostname, project_acct, queue_name, \
     XML_native, rocoto_exe, rocoto_scheduler, xml_set_nodesize, commitID, branchName
from CASE_config import create_run_scripts, allcases, many_procs
from datetime import datetime
import os, sys

######  parse command line parameters
force=False   #whether to force overwritting to skip user's choice of 1/2/3
submit=False  #whether to submit jobs after scripts were created
newlist=False #whether to write out a list file
rocoto=False  #whether to generat a rocoto xml file
helpme=False  #whether to show the help page
cmdline=''
for i in range(1,len(sys.argv)):
   cmdline=cmdline+' '+sys.argv[i]
if cmdline.find("force")>=0:    force=True
if cmdline.find("submit")>=0:   submit=True
if cmdline.find("newlist")>=0:  newlist=True
if cmdline.find("rocoto")>=0:   rocoto=True
if cmdline.find("help")>=0:     helpme=True
if cmdline=='': helpme=True

###### show the help page
if (helpme):
  print('\n\
Welcome to the DTC MPMC Test suite !\n\
(https://dtcenter.org/com-GSI/MPMC)\n\
The following is a brief description on how to use generate.py, report.py, run.py:\n\
\n\
generate.py           ----- show this help\n\
\n\
generate.py mpmc      ----- generate compiling and running scripts\n\
generate.py rotoco    ----- generate compiling, running scripts, and rocoto xml files\n\
generate.py submit    ----- generate compiling and running scripts, submit compiling jobs\n\
generate.py rocoto submit  -generate job scripts and rotoco xml files\n\
                           -also submit all jobs with rotoco workflow management\n\
  add "force" in the command line to force overwritting existed directories and files\n\
  add "newlist" in the command line to generate a new list file each time\n\
\n\
reprot.py list.all    ----- report both running and compiling results\n\
reprot.py list.all 1  ----- report compiling results\n\
reprot.py list.all 2  ----- report running results\n\
\n\
### if you cannot use rotoco, run.py can submit running jobs alternatively###\n\
run.py list.all                ----- generate running scripts\n\
run.py list.all submit         ----- generate running scripts and sumit them\n\
run.py list.all submit case#   ----- only submit given cases, such as 3,4,5,7\n\
run.py list.all submit enkf    ----- submit case22, 24, 26 if case21,23,25 completed\n\
### NO NEED to do run.py if you use rocoto  ######\n\
\n\
For further questions, contact Guoqing Ge at guoqing.ge@noaa.gov.\n\
    ')
  exit()

### show the build_root to be created
relPath=os.popen("basename "+build_root).read()
print('The MPMC test will be conducted under the following direcotry(b_branchName_commitID or build):\n\n   '+relPath)
choice=input("Input y to continue, n to exit >>>")
if choice=='n' or choice=='N':
  exit()

### check whether build_root exists, if yes, let user choose whether to do
if os.path.isdir(build_root) and not force:
  print('\nbuild_root directory "'+build_root+'" already exists\n')
  mtime=datetime.fromtimestamp(os.path.getmtime(build_root)).strftime("%Y%m%d_%H:%M:%S")
  print("Please choose: 1. overwrite it")
  print('               2. rename it to "'+ build_root+'_'+mtime+'"')
  print("               3. exit the program")
  choice=input("1, 2 or 3 ? >>>")
  if choice=='2' or choice=='2':
    os.rename(build_root, build_root+"_"+mtime)
    os.system("mkdir -p "+build_root)
    print('old directory is renamed to: '+build_root+"_"+mtime)
    print('Now working on '+build_root)
  elif choice=='1' or choice=='1':
    print('overwrite '+build_root)
  else:
    print('\nPlease remove or rename the old build_root "'+build_root+'"\n')
    exit()
else:
  os.system("mkdir -p "+build_root)

###write branchName and commitID to build_root
file1=open(build_root+'/branch_commit.txt', 'w')
file1.write(branchName+'\n')
file1.write(commitID)
file1.close()

##### Genereate compiling and running scripts ------------------------
flist_name="list."+datetime.now().strftime("%Y%m%d_%H:%M:%S")
flist=open("list.all","w")
sBuild=''; sCompiler=''; sMPI=''
for x in build_options:
  if not submit: print('.',end='', flush=True)  #if submit, don't need to print out status dot
  modlist=x.split(',')
  build_ID=modlist[0].strip() #the first word is build ID
  compiler=modlist[1].strip().replace('/','_') #the second word is the compiler
  mpi     =modlist[2].strip().replace('/','_') #the third  word is the MPI library
  sBuild=sBuild+build_ID+" "
  sCompiler=sCompiler+compiler+" "
  sMPI=sMPI+mpi+" "

  k=len(modlist)
  cc_name=modlist[k-2].strip()  #the second last word is the CC name
  cxx_name=modlist[k-1].strip() #the last word is the CXX name

  modules=module_pre
  for i in range(1,k-2):
    word=modlist[i].strip()
    if word.find("intel")>=0 or word.find("pgi")>=0 or word.find("gnu")>=0:
      word=word+" "+comp_post
    if word.find("netcdf")>=0 and hostname.startswith("Jet"):
      word="szip hdf5 "+word
    modules=modules+"module load "+word+"\n"

  if cmake_version:
    modules=modules+"module load "+cmake_version+"\n"

  bld_fullname=build_ID+'.'+compiler+'.'+mpi
  q_ready="#!/bin/sh\n#PBS -N "+bld_fullname+"\n"+q_directives
  q_ready=q_ready+"#PBS -o output."+build_ID+"\n"

  rmfiles1="CMakeCache.txt CMakeFiles Makefile DartConfiguration.tcl src done.compiling"
  rmfiles2="include lib libsrc util Testing regression_var.out cmake_install.cmake CTestTestfile.cmake"
  cmake1="cmake -DENKF_MODE=GFS -DBUILD_CORELIBS=ON -DCMAKE_C_COMPILER="+cc_name+" -DCMAKE_CXX_COMPILER="+cxx_name
  cmake2="cmake -DENKF_MODE=WRF -DBUILD_GSDCLOUD_ARW=ON -DBUILD_UTIL_COM=ON -DBUILD_CORELIBS=ON -DCMAKE_C_COMPILER="+cc_name+" -DCMAKE_CXX_COMPILER="+cxx_name

  job_script=q_ready+'\n'+modules+'\n'
  job_script=job_script+"cd "+build_root+"/"+bld_fullname+'\n\n'
  #### to build enkf_gfs.x, gsi.x
  job_script=job_script+"rm -rf "+rmfiles1+' '+rmfiles2+" bin\n"  #to get a clean start
  job_script=job_script+cmake1+" "+ProdGSI_root+"\n"
  job_script=job_script+"make -j8\n"
  job_script=job_script+"make -j2\n\n"  ### some build options require to do this to get enkf executables
  #### to build enkf_arw.x, gsi.x and all community utilities
  #job_script=job_script+"rm -rf CMakeCache.txt CMakeFiles\n"  # don't remove bin/ directory at this step
  job_script=job_script+"rm -rf "+rmfiles1+' '+rmfiles2+"\n"  # don't remove bin/ directory at this step
  job_script=job_script+cmake2+" "+ProdGSI_root+"\n"
  job_script=job_script+"make -j8\n"
  job_script=job_script+"make -j2\n\n"  ### some build options require to do this to get enkf executables
  ### link executables to get ready for case running test
  job_script=job_script+"ln -sf ../bin/gsi.x run/gsi.x\n" 
  job_script=job_script+"ln -sf ../bin/enkf_wrf.x run/enkf_wrf.x\n" 
  job_script=job_script+"ln -sf ../bin/enkf_gfs.x run/enkf_gfs.x\n\n"  #get ready for case running test
  job_script=job_script+"touch done.compiling #Notify rocoto that compiling was done\n"

  #### create the directory and write out the compiling job script
  os.system("mkdir -p "+build_root+"/"+bld_fullname)
  os.system("mkdir -p "+build_root+"/"+bld_fullname+"/run") ## for case running
  jobfile=build_root+"/"+bld_fullname+"/compile.sh"
  fjob=open(jobfile,"w")
  fjob.write(job_script)
  fjob.close()
  os.system("chmod +x "+jobfile)
  if submit:
    os.system("cd "+build_root+"/"+bld_fullname+"; qsub "+jobfile)
    os.system("rm -f "+build_root+"/"+bld_fullname+"/done.compiling")

  #### create the case running job scripts
  create_run_scripts(build_ID, modules, build_root+"/"+bld_fullname+"/run")

  #### write the build directory to a list file
  flist.write(build_root+"/"+bld_fullname+"\n")
#
#---------- End of loop through build_options ---------|

###################### generate the list file --------------
### this list file records every build directory
flist.close()
os.system("/bin/cp list.all "+build_root)  #save a copy in the build_root
if newlist:
  os.system("/bin/cp list.all "+flist_name)
if submit:
  print("\n\nJob scripts created and compiling jobs submitted!\n")
  if newlist: print("The list file is: "+flist_name+"\n\n")
else:
  print("\n\nJob scripts created! \n")
  if newlist: print("The list file is: "+flist_name+"\n\n")

#####################generate rocoto xml file --------------
if rocoto:
  GSIcases='01,02,03,04,05,07,09,10,11,21,23,25' #gge.debug
  caselist=GSIcases.split(',')
  sCase=''; sNum=''
  for x in caselist:
    for i in range(len(allcases)):
      if allcases[i].find(x) >=0:
        sCase=sCase+allcases[i]+' '
        sNum=sNum+"case"+x+' '

  os.system("rm -f mpmc.db mpmc_lock.db")
  file1=open('.rocoto.template', "r")
  frocoto=open('mpmc.xml', 'w')
  while True:
    line=file1.readline()
    if not line:
      break
    if line.startswith('<!ENTITY ACCOUNT '):
      frocoto.write('<!ENTITY ACCOUNT "'+project_acct+'">\n')
    elif line.startswith('<!ENTITY QUEUE '):
      frocoto.write('<!ENTITY QUEUE "'+queue_name+'">\n')
    elif line.startswith('<!ENTITY SCHEDULER '):
      frocoto.write('<!ENTITY SCHEDULER "'+rocoto_scheduler+'">\n')
    elif line.startswith('<!ENTITY BUILD_ROOT '):
      frocoto.write('<!ENTITY BUILD_ROOT "'+build_root+'">\n')
    elif line.startswith('<!ENTITY MPMC_ROOT '):
      frocoto.write('<!ENTITY MPMC_ROOT "'+MPMC_root+'">\n')
    elif line.startswith('<!ENTITY NATIVE '):
      frocoto.write('<!ENTITY NATIVE "'+XML_native+'">\n')
#   elif line.startswith(''):
#     frocoto.write(+'">\n')
    elif line.startswith("<!ENTITY build_ID "):
      frocoto.write('<!ENTITY build_ID "'+sBuild+'">\n')
    elif line.startswith('<!ENTITY compiler '):
      frocoto.write('<!ENTITY compiler "'+sCompiler+'">\n') 
    elif line.startswith('<!ENTITY mpi '):
      frocoto.write('<!ENTITY mpi "'+sMPI+'">\n') 
    elif line.startswith('<!ENTITY case_ID '):
      frocoto.write('<!ENTITY case_ID "'+sCase+'">\n') 
    elif line.startswith('<!ENTITY casenum '):
      frocoto.write('<!ENTITY casenum "'+sNum+'">\n') 
    elif line.startswith('<!ENTITY ENKF_PROC '):
      frocoto.write('<!ENTITY ENKF_PROC "'+many_procs+'">\n') 
    elif line.find('<nodesize>')>=0:
      if xml_set_nodesize: frocoto.write(line)
    else:
      frocoto.write(line)
  file1.close(); frocoto.close()

  file1=open("myrocoto.ksh","w")
  file1.write("#!/bin/ksh\n")
  file1.write("dir="+MPMC_root+"\n")
  file1.write(rocoto_exe+" -w ${dir}/mpmc.xml -d ${dir}/mpmc.db\n")
  file1.close()
  os.system("chmod +x myrocoto.ksh")

  ### add an entry to crontab to run myrocoto.ksh every 5 minutes
  if submit:
    mpmcCron="*/5 * * * * "+MPMC_root+"/myrocoto.ksh"
    crontab=os.popen("crontab -l").read()
    if crontab.find(mpmcCron)<0:  ## the MPMC cronjob is not in crontab
      os.system('(crontab -l;echo "'+mpmcCron+'") | crontab -')

  print('Rocoto workflow files " mpmc.xml and myrocoto.ksh " generated\n')
  if submit: print('An entry added into the crontab to run myrocoto.ksh every 5 minutes.\n')
  print('')


