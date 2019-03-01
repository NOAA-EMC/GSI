#!/usr/bin/python3
# the first line does not matter, it will be replaced by running "initmpmc"
###################################################
# This python script will generate job scripts for MPMC test suite cases
# and be able to submit jobs automatically.
#
# by Guoqing Ge, 2018/8/18, guoqing.ge@noaa.gov
#
# There are two ways to run this script:
#
#   1. Generate/submit jobs for a given directory:
#        run.py  <GSI_build_directory>  [option]   
#
#         *** only need to provide the realtive path name, such as "ti04.intel_17.0.5.239_impi_5.1.2.150"
#         *** run.py know where to find this directory by checking your MPMC_config.py file
#
#   2. Generate/submit jobs for all build directories
#        run.py <build_directory_list_file>  [option]
#
#         *** when you do GSI/EnKF MPMC building, a file named "list.all" is generated
#             you can use this "list.all" file,
#             or you can mannually make a list file by putting relative or absolute paths
#                 of each build directory you want to do runnning test
#         *** run.py will loop through the list file to submit jobs for each build directory until MAXJOBS reached.
#         *** It will write out three files: dir.done, dir.notready, dir.todo
#         *** Where dir.done lists directories whose jobs have been submitted
#         ***   dir.notready lists those directories who failed to compile and hence not ready for case runs
#         ***       dir.todo lists those directories who have not get a chance to run cases due to MAXJOBS exceeded
#                       you can use dir.todo as the build_directory_list_file to run tests for left-over build directories
#
#   BY DEFAULT, run.py will do dry run and only generate job scripts at the run/ subdirecotry under the build direcotry
#               ** This will avoid accidentally submitting too many jobs **
#   When you are sure all is ready, use "submit" as the 2nd command line parameter:
#   e.g:    
#     run.py  intel_17.0.5.239_impi_5.1.2.150_ti04  submit
#
#   3. advanced running of run.py (to submit only a few test cases):
#         run.py <direcotry|list_file> submit case_numbers_comma_seperated
#         e.g:  run.py intel.txt submit 3,4,5,7       #only run case03,case04,case05,case07
#
###################################################
#
MAXJOBS=300   ### maximum jobs allowed at the same time
import os, sys
from MPMC_config import ProdGSI_root, build_root, project_acct, queue_name, hostname, serial_run
from datetime import datetime
from CASE_config import PBS_extras, MPI_CMD_ORG, crtm_dir, data_root, myARCH, few_cpu_res, \
  many_cpu_res, allcases, cases, generateAjob

#############  This this beginning of the program   ############################
comgsi_dir=ProdGSI_root

### parse the command line parameters
arguments = len(sys.argv) - 1
if (arguments >=1):
  cmdparam=sys.argv[1]
else:
  print("Usage: run.py  <one_build_directory | list_file_name>  [submit] [case_numbers[,]]")
  exit()

dry_run=True #by default, it is dry run so as to avoid submitting too many jobs  accidentally
if (arguments >=2):
  if (sys.argv[2] == "submit" ):
    dry_run=False

#determine what cases to run
if (arguments >=3 and sys.argv[3]=='enkf'):
  caselist='22,24,26'.split(',')
elif (arguments >=3 and sys.argv[3]!='all'):
  caselist=sys.argv[3].split(',')
else:
  fullSTEP1='1,2,3,4,5,7,9,10,11,21,23,25'
  caselist=fullSTEP1.split(',')

if dry_run:
  fullLIST='1,2,3,4,5,7,9,10,11,21,22,23,24,25,26'
  caselist=fullLIST.split(',')

mycases=[]
for x in caselist:
  try:
    k=int(x)
  except ValueError:
    print("'"+x+"' is not a correct case number")
  else:
    found=False
    for i in range(len(allcases)):
      if allcases[i].find("case{0:02d}".format(k)) >=0:
        if not dry_run: print("will run "+ allcases[i])
        mycases.append(allcases[i])
        found=True
    if not found:
      if not dry_run: print("case # "+str(k)+" is not prepared yet")
if (len(mycases)<1):
  print("No valid case number provided, exit")
  exit()

#if dry_run:
#  mycases=allcases

fullpath=build_root +"/"+cmdparam
cwd = os.getcwd()
if (os.path.isdir(fullpath)):
  print("You want to run MPMC cases for the given directory: "+fullpath)
  dir_list=[fullpath]
elif (os.path.isfile(cwd+"/"+cmdparam)):
  print("You want to run MPMC cases for all directories listed in the file: "+fullpath)
  with open(cwd+"/"+cmdparam) as f:
    dir_list = f.readlines()
  if(len(dir_list)<=0): print("\nThe list file is empty, exit\n"); exit()
  dir_list = [x.strip() for x in dir_list]  #strip "\n"
  if (dir_list[0][0]!= "/"):  # if not absolute path, add the prefix based on "build_root"
    for i in range(len(dir_list)):
      dir_list[i]=build_root +"/"+dir_list[i]
else:
  print("You don't run me correctly!")
  print(cmdparam+" does not exist under "+build_root+"\n"+cmdparam+" is not a list file\n")
  print("Usage: run.py  <one_build_directory | list_file_name>  [submit] [case_numbers[,]]")
  exit()

tot=len(dir_list)
if tot>1:
  print("There are "+str(tot)+" directories in total")

os.system("mkdir -p dir.tmp")
if (tot > 1 and not dry_run): #only need to write out dir.* files when working on more then 1 directory
  if os.path.isfile("dir.tmp/dir.todo"):
    os.rename("dir.tmp/dir.todo", "dir.tmp/dir.todo_"+datetime.now().strftime("%Y%m%d_%H%M%S"))
  if os.path.isfile("dir.good"):
    os.rename("dir.tmp/dir.good", "dir.tmp/dir.good_"+datetime.now().strftime("%Y%m%d_%H%M%S"))
  if os.path.isfile("dir.notready"):
    os.rename("dir.tmp/dir.notready", "dir.tmp/dir.notready_"+datetime.now().strftime("%Y%m%d_%H%M%S"))
  fgood=open("dir.tmp/dir.done", "w") #List those directories where case run jobs have been submitted
  fbad=open("dir.tmp/dir.notready","w") #List those directories who failed to compile and hence not ready for case runs
  ftodo=open("dir.tmp/dir.todo","w") #List those directories who have not get a chance to run cases due to MAXJOBS exceeded

jobknt=0
buildknt=0
for mybuild in dir_list:
  buildknt=buildknt+1
  if (jobknt > MAXJOBS and not dry_run ):
    print("Maxium number of jobs ("+str(MAXJOBS)+") reached")
    break

  print("\nNow working on "+mybuild)
  tem=mybuild.rstrip('/')
  kk=tem.rfind('/')
  build_ID=mybuild[kk+1:kk+5]

  ### test whether gsi and enkf executables are available
  if ("22" in caselist) or ("24" in caselist) or "26" in caselist: #enkf step
    if os.path.isfile(mybuild+"/bin/enkf_wrf.x") and os.path.isfile(mybuild+"/bin/enkf_gfs.x"):
      print("enkf_wrf.x and enkf_gfs.x are available\n",end='')
    else:
      print("at leaset one of enkf_wrf.x and enkf_gfs.x is not available. Exit this directory\n")
      continue
  else: #GSI step
    if os.path.isfile(mybuild+"/bin/gsi.x"):
      print("gsi.x is avaialbe")
    else: 
      print("gsi.x is not available. Exit this directory")
      if(tot>1 and not dry_run): fbad.write(mybuild+"\n")
      continue  #### goto next directory

  ### read module informatin from compile.sh
  modules=""
  file1=open(mybuild+"/compile.sh", "r")
  while True:
    line=file1.readline()
    if not line:
      break
    if line.startswith("source"):
      modules=modules+line+"module purge\n"
    if line.startswith("module"):
      modules=modules+line
  file1.close()
  if modules.find("module load mpt") >=0:
    MPI_CMD='      RUN_COMMAND="mpiexec_mpt dplace -s 1  " ;;\n'
  else:
    MPI_CMD=MPI_CMD_ORG

  job_dir=mybuild+"/run"
  if os.path.isdir(mybuild):
    if not os.path.isdir(job_dir):
     os.system("mkdir "+job_dir)
  else:
     print("Directory "+job_dir+" does not exist")

  for x in mycases:
    if (x.find("wrfchem")>=0 ) or (x.find("cmaq")>=0):
      srcfname=comgsi_dir+"/ush/comgsi_run_chem.ksh"
    elif (x.find("enkf")>=0 and x.find("gfs")>=0):
      srcfname=comgsi_dir+"/ush/comenkf_run_gfs.ksh"
    elif (x.find("enkf")>=0): #enkf regional
      srcfname=comgsi_dir+"/ush/comenkf_run_regional.ksh"
    elif (x.find("gfs")>=0):
      srcfname=comgsi_dir+"/ush/comgsi_run_gfs.ksh"
    else:
      srcfname=comgsi_dir+"/ush/comgsi_run_regional.ksh"

    jobfname=generateAjob(x,srcfname, build_ID,modules,job_dir, MPI_CMD)
    jobknt=jobknt+1
    if not dry_run:
      if serial_run: #run jobs one by one
        os.system("cd "+job_dir+"; "+jobfname+">out."+x)
        print("Job # "+ str(jobknt) +" : "+jobfname + " Done.")
      else: #paralle run, submit jobs to computing nodes
        os.system("cd "+job_dir+"; qsub "+jobfname)
        print("Job # "+ str(jobknt) +" : "+jobfname + " submitted.")
    else:
      print("It's a dry run. Job script # "+str(jobknt)+" : "+jobfname+" generated.")

  if (tot>1 and not dry_run): fgood.write(mybuild+"\n")
  
if(tot>1 and not dry_run):
  for mybuild in dir_list[buildknt-1:]:
    ftodo.write(mybuild+"\n")
  fgood.close()
  fbad.close()
  ftodo.close()

