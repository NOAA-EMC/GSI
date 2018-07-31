#!/usr/bin/python3
# the first line does not matter, it will be replaced by running "python initMPMC.py"
###################################################
# This python script will generate case running job scripts for most tutorial cases
# and be able to submit jobs automatically when input "submit" as the second command line parameter
#
# by Guoqing Ge, 2018/8/18, guoqing.ge@noaa.gov
#
# There are two ways to run this script:
#
#   1. Generate/submit jobs for a given directory:
#        run.py  <GSI_build_directory_name>  [option]   
#
#         *** only need to provide the realtive path name, such as "intel_17.0.5.239_impi_5.1.2.150_ti04"
#         *** run.py know where to find this directory by checking your pyrunconfig.py file
#
#   2. Generate/submit jobs for all build directories
#        run.py <build_directory_list_file_name>  [option]
#
#         *** when you do GSI/EnKF MPMC building, a file named test_directory_list_yymmdd_hhmmss
#              is generated which lists the absolute path of each build directory, you can use this list file,
#              or you can mannually make a list file by putting relative or absolute paths of each build 
#              directory you want to test
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
import socket
hostname=socket.gethostname()
PBS_extras=""
MPI_CMD_ORG='      RUN_COMMAND="mpirun -np ${GSIPROC} " ;;\n'
if hostname.startswith("tfe"):  ## THEIA
  print("It's on Theia.")
  crtm_dir="/scratch4/BMC/comgsi/case_data/CRTM_2.2.3"
  data_root="/scratch4/BMC/comgsi/case_data"
  myARCH="  ARCH='LINUX_PBS'\n" 
  few_cpu_res="procs=4"; few_procs="4" 
  many_cpu_res="procs=24"; many_procs="24"  #24 cores/node

elif hostname.startswith("fe"): ## Jet
  print("It's on Jet.")
  crtm_dir="/lfs3/projects/wrfruc/gge/MPMC/case_data/CRTM_2.2.3"
  data_root="/lfs3/projects/wrfruc/gge/MPMC/case_data"
  myARCH="  ARCH='LINUX_PBS'\n" 
  few_cpu_res="procs=4"; few_procs="4" 
  #PBS_extras="#PBS -l partition=tjet\n" #12 cores/node
  PBS_extras="#PBS -l partition=xjet\n" #24 cores/node
  many_cpu_res="procs=24"; many_procs="24" 

elif hostname.startswith("cheyenne")>=0:  ## Cheyenne
  print("It's on Cheyenne.")
  crtm_dir="/glade/p/ral/jntp/DAtask/case_data/CRTM_2.2.3"
  data_root="/glade/p/ral/jntp/DAtask/case_data"
  myARCH="  ARCH='LINUX_PBS'\n" 
  few_cpu_res="select=1:ncpus=4:mpiprocs=4"; few_procs="4" 
  many_cpu_res="select=1:ncpus=36:mpiprocs=36"; many_procs="36"  #Dual-socket nodes, 36 cores per node, 20 ensembles, need at least 20cores
else:
  print("I'm new to Host: "+hostname+". Please set up crtm_dir, data_root and myARCH in run.py")
  exit()

#####################  generate a job script for a case #########
def generatejob(case_name, srcfname, build_ID):
  global jobknt
  if case_name.find('enkf')>=0:
    cpu_res=many_cpu_res
  else:
    cpu_res=few_cpu_res
  jobres="### in PBS, cannnot put comments after job decription commands\n" \
  +"#PBS -A "+project_code+"\n" \
  +"#PBS -l walltime=00:20:00\n" \
  +"#PBS -N "+build_ID+"-"+case_name+"\n" \
  +"#PBS -l "+cpu_res+"\n" \
  +"#PBS -q "+queue_name+"\n" \
  +PBS_extras \
  +"#PBS -o out."+case_name+"\n" \
  +"#PBS -j oe\n\n" \
  +modules+"\n\n" \
  +"set -x\n"

  common={"set -x"                   :jobres \
      ,'      RUN_COMMAND="mpirun -np ${GSIPROC} "'  :MPI_CMD \
      ,"  ARCH="                     :myARCH \
      ,"cp ${GSI_EXE} gsi.x"         :"ln -sf ${GSI_EXE} gsi.x\n"  #link to gsi to avoid unneccessary duplicates
      ,"cp $ENKF_EXE enkf.x"         :"ln -sf ${ENKF_EXE} enkf.x\n"  #link to enkf to avoid unneccessary duplicates
      ,"  JOB_DIR="                  :"  JOB_DIR="+job_dir+"\n"   \
      ,"  RUN_NAME="                 :"  RUN_NAME="+case_name+"\n" \
      ,"  GSI_ROOT="                 :"  GSI_ROOT="+comgsi_dir+"\n" \
      ,"  CRTM_ROOT="                :"  CRTM_ROOT="+crtm_dir+"\n"   \
      }

  jobfname=job_dir+'/job.'+case_name
  jobfile = open(jobfname, "w")
  mydict=cases[case_name]

  file1=open(srcfname, "r")
  while True:
    line=file1.readline()
    if not line:
      break
    found=False
    for key in common.keys():
      if line.startswith(key):
        jobfile.write(common[key])
        found=True
        break
    for key in mydict.keys():
      if line.startswith(key):
        jobfile.write(mydict[key])
        found=True
        break
    if not found:
      jobfile.write(line)

  jobfile.close()
  file1.close()
  import subprocess 
  jobknt=jobknt+1
  os.system("chmod +x "+jobfname)
  if not dry_run:
    subprocess.call("cd " + job_dir + "; qsub " + jobfname,shell=True)
    print("Job # "+ str(jobknt) +" : "+jobfname + " submitted.")
  else:
    print("It's a dry run. Job script # "+str(jobknt)+" : "+jobfname+" generated.")

  return jobfname
#-----------------------End of generatejob(...)---------------

########################################################
######## variables to be used
hybrid_yes="  if_hybrid=Yes    # Yes, or, No -- case sensitive !\n"
EnVar4D_yes="  if_4DEnVar=Yes   # Yes, or, No -- case sensitive (set if_hybrid=Yes first)!\n"
linkallobs ="       ln -s ${srcobsfile[$ii]}  ${gsiobsfile[$ii]}\n"
#----------------------
#**** substitutions case by case based on comgsi_run_regional.ksh
#**** default comgsi_run_regional.ksh in the release is for case03-conv ****
jobproc_few="  GSIPROC="+few_procs+"\n"
jobproc_many="  GSIPROC="+many_procs+"\n"

com_obs="  OBS_ROOT="+data_root+"/2017051318/obs\n"
com_bkg="  BK_ROOT="+data_root+"/2017051318/bkg\n"
com_ens="  ENS_ROOT="+data_root+"/2017051318/gfsens\n"
#
nmmb_obs="  OBS_ROOT="+data_root+"/nmmb_2012062812/obs\n"
nmmb_bkg="  BK_ROOT="+data_root+"/nmmb_2012062812/bkg\n"
nmmb_ens="  ENS_ROOT="+data_root+"/nmmb_2012062812/gfsens\n"
#
chem_obs="  OBS_ROOT="+data_root+"/chemdata/obs\n"
chem_bkg="  BK_ROOT="+data_root+"/chemdata/bkg\n"
#
gfs_obs="  OBS_ROOT="+data_root+"/T62.gfs/obs\n"
gfs_bkg="  BK_ROOT="+data_root+"/T62.gfs/bkg\n"
#
case2124_obs="  OBS_ROOT="+data_root+"/enkf_arw_2014021300/obs\n"
case2124_bkg="  BK_ROOT="+data_root+"/enkf_arw_2014021300/bkg\n"
#
case2526_obs="  OBS_ROOT="+data_root+"/enkf_glb_T62/obs\n"
case2526_bkg="  BK_ROOT="+data_root+"/enkf_glb_T62/bkg\n"
#
cases={ \
     "case01-oneobs-glb":{"  GSI_NAMELIST=":"  GSI_NAMELIST=${GSI_ROOT}/scripts/comgsi_namelist_oneobs.sh\n", \
             "ln -s ${PREPBUFR} ./prepbufr":"#ln -s ${PREPBUFR} ./prepbufr\n", "  bkcv_option=NAM":"  bkcv_option=GLOBAL\n", \
             "  OBS_ROOT=":com_obs,"  BK_ROOT=":com_bkg,"  ENS_ROOT=":com_ens,"  GSIPROC=":jobproc_few } \
    ,"case02-oneobs-nam":{"  GSI_NAMELIST=":"  GSI_NAMELIST=${GSI_ROOT}/scripts/comgsi_namelist_oneobs.sh\n", \
             "ln -s ${PREPBUFR} ./prepbufr":"#ln -s ${PREPBUFR} ./prepbufr\n", \
             "  OBS_ROOT=":com_obs,"  BK_ROOT=":com_bkg,"  ENS_ROOT=":com_ens,"  GSIPROC=":jobproc_few} \
    ,"case03-conv":{"  OBS_ROOT=":com_obs,"  BK_ROOT=":com_bkg,"  ENS_ROOT=":com_ens,"  GSIPROC=":jobproc_few} \
    ,"case04-allobs":{"#      ln -s ${srcobsfile[$ii]}  ${gsiobsfile[$ii]}":linkallobs, \
             "  OBS_ROOT=":com_obs,"  BK_ROOT=":com_bkg,"  ENS_ROOT=":com_ens,"  GSIPROC=":jobproc_few} \
    ,"case05-3DEnVar":{"  if_hybrid=":hybrid_yes,"  OBS_ROOT=":com_obs,"  BK_ROOT=":com_bkg,"  ENS_ROOT=":com_ens,"  GSIPROC=":jobproc_few} \
    ,"case07-4DEnVar":{"  if_hybrid=":hybrid_yes,"  if_4DEnVar=":EnVar4D_yes,"  BK_FILE=${BK_ROOT}":"  BK_FILE=${BK_ROOT}/wrfout_d01_2017-05-13_18:00:00\n", \
             "  OBS_ROOT=":com_obs,"  BK_ROOT=":com_bkg,"  ENS_ROOT=":com_ens,"  GSIPROC=":jobproc_few} \
    ,"case08-nmmb":{"  ANAL_TIME=":"  ANAL_TIME=2012062812\n","  bk_core=ARW":"  bk_core=NMMB\n", \
             "  OBS_ROOT=":nmmb_obs,"  BK_ROOT=":nmmb_bkg,"  ENS_ROOT=":nmmb_ens,"  GSIPROC=":jobproc_few,
             "  BK_FILE=${BK_ROOT}":"  BK_FILE=${BK_ROOT}/input_domain_01_nemsio\n", "  PREPBUFR=":"  PREPBUFR=${OBS_ROOT}/gfs.t${HH}z.prepbufr.nr\n"} \

    ,"case09-wrfchem":{"  OBS_ROOT=":chem_obs,"  BK_ROOT=":chem_bkg,"  GSIPROC=":jobproc_few } \
    ,"case10-cmaq":{"  OBS_ROOT=":chem_obs,"  BK_ROOT=":chem_bkg,"  GSIPROC=":jobproc_few,"  ANAL_TIME=":"  ANAL_TIME=2013062112\n","  bk_core=":"  bk_core=CMAQ\n", \
             "  BK_FILE=":"  BK_FILE=${BK_ROOT}/cmaq2gsi_4.7_20130621_120000.bin\n","  PREPBUFR=":"  PREPBUFR=${OBS_ROOT}/anow.2013062112.bufr\n"} \
    ,"case11-gfs":{"  OBS_ROOT=":gfs_obs,"  BK_ROOT=":gfs_bkg,"  GSIPROC=":jobproc_few } \

    ,"case21-observer-conv":{"  OBS_ROOT=":case2124_obs,"  BK_ROOT=":case2124_bkg,"  GSIPROC=":jobproc_few, "  ANAL_TIME=":"  ANAL_TIME=2014021300\n",  \
     "  PREPBUFR=":"  PREPBUFR=${OBS_ROOT}/gdas1.t${HH}z.prepbufr.nr\n", "  BK_FILE=${BK_ROOT}":"  BK_FILE=${BK_ROOT}/wrfarw.ensmean\n","  if_observer":"  if_observer=Yes\n"} \
    ,"case22-enkf-conv":{"  OBS_ROOT=":case2124_obs,"  BK_ROOT=":case2124_bkg,"  GSIPROC=":jobproc_many, "  diag_ROOT":"  diag_ROOT=../case21-observer-conv\n" } \

    ,"case23-observer-allobs":{"  OBS_ROOT=":case2124_obs,"  BK_ROOT=":case2124_bkg,"  GSIPROC=":jobproc_few, "  ANAL_TIME=":"  ANAL_TIME=2014021300\n", \
             "  PREPBUFR=":"  PREPBUFR=${OBS_ROOT}/gdas1.t${HH}z.prepbufr.nr\n", "  BK_FILE=${BK_ROOT}":"  BK_FILE=${BK_ROOT}/wrfarw.ensmean\n", \
             "  if_observer":"  if_observer=Yes\n", "#      ln -s ${srcobsfile[$ii]}  ${gsiobsfile[$ii]}":linkallobs } \
    ,"case24-enkf-allobs":{"  OBS_ROOT=":case2124_obs,"  BK_ROOT=":case2124_bkg,"  GSIPROC=":jobproc_many,"  diag_ROOT":"  diag_ROOT=../case23-observer-allobs\n", \
             '  list="conv"':'  list="conv amsua_n18 mhs_n18 hirs4_n19"\n' } \

    ,"case25-observer-gfs":{"  OBS_ROOT=":case2526_obs,"  BK_ROOT=":case2526_bkg,"  GSIPROC=":jobproc_few, "  ANAL_TIME=":"  ANAL_TIME=2014092918\n",  \
             "  PREPBUFR=":"  PREPBUFR=${OBS_ROOT}/gdas1.t18z.prepbufr.nr\n","  if_observer":"  if_observer=Yes\n", "  GFSCASE=":"  GFSCASE=enkf_glb_t62\n" } \
    ,"case26-enkf-gfs":{"  OBS_ROOT=":case2526_obs,"  BK_ROOT=":case2526_bkg,"  GSIPROC=":jobproc_many,"  diag_ROOT":"  diag_ROOT=../case25-observer-gfs\n" } \
    }

allcases=[\
    "case01-oneobs-glb","case02-oneobs-nam", 
    "case03-conv","case04-allobs","case05-3DEnVar","case07-4DEnVar", \
#   "case08-nmmb", \  #remove case08 from test suite due to difficulties to fix bugs and nmmb is phasing away to fv3
    "case09-wrfchem","case10-cmaq","case11-gfs", \
#   "case12-fv3", "case13-cloudana"
    "case21-observer-conv", "case22-enkf-conv", "case23-observer-allobs","case24-enkf-allobs","case25-observer-gfs","case26-enkf-gfs" ]


#############  This this beginning of the program   ############################
from pyrunconfig import gsi_src_directory, build_directory_location,project_code, queue_name
import os, sys
from datetime import datetime

if (sys.version_info < (3, 0)):
   print('Your python version is:'+str(sys.version_info.major)+'.'+str(sys.version_info.minor))
   print('You must run this program with python v3.0 and above')
   sys.exit()

comgsi_dir=gsi_src_directory


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

fullpath=build_directory_location +"/"+cmdparam
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
  if (dir_list[0][0]!= "/"):  # if not absolute path, add the prefix based on "build_directory_location"
    for i in range(len(dir_list)):
      dir_list[i]=build_directory_location +"/"+dir_list[i]
else:
  print("You don't run me correctly!")
  print("Usage: run.py  <one_build_directory | list_file_name>  [submit] [case_numbers[,]]")
  exit()

tot=len(dir_list)
if tot>1:
  print("There are "+str(tot)+" directories in total")

os.system("mkdir -p dir.tmp")
if (tot > 1 and not dry_run): #only need to write out dir.* files when working on more then 1 directory
  if os.path.isfile("dir.tmp/dir.todo"):
    os.rename("dir.tmp/dir.todo", "dir.tmp/dir.todo_"+datetime.now().strftime("%y%m%d_%H%M%S"))
  if os.path.isfile("dir.good"):
    os.rename("dir.tmp/dir.good", "dir.tmp/dir.good_"+datetime.now().strftime("%y%m%d_%H%M%S"))
  if os.path.isfile("dir.notready"):
    os.rename("dir.tmp/dir.notready", "dir.tmp/dir.notready_"+datetime.now().strftime("%y%m%d_%H%M%S"))
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
  mylen=len(mybuild)
  if mybuild[mylen-1]=="/":
    build_ID=mybuild[mylen-5:mylen-1]
  else:
    build_ID=mybuild[mylen-4:mylen]

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

  ###copy gsi.x from mybuild/bin to job_dir
  os.system("ln -sf "+mybuild+"/bin/gsi.x "+job_dir+"/gsi.x")
  if ("22" in caselist) or ("24" in caselist) or "26" in caselist: #copy enkf executables
    os.system("ln -sf "+mybuild+"/bin/enkf_wrf.x "+job_dir+"/enkf_wrf.x")
    os.system("ln -sf "+mybuild+"/bin/enkf_gfs.x "+job_dir+"/enkf_gfs.x")

  for x in mycases:
    if (x.find("wrfchem")>=0 ) or (x.find("cmaq")>=0):
      srcfname=comgsi_dir+"/scripts/comgsi_run_chem.ksh"
    elif (x.find("enkf")>=0 and x.find("gfs")>=0):
      srcfname=comgsi_dir+"/scripts/comenkf_run_gfs.ksh"
    elif (x.find("enkf")>=0): #enkf regional
      srcfname=comgsi_dir+"/scripts/comenkf_run_regional.ksh"
    elif (x.find("gfs")>=0):
      srcfname=comgsi_dir+"/scripts/comgsi_run_gfs.ksh"
    else:
      srcfname=comgsi_dir+"/scripts/comgsi_run_regional.ksh"

    jobfname=generatejob(x,srcfname, build_ID)

  if (tot>1 and not dry_run): fgood.write(mybuild+"\n")

  
if(tot>1 and not dry_run):
  for mybuild in dir_list[buildknt-1:]:
    ftodo.write(mybuild+"\n")
  fgood.close()
  fbad.close()
  ftodo.close()

