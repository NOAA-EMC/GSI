###################################################
# This script generates job scripts for MPMC test suite cases
#
# by Guoqing Ge, 2018/8/18, guoqing.ge@noaa.gov
#
###################################################
#
from MPMC_config import ProdGSI_root, build_root, project_acct, queue_name, hostname, rocoto_scheduler
import os

SBATCH_extras=""
MPI_CMD_ORG='      RUN_COMMAND="mpirun -np ${GSIPROC} " ;;\n'
if hostname.startswith("Theia"):  ## Theia
  crtm_dir="/scratch4/BMC/comgsi/case_data/CRTM_v2.3.0"
  data_root="/scratch4/BMC/comgsi/case_data"
  myARCH="  ARCH='LINUX_PBS'\n" 
  few_cpu_res="--ntasks=4"; few_procs="4" 
  many_cpu_res="--ntasks=24"; many_procs="24"  #24 cores/node
  MPI_CMD_ORG='      RUN_COMMAND="srun " ;;\n'

elif hostname.startswith("Hera"):  ## Hera
  crtm_dir="/scratch1/BMC/comgsi/case_data/CRTM_v2.3.0"
  data_root="/scratch1/BMC/comgsi/case_data"
  myARCH="  ARCH='LINUX_PBS'\n" 
  few_cpu_res="--ntasks=4"; few_procs="4" 
  many_cpu_res="--ntasks=40"; many_procs="40"  #40 cores/node
  MPI_CMD_ORG='      RUN_COMMAND="srun " ;;\n'

elif hostname.startswith("Jet"): ## Jet
  crtm_dir="/lfs1/projects/wrfruc/gge/MPMC/case_data/CRTM_v2.3.0"
  data_root="/lfs1/projects/wrfruc/gge/MPMC/case_data"
  myARCH="  ARCH='LINUX_PBS'\n" 
  few_cpu_res="--ntasks=4"; few_procs="4" 
  #SBATCH_extras="#SBATCH --partition=tjet\n" #12 cores/node
  #SBATCH_extras="#SBATCH --partition=xjet\n" #24 cores/node
  SBATCH_extras="#SBATCH --partition=kjet\n" #40 cores/node
  many_cpu_res="--ntasks=40"; many_procs="40" 
  MPI_CMD_ORG='      RUN_COMMAND="srun " ;;\n'

elif hostname.startswith("Orion"):  ## Orion
  crtm_dir="/work/noaa/comgsi/MPMC/case_data/CRTM_v2.3.0"
  data_root="/work/noaa/comgsi/MPMC/case_data"
  myARCH="  ARCH='LINUX_PBS'\n" 
  few_cpu_res="--ntasks=4"; few_procs="4" 
  SBATCH_extras="#SBATCH --partition=orion\n" #40 cores/node
  many_cpu_res="--ntasks=40"; many_procs="40"  #40 cores/node
  MPI_CMD_ORG='      RUN_COMMAND="srun " ;;\n'

elif hostname.startswith("Cheyenne"):  ## Cheyenne
  crtm_dir="/glade/p/ral/jntp/DAtask/case_data/CRTM_v2.3.0"
  data_root="/glade/p/ral/jntp/DAtask/case_data"
  myARCH="  ARCH='LINUX_PBS'\n" 
  few_cpu_res="select=1:ncpus=4:mpiprocs=4"; few_procs="4" 
  many_cpu_res="select=2:ncpus=36:mpiprocs=36"; many_procs="36"  #Dual-socket nodes, 18 cores per socket, 20 ensembles, need at least 20cores

elif hostname.startswith("GSI_Docker"):  ## GSI_Docker
  crtm_dir="/tutorial/case_data/CRTM_v2.3.0"
  data_root="/tutorial/case_data"
  myARCH="  ARCH='LINUX'\n" 
  few_cpu_res="docker"; few_procs="1" 
  many_cpu_res="docker"; many_procs="1"

######################### variables definition #######################################
hybrid_yes="  if_hybrid=Yes    # Yes, or, No -- case sensitive !\n"
EnVar4D_yes="  if_4DEnVar=Yes   # Yes, or, No -- case sensitive (set if_hybrid=Yes first)!\n"
linkallobs ="       ln -s ${srcobsfile[$ii]}  ${gsiobsfile[$ii]}\n"
#----------------------
#**** substitutions to be made in run scripts based on template scripts in ProdGSI/ush
jobproc_few="  GSIPROC="+few_procs+"\n"
jobproc_many="  GSIPROC="+many_procs+"\n"
#
com_obs="  OBS_ROOT="+data_root+"/com_2018081212/obs\n"
com_bkg="  BK_ROOT="+data_root+"/com_2018081212/bkg\n"
com_ens="  ENS_ROOT="+data_root+"/com_2018081212/gfsens\n"
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
     "case01-oneobs-glb":{"  if_oneob=No":"  if_oneob=Yes\n", "  bkcv_option=NAM":"  bkcv_option=GLOBAL\n", \
             "ln -s ${PREPBUFR} ./prepbufr":"#ln -s ${PREPBUFR} ./prepbufr\n", \
             "  PREPBUFR=":"  PREPBUFR=${OBS_ROOT}/rap.t${HH}z.prepbufr.tm00\n", \
             "  ANAL_TIME=":"  ANAL_TIME=2018081212\n", \
             "  BK_FILE=${BK_ROOT}":"  BK_FILE=${BK_ROOT}/wrfout_d01_2018-08-12_12:00:00\n", \
             "  OBS_ROOT=":com_obs,"  BK_ROOT=":com_bkg,"  ENS_ROOT=":com_ens,"  GSIPROC=":jobproc_few } \
    ,"case02-oneobs-nam":{"  if_oneob=No":"  if_oneob=Yes\n", \
             "ln -s ${PREPBUFR} ./prepbufr":"#ln -s ${PREPBUFR} ./prepbufr\n", \
             "  PREPBUFR=":"  PREPBUFR=${OBS_ROOT}/rap.t${HH}z.prepbufr.tm00\n", \
             "  ANAL_TIME=":"  ANAL_TIME=2018081212\n", \
             "  BK_FILE=${BK_ROOT}":"  BK_FILE=${BK_ROOT}/wrfout_d01_2018-08-12_12:00:00\n", \
             "  OBS_ROOT=":com_obs,"  BK_ROOT=":com_bkg,"  ENS_ROOT=":com_ens,"  GSIPROC=":jobproc_few} \
    ,"case03-conv":{"  PREPBUFR=":"  PREPBUFR=${OBS_ROOT}/rap.t${HH}z.prepbufr.tm00\n", \
             "  BK_FILE=${BK_ROOT}":"  BK_FILE=${BK_ROOT}/wrfout_d01_2018-08-12_12:00:00\n", \
             "  ANAL_TIME=":"  ANAL_TIME=2018081212\n", \
             "  OBS_ROOT=":com_obs,"  BK_ROOT=":com_bkg,"  ENS_ROOT=":com_ens,"  GSIPROC=":jobproc_few} \
    ,"case04-allobs":{"#      ln -s ${srcobsfile[$ii]}  ${gsiobsfile[$ii]}":linkallobs, \
             "  PREPBUFR=":"  PREPBUFR=${OBS_ROOT}/rap.t${HH}z.prepbufr.tm00\n", \
             "  ANAL_TIME=":"  ANAL_TIME=2018081212\n", \
             "  BK_FILE=${BK_ROOT}":"  BK_FILE=${BK_ROOT}/wrfout_d01_2018-08-12_12:00:00\n", \
             "  OBS_ROOT=":com_obs,"  BK_ROOT=":com_bkg,"  ENS_ROOT=":com_ens,"  GSIPROC=":jobproc_few} \
    ,"case05-3DEnVar":{"  if_hybrid=":hybrid_yes,"  if_nemsio=No":"  if_nemsio=Yes\n", \
             "#      ln -s ${srcobsfile[$ii]}  ${gsiobsfile[$ii]}":linkallobs, \
             "  PREPBUFR=":"  PREPBUFR=${OBS_ROOT}/rap.t${HH}z.prepbufr.tm00\n", \
             "  ANAL_TIME=":"  ANAL_TIME=2018081212\n", \
             "  BK_FILE=${BK_ROOT}":"  BK_FILE=${BK_ROOT}/wrfout_d01_2018-08-12_12:00:00\n", \
             "  OBS_ROOT=":com_obs,"  BK_ROOT=":com_bkg,"  ENS_ROOT=":com_ens,"  GSIPROC=":jobproc_few} \
    ,"case07-4DEnVar":{"  if_hybrid=":hybrid_yes,"  if_4DEnVar=":EnVar4D_yes, "  if_nemsio=No":"  if_nemsio=Yes\n", \
             "  BK_FILE=${BK_ROOT}":"  BK_FILE=${BK_ROOT}/wrfout_d01_2018-08-12_12:00:00\n", \
             "  PREPBUFR=":"  PREPBUFR=${OBS_ROOT}/rap.t${HH}z.prepbufr.tm00\n", \
             "  ANAL_TIME=":"  ANAL_TIME=2018081212\n", \
             "  OBS_ROOT=":com_obs,"  BK_ROOT=":com_bkg,"  ENS_ROOT=":com_ens,"  GSIPROC=":jobproc_few} \
    ,"case08-nmmb":{"  ANAL_TIME=":"  ANAL_TIME=2012062812\n","  bk_core=ARW":"  bk_core=NMMB\n", \
             "  OBS_ROOT=":nmmb_obs,"  BK_ROOT=":nmmb_bkg,"  ENS_ROOT=":nmmb_ens,"  GSIPROC=":jobproc_few,
             "  BK_FILE=${BK_ROOT}":"  BK_FILE=${BK_ROOT}/input_domain_01_nemsio\n", "  PREPBUFR=":"  PREPBUFR=${OBS_ROOT}/gfs.t${HH}z.prepbufr.nr\n"} \

    ,"case09-wrfchem":{"  OBS_ROOT=":chem_obs,"  BK_ROOT=":chem_bkg,"  GSIPROC=":jobproc_few } \
    ,"case10-cmaq":{"  OBS_ROOT=":chem_obs,"  BK_ROOT=":chem_bkg,"  GSIPROC=":jobproc_few,"  ANAL_TIME=":"  ANAL_TIME=2013062112\n","  bk_core=":"  bk_core=CMAQ\n", \
             "  BK_FILE=":"  BK_FILE=${BK_ROOT}/cmaq2gsi_4.7_20130621_120000.bin\n","  PREPBUFR=":"  PREPBUFR=${OBS_ROOT}/anow.2013062112.bufr\n"} \
    ,"case11-gfs":{"  OBS_ROOT=":gfs_obs,"  BK_ROOT=":gfs_bkg,"  GSIPROC=":jobproc_few } \

    ,"case21-observer-conv":{"  OBS_ROOT=":case2124_obs,"  BK_ROOT=":case2124_bkg,"  GSIPROC=":jobproc_few, "  ANAL_TIME=":"  ANAL_TIME=2014021300\n",  \
             "  PREPBUFR=":"  PREPBUFR=${OBS_ROOT}/gdas1.t${HH}z.prepbufr.nr\n", "  BK_FILE=${BK_ROOT}":"  BK_FILE=${BK_ROOT}/wrfarw.ensmean\n", \
             "  if_observer":"  if_observer=Yes\n"} \
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

fullLIST='1,2,3,4,5,7,9,10,11,21,22,23,24,25,26'
caselist=fullLIST.split(',')

mycases=[]
for x in caselist:
  k=int(x)
  for i in range(len(allcases)):
    if allcases[i].find("case{0:02d}".format(k)) >=0:
      mycases.append(allcases[i])

#####################  generate runing job scripts for a build option #########
def create_run_scripts(build_ID, modules, job_dir):
  if modules.find("module load mpt") >=0:
    MPI_CMD='      RUN_COMMAND="mpiexec_mpt dplace -s 1  " ;;\n'
  else:
    MPI_CMD=MPI_CMD_ORG

  for x in mycases:
    if (x.find("wrfchem")>=0 ) or (x.find("cmaq")>=0):
      srcfname=ProdGSI_root+"/ush/comgsi_run_chem.ksh"
    elif (x.find("enkf")>=0 and x.find("gfs")>=0):
      srcfname=ProdGSI_root+"/ush/comenkf_run_gfs.ksh"
    elif (x.find("enkf")>=0): #enkf regional
      srcfname=ProdGSI_root+"/ush/comenkf_run_regional.ksh"
    elif (x.find("gfs")>=0):
      srcfname=ProdGSI_root+"/ush/comgsi_run_gfs.ksh"
    else:
      srcfname=ProdGSI_root+"/ush/comgsi_run_regional.ksh"

    jobfname=generateAjob(x,srcfname, build_ID, modules, job_dir, MPI_CMD)

#####################  generate a job script for a case #########
def generateAjob(case_name, srcfname, build_ID, modules, job_dir, MPI_CMD):
  global jobknt
  if case_name.find('enkf')>=0:
    cpu_res=many_cpu_res
  else:
    cpu_res=few_cpu_res

  if rocoto_scheduler.find('slurm')>=0:
    PBScmds="#SBATCH --account "+project_acct+"\n" \
    +"#SBATCH -t 00:20:00\n" \
    +"#SBATCH --job-name "+build_ID+"-"+case_name+"\n" \
    +"#SBATCH "+cpu_res+"\n" \
    +"#SBATCH --qos "+queue_name+"\n" \
    +SBATCH_extras \
    +"#SBATCH -o out."+case_name+"\n" \
    +modules+"\n\n" \
    +"set -x\n"
  else:
    PBScmds="### in PBS, cannnot put comments after job decription commands\n" \
    +"#PBS -A "+project_acct+"\n" \
    +"#PBS -l walltime=00:20:00\n" \
    +"#PBS -N "+build_ID+"-"+case_name+"\n" \
    +"#PBS -l "+cpu_res+"\n" \
    +"#PBS -q "+queue_name+"\n" \
    +"#PBS -o out."+case_name+"\n" \
    +"#PBS -j oe\n\n" \
    +modules+"\n\n" \
    +"set -x\n"

  common={"set -x"                   :PBScmds \
      ,'      RUN_COMMAND="mpirun -np ${GSIPROC} "'  :MPI_CMD \
      ,"  ARCH="                     :myARCH \
      ,"cp ${GSI_EXE} gsi.x"         :"ln -sf ${GSI_EXE} gsi.x\n"  #link to gsi to avoid unneccessary duplicates
      ,"cp $ENKF_EXE enkf.x"         :"ln -sf ${ENKF_EXE} enkf.x\n"  #link to enkf to avoid unneccessary duplicates
      ,"  JOB_DIR="                  :"  JOB_DIR="+job_dir+"\n"   \
      ,"  RUN_NAME="                 :"  RUN_NAME="+case_name+"\n" \
      ,"  GSI_ROOT="                 :"  GSI_ROOT="+ProdGSI_root+"\n" \
      ,"  CRTM_ROOT="                :"  CRTM_ROOT="+crtm_dir+"\n"   \
      }

  jobfname=job_dir+'/run.'+case_name
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
  os.system("chmod +x "+jobfname)

  return jobfname
#-----------------------End of generateAjob(...)---------------|

