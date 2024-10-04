###################################################
#
# by Guoqing Ge, 2018/8/28, guoqing.ge@noaa.gov
#
#
import os, getpass, socket; hostnode=socket.gethostname()
from datetime import datetime
######### only make changes to project_acct and/or queue_name ############
#
cheyenne=os.popen('grep -i "cheyenne" /etc/hosts | head -n1').read()
theia=os.popen('grep -i "theia" /etc/hosts | head -n1').read()
hera=os.popen('grep -i "hera" /etc/hosts | head -n1').read()
jet=os.popen('grep -i "jet" /etc/hosts | head -n1').read()
orion=os.popen('hostname | grep -i "orion" | head -n1').read()

if cheyenne:
  project_acct="P48503002"  #"P48500053"
  queue_name = 'premium'
  hostname="Cheyenne"  ### Don't change hostname

elif theia:
  project_acct="comgsi" #wrfruc
  queue_name = 'batch'
  hostname="Theia"  ### Don't change hostname

elif hera:
  project_acct="comgsi" #wrfruc
  queue_name = 'batch'
  hostname="Hera"  ### Don't change hostname

elif orion:
  project_acct="comgsi" #wrfruc
  queue_name = 'batch'
  hostname="Orion"  ### Don't change hostname

elif jet:
  project_acct="wrfruc"
  queue_name = 'windfall' #'batch'
  hostname="Jet"  ### Don't change hostname

elif hostnode.startswith("GSI_Docker"):
  project_acct="comgsi"
  queue_name = 'batch'
  hostname="GSI_Docker"  ### Don't change hostname

else:
  print("\nI'm new to host: "+hostnode+"\nEmail gsi-help@ucar.edu for helps\n")
  exit()

########## Make changes to the following variable only if really necessary--------------
ProdGSI_root = os.getcwd()+"/ProdGSI" #copy or link ProdGSI under MPMC_root or specifiy it here
#
MPMC_root = os.getcwd() # the MPMC scripts directory, i.e, current directory
branchName=os.popen('basename `git --git-dir '+ProdGSI_root+'/.git symbolic-ref HEAD`').read().strip()
commitID_full=os.popen('git --git-dir '+ProdGSI_root+'/.git log -1 |grep commit | head -1').read()[7:]
commitID=commitID_full[0:8]
if not commitID: #empty commitID
  build_root = os.getcwd()+"/build"
else:
  build_root = os.getcwd()+"/b_"+datetime.now().strftime("%Y%m%d")+"_"+branchName+"_"+commitID 
#
username=getpass.getuser()
################# read project_acct and queue_name from config.acct_queue if it exists--------------
if os.path.isfile("config.acct_queue"):
  with open("config.acct_queue",'r') as f1:
    project_acct=f1.readline().strip()
    queue_name=f1.readline().strip()
#
################## Users usually don't make changes after this line ############
#
module_pre = 'source /etc/profile.d/modules.sh\nmodule purge\n'
#construct PBS queue directives
q_directives =              '#PBS -A ' + project_acct + '\n'
q_directives = q_directives+'#PBS -l walltime=00:30:00 \n'  
q_directives = q_directives+'#PBS -q '+ queue_name+'\n'
q_directives = q_directives+'#PBS -j oe \n'
s_directives =              '#SBATCH --account ' + project_acct + '\n'
s_directives = s_directives+'#SBATCH -t 00:45:00 \n'  
s_directives = s_directives+'#SBATCH --qos '+ queue_name+'\n'
cmake_version = ''
comp_post=''
XML_native=''
serial_run=False
xml_set_nodesize=False
#
if hostname.startswith("Cheyenne"):  ######################################### Cheyenne
  xml_set_nodesize=True
  rocoto_exe='/glade/u/home/geguo/rocoto/bin/rocotorun'
  rocoto_scheduler='pbspro'
  cmake_version = 'cmake/3.9.1'
  q_directives = q_directives+'#PBS -l select=1:ncpus=8:mpiprocs=8\n'
  q_directives = q_directives+'#PBS -l inception=login\n'
  comp_post='ncarenv ncarcompilers'

elif hostname.startswith("Theia"):  ######################################### Theia
  rocoto_exe='/apps/rocoto/default/bin/rocotorun'
  rocoto_scheduler='slurm'
  s_directives = s_directives+'#SBATCH --ntasks=8\n'

elif hostname.startswith("Hera"):  ######################################### Hera
  rocoto_exe='/apps/rocoto/default/bin/rocotorun'
  rocoto_scheduler='slurm'
  s_directives = s_directives+'#SBATCH --ntasks=8\n'

elif hostname.startswith("Jet"):  ######################################### Jet
  rocoto_exe='/apps/rocoto/default/bin/rocotorun'
  rocoto_scheduler='slurm'
  s_directives = s_directives+'#SBATCH --ntasks=8\n'
  s_directives = s_directives+'#SBATCH --partition=kjet\n'
  #module_pre=module_pre+"module load newdefaults\n"  #specific for Jet

elif hostname.startswith("Orion"):  ######################################### Jet
  rocoto_exe='/apps/contrib/rocoto/1.3.1/bin/rocotorun'
  rocoto_scheduler='slurm'
  s_directives = s_directives+'#SBATCH --ntasks=8\n'
  s_directives = s_directives+'#SBATCH --partition=orion\n'
  module_pre = 'ulimit -s unlimited\nsource /home/gge/modulefiles/module.sh\nmodule purge\n'

elif hostname.startswith("GSI_Docker"):  ######################################### GSI_Docker
  rocoto_exe='/fake/rocotorun'
  rocoto_scheduler='pbsfake'
  q_directives = q_directives
  serial_run=True

#elif hostname.startswith("a_new_host"):
