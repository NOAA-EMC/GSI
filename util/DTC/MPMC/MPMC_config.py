import os, getpass, socket; hostnode=socket.gethostname()
######### only make changes to project_acct and/or queue_name ############
#
if hostnode.startswith("cheyenne"):    ## Cheyenne
  project_acct="P48503002"  #"P48500053"
  queue_name = 'premium'

elif hostnode.startswith("tfe"):  ## THEIA
  project_acct="wrfruc" #comgsi
  queue_name = 'batch'

elif hostnode.startswith("fe"):  ## Jet
  project_acct="wrfruc"
  queue_name = 'batch'

elif hostnode.startswith("GSI_Docker"):
  project_acct="comgsi"
  queue_name = 'batch'

else:
  print("I'm new to host: "+hostnode+"\nPlease set me up first")
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
  build_root = os.getcwd()+"/b_"+branchName+"_"+commitID 
#
username=getpass.getuser()
#
################## Users usually don't make changes after this line ############
#
if hostnode.startswith("cheyenne"):hostname="Cheyenne"### Don't change hostname
elif hostnode.startswith("tfe"):hostname="Theia"  ### Don't change hostname
elif hostnode.startswith("fe"):hostname="Jet"  ### Don't change hostname
elif hostnode.startswith("GSI_Docker"):hostname="GSI_Docker"  ### Don't change hostname
else: print("unkonw host:"+hostnode+"\n\n"); exit()
#
module_pre = 'source /etc/profile.d/modules.sh\nmodule purge\n'
#construct PBS queue directives
q_directives =              '#PBS -A ' + project_acct + '\n'
q_directives = q_directives+'#PBS -l walltime=00:30:00 \n'  
q_directives = q_directives+'#PBS -q '+ queue_name+'\n'
q_directives = q_directives+'#PBS -j oe \n'
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
  comp_post='ncarenv/1.2 ncarcompilers/0.4.1'

elif hostname.startswith("Theia"):  ######################################### Theia
  rocoto_exe='/apps/rocoto/1.2.4/bin/rocotorun'
  rocoto_scheduler='moabtorque'
  q_directives = q_directives+'#PBS -l procs=8\n'

elif hostname.startswith("Jet"):  ######################################### Jet
  rocoto_exe='/apps/rocoto/1.2.4.1/bin/rocotorun'
  rocoto_scheduler='moabtorque'
  XML_native='-l partition=xjet' #xjet 24 cores/node
  q_directives = q_directives+'#PBS -l procs=8\n'
  q_directives = q_directives+'#PBS -l partition=xjet\n'
  module_pre=module_pre+"module load newdefaults\n"  #specific for Jet

elif hostname.startswith("GSI_Docker"):  ######################################### GSI_Docker
  rocoto_exe='/fake/rocotorun'
  rocoto_scheduler='pbsfake'
  q_directives = q_directives
  serial_run=True

#elif hostname.startswith("a_new_host"):
 
#---------------------------------------------------------------|
#Read in build options
foptions=open("optionlist."+hostname, "r")
build_options=[]
while True:
  line=foptions.readline()
  if not line:
    break;
  if not (line.strip().startswith('#') or line.strip()==''):
    build_options.append(line.strip())
    
#for x in build_options:
#   print(x)


