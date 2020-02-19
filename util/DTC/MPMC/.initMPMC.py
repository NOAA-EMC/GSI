############################################################
# This script will add the python3 location
# to the beginning of the MPMC scripts
# so that they can run by just typing the script name
# at the command line without typing "python" first
# i.e:       "report.py list_file" 
# instead of "python report.py list_file"
#
# by Guoqing Ge, 2018/8/24, guoqing.ge@noaa.gov
#
############################################################
import socket,os
hostnode=socket.gethostname()
if hostnode.startswith("cheyenne"):  ## Cheyenne
  python3Location="/usr/bin/python3"
  hostname="Cheyenne"

elif hostnode.startswith("tfe"):  ## THEIA
  python3Location="/apps/intel/intelpython3/bin/python3"
  hostname="Theia"

elif hostnode.startswith("hfe"):  ## THEIA
  python3Location="/apps/intel/intelpython3/bin/python3"
  hostname="Hera"

elif hostnode.startswith("fe"): ## Jet
  python3Location="/lfs1/projects/wrfruc/gge/miniconda3/bin/python3"
  hostname="Jet"

elif hostnode.startswith("Orion"): ## Orion
  python3Location="/apps/python-3.7.5/bin/python3"
  hostname="Orion"

elif hostnode.startswith("GSI_Docker"): ## GSI_Docker
  python3Location="/usr/bin/python3"
  hostname="GSI_Docker"

else:
  print("I'm new to Host: "+hostnode+". Please set up me first")
  exit()

### prepend a line to a file
def add_python_location(filename):
  tmpfile=".tmp.py"
  file1=open(filename,"r")
  file2=open(tmpfile,"w")

  file2.write("#!"+python3Location+"\n")
  line1=file1.readline()
  if not (line1.startswith("#!")): #this line needs to be kept
    file2.write(line1)

  while True:
    line=file1.readline()
    if not line: break  ### end of file, exit the  while loop
    file2.write(line)

  file1.close()
  file2.close()
  os.system("rm -f "+filename+";mv "+tmpfile+" "+filename+"; chmod +x "+filename)

#-----------------------------------------------------------------------------------------
add_python_location("run.py")
#add_python_location("run.py")
add_python_location("report.py")
add_python_location("stopcronMPMC.py")

#
#### need to know ProdGSI location if it does not exist at current direcoty####
if os.path.isdir("../../../../ProdGSI"):
  os.system("ln -snf ../../../../ProdGSI ProdGSI")
  print("\n linked ../../../../ProdGSI as ./ProdGSI\n")
else:
  if not os.path.isdir("ProdGSI"):
    try: #Bind raw_input to input in Python 2:
      input = raw_input
    except NameError:
      pass
    print("It looks like there is no ProdGSI/ under current directory.\
        \nIf you have NOT cloned a copy of ProdGSI yet, clone it to current directory")
    print('Or you can link ProdGSI to current directory\n')
    exit()
    #ProdGSI = input('Enter the locaiton of ProdGSI >>>')
    #assert isinstance(ProdGSI, str)    # native str on Py2 and Py3
    #os.system("ln -sf "+ProdGSI+" ProdGSI")
    #print("\nDone! "+ProdGSI+" already linked as ./ProdGSI\n")

print("\nIt is on "+hostname+" and python3 is found at:"+python3Location)
print("\nPlese create a file\n      config.acct_queue\n to let MPMC know your project account and queue name")
print("    (account name in the first line and queue name in the second line)\n")
print("After that, do run.py  and then  report.py \n\n")

