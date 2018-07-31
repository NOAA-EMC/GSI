############################################################
# This script will add the python3 location
# to the beginning of the MPMC scripts
# so that they can run by just typing the script name
# at the command line without typing "python" first
# i.e:       "run.py list_file" 
# instead of "python run.py list_file"
# by Guoqing Ge, 2018/8/24
############################################################
import socket,os
hostname=socket.gethostname()
if hostname.startswith("cheyenne"):  ## Cheyenne
  python3Location="/usr/bin/python3"
  os.system("ln -sf pyrunconfig.py_cheyenne pyrunconfig.py")

elif hostname.startswith("tfe"):  ## THEIA
  python3Location="/apps/intel/intelpython3/bin/python3"
  os.system("ln -sf pyrunconfig.py_theia pyrunconfig.py")

elif hostname.startswith("fe"): ## Jet
  python3Location="/lfs3/projects/wrfruc/gge/miniconda3/bin/python3"
  os.system("ln -sf pyrunconfig.py_jet pyrunconfig.py")
else:
  print("I'm new to Host: "+hostname+". Please set up me first")
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


print("python3 is found at:"+python3Location)
print("\nIt is good to go. Happy  build.py -> run.py -> report.py  !!\n\n")

add_python_location("build.py")
add_python_location("run.py")
add_python_location("report.py")
