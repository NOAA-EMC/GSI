############################################################
# This script is to change the first line of python scripts
# to "#!/usr/bin/python3" to reduce uncessary git commits
#
# by Guoqing Ge, 2018/8/31, guoqing.ge@noaa.gov
#
############################################################
import os
python3Location="/usr/bin/python3"
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
