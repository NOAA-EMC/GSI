#!/usr/bin/python3
# the first line does not matter, it will be replaced by running "initmpmc"
####### This script is to remove the MPMC entry in the crontab
#######  by Guoqing Ge, 2018/8/31
#
import os
mpmcCron="*/5 * * * * "+os.getcwd()+"/myrocoto.ksh"
print(mpmcCron)
crontab=os.popen("crontab -l").read()
cronlist=crontab.split('\n')

##this is to add a line to crontab
#k1=crontab.find(mpmcCron)
#if k1<0:os.system('(crontab -l;echo "'+mpmcCron+'") | crontab -')
#exit()

##Now remove a line from crontab
yn=input("Are you sure to remove the above entry from the crontab? y/n>>>")
if yn=="n": exit()

f1=open('.tmp.crontab','w')
for x in cronlist:
  if x.find(mpmcCron)<0:
    f1.write(x+'\n')
f1.close()
os.system('cat .tmp.crontab | crontab -')
print("Done!\n-----------Current crontab----------------")
os.system("rm -f .tmp.crontab; crontab -l")
