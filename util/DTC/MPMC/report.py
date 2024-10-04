#!/usr/bin/python3
# the first line does not matter, it will be replaced by running "initmpmc"
###################################################
# This python script will check compiling and running results and generate a report
#
# by Guoqing Ge, 2018/8/20, guoqing.ge@noaa.gov
#
# Usage:
#   report.py <build_directory_list_file> [report_option]
#    where report_option is either 1 (compiling results), or 2 (running results)
#       if report_option is missing, do both 1 and 2
#
###################################################
#
def empty_fun():pass
import sys,os
from MPMC_config import build_root, hostname
from CASE_config import allcases
from datetime import datetime
if hostname.startswith("Cheyenne"): Cheyenne=True
else: Cheyenne=False

def compiling_check( mybuild ):
# 1-Makefile  2-gsi.x  3-enkf_gfs.x  4-enkf_wrf.x
# 5-ndate.x, nc_diag_cat.x and test_nc_unlimdims.x
# 6-community utilities (bufrtools, read_diag, etc)
# |1|2|3|4|5|6|
# |Y|Y|Y|Y|Y|Y|==>
   exelist=[['Makefile'], ['gsi.x'], ['enkf_gfs.x'], ['enkf_wrf.x'] \
       ,['ndate.x', 'nc_diag_cat.x','test_nc_unlimdims.x'] \
       ,['bufr_append_sample.x', 'bufr_decode_l2rwbufr.x', 'bufr_decode_radiance.x', \
       'bufr_decode_sample.x', 'bufr_encode_l2rwbufr.x', 'bufr_encode_radarbufr.x', \
       'bufr_encode_sample.x', 'prepbufr_append_retrieve.x', 'prepbufr_append_surface.x', \
       'prepbufr_append_upperair.x', 'prepbufr_decode_all.x', 'prepbufr_encode_surface.x', \
       'prepbufr_encode_upperair.x', 'prepbufr_inventory.x','enspreproc.x', 'initialens.x', \
       'process_NSSL_mosaic.x', 'read_diag_conv.x', 'read_diag_rad.x' ] \
       ]
   results = "|" 

   for x in exelist:
     allyes=True
     for y in x:
       if y=="Makefile":
         file1=mybuild+'/'+y
       else:
         file1=mybuild+'/bin/'+y
       if not (os.path.isfile(file1) and os.access(file1, os.R_OK)):
         print("NOT_FOUND: "+file1)
         allyes=False

     if allyes:
       results=results+"Y|"
     else:
       results=results+"N|"

   return results
#--------------------  end of function compiling_check
#
############## fucntion getModuleName: find the full module name in the src string
def getModuleName(src,target):
  k1=src.find(target)
  if k1>=0:
    k2=src.find(' ',k1)
    if k2<0: k2=len(src)
    mname=src[k1:k2].strip()
    return mname
  else:
    return ''
#--------------------- end of function of getModuleName(...)
#
row_results={}
for x in allcases: row_results[x]={}

### parse the command line parameters
arguments = len(sys.argv) - 1
if (arguments >=1):
  cmdparam=sys.argv[1]
else:
  cmdparam="list.all"
  report_option="all"

if cmdparam.find('help')>=0:
  print("Usage: report.py  [one_build_directory | list_file_name] [1|2]")
  exit()

report_option="all"
if (arguments >=2):
  report_option=sys.argv[2]

fullpath=build_root +"/"+cmdparam
cwd = os.getcwd()
if (os.path.isdir(fullpath)):
  print("Check running results in the given directory: "+fullpath)
  dir_list=[fullpath]
elif (os.path.isfile(cwd+"/"+cmdparam)):
  print("Check running results in all directories listed in the file: "+cwd+"/"+cmdparam)
  with open(cwd+"/"+cmdparam) as f:
    dir_list = f.readlines()
  if(len(dir_list)<=0): print("\nThe list file is empty, exit\n"); exit()
  dir_list = [x.strip() for x in dir_list]  #strip "\n"
  if (dir_list[0][0]!= "/"):  # if not absolute path, add the prefix based on "build_root"
    for i in range(len(dir_list)):
      dir_list[i]=build_root +"/"+dir_list[i]
else:
  print("You don't run me correctly!")
  print("Usage: report.py  <one_build_directory | list_file_name> [1|2]")
  exit()

tot=len(dir_list)
if tot>1:
  print("There are "+str(tot)+" directories in total")

build_list=[]; compiler=[]; netcdf=[]; mpi=[]; lapack=[]; build_result=[]
case_matrix={}
for mybuild in dir_list:
  #print("Now working on "+mybuild)
  print(".",end='',flush=True)
  if mybuild.endswith('/'): mybuild=mybuild[:-1] #remove trailing '/'
  k=mybuild.rfind('/')
  mybuild_short=mybuild[k+1:len(mybuild)] # get the relative path
  mybuild_parent=mybuild[0:k]             # get the parent directory
  build_list.append(mybuild_short)

  if(report_option=='1' or report_option=='all'):  #this step takes a little bit time, so only do it when necessary
    build_result.append(compiling_check(mybuild))
    ### read module informatin from compile.sh
    file1=open(mybuild+"/compile.sh", "r")
    found_lapack=False
    while True:
      line=file1.readline()
      if not line:
        break
      if line.startswith("module"):
        tem1=getModuleName(line,"intel")
        if tem1!='': compiler.append(tem1)
        tem1=getModuleName(line,"pgi")
        if tem1!='': compiler.append(tem1)
        tem1=getModuleName(line,"gnu")
        if tem1!='': compiler.append(tem1)
        tem1=getModuleName(line,"netcdf")
        if tem1!='': netcdf.append(tem1)
        tem1=getModuleName(line,"impi")
        if tem1!='': mpi.append(tem1)
        tem1=getModuleName(line,"mvapich2")
        if tem1!='': mpi.append(tem1)
        tem1=getModuleName(line,"openmpi")
        if tem1!='': mpi.append(tem1)
        tem1=getModuleName(line,"mpt")
        if tem1!='': mpi.append(tem1)
        tem1=getModuleName(line,"lapack")
        if tem1!='': lapack.append(tem1);found_lapack=True
        tem1=getModuleName(line,"openblas")
        if tem1!='': lapack.append(tem1);found_lapack=True
        tem1=getModuleName(line,"mkl")
        if tem1!='': lapack.append(tem1);found_lapack=True
    file1.close()
    if Cheyenne and (not found_lapack):
      lapack.append(" ")

  row_results=row_results.fromkeys(row_results,'')  #clear the values
  at_lease_one_job_submitted=False
  for x in allcases:
    case_dir=mybuild+"/run/"+x
    if os.path.isdir(case_dir): # directory created
      at_lease_one_job_submitted=True
      run_results=os.popen("grep 'PROGRAM GSI_ANL HAS ENDED' "+case_dir+"/stdout").read().strip()
      if run_results=='': # if empty, it may be an EnKF case
        run_results=os.popen("grep 'PROGRAM ENKF_ANL HAS ENDED' "+case_dir+"/stdout").read().strip()
      if run_results=='':
        row_results[x]='N' # 0 means failure
      else:
        row_results[x]='Y' # 1 means good
    else:
      row_results[x]='-' # - means case not genereate or submitted

  if (at_lease_one_job_submitted):
    case_matrix[mybuild_short]=row_results
  else:
    case_matrix[mybuild_short]={}

#read branchName and commitID from branch_commit.txt
file1=open(mybuild_parent+"/branch_commit.txt", "r")
branchName=file1.readline().strip()
commitID=file1.readline().strip()
file1.close()
print('\n\n'+datetime.now().strftime("%Y%m%d")+ '  Tested on commit: '+commitID+' branch: '+branchName, end='\n')
tem=max(build_list,key=len)
longest=(len(tem))
#
###### generate reports of GSI/EnKF running results   ##########################################
if(len(case_matrix)==0): print("No job has been submitted")
if (len(case_matrix)>0 and (report_option=='2' or report_option=='all')):
  width_tot=len(allcases)*3+ longest +2
  print('')
  ### write out case names for easy reference
  myknt=1
  for x in allcases:
    if (myknt%4==0):print(x+'\n',end='');
    else:print(x,end=', ')
    myknt=myknt+1
  print('\n\n',end='')
    
  title='----- GSI/EnKF Running Tests'
  print(title+'-'*(width_tot-len(title)))
  first_time=True
  for x in build_list:
    if not any(case_matrix[x]): continue
    MAX_CASES=40  ### maximum number of MPMC cases
    one_row=[None] * MAX_CASES
    ###if first_time: print('|'+'case#'.rjust(longest)+"|",end='')
    if first_time: print('case#'.rjust(longest+1)+"|",end='')
    for i in range(MAX_CASES):
      sNumber=str(i+1).zfill(2) 
      for key,value in case_matrix[x].items():
        if key.startswith("case"+sNumber):
          if first_time: print(sNumber,end='|')
          one_row[i]=value
          break
    
    if first_time:
      print('\n',end='')
      first_time=False

    ###print('+'+ '-'*longest+"+"+"--+"*15)  ## comment out to save display space

    ###print('|'+x.ljust(longest),end='|')
    print(x.ljust(longest+1),end='|')
    for i in range(len(one_row)):
      if (one_row[i]!=None):
        print(one_row[i].ljust(2),end='|')
    print('\n',end='')

  ###print('+'+ '-'*longest+"+"+"--+"*15)  ##  comment out to save display space

##################### Generate report of GSI/EnKF Compiling test results ##########################################
if (report_option=="1" or report_option=='all'):
  tem=max(compiler,key=len); max_compiler=len(tem)
  tem=max(mpi,key=len); max_mpi=len(tem)
  tem=max(netcdf,key=len); max_netcdf=len(tem)
  if (len(lapack)>0):
    tem=max(lapack,key=len); max_lapack=len(tem)
  else:
    max_lapack=0
  width_tot=8+max_compiler+2+max_mpi+2+max_netcdf+2+max_lapack+2+longest+2
  print('')
  title='----- GSI/EnKF Compiling Tests'
  print(title+'-'*(width_tot-len(title)))
  print('1-Makefile  2-gsi.x  3-enkf_gfs.x  4-enkf_wrf.x 5-ndate.x, nc_diag_cat.x and test_nc_unlimdims.x \
       \n6-community utilities (read_diag, etc) \
       \n|1|2|3|4|5|6|')
  for i in range(len(dir_list)):
    print(build_result[i].ljust(8),end='=>|')
    print(build_list[i][0:2].ljust(3),end='|')
    print(compiler[i].ljust(max_compiler+1),end='|')
    print(mpi[i].ljust(max_mpi+1),end='|')
    print(netcdf[i].ljust(max_netcdf+1),end='|')
    if (len(lapack)>0):
      print(lapack[i].ljust(max_lapack+1),end='|')
    print('>'+build_list[i].ljust(longest),end='')
    print('\n',end='')

print('\n',end='')

