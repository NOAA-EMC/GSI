#!/usr/bin/env python
# calcanl_gfs.py
# cory.r.martin@noaa.gov
# 2019-10-11
# script to run executables to produce netCDF analysis
# on GFS gaussian grid for downstream users
import os
import shutil
import subprocess
import sys
from collections import OrderedDict

# function to translate shell variables to python logicals
def isTrue(str_in):
  str_in = str_in.upper()
  if str_in in ['YES','.TRUE.']:
    status = True
  else:
    status = False

  return status

# function to check if path exists and if not, symlink it
def link_file(from_file, to_file):
  if not os.path.exists(to_file):
    if not os.path.islink(to_file):
      os.symlink(from_file, to_file)

# run the function if this script is called from the command line
if __name__ == '__main__':
