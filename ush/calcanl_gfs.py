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
import gsi_utils
from collections import OrderedDict


# function to calculate analysis from a given increment file and background
def calcanl_gfs(DoIAU, l4DEnsVar, Write4Danl, ComOut, APrefix, 
                FixDir, atmges_ens_mean, RunDir, 
                ExecCMD, ExecAnl, ExecChgresGes, ExecChgresInc)

  ######## copy and link files
  if DoIAU and l4DEnsVar and Write4Danl:
    nFH=6
    for fh in range(3,10):
      if fh == 6:
        shutil.copy('siginc.nc', ComOut+'/'+APrefix+'atminc.nc')
      else
        shutil.copy('sigi'+format(fh, '02')+'.nc', ComOut+'/'+APrefix+'atmi'+format(fh, '02')+'.nc')
  else
    shutil.copy('siginc.nc', ComOut+'/'+APrefix+'atminc.nc')

  shutil.copy(ExecChgresGes, RunDir+'/chgres_ges.x')
  shutil.copy(ExecChgresInc, RunDir+'/chgres_inc.x')
  shutil.copy(ExecAnl, RunDir+'/calc_anl.x')

  # we need an analysis on the ensemble resolution as well as on the determinstic
  gsi_utils.link_file(ComOut+'/'+APrefix+'atmanl.ensres.nc', 'siganl.ensres')

  ######## get dimension information from background and increment files
  AnlDims = gsi_utils.get_ncdims('siginc.nc')
  GesDims = gsi_utils.get_ncdims('sigf06') 

  levs = AnlDims['pfull']
  LonA = AnlDims['grid_xt']
  LatA = AnlDims['grid_yt']

  # vertical coordinate info
  # NOTE I Don't know why this file is 128 when it should be 127, perhaps a mislabeled filename?
  if levs == 127 then
    levs2 = 128
  else:
    levs2 = levs 
  siglevel = FixDir+'/global_hyblev.l'+str(levs2)+'.txt'

  os.environ['OMP_NUM_THREADS'] = str(NThreads)

  ######## interpolate increment to full background resolution

  ######## generate analysis from interpolated increment
  # set up the namelist
  namelist = OrderedDict()
  namelist["setup"] =  {"datapath": "'./'",
                        "analysis_filename": "'siganl'",
                        "firstguess_filename": "'sigf06'",
                        "increment_filename": "'siginc.nc.fullres'",
                        "nhr_assim": AssimFreq,
                        "use_nemsio": ".false.")}
  
  gsi_utils.write_nml(namelist, RunDir+'/calc_analysis.nml')

  # run the executable
  try:
    err = subprocess.check_call(ExecCMD+' '+RunDir+'/calc_anl.x', shell=True)
  except subprocess.CalledProcessError as e:
    print('Error with calc_anl.x, exit code='+str(e.returncode))
    sys.exit(e.returncode)

  ######## run chgres to get background on ensemble resolution
  # set up the namelist
  namelist = OrderedDict()
  namelist["chgres_setup"] =  {"i_output": str(LonA),
                               "j_output": str(LatA),
                               "input_file": "'sigf06'",
                               "output_file": "'sigf06.ensres'",
                               "terrain_file": "'"+atmges_ens_mean+"'",
                               "vcoord_file": "'"+siglevel+"'"}
  
  gsi_utils.write_nml(namelist, RunDir+'/chgres_nc_gauss.nml')

  # run the executable
  try:
    err = subprocess.check_call(ExecCMD+' '+RunDir+'/chgres_ges.x', shell=True)
  except subprocess.CalledProcessError as e:
    print('Error with chgres_ges.x, exit code='+str(e.returncode))
    sys.exit(e.returncode)

  ######## generate ensres analysis from interpolated background

  # set up the namelist
  namelist = OrderedDict()
  namelist["setup"] =  {"datapath": "'./'",
                        "analysis_filename": "'siganl.ensres'",
                        "firstguess_filename": "'sigf06.ensres'",
                        "increment_filename": "'siginc.nc'",
                        "nhr_assim": AssimFreq,
                        "use_nemsio": ".false.")}

  
  gsi_utils.write_nml(namelist, RunDir+'/calc_analysis.nml')

  # run the executable
  try:
    err = subprocess.check_call(ExecCMD+' '+RunDir+'/calc_anl.x', shell=True)
  except subprocess.CalledProcessError as e:
    print('Error with calc_anl.x, exit code='+str(e.returncode))
    sys.exit(e.returncode)


# run the function if this script is called from the command line
if __name__ == '__main__':
  DoIAU = gsi_utils.isTrue(os.getenv('DOIAU', 'NO'))
  l4DEnsVar = gsi_utils.isTrue(os.getenv('l4densvar', 'NO'))
  Write4Danl = gsi_utils.isTrue(os.getenv('lwrite4dan', 'NO'))
  ComOut = os.getenv('COMOUT', './')
  APrefix = os.getenv('APREFIX', '')
  NThreads = os.getenv('NTHREADS_CHGRES', 1)
  FixDir = os.getenv('FIXgsm', './')
  atmges_ens_mean = os.getenv('ATMGES_ENSMEAN', './atmges_ensmean')
  RunDir = os.getenv('DATA', './')
  ExecCMD = os.getenv('APRUN_CALCINC', '')
  ExecAnl = os.getenv('CALCANLEXEC', './calc_analysis.x')
  ExecChgresGes = os.getenv('CHGRESNCEXEC', './chgres_nc_gauss.exe')
  ExecChgresInc = os.getenv('CHGRESINCEXEC', './chgres_increment.exe')
  calcanl_gfs(DoIAU, l4DEnsVar, Write4Danl, ComOut, APrefix, 
              FixDir, atmges_ens_mean, RunDir, 
              ExecCMD, ExecAnl, ExecChgresGes, ExecChgresInc)
