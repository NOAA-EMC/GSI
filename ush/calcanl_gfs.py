nf#!/usr/bin/env python
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
def calcanl_gfs(DoIAU, l4DEnsVar, Write4Danl, ComOut, APrefix, ASuffix, 
                FixDir, atmges_ens_mean, RunDir, NThreads, NEMSGet, IAUHrs, 
                ExecCMD, ExecCMDMPI, ExecAnl, ExecChgresGes, ExecChgresInc):

  ######## copy and link files
  if DoIAU and l4DEnsVar and Write4Danl:
    for fh in IAUHrs:
      if fh == 6:
        # for archiving
        shutil.copy('siginc.nc', ComOut+'/'+APrefix+'atminc.nc')
        gsi_utils.link_file(ComOut+'/'+APrefix+'atmanl.ensres'+ASuffix, 'anl.ensres.06')
        # for calc_analysis
        gsi_utils.link_file('siginc.nc', 'siginc.nc.06')
        gsi_utils.link_file('sigf06', 'ges.06')
        gsi_utils.link_file('siganl', 'anl.06')
      else:
        if os.path.isfile('sigi'+format(fh, '02')+'.nc'):
          # for archiving
          shutil.copy('sigi'+format(fh, '02')+'.nc', ComOut+'/'+APrefix+'atmi'+format(fh, '03')+'.nc')
          gsi_utils.link_file(ComOut+'/'+APrefix+'atma'+format(fh, '03')+'.ensres'+ASuffix, 'anl.ensres.'+format(fh, '02'))
          gsi_utils.link_file(ComOut+'/'+APrefix+'atma'+format(fh, '03')+ASuffix, 'anl.'+format(fh, '02'))
          # for calc_analysis
          gsi_utils.link_file('sigi'+format(fh, '02')+'.nc', 'siginc.nc.'+format(fh, '02'))
          gsi_utils.link_file('sigf'+format(fh, '02'), 'ges.'+format(fh, '02'))
          gsi_utils.link_file('siga'+format(fh, '02'), 'anl.'+format(fh, '02'))
  else:
    # for archiving
    shutil.copy('siginc.nc', ComOut+'/'+APrefix+'atminc.nc')
    gsi_utils.link_file(ComOut+'/'+APrefix+'atmanl.ensres'+ASuffix, 'anl.ensres.06')
    # for calc_analysis
    gsi_utils.link_file('siginc.nc', 'siginc.nc.06')
    gsi_utils.link_file('sigf06', 'ges.06')
    gsi_utils.link_file('siganl', 'anl.06')

  shutil.copy(ExecChgresGes, RunDir+'/chgres_ges.x')
  shutil.copy(ExecChgresInc, RunDir+'/chgres_inc.x')
  shutil.copy(ExecAnl, RunDir+'/calc_anl.x')

  # determine if the analysis is to be written in netCDF or NEMSIO
  if ASuffix == ".nc":
     nemsanl = ".false."
  else:
     nemsanl = ".true."

  ######## get dimension information from background and increment files
  AnlDims = gsi_utils.get_ncdims('siginc.nc')
  if ASuffix == ".nc":
    GesDims = gsi_utils.get_ncdims('sigf06') 
  else:
    GesDims = gsi_utils.get_nemsdims('sigf06',NEMSGet)

  levs = AnlDims['lev']
  LonA = AnlDims['lon']
  LatA = AnlDims['lat']
  LonB = GesDims['grid_xt']
  LatB = GesDims['grid_yt']

  # vertical coordinate info
  # NOTE I Don't know why this file is 128 when it should be 127, perhaps a mislabeled filename?
  if levs == 127:
    levs2 = 128
  else:
    levs2 = levs 
  siglevel = FixDir+'/global_hyblev.l'+str(levs2)+'.txt'

  ######## interpolate increment to full background resolution
  nFH=0
  ExecCMD = ExecCMD.replace("$ncmd","1")
  for fh in IAUHrs:
    # first check to see if increment file exists
    if (os.path.isfile('siginc.nc.'+format(fh, '02'))):
      nFH+=1
      # set up the namelist
      namelist = OrderedDict()
      namelist["setup"] = {"lon_out": LonB,
                           "lat_out": LatB,
                           "lev": levs,
                           "infile": "'siginc.nc."+format(fh, '02')+"'",
                           "outfile": "'inc.fullres."+format(fh, '02')+"'",
                         }
      gsi_utils.write_nml(namelist, RunDir+'/fort.43')

      # run the executable
      try:
        print('interp_inc',namelist)
        err = subprocess.check_call(ExecCMD+' '+RunDir+'/chgres_inc.x', shell=True)
      except subprocess.CalledProcessError as e:
        print('Error with chgres_inc.x, exit code='+str(e.returncode))
        print(locals())
        sys.exit(e.returncode)

  ######## generate analysis from interpolated increment
  os.environ['OMP_NUM_THREADS'] = str(NThreads)
  os.environ['ncmd'] = str(nFH)
  ExecCMDMPI = ExecCMDMPI.replace("$ncmd",str(nFH))

  # set up the namelist
  namelist = OrderedDict()
  namelist["setup"] =  {"datapath": "'./'",
                        "analysis_filename": "'anl'",
                        "firstguess_filename": "'ges'",
                        "increment_filename": "'inc.fullres'",
                        "nhrs_assim": nFH,
                        "use_nemsio_anl": nemsanl,
                       }
  
  gsi_utils.write_nml(namelist, RunDir+'/calc_analysis.nml')

  # run the executable
  try:
    print('fullres_calc_anl',namelist)
    err = subprocess.check_call(ExecCMDMPI+' '+RunDir+'/calc_anl.x', shell=True)
  except subprocess.CalledProcessError as e:
    print('Error with calc_anl.x, exit code='+str(e.returncode))
    print(locals())
    sys.exit(e.returncode)

  ######## run chgres to get background on ensemble resolution
  for fh in IAUHrs:
    # first check to see if increment file exists
    if (os.path.isfile('siginc.nc.'+format(fh, '02'))):
      # set up the namelist
      namelist = OrderedDict()
      namelist["chgres_setup"] =  {"i_output": str(LonA),
                                   "j_output": str(LatA),
                                   "input_file": "'ges."+format(fh, '02')+"'",
                                   "output_file": "'ges.ensres."+format(fh, '02')+"'",
                                   "terrain_file": "'"+atmges_ens_mean+"'",
                                   "vcoord_file": "'"+siglevel+"'",
                                  }
      
      gsi_utils.write_nml(namelist, RunDir+'/chgres_nc_gauss.nml')
    
      # run the executable
      try:
        print('chgres_ges',namelist)
        err = subprocess.check_call(ExecCMD+' '+RunDir+'/chgres_ges.x', shell=True)
      except subprocess.CalledProcessError as e:
        print('Error with chgres_ges.x, exit code='+str(e.returncode))
        print(locals())
        sys.exit(e.returncode)

  ######## generate ensres analysis from interpolated background

  # set up the namelist
  namelist = OrderedDict()
  namelist["setup"] =  {"datapath": "'./'",
                        "analysis_filename": "'anl.ensres'",
                        "firstguess_filename": "'ges.ensres'",
                        "increment_filename": "'siginc.nc'",
                        "nhrs_assim": nFH,
                        "use_nemsio_anl": nemsanl,
                       }

  
  gsi_utils.write_nml(namelist, RunDir+'/calc_analysis.nml')

  # run the executable
  try:
    print('ensres_calc_anl:',namelist)
    err = subprocess.check_call(ExecCMDMPI+' '+RunDir+'/calc_anl.x', shell=True)
    print(locals())
  except subprocess.CalledProcessError as e:
    print('Error with calc_anl.x, exit code='+str(e.returncode))
    print(locals())
    sys.exit(e.returncode)


# run the function if this script is called from the command line
if __name__ == '__main__':
  DoIAU = gsi_utils.isTrue(os.getenv('DOIAU', 'NO'))
  l4DEnsVar = gsi_utils.isTrue(os.getenv('l4densvar', 'NO'))
  Write4Danl = gsi_utils.isTrue(os.getenv('lwrite4danl', 'NO'))
  ComOut = os.getenv('COMOUT', './')
  APrefix = os.getenv('APREFIX', '')
  ASuffix= os.getenv('ASUFFIX', '')
  NThreads = os.getenv('NTHREADS_CHGRES', 1)
  FixDir = os.getenv('FIXgsm', './')
  atmges_ens_mean = os.getenv('ATMGES_ENSMEAN', './atmges_ensmean')
  RunDir = os.getenv('DATA', './')
  ExecCMD = os.getenv('APRUN_CALCANL', '')
  ExecCMDMPI = os.getenv('APRUN_CALCINC', '')
  ExecAnl = os.getenv('CALCANLEXEC', './calc_analysis.x')
  ExecChgresGes = os.getenv('CHGRESNCEXEC', './chgres_nc_gauss.exe')
  ExecChgresInc = os.getenv('CHGRESINCEXEC', './chgres_increment.exe')
  NEMSGet = os.getenv('NEMSIOGET','nemsio_get')
  IAUHrs = map(int,os.getenv('IAUFHRS','6').split(',')) 

  print(locals())
  calcanl_gfs(DoIAU, l4DEnsVar, Write4Danl, ComOut, APrefix, ASuffix, 
              FixDir, atmges_ens_mean, RunDir, NThreads, NEMSGet, IAUHrs, 
              ExecCMD, ExecCMDMPI, ExecAnl, ExecChgresGes, ExecChgresInc)
