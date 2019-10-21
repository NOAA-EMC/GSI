### gsi_utils.py
###          a collection of functions, classes, etc.
###          used for the GSI global analysis 

def isTrue(str_in):
  """ isTrue(str_in)
   - function to translate shell variables to python logical variables

   input: str_in - string (should be like 'YES', 'TRUE', etc.)
   returns: status (logical True or False)

  """
  str_in = str_in.upper()
  if str_in in ['YES','.TRUE.']:
    status = True
  else:
    status = False
  return status

def link_file(from_file, to_file):
  """ link_file(from_file, to_file)
   - function to check if a path exists, and if not, make a symlink
   input: from_file - string path
          to_file   - string path
  """
  import os
  if not os.path.exists(to_file):
    if not os.path.islink(to_file):
      os.symlink(from_file, to_file)

def write_nml(nml_dict, nml_file):
  """ write_nml(nml_dict, nml_file)
   - function to write out namelist dictionary nml_dict to file nml_file
   input: nml_dict - dictionary of dictionaries
                     first dictionary is &nml, second is nmlvar='value'
                     NOTE: this shoudl be an OrderedDict or else it might fail
          nml_file - string path to write namelist file to
  """
  nfile = open(nml_file, 'w')

  for nml, nmlvars in nml_dict.items():
    nfile.write('&'+nml+'\n')
    for var, val in nmlvars.items():
      nfile.write('  '+var+' = '+val+'\n')
    nfile.write('/\n\n')
  nfile.close() 


def get_ncdims(ncfile):
  """ get_ncdims(ncfile)
   - function to return dictionary of netCDF file dimensions and their lengths
   input: ncfile - string to path to netCDF file
   output: ncdims - dictionary where key is the name of a dimension and the
                    value is the length of that dimension

               ex:  ncdims['pfull'] = 127
  """
  import netCDF4 as nc
  ncf = nc.Dataset(ncfile)
  ncdims = {}
  for d in ncf.dimensions.keys():
    ncdims[d] = int(len(ncf.dimensions[d]))
  ncf.close()
  
  return ncdims

def get_timeinfo(ncfile):
  """ get_timeinfo(ncfile)
   - function to return datetime objects of initialized time and valid time
   input: ncfile - string to path to netCDF file
   returns: inittime, validtime - datetime objects
            nfhour - integer forecast hour
  """
  import netCDF4 as nc
  import datetime as dt
  from cftime import _parse_date
  ncf = nc.Dataset(ncfile)
  time_units = ncf['time'].units
  date_str = time_units.split('since ')[1]
  initstr = '%04i%02i%02i%02i' % _parse_date(date_str)[0:4]
  inittime = dt.datetime.strptime(initstr,"%Y%m%d%H")
  nfhour = int(ncf['time'][0])
  validtime = inittime + dt.timedelta(hours=nfhour)
  ncf.close()

  return inittime, validtime, nfhour
