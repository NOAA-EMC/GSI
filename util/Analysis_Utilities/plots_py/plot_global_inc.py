#!/usr/bin/env python
import argparse
import netCDF4 as nc
import matplotlib
matplotlib.use('agg')
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import numpy as np

def plot_2d_inc(inpath, varname, lev, plotpath):
  # read in the increment array
  ncf = nc.Dataset(inpath)
  incvar = ncf.variables[varname][:]
  inclev = incvar[lev,...]
  # calculate min/max for plotting
  minval = np.nanmin(inclev)
  maxval = np.nanmax(inclev)
  prange = max([minval,maxval], key=abs)
  # set up map/figure
  ax = plt.axes(projection=ccrs.PlateCarree())
  ax.coastlines(resolution='10m')
  # get lat lon values
  lat = ncf.variables['lat'][:]
  lon = ncf.variables['lon'][:]
  lons, lats = np.meshgrid(lon,lat)
  plt.pcolormesh(lons, lats, inclev, vmin=-prange, vmax=prange, cmap='bwr')
  plt.colorbar(orientation='horizontal',label='minval='+'{:.4f}'.format(minval)+'    maxval='+'{:.4f}'.format(maxval))
  plt.title(inpath+'\n'+varname+' Analysis Increment Lev='+str(lev+1))
  plt.savefig(plotpath)

if __name__ == '__main__':
  parser = argparse.ArgumentParser(description='Plot Global GSI Analysis Increments')
  parser.add_argument('-i', '--input', help='path to input netCDF increment file (ex: /path/to/siginc.nc',
                      type=str, required=True)
  parser.add_argument('-v', '--var', help='name of variable (ex: T_inc)', type=str, required=True)
  parser.add_argument('-l', '--lev', help='model level to plot (ex: 127)', type=int, required=True)
  parser.add_argument('-p', '--plot', help='path to output figure (ex: /path/to/plot.png', type=str, required=True)
  args = parser.parse_args()

  plot_2d_inc(args.input, args.var, args.lev-1, args.plot)
