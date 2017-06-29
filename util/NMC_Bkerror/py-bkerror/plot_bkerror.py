#!/usr/bin/env python

import sys
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from matplotlib import pyplot as plt
import numpy as np
from bkerror import bkerror
from splat import splat
try:
    plot_map = True
    import lib_mapping as lmapping
except:
    plot_map = False
    print 'lib_mapping module is not in your path'
    print 'No maps will be produced'

# bkerror file to read; e.g. global_berror.l64y258.f77
parser = ArgumentParser(description='read background error file and plot',formatter_class=ArgumentDefaultsHelpFormatter)
parser.add_argument('-f','--filename',help='background error file to read and plot',type=str,required=True)
args = parser.parse_args()

filename = args.filename

nsig,nlat,nlon = bkerror.get_header(filename)
ivar,agvin,bgvin,wgvin,corzin,hscalesin,vscalesin,corq2in,corsstin,hsstin,corpin,hscalespin = bkerror.get_bkerror(filename,nsig,nlat,nlon)
var = (ivar.tostring()).replace('\x00','')[:-1].split('|')

# Print some info about the file
print 'nsig = %d, nlat = %d, nlon = %d, nvar = %d' % (nsig,nlat,nlon,len(var))
print 'variables in %s' % filename
print ', '.join(var)
print 'agvin.shape: ', agvin.shape
print 'bgvin.shape: ', bgvin.shape
print 'wgvin.shape: ', wgvin.shape
print 'corzin.shape: ', corzin.shape
print 'hscalesin.shape: ', hscalesin.shape
print 'vscalesin.shape: ', vscalesin.shape
print 'corq2in.shape: ', corq2in.shape
print 'corsstin.shape: ', corsstin.shape
print 'hsstin.shape: ', hsstin.shape
print 'corpin.shape: ', corpin.shape
print 'hscalespin.shape: ', hscalespin.shape

idrt = 4
slat,wlat = splat(idrt,nlat)
glat = 180. / np.arccos(-1.) * np.arcsin(slat[::-1])
glon = np.linspace(0.,360.,nlon,endpoint=False)
glev = np.arange(1,nsig+1)

zg,xg = np.meshgrid(glev,glat)

cmapdiv = 'Spectral_r'
cmappos = 'Spectral_r'

aglevs = [1,10,20,40,50,60]
for lev in aglevs:
    print 'plotting agv at level = %d'  % lev
    plt.figure()
    z = agvin[:,:,lev-1]
    plt.contourf(xg,zg,z,21,vmin=-z.max(),cmap=cmapdiv,extend='both')
    plt.colorbar()
    plt.title('agv at level = %d' % lev)
    plt.savefig('agvl%02d.pdf' % lev)


print 'plotting bgv and wgv'
plt.figure()
plt.subplot(2,1,1)
plt.contourf(xg,zg,bgvin,21,vmin=-bgvin.max(),cmap=cmapdiv,extend='both')
plt.colorbar()
plt.title('bgv')
plt.subplot(2,1,2)
plt.contourf(xg,zg,wgvin,21,vmin=-wgvin.max(),cmap=cmapdiv,extend='both')
plt.colorbar()
plt.title('wgv')
plt.savefig('bgvwgv.pdf')

for i in range(6):

    varname = var[i].strip()

    print 'plotting %s'  % varname

    plt.figure()
    plt.subplot(3,1,1)
    z = corzin[:,:,i]
    plt.contourf(xg,zg,z,21,cmap=cmappos,extend='both')
    plt.colorbar()
    plt.title('corz')

    plt.subplot(3,1,2)
    z = hscalesin[:,:,i]/1000.
    plt.contourf(xg,zg,z,21,cmap=cmappos,extend='both')
    plt.colorbar()
    plt.title('horizontal scales (km)')

    plt.subplot(3,1,3)
    z = 1./vscalesin[:,:,i]
    plt.contourf(xg,zg,z,21,cmap=cmappos,extend='both')
    plt.colorbar()
    plt.title('vertical scales')

    plt.suptitle('variable = %s' % varname)
    plt.savefig('%s.pdf' % varname)

print 'plotting corq2'
plt.figure()
plt.subplot(1,1,1)
z = corq2in
plt.contourf(xg,zg,z,21,cmap=cmappos)
plt.colorbar()
plt.title('corq2')
plt.savefig('corq2.pdf')

if plot_map:
    proj = lmapping.Projection('mill',resolution='c',llcrnrlat=-80.,urcrnrlat=80.)
    bmap = lmapping.createMap(proj)
    gglon,gglat = np.meshgrid(glon,glat)
    xm,ym = bmap(gglon,gglat)

    print 'plotting sst'
    plt.figure()
    plt.subplot(2,1,1)
    lmapping.drawMap(bmap,proj)
    z = corsstin
    c = bmap.contourf(xm,ym,z,21,cmap=cmappos,extend='both')
    bmap.colorbar(c,'right',size='5%',pad='2%')
    plt.title('cor')

    plt.subplot(2,1,2)
    lmapping.drawMap(bmap,proj)
    z = hsstin
    c = bmap.contourf(xm,ym,z,21,cmap=cmappos,extend='both')
    bmap.colorbar(c,'right',size='5%',pad='2%')
    plt.title('horizontal scales (km)')

    plt.suptitle('variable = sst')
    plt.savefig('sst.pdf')

#plt.show()
sys.exit(0)
