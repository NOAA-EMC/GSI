#!/usr/bin/env python

import sys
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from matplotlib import pyplot as plt
import numpy as np
from bkerror import bkerror
from splat import splat

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

# First plot bgvin and wgvin
plt.figure(figsize=(8,11))

plt.subplot(2,1,1)
plt.contourf(xg,zg,bgvin,cmap='jet');
plt.colorbar();
plt.title('bgvin')

plt.subplot(2,1,2)
plt.contourf(xg,zg,wgvin,cmap='jet');
plt.colorbar();
plt.title('wgvin')

plt.savefig('bgvinwgvin.jpg')

for i in range(6):

    varname = var[i].strip()

    print 'plotting variable %s'  % varname

    plt.figure(figsize=(8,11))

    plt.subplot(3,1,1)
    plt.contourf(xg,zg,corzin[:,:,i],cmap='jet');
    plt.colorbar();
    plt.title('corz')

    plt.subplot(3,1,2)
    plt.contourf(xg,zg,hscalesin[:,:,i],cmap='jet');
    plt.colorbar();
    plt.title('horizontal scales')

    plt.subplot(3,1,3)
    plt.contourf(xg,zg,vscalesin[:,:,i],cmap='jet');
    plt.colorbar();
    plt.title('vertical scales')

    plt.suptitle('variable = %s' % varname)

    plt.savefig('%s.jpg' % varname)

# Plot corq2in
plt.figure(figsize=(8,11))

plt.subplot(1,1,1)
plt.contourf(xg,zg,corq2in,cmap='jet');
plt.colorbar();
plt.title('corq2in')
plt.savefig('corq2in.jpg')

plt.show()

sys.exit(0)
