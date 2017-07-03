#!/usr/bin/env python

import sys
import numpy as np
from scipy.interpolate import interp1d, interp2d
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from bkerror import bkerror
from splat import splat

# bkerror file to read; e.g. global_berror.l64y258.f77
parser = ArgumentParser(description='read and interpolate background error file',formatter_class=ArgumentDefaultsHelpFormatter)
parser.add_argument('-f','--filename',help='background error file to read',type=str,required=True)
parser.add_argument('-i','--imax',help='interpolate to imax',type=int,required=False,default=None)
parser.add_argument('-j','--jmax',help='interpolate to jmax',type=int,required=False,default=None)
args = parser.parse_args()

filename = args.filename
jmax = args.jmax
imax = args.imax

interp_kind = 'cubic'

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

if imax is None or jmax is None:
    sys.exit(0)

ssig = np.arange(nsig)
ssig2 = np.arange(nsig*nsig)
ssig3 = np.arange(nsig*6)

idrt = 4
slat,wlat = splat(idrt,nlat)
rlat = 180. / np.arccos(-1.) * np.arcsin(slat[::-1])
slon = np.linspace(0.,360.,nlon,endpoint=False)

nlon_n = imax
nlat_n = jmax
slat_n,wlat_n = splat(idrt,nlat_n)
rlat_n = 180. / np.arccos(-1.) * np.arcsin(slat_n[::-1])
slon_n = np.linspace(0.,360.,nlon_n,endpoint=False)

print 'Interpolate from %d to %d' % (nlat, nlat_n)

tmp = agvin.reshape(nlat,-1)
f = interp2d(ssig2,slat,tmp,kind=interp_kind)
tmp_n = f(ssig2,slat_n)
agvin_n = np.array(tmp_n.reshape(nlat_n,nsig,nsig),order='F',dtype=np.float32)

f = interp2d(ssig,slat,bgvin,kind=interp_kind)
tmp_n = f(ssig,slat_n)
bgvin_n = np.array(tmp_n,order='F',dtype=np.float32)

f = interp2d(ssig,slat,wgvin,kind=interp_kind)
wgvin_n = f(ssig,slat_n)
bgvin_n = np.array(tmp_n,order='F',dtype=np.float32)

tmp = corzin.reshape(nlat,-1)
f = interp2d(ssig3,slat,tmp,kind=interp_kind)
tmp_n = f(ssig3,slat_n)
corzin_n = np.array(tmp_n.reshape(nlat_n,nsig,6),order='F',dtype=np.float32)

tmp = hscalesin.reshape(nlat,-1)
f = interp2d(ssig3,slat,tmp,kind=interp_kind)
tmp_n = f(ssig3,slat_n)
hscalesin_n = np.array(tmp_n.reshape(nlat_n,nsig,6),order='F',dtype=np.float32)

tmp = vscalesin.reshape(nlat,-1)
f = interp2d(ssig3,slat,tmp,kind=interp_kind)
tmp_n = f(ssig3,slat_n)
vscalesin_n = np.array(tmp_n.reshape(nlat_n,nsig,6),order='F',dtype=np.float32)

f = interp2d(ssig,slat,corq2in,kind=interp_kind)
tmp_n = f(ssig,slat_n)
corq2in_n = np.array(tmp_n,order='F',dtype=np.float32)

f = interp2d(slon,slat,corsstin,kind=interp_kind)
tmp_n = f(slon_n,slat_n)
corsstin_n = np.array(tmp_n,order='F',dtype=np.float32)

f = interp2d(slon,slat,hsstin,kind=interp_kind)
tmp_n = f(slon_n,slat_n)
hsstin_n = np.array(tmp_n,order='F',dtype=np.float32)

f = interp1d(slat,corpin,kind=interp_kind)
tmp_n = f(slat_n)
corpin_n = np.array(tmp_n,order='F',dtype=np.float32)

f = interp1d(slat,hscalespin,kind=interp_kind)
tmp_n = f(slat_n)
hscalespin_n = np.array(tmp_n,order='F',dtype=np.float32)

# Print some info about the interpolated data
print 'nsig = %d, nlat = %d, nlon = %d, nvar = %d' % (nsig,nlat_n,nlon_n,len(var))
print 'agvin_n.shape: ', agvin_n.shape
print 'bgvin_n.shape: ', bgvin_n.shape
print 'wgvin_n.shape: ', wgvin_n.shape
print 'corzin_n.shape: ', corzin_n.shape
print 'hscalesin_n.shape: ', hscalesin_n.shape
print 'vscalesin_n.shape: ', vscalesin_n.shape
print 'corq2in_n.shape: ', corq2in_n.shape
print 'corsstin_n.shape: ', corsstin_n.shape
print 'hsstin_n.shape: ', hsstin_n.shape
print 'corpin_n.shape: ', corpin_n.shape
print 'hscalespin_n.shape: ', hscalespin_n.shape

fname = 'berror_stats'
bkerror.put_bkerror(fname,ivar,\
        agvin_n,bgvin_n,wgvin_n,\
        corzin_n,hscalesin_n,vscalesin_n,\
        corq2in_n,corsstin_n,hsstin_n,corpin_n,hscalespin_n)

# check data in berror_stats
nsig,nlat,nlon = bkerror.get_header(fname)
ivar,agvin,bgvin,wgvin,corzin,hscalesin,vscalesin,corq2in,corsstin,hsstin,corpin,hscalespin = bkerror.get_bkerror(fname,nsig,nlat,nlon)
var = (ivar.tostring()).replace('\x00','')[:-1].split('|')

# Print some info about the file
print 'nsig = %d, nlat = %d, nlon = %d, nvar = %d' % (nsig,nlat,nlon,len(var))
print 'variables in %s' % fname
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

print 'differences'
print np.abs(agvin-agvin_n).max()
print np.abs(bgvin-bgvin_n).max()
print np.abs(wgvin-wgvin_n).max()
print np.abs(hscalesin-hscalesin_n).max()
