#!/usr/bin/env python

import sys
import numpy as np
from scipy.interpolate import interp1d, interp2d
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from bkerror import bkerror
from splat import splat


class GSIbkgerr(object):
    '''
    Object containing GSI static background error information
    '''
    def __init__(self,filename):
        '''
        Read and store GSI background error file.
        '''
        nsig,nlat,nlon = bkerror.get_header(filename)
        ivar,agvin,bgvin,wgvin,corzin,hscalesin,vscalesin,corq2in,corsstin,hsstin,corpin,hscalespin = bkerror.get_bkerror(filename,nsig,nlat,nlon)
        var = (ivar.tostring()).replace('\x00','')[:-1].split('|')

        self.filename = filename

        self.nsig = nsig
        self.nlat = nlat
        self.nlon = nlon

        self.ivar = ivar
        self.var = var

        self.agvin = agvin
        self.bgvin = bgvin
        self.wgvin = wgvin
        self.corzin = corzin
        self.hscalesin = hscalesin
        self.vscalesin = vscalesin
        self.corq2in = corq2in
        self.corsstin = corsstin
        self.hsstin = hsstin
        self.corpin = corpin
        self.hscalespin = hscalespin

        return


    def print_summary(self):
        '''
        Print a summary of the GSI background error file
        '''

        print
        print 'file = %s' % self.filename
        print 'nsig = %d, nlat = %d, nlon = %d, nvar = %d' % (self.nsig,self.nlat,self.nlon,len(self.var))
        print 'variables = %s' % ', '.join(self.var)
        print 'agv.shape: ', self.agvin.shape
        print 'bgv.shape: ', self.bgvin.shape
        print 'wgv.shape: ', self.wgvin.shape
        print 'corz.shape: ', self.corzin.shape
        print 'hscales.shape: ', self.hscalesin.shape
        print 'vscales.shape: ', self.vscalesin.shape
        print 'corq2.shape: ', self.corq2in.shape
        print 'corsst.shape: ', self.corsstin.shape
        print 'hsst.shape: ', self.hsstin.shape
        print 'corp.shape: ', self.corpin.shape
        print 'hscalesp.shape: ', self.hscalespin.shape
        print

        return


# bkerror file to read; e.g. global_berror.l64y258.f77
parser = ArgumentParser(description='read and interpolate background error file',formatter_class=ArgumentDefaultsHelpFormatter)
parser.add_argument('--filename',help='background error file to read',type=str,required=True)
parser.add_argument('--imax',help='interpolate to imax',type=int,required=False,default=None)
parser.add_argument('--jmax',help='interpolate to jmax',type=int,required=False,default=None)
parser.add_argument('--interpkind',help='interpolation kind ',type=str,required=False,default='cubic',choices=['linear','cubic'])
args = parser.parse_args()

jmax = args.jmax
imax = args.imax
interp_kind = args.interpkind

# Read and print some info about the file we are reading from
gsi = GSIbkgerr(args.filename)
gsi.print_summary()

if imax is None or jmax is None:
    sys.exit(0)

# Read in the same file for interpolation
gsi_n = GSIbkgerr(args.filename)
gsi_n.filename = 'berror_stats'

ssig = np.arange(gsi.nsig)
ssig2 = np.arange(gsi.nsig*gsi.nsig)
ssig3 = np.arange(gsi.nsig*6)

idrt = 4
glat,wlat = splat(idrt,gsi.nlat)
slon = np.linspace(0.,360.,gsi.nlon,endpoint=False)
slat = 180. / np.arccos(-1.) * np.arcsin(glat[::-1])

gsi_n.nlon = imax
gsi_n.nlat = jmax
glat_n,wlat_n = splat(idrt,gsi_n.nlat)
slon_n = np.linspace(0.,360.,gsi_n.nlon,endpoint=False)
slat_n = 180. / np.arccos(-1.) * np.arcsin(glat_n[::-1])

print 'Interpolate from %d to %d' % (gsi.nlat, gsi_n.nlat)

tmp = gsi.agvin.reshape(gsi.nlat,-1)
f = interp2d(ssig2,slat,tmp,kind=interp_kind)
tmp_n = f(ssig2,slat_n)
gsi_n.agvin = np.array(tmp_n.reshape(gsi_n.nlat,gsi_n.nsig,gsi_n.nsig),dtype=np.float32)

f = interp2d(ssig,slat,gsi.bgvin,kind=interp_kind)
tmp_n = f(ssig,slat_n)
gsi_n.bgvin = np.array(tmp_n,dtype=np.float32)

f = interp2d(ssig,slat,gsi.wgvin,kind=interp_kind)
tmp_n = f(ssig,slat_n)
gsi_n.wgvin = np.array(tmp_n,dtype=np.float32)

tmp = gsi.corzin.reshape(gsi.nlat,-1)
f = interp2d(ssig3,slat,tmp,kind=interp_kind)
tmp_n = f(ssig3,slat_n)
gsi_n.corzin = np.array(tmp_n.reshape(gsi_n.nlat,gsi_n.nsig,6),dtype=np.float32)

tmp = gsi.hscalesin.reshape(gsi.nlat,-1)
f = interp2d(ssig3,slat,tmp,kind=interp_kind)
tmp_n = f(ssig3,slat_n)
gsi_n.hscalesin = np.array(tmp_n.reshape(gsi_n.nlat,gsi_n.nsig,6),dtype=np.float32)

tmp = gsi.vscalesin.reshape(gsi.nlat,-1)
f = interp2d(ssig3,slat,tmp,kind=interp_kind)
tmp_n = f(ssig3,slat_n)
gsi_n.vscalesin = np.array(tmp_n.reshape(gsi_n.nlat,gsi_n.nsig,6),dtype=np.float32)

f = interp2d(ssig,slat,gsi.corq2in,kind=interp_kind)
tmp_n = f(ssig,slat_n)
gsi_n.corq2in = np.array(tmp_n,dtype=np.float32)

f = interp2d(slon,slat,gsi.corsstin,kind=interp_kind)
tmp_n = f(slon_n,slat_n)
gsi_n.corsstin = np.array(tmp_n,dtype=np.float32)

f = interp2d(slon,slat,gsi.hsstin,kind=interp_kind)
tmp_n = f(slon_n,slat_n)
gsi_n.hsstin = np.array(tmp_n,dtype=np.float32)

f = interp1d(slat.astype(np.float),gsi.corpin.astype(np.float),kind=interp_kind,fill_value="extrapolate")
tmp_n = f(slat_n)
gsi_n.corpin = np.array(tmp_n,dtype=np.float32)

f = interp1d(slat.astype(np.float),gsi.hscalespin.astype(np.float),kind=interp_kind,fill_value="extrapolate")
tmp_n = f(slat_n)
gsi_n.hscalespin = np.array(tmp_n,dtype=np.float32)

# Print some info about the interpolated data
gsi_n.print_summary()

bkerror.put_bkerror(gsi_n.filename,gsi_n.ivar,\
        gsi_n.agvin,gsi_n.bgvin,gsi_n.wgvin,\
        gsi_n.corzin,gsi_n.hscalesin,gsi_n.vscalesin,\
        gsi_n.corq2in,gsi_n.corsstin,gsi_n.hsstin,gsi_n.corpin,gsi_n.hscalespin)

# check data in berror_stats
gsi_rn = GSIbkgerr(gsi_n.filename)
gsi_rn.print_summary()

print 'differences'
print np.abs(gsi_n.agvin-gsi_rn.agvin).max()
print np.abs(gsi_n.bgvin-gsi_rn.bgvin).max()
print np.abs(gsi_n.wgvin-gsi_rn.wgvin).max()
print np.abs(gsi_n.hscalesin-gsi_rn.hscalesin).max()
