#!/usr/bin/env python

import sys
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from bkerror import bkerror

# bkerror file to read; e.g. global_berror.l64y258.f77
parser = ArgumentParser(description='read background error file',formatter_class=ArgumentDefaultsHelpFormatter)
parser.add_argument('-f','--filename',help='background error file to read',type=str,required=True)
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

sys.exit(0)
