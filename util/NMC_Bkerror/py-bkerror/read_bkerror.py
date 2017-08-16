#!/usr/bin/env python

import sys
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from bkerror import bkerror

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
parser = ArgumentParser(description='read background error file',formatter_class=ArgumentDefaultsHelpFormatter)
parser.add_argument('-f','--filename',help='background error file to read',type=str,required=True)
args = parser.parse_args()

gsi = GSIbkgerr(args.filename)
gsi.print_summary()

sys.exit(0)
