#!/usr/bin/env python

import sys
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from matplotlib import pyplot as plt
import numpy as np
from bkerror import bkerror
from splat import splat
try:
    import lib_mapping as lmapping
    plot_map = True
except:
    print 'lib_mapping module is not in your path'
    print 'No maps will be produced'
    plot_map = False


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
parser = ArgumentParser(description='read background error file and plot',formatter_class=ArgumentDefaultsHelpFormatter)
parser.add_argument('-f','--filename',help='background error file to read and plot',type=str,required=True)
args = parser.parse_args()

gsi = GSIbkgerr(args.filename)
gsi.print_summary()

idrt = 4
slat,wlat = splat(idrt,gsi.nlat)
glat = 180. / np.arccos(-1.) * np.arcsin(slat[::-1])
glon = np.linspace(0.,360.,gsi.nlon,endpoint=False)
glev = np.arange(1,gsi.nsig+1)

zg,xg = np.meshgrid(glev,glat)

cmapdiv = 'Spectral_r'
cmappos = 'Spectral_r'

aglevs = [1,10,20,40,50,60]
for lev in aglevs:
    print 'plotting agv at level = %d'  % lev
    plt.figure()
    z = gsi.agvin[:,:,lev-1]
    plt.contourf(xg,zg,z,21,vmin=-z.max(),cmap=cmapdiv,extend='both')
    plt.colorbar()
    plt.title('agv at level = %d' % lev,fontsize=12,fontweight='normal')
    plt.savefig('agvl%02d.pdf' % lev)


print 'plotting bgv and wgv'
plt.figure()
plt.subplot(2,1,1)
plt.contourf(xg,zg,gsi.bgvin,21,vmin=-gsi.bgvin.max(),cmap=cmapdiv,extend='both')
plt.colorbar()
plt.title('bgv',fontsize=12,fontweight='normal')
plt.subplot(2,1,2)
plt.contourf(xg,zg,gsi.wgvin,21,vmin=-gsi.wgvin.max(),cmap=cmapdiv,extend='both')
plt.colorbar()
plt.title('wgv',fontsize=12,fontweight='normal')
plt.savefig('bgvwgv.pdf')

for i in range(6):

    varname = gsi.var[i].strip()

    print 'plotting %s'  % varname

    plt.figure()
    plt.subplot(3,1,1)
    z = gsi.corzin[:,:,i]
    plt.contourf(xg,zg,z,21,cmap=cmappos,extend='both')
    plt.colorbar()
    plt.title('correlation',fontsize=12,fontweight='normal')

    plt.subplot(3,1,2)
    z = gsi.hscalesin[:,:,i]/1000.
    plt.contourf(xg,zg,z,21,cmap=cmappos,extend='both')
    plt.colorbar()
    plt.title('horizontal scales (km)',fontsize=12,fontweight='normal')

    plt.subplot(3,1,3)
    z = 1./gsi.vscalesin[:,:,i]
    plt.contourf(xg,zg,z,21,cmap=cmappos,extend='both')
    plt.colorbar()
    plt.title('vertical scales',fontsize=12,fontweight='normal')

    plt.suptitle('variable = %s' % varname,fontsize=14,fontweight='bold')
    plt.savefig('%s.pdf' % varname)

print 'plotting corq2'
plt.figure()
plt.subplot(1,1,1)
z = gsi.corq2in
plt.contourf(xg,zg,z,21,cmap=cmappos)
plt.colorbar()
plt.title('corq2',fontsize=12,fontweight='normal')
plt.savefig('corq2.pdf')

print 'plotting surface pressure'
plt.figure()
plt.subplot(1,2,1)
plt.plot(glat,gsi.corpin,'b.')
plt.plot(glat,gsi.corpin,'b-')
plt.xlabel('latitude')
plt.xlim(-90,90)
plt.ylabel('correlation',fontsize=12,fontweight='normal')
plt.title('correlation',fontsize=12,fontweight='normal')
plt.subplot(1,2,2)
plt.plot(glat,gsi.hscalespin/1000.,'r-')
plt.plot(glat,gsi.hscalespin/1000.,'r.')
plt.xlabel('latitude')
plt.xlim(-90,90)
plt.ylabel('horizontal scales (km)',fontsize=12,fontweight='normal')
plt.title('horizontal scales (km)',fontsize=12,fontweight='normal')

plt.suptitle('variable = ps',fontsize=14,fontweight='bold')
plt.savefig('ps.pdf')

if plot_map:
    proj = lmapping.Projection('mill',resolution='c',llcrnrlat=-80.,urcrnrlat=80.)
    bmap = lmapping.createMap(proj)
    gglon,gglat = np.meshgrid(glon,glat)
    xm,ym = bmap(gglon,gglat)

    print 'plotting sst'
    plt.figure()
    plt.subplot(2,1,1)
    lmapping.drawMap(bmap,proj)
    z = gsi.corsstin
    c = bmap.contourf(xm,ym,z,21,cmap=cmappos,extend='both')
    bmap.colorbar(c,'right',size='5%',pad='2%')
    plt.title('correlation',fontsize=12,fontweight='normal')

    plt.subplot(2,1,2)
    lmapping.drawMap(bmap,proj)
    z = gsi.hsstin
    c = bmap.contourf(xm,ym,z,21,cmap=cmappos,extend='both')
    bmap.colorbar(c,'right',size='5%',pad='2%')
    plt.title('horizontal scales (km)',fontsize=12,fontweight='normal')

    plt.suptitle('variable = sst',fontsize=14,fontweight='bold')
    plt.savefig('sst.pdf')

#plt.show()
sys.exit(0)
