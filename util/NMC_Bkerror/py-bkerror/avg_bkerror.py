from bkerror import bkerror
import numpy as np

nmon = 0
for mon in ['jan','feb','mar','apr','may','june','july','aug','sept','oct','nov','dec']:
    filename = '../../../../../staticB/24h/global_berror.l127y770.f77_%ssmooth0p5' % (mon,)
    print nmon,filename
    nsig,nlat,nlon = bkerror.get_header(filename)
    ivar,agvin,bgvin,wgvin,corzin,hscalesin,vscalesin,corq2in,corsstin,hsstin,corpin,hscalespin = bkerror.get_bkerror(filename,nsig,nlat,nlon)
    if not nmon:
        print 'initalize arrays'
        agvout = np.zeros(agvin.shape, agvin.dtype)
        bgvout = np.zeros(bgvin.shape, bgvin.dtype)
        wgvout = np.zeros(wgvin.shape, wgvin.dtype)
        corzout = np.zeros(corzin.shape, corzin.dtype)
        hscalesout = np.zeros(hscalesin.shape, hscalesin.dtype)
        vscalesout = np.zeros(vscalesin.shape, vscalesin.dtype)
        corq2out = np.zeros(corq2in.shape, corq2in.dtype)
        corsstout = np.zeros(corsstin.shape, corsstin.dtype)
        hsstout = np.zeros(hsstin.shape, hsstin.dtype)
        corpout = np.zeros(corpin.shape, corpin.dtype)
        hscalespout = np.zeros(hscalespin.shape, hscalespin.dtype)
    agvout += agvin/12.
    bgvout += bgvin/12.
    wgvout += wgvin/12.
    corzout += corzin/12.
    hscalesout += hscalesin/12.
    vscalesout += vscalesin/12.
    corq2out += corq2in/12.
    corsstout += corsstin/12.
    hsstout += hsstin/12.
    corpout += corpin/12.
    hscalespout += hscalespin/12.
    nmon += 1

filename = '../../../../../staticB/24h/global_berror.l127y770.f77_annmeansmooth0p5'
print filename
bkerror.put_bkerror(filename,ivar,\
        agvout,bgvout,wgvout,\
        corzout,hscalesout,vscalesout,\
        corq2out,corsstout,hsstout,corpout,hscalespout)

