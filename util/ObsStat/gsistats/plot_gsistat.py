#!/usr/bin/env python

###############################################################
# < next few lines under version control, D O  N O T  E D I T >
# $Date$
# $Revision$
# $Author$
# $Id$
###############################################################

import sys
import numpy as np
import matplotlib
from matplotlib import pyplot as plt
from matplotlib import rcParams, ticker
from matplotlib import gridspec as gspec
from datetime import datetime
from argparse import ArgumentParser,ArgumentDefaultsHelpFormatter

########## F U N C T I O N S ##########

def read_gsistat(filename,varname):
    if   ( varname == 'ps' ):
        data   = np.loadtxt(filename,usecols=(0,6,8))
        dates  = np.array([datetime.strptime(str(d),'%Y%m%d%H') for d in data[:,0].astype(np.int)])
        counts = data[:,1].astype(np.int).sum(axis=0)
        rmses  = data[:,2].astype(np.float).mean(axis=0)
    elif ( varname == 'oz' ):
        data   = np.loadtxt(filename,usecols=(0,4))
        dates  = np.array([datetime.strptime(str(d),'%Y%m%d%H') for d in data[:,0].astype(np.int)])
        counts = np.NaN
        rmses  = data[:,1].astype(np.float).mean(axis=0)
    else:
        data   = np.loadtxt(filename,usecols=(0,7,8,9,10,11,12,13,14,15,16,17,18))
        dates  = np.array([datetime.strptime(str(d),'%Y%m%d%H') for d in data[::2,0].astype(np.int)])
        counts = data[ ::2,1:].astype(np.int).sum(axis=0)
        rmses  = data[1::2,1:].astype(np.float).mean(axis=0)

    return dates,counts,rmses

def check_dates(dates1,dates2):
    for date1,date2 in zip(dates1,dates2):
        if ( date1 != date2 ): return True
    return False

def main():

    parser = ArgumentParser(description = 'Comparison plot of GSI statistics',formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('-x','--expid',help='list of experiment IDs',nargs='+',required=True)
    parser.add_argument('-l','--label',help='list of labels for experiment IDs',nargs='+',required=False)
    parser.add_argument('-s','--save_figure',help='save figures as png and pdf',action='store_true',required=False)
    args = parser.parse_args()

    # experiments to compare:
    expids = args.expid
    if ( args.label == None ):
        labels = expids
    else:
        labels = expids if ( len(args.label) != len(expids) ) else args.label
#    labels = expids if ( args.label == None ) else expids if ( len(args.label) != len(expids) ) else args.label
    save_figure = args.save_figure
    if ( save_figure ): matplotlib.use('Agg')

    ########## N O  N E E D  T O  M O D I F Y  B E L O W ##########

    # Line/marker colors for experiments ('k' is the first)
    mc = ['k', 'r', 'g', 'b', 'm','c','y']

    # set figure params one time only.
    rcParams['figure.subplot.left'] = 0.1
    rcParams['figure.subplot.top']  = 0.85
    rcParams['legend.fontsize']     = 12
    rcParams['axes.grid']           = True

    dates  = {} ; counts = {} ; rmses  = {}
    vars = ['ps','uv','t','q','oz']
    for expid in expids:
        dates[expid]  = {} ; counts[expid] = {} ; rmses[expid]  = {}
        for var in vars:
            dates[expid][var]  = [] ; counts[expid][var] = [] ; rmses[expid][var]  = []
            filename = './data/%s/gsistat_%s.out' % (expid,var)
            dates[expid][var],counts[expid][var],rmses[expid][var] = read_gsistat(filename,var)

    for expid in expids[1:]:
        for var in vars:
            err = check_dates(dates[expids[0]][var],dates[expid][var])
            if ( err ):
                print 'dates mismatch between %s and %s' %(expids[0],expid)
                sys.exit(1)

    fig1 = plt.figure(figsize=(10,8))
    plt.subplots_adjust(hspace=0.3)
    gs = gspec.GridSpec(1,3)

    levs = [1000, 900, 800, 600, 400, 300, 250, 200, 150, 100, 50, 0]

    for v, var in  enumerate(['uv','t','q']):

        xmin = 999
        xmax = 0
        ax = plt.subplot(gs[v])
        for e,expid in enumerate(expids):
            profile = rmses[expid][var][:-1]
            ax.plot(profile,levs[:-1],marker='o',color=mc[e],mfc=mc[e],mec=mc[e],label=labels[e])
            if ( var in ['q'] ):
                xmin_,xmax_ = np.min(profile[:-1]),np.max(profile[:-1])
            else:
                xmin_,xmax_ = np.min(profile),np.max(profile)
            if ( xmin_ < xmin ): xmin = xmin_
            if ( xmax_ > xmax ): xmax = xmax_
        if ( v in [0] ): plt.legend(loc=0,numpoints=1)
        if ( v in [0] ): plt.ylabel('pressure (hPa)')

        if ( var == 'uv' ):
            var_unit = 'm/s'
            var_name = 'Winds'
        elif ( var == 't' ):
            var_unit = 'K'
            var_name = 'Temperature'
        elif ( var == 'q' ):
            var_unit = 'frac'
            var_name = 'normalized Sp. Humidity'

        plt.xlabel('magnitude (%s)' % var_unit)
        plt.title(var_name,fontsize=14)
        plt.ylim(1020,95)
        ax.set_yscale('log')
        ax.yaxis.set_major_locator(ticker.LogLocator(base=10.0,subs=np.arange(1,10)))
        ax.yaxis.set_major_formatter(ticker.FormatStrFormatter("%g"))
        xmin = xmin - (xmax-xmin)*0.1
        xmax = xmax + (xmax-xmin)*0.1
        plt.xlim(xmin,xmax)

    sdatestr = dates[expids[0]]['uv'][ 0].strftime('%Y%m%d%H')
    edatestr = dates[expids[0]]['uv'][-1].strftime('%Y%m%d%H')
    plt.figtext(0.5,0.93,'RMSE O-F (%s-%s)'%(sdatestr,edatestr),horizontalalignment='center',fontsize=18)

    if ( save_figure ):
        fname = 'gsistat_uvtq'
        plt.savefig('./%s.pdf'%(fname))
        plt.savefig('./%s.png'%(fname))

    # Now handle PS and Oz
    rcParams['axes.grid'] = False

    width = 0.8
    index = np.arange(1,len(expids)+1) - width/2.0

    fig2 = plt.figure(figsize=(10,8))
    plt.subplots_adjust(hspace=0.3)
    gs = gspec.GridSpec(1,2)

    for v, var in  enumerate(['ps','oz']):

        ymin = 2.0e10
        ymax = 0
        ax = plt.subplot(gs[v])
        ax.plot(np.zeros(len(index)),'k-')
        for e,expid in enumerate(expids):
            ax.bar(index[e],rmses[expid][var],width,color=mc[e],edgecolor=mc[e],linewidth=0.0)
            if ( rmses[expid][var] < ymin ): ymin = rmses[expid][var]
            if ( rmses[expid][var] > ymax ): ymax = rmses[expid][var]

        if ( v in [0] ): plt.legend(loc=0,numpoints=1)

        if ( var == 'ps' ):
            var_unit = 'hPa'
            var_name = 'Surface Pressure RMSE'
        elif ( var == 'oz' ):
            var_unit = ''
            var_name = 'Ozone Penalty'

        newxticklocs = index + width/2.0
        plt.xticks(newxticklocs,labels)
        plt.xlim(index[0]-width/2.0,index[-1]+width*1.5)
        if ( var == 'oz' ):
            ymin = ymin - (ymax-ymin)*0.1
            ymax = ymax + (ymax-ymin)*0.1
            plt.ylim(ymin,ymax)

        plt.xlabel('experiments')
        plt.title(var_name,fontsize=14)

    sdatestr = dates[expids[0]]['uv'][ 0].strftime('%Y%m%d%H')
    edatestr = dates[expids[0]]['uv'][-1].strftime('%Y%m%d%H')
    plt.figtext(0.5,0.93,'(%s-%s)'%(sdatestr,edatestr),horizontalalignment='center',fontsize=18)

    if ( save_figure ):
        fname = 'gsistat_psoz'
        plt.savefig('./%s.pdf'%(fname))
        plt.savefig('./%s.png'%(fname))

    if ( not save_figure ): plt.show()

    sys.exit(0)

if __name__ == '__main__': main()
