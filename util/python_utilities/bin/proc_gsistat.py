#!/usr/bin/env python

###############################################################
# < next few lines under version control, D O  N O T  E D I T >
# $Date$
# $Revision$
# $Author$
# $Id$
###############################################################

import os
import sys
import numpy as np
import pandas as pd
from datetime import datetime
from matplotlib import ticker
from matplotlib import pyplot as plt
from matplotlib import gridspec as gspec
from argparse import ArgumentParser,ArgumentDefaultsHelpFormatter

import lib_plotting as lplotting
import lib_GSI as lgsi

def get_data(gsistat,varname,select=None,level=None):
    df = []
    for i in gsistat:
        tmp = i.extract(varname)
        if select is not None:
            tmp = tmp.xs(select,level=level,drop_level=False)
        df.append(tmp)
    df = pd.concat(df)
    return df

def get_inst_data(gsistat,instname,select=None,level=None):
    df = []
    for i in gsistat:
        tmp = i.extract_instrument('rad',instname)
        if select is not None:
            tmp = tmp.xs(select,level=level,drop_level=False)
        df.append(tmp)
    df = pd.concat(df)
    return df

def plot_ps(ps):

    fig = plt.figure(figsize=(10,8))
    ax = plt.subplot(111,frame_on=False)
    ax.xaxis.set_visible(False)
    ax.yaxis.set_visible(False)

    cell_text = []
    for e,expid in enumerate(expids):
        txt = ps[expid].mean()
        count,rms,bias = int(txt['count']),'%8.5f'%txt['rms'],'%8.5f'%txt['bias']
        cell_text.append([count,rms,bias])

    row_labels = labels
    col_labels = ['count','rms','bias']
    plt.table(cellText=cell_text,cellLoc='center',rowLabels=row_labels,colLabels=col_labels,loc='center')
    plt.title('Surface Pressure\n%s'%title_substr,fontsize='x-large',fontweight='bold')

    return fig

def plot_profile(uv,t,q,stat='rms'):

    def _collect_data(dfin,stat):

        tmpdf = []
        for e,expid in enumerate(expids):
            tmp = dfin[expid].xs(stat,level='stat',drop_level=False).mean()[:-1]
            tmp.name = expids[e]
            tmpdf.append(tmp)
        df_mean = pd.concat(tmpdf,axis=1)

        return df_mean

    uv_mean = _collect_data(uv,stat)
    t_mean  = _collect_data(t, stat)
    q_mean  = _collect_data(q, stat)

    fig = plt.figure(figsize=(10,8))
    plt.subplots_adjust(top=0.875,hspace=0.3)
    gs = gspec.GridSpec(1,3)

    lmin,lmax = 1020,95
    levs = [1000, 900, 800, 600, 400, 300, 250, 200, 150, 100, 50]

    for v,var in enumerate(['uv','t','q']):

        xmin = 1.e10
        xmax = 0
        ax = plt.subplot(gs[v])

        for e,expid in enumerate(expids):
            if var in 'uv':
                profile = uv_mean[expid].values
            elif var in 't':
                profile = t_mean[expid].values
            elif var in 'q':
                profile = q_mean[expid].values

            if stat in ['count']:
                profile = profile / 100.0

            ax.plot(profile,levs,marker='o',label=labels[e],color=mc[e],mfc=mc[e],mec=mc[e],linewidth=2.0,alpha=alpha)

            if e == 0 and stat == 'bias':
                plt.vlines(0.,lmin,lmax,colors='k',linestyles='--',linewidth=2.0,label=None)

            if ( var in ['q'] ):
                xmin_,xmax_ = np.min(profile[:-1]),np.max(profile[:-1])
            else:
                xmin_,xmax_ = np.min(profile),np.max(profile)
            if ( xmin_ < xmin ): xmin = xmin_
            if ( xmax_ > xmax ): xmax = xmax_

        if ( v in [0] ): plt.legend(loc=0,numpoints=1)

        if ( v in [0] ): plt.ylabel('pressure (hPa)',fontsize=12)

        if ( var == 'uv' ):
            var_unit = 'm/s'
            var_name = 'Winds'
        elif ( var == 't' ):
            var_unit = 'K'
            var_name = 'Temperature'
        elif ( var == 'q' ):
            var_unit = 'frac'
            var_name = 'Sp. Humidity'

        if stat in ['rms','bias']:
            plt.xlabel('magnitude (%s)' % var_unit,fontsize=12)
            plt.suptitle('%s O-F\n%s' % (stat.upper(),title_substr),fontsize='x-large',fontweight='bold')
        elif stat in ['count']:
            plt.xlabel('count (# x 100)',fontsize=12)
            plt.suptitle('Observation Counts\n%s'%title_substr,fontsize='x-large',fontweight='bold')
        plt.title(var_name,fontsize='large')
        plt.ylim(lmin,lmax)
        ax.set_yscale('log')
        if v in [0]:
            ax.yaxis.set_major_locator(ticker.LogLocator(base=10.0,subs=np.arange(1,10)))
            ax.yaxis.set_major_formatter(ticker.FormatStrFormatter("%g"))
        else:
            ax.set_yticklabels([])
        xmin = xmin - (xmax-xmin)*0.1
        xmax = xmax + (xmax-xmin)*0.1
        plt.xlim(xmin,xmax)

    return fig

def plot_cost(minim):

    # Collect all experiments into a single DataFrame
    tmpdf = []
    tmpdf2 = []
    for e,expid in enumerate(expids):
        tmp = minim[expid].groupby(level=['Outer','Inner']).mean().dropna()['J']
        tmp.name = labels[e]
        tmpdf.append(tmp)
        tmp = minim[expid]['J']
        tmp.name = labels[e]
        tmpdf2.append(tmp)

    # Scale the cost-function with 1e5
    df = pd.concat(tmpdf,axis=1) / 1.e5
    df2 = pd.concat(tmpdf2,axis=1) / 1.e5

    fig,ax = plt.subplots(figsize=(10,8))

    if len(expids) == 1:
        lc = mc[0]
    else:
        lc = mc[:len(expids)]

    # Plot the spaghetti of all dates, followed by the mean
    for idate in df2.index.get_level_values('date').unique():
        tmp = df2.xs(idate,level='date')
        tmp.plot(ax=ax,kind='line',linewidth=0.75,alpha=alpha/2.,color=lc,label=None,legend=False)
    df.plot(ax=ax,kind='line',linewidth=2.,alpha=alpha,color=lc)

    # This is needed to show the second+ outerloops with correct indices
    xticks = ax.get_xticks()
    xticklabels = [item.get_text() for item in ax.get_xticklabels()]
    for i, (xtick,xticklabel) in enumerate(zip(xticks,xticklabels)):
        tmp = str(xticklabel)
        if tmp:
            if int(tmp[1]) > 1:
                xticks[i] = xticks[i] + 1
                it = int(tmp.split(',')[-1].strip().split(')')[0]) + 1
                xticklabels[i] = u'(2, %s)' % str(it)
    ax.set_xticks(xticks[:-1])
    ax.set_xticklabels(xticklabels[:-1])
    ax.set_xlabel('Iteration',fontsize=12)
    ax.set_ylabel('Cost function (x10$^5$)',fontsize=12)

    ymin,ymax = np.min(df2.min()),np.max(df2.max())
    dy = ymax - ymin
    ymin,ymax = ymin-0.1*dy,ymax+0.1*dy
    plt.ylim(ymin,ymax)
    plt.title('Cost function reduction\n%s'%title_substr,fontsize='x-large',fontweight='bold')

    return fig

def plot_gradient(minim):

    # Collect all experiments into a single DataFrame
    tmpdf = []
    tmpdf2 = []
    for e,expid in enumerate(expids):
        tmp = minim[expid].groupby(level=['Outer','Inner']).mean().dropna()['gJ']
        tmp.name = labels[e]
        tmpdf.append(tmp)
        tmp = minim[expid]['gJ']
        tmp.name = labels[e]
        tmpdf2.append(tmp)

    # Scale the cost-function with 1e5
    df = pd.concat(tmpdf,axis=1)
    df2 = pd.concat(tmpdf2,axis=1)

    fig,ax = plt.subplots(figsize=(10,8))

    if len(expids) == 1:
        lc = mc[0]
    else:
        lc = mc[:len(expids)]

    # Plot the spaghetti of all dates, followed by the mean
    for idate in df2.index.get_level_values('date').unique():
        tmp = df2.xs(idate,level='date')
        tmp.plot(ax=ax,kind='line',logy=True,linewidth=0.75,alpha=alpha/2.,color=lc,label=None,legend=False)
    df.plot(ax=ax,kind='line',logy=True,linewidth=2.,alpha=alpha,color=lc)

    # This is needed to show the second+ outerloops with correct indices
    xticks = ax.get_xticks()
    xticklabels = [item.get_text() for item in ax.get_xticklabels()]
    for i, (xtick,xticklabel) in enumerate(zip(xticks,xticklabels)):
        tmp = str(xticklabel)
        if tmp:
            if int(tmp[1]) > 1:
                xticks[i] = xticks[i] + 1
                it = int(tmp.split(',')[-1].strip().split(')')[0]) + 1
                xticklabels[i] = u'(2, %s)' % str(it)
    ax.set_xticks(xticks[:-1])
    ax.set_xticklabels(xticklabels[:-1])
    ax.set_xlabel('Iteration',fontsize=12)
    ax.set_ylabel('Gradient of Cost function',fontsize=12)

    ymin,ymax = np.min(df2.min()),np.max(df2.max())
    dy = ymax - ymin
    ymin,ymax = ymin-0.1*dy,ymax+0.1*dy
    plt.ylim(ymin,ymax)
    plt.title('Gradient reduction\n%s'%title_substr,fontsize='x-large',fontweight='bold')

    return fig

def get_yticklabels_new(ax):

    yticklabels = ax.get_yticklabels()
    yticklabels_new = []
    instp = None
    for l,lab in enumerate(yticklabels):
        lab = str(lab.get_text())
        inst,sat = lab.replace('(','').replace(')','').split(',')
        if inst == instp:
            new_label = sat
        else:
            instp = inst
            new_label = '%s, %s' % (inst,sat)
        yticklabels_new.append(new_label.upper())

    return yticklabels_new

def plot_sat(dfin,otype=''):

    # Collect all experiments into a single DataFrame
    read,keep,assim = [],[],[]
    for e,expid in enumerate(expids):
        tmp = dfin[expid].mean(level=['instrument','satellite'])
        tmp[['read','keep','assim']] = tmp[['read','keep','assim']].astype(np.int)
        for stat in ['read','keep','assim']:
            tmp2 = tmp[stat]
            tmp2.name = labels[e]
            exec('%s.append(tmp2)'%stat)

    read = pd.concat(read,axis=1)
    keep = pd.concat(keep, axis=1)
    assim= pd.concat(assim,axis=1)

    if len(expids) == 1:
        lc = mc[0]
    else:
        lc = mc[:len(expids)]

    fig1,ax1 = plt.subplots(figsize=(10,8))
    read.plot(ax=ax1,kind='barh',logx=True,color=lc,alpha=alpha,fontsize=12,edgecolor='k',linewidth=0.0)
    titlestr = 'Read : # of %s observations\n%s' % (otype,title_substr)
    ax1.set_title(titlestr,fontsize='x-large')
    yticklabels_new = get_yticklabels_new(ax1)
    ax1.set_yticklabels(yticklabels_new,fontsize=8)

    fig2,ax2 = plt.subplots(figsize=(10,8))
    assim.plot(ax=ax2,kind='barh',logx=True,color=lc,alpha=alpha,fontsize=12,edgecolor='k',linewidth=0.0)
    titlestr = 'Assimilated: # of %s observations\n%s' % (otype,title_substr)
    ax2.set_title(titlestr,fontsize='x-large')
    yticklabels_new = get_yticklabels_new(ax2)
    ax2.set_yticklabels(yticklabels_new,fontsize=8)

    return [fig1,fig2]

def plot_channel(dfin,inst=''):

    # Collect all experiments into a single DataFrame
    assim = []
    for e,expid in enumerate(expids):
        tmp = dfin[expid].mean(level=['satellite','channel'])
        tmp[['nassim']] = tmp[['nassim']].astype(np.int)
        tmp2 = tmp['nassim']
        tmp2.name = labels[e]
        assim.append(tmp2)

    assim = pd.concat(assim,axis=1)

    if len(expids) == 1:
        lc = mc[0]
    else:
        lc = mc[:len(expids)]

    fig,ax = plt.subplots(figsize=(10,8))
    assim.plot(ax=ax,kind='barh',logx=True,width=0.9,sort_columns=True,color=lc,alpha=alpha,fontsize=12,edgecolor='k',linewidth=0.0)
    titlestr = 'Assimilated: # of %s observations\n%s' % (inst.upper(),title_substr)
    ax.set_title(titlestr,fontsize='x-large')
    yticklabels_new = get_yticklabels_new(ax)
    ax.set_yticklabels(yticklabels_new,fontsize=8)

    return fig

if __name__ == '__main__':

    global expids,labels,save_figure
    global title_substr
    global mc,alpha

    parser = ArgumentParser(description = 'Process gsistat.gdas.YYYYMMDDHH file',formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('-x','--expid',help='experiment ID',type=str,nargs='+',required=True)
    parser.add_argument('-b','--begin_date',help='beginning date',type=str,metavar='YYYYMMDDHH',required=True)
    parser.add_argument('-e','--end_date',help='ending date',type=str,metavar='YYYYMMDDHH',default=None,required=False)
    parser.add_argument('-a','--archive_dir',help='archive directory',type=str,nargs='+',required=False,default=['/da/noscrub/%s/archive'%os.environ['USER']])
    parser.add_argument('-l','--label',help='list of labels for experiment IDs',nargs='+',required=False)
    parser.add_argument('-s','--save_figure',help='save figures as png and pdf',action='store_true',required=False)
    parser.add_argument('-i','--instruments',help='list of instruments to show',nargs='+',required=False, default=None)

    args = parser.parse_args()

    expids = args.expid
    bdate = datetime.strptime(args.begin_date,'%Y%m%d%H')
    edate = bdate if args.end_date is None else datetime.strptime(args.end_date,'%Y%m%d%H')
    archdirs = args.archive_dir
    save_figure = args.save_figure
    labels = expids if args.label is None else expids if len(args.label) != len(expids) else args.label
    instruments = args.instruments

    if ( edate < bdate ):
        print 'start date cannot be after end date, switching!'
        bdate,edate = edate,bdate

    if len(expids) > 1 and len(archdirs) == 1:
        archdirs = archdirs * len(expids)

    # Collect all the objects for all expids and all dates
    gsistat, ps, uv, t, q, minim, oz, rad = {}, {}, {}, {}, {}, {}, {}, {}
    for expid,archdir in zip(expids,archdirs):

        print 'reading in data for experiment ... %s' % expid

        gsistat[expid] = []
        for adate in pd.date_range(bdate,edate,freq='6H'):
            fname = os.path.join(archdir,expid,'gsistat.gdas.%s'%adate.strftime('%Y%m%d%H'))
            if not os.path.exists(fname):
                print '\033[1;31m' + '%s does not exist' % fname + '\033[1;m'
                continue
            gsistat[expid].append(lgsi.GSIstat(fname,adate.to_datetime()))

        ps[expid] = get_data(gsistat[expid],'ps',select=[1,180,'0000'],level=['it','type','stype'])
        uv[expid] = get_data(gsistat[expid],'uv',select=[1,220],level=['it','type'])
        t[expid] = get_data(gsistat[expid],'t', select=[1,120],level=['it','type'])
        q[expid] = get_data(gsistat[expid],'q', select=[1,120],level=['it','type'])
        minim[expid] = get_data(gsistat[expid],'cost')
        oz[expid] = get_data(gsistat[expid],'oz',select=[1],level=['it'])
        rad[expid] = get_data(gsistat[expid],'rad',select=[1],level=['it'])

    # If instruments are desired, get them too
    if instruments is not None:
        insts = {}
        for inst in instruments:
            insts[inst] = {}
            tmp = {}
            for expid in expids:
                expid_gsistat = gsistat[expid]
                expid_inst = get_inst_data(expid_gsistat,inst,select=[1],level=['it'])
                tmp[expid] = expid_inst
            insts[inst] = tmp

    # Start plotting

    mc = ['k', 'r', 'g', 'b', 'm','c','y']
    alpha = 0.8

    if bdate == edate:
        title_substr = '%s' % bdate.strftime('%Y%m%d%H')
    else:
        title_substr = '%s-%s' % (bdate.strftime('%Y%m%d%H'),edate.strftime('%Y%m%d%H'))

    plt.close('all')

    figs = []; fignames = []
    fig = plot_ps(ps) ; figs.append(fig) ; fignames.append('ps')
    fig = plot_profile(uv,t,q,stat='rms') ; figs.append(fig) ; fignames.append('rms')
    fig = plot_profile(uv,t,q,stat='bias') ; figs.append(fig) ; fignames.append('bias')
    fig = plot_profile(uv,t,q,stat='count') ; figs.append(fig) ; fignames.append('count')
    fig = plot_cost(minim) ; figs.append(fig) ; fignames.append('cost')
    fig = plot_gradient(minim) ; figs.append(fig) ; fignames.append('gradient')
    fig = plot_sat(oz,otype='ozone') ; figs += fig; fignames += ['oz_read','oz_assim']
    fig = plot_sat(rad,otype='radiance') ; figs.append(fig) ; fignames.append(['rad_read','rad_assim'])
    if instruments is not None:
        for inst in instruments:
            fig = plot_channel(insts[inst],inst=inst) ; figs.append(fig) ; fignames.append(inst)

    if save_figure:
        for fig,figname in zip(figs,fignames):
            figname = './gsistat_%s' % figname
            lplotting.savefigure(fig,figname,format='png')
        plt.close('all')
    else:
        plt.show()

    sys.exit(0)
