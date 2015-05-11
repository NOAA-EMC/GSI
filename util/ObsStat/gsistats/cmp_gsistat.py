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
from datetime import datetime
from argparse import ArgumentParser,ArgumentDefaultsHelpFormatter

def main():

    parser = ArgumentParser(description = 'Comparison table of GSI statistics',formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('-x','--expid',help='experiment IDs to compare',nargs=2,required=True)
    args = parser.parse_args()

    expids = args.expid

    vars =  ['ps', 'uv', 't', 'q', 'oz']

    for v,var in enumerate(vars):
        file1 = './data/%s/gsistat_%s.out' % (expids[0],var)
        dates1,counts1,rmses1 = read_gsistat(file1,var)
        file2 = './data/%s/gsistat_%s.out' % (expids[1],var)
        dates2,counts2,rmses2 = read_gsistat(file2,var)
        if ( check_dates(dates1,dates2) ):
            print 'dates mismatch between %s and %s\n' %(expids[0],expids[1])
            sys.exit(1)

        if ( v == 0 ):
            print "%s - %s, total %d\n" % (dates1[0].strftime('%Y%m%d%H'),dates1[-1].strftime('%Y%m%d%H'),len(dates1))

        cmp_var(var,counts1,rmses1,counts2,rmses2)

    sys.exit(0)

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

def cmp_var(varname,counts1,rms1,counts2,rms2):

    diff = rms1 - rms2

    if varname == 'ps':

        print "================ %s ===============" % varname.center(2)
        print " COUNTS1  COUNTS2 RMSE1 RMSE2  DIFF"
        print "-----------------------------------"
        print "%8i %8i %5.2f %5.2f %5.2f" % (counts1,counts2,rms1,rms2,diff)

    elif varname == 'oz':

        print "============ %s =============" % varname.center(2)
        print " PEN1    PEN2    DIFF        "
        print "-----------------------------"
        print "%8.2f %8.2f %5.2f" % (rms1,rms2,diff)

    else:

        ptops = [1000, 900, 800, 600, 400, 300, 250, 200, 150, 100,  50,    0]
        pbots = [1200,1000, 900, 800, 600, 400, 300, 250, 200, 150, 100, 2000]
        print "===================== %s ====================" % varname.center(2)
        print "lev1-lev2  COUNTS1  COUNTS2 RMSE1 RMSE2  DIFF"
        print "---------------------------------------------"
        for (pb,pt,count1,count2,rmse1,rmse2,diff2) in zip(pbots,ptops,counts1,counts2,rms1,rms2,diff):
            print "%4i-%4i %8i %8i %5.2f %5.2f %5.2f" % (pb,pt,count1,count2,rmse1,rmse2,diff2)

    print

    return

if __name__ == '__main__': main()
