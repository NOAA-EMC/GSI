#!/usr/bin/env python

###############################################################
# < next few lines under version control, D O  N O T  E D I T >
# $Date$
# $Revision$
# $Author$
# $Id$
###############################################################

# parse error statistics for PS, UV, T, Q and OZ from gsistat.gdas.YYYYMMDDHH

import os
import sys
import subprocess
from datetime import datetime,timedelta
from argparse import ArgumentParser,ArgumentDefaultsHelpFormatter

def main():
    parser = ArgumentParser(description = 'Parse gsistat.gdas.YYYYMMDDHH file for O-F statistics',formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('-x','--expid',help='experiment ID',type=str,required=True)
    parser.add_argument('-s','--start_date',help='starting date',type=str,metavar='YYYYMMDDHH',required=True)
    parser.add_argument('-e','--end_date',help='ending date',type=str,metavar='YYYYMMDDHH',required=True)
    parser.add_argument('-a','--archive_dir',help='archive directory',type=str,required=False,default='/da/noscrub/%s/archive'%os.environ['USER'])
    args = parser.parse_args()

    expid   = args.expid
    sdate   = args.start_date ; sdate = datetime.strptime(sdate,'%Y%m%d%H')
    edate   = args.end_date   ; edate = datetime.strptime(edate,'%Y%m%d%H')
    archdir = '%s/%s'% (args.archive_dir,expid)

    if ( edate < sdate ):
        print 'start date cannot be after end date, switching!'
        adate = edate
        edate = sdate
        sdate = adate

    datadir = './data/%s' % expid
    cmd = 'mkdir -p %s' % datadir
    subprocess.check_call(cmd,shell=True)

    vars = ['ps', 'uv', 't', 'q', 'oz']
    for var in vars:
        exec('%s = []' % var)

    adate = sdate
    while ( adate <= edate ):

        adatestr = adate.strftime('%Y%m%d%H')
        fname = '%s/gsistat.gdas.%s' % (archdir,adatestr)
        if ( not os.path.exists(fname) ):
            print '%s does not exist and should!' % fname
            sys.exit(1)

        print 'Processing ... %s' % fname

        cmd = """grep ' o-g 01      ps     180' %s""" % fname
        tmp = subprocess.check_output(cmd,shell=True)
        ps.append('%s %s' % (adatestr,tmp))

        cmd = """grep ' o-g 01      uv     220' %s | grep count""" % fname
        tmp = subprocess.check_output(cmd,shell=True)
        uv.append('%s %s' % (adatestr,tmp))
        cmd = """grep ' o-g 01      uv     220' %s | grep rms"""   % fname
        tmp = subprocess.check_output(cmd,shell=True)
        uv.append('%s %s' % (adatestr,tmp))

        cmd = """grep ' o-g 01       t     120' %s | grep count""" % fname
        tmp = subprocess.check_output(cmd,shell=True)
        t.append('%s %s' % (adatestr,tmp))
        cmd = """grep ' o-g 01       t     120' %s | grep rms"""   % fname
        tmp = subprocess.check_output(cmd,shell=True)
        t.append('%s %s' % (adatestr,tmp))

        cmd = """grep ' o-g 01       q     120' %s | grep count""" % fname
        tmp = subprocess.check_output(cmd,shell=True)
        q.append('%s %s' % (adatestr,tmp))
        cmd = """grep ' o-g 01       q     120' %s | grep rms"""   % fname
        tmp = subprocess.check_output(cmd,shell=True)
        q.append('%s %s' % (adatestr,tmp))

        cmd = """grep 'ozone total   penalty_all= ' %s | head -n 1""" % fname
        tmp = subprocess.check_output(cmd,shell=True)
        oz.append('%s %s' % (adatestr,tmp))

        adate = adate + timedelta(hours=6)

    for var in vars:
        exec('vardump = %s' % var)
        fname = '%s/gsistat_%s.out' % (datadir,var)
        fh = open(fname,'w')
        fh.writelines('%s' % line for line in vardump)
        fh.close()

    sys.exit(0)

if __name__ == '__main__': main()
