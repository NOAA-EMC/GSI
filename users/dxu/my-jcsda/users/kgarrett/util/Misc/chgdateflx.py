#!/usr/bin/env python

###############################################################
# < next few lines under version control, D O  N O T  E D I T >
# $URL$
# $Revision$
# $Date$
# $Author$
# $Id$
###############################################################

###############################################################
# Script to change the initialization date and adjust the
# corressponding steps to make the flx file behave as if it
# originated from a model forecast without IAU.
###############################################################

import sys,pygrib,shutil
from argparse import ArgumentParser,ArgumentDefaultsHelpFormatter
from datetime import datetime,timedelta

def main():

    parser = ArgumentParser(description='change date information in flux files',formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('-i','--input',help='input GRIB filename',type=str,required=True)
    parser.add_argument('-d','--debug',help='verbose debugging',action='store_true',required=False)
    parser.add_argument('-o','--overwrite',help='overwrite input GRIB filename. If FALSE, appends "-redated" to input filename',action='store_true',required=False)
    args = parser.parse_args()

    gribfile_in  = args.input
    debug        = args.debug
    overwrite    = args.overwrite

    gribfile_out = '%s-redated' % gribfile_in

    # Open input and output GRIB files
    try:
        igrbs = pygrib.open(gribfile_in)
        ogrbs = open(gribfile_out,'wb')
    except:
        print 'Unexpected error:', sys.exc_info()[0]
        raise

    # Loop over records in input GRIB file and write to output GRIB file
    for igrb in igrbs:
        if ( debug ): print igrb
        ogrb = redate(igrb)
        omsg = ogrb.tostring()
        ogrbs.write(omsg)

    # Close input and output GRIB files
    try:
        igrbs.close()
        ogrbs.close()
    except:
        print 'Unexpected error:', sys.exc_info()[0]
        raise

    # Replace input GRIB file by output GRIB file
    if ( overwrite ):
        try:
            shutil.move(gribfile_out,gribfile_in)
        except:
            print 'Unexpected error:', sys.exc_info()[0]
            raise

    sys.exit(0)

def redate(grb,fhr=6):

    # Note:
    # replacing julianDay updates all dataDate, dataTime, etc. information automatically
    # replacing startStep and endStep updates stepRange, P1, P2 information automatically

    # Read in time and step information from grib record
    DataTime = pygrib.julian_to_datetime(grb['julianDay'])
    startStep = grb['startStep']
    endStep   = grb['endStep']

    # Desired time and step information
    DataTime = DataTime + timedelta(hours=fhr)
    startStep = 0 if ( startStep <= fhr ) else startStep - fhr
    endStep   = 0 if (   endStep <= fhr ) else   endStep - fhr

    # Replace time and step information in grib record
    grb['julianDay'] = pygrib.datetime_to_julian(DataTime)
    grb['startStep'] = startStep
    grb['endStep']   =   endStep

    return grb

if __name__ == '__main__': main()
