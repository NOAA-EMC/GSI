#!/bin/sh
# This shell script which creates the fill.nc file from fill.cdl.
# $Id: create_fills.sh,v 1.1 2006/01/03 13:53:08 ed Exp $

echo "*** Creating fills.nc."
set -e
../ncgen/ncgen -b $srcdir/fills.cdl
echo "*** SUCCESS!"
exit 0
