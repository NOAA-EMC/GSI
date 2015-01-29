#!/bin/sh
# This shell script tests ncdump for netcdf-4
# $Id: tst_netcdf4.sh,v 1.7 2007/02/15 16:03:48 ed Exp $

set -e
echo ""
echo "*** Testing ncgen and ncdump test output for netCDF-4 format."
echo "*** creating netcdf-4 file c0.nc from c0.cdl..."
../ncgen/ncgen -v3 -b -o c0.nc $srcdir/../ncgen/c0.cdl
echo "*** creating c1.cdl from c0.nc..."
./ncdump -n c1 c0.nc > c1.cdl
echo "*** comparing c1.cdl with ref_ctest1_nc4.cdl..."
diff c1.cdl $srcdir/ref_ctest1_nc4.cdl

echo
echo "*** Testing ncgen and ncdump test output for netCDF-4 classic format."
echo "*** creating netcdf-4 classic file c0.nc from c0.cdl..."
../ncgen/ncgen -v4 -b -o c0.nc $srcdir/../ncgen/c0.cdl
echo "*** creating c1.cdl from c0.nc..."
./ncdump -n c1 c0.nc > c1.cdl
echo "*** comparing c1.cdl with ref_ctest1_nc4c.cdl..."
diff c1.cdl $srcdir/ref_ctest1_nc4c.cdl

echo
echo "*** Testing ncdump output for netCDF-4 features."
echo "*** dumping tst_solar_1.nc to tst_solar_1.cdl..."
./ncdump tst_solar_1.nc > tst_solar_1.cdl
echo "*** comparing tst_solar_1.cdl with ref_tst_solar_1.cdl..."
diff tst_solar_1.cdl $srcdir/ref_tst_solar_1.cdl
echo "*** dumping tst_solar_2.nc to tst_solar_2.cdl..."
./ncdump tst_solar_2.nc > tst_solar_2.cdl
echo "*** comparing tst_solar_2.cdl with ref_tst_solar_2.cdl..."
diff tst_solar_2.cdl $srcdir/ref_tst_solar_2.cdl

echo
echo "*** All ncgen and ncdump test output for netCDF-4 format passed!"
exit 0
