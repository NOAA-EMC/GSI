#!/bin/sh
# This shell script tests lengths of small netcdf files and tests 
# that rewriting a numeric value doesn't change file length
# $Id: tst_lengths.sh,v 1.5 2006/08/29 15:25:47 russ Exp $

# cat > rewrite-scalar.c << EOF
# #include <stdio.h>
# #include <netcdf.h>
# #define ERR do {fflush(stdout); fprintf(stderr, "Error, %s, line: %d\n", __FILE__, __LINE__); return(1);} while (0)

# int
# main(int ac, char *av[]) {
#     int ncid, varid, data[] = {42};
#     if (nc_open(av[1], NC_WRITE, &ncid)) ERR;
#     if (nc_inq_varid(ncid, av[2], &varid)) ERR;
#     if (nc_put_var_int(ncid, varid, data)) ERR;
#     if (nc_close(ncid)) ERR;
#     return 0;
# }
# EOF
# cat > test-len.sh << 'EOF'
# #!/bin/sh
# # test that length of file $1 is $2
# len=`ls -l $1|awk '{print $5}'`
# if [ $len = $2 ]; then
#   exit 0
# else
#   echo "### Failure: file $1 has length $len instead of expected $2"
#   exit 1
# fi
# EOF
# chmod +x ./test-len.sh
# cc -g -o rewrite-scalar -I../libsrc rewrite-scalar.c -L../libsrc -lnetcdf
# echo "netcdf small {variables: byte t; data: t = 1;}" > small.cdl
set -e
echo "*** testing length of classic file"
../ncgen/ncgen -b ${srcdir}/small.cdl
test `ls -l small.nc | awk '{print $5}'` = 68;

echo "*** testing length of classic file written with NOFILL"
../ncgen/ncgen -b -x ${srcdir}/small.cdl
test `ls -l small.nc | awk '{print $5}'` = 68;

echo "*** testing length of rewritten classic file"
../ncgen/ncgen -b ${srcdir}/small.cdl && ./rewrite-scalar small.nc t
test `ls -l small.nc | awk '{print $5}'` = 68;

echo "*** testing length of rewritten classic file written with NOFILL"
../ncgen/ncgen -b -x ${srcdir}/small.cdl && ./rewrite-scalar small.nc t
test `ls -l small.nc | awk '{print $5}'` = 68;

echo "*** testing length of 64-bit offset file"
../ncgen/ncgen -b -k64-bit-offset ${srcdir}/small.cdl
test `ls -l small.nc | awk '{print $5}'` = 72;

echo "*** testing length of 64-bit offset file written with NOFILL"
../ncgen/ncgen -b -k64-bit-offset -x ${srcdir}/small.cdl
test `ls -l small.nc | awk '{print $5}'` = 72;

echo "*** testing length of rewritten 64-bit offset file"
../ncgen/ncgen -b -k64-bit-offset ${srcdir}/small.cdl && ./rewrite-scalar small.nc t
test `ls -l small.nc | awk '{print $5}'` = 72;

echo "*** testing length of rewritten 64-bit offset file written with NOFILL"
../ncgen/ncgen -b -k64-bit-offset -x ${srcdir}/small.cdl && ./rewrite-scalar small.nc t
test `ls -l small.nc | awk '{print $5}'` = 72;

# test with only one record variable of type byte or short, which need
# not be 4-byte aligned
echo "*** testing length of one-record-variable classic file"
../ncgen/ncgen -b ${srcdir}/small2.cdl
test `ls -l small2.nc | awk '{print $5}'` = 101;

echo "*** testing length of one-record-variable classic file written with NOFILL"
../ncgen/ncgen -b -x ${srcdir}/small2.cdl
test `ls -l small2.nc | awk '{print $5}'` = 101;

echo "*** testing length of one-record-variable 64-bit offset file"
../ncgen/ncgen -b -k64-bit-offset ${srcdir}/small2.cdl
test `ls -l small2.nc | awk '{print $5}'` = 105;

echo "*** testing length of one-record-variable 64-bit offset file written with NOFILL"
../ncgen/ncgen -b -k64-bit-offset -x ${srcdir}/small2.cdl
test `ls -l small2.nc | awk '{print $5}'` = 105;
