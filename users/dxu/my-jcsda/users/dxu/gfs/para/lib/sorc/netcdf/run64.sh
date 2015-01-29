#BV: 2/05/2007


#cd /nwprod/lib/sorc/netcdf
#tar xvf netcdf.tar
#cd /nwprod/lib/sorc/netcdf/netcdf-3.5.0/src
set -x
currdir=$(pwd)
cd ../../
libdir=$(pwd)
cd $currdir/netcdf-3.5.0/src
export CC=xlc_r
export CPPFLAGS=-DNDEBUG
export CFLAGS=-O
export FC=xlf_r
export FFLAGS=-O
export F90=xlf90_r
export F90FLAGS=-qsuffix=f=f90   # Note: no "-O" option
export CXX=xlC_r
export CXXFLAGS=-O
#   For the 64-bit mode on IBM SP
export ARFLAGS='-X 64 cru'
export NMFLAGS='-X 64'
make  clean
./configure 
make 
make install 
cd ../lib
mv libnetcdf.a $libdir/libnetcdf_64.a 
chmod 775 $libdir/libnetcdf_64.a
mkdir -p $libdir/incmod/netcdf-3.5.0
mv ../include/* $libdir/incmod/netcdf-3.5.0/
chmod 775 $libdir/incmod/netcdf-3.5.0/*
