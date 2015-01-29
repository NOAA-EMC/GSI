#cd /nwprod/lib/sorc/netcdf
#tar xvf netcdf.tar
cd /nwprod/lib/sorc/netcdf/netcdf-3.5.0/src
export CXX="" 
export CC="/usr/bin/ncepxlc" 
export FC="/usr/bin/ncepxlf" 
export F90="" 
export CFLAGS="-q32" 
export FFLAGS="-O -q32" 
export ARFLAGS='-X 32 cru'
export NMFLAGS='-X 32'
make    clean
./configure 
make 
make install 
cd ../lib
cp libnetcdf.a /nwprod/lib/libnetcdf.a 
chmod 775 /nwprod/lib/libnetcdf.a
