#!/bin/ksh
set -x

#-----------------------------------------------------------------------------------------------
##PLEASE DONOT RUN THIS SCRIPT IF YOU ARE NOT BUILDING THE ENTIRE PACKAGE FROM SCRATCH !!!
#-----------------------------------------------------------------------------------------------

## to install the entire package on a new computer plaform, users need
## to first build the following librairies and utilities, then compile 
## a list of executables. Check each one carefully if it fails to compile.   

machine=BADGER   ;#IBM or ZEUS, JET, GAEA, WCOSS

if [ $machine = IBM ];then
 FCMP=xlf_r
 CCMP=xlc_r
elif [ $machine = WCOSS -o $machine = ZEUS -o $machine = JET -o $machine = BADGER ];then
 FCMP=ifort
 CCMP=cc
elif [ $machine = GAEA ];then
 FCMP=ftn
 CCMP=icc
else
 echo " machine=$machine not define. Add yours !"
 exit
fi

curdir=`pwd`


#--first libraries. Better to make use of admin built libraries if they exist.
# also note bufr lib takes a long time to build (~hour)
# These libraries may not be able to compile on IBM CCS. 

setlib=yes
if [ $machine = IBM ]; then setlib=no; fi
if [ $setlib = yes ]; then
for libname in bacio bufr ip sp sigio w3lib-2.0 ; do
  rm $curdir/nwprod/lib/*${libname}*.a
  rm $curdir/nwprod/lib/incmod/*/*${libname}*.mod
  cd $curdir/nwprod/lib/sorc/$libname
  rm *.o *.mod
  ./makefile.sh $FCMP $CCMP
  if [ $? -ne 0 ]; then exit ;fi
done
fi



#--then utilities
setutil=yes
if [ $machine = IBM -o $machine = WCOSS -o $machine = BADGER ]; then 
 setutil=no
 cp -p /nwprod/util/exec/copygb   $curdir/nwprod/util/exec/.
 cp -p /nwprod/util/exec/grbindex $curdir/nwprod/util/exec/.
 cp -p /nwprod/util/exec/ndate    $curdir/nwprod/util/exec/.
 cp -p /nwprod/util/exec/nhour    $curdir/nwprod/util/exec/.
fi

if [ $setutil = yes ]; then
for utilname in copygb grbindex ndate nhour ; do
  rm $curdir/nwprod/util/exec/$utilname                 
  cd $curdir/nwprod/util/sorc/${utilname}.fd
  rm *.o *.mod
  ./makefile.sh $FCMP 
  if [ $? -ne 0 ]; then exit ;fi
done
fi
wgrib=`which wgrib`
cp -p $wgrib $curdir/nwprod/util/exec/.



#--lastly program executables
cd $curdir/exe/sorc
   rm *.o *.mod ../grid2grid.x
   ./makefile.sh $FCMP
   if [ $? -ne 0 ]; then exit ;fi

cd $curdir/fit2obs/sorc
   rm *.o *.mod *.x
   ./makefile.sh $FCMP
   if [ $? -ne 0 ]; then exit ;fi

cd $curdir/manl
   rm *.o *.mod *.exe
   ./makefile.sh $FCMP
   if [ $? -ne 0 ]; then exit ;fi

cd $curdir/precip/sorc
   rm *.o *.mod ../exec/*.x
   ./makefile.sh $FCMP
   if [ $? -ne 0 ]; then exit ;fi

cd $curdir/precip/sorc_qpf
   rm *.o *.mod ../exec/PVRFY*
   ./makefile.sh $FCMP
   if [ $? -ne 0 ]; then exit ;fi


##--grid-to-obs
 cd $curdir/nwprod/sorc/verf_gridtobs_prepfits.fd
    rm *.o *.mod ../../exec/verf_gridtobs*
    ./makefile.sh $FCMP
    if [ $? -ne 0 ]; then exit ;fi

 cd $curdir/nwprod/sorc/verf_gridtobs_gridtobs.fd
    rm *.o *.mod
    ./makefile.sh $FCMP
    if [ $? -ne 0 ]; then exit ;fi

 cd $curdir/nwprod/sorc/verf_gridtobs_editbufr.fd
    rm *.o *.mod
    ./makefile.sh $FCMP
    if [ $? -ne 0 ]; then exit ;fi

