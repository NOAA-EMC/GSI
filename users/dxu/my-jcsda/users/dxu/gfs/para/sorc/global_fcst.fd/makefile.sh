#!/bin/ksh
set -x

mac=$(hostname | cut -c1-1)
mac2=$(hostname | cut -c1-2)
export USE_MKL=YES
export USE_MKL=${USE_MKL:-YES}

#export debug=YES                                # uncomment this line if you want debug option
#export CLEAN=NO                                 # uncomment this if you don't want to clean before
                                                 # compiling
export ICS_VERSION=14.0.1
#
#---------------------------------------------------------
if [ $mac2 = ga ] ; then                         # For GAEA
                                                 # --------
 machine=gaea
 center=${center:-ncep}
 BASEDIR=/lustre/f1/unswept/$center/$LOGNAME
 ptmp=/lustre/f1/$center/$LOGNAME/ptmp
#export LIBDIR=$BASEDIR/para/lib
 export LIBDIR=/lustre/f1/unswept/ncep/Shrinivas.Moorthi/nceplibs/nwprod/lib
 export NCEPLIB=$LIBDIR
 export ESMFDIR=$BASEDIR/ESMF/esmf
 export sigiov="4"
#---------------------------------------------------------
elif [ $mac = z -o $mac = h -o $mac = f ] ; then # For ZEUS
                                                 # --------
 machine=zeus
 ptmp=/scratch2/portfolios/NCEPDEV/ptmp/$LOGNAME
 export LIBDIR=/contrib/nceplibs/nwprod/lib
 export NCEPLIB=/contrib/nceplibs/dev/lib
 export ESMF_LIB=/apps/esmf/3.1.0rp5/intel/mpt/lib/libO/Linux.intel.64.mpi.default        
 export ESMF_MOD=/apps/esmf/3.1.0rp5/intel/mpt/mod/modO/Linux.intel.64.mpi.default        
#---------------------------------------------------------
elif [ $mac = t -o $mac = e -o $mac = g ] ; then # For WCOSS
                                                 # --------
 machine=wcoss
 ptmp="/ptmpp1/$LOGNAME"
 export LIBDIR=/nwprod/lib
 export NCEPLIB=/usrx/local/nceplibs
 export ESMF_LIB=/usrx/local/esmf-3.1.0rp5/lib/libO/Linux.intel.64.intelmpi.default
 export ESMF_MOD=/usrx/local/esmf-3.1.0rp5/mod/modO/Linux.intel.64.intelmpi.default

 if [ $USE_MKL = YES ] ; then
   export ALIGN="-align array32byte"             # For bit reproducibility on wcoss
   . /usrx/local/Modules/3.2.9/init/ksh          # To enable module unload and load
   module unload ics
   export ICS_VERSION=${ICS_VERSION:-14.0.1}
   module load ics/$ICS_VERSION
  export PRECISE=source
# export PRECISE=precise
 fi
fi
#---------------------------------------------------------

export ALIGN=${ALIGN:-""}
#
#  WARNING!!! The default endianness is local to the machine.
#   If your initial conditions are bigendian and want to compile on littleendian
#   machine, you must set NATIVE_ENDIAN=NO
#
NATIVE_ENDIAN=NO
#
sorc_dir=$(pwd)
exec_dir=$(pwd)
mkdir -p $exec_dir
#
export LIBDIR=${LIBDIR:-../../lib}
make_dir=$ptmp/branch/sorc/$(basename $sorc_dir)

#####################################################################
if [ $make_dir = $(pwd) ] ; then
  echo "The make_dir is the current directory - compilation aborted"
  echo "To continue compiling, comment out this if loop and rerun"
  exit
fi
#####################################################################

#mkdir -p $make_dir
#cd $make_dir || exit 99
#[ $? -ne 0 ] && exit 8
#
#if [ ${CLEAN:-YES} = YES ] ; then
# rm $make_dir/*.o
# rm $make_dir/*.mod
#fi
#
#tar -cf- -C$sorc_dir .|tar -xf-

if [ $NATIVE_ENDIAN = YES ] ; then
 cp $sorc_dir/sigio_r_module_native.f sigio_r_module.f
 cp $sorc_dir/bafrio_native.f         bafrio.f
fi
export mono=${mono:-""}
if [ $mono = _mono ] ; then
 cp $sorc_dir/gloopa_opt_gg$mono.f gloopa_opt_gg.f
fi
Makefile=Makefile$mono

#
export PRECISE=${PRECISE:-precise}
#-------------------------------------------------------------------
#-------------------------------------------------------------------
if [ $machine = gaea -o $machine = zeus -o $machine = wcoss ] ; then
 export W3LIB=w3lib-2.0_d
 export CFLAGS="-DLINUX"
 export FINCM="-I$LIBDIR/incmod/w31ib-2.0_d -I$NCEPLIB/incmod/sigio_v2.0.1_beta"
 export ARCHM=
 export PGSZM=
 export FRRM=-FR

 export debug=${debug:-NO}
 if [ $debug = YES ] ; then
   export OPTSB="-g -O0 -check all -ftrapuv -convert big_endian $ALIGN -fp-stack-check -fstack-protector -heap-arrays -recursive $ALIGN"  
   export OPTSBT="$OPTSB -traceback"
   export EXECM=$exec_dir/global_fcst_dbg
 else
   if [ $machine = zeus ]; then
    export OPTSB="-g -O3 -convert big_endian $ALIGN -fp-model $PRECISE "  
    export OPTSBX="-g -O3 -convert big_endian $ALIGN -fp-model $PRECISE "  
   else
    export OPTSB="-g -O3 -convert big_endian $ALIGN -fp-model $PRECISE -xAVX "  
    export OPTSBX="-g -O3 -convert big_endian $ALIGN -fp-model $PRECISE -xAVX "  
   fi
   export OPTSBT=$OPTSB
   export OPTSBTX=$OPTSBX
   export EXECM=$exec_dir/global_fcst
 fi

 export OPTSIOM="$OPTSBT -r8 -openmp"
 export OPTSIOX="$OPTSBTX -r8 -openmp"
 export OPTSM="$OPTSBT -r8 -openmp"
 export OPTS_SERM="$OPTSBT -r8 $ARCHM"
 export OPTS90M="$OPTSBT   -r8 "
 export OPTS90AM="$OPTSBT  -r8 "
 export LDFLAGSM=$PGSZM

 #----------------------------
 if [ $machine = gaea ] ; then
   export F77M=ftn
   export F90M=ftn
   export F77B=$F77M
   export FCC=cc
   export LDRM=ftn
   export FINC=-I$ESMF_MOD
   export LIBSM="-L$NCEPLIB -lsigio_v2.0.1_beta -L$LIBDIR -lbacio_4 -lsp_d -l$W3LIB -lrt -lstdc++ -L$ESMF_LIB -lesmf"
 #----------------------------
 elif [ $machine = wcoss ] ; then
   export F77M=mpiifort
   export F90M=mpiifort
   export F77B=$F77M
   export FCC=mpcc
   export LDRM=mpiifort
   if [ $USE_MKL = YES ] ; then
      module unload ics
      module load ics/$ICS_VERSION
      export LDFLAGSM="$PGSZM -openmp -lmkl_intel_lp64 -lmkl_core -lmkl_sequential -lpthread -lm"
   else
      export LDFLAGSM="$PGSZM -openmp "
   fi
   export FINC=-I$ESMF_MOD
   export FINCM="-I$NCEPLIB/incmod/sigio_v2.0.1_beta -I$LIBDIR/incmod/w3emc_v2.0.3_d -I$LIBDIR/incmod/w3nco_v2.0.3_d"
   export LIBSM="-L$NCEPLIB -lsigio_v2.0.1_beta -L$LIBDIR -lbacio_4 -lnemsio -lsp_d -lw3emc_v2.0.3_d -lw3nco_v2.0.3_d -lrt -lstdc++ -L$ESMF_LIB -lesmf"
 #----------------------------
 else
   export F77M="ifort -openmp"  #-recursive"
   export F90M="ifort -openmp"  #-recursive"
   export F77B="ifort "         #-recursive" #-openmp"  #-recursive"
   export F90B="ifort "         #-recursive" #-openmp"  #-recursive"
   export LDRM="ifort -lmpi"
   export W3LIB=w3nco_d
   export FCC=cc
   if [ $USE_MKL = YES ] ; then
      export LDFLAGSM="$PGSZM -openmp -mkl"
   else
      export LDFLAGSM="$PGSZM -openmp "
   fi
   export FINC=-I$ESMF_MOD
   export FINCM="-I$NCEPLIB/incmod/sigio_v2.0.1_beta -I$LIBDIR/incmod/$W3LIB"
   export LIBSM="-L$NCEPLIB -lsigio_v2.0.1_beta -L$LIBDIR -lbacio_4 -lnemsio -lsp_d -l$W3LIB -lrt -lstdc++ -L$ESMF_LIB -lesmf"
 fi
 #----------------------------
else
 echo 'machine not supported at this time'
 exit
fi
#-------------------------------------------------------------------
#-------------------------------------------------------------------

echo $F77M
if [ $USE_MKL = YES ] ; then
  make -f Makefile
else
  make -f Makefile model-mpi-port
fi
