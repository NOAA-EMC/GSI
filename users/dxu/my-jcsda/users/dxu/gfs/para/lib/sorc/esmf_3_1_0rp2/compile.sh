set -x

#export envir=para 
#export HOMEDIR=/nw${envir}
export HOMEDIR=${HOMEDIR:-/global/save/wx23sm/GFS/f2010/trunk/para}
export ESMF_DIR=$HOMEDIR/lib/sorc/esmf_3_1_0rp2
cd $ESMF_DIR
export ESMF_BOPT=O
unset ESMF_ARCH ESMF_PREC
gmake clobber
gmake
cd $HOMEDIR/lib/sorc/esmf_3_1_0rp2/lib/libO/AIX.default.64.mpi.default
mv libesmf.a $HOMEDIR/lib/libesmf_3_1_0rp2.a
mkdir $HOMEDIR/lib/incmod/esmf_3_1_0rp2
cd $HOMEDIR/lib/sorc/esmf_3_1_0rp2/mod/modO/AIX.default.64.mpi.default
mv * $HOMEDIR/lib/incmod/esmf_3_1_0rp2
cd $ESMF_DIR
gmake clobber  # to clean up the unnecessary files in the src directory
