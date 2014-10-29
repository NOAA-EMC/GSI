#!/bin/sh
#BSUB -oo chgres.out.%J 
#BSUB -eo chgres.out.%J
#BSUB -J chgres.dana      
#BSUB -P GFS-MTN
#BSUB -network type=sn_all:mode=US
#BSUB -q dev
#BSUB -W 00:30 
#BSUB -n 1
#BSUB -R span[ptile=1]
###BSUB -R "rusage[mem=25000]"
###BSUB -R affinity[core:distribute=balance]
#BSUB -R affinity[core(16)]
#BSUB -x
#BSUB -a poe
set -ax
adate=2012070100
edate=2012070100
tmpdir=/stmp/$LOGNAME/chgres
#datdir=/ptmp/wx23dc/surut1148
datdir=/global/noscrub/Kate.Howard/dump/$adate/gdas
#savdir=/ptmp/$LOGNAME/prhan

# Uncomment the JCAP,LEVS,LONB,LATB block for the
# resolution you want
# T574 L64 SL GFS
#export JCAP=1534
export JCAP=1534
export LEVS=64
if [ $JCAP -eq 1534 ]; then
export LONB=3072
export LATB=1536 
elif [ $JCAP -eq 1148 ]; then
export LONB=2304
export LATB=1152
elif [ $JCAP -eq 574 ]; then
export LONB=1152
export LATB=576
elif [ $JCAP -eq 382 ]; then
export LONB=768
export LATB=384
elif [ $JCAP -eq 254 ]; then
export LONB=512
export LATB=256
fi

# Make $tmpdir and $savdir.  Do work in $tmpdir
#mkdir -p $savdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf $tmpdir/*


# Set variables for global_chgres script.  Do not
# change these.
export machine=WCOSS
export DATA=DATA
export VERBOSE=YES
export mod=gdas
#export CHGRESSH=/global/save/emc.glopara/svn/gfs/trunk/para/ush/global_chgres.sh
export CHGRESSH=/nwprod/ush/global_chgres.sh
#export CHGRESEXEC=/global/save/wx23sm/wcoss/global_chgres.fd_wcoss/global_chgres
export CHGRESEXEC=/nwprod/exec/global_chgres
export NTHREADS=16
export OMP_NUM_THREADS=${NTHREADS}
export CHGRESTHREAD=${NTHREADS}

export
CHGRESVARS="use_ufo=.true.,NTRAC=3,IDVC=2,IVSSFC=200509,NVCOORD=2,IVSSIG=200509,IDVM=0,idvt=21,IALB=0,idsl=1,lsoil=4"
export FIXGLOBAL=/global/save/emc.glopara/svn/gfs/trunk/para/fix/fix_am
export FIXUTIL=/global/save/emc.glopara/svn/gfs/trunk/para/util/fix
export SIGLEVEL=${FIXGLOBAL}/global_hyblev.l64.txt

export OROGRAPHY=${FIXGLOBAL}/global_orography.t$JCAP.$LONB.$LATB.grb
export OROGRAPHY_UF=${FIXGLOBAL}/global_orography_uf.t$JCAP.$LONB.$LATB.grb
export SLMASK=${FIXGLOBAL}/global_slmask.t$JCAP.$LONB.$LATB.grb
#export LONSPERLAT=${FIXUTIL}/global_lonsperlat.t$JCAP.txt
export LONSPERLAT=${FIXGLOBAL}/global_lonsperlat.t$JCAP.$LONB.$LATB.txt

#export APRUNC="omplace -nt $NTHREADS"
# Change resolution of sigma files
#list="001 002 003 004 005 006 007 008 009 010 011 012 013 014 015 016 017 018 019 020 021 022 023 024 025 026 027 028 029 030 031 032 033 034 035 036 037 038 039 040 041 042 043 044 045 046 047 048 049 050 051 052 053 054 055 056 057 058 059 060 061 062 063 064 065 066 067 068 069 070 071 072 073 074 075 076 077 078 079 080"
#list="042"

#for file in $list; do

while [ $adate -le $edate ] ; do
export PGMOUT="stdout.$adate"
export PGBERR="stderr.$adate"
   #export SIGINP=$datdir/siganl_${adate}_mem${file}
   #export SIGOUT=$savdir/siganl_${adate}_mem${file}
   #export SFCINP=$datdir/sfcanl_${adate}_mem${file}
   #export SFCOUT=$savdir/sfcanl_${adate}_mem${file}
   export SIGINP=$datdir/siganl.$mod.${adate}
   export SIGOUT=$tmpdir/siganl.$mod.${adate}
   export SFCINP=$datdir/sfcanl.$mod.${adate}
   export SFCOUT=$tmpdir/sfcanl.$mod.${adate}
   $CHGRESSH
adate=`${ndate_dir}/ndate  +24 $adate`
#mv $SFCOUT $SIGOUT $savdir
done

exit
