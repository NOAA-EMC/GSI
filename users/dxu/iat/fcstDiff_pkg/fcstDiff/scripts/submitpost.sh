#!/bin/ksh --login
#BSUB -L /bin/ksh
#BSUB -a poe
#BSUB -P GFS-MTN
#BSUB -e /ptmp/Dana.Carlis/prgm142a/gm142a2013090800gfspost1.test
#BSUB -o /ptmp/Dana.Carlis/prgm142a/gm142a2013090800gfspost1.test
#BSUB -J gm142a2013090800gfspost1_test
#BSUB -network type=sn_all:mode=US
#BSUB -q dev
#BSUB -n 32
#BSUB -R span[ptile=4]
#BSUB -R affinity[cpu(4):distribute=balance]
#BSUB -x
#BSUB -W 01:00
set -x

export VERBOSE=YES

# CDATE is the cycle start date,
export CDATE=2013090800
export PSLOT=gm142a
ROTDIR=/ptmp/$USER/pr$PSLOT
TMPDIR=/stmp/$USER/${PSLOT}${CDATE}gfspost 
if [! -s $ROTDIR ] ; then mkdir -p $ROTDIR; fi
if [ ! -s $TMPDIR ] ; then mkdir -p $TMPDIR ; cd $TMPDIR; fi
# specify pressure levels
#export \
#POSTGPVARS="KPO=47,PO=1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1.,"

#######################################

BASEDIR=/global/save/emc.glopara/svn/gfs/trunk/para
HOMEDIR=/global/save/Dana.Carlis/para_gfs/prgm142a
export CTL_FCS=$BASEDIR/parms/parm_am/gfs_cntrl.parm
export CTL_ANL=$BASEDIR/parms/parm_am/gfs_cntrl.parm_anl
export POSTGPSH_NP=$HOMEDIR/global_nceppost.sh
export POSTGPEXEC_NP=/global/noscrub/Hui-Ya.Chuang/trunk/ncep_post
export DUMP=gfs

for h in anl; do
$POSTGPSH_NP $ROTDIR/sig${h}.$DUMP.$CDATE no no pgb${h}.$DUMP.$CDATE pgb${h}.$DUMP.$CDATE.idx 2880 1441 
cp -p pgb${h}.$DUMP.$CDATE $ROTDIR
done
#exit
for h in 00 12 24 36 48 60 72 84 96 108 120; do
$POSTGPSH_NP $ROTDIR/sigf${h}.$DUMP.$CDATE flxf${h}.$DUMP.$CDATE no pgbf${h}.$DUMP.$CDATE pgb${h}.$DUMP.$CDATE.idx 2880 1441
cp -p pgb${h}.$DUMP.$CDATE $ROTDIR
done
echo $?


