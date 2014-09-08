#!/bin/ksh
set -x

#------------------------------------------------------
#--run global_tracker.sh offline to produce track stats
#--Fanglin Yang, July 2013
#------------------------------------------------------
export chost=`echo $(hostname)|cut -c 1-1`

export PSLOT=${1:-prnurads}                                                           
export CDATE=${2:-2013073000} 
export flag_pgb=${3:-h}                         ;#pgb${flag_pgb}.gfs.$CDATE used for computing tracks
export COMOUT=${4:-/ptmp/Russ.Treadon/$PSLOT}   ;#forecast output data dir

#----------------------------------------------------
if [ $chost = g -o $chost = t ]; then
 export srcdir=/global/save/Fanglin.Yang/VRFY/hurtrack                                
 export stmp=/stmpd2/$LOGNAME                                              ;#running directory
 export dumpdir=/global/noscrub/Kate.Howard/dump   
 export HOMEDIR=/global/save/Fanglin.Yang/GFS/gfs_trunk/para                          
 export COMDIR=
 export archsyndir=/com/arch/prod/syndat
 export exectrkdir=/nwprod/util/exec
 export GETTRKEXEC=$exectrkdir/gettrk
 export PARATRKR=$HOMEDIR/util/ush/extrkr.sh
elif [ $chost = f ]; then
 export srcdir=/scratch2/portfolios/NCEPDEV/global/save/Fanglin.Yang/VRFY/hurtrack
 export stmp=/scratch2/portfolios/NCEPDEV/stmp/$LOGNAME                  ;#running directory
 export dumpdir=/scratch2/portfolios/NCEPDEV/global/noscrub/dump
 export HOMEDIR=/scratch1/portfolios/NCEPDEV/da/save/George.Gayno/jpss/gfs/trunk/para
 export COMDIR=/scratch2/portfolios/NCEPDEV/rstprod
 export archsyndir=/scratch2/portfolios/NCEPDEV/rstprod/com/arch/prod/syndat
 export exectrkdir=/scratch2/portfolios/NCEPDEV/global/save/Fanglin.Yang/jet_src/para/util/exec
 export GETTRKEXEC=$exectrkdir/gettrk
 export PARATRKR=/scratch1/portfolios/NCEPDEV/da/save/George.Gayno/jpss/ush/extrkr.sh
fi
export NWPROD=$HOMEDIR
export USHDIR=$HOMEDIR/ush

export rundir=$stmp/$PSLOT/${PSLOT}${CDATE}gfstrack    ;#tracker running directory 
if [ -s $rundir ]; then rm -rf $rundir ; fi
mkdir -p $rundir
export CDUMP=gfs     
export nknd=0        
export GDATE=`$NWPROD/util/exec/ndate -6 $CDATE`
cp $dumpdir/$CDATE/$CDUMP/tcvitl.$CDUMP.$CDATE $rundir/.   ;#copy gfs tcvital files
cp $dumpdir/$GDATE/gdas/tcvitl.gdas.$GDATE $rundir/.       ;#copy gdas tcvital files

export savetrackdir=${savetrackdir:-$stmp/$PSLOT}
if [ ! -s $savetrackdir ]; then mkdir -p $savetrackdir ; fi
$srcdir/global_tracker.sh $CDATE $CDUMP $COMOUT $rundir $nknd

exit




