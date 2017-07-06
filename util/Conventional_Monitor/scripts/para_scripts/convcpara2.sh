#!/bin/ksh

set -ax
export list=$list0


#------------------------------------------------------------------
# Set environment variables.
tmpdir=/stmp/wx20es/conv${SUFFIX}.$PDATE
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir


#--------------------------------------------------------------------
#   Set environment variables to export to subsequent scripts

export LLQ=/u/wx20mi/bin/llq2
export WEBDIR=/home/people/emc/www/htdocs/gmb/gdas/convention/${SUFFIX}/pngs
#export WEBDIR=/export/lnx42/wd20xs/gsiqcweb2/opr
export EXEDIR=/u/wx20es/home/convweb/exec
export FIXDIR=/nwpara/fix 
export CTLDIR=/u/wx20es/home/convweb/ctldir
export WSUSER=wd20xs
export WS=rzdm.ncep.noaa.gov

export STNMAP=/usrx/local/grads/bin/stnmap
export GRADS=/usrx/local/grads/bin/grads
export GSCRIPTS=/u/wx20es/home/convweb/gscripts
export ACOUNT=RDAS-MTN
export nreal_ps=17
export nreal_q=18
export nreal_t=17
export nreal_uv=21

export NPREDR=5

# Copy data files file to local data directory.  Untar radstat file.  Change DATDIR definition

GDATE=`$NDATE -6 $PDATE`
gsdate=`echo $GDATE|cut -c1-8`
GCYA=`echo $GDATE|cut -c9-10`
GDATDIR=/com/gfs/para/gdas.$gsdate

export DATDIRL=/stmp/wx20es/datconv_$SUFFIX
rm -rf $DATDIRL
mkdir -p $DATDIRL
$NCP $DATDIR/gdas1.t${CYA}z.cnvstat $DATDIRL/cnvstat.$PDATE
$NCP $DATDIR/gdas1.t${CYA}z.pgrbanl $DATDIRL/pgbanl.$PDATE
$NCP $GDATDIR/gdas1.t${GCYA}z.pgrbf06 $DATDIRL/pgbf06.$GDATE

cd $DATDIRL
tar -xvf cnvstat.$PDATE
uncompress *Z
rm cnvstat.$PDATE

export DATDIR=$DATDIRL


# Export variables
export listvar=PDATE,NDATE,DATDIR,TANKDIR,LLQ,WEBDIR,EXEDIR,FIXDIR,LOGDIR,SCRIPTS,GSCRIPTS,CTLDIR,STNMAP,GRADS,USER,SUB,SUFFIX,NPREDR,NCP,PLOT,PREFIX,ACOUNT,nreal_ps,nreal_q,nreal_t,nreal_uv,WS,WSUSER,listvar


#------------------------------------------------------------------
#   Clean up $tmpdir  Submit copy job for next day.
#
cd $tmpdir
cd ../
rm -rf $tmpdir

rm $LOGDIR/horz_hist.log
$SUB -a $ACOUNT -e $listvar -j horz_hist_${SUFFIX} -q dev -g devonprod -t 0:30:00 -o $LOGDIR/horz_hist.log $SCRIPTS/horz_hist.sh_new


rm $LOGDIR/time_vert.log
$SUB -a $ACOUNT -e $listvar -j time_vert_${SUFFIX} -q dev -g devonprod -t 0:30:00 -o $LOGDIR/time_vert.log $SCRIPTS/time_vert.sh

rm $LOGDIR/update.log
$SUB -a $ACOUNT -e $listvar -j update_${SUFFIX} -q dev -g devonprod -t 0:30:00 -o $LOGDIR/update.log $SCRIPTS/update.sh
#
exit
