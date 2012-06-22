#!/bin/ksh

#---------------------------------------------------------------------------
#
#  mk_horiz_plots.sh
#
#---------------------------------------------------------------------------
echo start mk_horiz_plots.sh

set -ax
export list=$listvar

SUFFIX=$1
PDATE=$2


#------------------------------------------------------------------
#  Set up dates and cycle times for the 4 time periods starting
#  at $PDATE

D4="${PDATE}"
D3=`$NDATE -06 $PDATE`
D2=`$NDATE -12 $PDATE`
D1=`$NDATE -18 $PDATE`

export DATES="$D1 $D2 $D3 $D4"


#---------------------------------------------------------------------------
#  Set up the DATADIR, copy the radstat files to a temporary directory,
#  untar and extract needed ges files. 

DATADIR="${STMP_USER}/horiz_${SUFFIX}.${PDATE}"
rm -rf $DATADIR
mkdir -p $DATADIR
cd $DATADIR

#--------------------------------------------------------------------
#   Copy extraction program to working directory

$NCP $EXEDIR/horiz.${RAD_AREA}.x  ./horiz.x


#--------------------------------------------------------------------
#--------------------------------------------------------------------
#
#   Run horiz.x program to build the data files for each cycle

datdir=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX} radstat_location`

for date in ${DATES}; do
   sdate=`echo $date | cut -c1-8`
   cycle=`echo $date | cut -c9-10`

   if [[ $RAD_AREA = "glb" ]]; then

#      datdir=`${SCRIPTS}/get_datadir.sh ${SUFFIX} ${RADMON_PARM}/data_map`
      
      if [[ -d ${datdir}/gdas.${sdate} ]]; then
#         datdir=${datdir}/gdas.${sdate}
         radstat=${datdir}/gdas.${sdate}/gdas1.t${cycle}z.radstat
      else
         radstat=${datdir}/radstat.gdas.${date}
      fi

      $NCP ${radstat} ${date}.radstat

   else
#      /bin/sh ${RADMON_DATA_EXTRACT}/ush/getbestndas_radstat.sh $date $DATADIR /com/nam/prod
      /bin/sh ${RADMON_DATA_EXTRACT}/ush/getbestndas_radstat.sh $date $DATADIR $datdir
      mv ./radstat.${date} ${date}.radstat
   fi

   tar -xvf ${date}.radstat
   for sat in ${SATYPE}; do
      if [[ -s ./${sat}.Z ]]; then		#  rm previous files 
         rm -f ./${sat}.Z
      fi
      if [[ -s ./${sat} ]]; then
         rm -f ./${sat}
      fi
      mv diag_${sat}_ges.${date}.Z ${sat}.Z
      uncompress ${sat}.Z
   done

   rm -f $diag_*.${date}.Z
   rm -f ${date}.radstat


   iyy=`echo $date | cut -c1-4`
   imm=`echo $date | cut -c5-6`
   idd=`echo $date | cut -c7-8`
   ihh=`echo $date | cut -c9-10`

   nchanl=-999

   for sat in ${SATYPE}; do
      nchanl=`grep "title" ${sat}.ctl |cut -c27-32`

cat << EOF > input.$sat.$ihh
 &INPUT
  satname='${sat}',
  iyy=${iyy},
  imm=${imm},
  idd=${idd},
  ihh=${ihh},
  idhh=-18,
  incr=6,
  nchanl=${nchanl},
  suffix='${SUFFIX}',
 /
EOF
      timex ./horiz.x < input.$sat.$ihh >   stdout.$sat.$ihh


      rm -f $TANKDIR/horiz/stdout.$sat.$ihh
      $NCP stdout.$sat.$ihh        $TANKDIR/horiz/

   done
done

for sat in ${SATYPE}; do
   $NCP ${sat}.ctl*             $TANKDIR/horiz/
   compress -f $TANKDIR/horiz/${sat}.ctl
   chmod a+r ${sat}*.ieee_d*
done


PID="a"

#---------------------------------------------------------------------------
#  Split off airs_aqua and iasi_metop into separate jobs if they are in
#  the $SATYPE list.
#

use_airs_aqua=0
use_iasi_metop=0
export PTYPE="obs cor obsges obsnbc"

for sat in ${SATYPE}; do
   nchanl=`grep "title" ${sat}.ctl |cut -c27-32`
   if [[ $nchanl -ge 200 ]]; then
      bigSATLIST=" $sat $bigSATLIST "
#   fi
#   if [[ $sat = "iasi_metop-a" ]]; then
#      use_iasi_metop=1
#   elif [[ $sat = "airs_aqua" ]]; then
#      use_airs_aqua=1
   else
      SATLIST=" $sat $SATLIST " 
   fi
done

cmdfile="./cmdfile_horiz_${SUFFIX}_${PID}"
rm -f $cmdfile

>$cmdfile
for sat in ${SATLIST}; do
  echo "$SCRIPTS/plot_horiz.sh $sat" >> $cmdfile
done

ntasks=`cat $cmdfile|wc -l`
jobname=plot_${SUFFIX}_horiz_${PID}

export listvars=LOADLQ,PDATE,DATES,NDATE,NCP,DATADIR,TANKDIR,WEB_SVR,WEB_USER,WEBDIR,EXEDIR,LOGDIR,PLOT_WORK_DIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,PTMP_USER,STMP_USER,USER_CLASS,SUB,SUFFIX,PID,ACOUNT,PTYPE,SATLIST,IMGNDIR,listvars

$SUB -a $ACOUNT -e $listvars -j ${jobname} -u $USER -t 1:00:00 -o $LOGDIR/horiz_${PID}.log -p $ntasks -q dev -g $USER_CLASS /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes 


#------------

for sat in ${bigSATLIST}; do
   export SATLIST=$sat

#  --------
   export PTYPE="obs cor"
   export listvars=LOADLQ,PDATE,DATES,NDATE,NCP,DATADIR,TANKDIR,WEB_SVR,WEB_USER,WEBDIR,EXEDIR,LOGDIR,PLOT_WORK_DIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,PTMP_USER,STMP_USER,USER_CLASS,SUB,SUFFIX,PID,ACOUNT,PTYPE,SATLIST,IMGNDIR,listvars

   PID="${sat}_1"
   cmdfile="./cmdfile_horiz_${SUFFIX}_${PID}"

   rm -f $cmdfile
>$cmdfile
   echo "$SCRIPTS/plot_horiz.sh $sat" >> $cmdfile

   ntasks=`cat $cmdfile|wc -l`
   jobname=plot_${SUFFIX}_horiz_${PID}
   
   $SUB -a $ACOUNT -e $listvars -j ${jobname} -u $USER -t 3:45:00 -o $LOGDIR/horiz_${PID}.log -p $ntasks -q dev -g $USER_CLASS /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes 

#  --------
   PID="${sat}_2"
   cmdfile="./cmdfile_horiz_${SUFFIX}_${PID}"
   export PTYPE="obsges obsnbc"
   export listvars=LOADLQ,PDATE,DATES,NDATE,NCP,DATADIR,TANKDIR,WEB_SVR,WEB_USER,WEBDIR,EXEDIR,LOGDIR,PLOT_WORK_DIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,PTMP_USER,STMP_USER,USER_CLASS,SUB,SUFFIX,PID,ACOUNT,PTYPE,SATLIST,IMGNDIR,listvars

   rm -f $cmdfile
>$cmdfile
   echo "$SCRIPTS/plot_horiz.sh $sat" >> $cmdfile

   ntasks=`cat $cmdfile|wc -l`
   jobname=plot_${SUFFIX}_horiz_${PID}
   
   $SUB -a $ACOUNT -e $listvars -j ${jobname} -u $USER -t 3:45:00 -o $LOGDIR/horiz_${PID}.log -p $ntasks -q dev -g $USER_CLASS /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes 

done 

#if [[ $use_airs_aqua -eq 1 ]]; then
#   export SATLIST="airs_aqua"
#   PID="airs"
#
#   cmdfile="./cmdfile_horiz_${SUFFIX}_${PID}"
#   rm -f $cmdfile
#
#>$cmdfile
#   for sat in ${SATLIST}; do
#      echo "$SCRIPTS/plot_horiz.sh $sat" >> $cmdfile
#   done
#
#   ntasks=`cat $cmdfile|wc -l`
#   jobname=plot_${SUFFIX}_horiz_${PID}
#   
#   $SUB -a $ACOUNT -e $listvars -j ${jobname} -u $USER -t 2:00:00 -o $LOGDIR/horiz_${PID}.log -p $ntasks -q dev -g $USER_CLASS /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes 
#
#fi

#if [[ $use_iasi_metop -eq 1 ]]; then
#   export SATLIST="iasi_metop-a"
#   export PTYPE="obs cor"
#   export listvars=LOADLQ,PDATE,DATES,NDATE,NCP,DATADIR,TANKDIR,WEB_SVR,WEB_USER,WEBDIR,EXEDIR,LOGDIR,PLOT_WORK_DIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,PTMP_USER,STMP_USER,USER_CLASS,SUB,SUFFIX,PID,ACOUNT,PTYPE,SATLIST,SUFFIX,listvars
#   PID="iasi_1"
#
#   cmdfile="./cmdfile_horiz_${SUFFIX}_${PID}"
#   rm -f $cmdfile
#
#>$cmdfile
#   for sat in ${SATLIST}; do
#      echo "$SCRIPTS/plot_horiz.sh $sat" >> $cmdfile
#   done
#
#   ntasks=`cat $cmdfile|wc -l`
#   jobname=plot_${SUFFIX}_horiz_${PID}
#   
#   $SUB -a $ACOUNT -e $listvars -j ${jobname} -u $USER -t 3:45:00 -o $LOGDIR/horiz_${PID}.log -p $ntasks -q dev -g $USER_CLASS /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes 
#
#
#   PID="iasi_2"
#   cmdfile="./cmdfile_horiz_${SUFFIX}_${PID}"
#   rm -f $cmdfile
#   export PTYPE="obsges obsnbc"
#   export listvars=LOADLQ,PDATE,DATES,NDATE,NCP,DATADIR,TANKDIR,WEB_SVR,WEB_USER,WEBDIR,EXEDIR,LOGDIR,PLOT_WORK_DIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,PTMP_USER,STMP_USER,USER_CLASS,SUB,SUFFIX,PID,ACOUNT,PTYPE,SATLIST,IMGNDIR,listvars
#
#>$cmdfile
#   for sat in ${SATLIST}; do
#      echo "$SCRIPTS/plot_horiz.sh $sat" >> $cmdfile
#   done
#
#   ntasks=`cat $cmdfile|wc -l`
#   jobname=plot_${SUFFIX}_horiz_${PID}
#   
#   $SUB -a $ACOUNT -e $listvars -j ${jobname} -u $USER -t 3:45:00 -o $LOGDIR/horiz_${PID}.log -p $ntasks -q dev -g $USER_CLASS /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes 
#
#fi


echo finish mk_horiz_plots.sh
exit
