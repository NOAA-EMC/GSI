#!/bin/ksh

#---------------------------------------------------------------------------
#
#  mk_horiz_plots.sh
#
#---------------------------------------------------------------------------
echo start mk_horiz_plots.sh

set -ax
export list=$listvar

#SUFFIX=$1
#PDATE=$2


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

# TEMP until XML:simple becomes avail to processing nodes
#datdir=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX} radstat_location`

for date in ${DATES}; do
   sdate=`echo $date | cut -c1-8`
   cycle=`echo $date | cut -c9-10`

   if [[ $RAD_AREA = "glb" ]]; then

      if [[ -d ${datdir}/gdas.${sdate} ]]; then
         radstat=${datdir}/gdas.${sdate}/gdas1.t${cycle}z.radstat
      else
         radstat=${datdir}/radstat.gdas.${date}
      fi

if [[ -s ${radstat} ]]; then
   echo radstat is good to go
else
   echo Houston we have a problem
fi

      $NCP ${radstat} ${date}.radstat

   else
      /bin/sh ${RADMON_DATA_EXTRACT}/ush/getbestndas_radstat.sh $date $DATADIR $datdir
      mv ./radstat.${date} ${date}.radstat
   fi

   for sat in ${SATYPE}; do
      if [[ -s ./${sat}.${COMPRESS_SUFF} ]]; then		#  rm previous files 
         rm -f ./${sat}.${COMPRESS_SUFF}
      fi
   done

   tar -xvf ${date}.radstat
   for sat in ${SATYPE}; do
      mv diag_${sat}_ges.${date}.${COMPRESS_SUFF} ${sat}.${COMPRESS_SUFF}
      ${UNCOMPRESS} ${sat}.${COMPRESS_SUFF}
   done

   rm -f $diag_*.${date}.${COMPRESS_SUFF}
   rm -f ${date}.radstat


   iyy=`echo $date | cut -c1-4`
   imm=`echo $date | cut -c5-6`
   idd=`echo $date | cut -c7-8`
   ihh=`echo $date | cut -c9-10`

   nchanl=-999

   for sat in ${SATYPE}; do

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
  little_endian=${LITTLE_ENDIAN},
 /
EOF
      $TIMEX ./horiz.x < input.$sat.$ihh >   stdout.$sat.$ihh

      rm -f $TANKDIR/horiz/stdout.$sat.$ihh
      $NCP stdout.$sat.$ihh        $TANKDIR/horiz/stdout.$sat.$ihh

   done
done

if [[ ! -d $IMGNDIR/horiz ]]; then
   mkdir -p $IMGNDIR/horiz
fi

for sat in ${SATYPE}; do
   $NCP ${sat}.ctl*             $IMGNDIR/horiz/${sat}.ctl
   ${COMPRESS} -f $TANKDIR/horiz/${sat}.ctl
   chmod a+r ${sat}*.ieee_d*
done


PID="a"

#---------------------------------------------------------------------------
#  Split off all sat/instrument sources that are of significant size into 
#  separate jobs if they are in the $SATYPE list.
#

export PTYPE="obs cor obsges obsnbc"

for sat in ${SATYPE}; do
   nchanl=`cat ${sat}.ctl | awk '/title/{print $NF}'`
   if [[ $nchanl -ge 80 ]]; then
      bigSATLIST=" $sat $bigSATLIST "
   else
      SATLIST=" $sat $SATLIST " 
   fi
done


#---------------------------------------------------------------------------
#  submit the plot jobs
#
export listvars=LOADLQ,PDATE,DATES,NDATE,NCP,DATADIR,TANKDIR,WEB_SVR,WEB_USER,WEBDIR,EXEDIR,LOGDIR,PLOT_WORK_DIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,PTMP_USER,STMP_USER,USER_CLASS,SUB,SUFFIX,PID,ACCOUNT,PTYPE,SATLIST,IMGNDIR,COMPRESS_SUFF,COMPRESS,UNCOMPRESS,listvars

if [[ $MY_OS = "aix" ]]; then				# CCS/aix
   cmdfile="./cmdfile_horiz_${SUFFIX}_${PID}"
   logfile=${LOGDIR}/horiz_${PID}.log
   rm -f $cmdfile

>$cmdfile
   for sat in ${SATLIST}; do
     echo "$SCRIPTS/plot_horiz.sh $sat" >> $cmdfile
   done

   ntasks=`cat $cmdfile|wc -l`
   jobname=plot_${SUFFIX}_hrz_${PID}


   $SUB -a $ACCOUNT -e $listvars -j ${jobname} -u $USER -t 1:00:00 -o ${logfile} -p $ntasks -q dev -g $USER_CLASS /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes 

else							# zeus/linux
   for sat in ${SATLIST}; do
      jobname=horiz_${sat}
      cmdfile="./cmdfile_horiz_${SUFFIX}_${sat}"
      logfile=${LOGDIR}/horiz_${sat}.log

      rm -f ${cmdfile}
      rm -f ${logfile}

      echo "$SCRIPTS/plot_horiz.sh $sat" >> $cmdfile

      $SUB -A $ACCOUNT -l procs=${ntasks},walltime=0:50:00 -N ${jobname} -v $listvars -j oe -o ${logfile} $cmdfile
   done
fi

#------------

for sat in ${bigSATLIST}; do
   export SATLIST=$sat

#  --------
   export PTYPE="obs cor"
   export listvars=LOADLQ,PDATE,DATES,NDATE,NCP,DATADIR,TANKDIR,WEB_SVR,WEB_USER,WEBDIR,EXEDIR,LOGDIR,PLOT_WORK_DIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,PTMP_USER,STMP_USER,USER_CLASS,SUB,SUFFIX,PID,ACCOUNT,PTYPE,SATLIST,IMGNDIR,COMPRESS_SUFF,COMPRESS,UNCOMPRESS,listvars

   PID="${sat}_1"
   cmdfile="./cmdfile_horiz_${SUFFIX}_${PID}"

   rm -f $cmdfile
>$cmdfile
   echo "$SCRIPTS/plot_horiz.sh $sat" >> $cmdfile

   ntasks=`cat $cmdfile|wc -l`
   jobname=plot_${SUFFIX}_hrz_${PID}
   
   if [[ $MY_OS = "aix" ]]; then
      $SUB -a $ACCOUNT -e $listvars -j ${jobname} -u $USER -t 3:45:00 -o $LOGDIR/horiz_${PID}.log -p $ntasks -q dev -g $USER_CLASS /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes 
   else
      $SUB -A $ACCOUNT -l procs=${ntasks},walltime=1:20:00 -N ${jobname} -v $listvars -j oe -o $LOGDIR/horiz_${PID}.log $cmdfile
   fi

#  --------
   PID="${sat}_2"
   cmdfile="./cmdfile_horiz_${SUFFIX}_${PID}"
   export PTYPE="obsges obsnbc"
   export listvars=LOADLQ,PDATE,DATES,NDATE,NCP,DATADIR,TANKDIR,WEB_SVR,WEB_USER,WEBDIR,EXEDIR,LOGDIR,PLOT_WORK_DIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,PTMP_USER,STMP_USER,USER_CLASS,SUB,SUFFIX,PID,ACCOUNT,PTYPE,SATLIST,IMGNDIR,COMPRESS_SUFF,COMPRESS,UNCOMPRESS,listvars

   rm -f $cmdfile
>$cmdfile
   echo "$SCRIPTS/plot_horiz.sh $sat" >> $cmdfile

   ntasks=`cat $cmdfile|wc -l`
   jobname=plot_${SUFFIX}_hrz_${PID}
   
   if [[ $MY_OS = "aix" ]]; then
      $SUB -a $ACCOUNT -e $listvars -j ${jobname} -u $USER -t 3:45:00 -o $LOGDIR/horiz_${PID}.log -p $ntasks -q dev -g $USER_CLASS /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes 
   else
      $SUB -A $ACCOUNT -l procs=${ntasks},walltime=1:00:00 -N ${jobname} -v $listvars -j oe -o $LOGDIR/horiz_${PID}.log $cmdfile
   fi

done 


echo finish mk_horiz_plots.sh
exit
