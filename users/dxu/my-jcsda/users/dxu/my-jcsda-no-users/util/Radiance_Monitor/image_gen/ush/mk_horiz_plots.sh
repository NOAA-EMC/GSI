#!/bin/ksh

#---------------------------------------------------------------------------
#
#  mk_horiz_plots.sh
#
#---------------------------------------------------------------------------
echo start mk_horiz_plots.sh

set -ax


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

$NCP ${IG_EXEC}/horiz.x  ./horiz.x


#--------------------------------------------------------------------
#--------------------------------------------------------------------
#
#   Run horiz.x program to build the data files for each cycle

echo datdir = $datdir

for date in ${DATES}; do
   sdate=`echo $date | cut -c1-8`
   cycle=`echo $date | cut -c9-10`

   if [[ $RAD_AREA = "glb" ]]; then

      if [[ -d ${datdir}/gdas.${sdate} ]]; then
         radstat=${datdir}/gdas.${sdate}/gdas1.t${cycle}z.radstat
      else
         radstat=${datdir}/radstat.gdas.${date}
      fi

      $NCP ${radstat} ${date}.radstat

   else
      /bin/sh ${RADMON_DATA_EXTRACT}/ush/getbestndas_radstat.sh $date $DATADIR $datdir
      mv ./radstat.${date} ${date}.radstat
   fi

   for sat in ${SATYPE}; do
      if [[ -s ./${sat}.${Z} ]]; then		#  rm previous files 
         rm -f ./${sat}.${Z}
      fi
      if [[ -s ./${sat} ]]; then		#  rm previous files 
         rm -f ./${sat}
      fi
   done

   tar -xvf ${date}.radstat
   for sat in ${SATYPE}; do
      mv diag_${sat}_ges.${date}.${Z} ${sat}.${Z}
      ${UNCOMPRESS} ${sat}.${Z}
   done

   rm -f $diag_*.${date}.${Z}
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
      ./horiz.x < input.$sat.$ihh >   stdout.$sat.$ihh

      rm -f $TANKDIR/horiz/stdout.$sat.$ihh
      $NCP stdout.$sat.$ihh        $TANKDIR/horiz/stdout.$sat.$ihh

   done
done

if [[ ! -d $IMGNDIR/horiz ]]; then
   mkdir -p $IMGNDIR/horiz
fi

for sat in ${SATYPE}; do

   if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "zeus" ]]; then
      sed -e 's/cray_32bit_ieee/ /' ${sat}.ctl > tmp_${type}.ctl
      mv -f tmp_${type}.ctl ${sat}.ctl
   fi

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
   nchanl=`cat ${sat}.ctl | gawk '/title/{print $NF}'`
   if [[ $nchanl -ge 80 ]]; then
      bigSATLIST=" $sat $bigSATLIST "
   else
      SATLIST=" $sat $SATLIST " 
   fi
done


#---------------------------------------------------------------------------
#  submit the plot jobs
#

if [[ $MY_MACHINE = "wcoss" ]]; then
   cmdfile="./cmdfile_horiz_${SUFFIX}_${PID}"
   logfile=${LOGdir}/horiz_${PID}.log
   rm -f $cmdfile

>$cmdfile
   for sat in ${SATLIST}; do
     echo "$IG_SCRIPTS/plot_horiz.sh $sat" >> $cmdfile
   done

   chmod 755 $cmdfile
   ntasks=`cat $cmdfile|wc -l`
   jobname=plot_${SUFFIX}_hrz_${PID}

   $SUB -q $JOB_QUEUE -P $PROJECT -R affinity[core] -M 500 -o ${logfile} -W 0:45 -J ${jobname} $cmdfile

else							# zeus/linux
   for sat in ${SATLIST}; do
      jobname=horiz_${sat}
      cmdfile="./cmdfile_horiz_${SUFFIX}_${sat}"
      logfile=${LOGdir}/horiz_${sat}.log

      rm -f ${cmdfile}
      rm -f ${logfile}

      echo "$IG_SCRIPTS/plot_horiz.sh $sat" >> $cmdfile

      $SUB -A $ACCOUNT -l procs=${ntasks},walltime=0:50:00 -N ${jobname} -V -j oe -o ${logfile} $cmdfile
   done
fi

#------------

for sat in ${bigSATLIST}; do
   export SATLIST=$sat

#  --------
   export PTYPE="obs cor"

   PID="${sat}_1"
   cmdfile="./cmdfile_horiz_${SUFFIX}_${PID}"

   rm -f $cmdfile
>$cmdfile
   echo "$IG_SCRIPTS/plot_horiz.sh $sat" >> $cmdfile
   chmod 755 $cmdfile

   ntasks=`cat $cmdfile|wc -l`
   jobname=plot_${SUFFIX}_hrz_${PID}
   
   if [[ $MY_MACHINE = "wcoss" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -R affinity[core] -M 500 -o ${logfile} -W 2:45 -J ${jobname} $cmdfile
   else
      $SUB -A $ACCOUNT -l procs=${ntasks},walltime=2:00:00 -N ${jobname} -V -j oe -o $LOGdir/horiz_${PID}.log $cmdfile
   fi

#  --------
   PID="${sat}_2"
   cmdfile="./cmdfile_horiz_${SUFFIX}_${PID}"
   export PTYPE="obsges obsnbc"

   rm -f $cmdfile
>$cmdfile
   echo "$IG_SCRIPTS/plot_horiz.sh $sat" >> $cmdfile
   chmod 755 $cmdfile

   ntasks=`cat $cmdfile|wc -l`
   jobname=plot_${SUFFIX}_hrz_${PID}
   
   if [[ $MY_MACHINE = "wcoss" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -R affinity[core] -M 500 -o ${logfile} -W 2:45 -J ${jobname} $cmdfile
   else
      $SUB -A $ACCOUNT -l procs=${ntasks},walltime=2:00:00 -N ${jobname} -V -j oe -o $LOGdir/horiz_${PID}.log $cmdfile
   fi

done 


echo finish mk_horiz_plots.sh
exit
