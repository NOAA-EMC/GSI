#! /bin/bash

#------------------------------------------------------------------
#
#  plot_time.sh
#
#------------------------------------------------------------------

set -ax
export list=$listvars

SATYPE2=$1
PVAR=$2
PTYPE=$3

echo "Starting plot_time.sh"

#-----------------------------------------------
# Make sure IMGNDIR/time directory exists and
# set necessary environment variables.
#
if [[ ! -d ${IMGNDIR}/time ]]; then
   mkdir -p ${IMGNDIR}/time
fi
tmpdir=${PLOT_WORK_DIR}/plot_time_${RADMON_SUFFIX}_${SATYPE2}.$PDATE.${PVAR}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

plot_time_count=plot_time_count.${RAD_AREA}.gs
echo plot_time_count = ${plot_time_count}

plot_time_sep=plot_time_sep.${RAD_AREA}.gs
echo plot_time_sep = ${plot_time_sep}


echo PLOT_WORK_DIR = ${PLOT_WORK_DIR}
echo tmpdir        = ${tmpdir}

#------------------------------------------------------------------
#   Set dates
#
bdate=${START_DATE}
edate=$PDATE
bdate0=`echo $bdate|cut -c1-8`
edate0=`echo $edate|cut -c1-8`

#--------------------------------------------------------------------
# Set ctldir to point to correct control file source
#
imgdef=`echo ${#IMGNDIR}`
if [[ $imgdef -gt 0 ]]; then
  ctldir=$IMGNDIR/time
else
  ctldir=$TANKDIR/time
fi

echo ctldir = $ctldir


#--------------------------------------------------------------------
# Loop over satellite types.  Copy data files, create plots and
# place on the web server.
#
# Data file location may either be in angle, bcoef, bcor, and time
# subdirectories under $TANKDIR, or in the Operational organization
# of radmon.YYYYMMDD directories under $TANKDIR.
#
for type in ${SATYPE2}; do
   
   $NCP $ctldir/${type}.ctl* ./
   if [[ -s ./${type}.ctl.${Z} ]]; then
      ${UNCOMPRESS} ./${type}.ctl.${Z}
   fi

   if [[ $USE_ANL = 1 ]]; then
      $NCP $ctldir/${type}_anl.ctl* ./
      ${UNCOMPRESS} ./${type}_anl.ctl.${Z}
   fi

   cdate=$bdate
   while [[ $cdate -le $edate ]]; do

      if [[ $REGIONAL_RR -eq 1 ]]; then
         tdate=`$NDATE +6 $cdate`
         day=`echo $tdate | cut -c1-8 `
         cyc=`echo $cdate | cut -c9-10`
         . ${IG_SCRIPTS}/rr_set_tz.sh $cyc
      else 
         day=`echo $cdate | cut -c1-8 `
         cyc=`echo $cdate | cut -c9-10`
      fi

      ieee_src=${TANKverf}/${RUN}.${day}/${cyc}/${MONITOR}
      if [[ ! -d ${ieee_src} ]]; then
         ieee_src=${TANKverf}/${RUN}.${day}/${MONITOR}
      fi
      if [[ ! -d ${ieee_src} ]]; then
         ieee_src=${TANKverf}/${MONITOR}.${day}
      fi
      if [[ ! -d ${ieee_src} ]]; then
         ieee_src=${TANKverf}/${RUN}.${day}
      fi


      if [[ -d ${ieee_src} ]]; then
         test_file=${ieee_src}/time.${type}.${cdate}.ieee_d

         if [[ $USE_ANL = 1 ]]; then
            test_file2=${ieee_src}/time.${type}_anl.${cdate}.ieee_d
         else
            test_file2=
         fi

         if [[ -s $test_file ]]; then
            $NCP ${test_file} ./${type}.${cdate}.ieee_d
         elif [[ -s ${test_file}.${Z} ]]; then
            $NCP ${test_file}.${Z} ./${type}.${cdate}.ieee_d.${Z}
         fi

         if [[ -s $test_file2 ]]; then
            $NCP ${test_file2} ./${type}_anl.${cdate}.ieee_d
         elif [[ -s ${test_file2}.${Z} ]]; then
            $NCP ${test_file2}.${Z} ./${type}_anl.${cdate}.ieee_d.${Z}
         fi
      fi

      adate=`$NDATE +${CYCLE_INTERVAL} $cdate`
      cdate=$adate
   done

   ${UNCOMPRESS} ./*.ieee_d.${Z}

   if [[ $PLOT_STATIC_IMGS -eq 1 ]]; then
      for var in ${PTYPE}; do
         echo $var
         if [ "$var" =  'count' ]; then 
cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${IG_GSCRIPTS}/${plot_time_count} ${type} ${var} ${PLOT_ALL_REGIONS} x1100 y850'
'quit'
EOF
         elif [ "$var" =  'penalty' ]; then
cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${IG_GSCRIPTS}/${plot_time_count} ${type} ${var} ${PLOT_ALL_REGIONS} x1100 y850'
'quit'
EOF
         else
cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${IG_GSCRIPTS}/${plot_time_sep} ${type} ${var} ${PLOT_ALL_REGIONS} x1100 y850'
'quit'
EOF
         fi

         echo "running GrADS on ${tmpdir}/${type}_${var}.gs"
         $GRADS -bpc "run ${tmpdir}/${type}_${var}.gs"

      done 
   fi

   #----------------------------------------------------------
   #  mk_digital_time.sh  creates the data files used by the 
   #    html/js web files for interactive chart generation.
   #
   $NCP ${IG_SCRIPTS}/mk_digital_time.sh  .
   ./mk_digital_time.sh  ${type}
   rm -f mk_digital_time.sh 

done

#--------------------------------------------------------------------
# Copy image files to $IMGNDIR.
#
if [[ $PLOT_STATIC_IMGS -eq 1 ]]; then
   cp -f *.png  ${IMGNDIR}/time
fi


#--------------------------------------------------------------------
# Clean $tmpdir.
cd $tmpdir
cd ../
rm -rf $tmpdir

echo "Exiting plot_time.sh"
exit

