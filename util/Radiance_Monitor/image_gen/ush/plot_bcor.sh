#! /bin/ksh

#------------------------------------------------------------------
#
#  plot_bcor.sh
#
#------------------------------------------------------------------

set -ax
export list=$listvars

SATYPE2=$1
PVAR=$2
PTYPE=$3

echo "begin plot_bcor.sh"

#------------------------------------------------------------------
# Set environment variables.

word_count=`echo $PTYPE | wc -w`
echo word_count = $word_count

if [[ $word_count -le 1 ]]; then
   tmpdir=${PLOT_WORK_DIR}/plot_bcor_${RADMON_SUFFIX}_${SATYPE2}.$PDATE.${PVAR}.${PTYPE}
else
   tmpdir=${PLOT_WORK_DIR}/plot_bcor_${RADMON_SUFFIX}_${SATYPE2}.$PDATE.${PVAR}
fi

rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

plot_bcor_count=plot_bcor_count.${RAD_AREA}.gs
plot_bcor_sep=plot_bcor_sep.${RAD_AREA}.gs


#------------------------------------------------------------------
#   Set dates

bdate=${START_DATE}
edate=$PDATE
bdate0=`echo $bdate|cut -c1-8`
edate0=`echo $edate|cut -c1-8`


#--------------------------------------------------------------------
# Set ctldir to point to control file directory

imgdef=`echo ${#IMGNDIR}`
if [[ $imgdef -gt 0 ]]; then
  ctldir=$IMGNDIR/bcor
else
  ctldir=$TANKDIR/bcor
fi

echo ctldir = $ctldir


#--------------------------------------------------------------------
# Loop over satellite types.  Copy data files, create plots and
# place on the web server.
#
# Data file location may either be in angle, bcoef, bcor, and time
# subdirectories under $TANKDIR, or in the Operational organization
# of radmon.YYYYMMDD directories under $TANKDIR.

for type in ${SATYPE2}; do

   $NCP $ctldir/${type}.ctl* ./
   ${UNCOMPRESS} *.ctl.${Z}

   cdate=$bdate
   while [[ $cdate -le $edate ]]; do
      if [[ $REGIONAL_RR -eq 1 ]]; then
         tdate=`$NDATE +6 $cdate`
         day=`echo $tdate | cut -c1-8 `
         cyc=`echo $cdate | cut -c9-10`
         . ${IG_SCRIPTS}/rr_set_tz.sh $cyc
      else
         day=`echo $cdate | cut -c1-8 `
         cyc=`echo $cdate | cut -c9-10 `
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

      nfile_src=`ls -l ${ieee_src}/*${cdate}*ieee_d* | egrep -c '^-'`

      echo "nfile_src = $nfile_src"

      if [[ -d ${ieee_src} ]]; then
         if [[ $REGIONAL_RR -eq 1 ]]; then
            test_file=${ieee_src}/${rgnHH}.bcor.${type}.${cdate}.ieee_d.${rgnTM}
         else 
            test_file=${ieee_src}/bcor.${type}.${cdate}.ieee_d
         fi

         if [[ $USE_ANL = 1 ]]; then
            if [[ $REGIONAL_RR -eq 1 ]]; then
               test_file=${ieee_src}/${rgnHH}.bcor.${type}_anl.${cdate}.ieee_d.${rgnTM}
            else
               test_file2=${ieee_src}/bcor.${type}_anl.${cdate}.ieee_d
            fi
         else
            test_file2=
         fi

         if [[ -s $test_file ]]; then
            $NCP ${test_file} ./${type}.${cdate}.ieee_d
         elif [[ -s ${test_file}.${Z} ]]; then
            $NCP ${test_file}.${Z} ./${type}.${cdate}.ieee_d.${Z}
         fi
      fi

      adate=`$NDATE +${CYCLE_INTERVAL} $cdate`
      cdate=$adate
   done
   ${UNCOMPRESS} *.ieee_d.${Z}

   for var in ${PTYPE}; do
      echo $var
      if [ "$var" =  'count' ]; then
cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${IG_GSCRIPTS}/${plot_bcor_count} ${type} ${var} ${PLOT_ALL_REGIONS} x1100 y850'
'quit'
EOF
      else
cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${IG_GSCRIPTS}/${plot_bcor_sep} ${type} ${var} ${PLOT_ALL_REGIONS} x1100 y850'
'quit'
EOF
      fi

      echo ${tmpdir}/${type}_${var}.gs
      $GRADS -bpc "run ${tmpdir}/${type}_${var}.gs"
   done 

#--------------------------------------------------------------------
# Delete data files

#   rm -f ${type}.ieee_d
#   rm -f ${type}.ctl

done

#--------------------------------------------------------------------
# Copy image files to $IMGNDIR to set up for mirror to web server.
# Delete images and data files.

if [[ ! -d ${IMGNDIR}/bcor ]]; then
   mkdir -p ${IMGNDIR}/bcor
fi
cp -r *.png  ${IMGNDIR}/bcor



#--------------------------------------------------------------------
# Clean $tmpdir  

cd $tmpdir
cd ../
rm -rf $tmpdir


#--------------------------------------------------------------------
# If this is the last bcor plot job to finish then rm PLOT_WORK_DIR.
#

#count=`ls ${LOADLQ}/*plot*_${RADMON_SUFFIX}* | wc -l`
#complete=`grep "COMPLETED" ${LOADLQ}/*plot*_${RADMON_SUFFIX}* | wc -l`

#running=`expr $count - $complete`

#if [[ $running -eq 1 ]]; then
#   cd ${PLOT_WORK_DIR}
#   cd ../
#   rm -rf ${PLOT_WORK_DIR}
#fi

echo "end plot_bcor.sh"
exit
