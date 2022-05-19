#! /bin/bash

#------------------------------------------------------------------
#
#  plot_angle.sh
#
#------------------------------------------------------------------

set -ax
export list=$listvar

SATYPE2=$1
PVAR=$2
PTYPE=$3

export PLOT_ALL_REGIONS=${PLOT_ALL_REGIONS:-1}

plot_angle_count=plot_angle_count.${RAD_AREA}.gs
plot_angle_sep=plot_angle_sep.${RAD_AREA}.gs


#------------------------------------------------------------------
# Create $wrkdir

word_count=`echo $PTYPE | wc -w`
echo word_count = $word_count

if [[ $word_count -le 1 ]]; then
   wrkdir=${PLOT_WORK_DIR}/plot_angle_${RADMON_SUFFIX}_${SATYPE2}.$PDATE.${PVAR}.${PTYPE}
else
   wrkdir=${PLOT_WORK_DIR}/plot_angle_${RADMON_SUFFIX}_${SATYPE2}.$PDATE.${PVAR}
fi
rm -rf $wrkdir
mkdir -p $wrkdir
cd $wrkdir



#------------------------------------------------------------------
#   Set dates

bdate=${START_DATE}
edate=$PDATE
bdate0=`echo $bdate|cut -c1-8`
edate0=`echo $edate|cut -c1-8`



#--------------------------------------------------------------------
# Copy control files to $wrkdir
ctldir=$IMGNDIR/angle

for type in ${SATYPE2}; do
  $NCP $ctldir/${type}*.ctl* ./
done
${UNCOMPRESS} *.ctl.${Z}


#--------------------------------------------------------------------
# Loop over satellite types.  Copy data files, create plots and 
# move to $IMGNDIR. 
#
for type in ${SATYPE2}; do

   $NCP $ctldir/${type}*.ctl* ./
   ${UNCOMPRESS} *.ctl.${Z}

   cdate=$bdate

   #-------------------------------------
   #  Locate and copy data files.
   #
   while [[ $cdate -le $edate ]]; do

      if [[ $REGIONAL_RR -eq 1 ]]; then
         tdate=`$NDATE +6 $cdate`
         day=`echo $tdate | cut -c1-8`
         cyc=`echo $cdate | cut -c9-10`
         . ${IG_SCRIPTS}/rr_set_tz.sh $cyc
      else
         day=`echo $cdate | cut -c1-8`
         cyc=`echo $cdate | cut -c9-10`
      fi

      #----------------------------------------------------
      #  Attempt to locate the extracted ieee data files.
      #
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

      #-----------------------------------------------------------
      #  Locate the data files, first checking for a tar file,
      #  and copy them locally.
      #
      if [[ -e ${ieee_src}/radmon_angle.tar && -e ${ieee_src}/radmon_angle.tar.${Z} ]]; then
         echo "Located both radmon_angle.tar and radmon_angle.tar.${Z} in ${ieee_src}.  Unable to plot."
         exit 20

      elif [[ -e ${ieee_src}/radmon_angle.tar || -e ${ieee_src}/radmon_angle.tar.${Z} ]]; then
         files=`tar -tf ${ieee_src}/radmon_angle.tar* | grep ${type} | grep ieee_d`
         if [[ ${files} != "" ]]; then
            tar -xf ${ieee_src}/radmon_angle.tar* ${files}
         fi

      else				
         files=`ls ${ieee_src}/angle.*${type}*ieee_d*`
         for f in ${files}; do
            $NCP ${f} .
         done
      fi

      adate=`$NDATE +${CYCLE_INTERVAL} ${cdate}`
      cdate=$adate

   done

   ${UNCOMPRESS} $wrkdir/*.ieee_d.${Z}

   #-----------------------------------------------
   #  Remove 'angle.' from the *ieee_d file names.
   #
   prefix="angle."
   dfiles=`ls *.ieee_d`
   for file in $dfiles; do
      newfile=`basename $file | sed -e "s/^$prefix//"`
      mv ./${file} ./${newfile}
   done

   #-----------------------------------------------------------------------
   #  mk_digital_ang.sh produces the text files used by the js/html files
   #  to generate the interactive charts
   #
   $NCP ${IG_SCRIPTS}/mk_digital_ang.sh .
   ./mk_digital_ang.sh ${type}


   #--------------------------------------------------------------------
   #  Conditionally execute GrADS plotting.
   #
   if [[ $PLOT_STATIC_IMGS -eq 1 ]]; then 
      for var in ${PTYPE}; do
         echo $var

         if [ "$var" =  'count' ]; then

cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${IG_GSCRIPTS}/${plot_angle_count} ${type} ${var} ${PLOT_ALL_REGIONS} ${PLOT_SUB_AVGS} x1100 y850'
'quit'
EOF

         elif [ "$var" =  'penalty' ]; then
cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${IG_GSCRIPTS}/${plot_angle_count} ${type} ${var} ${PLOT_ALL_REGIONS} ${PLOT_SUB_AVGS} x1100 y850'
'quit'
EOF
         else

cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${IG_GSCRIPTS}/${plot_angle_sep} ${type} ${var} ${PLOT_ALL_REGIONS} ${PLOT_SUB_AVGS} x1100 y850'
'quit'
EOF
         fi


         $GRADS -bpc "run ${wrkdir}/${type}_${var}.gs"
      done 

   fi

done

#--------------------------------------------------------------------
# Copy image files to $IMGNDIR.
#
if [[ ${RAD_AREA} = "rgn" || $PLOT_STATIC_IMGS -eq 1 ]]; then
   find . -name '*.png' -exec cp -pf {} ${IMGNDIR}/angle/ \;
fi


#--------------------------------------------------------------------
# Clean $wrkdir 
#
echo Removing wrkdir = $wrkdir
cd $wrkdir
cd ../
rm -rf $wrkdir

exit
