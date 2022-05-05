#! /bin/bash

#------------------------------------------------------------------
#
#  plot_bcoef.sh
#
#------------------------------------------------------------------

set -ax
export list=$listvar



#------------------------------------------------------------------
# Set environment variables.

tmpdir=${PLOT_WORK_DIR}/plot_bcoef_${RADMON_SUFFIX}.$PDATE
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

plot_bcoef=plot_bcoef.gs



#------------------------------------------------------------------
#   Set dates
bdate=${START_DATE}
edate=$PDATE
bdate0=`echo $bdate|cut -c1-8`
edate0=`echo $edate|cut -c1-8`


#--------------------------------------------------------------------
# Copy executable and control files to $tmpdir
ctldir=$IMGNDIR/bcoef
$NCP ${IG_SCRIPTS}/mk_digital_bcoef.sh .


#--------------------------------------------------------------------
# Loop over satellite types.  Copy data files, create plots and
# place on the web server.
#
for type in ${SATYPE}; do

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
      #  Locate the ieee_d files, first checking for a tar file,
      #  and copy them locally.
      #
      if [[ -e ${ieee_src}/radmon_bcoef.tar && -e ${ieee_src}/radmon_bcoef.tar.${Z} ]]; then
         echo "Located both radmon_bcoef.tar and radmon_bcoef.tar.${Z} in ${ieee_src}.  Unable to plot."
         exit 21

      elif [[ -e ${ieee_src}/radmon_bcoef.tar || -e ${ieee_src}/radmon_bcoef.tar.${Z} ]]; then
         files=`tar -tf ${ieee_src}/radmon_bcoef.tar* | grep ${type} | grep ieee_d`
         if [[ ${files} != "" ]]; then
            tar -xf ${ieee_src}/radmon_bcoef.tar* ${files}
         fi

      else				
         files=`ls ${ieee_src}/bcoef.*${type}*ieee_d*`
         for f in ${files}; do
            $NCP ${f} .
         done
      fi

      adate=`$NDATE +${CYCLE_INTERVAL} ${cdate}`
      cdate=$adate

   done

   ${UNCOMPRESS} *.ieee_d.${Z}

   #-----------------------------------------------
   #  Remove 'bcoef.' from the *ieee_d file names.
   #
   prefix="bcoef."
   dfiles=`ls *.ieee_d`
   for file in $dfiles; do
      newfile=`basename $file | sed -e "s/^$prefix//"`
      mv ./${file} ./${newfile}
   done

   if [[ $PLOT_STATIC_IMGS -eq 1 ]]; then
      list="mean atmpath clw lapse2 lapse cos_ssmis sin_ssmis emiss ordang4 ordang3 ordang2 ordang1"
      for var in $list; do

cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${IG_GSCRIPTS}/${plot_bcoef} ${type} ${var} x1100 y850'
'quit'
EOF
         $GRADS -bpc "run ${tmpdir}/${type}_${var}.gs"


      done 
      if [[ ! -d ${IMGNDIR}/bcoef ]]; then
         mkdir -p ${IMGNDIR}/bcoef
      fi
      mv *.png  ${IMGNDIR}/bcoef
   fi


   #--------------------------------------------------------------------------
   #  mk_digital_bcoef.sh produces the data files needed by the js/html files 
   #  to generate the interactive charts.
   #
   ./mk_digital_bcoef.sh ${type}

   rm -f ${type}*ieee_d

done


#--------------------------------------------------------------------
# Clean $tmpdir.  Submit done job.
#
cd $tmpdir
cd ../
rm -rf $tmpdir


exit

