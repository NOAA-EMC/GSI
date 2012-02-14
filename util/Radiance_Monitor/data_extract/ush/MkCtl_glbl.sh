#!/bin/sh

#--------------------------------------------------------------------
#  MkCtl_glbl.sh
#
#  Create control files for global (GDAS) radiance diagnostic data.
#  The control files will be placed in the $TANKDIR subdirectories.
#--------------------------------------------------------------------

function usage {
  echo "Usage:  MkCtl_glbl.sh suffix run_envir"
  echo "            File name for MkCtl_glbl.sh may be a full or relative path"
  echo "            Suffix is the indentifier for this data source (i.e. opr, oex)."
  echo "            The run_envir maybe dev, or para." 
}

set -ax
echo start MkCtl_glbl.sh

nargs=$#
if [[ $nargs -ne 2 ]]; then
   usage
   exit 1
fi

this_file=`basename $0`
this_dir=`dirname $0`

export SUFFIX=$1
export RUN_ENVIR=$2

echo $SUFFIX
echo RUN_ENVIR = $RUN_ENVIR

echo VRFYRAD_DIR = $VRFYRAD_DIR


#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------
export RAD_AREA=glb

if [[ $RUN_ENVIR = para || $RUN_ENVIR = prod ]]; then
   this_dir=${VRFYRAD_DIR}
fi


top_parm=${this_dir}/../../parm

if [[ -s ${top_parm}/RadMon_config ]]; then
   . ${top_parm}/RadMon_config
else
   echo "Unable to source RadMon_config file in ${top_parm}"
   exit
fi


. ${RADMON_DATA_EXTRACT}/parm/data_extract_config
. ${PARMverf_rad}/glbl_conf

echo IMGNDIR = $IMGNDIR


#--------------------------------------------------------------------
#  Empty out the MAIL variables.  If we're just building control
#  files we don't need any mail notifications.
#--------------------------------------------------------------------
export MAIL_TO=
export MAIL_CC=


mkdir -p $TANKDIR
mkdir -p $LOGDIR

tmpdir=${WORKverf_rad}/check_rad${SUFFIX}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir


export SCRIPTS=$USHverf_rad
export MAKE_CTL=1
export MAKE_DATA=0

need_radstat=1

#------------------------------------------------------------------
#  define data file sources depending on $RUN_ENVIR
#
#  need to idenfity correct output location(s) for binary files
#------------------------------------------------------------------
if [[ $RUN_ENVIR = dev ]]; then

   export DATDIR=`get_datadir ${SUFFIX} ${RADMON_PARM}/data_map`


   #---------------------------------------------------------------
   # Get date of cycle to process.  Start at the stored prodate
   # value and search backwards for a radstat file. 
   #---------------------------------------------------------------
   export PDATE=`get_prodate ${SUFFIX} ${RADMON_PARM}/data_map`

   ctr=0
   while [[ $need_radstat -eq 1 && $ctr -lt 10 ]]; do

      sdate=`echo $PDATE|cut -c1-8`
      export CYA=`echo $PDATE|cut -c9-10`
      testdir=${DATDIR}/gdas.$sdate

      #---------------------------------------------------------------
      # Locate required files or reset PDATE and try again.          
      #---------------------------------------------------------------
      if [[ -s $testdir/gdas1.t${CYA}z.radstat ]]; then

         biascr=${testdir}/gdas1.t${CYA}z.abias  
         satang=${testdir}/gdas1.t${CYA}z.satang
         radstat=${testdir}/gdas1.t${CYA}z.radstat
         need_radstat=0
         export DATDIR=${testdir}
      else
         export PDATE=`ndate -06 $PDATE` 
         ctr=$(( $ctr + 1 ))
      fi
   done

elif [[ $RUN_ENVIR = para ]]; then

   #---------------------------------------------------------------
   # Locate required files.             
   #---------------------------------------------------------------
   export DATDIR=$COMOUT 
   export PDATE=$CDATE

   ctr=0

   while [[ $need_radstat -eq 1 && $ctr -lt 10 ]]; do

      if [[ -s $DATDIR/radstat.gdas.${PDATE} ]]; then

         biascr=$DATDIR/biascr.gdas.${PDATE}  
         satang=$DATDIR/satang.gdas.${PDATE}
         radstat=$DATDIR/radstat.gdas.${PDATE}
         need_radstat=0

         echo biascr  = $biascr
         echo satang  = $satang
         echo radstat = $radstat
      else
         export PDATE=`ndate -06 $PDATE` 
         ctr=$(( $ctr + 1 ))
      fi
   done

else
   export MAKE_CTL=0
   echo RUN_ENVIR = $RUN_ENVIR
   cd ${WORKDIR}
   cd ..
   rm -rf ${WORKDIR} 
   exit 0
fi


#--------------------------------------------------------------------
#  Process if radstat file exists.
#--------------------------------------------------------------------
if [[ -s ${radstat} ]]; then
                                         

   #------------------------------------------------------------------
   #  make working directory, TANKverf_rad, TANKDIR
   #------------------------------------------------------------------
   mkdir -p $WORKDIR
   mkdir -p $TANKverf_rad
   mkdir -p $TANKDIR

   export DATDIRL=${WORKDIR}/datrad_$SUFFIX
   rm -rf $DATDIRL
   mkdir -p $DATDIRL
   cd $DATDIRL


   #------------------------------------------------------------------
   #  Copy data files file to local data directory.  
   #  Untar radstat file.  Change DATDIR definition.
   #------------------------------------------------------------------

   $NCP $biascr  $DATDIRL/biascr.$PDATE
   $NCP $satang  $DATDIRL/satang.$PDATE
   $NCP $radstat $DATDIRL/radstat.$PDATE

   tar -xvf radstat.$PDATE
   rm radstat.$PDATE

   export exp_satype=`ls -l d*ges* | sed -e 's/_/ /g;s/\./ /' | awk '{ print $10 "_" $11 }'`
   echo $exp_satype

   export DATDIR=$DATDIRL


   #------------------------------------------------------------------
   # Export variables
   #------------------------------------------------------------------
   export listvar=RAD_AREA,DO_DIAG_RPT,DO_DATA_RPT,PDATE,NDATE,DATDIR,TANKDIR,IMGNDIR,LOADLQ,EXEDIR,LOGDIR,SCRIPTS,USER_CLASS,SUB,SUFFIX,SATYPE,NCP,ACOUNT,MAIL_TO,MAIL_CC,DISCLAIMER,REGION,WORKDIR,MAKE_CTL,MAKE_DATA,listvar


   #------------------------------------------------------------------
   #   Submit data processing jobs to make ctl files.
   #------------------------------------------------------------------
   rm $LOGDIR/angle.log
   $SUB -a $ACOUNT -e $listvar -j MkCtl_angle_${SUFFIX} -q dev -g ${USER_CLASS} -t 0:15:00 -o $LOGDIR/mkctl_angle.log $SCRIPTS/verf_angle.sh

   rm $LOGDIR/bcoef.log
   $SUB -a $ACOUNT -e $listvar -j MkCtl_bcoef_${SUFFIX} -q dev -g ${USER_CLASS} -t 0:15:00 -o $LOGDIR/mkctl_bcoef.log $SCRIPTS/verf_bcoef.sh

   rm $LOGDIR/bcor.log
   $SUB -a $ACOUNT -e $listvar -j MkCtl_bcor_${SUFFIX} -q dev -g ${USER_CLASS} -t 0:15:00 -o $LOGDIR/mkctl_bcor.log $SCRIPTS/verf_bcor.sh

   rm $LOGDIR/time.log
   $SUB -a $ACOUNT -e $listvar -j MkCtl_time_${SUFFIX} -q dev -g ${USER_CLASS} -t 0:15:00 -o $LOGDIR/mkctl_time.log $SCRIPTS/verf_time.sh

else
   echo unable to locate a radstat file, check starting PDATE or DATDIR assignment.
fi

#--------------------------------------------------------------------
# Clean up and exit
#--------------------------------------------------------------------
cd $tmpdir
cd ../
rm -rf $tmpdir

echo end MkCtl_glbl.sh
exit
