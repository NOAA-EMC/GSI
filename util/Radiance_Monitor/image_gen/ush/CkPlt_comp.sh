#!/bin/sh

#--------------------------------------------------------------------
#
#  CkPlt_comp.sh
#
#  This script reads the plot_comp_config file in the ../parm directory
#  and plots the requested comparision plots.  Data may be plotted 
#  from either global or regional sources.
#
#  Supported plots include:
#    plot_fs_obsnum_comp.sh
#
#  Note:  this does not generate any data files (*.ieee_d).  Those 
#  must be already created for this script to function correctly.
#
#--------------------------------------------------------------------

set -ax
echo start CkPlt_comp.sh

#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------

this_dir=`dirname $0`
top_parm=${this_dir}/../../parm

if [[ -s ${top_parm}/RadMon_config ]]; then
   . ${top_parm}/RadMon_config
else
   echo "Unable to source ${top_parm}/RadMon_config"
   exit
fi


. ${RADMON_IMAGE_GEN}/parm/plot_rad_conf

export PLOT_WORK_DIR=$PLOT_COMP_DIR
if [[ -d $PLOT_WORK_DIR ]]; then
   rm -rf $PLOT_WORK_DIR
fi
mkdir $PLOT_WORK_DIR


#--------------------------------------------------------------------
#  comp_conf is the configuration file which controls plotting
#  tmp_conf is the updated configuration file which will replace
#    comp_conf if any plotting is done.
#--------------------------------------------------------------------
comp_conf="${RADMON_IMAGE_GEN}/parm/plot_comp_config"
tmp_conf="${RADMON_IMAGE_GEN}/parm/.tmp_comp_config"
>${tmp_conf}


#--------------------------------------------------------------------
#  Step through plot_comp_config file and run requested comparison
#  plots.
#--------------------------------------------------------------------
ctr=0
plots_done=0

while read line; do

   test=`echo $line | nawk '{print $1}'`
   test1=`echo $test | cut -c1`      

   if [[ $test1 != "#" ]]; then
      found_plot=1
      echo "found $plot"
      plot=$test
      last_run=`echo $line | nawk '{print $2}'`
      area=`echo $line | nawk '{print $3}'`
      data=`echo $line | nawk '{print $4}'`
      suffix1=`echo $line | nawk '{print $5}'`
      suffix2=`echo $line | nawk '{print $6}'`
      suffix3=`echo $line | nawk '{print $7}'`

      short_plot=`echo $plot | cut -f 2- -d "_" | cut -f 1 -d "."`

      echo $plot
      echo $short_plot
      echo $area
      echo $data
      echo $last_run
      echo $suffix1
      echo $suffix2
      echo $suffix3

      export SUFFIX=${suffix1}




      if [[ $area == "glb" ]]; then
         . ${RADMON_IMAGE_GEN}/parm/glbl_comp_conf
         export RAD_AREA=$area
      elif [[ $area == "rgn" ]]; then
         . ${RADMON_IMAGE_GEN}/parm/rgnl_comp_conf
         export RAD_AREA=$area
      else
         echo unable to parse area assignment in ${comp_conf}
         export RAD_AREA=
      fi

      if [[ $data != "ges" && $data != "anl" ]]; then
         echo unable to parse data assignment in ${comp_conf}
         echo will use ges data by default
         data="ges"
      fi

      if [[ $area == "glb" || $area == "rgn" ]]; then
         mkdir -p $LOGDIR

         #--------------------------------------------------------------
         # Set up SUFFIX, TANKDIR and IMGNDIR for this plot.
         #--------------------------------------------------------------
         suff1=`echo ${#suffix1}`
         if [[ $suff1 -gt 0 ]]; then
            export SUFFIX1=$suffix1
            export TANKDIR1=${TANKDIR}/${SUFFIX1}
            prodate1=`${SCRIPTS}/get_prodate.sh ${SUFFIX1} ${DATA_MAP}`
         else
            exit
         fi

         suff2=`echo ${#suffix2}`
         if [[ $suff2 -gt 0 ]]; then
            export SUFFIX2=$suffix2
            export TANKDIR2=${TANKDIR}/${SUFFIX2}
            prodate2=`${SCRIPTS}/get_prodate.sh ${SUFFIX2} ${DATA_MAP}`
         else
            exit
         fi

         suff3=`echo ${#suffix3}`
         if [[ $suff3 -gt 0 ]]; then
            export SUFFIX3=$suffix3
            export TANKDIR3=${TANKDIR}/${SUFFIX3}
            prodate3=`${SCRIPTS}/get_prodate.sh ${SUFFIX3} ${DATA_MAP}`
         fi

         #--------------------------------------------------------------
         # Get date of cycle to process.  Make sure all sources have
         # data for the desired cycle.  Exit if any have not processed
         # the requested cycle.
         #--------------------------------------------------------------

         export PDATE=`$NDATE +06 $last_run`  

         abort_run=0
         if [[ ${prodate1} -lt $PDATE ]]; then
            echo $SUFFIX1 processing date $prodate1 is not up to $PDATE
            abort_run=1
         fi
         if [[ ${prodate2} -lt $PDATE ]]; then
            echo $SUFFIX2 processing date $prodate2 is not up to $PDATE
            abort_run=1
         fi
         if [[ $suff3 -gt 0 ]]; then
            if [[ ${prodate3} -lt $PDATE ]]; then
               echo $SUFFIX3 processing date $prodate3 is not up to $PDATE
               abort_run=1
            fi
         fi

         #-------------------------------------------------------------
         #  Get the SATYPE for SUFFIX
         #
         export USE_STATIC_SATYPE=`${SCRIPTS}/get_satype.sh ${SUFFIX} ${DATA_MAP}`

         #-------------------------------------------------------------
         #  If USE_STATIC_SATYPE == 0 then assemble the SATYPE list from
         #  available data files in $TANKDIR1/angle
         #  If USE_STATIC_SATYPE == 1 then load SATYPE from the SATYPE.txt
         #  file.
         #-------------------------------------------------------------
         if [[ $USE_STATIC_SATYPE -eq 0 ]]; then
   
            test_list=`ls $TANKDIR1/angle/*.${PDATE}.ieee_d*`

            for test in ${test_list}; do
               this_file=`basename $test`
               tmp=`echo "$this_file" | cut -d. -f1`
               echo $tmp
               SATYPE_LIST="$SATYPE_LIST $tmp"
            done

            SATYPE=$SATYPE_LIST

         else
            TANKDIR_INFO=${TANKDIR1}/info
            STATIC_SATYPE_FILE=${TANKDIR_INFO}/SATYPE.txt

            #-------------------------------------------------------------
            #  Load the SATYPE list from the STATIC_SATYPE_FILE or exit
            #  if unable to locate it.
            #-------------------------------------------------------------
            if [[ -s $STATIC_SATYPE_FILE ]]; then
               SATYPE=""
               SATYPE=`cat ${STATIC_SATYPE_FILE}`
            else
               echo Unable to locate $STATIC_SATYPE_FILE, must exit.
               abort_run=1
            fi
         fi

         echo $SATYPE

         echo abort_run = $abort_run

         if [[ $abort_run == "0" ]]; then

            #------------------------------------------------------------------
            # Export variables and submit plot script
            #------------------------------------------------------------------
            export listvar=PARM,RAD_AREA,PDATE,NDATE,TANKDIR1,TANKDIR2,TANKDIR3,IMGNDIR1,IMGNDIR2,IMGNDIR3,LOADLQ,LLQ,WEB_SVR,WEB_USER,WEBDIR,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,U_USER,PTMP_USER,STMP_USER,USER_CLASS,SUB,SUFFIX,SUFFIX1,SUFFIX2,SUFFIX3,SATYPE,NCP,PLOT_WORK_DIR,ACOUNT,DISCLAIMER,listvar


            #------------------------------------------------------------------
            # submit plot script
            #------------------------------------------------------------------
            ctr=$(( $ctr + 1 ))
            cmdfile=${PLOT_WORK_DIR}/cmdfile_${short_plot}_${RAD_AREA}_${ctr}
            jobname=plot_${short_plot}_${RAD_AREA}_${ctr}	
            rm -f $cmdfile

            for type in ${SATYPE}; do
               echo ${plot} ${type} ${data} >> $cmdfile
            done

            ntasks=`cat $cmdfile|wc -l`
            ((nprocs=(ntasks+1)/2))

            $SUB -a $ACOUNT -e $listvar -j ${jobname} -u $USER -t 0:10:00 -o $LOGDIR/plot_${short_plot}.log -p $ntasks/1/N -q dev -g ${USER_CLASS}  /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered
            plots_done=1

            echo "$plot $PDATE $RAD_AREA $data $suffix1 $suffix2 $suffix3" >> $tmp_conf

         else
            echo $line >> $tmp_conf 
         fi
      fi
   else
      echo $line >> $tmp_conf 
   fi

   #------------------------------------------------------------------
   #  empty out all the suffix assignments for the next run
   #------------------------------------------------------------------
   suffix1=;suffix2=;suffix3=
   SUFFIX1=;SUFFIX2=;SUFFIX3= 

done < "${comp_conf}"


#------------------------------------------------------------------
#  If plotting occured then replace comp_conf with updated time(s).
#------------------------------------------------------------------
if [[ ${plots_done} == "1" ]]; then
  mv -f ${tmp_conf} ${comp_conf} 
fi


echo end CkPlt_comp.sh

exit
