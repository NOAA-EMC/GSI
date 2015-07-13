#!/bin/sh

function usage {
#  echo "Usage:  MinMonPlt.sh SUFFIX [PDATE] [EDATE]"
  echo "Usage:  MinMonPlt.sh SUFFIX [PDATE]"
  echo "            SUFFIX is data source identifier that matches data in "
  echo "              the $M_TANKverf/stats directory."
  echo "            PDATE (format:  YYYYMMDDHH) optional, is only/first date to plot"
#  echo "            EDATE (format:  YYYYMMDDHH) optional, is last date to plot"
}

set -ax
echo start MinMonPlt.sh

nargs=$#
if [[ $nargs -lt 1 ]]; then
   usage
   exit 1
fi

export SUFFIX=$1

if [[ $nargs -ge 2 ]]; then
   export PDATE=$2
fi
#if [[ $nargs -eq 3 ]]; then
#   export EDATE=$3
#fi

this_file=`basename $0`
this_dir=`dirname $0`

#--------------------------------------------------
# source verison, config, and user_settings files
#--------------------------------------------------
top_parm=${this_dir}/../../parm

minmon_version_file=${minmon_version:-${top_parm}/MinMon.ver}
if [[ -s ${minmon_version_file} ]]; then
   . ${minmon_version_file}
   echo "able to source ${minmon_version_file}"
else
   echo "Unable to source ${minmon_version_file} file"
   exit 2
fi

minmon_config=${minmon_config:-${top_parm}/MinMon_config}
if [[ -s ${minmon_config} ]]; then
   . ${minmon_config}
   echo "able to source ${minmon_config}"
else
   echo "Unable to source ${minmon_config} file"
   exit 3
fi

minmon_user_settings=${minmon_user_settings:-${top_parm}/MinMon_user_settings}
if [[ -s ${minmon_user_settings} ]]; then
   . ${minmon_user_settings}
   echo "able to source ${minmon_user_settings}"
else
   echo "Unable to source ${minmon_user_settings} file"
   exit 4
fi

plot_minmon_conf=${plot_minmon_conf:-${M_IG_PARM}/plot_minmon_conf}
if [[ -s ${plot_minmon_conf} ]]; then
   . ${plot_minmon_conf}
   echo "able to source ${plot_minmon_conf}"
else
   echo "Unable to source ${plot_minmon_conf} file"
   exit 5
fi


#--------------------------------------------------------------------
#  Check for my monitoring use.  Abort if running on prod machine.
#--------------------------------------------------------------------
if [[ RUN_ONLY_ON_DEV =  1 ]]; then
   is_prod=`${M_IG_SCRIPTS}/onprod.sh`
   if [[ $is_prod = 1 ]]; then
      exit 10
   fi
fi


#--------------------------------------------------------------------
#  Specify TANKDIR for this suffix
#--------------------------------------------------------------------
if [[ $GLB_AREA -eq 1 ]]; then
   export TANKDIR=${M_TANKverf}/stats/${SUFFIX}
else
   export TANKDIR=${M_TANKverf}/stats/regional/${SUFFIX}
fi

#--------------------------------------------------------------------
#  If PDATE wasn't specified as an argument then plot the last
#  available cycle.
#--------------------------------------------------------------------
if [[ ${#PDATE} -le 0 ]]; then
   echo "PDATE not specified:  setting PDATE using last cycle"
   export PDATE=`${M_IG_SCRIPTS}/find_cycle.pl GDAS 1 ${TANKDIR}`
else
   echo "PDATE was specified:  $PDATE"
fi

#--------------------------------------------------------------------
#  Create the WORKDIR and link the data files to it
#--------------------------------------------------------------------
if [[ -d $WORKDIR ]]; then
  rm -rf $WORKDIR
fi
mkdir $WORKDIR
cd $WORKDIR

#--------------------------------------------------------------------
#  Copy gnorm_data.txt file to WORKDIR.
#--------------------------------------------------------------------
pdy=`echo $PDATE|cut -c1-8`
gnorm_file=${TANKDIR}/minmon.${pdy}/${SUFFIX}.gnorm_data.txt
local_gnorm=gnorm_data.txt

if [[ -s ${gnorm_file} ]]; then
   cp ${gnorm_file} ./${local_gnorm}
else
   echo "WARNING:  Unable to locate ${gnorm_file}!"
fi

#------------------------------------------------------------------
#  Copy the cost.txt and cost_terms.txt files files locally
#
#  These aren't used for processing but will be pushed to the
#    server from the tmp dir.
#------------------------------------------------------------------
costs=${TANKDIR}/minmon.${pdy}/${SUFFIX}.${PDATE}.costs.txt
cost_terms=${TANKDIR}/minmon.${pdy}/${SUFFIX}.${PDATE}.cost_terms.txt

if [[ -s ${costs} ]]; then
   cp ${costs} ${WORKDIR}/${SUFFIX}.${PDATE}.costs.txt
else
   echo "WARNING:  Unable to locate ${costs}"
fi

if [[ -s ${cost_terms} ]]; then
  cp ${cost_terms} ${WORKDIR}/${SUFFIX}.${PDATE}.cost_terms.txt 
else
   echo "WARNING:  Unable to locate ${cost_terms}"
fi


bdate=`$NDATE -174 $PDATE`
edate=$PDATE
cdate=$bdate

#------------------------------------------------------------------
#  Add links for required data files (gnorms and reduction) to 
#   enable calculation of 7 day average
#------------------------------------------------------------------
while [[ $cdate -le $edate ]]; do
   echo "processing cdate = $cdate"
   pdy=`echo $cdate|cut -c1-8`

   gnorms_file=${TANKDIR}/minmon.${pdy}/${SUFFIX}.${cdate}.gnorms.ieee_d
   local_gnorm=${cdate}.gnorms.ieee_d

   reduct_file=${TANKDIR}/minmon.${pdy}/${SUFFIX}.${cdate}.reduction.ieee_d
   local_reduct=${cdate}.reduction.ieee_d

   if [[ -s ${gnorms_file} ]]; then
      ln -s ${gnorms_file} ${WORKDIR}/${local_gnorm}
   else
      echo "WARNING:  Unable to locate ${gnorms_file}"
   fi
   if [[ -s ${reduct_file} ]]; then
      ln -s ${reduct_file} ${WORKDIR}/${local_reduct}
   else
      echo "WARNING:  Unable to locate ${reduct_file}"
   fi

   adate=`$NDATE +6 $cdate`
   cdate=$adate
done


#--------------------------------------------------------------------
#  Main processing loop.  
#  Run extract_all_gnorms.pl script and generate single cycle plot.
#
#  RM this loop or add an optional end date to the args list and 
#  process each date in turn.
#
#  And alternate plot method might be to simply plot the last 
#  available cycle if no PDATE is included.  Could use find_cycle.pl
#  to find the last one and done.
#
#  Also should an attempt to plot a date for which there is no data
#  produce an error exit?  I think so.
#--------------------------------------------------------------------
not_done=1
ctr=0
area=glb
if [[ $GLB_AREA -eq 0 ]]; then
   area=rgn
fi

while [ $not_done -eq 1 ] && [ $ctr -le 20 ]; do

   #-----------------------------------------------------------------
   #  copy over the control files and update the tdef lines 
   #  according to the $suffix
   #-----------------------------------------------------------------
   if [[ ! -e ${WORKDIR}/allgnorm.ctl ]]; then
      cp ${M_IG_GRDS}/${area}_allgnorm.ctl ${WORKDIR}/allgnorm.ctl
   fi
 
   if [[ ! -e ${WORKDIR}/reduction.ctl ]]; then
      cp ${M_IG_GRDS}/${area}_reduction.ctl ${WORKDIR}/reduction.ctl
   fi

  
   # 
   # update the tdef line in the ctl files
   # 
   bdate=`$NDATE -168 $PDATE`
   ${M_IG_SCRIPTS}/update_ctl_tdef.sh ${WORKDIR}/allgnorm.ctl ${bdate}
   ${M_IG_SCRIPTS}/update_ctl_tdef.sh ${WORKDIR}/reduction.ctl ${bdate}

#   if [[ $AREA = "glb" ]]; then
#      ${SCRIPTS}/update_ctl_xdef.sh ${WORKDIR}/allgnorm.ctl 202 
#   fi

   #######################
   # Q:  does NDAS really use 101 instead of 102?  That can't be somehow....
   #######################

   if [[ $SUFFIX = "RAP" ]]; then
      ${M_IG_SCRIPTS}/update_ctl_xdef.sh ${WORKDIR}/allgnorm.ctl 102 
   fi

   #-----------------------------------------------------------------
   #  Copy the plot script and build the plot driver script 
   #-----------------------------------------------------------------
   if [[ ! -e ${WORKDIR}/plot_gnorms.gs ]]; then
      cp ${M_IG_GRDS}/plot_gnorms.gs ${WORKDIR}/.
   fi
   if [[ ! -e ${WORKDIR}/plot_reduction.gs ]]; then
      cp ${M_IG_GRDS}/plot_reduction.gs ${WORKDIR}/.
   fi
 
 
cat << EOF >${PDATE}_plot_gnorms.gs
'open allgnorm.ctl'
'run plot_gnorms.gs $SUFFIX $PDATE x1100 y850'
'quit'
EOF

cat << EOF >${PDATE}_plot_reduction.gs
'open reduction.ctl'
'run plot_reduction.gs $SUFFIX $PDATE x1100 y850'
'quit'
EOF

  #-----------------------------------------------------------------
  #  Run the plot driver script and move the image into ./tmp
  #-----------------------------------------------------------------
  GRADS=`which grads`
  $TIMEX $GRADS -blc "run ${PDATE}_plot_gnorms.gs"
  $TIMEX $GRADS -blc "run ${PDATE}_plot_reduction.gs"

  if [[ ! -d ${WORKDIR}/tmp ]]; then
     mkdir ${WORKDIR}/tmp
  fi
  mv *.png tmp/.

  #-----------------------------------------------------------------
  #  copy the modified gnorm_data.txt file to tmp
  #-----------------------------------------------------------------
  cp gnorm_data.txt tmp/${SUFFIX}.gnorm_data.txt

 
  ctr=`expr $ctr + 1`
done

#-----------------------------------------------------------------
# copy all cost files to tmp 
#-----------------------------------------------------------------
cp *cost*.txt tmp/.

#--------------------------------------------------------------------
#  If error reporting is enabled:
#    - if there is an errmsg.txt for this cycle
#      then mail it to the MAIL_TO and MAIL_CC recipients
#--------------------------------------------------------------------
if [[ ${DO_ERROR_RPT} -eq 1 ]]; then

   err_msg=${TANKDIR}/minmon.${pdy}/${SUFFIX}.${PDATE}.errmsg.txt
   if [[ -e $err_msg ]]; then
      err_rpt="./err_rpt.txt"
      `cat $err_msg > $err_rpt`
      echo "" >> $err_rpt
      echo "" >> $err_rpt
      echo "" >> $err_rpt
      echo "*********************** WARNING ***************************" >> $err_rpt
      echo "THIS IS AN AUTOMATED EMAIL.  REPLIES TO SENDER WILL NOT BE"  >> $err_rpt
      echo "RECEIVED.  PLEASE DIRECT REPLIES TO $MAIL_TO"                >> $err_rpt
      echo "*********************** WARNING ***************************" >> $err_rpt
     
      if [[ $MAIL_CC == "" ]]; then
         /bin/mail -s RadMon_error_report ${MAIL_TO}< ${err_rpt}
      else
         /bin/mail -s RadMon_error_report -c "${MAIL_CC}" ${MAIL_TO}< ${err_rpt}
      fi
   fi
  
#if [[ -s ${err_rpt} ]]; then
#      lines=`wc -l <${err_rpt}`
#      if [[ $lines -gt 2 ]]; then
#echo "" >> $err_rpt
#echo "" >> $err_rpt
#echo "" >> $err_rpt
#echo "*********************** WARNING ***************************" >> $err_rpt
#echo "THIS IS AN AUTOMATED EMAIL.  REPLIES TO SENDER WILL NOT BE"  >> $err_rpt
#echo "RECEIVED.  PLEASE DIRECT REPLIES TO edward.safford@noaa.gov" >> $err_rpt
#echo "*********************** WARNING ***************************" >> $err_rpt
#
#         if [[ $MAIL_CC == "" ]]; then
#            /bin/mail -s RadMon_error_report ${MAIL_TO}< ${err_rpt}
#         else
#            /bin/mail -s RadMon_error_report -c "${MAIL_CC}" ${MAIL_TO}< ${err_rpt}
#         fi
#      fi
#   fi


fi

#--------------------------------------------------------------------
#  Push the image & txt files over to the server
#--------------------------------------------------------------------
   if [[ $MY_MACHINE = "wcoss" ]]; then
      cd ./tmp
      $RSYNC -ave ssh --exclude *.ctl*  ./ \
        ${WEBUSER}@${WEBSERVER}:${WEBDIR}/
   fi
#--------------------------------------------------------------------
#  Call update_save.sh to copy latest 15 days worth of data files 
#  from $TANKDIR to /sss.../da/save so prod machine can access the 
#  same data.
#--------------------------------------------------------------------

#   ${SCRIPTS}/update_sss.sh

#cd ${WORKDIR}
#cd ..
#rm -rf ${WORKDIR}

echo end MinMonPlt.sh
exit
