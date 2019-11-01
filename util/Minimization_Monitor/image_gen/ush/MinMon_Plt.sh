#!/bin/sh
set -ax

function usage {
  echo " "
  echo "Usage:  MinMonPlt.sh MINMON_SUFFIX "
  echo "            MINMON_SUFFIX is data source identifier that matches data in "
  echo "              the $M_TANKverf/stats directory."
  echo "            -p | --pdate yyyymmddcc to specify the cycle to be plotted"
  echo "              if unspecified the last available date will be plotted"
  echo "            -r | --run   the gdas|gfs run to be plotted"
  echo "              use only if data in TANKdir stores both runs" 
  echo " "
}

echo start MinMonPlt.sh

nargs=$#
if [[ $nargs -lt 1 || $nargs -gt 5 ]]; then
   usage
   exit 1
fi

RUN=gdas
while [[ $# -ge 1 ]]
do
   key="$1"
   echo $key

   case $key in
      -p|--pdate)
         PDATE="$2"
         shift # past argument
      ;;
      -r|--run)
         export RUN="$2"
         shift # past argument
      ;;
      *)
         #any unspecified key is MINMON_SUFFIX
         export MINMON_SUFFIX=$key
      ;;
   esac

   shift
done

if [[ ${#RUN} -le 0 ]]; then 
   export RUN=gdas
fi

echo "MINMON_SUFFIX = $MINMON_SUFFIX"
echo "PDATE         = $PDATE"
echo "RUN           = $RUN"



if [[ ${#RUN} -gt 0 ]]; then 
   run_suffix=${MINMON_SUFFIX}_${RUN}
else
   run_suffix=${MINMON_SUFFIX}
fi

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


#--------------------------------------------------------------------
#  Specify TANKDIR for this suffix
#--------------------------------------------------------------------
if [[ $GLB_AREA -eq 1 ]]; then
   export TANKDIR=${M_TANKverf}/stats/${MINMON_SUFFIX}
else
   export TANKDIR=${M_TANKverf}/stats/regional/${MINMON_SUFFIX}
fi

#--------------------------------------------------------------------
#  If PDATE wasn't specified as an argument then plot the last
#  available cycle.
#--------------------------------------------------------------------
if [[ ${#PDATE} -le 0 ]]; then
   echo "PDATE not specified:  setting PDATE using last cycle"
   export PDATE=`${M_IG_SCRIPTS}/find_cycle.pl --cyc 1 --dir ${TANKDIR}`
else
   echo "PDATE was specified:  $PDATE"
fi


#--------------------------------------------------------------------
#  Create the WORKDIR and link the data files to it
#--------------------------------------------------------------------
pid=${pid:-$$}

if [[ ${#RUN} -gt 0 ]]; then
   WORKDIR=${WORKDIR}/IG.${RUN}.${PDATE}.o${pid}
else
   WORKDIR=${WORKDIR}/IG.${PDATE}.o${pid}
fi

if [[ ! -d $WORKDIR ]]; then
   mkdir -p $WORKDIR
fi
cd $WORKDIR

#--------------------------------------------------------------------
#  Copy gnorm_data.txt file to WORKDIR.
#--------------------------------------------------------------------
pdy=`echo $PDATE|cut -c1-8`
cyc=`echo $PDATE|cut -c9-10`
echo TANKDIR = ${TANKDIR}

gnorm_dir=${TANKDIR}/${RUN}.${pdy}/${cyc}/minmon

gnorm_file=${gnorm_dir}/gnorm_data.txt

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
costs=${gnorm_dir}/${PDATE}.costs.txt
cost_terms=${gnorm_dir}/${PDATE}.cost_terms.txt

if [[ -s ${costs} ]]; then
   cp ${costs} ${WORKDIR}/${run_suffix}.${PDATE}.costs.txt
else
   echo "WARNING:  Unable to locate ${costs}"
fi

if [[ -s ${cost_terms} ]]; then
  cp ${cost_terms} ${WORKDIR}/${run_suffix}.${PDATE}.cost_terms.txt 
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

   pdy=`echo $cdate | cut -c1-8`
   cyc=`echo $cdate | cut -c9-10`

   gnorm_dir=${TANKDIR}/${RUN}.${pdy}/${cyc}/minmon

   gnorms_file=${gnorm_dir}/${cdate}.gnorms.ieee_d
   local_gnorm=${cdate}.gnorms.ieee_d

   reduct_file=${gnorm_dir}/${cdate}.reduction.ieee_d
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
      cp ${M_IG_GRDS}/${area}_allgnorm.ctl ${WORKDIR}/orig_allgnorm.ctl
      cp ${WORKDIR}/orig_allgnorm.ctl ${WORKDIR}/allgnorm.ctl
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
   

   #######################
   # Q:  does NDAS really use 101 instead of 102?  That can't be somehow....
   #######################

#   if [[ $MINMON_SUFFIX = "RAP" ]]; then
#      ${M_IG_SCRIPTS}/update_ctl_xdef.sh ${WORKDIR}/allgnorm.ctl 102 
#   fi

   #-----------------------------------------------------------------
   #  Copy the plot script and build the plot driver script 
   #-----------------------------------------------------------------
   if [[ ! -e ${WORKDIR}/plot_gnorms.gs ]]; then
      cp ${M_IG_GRDS}/plot_gnorms.gs ${WORKDIR}/.
   fi
   if [[ ! -e ${WORKDIR}/plot_reduction.gs ]]; then
      cp ${M_IG_GRDS}/plot_reduction.gs ${WORKDIR}/.
   fi
   if [[ ! -e ${WORKDIR}/plot_4_gnorms.gs ]]; then
      cp ${M_IG_GRDS}/plot_4_gnorms.gs ${WORKDIR}/.
   fi

 
cat << EOF >${PDATE}_plot_gnorms.gs
'open allgnorm.ctl'
'run plot_gnorms.gs $run_suffix $PDATE x1100 y850'
'quit'
EOF

cat << EOF >${PDATE}_plot_reduction.gs
'open reduction.ctl'
'run plot_reduction.gs $run_suffix $PDATE x1100 y850'
'quit'
EOF

cat << EOF >${PDATE}_plot_4_gnorms.gs
'open allgnorm.ctl'
'run plot_4_gnorms.gs $run_suffix $PDATE x1100 y850'
'quit'
EOF


  #-----------------------------------------------------------------
  #  Run the plot driver script and move the image into ./tmp
  #-----------------------------------------------------------------
  GRADS=`which grads`

  $TIMEX $GRADS -blc "run ${PDATE}_plot_gnorms.gs"
  $TIMEX $GRADS -blc "run ${PDATE}_plot_reduction.gs"
  $TIMEX $GRADS -blc "run ${PDATE}_plot_4_gnorms.gs"

  if [[ ! -d ${WORKDIR}/tmp ]]; then
     mkdir ${WORKDIR}/tmp
  fi
  mv *.png tmp/.

  #-----------------------------------------------------------------
  #  copy the modified gnorm_data.txt file to tmp
  #-----------------------------------------------------------------
  cp gnorm_data.txt tmp/${run_suffix}.gnorm_data.txt

 
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

   err_msg=${TANKDIR}/${RUN}.${pdy}/${cyc}/minmon/${PDATE}.errmsg.txt

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
         /bin/mail -s MinMon_error_report ${MAIL_TO}< ${err_rpt}
      else
         /bin/mail -s MinMon_error_report -c "${MAIL_CC}" ${MAIL_TO}< ${err_rpt}
      fi
   fi

fi

#--------------------------------------------------------------------
#  Push the image & txt files over to the server
#--------------------------------------------------------------------
   if [[ ${MY_MACHINE} = "wcoss" || ${MY_MACHINE} = "cray" || \
	 ${MY_MACHINE} = "wcoss_d" ]]; then
      cd ./tmp
      $RSYNC -ave ssh --exclude *.ctl*  ./ \
        ${WEBUSER}@${WEBSERVER}:${WEBDIR}/$run_suffix/
   fi

#--------------------------------------------------------------------
#  Call nu_make_archive.sh to write archive files to hpss and
#  update the prod machine with any missing M_TANKDIR directories.
#--------------------------------------------------------------------
#   if [[ ${DO_ARCHIVE} -eq 1 ]]; then
#      ${M_IG_SCRIPTS}/nu_make_archive.sh
#   fi

#cd ${WORKDIR}
#cd ..
#rm -rf ${WORKDIR}

echo "end MinMonPlt.sh"
exit
