#!/bin/sh

#--------------------------------------------------------------------
#
#  plot_fs_obsnum_comp.sh
#
#  This script generates a comparison plot of observation counts and
#  ges with bias correction from two or three data sources.
#
#  Note:  this does not generate any data files.  Those must be 
#  already created for this script to function correctly.
#
#  See the ../parm/plot_*_comp 
#--------------------------------------------------------------------
echo start plot_fs_obsnum_comp.sh

set -ax
date
export list=$listvar

type=$1
data=$2	  #ges or anl, ges is default

#
# set anl if we're plotting analysis data
#
anl=
if [[ $data == "anl" ]]; then
   anl=_anl
fi

suff3=${#SUFFIX3}


workdir=${PLOT_WORK_DIR}/plot_fs_obsnum_${RAD_AREA}_${type}${anl}
mkdir -p ${workdir}
cd ${workdir}

#------------------------------------------------------------------
#   Copy the data files over and rename according to SUFFIX 
#------------------------------------------------------------------
PDY=`echo $PDATE|cut -c1-8`

if [[ -s ${TANKDIR1}/radmon.${PDY}/time.${type}.${PDATE}${anl}.ieee_d.Z ]]; then
   $NCP ${TANKDIR1}/radmon.${PDY}/time.${type}.${PDATE}${anl}.ieee_d.Z ${workdir}/${SUFFIX1}.${PDATE}.ieee_d.Z
else
   $NCP $TANKDIR1/time/${type}.${PDATE}${anl}.ieee_d.Z ${workdir}/${SUFFIX1}.${PDATE}.ieee_d.Z
fi

if [[ -s ${TANKDIR2}/radmon.${PDY}/time.${type}.${PDATE}${anl}.ieee_d.Z ]]; then
   $NCP ${TANKDIR2}/radmon.${PDY}/time.${type}.${PDATE}${anl}.ieee_d.Z ${workdir}/${SUFFIX2}.${PDATE}.ieee_d.Z
else
   $NCP $TANKDIR2/time/${type}.${PDATE}${anl}.ieee_d.Z ${workdir}/${SUFFIX2}.${PDATE}.ieee_d.Z
fi

if [[ $suff3 -gt 0 ]]; then
   if [[ -s ${TANKDIR3}/radmon.${PDY}/time.${type}.${PDATE}${anl}.ieee_d.Z ]]; then
      $NCP ${TANKDIR3}/radmon.${PDY}/time.${type}.${PDATE}${anl}.ieee_d.Z ${workdir}/${SUFFIX3}.${PDATE}.ieee_d.Z
   else
      $NCP $TANKDIR3/time/${type}.${PDATE}${anl}.ieee_d.Z ${workdir}/${SUFFIX3}.${PDATE}.ieee_d.Z
   fi
fi
uncompress ${workdir}/*.Z

#------------------------------------------------------------------
#   Copy a control file over, update the time, rename according 
#   to SUFFIX, and change the data file name according to SUFFIX. 
#------------------------------------------------------------------
ctldir=
if [[ -s ${IMGNDIR1}/time/${type}${anl}.ctl.Z || -s ${IMGNDIR1}/time/${type}${anl}.ctl ]]; then
   ctldir="${IMGNDIR1}/time" 
elif [[ -s ${TANKDIR1}/radmon.${PDY}/time.${type}${anl}.ctl.Z || -s ${TANKDIR1}/radmon.${PDY}/time.${type}${anl}.ctl ]]; then
   ctldir=${TANKDIR1}/radmon.${PDY}
elif [[ -s ${TANKDIR1}/time/${type}${anl}.ctl.Z || -s ${TANKDIR1}/time/${type}${anl}.ctl ]]; then
   ctldir=${TANKDIR1}/time 
elif [[ -s ${IMGNDIR2}/time/${type}${anl}.ctl.Z || -s ${IMGNDIR2}/time/${type}${anl}.ctl ]]; then
   ctldir=${IMGNDIR2}/time 
elif [[ -s ${TANKDIR2}/radmon.${PDY}/time.${type}${anl}.ctl.Z || -s ${TANKDIR2}/radmon.${PDY}/time.${type}${anl}.ctl ]]; then
   ctldir=${TANKDIR2}/radmon.${PDY}
elif [[ -s ${TANKDIR2}/time/${type}${anl}.ctl.Z || -s ${TANKDIR2}/time/${type}${anl}.ctl ]]; then
   ctldir=${TANKDIR2}/time 
elif [[ $suff3 -gt 0 ]]; then
   if [[ -s ${IMGNDIR3}/time/${type}${anl}.ctl.Z || -s ${IMGNDIR3}/time/${type}${anl}.ctl ]]; then
      ctldir=${IMGNDIR3}/time 
   elif [[ -s ${TANKDIR3}/radmon.${PDY}/time.${type}${anl}.ctl.Z || -s ${TANKDIR3}/radmon.${PDY}/time.${type}${anl}.ctl ]]; then
      ctldir=${TANKDIR3}/radmon.${PDY}
   elif [[ -s ${TANKDIR3}/time/${type}${anl}.ctl.Z || -s ${TANKDIR3}/time/${type}${anl}.ctl ]]; then
      ctldir=${TANKDIR3}/time 
   fi
else
   echo "Unable to locate any ctl file"
fi

nctldir=${#ctldir}
if [[ ${nctldir} -gt 0 ]]; then 
   if [[ -s ${ctldir}/time.${type}${anl}.ctl.Z ]]; then 
      $NCP ${ctldir}/time.${type}${anl}.ctl.Z ${workdir}/${type}${anl}.ctl.Z
   elif [[ -s ${ctldir}/time.${type}${anl}.ctl ]]; then
      $NCP ${ctldir}/time.${type}${anl}.ctl ${workdir}/${type}${anl}.ctl
   else
      $NCP ${ctldir}/${type}${anl}.ctl* ${workdir}/.
   fi

   if [[ -s ${workdir}/${type}${anl}.ctl.Z ]]; then
      uncompress ${workdir}/${type}${anl}.ctl.Z
   fi

   ${SCRIPTS}/update_ctl_tdef.sh ${type}${anl}.ctl ${PDATE}

   $NCP ${type}${anl}.ctl ${SUFFIX1}${anl}.ctl
   ${SCRIPTS}/update_ctl_fname.sh ${SUFFIX1}${anl}.ctl ${SUFFIX1} 

   $NCP ${type}${anl}.ctl ${SUFFIX2}${anl}.ctl
   ${SCRIPTS}/update_ctl_fname.sh ${SUFFIX2}${anl}.ctl ${SUFFIX2} 

   if [[ $suff3 -gt 0 ]]; then
      $NCP ${type}${anl}.ctl ${SUFFIX3}${anl}.ctl
      ${SCRIPTS}/update_ctl_fname.sh ${SUFFIX3}${anl}.ctl ${SUFFIX3} 
   fi

   $NCP ${GSCRIPTS}/setrange.gs ${workdir}/setrange.gs

   run_line="run ${GSCRIPTS}/plot_fs_obsnum_comp.gs ${type} ${data} ${SUFFIX1} ${SUFFIX2}"

   if [[ $suff3 -gt 0 ]]; then 
      plotscript=${type}_${SUFFIX1}_${SUFFIX2}_${SUFFIX3}${anl}.gs
cat << EOF > ${workdir}/${plotscript}
'open ${SUFFIX1}${anl}.ctl'
'open ${SUFFIX2}${anl}.ctl'
'open ${SUFFIX3}${anl}.ctl'
'run ${GSCRIPTS}/plot_fs_obsnum_comp.gs ${type} ${data} ${SUFFIX1} ${SUFFIX2} ${SUFFIX3}'
'quit'
EOF
   else
      plotscript=${type}_${SUFFIX1}_${SUFFIX2}${anl}.gs
cat << EOF > ${workdir}/${plotscript}
'open ${SUFFIX1}${anl}.ctl'
'open ${SUFFIX2}${anl}.ctl'
'run ${GSCRIPTS}/plot_fs_obsnum_comp.gs ${type} ${data} ${SUFFIX1} ${SUFFIX2}'
'quit'
EOF
   fi

   timex $GRADS -bpc "run ${workdir}/${plotscript}"

   #------------------------------------------------------------------
   #  copy to web server
   #------------------------------------------------------------------
   export SUFFIX=${SUFFIX1}
   ssh -l ${WEB_USER} ${WEB_SVR} "mkdir -p ${WEBDIR}/comp"
   scp *.png ${WEB_USER}@${WEB_SVR}:${WEBDIR}/comp/.


   #------------------------------------------------------------------
   #  clean up files
   #------------------------------------------------------------------
#   cd ${workdir}
#   cd ../
#   rm -rf $workdir
      
fi


echo end plot_fs_obsnum_comp.sh

exit
