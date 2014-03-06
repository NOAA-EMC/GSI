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


workdir=${PLOT_WORK_DIR}/plot_fs_obsnum_${SUFFIX1}_${type}${anl}
mkdir -p ${workdir}
cd ${workdir}



#------------------------------------------------------------------
#   Copy the data files over and rename according to SUFFIX 
#------------------------------------------------------------------
PDY=`echo $PDATE|cut -c1-8`

target1=${TANKDIR1}/radmon.${PDY}/time.${type}.${PDATE}${anl}.ieee_d
dest1=${workdir}/${SUFFIX1}.${PDATE}.ieee_d
if [[ -s ${target1}.${Z} ]]; then 
   $NCP ${target1}.${Z} ${dest1}.${Z}
else
   $NCP ${target1} ${dest1}
fi

target2=${TANKDIR2}/radmon.${PDY}/time.${type}.${PDATE}${anl}.ieee_d
dest2=${workdir}/${SUFFIX2}.${PDATE}.ieee_d
if [[ -s ${target2}.${Z} ]]; then 
   $NCP ${target2}.${Z} ${dest2}.${Z}
else
   $NCP ${target2} ${dest2}
fi


if [[ $suff3 -gt 0 ]]; then
   target3=${TANKDIR3}/radmon.${PDY}/time.${type}.${PDATE}${anl}.ieee_d
   dest3=${workdir}/${SUFFIX3}.${PDATE}.ieee_d
   if [[ -s ${target3}.${Z} ]]; then 
      $NCP ${target3}.${Z} ${dest3}.${Z}
   else
      $NCP ${target3} ${dest3}
   fi
fi

${UNCOMPRESS} ${workdir}/*.${Z}

#------------------------------------------------------------------
#   Copy a control file over, update the time, rename according 
#   to SUFFIX, and change the data file name according to SUFFIX. 
#------------------------------------------------------------------
ctldir=
if [[ -s ${IMGNDIR1}/time/${type}${anl}.ctl.${Z} || -s ${IMGNDIR1}/time/${type}${anl}.ctl ]]; then
   ctldir="${IMGNDIR1}/time" 
elif [[ -s ${TANKDIR1}/radmon.${PDY}/time.${type}${anl}.ctl.${Z} || -s ${TANKDIR1}/radmon.${PDY}/time.${type}${anl}.ctl ]]; then
   ctldir=${TANKDIR1}/radmon.${PDY}
elif [[ -s ${TANKDIR1}/time/${type}${anl}.ctl.${Z} || -s ${TANKDIR1}/time/${type}${anl}.ctl ]]; then
   ctldir=${TANKDIR1}/time 
elif [[ -s ${IMGNDIR2}/time/${type}${anl}.ctl.${Z} || -s ${IMGNDIR2}/time/${type}${anl}.ctl ]]; then
   ctldir=${IMGNDIR2}/time 
elif [[ -s ${TANKDIR2}/radmon.${PDY}/time.${type}${anl}.ctl.${Z} || -s ${TANKDIR2}/radmon.${PDY}/time.${type}${anl}.ctl ]]; then
   ctldir=${TANKDIR2}/radmon.${PDY}
elif [[ -s ${TANKDIR2}/time/${type}${anl}.ctl.${Z} || -s ${TANKDIR2}/time/${type}${anl}.ctl ]]; then
   ctldir=${TANKDIR2}/time 
elif [[ $suff3 -gt 0 ]]; then
   if [[ -s ${IMGNDIR3}/time/${type}${anl}.ctl.${Z} || -s ${IMGNDIR3}/time/${type}${anl}.ctl ]]; then
      ctldir=${IMGNDIR3}/time 
   elif [[ -s ${TANKDIR3}/radmon.${PDY}/time.${type}${anl}.ctl.${Z} || -s ${TANKDIR3}/radmon.${PDY}/time.${type}${anl}.ctl ]]; then
      ctldir=${TANKDIR3}/radmon.${PDY}
   elif [[ -s ${TANKDIR3}/time/${type}${anl}.ctl.${Z} || -s ${TANKDIR3}/time/${type}${anl}.ctl ]]; then
      ctldir=${TANKDIR3}/time 
   fi
else
   echo "Unable to locate any ctl file"
fi

nctldir=${#ctldir}
if [[ ${nctldir} -gt 0 ]]; then 
   if [[ -s ${ctldir}/time.${type}${anl}.ctl.${Z} ]]; then 
      $NCP ${ctldir}/time.${type}${anl}.ctl.${Z} ${workdir}/${type}${anl}.ctl.${Z}
   elif [[ -s ${ctldir}/time.${type}${anl}.ctl ]]; then
      $NCP ${ctldir}/time.${type}${anl}.ctl ${workdir}/${type}${anl}.ctl
   else
      $NCP ${ctldir}/${type}${anl}.ctl* ${workdir}/.
   fi

   if [[ -s ${workdir}/${type}${anl}.ctl.${Z} ]]; then
      ${UNCOMPRESS} ${workdir}/${type}${anl}.ctl.${Z}
   fi

   #-------------------------------------------------------------------
   #   Update the time definition (tdef) line in the angle control
   #   files. Conditionally rm "cray_32bit_ieee" from the options line.
  
   ctl_file=${type}${anl}.ctl 

   ${IG_SCRIPTS}/update_ctl_tdef.sh ${ctl_file} ${PDATE}

   if [[ $MY_MACHINE = "wcoss" ]]; then
      sed -e 's/cray_32bit_ieee/ /' ${ctl_file} > tmp_${type}.ctl
      mv -f tmp_${type}.ctl ${ctl_file}
   fi

   $NCP ${type}${anl}.ctl ${SUFFIX1}${anl}.ctl
   ${IG_SCRIPTS}/update_ctl_fname.sh ${SUFFIX1}${anl}.ctl ${SUFFIX1} 

   $NCP ${type}${anl}.ctl ${SUFFIX2}${anl}.ctl
   ${IG_SCRIPTS}/update_ctl_fname.sh ${SUFFIX2}${anl}.ctl ${SUFFIX2} 

   if [[ $suff3 -gt 0 ]]; then
      $NCP ${type}${anl}.ctl ${SUFFIX3}${anl}.ctl
      ${IG_SCRIPTS}/update_ctl_fname.sh ${SUFFIX3}${anl}.ctl ${SUFFIX3} 
   fi

   $NCP ${IG_GSCRIPTS}/setrange.gs ${workdir}/setrange.gs

   run_line="run ${IG_GSCRIPTS}/plot_fs_obsnum_comp.gs ${type} ${data} ${SUFFIX1} ${SUFFIX2}"


   if [[ $suff3 -gt 0 ]]; then 
      plotscript=${type}_${SUFFIX1}_${SUFFIX2}_${SUFFIX3}${anl}.gs
cat << EOF > ${workdir}/${plotscript}
'open ${SUFFIX1}${anl}.ctl'
'open ${SUFFIX2}${anl}.ctl'
'open ${SUFFIX3}${anl}.ctl'
'run ${IG_GSCRIPTS}/plot_fs_obsnum_comp.gs ${type} ${data} ${SUFFIX1} ${SUFFIX2} ${SUFFIX3}'
'quit'
EOF
   else
      plotscript=${type}_${SUFFIX1}_${SUFFIX2}${anl}.gs
cat << EOF > ${workdir}/${plotscript}
'open ${SUFFIX1}${anl}.ctl'
'open ${SUFFIX2}${anl}.ctl'
'run ${IG_GSCRIPTS}/plot_fs_obsnum_comp.gs ${type} ${data} ${SUFFIX1} ${SUFFIX2}'
'quit'
EOF
   fi

   $GRADS -bpc "run ${workdir}/${plotscript}"

   #------------------------------------------------------------------
   #  copy to imgn mirror directory
   #------------------------------------------------------------------
   imgndir=${IMGNDIR1}/pngs/comp
   if [[ ! -d $imgndir ]]; then
      mkdir -p $imgndir
   fi

   $NCP *.png ${imgndir}/.

   #------------------------------------------------------------------
   #  clean up files
   #------------------------------------------------------------------
   cd ${workdir}
   cd ../
   rm -rf $workdir
      
fi


echo end plot_fs_obsnum_comp.sh

exit
