#!/bin/ksh

# set -x

HRRRE_ORGPATH=/mnt/lfs1/projects/wrfruc/HRRRE/forecast
SAVEPATH=/mnt/lfs1/projects/rtwbl/mhu/test/gsi/hrrre/data
workdir=/mnt/lfs1/projects/rtwbl/mhu/test/gsi/hrrre/saveperts/work
execfile=/mnt/lfs1/projects/rtwbl/mhu/test/gsi/hrrre/saveperts/save_arw_ens.x

if [ ! -d "${workdir}" ]; then
   mkdir -p ${workdir}
   echo ' create work directory ', ${workdir}
   cd ${workdir}
else
   cd ${workdir}
#   rm filelist03*
#   rm namelist_enspert*
#   rm wrfout_d01_const
#   rm fort.*
   rm *
fi

currentdate=`date -u +"%Y%m%d %H"`
starttime=`date -u +"%Y%m%d 00" -d "${currentdate}"`
YYYYMMDD00=`date -u +"%Y%m%d%H" -d "${starttime}"`

ln -s ${SAVEPATH}/wrfout_d02_2018-07-17_09_00_00 wrfout_d01_const
cp ${execfile} .

no_member=9
fcsthh=12
while [[ $fcsthh -le 36 ]];do
   
   time_str=`date "+%Y-%m-%d_%H_%M_%S" -d "${starttime} ${fcsthh} hours"`

   ensmem=1
   while [[ $ensmem -le $no_member ]];do
      ensmemid=`printf %4.4i $ensmem`
      filenow=${HRRRE_ORGPATH}/${YYYYMMDD00}/wrfprd_mem${ensmemid}/wrfout_d02_${time_str}
      echo ${filenow} >> filelist03
      filenow=${HRRRE_ORGPATH}/${YYYYMMDD00}/postprd_mem${ensmemid}/wrfnat_mem${ensmemid}_${fcsthh}.grib2
      echo ${filenow} >> filelist03_grib2
      
# next member
      (( ensmem += 1 ))

   done

# got namelist ready
cat << EOF > namelist_enspert
&SETUP
   n_ens=9,
   initialtime=${YYYYMMDD00},
   fcsthh=${fcsthh},
/
EOF
#
# run 
  ./save_arw_ens.x > runlog_${YYYYMMDD00}f${fcsthh} 2>&1

# save those configure files
   mv namelist_enspert namelist_enspert_${YYYYMMDD00}f${fcsthh}
   mv filelist03 filelist03_${YYYYMMDD00}f${fcsthh}
   mv filelist03_grib2 filelist03_grib2_${YYYYMMDD00}f${fcsthh}

   mv enspert_${YYYYMMDD00}* ${SAVEPATH}/.
   mv ensmean_${YYYYMMDD00}* ${SAVEPATH}/.
   rm fort.9*
# next ensemble forecast
   (( fcsthh += 3 ))
done

exit
