#! /bin/ksh

#------------------------------------------------------------------
#  archive.sh
#
#    Copy latest 15 days worth of data to /sss../save directory
#------------------------------------------------------------------

set -ax
arch=/sss/emc/da/save/Edward.Safford/nbns/stats/convweb/copr
TANKDIR=~/nbns/stats/convweb/copr

#--------------------------------------------------------------------
# time_vert data files
#
#   Add the latest 15 days from $TANKDIR are in the archive,
#   reduce archive to 15 total.
#  
time_tank="${TANKDIR}/time_vert"
time_arch="${arch}/time_vert"
if [[ ! -d $time_arch ]]; then
   mkdir -p $time_arch
fi

cp_list=`ls $time_tank/*.tar | sort | tail -15`

for file in ${cp_list}; do
   fn=`basename ${file}`
   echo $fn 
   if [[ ! -e ${time_arch}/${fn} ]]; then
      cp ${file} ${time_arch}/${fn}
   fi
done 

total=`ls -1 ${time_arch}/*.tar | wc -l`
((extra=total-15))

if [[ $extra -gt 0 ]]; then
   echo removing $extra files
   `ls -1 ${time_arch}/*.tar | head -${extra} | xargs rm -rf`
fi


#--------------------------------------------------------------------
# horz_hist data files
#
#   Add the latest 15 days from $TANKDIR are in the archive,
#   reduce archive to 15 total.
#  
horz_tank="${TANKDIR}/horz_hist"
horz_arch="${arch}/horz_hist"
gesanl="anl ges"

for type in $gesanl; do
   archdir=$horz_arch/${type}
   horzdir=${horz_tank}/${type}
   if [[ ! -d $archdir ]]; then
      mkdir -p $archdir
   fi

   cp_list=`ls $horzdir/*.tar | sort | tail -15`

   for file in ${cp_list}; do
      fn=`basename ${file}`
      echo $fn 
      if [[ ! -e ${archdir}/${fn} ]]; then
         cp ${file} ${archdir}/${fn}
      fi
   done 

   total=`ls -1 ${archdir}/*.tar | wc -l`
   ((extra=total-15))

   if [[ $extra -gt 0 ]]; then
      `ls -1 ${archdir}/*.tar | head -${extra} | xargs rm -rf`
   fi

done

exit
 
