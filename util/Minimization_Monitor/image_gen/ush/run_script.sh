#!/bin/sh

#----------------------------------------------------------------------
#
#  This is a generic image generation leader script.  
#    The MinMon_Plt.sh script may be called directly or this script
#    may be used to override specific default values in the parm files
#    and/or MinMon_Plt.sh script.
#
#  MINMON_SUFFIX corresponds to the $NET value
#
#  RUN is either gfs or gdas
#
#  MAIL_CC list is used if warnings messages are set to on.
#   
#  idate, if set, will override the next value in the local
#    pen_data_map.xml file.  
#----------------------------------------------------------------------

MINMON_SUFFIX=testmm
RUN=gfs

#export MAIL_CC=
#idate=


#----------------------------------------------------------------------
#  no changes should be necessary below this point.
#----------------------------------------------------------------------
. ../../parm/MinMon.ver
. ../../parm/MinMon_config

if [[ $idate == "" ]]; then
   data_map=${M_IG_SCRIPTS}/pen_data_map.xml

   imgdate=`${M_IG_SCRIPTS}/query_data_map.pl \
	${data_map} ${MINMON_SUFFIX}_${RUN} imgdate`
   echo "imgdate = $imgdate"


   idate=`$NDATE +6 $imgdate`
fi

pdy=`echo $idate | cut -c1-8`
cyc=`echo $idate | cut -c9-10`
echo "idate = $idate"

echo "tank = ${M_TANKverf}/stats/${MINMON_SUFFIX}"

prodate=`${M_IG_SCRIPTS}/find_cycle.pl \
	--dir ${M_TANKverf}/stats/${MINMON_SUFFIX} \
	--cyc 1 --run ${RUN}`

echo "imgdate, prodate = $imgdate, $prodate"


if [[ $idate -le $prodate ]]; then

   echo " firing MinMon_Plt.sh"
   ${M_IG_SCRIPTS}/MinMon_Plt.sh ${MINMON_SUFFIX} -p $idate -r $RUN  \
	1>${LOGdir}/IG.${run}.${PDY}.${cyc}.log   \
	2>${LOGdir}/IG.${run}.${PDY}.${cyc}.err

   rc=`${M_IG_SCRIPTS}/update_data_map.pl ${data_map} \
	${MINMON_SUFFIX}_${RUN} imgdate ${idate}`

fi


exit
