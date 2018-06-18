#! /bin/ksh

#------------------------------------------------------------------
#  plot_horiz.sh
#

echo "begin plot_horiz.sh"

set -ax

export SATYPE2=$1
export PVAR=$2
export PTYPE=$3
export string=ges

echo "SATYPE2, PVAR, PTYPE = $SATYPE2, $PVAR, $PTYPE"
echo "RUN = $RUN"

#------------------------------------------------------------------
# Set work space for this SATYPE2 source.
#
tmpdir=${WORKDIR}/${SATYPE2}.$PDATE.${PVAR}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

#------------------------------------------------------------------
#   Set dates
#
bdate=`$NDATE -18 $PDATE`
edate=$PDATE
bdate0=`echo $bdate|cut -c1-8`
edate0=`echo $edate|cut -c1-8`


#--------------------------------------------------------------------
# Copy control and data files to $tmpdir
#
tankdir_bdate0=${TANKDIR}/${RUN}.${bdate0}/oznmon/horiz
tankdir_edate0=${TANKDIR}/${RUN}.${edate0}/oznmon/horiz

$NCP ${tankdir_bdate0}/${SATYPE2}.ctl ./
#############################################
#  NOTE:  need to modify tdef line in .ctl file for this cycle!!
#############################################

$NCP ${tankdir_bdate0}/${SATYPE2}*${bdate0}* ./
$NCP ${tankdir_edate0}/${SATYPE2}*${edate0}* ./
$NCP ${OZN_IG_GSCRPTS}/cbarnew.gs ./


for type in ${SATYPE2}; do
   date

   $STNMAP -i ${type}.ctl

   for var in ${PTYPE}; do

cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${OZN_IG_GSCRPTS}/plot_horiz_${string}.gs ${OZNMON_SUFFIX} ${RUN} ${type} ${var} x800 y700'
'quit'
EOF
      $GRADS -blc "run ${tmpdir}/${type}_${var}.gs"   

      #------------------------------
      #  rename the analysis plots
      #
#      if [[ $string == 'anl' ]] ; then
#         levlist='1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22'
#         if [[ ${var} == 'ges' ]] ; then
#            if [[ $type == 'omi_aura' || $type == 'gome_metop-a' || $type == 'gome_metop-b' ]] ; then
#               mv ${type}.ges_1.png ${type}.anl_1.png
#            else
#               for level in $levlist ; do
#                  mv ${type}.ges_${level}.png ${type}.anl_${level}.png
#               done
#            fi
#         fi
#         if [[ ${var} == 'obsges' ]] ; then
#            if [[ $type == 'omi_aura' || $type == 'gome_metop-a' || $type == 'gome_metop-b' ]] ; then
#               mv ${type}.obsges_1.png ${type}.obsanl_1.png
#            else
#               for level in $levlist ; do
#                  mv ${type}.obsges_${level}.png ${type}.obsanl_${level}.png
#               done
#            fi
#         fi
#      fi

   done 

   #--------------------------------------------------------------------
   #   Move image files to TANK
   #

   ${NCP} *.png ${OZN_IMGN_TANKDIR}/.
 

#   if [[ $transfer_plot -eq 1 ]] ; then
##     transfer plots from wcoss to rzdm
#      rm -f $LOGDIR/transfer_horiz_${SATYPE2}.log
#      export subdir=horiz
#      export listvar1=PDATE,webpsw,webmch,webid,WEBDIR,LOGDIR,USER,SUB,SUFFIX,SATYPE2,string,PVAR,subdir,tmpdir,listvar1
#      $SUB -P ${PROJECT} -q transfer -o $LOGDIR/transfer_horiz_${SATYPE2}.log -M 30 -W 0:45 -R affinity[core] -J transfer_horiz ${SCRIPTS}/transfer.sh
#   fi
  
done


#--------------------------------------------------------------------
# Clean $tmpdir.  Submit done job.
#cd $tmpdir
#cd ../
##rm -rf $tmpdir

echo "end plot_horiz.sh"
exit

