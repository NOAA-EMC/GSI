#! /bin/bash

#------------------------------------------------------------------
#
#  mk_digital_ang.sh
#
#  This script is to be called by plot_angle.sh. It produces 
#  the following text files containing digital data for the 
#  js/html files to build images:
#      [satype].[chan].[ang_plot_type].time.txt 
#      [satype].chan.txt 
#
#  These files are then moved to the IMGNDIR/angle directory where 
#  they will be mirrored to the webserver by the transfer scripts.
#
#  Algorithm:
#     1)  Build these files:
#         input (namelist)
#         times.txt  (times of data files, newest to oldest, 1 per 
#                     line, no delimiters)
#    
#     2)  Copy the chan.txt from the time directory and modify the 
#           ncycles (line 1) to nsteps.
#
#     3)  Run radmon_ig_angle.x <input >out
#
#     4)  Move [sat].[n].[ang_type].times.txt to ~/imgn/[source]/pngs/angle
#------------------------------------------------------------------


#------------------------------------------------------------
#  function large_mv()
#
#  Requires subdirectory argument.
#
#  In some cases there are too many files to load into a
#  single command without exceeding the machine's argument
#  limit.  This function circumvents that problem.
#------------------------------------------------------------
function large_mv () {
   while read imgf; do
      if [[ -s $imgf ]]; then
         mv $imgf ${IMGNDIR}/$1/.
      fi
   done
}


set -ax

SATYPE_LIST=$1

echo "Starting mk_digital_ang.sh"


#----------------------------------
#  copy angle executable locally
#
ang_exec="radmon_ig_angle.x"
if [[ ! -e $ang_exec ]]; then
   $NCP ${IG_EXEC}/${ang_exec}  ./${ang_exec}
fi


#----------------------------------
#  define output files
#
input="input.txt"
times="times.txt"
chanf="chan.txt"
tmp="tmp.txt"


#----------------------------------
#  copy over the scaninfo file 
#
scaninfo="scaninfo.txt"
scaninfo_loc=${HOMEgdas/fix/gdas_radmon_scaninfo.txt}
if [[ ! -e ${scaninfo_loc} ]]; then
  echo "Unable to locate scaninfo file: ${scaninfo_loc}"
  exit 4
fi

$NCP $HOMEgdas/fix/gdas_radmon_scaninfo.txt $scaninfo


#--------------------------------------------
# Loop over SATYPE_LIST
#
for type in ${SATYPE_LIST}; do

   #------------------------------
   #  build input (namelist) file
   #
   nchanl=`cat ./${type}.ctl | grep title |gawk '{print $4}'`
   ncycle=`ls -l ./${type}.*.ieee_d | wc -l`

   scan_start=`cat ./$scaninfo | grep $type | gawk '{print $2}'`
   scan_stepsz=`cat ./$scaninfo | grep $type | gawk '{print $3}'`
   scan_nstep=`cat ./$scaninfo | grep $type | gawk '{print $4}'`

   nregion=5
   if [[ $RAD_AREA = 'rgn' ]]; then
      nregion=1
   fi

   rm -f ${input}


   cat <<EOF > ${input}
   &INPUT
     satname=${type},
     nchanl=${nchanl},
     ncycle=${ncycle},
     scan_start=${scan_start},
     scan_stepsz=${scan_stepsz},
     scan_nstep=${scan_nstep},
     nregion=${nregion},
    /
EOF

   #------------------------------
   #  build times.txt
   #    manipulate a ls -l of the available data files
   #
   rm $times

   ls -1 ${type}.*.ieee_d | gawk -F. '{print $2}' > $tmp 
   tac $tmp > $times
   rm $tmp

   #------------------------------
   #  build chan.txt using ctl file
   #
   rm $chanf
   grep iuse ${type}.ctl | gawk '{print $5 ", " $8 ", " $14 ", " $17}' > $chanf


   #------------------------------
   #  run times.x
   #
   ./${ang_exec} <$input >${type}.angle.out


   #---------------------------------------------------------------
   #  copy ${type}.chan.txt from the time directory and substitute 
   #  nsteps for ncycles on the first line.
   $NCP ${IMGNDIR}/time/${type}.chan.txt .
   
   ncyc=`cat ${type}.chan.txt | grep ${type} | gawk '{print $3}'`
   
   sed "1 s/$ncyc/$scan_nstep/" ${type}.chan.txt >tmp.txt
   mv -f tmp.txt ${type}.chan.txt

   #------------------------------
   #  mv output files to IMGNDIR
   #
   ls -1 ${type}.*.angle.txt | large_mv angle
   mv -f ${type}.chan.txt     ${IMGNDIR}/angle/.
   ls -1 ${type}.*.time.txt  | large_mv time

done


echo "Exiting mk_digital_ang.sh"
exit

