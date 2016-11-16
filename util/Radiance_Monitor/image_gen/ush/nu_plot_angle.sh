#! /bin/ksh

#------------------------------------------------------------------
#
#  nu_plot_angle.sh
#
#  This is designed to be called by the existing plot_angle.sh
#  script.  It produces [satype].[n].[ang_type].time.txt and one [satype].chan.txt
#  files for each satype in the input SATYPE_LIST parameter.  These
#  files are then moved to the IMGNDIR/angle directory where they will
#  eventually be mirrored to the webserver.
#
#  Algorithm:
#     1)  Build these files:
#         input (namelist)
#         times.txt  (times of data files, newest to oldest, 1 per line, no delimiters)
#    
#     2)  Copy the chan.txt from the time directory and modify the ncycles (line 1) 
#         to nsteps.
#
#     3)  Run angle.x <input >out
#
#     4)  Move [sat].[n].[ang_type].times.txt to ~/imgn/[source]/pngs/angle
#------------------------------------------------------------------
set -ax

SATYPE_LIST=$1

echo "Starting nu_plot_angle.sh"

#--------------------------------------------
#  Make sure image repository exists.
#  This is used to mirror to the web server.
#
if [[ ! -d ${IMGNDIR}/angle ]]; then
   mkdir -p ${IMGNDIR}/angle
fi


#----------------------------------
#  copy time.x executable locally
#
ang_exec="angle.x"
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

   rm -f ${input}


   cat <<EOF > ${input}
   &INPUT
     satname=${type},
     nchanl=${nchanl},
     ncycle=${ncycle},
     scan_start=${scan_start},
     scan_stepsz=${scan_stepsz},
     scan_nstep=${scan_nstep}
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
   cp -f ${type}.*.angle.txt  ${IMGNDIR}/angle/.
   cp -f ${type}.chan.txt     ${IMGNDIR}/angle/.
   cp -f ${type}.*.time.txt   ${IMGNDIR}/time/.

done


echo "Exiting nu_plot_time.sh"
exit

