#! /bin/ksh

#------------------------------------------------------------------
#
#  nu_plot_time.sh
#
#  This is designed to be called by the existing plot_time.sh
#  script.  It produces [satype].[n].time.txt and one [satype].chan.txt
#  files for each satype in the input SATYPE_LIST parameter.  These
#  files are then moved to the IMGNDIR/time directory where they will
#  eventually be mirrored to the webserver.
#
#  Algorithm:
#     1)  Build these files:
#         input (namelist)
#         times.txt  (times of data files, newest to oldest, 1 per line, no delimiters)
#         chan.txt   (extracted from *.ctl file)
#            actual channel number, use flag, wavelength, frequency (csv format)
#
#     2)  Run time.x <input >out
#
#     3)  Move [sat].[n].times.txt to ~/imgn/[source]/pngs/time
#------------------------------------------------------------------
set -ax

SATYPE_LIST=$1

echo "Starting nu_plot_time.sh"
echo "  SATYPE_LIST = $1"

#--------------------------------------------
#  Make sure image repository exists.
#  This is used to mirror to the web server.
#
if [[ ! -d ${IMGNDIR}/time ]]; then
   mkdir -p ${IMGNDIR}/time
fi


#----------------------------------
#  copy time.x executable locally
#
time_exec="time.x"
if [[ ! -e $time_exec ]]; then
   $NCP ${IG_EXEC}/${time_exec}  ./${time_exec}
fi

#----------------------------------
#  define output files
#
input="input.txt"
times="times.txt"
chanf="chan.txt"
tmp="tmp.txt"
nu_tmp="nu_tmp.txt"

#--------------------------------------------
# Loop over SATYPE_LIST
#
for type in ${SATYPE_LIST}; do

   #------------------------------
   #  build input (namelist) file
   #
   nchanl=`cat ./${type}.ctl | grep title |gawk '{print $4}'`
   ncycle=`ls -l ./${type}.*.ieee_d | wc -l`
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
     nregion=${nregion},
    /
EOF

   #------------------------------
   #  Build times.txt
   #    Manipulate a ls -l of the available data files
   #    sorted newest to oldest.  Fill in any missing times.
   #    This is to ensure we end up with rmiss values in the 
   #    data files where we're actually missing data files 
   #    so any outages are readily apparent.
   #
   rm $times
  
   ls -1 ${type}.*.ieee_d | gawk -F. '{print $2}' > $tmp 
   last_time=`cat $tmp | gawk 'NR==1{print $1}'`
   tac $tmp > $nu_tmp
   first_time=`cat $nu_tmp | gawk 'NR==1{print $1}'`
   echo "start_time, end_time = $start_time, $end_time"

   rm $tmp
   rm $nu_tmp

   echo $first_time > $times
   cdate=`$NDATE -6 $first_time` 
   ctr=1
   while [[ $cdate -ge $last_time && $ctr -le 200 ]]; do
      echo $cdate >> $times
      tdate=$cdate
      cdate=`$NDATE -6 $tdate`
      ctr=$(($ctr+1))
   done


   #------------------------------
   #  build chan.txt using ctl file
   #
   rm $chanf
   grep iuse ${type}.ctl | gawk '{print $5 ", " $8 ", " $14 ", " $17}' > $chanf


   #------------------------------
   #  run times.x
   #
   ./${time_exec} <$input >${type}.times.out


   #------------------------------
   #  mv output files to IMGNDIR
   #
   cp -f ${type}.*.time.txt  ${IMGNDIR}/time/.
   cp -f ${type}.chan.txt     ${IMGNDIR}/time/.

done


echo "Exiting nu_plot_time.sh"
exit

