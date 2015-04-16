#! /bin/ksh

#------------------------------------------------------------------
#
#  nu_plot_bcoef.sh
#
#  This is designed to be called by the existing plot_bcoef.sh
#  script.  It produces [satype].[n].time.txt files for each
#  in the input SATYPE_LIST parameter.  These files are then moved
#  to the IMGNDIR/time directory where they will eventually be
#  mirrored to the webserver.
#
#  Algorithm:
#     1)  Build these files:
#         input (namelist)
#         times.txt  (times of data files, newest to oldest, 1 per line, no delimiters)
#
#     2)  Run bcoef.x <input >out
#
#     3)  Move [sat].[n].times.txt to ~/imgn/[source]/pngs/time
#------------------------------------------------------------------
set -ax

SATYPE_LIST=$1

echo "Starting nu_plot_bcoef.sh"
echo "  SATYPE_LIST = $1"

#--------------------------------------------
#  Make sure image repository exists.
#  This is used to mirror to the web server.
#
#if [[ ! -d ${IMGNDIR}/time ]]; then
#   mkdir -p ${IMGNDIR}/time
#fi


#----------------------------------
#  copy bcoef.x executable locally
#
bcoef_exec="bcoef.x"
if [[ ! -e $bcoef_exec ]]; then
   $NCP ${IG_EXEC}/${bcoef_exec}  ./${bcoef_exec}
fi

#----------------------------------
#  define output files
#
input="input.txt"
times="times.txt"
chanf="chan.txt"
tmp="tmp.txt"

#--------------------------------------------
# Loop over SATYPE_LIST
#
for type in ${SATYPE_LIST}; do

   #------------------------------
   #  build input (namelist) file
   #
   nchanl=`cat ./${type}.ctl | grep title |gawk '{print $4}'`
   ncycle=`ls -l ./${type}.*.ieee_d | wc -l`
   rm -f ${input}

   cat <<EOF > ${input}
   &INPUT
     satname=${type},
     nchanl=${nchanl},
     ncycle=${ncycle},
    /
EOF

   #------------------------------
   #  build times.txt
   #    manipulate a ls -l of the available data files
   #    sorted newest to oldest
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
   #  run bcoef.x
   #
   ./${bcoef_exec} <$input >${type}.bcoef.out


   #------------------------------
   #  mv output files to IMGNDIR
   #
   cp -f ${type}.*.bcoef.txt  ${IMGNDIR}/bcoef/.
#   cp -f ${type}.chan.txt     ${IMGNDIR}/time/.

done


echo "Exiting nu_plot_bcoef.sh"
exit

