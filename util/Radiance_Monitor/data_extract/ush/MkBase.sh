#!/bin/ksh

#-------------------------------------------------------------------
#
#  script:  MkBase.sh
#
#  purpose:  Generate the baseline stat files for each satelite 
#            by channel and region.  Baseline stat includes the 
#            30 day average number of obs and sdv, and 30 day avg
#            penalty and sdv.  These files are used only if the 
#            diagnostic reports are switched on. 
#
#  calling:  MkBase.sh suffix 1>log 2>err
#-------------------------------------------------------------------

set -ax
date
export list=$listvar

function usage {
  echo "Usage:  MkBase.sh suffix 1>log 2>err"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $TANKDIR/stats directory."
  echo "            Redirection of log and err files is recommended for "
  echo "              diagnostic purposes, but not essential"
}

nargs=$#
if [[ $nargs -ne 1 ]]; then
   usage
   exit 1
fi

export SUFFIX=$1

this_file=`basename $0`
this_dir=`dirname $0`


#------------------------------------------------------------------
# Set environment variables.
#-------------------------------------------------------------------
top_parm=${this_dir}/../../parm

if [[ -s ${top_parm}/RadMon_config ]]; then
   . ${top_parm}/RadMon_config
else
   echo "Unable to source RadMon_config file in ${top_parm}"
   exit 2
fi

. ${RADMON_DATA_EXTRACT}/parm/data_extract_config

#echo SCRIPTS = ${SCRIPTS}

#--------------------------------------------------------------------
# Get the area (glb/rgn) for this suffix
#--------------------------------------------------------------------
echo DATA_MAP = $DATA_MAP

#area=`${SCRIPTS}/get_area.sh ${SUFFIX} ${DATA_MAP}`
area=`${USHverf_rad}/querry_data_map.pl ${DATA_MAP} ${SUFFIX} area`
echo $area

if [[ $area = glb ]]; then
   . ${PARMverf_rad}/glbl_conf
else
   . ${PARMverf_rad}/rgnl_conf
fi


#-------------------------------------------------------------------
#  Set dates
#    BDATE is beginning date for the 30/60 day range
#    EDATE is ending date for 30/60 day range (always use 00 cycle) 
#-------------------------------------------------------------------
#EDATE=`${SCRIPTS}/get_prodate.sh ${SUFFIX} ${DATA_MAP}`
EDATE=`${USHverf_rad}/querry_data_map.pl ${DATA_MAP} ${SUFFIX} prodate`
EDATE=2012032000
echo $EDATE

sdate=`echo $EDATE|cut -c1-8`
EDATE=${sdate}00
BDATE=`$NDATE -720 $EDATE`

echo EDATE = $EDATE
echo BDATE = $BDATE

tmpdir=${STMP_USER}/base_${SUFFIX}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

#-------------------------------------------------------------------
#  Loop over $SATYPE and build base files for each
#-------------------------------------------------------------------
SATYPE=`cat ${TANKDIR}/info/SATYPE.txt`
for type in ${SATYPE}; do

   #-------------------------------------------------------------------
   #  Create $tmpdir
   #-------------------------------------------------------------------
   workdir=${tmpdir}/${type}.$EDATE
   mkdir -p $workdir
   cd $workdir

   #-------------------------------------------------------------------
   #  Create the cycle_hrs.txt file
   #-------------------------------------------------------------------
   cdate=$BDATE
   while [[ $cdate -le $EDATE ]]; do
      echo $cdate >> cycle_hrs.txt
      adate=`$NDATE +6 $cdate`
      cdate=$adate
   done

   #-------------------------------------------------------------------
   #  Copy the data files and ctl file to workdir
   #-------------------------------------------------------------------
   cdate=$BDATE
   while [[ $cdate -le $EDATE ]]; do
      day=`echo $cdate | cut -c1-8 `

      if [[ -d ${TANKDIR}/radmon.${day} ]]; then
         test_file=${TANKDIR}/radmon.${day}/time.${type}.${cdate}.ieee_d
         if [[ -s $test_file ]]; then
            $NCP ${test_file} ./${type}.${cdate}.ieee_d
         elif [[ -s ${test_file}.Z ]]; then
            $NCP ${test_file}.Z ./${type}.${cdate}.ieee_d.Z
         fi
      fi
      if [[ ! -s ${type}.${cdate}.ieee_d && ! -s ${type}.${cdate}.ieee_d.Z ]]; then
         $NCP $TANKDIR/time/${type}.${cdate}.ieee_d* ./
      fi

      adate=`$NDATE +6 $cdate`
      cdate=$adate
   done


   day=`echo $EDATE | cut -c1-8 `
   test_file=${TANKDIR}/radmon.${day}/time.${type}.ctl
 
   if [[ -s ${test_file} ]]; then
      $NCP $TANKDIR/radmon.${day}/time.${type}.ctl ${type}.ctl
   elif [[ -s ${test_file}.Z ]]; then
      $NCP $TANKDIR/radmon.${day}/time.${type}.ctl.Z ${type}.ctl.Z
   else
      $NCP $TANKDIR/time/${type}.ctl* ./
   fi

   uncompress *.Z

   #-------------------------------------------------------------------
   #  Get the number of channels for this $type
   #-------------------------------------------------------------------
   line=`cat ${type}.ctl | grep title`
   nchan=`echo $line|nawk '{print $4}'`
   echo channels = $nchan

   #-------------------------------------------------------------------
   #  Cut out the iuse flags from the ctl file and dump them
   #  into the channel.txt file for make_base executable to access
   #-------------------------------------------------------------------
   nawk '/iuse/{print $8}' ${type}.ctl >> channel.txt

   #-------------------------------------------------------------------
   #  Copy the executable and run it 
   #------------------------------------------------------------------
   out_file=${type}.base
   $NCP ${HOMEgfs}/exec/make_base ./

cat << EOF > input
 &INPUT
  satname='${type}',
  n_chan=${nchan},
  nregion=1,
  nfile=121,
  date='${EDATE}',
  out_file='${out_file}',
 /
EOF

   timex make_base < input > stdout.${type}.base

   #-------------------------------------------------------------------
   #  Copy base file back to $tmpdir 
   #-------------------------------------------------------------------
   $NCP $out_file ${tmpdir}/.

   #-------------------------------------------------------------------
   #  Clean up
   #-------------------------------------------------------------------
   cd $tmpdir
   rm -rf $workdir

done


#-------------------------------------------------------------------
#  Pack all basefiles into a tar file and move it to $TANKDIR/info
#-------------------------------------------------------------------

cd $tmpdir
basefile=radmon_base.tar
tar -cvf ${basefile} *.base
compress ${basefile}

$NCP ${basefile}.Z ${TANKDIR}/info/.

#-------------------------------------------------------------------
#  Clean up $tmpdir
#-------------------------------------------------------------------
cd ..
rm -rf $tmpdir

exit
