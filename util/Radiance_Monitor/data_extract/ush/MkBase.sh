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

function usage {
  echo "Usage:  MkBase.sh suffix 1>log 2>err"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $TANKverf/stats directory."
  echo "            Redirection of log and err files is recommended for "
  echo "              diagnostic purposes, but not essential"
}

nargs=$#
if [[ $nargs -lt 1 && $nargs -gt 2 ]]; then
   usage
   exit 1
fi

SUFFIX=$1

if [[ $nargs -eq 2 ]]; then
   SATYPE=$2
   SINGLE_SAT=1
fi

this_file=`basename $0`
this_dir=`dirname $0`


#------------------------------------------------------------------
# Set environment variables.
#-------------------------------------------------------------------
top_parm=${this_dir}/../../parm
export RADMON_CONFIG=${RADMON_CONFIG:-${top_parm}/RadMon_config}

#if [[ -s ${top_parm}/RadMon_config ]]; then
#   . ${top_parm}/RadMon_config
#else
#   echo "Unable to source RadMon_config file in ${top_parm}"
#   exit 2
#fi
if [[ -s ${RADMON_CONFIG} ]]; then
   . ${RADMON_CONFIG}
else
   echo "Unable to source ${RADMON_CONFIG} file"
   exit 2
fi

#if [[ -s ${top_parm}/RadMon_user_settings ]]; then
#   . ${top_parm}/RadMon_user_settings
#else
#   echo "Unable to source RadMon_user_settings file in ${top_parm}"
#   exit 2
#fi
if [[ -s ${RADMON_USER_SETTINGS} ]]; then
   . ${RADMON_USER_SETTINGS}
else
   echo "Unable to source ${RADMON_USER_SETTINGS} file"
   exit 2
fi

#. ${RADMON_DATA_EXTRACT}/parm/data_extract_config
. ${DE_PARM}/data_extract_config


#--------------------------------------------------------------------
# Get the area (glb/rgn) for this suffix
#--------------------------------------------------------------------

area=$RAD_AREA
echo $area

#if [[ $area = glb ]]; then
#   . ${PARMverf_rad}/glbl_conf
#   . ${DE_PARM}/glbl_conf
#else
#   . ${PARMverf_rad}/rgnl_conf
#   . ${DE_PARM}/rgnl_conf
#fi


#-------------------------------------------------------------------
#  Set dates
#    BDATE is beginning date for the 30/60 day range
#    EDATE is ending date for 30/60 day range (always use 00 cycle) 
#-------------------------------------------------------------------
#EDATE=`${USHverf_rad}/find_cycle.pl 1 ${TANKDIR}`
#EDATE=`${DE_SCRIPTS}/find_cycle.pl 1 ${TANKDIR}`
EDATE=`${DE_SCRIPTS}/find_cycle.pl 1 ${TANKverf}`
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
#  If no single sat source was supplied at the command line then 
#  find or build $SATYPE list for this data source.
#-------------------------------------------------------------------
if [[ $SINGLE_SAT -eq 0 ]]; then
#   if [[ -e ${TANKDIR}/info/SATYPE.txt ]]; then
   if [[ -e ${TANKverf}/info/SATYPE.txt ]]; then
#      SATYPE=`cat ${TANKDIR}/info/SATYPE.txt`
      SATYPE=`cat ${TANKverf}/info/SATYPE.txt`
   else
      PDY=`echo $EDATE|cut -c1-8`

#      if [[ -d ${TANKDIR}/radmon.${PDY} ]]; then
      if [[ -d ${TANKverf}/radmon.${PDY} ]]; then
#         test_list=`ls ${TANKDIR}/radmon.${PDY}/angle.*${EDATE}.ieee_d*`
         test_list=`ls ${TANKverf}/radmon.${PDY}/angle.*${EDATE}.ieee_d*`
         for test in ${test_list}; do
            this_file=`basename $test`
            tmp=`echo "$this_file" | cut -d. -f2`
            echo $tmp
            #----------------------------------------------------------   
            #  remove sat/instrument_anl names so we don't end up
            #  with both "airs_aqua" and "airs_aqua_anl" if analysis
            #  files are being generated for this source.
            #----------------------------------------------------------   
            test_anl=`echo $tmp | grep "_anl"`
            if [[ $test_anl = "" ]]; then
               SATYPE_LIST="$SATYPE_LIST $tmp"
            fi
         done
      fi
      SATYPE=$SATYPE_LIST
   fi
fi

echo $SATYPE


#-------------------------------------------------------------------
#  Loop over $SATYPE and build base files for each
#-------------------------------------------------------------------
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

#      if [[ -d ${TANKDIR}/radmon.${day} ]]; then
      if [[ -d ${TANKverf}/radmon.${day} ]]; then
#         test_file=${TANKDIR}/radmon.${day}/time.${type}.${cdate}.ieee_d
         test_file=${TANKverf}/radmon.${day}/time.${type}.${cdate}.ieee_d
         if [[ -s $test_file ]]; then
            $NCP ${test_file} ./${type}.${cdate}.ieee_d
         elif [[ -s ${test_file}.${Z} ]]; then
            $NCP ${test_file}.${Z} ./${type}.${cdate}.ieee_d.${Z}
         fi
      fi
      if [[ ! -s ${type}.${cdate}.ieee_d && ! -s ${type}.${cdate}.ieee_d.${Z} ]]; then
#         $NCP $TANKDIR/time/${type}.${cdate}.ieee_d* ./
         $NCP $TANKverf/time/${type}.${cdate}.ieee_d* ./
      fi

      adate=`$NDATE +6 $cdate`
      cdate=$adate
   done


   day=`echo $EDATE | cut -c1-8 `
#   test_file=${TANKDIR}/radmon.${day}/time.${type}.ctl
   test_file=${TANKverf}/radmon.${day}/time.${type}.ctl
 
   if [[ -s ${test_file} ]]; then
#      $NCP $TANKDIR/radmon.${day}/time.${type}.ctl ${type}.ctl
      $NCP $TANKverf/radmon.${day}/time.${type}.ctl ${type}.ctl
   elif [[ -s ${test_file}.${Z} ]]; then
#      $NCP $TANKDIR/radmon.${day}/time.${type}.ctl.${Z} ${type}.ctl.${Z}
      $NCP $TANKverf/radmon.${day}/time.${type}.ctl.${Z} ${type}.ctl.${Z}
   else
#      $NCP $TANKDIR/time/${type}.ctl* ./
      $NCP $TANKverf/time/${type}.ctl* ./
   fi

   ${UNCOMPRESS} *.${Z}

   #-------------------------------------------------------------------
   #  Get the number of channels for this $type
   #-------------------------------------------------------------------
   line=`cat ${type}.ctl | grep title`
   nchan=`echo $line|gawk '{print $4}'`
   echo channels = $nchan

   #-------------------------------------------------------------------
   #  Cut out the iuse flags from the ctl file and dump them
   #  into the channel.txt file for make_base executable to access
   #-------------------------------------------------------------------
   gawk '/iuse/{print $8}' ${type}.ctl >> channel.txt

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

   ./make_base < input > stdout.${type}.base

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
#  Pack all basefiles into a tar file and move it to $TANKverf/info.
#  If a SINGLE_SAT was supplied at the command line then copy the
#  existing $basefile and add/replace the requested sat, leaving
#  all others in the $basefile unchanged.
#-------------------------------------------------------------------
#if [[ ! -d ${TANKDIR}/info ]]; then
if [[ ! -d ${TANKverf}/info ]]; then
#   mkdir -p ${TANKDIR}/info
   mkdir -p ${TANKverf}/info
fi

cd $tmpdir
basefile=radmon_base.tar

if [[ $SINGLE_SAT -eq 0 ]]; then
   tar -cvf ${basefile} *.base
else
   newbase=$tmpdir/newbase
   mkdir $newbase
   cd $newbase

   #  copy over existing $basefile
#   if [[ -e ${TANKDIR}/info/${basefile} || -e ${TANKDIR}/info/${basefile}.${Z} ]]; then
   if [[ -e ${TANKverf}/info/${basefile} || -e ${TANKverf}/info/${basefile}.${Z} ]]; then
#      $NCP ${TANKDIR}/info/${basefile}* .
      $NCP ${TANKverf}/info/${basefile}* .
      if [[ -e ${basefile}.${Z} ]]; then
         $UNCOMPRESS ${basefile}.${Z}
      fi
      tar -xvf ${basefile}
      rm ${basefile}
   fi

   #  copy new *.base file from $tmpdir and build new $basefile (tar file)
   cp -f $tmpdir/*.base .
   tar -cvf ${basefile} *.base
   mv -f ${basefile} $tmpdir/.
   cd $tmpdir
# keep for testing
#   rm -rf $newbase

fi

#  Remove the old version of the $basefile
#if [[ -e ${TANKDIR}/info/${basefile} || -e ${TANKDIR}/info/${basefile}.${Z} ]]; then
if [[ -e ${TANKverf}/info/${basefile} || -e ${TANKverf}/info/${basefile}.${Z} ]]; then
#   rm -f ${TANKDIR}/info/${basefile}*
   rm -f ${TANKverf}/info/${basefile}*
fi

${COMPRESS} ${basefile}
#$NCP ${basefile}.${Z} ${TANKDIR}/info/.
$NCP ${basefile}.${Z} ${TANKverf}/info/.

#-------------------------------------------------------------------
#  Clean up $tmpdir
#-------------------------------------------------------------------
# keep for testing
#cd ..
#rm -rf $tmpdir

exit
