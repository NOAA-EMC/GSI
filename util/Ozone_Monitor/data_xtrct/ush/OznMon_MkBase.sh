#!/bin/ksh

#-------------------------------------------------------------------
#
#  script:   OznMon_MkBase.sh
#
#  purpose:  Generate the baseline stat files for each instrument
#            by level and region.  Baseline stat includes the 
#            30 day average number of obs and sdv, and 30 day avg
#            penalty and sdv.  These files are used only if the 
#            diagnostic reports are switched on. 
#
#  calling:  OznMon_MkBase.sh suffix 1>log 2>err
#-------------------------------------------------------------------

set -ax
date

function usage {
  echo "Usage:  OznMon_MkBase.sh [-s|--sat sat_name] suffix "
  echo "            Suffix is data source identifier that matches data in "
  echo "              the TANKverf/stats directory."
  echo "            -s|--sat (optional) restricts the list of sat/instrument "
  echo "              sources.  If no sat is specified then all "
  echo "              sat/instrument sources will be included." 
  echo "            -r|--run indicates RUN value, usually gfs|gdas"
}

nargs=$#
if [[ $nargs -lt 1 || $nargs -gt 5 ]]; then
   usage
   exit 1
fi

SINGLE_SAT=0
RUN=gdas

#-----------------------------------------------
#  Process command line arguments
#
while [[ $# -ge 1 ]]
do
   key="$1"
   echo $key

   case $key in
      -r|--run)
         RUN="$2"
         shift # past argument
      ;;
      -s|--sat)
         SATYPE="$2"
         SINGLE_SAT=1
         shift # past argument
      ;;
      *)
         #any unspecified key is OZNMON_SUFFIX
         export OZNMON_SUFFIX=$key
      ;;
   esac

   shift
done

this_file=`basename $0`
this_dir=`dirname $0`

echo "OZNMON_SUFFIX, SATYPE = $OZNMON_SUFFIX, $SATYPE"

#--------------------------------------------------------------------
# Get the area (glb/rgn) for this suffix
#--------------------------------------------------------------------
export GLB_AREA=${GLB_AREA:-1}
#area=$RAD_AREA
#echo $area, $REGIONAL_RR

#------------------------------------------------------------------
# Set environment variables.
#-------------------------------------------------------------------
top_parm=${this_dir}/../../parm

oznmon_version=${oznmon_version:-${top_parm}/OznMon.ver}
if [[ -s ${oznmon_version} ]]; then
   . ${oznmon_version}
else
   echo "Unable to source ${oznmon_version} file"
   exit 2
fi

oznmon_user_settings=${oznmon_user_settings:-${top_parm}/OznMon_user_settings}
if [[ -s ${oznmon_user_settings} ]]; then
   . ${oznmon_user_settings}
else
   echo "Unable to source ${oznmon_user_settings} file"
   exit 3
fi


oznmon_config=${oznmon_config:-${top_parm}/OznMon_config}
if [[ -s ${oznmon_config} ]]; then
   . ${oznmon_config}
else
   echo "Unable to source ${oznmon_config} file"
   exit 4
fi


#-------------------------------------------------------------------
#  Set dates
#    BDATE is beginning date for the 30/60 day range
#    EDATE is ending date for 30/60 day range (always use 00 cycle) 
#-------------------------------------------------------------------
EDATE=`${OZN_DE_SCRIPTS}/find_cycle.pl --cyc 1 --run ${RUN} --dir ${OZN_STATS_TANKDIR}`
echo $EDATE

sdate=`echo $EDATE|cut -c1-8`
EDATE=${sdate}00
BDATE=`$NDATE -1080 $EDATE`	# 45 days
#BDATE=`$NDATE -240 $EDATE`	# 10 days

echo EDATE = $EDATE
echo BDATE = $BDATE

tmpdir=${OZN_WORK_DIR}/base_${OZNMON_SUFFIX}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

#-------------------------------------------------------------------
#  If no single sat source was supplied at the command line then 
#  find or build $SATYPE list for this data source.
#-------------------------------------------------------------------
if [[ $SINGLE_SAT -eq 0 ]]; then

   if [[ -e ${HOMEgdas_ozn}/fix/gdas_oznmon_satype.txt ]]; then
      SATYPE=`cat ${HOMEgdas_ozn}/fix/gdas_oznmon_satype.txt`
   else
      PDY=`echo $EDATE|cut -c1-8`
      cyc=`echo $EDATE|cut -c9-10`

      test_dir=${OZN_STATS_TANKDIR}/${RUN}.${PDY}/${cyc}/oznmon/time
      if [[ -d ${test_dir} ]]; then
         test_list=`ls ${test_dir}/*.${EDATE}.ieee_d*`

         for test in ${test_list}; do
            this_file=`basename $test`
            tmp=`echo "$this_file" | cut -d. -f1`
            echo $tmp

            #----------------------------------------------------------   
            #  remove sat/instrument_anl names so we don't end up
            #  with both "omi_aura" and "omi_aura_anl" if analysis
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
#   cdate=$BDATE
#   nfiles=0
#   while [[ $cdate -le $EDATE ]]; do
#      echo $cdate >> cycle_hrs.txt
#      adate=`$NDATE +${CYCLE_INTERVAL} $cdate`
#      cdate=$adate
#      nfiles=`expr $nfiles + 1`
#   done


   #-------------------------------------------------------------------
   #  Copy the data files and ctl file to workdir
   #-------------------------------------------------------------------
   have_ctl=0
   cdate=$BDATE
    
   while [[ $cdate -le $EDATE ]]; do

      pdy=`echo $cdate | cut -c1-8 `
      cyc=`echo $cdate | cut -c9-10`

      test_dir=${OZN_STATS_TANKDIR}/${RUN}.${pdy}/${cyc}/oznmon/time

      if [[ -d ${test_dir} ]]; then
         test_file=${test_dir}/${type}.${cdate}.ieee_d

         if [[ -s $test_file ]]; then
            $NCP ${test_file} ./${type}.${cdate}.ieee_d
            echo $cdate >> cycle_hrs.txt
         elif [[ -s ${test_file}.${Z} ]]; then
            $NCP ${test_file}.${Z} ./${type}.${cdate}.ieee_d.${Z}
            echo $cdate >> cycle_hrs.txt
         else 
            echo "WARNING:  unable to locate ${test_file}"
         fi
      fi


      if [[ $have_ctl -eq 0 ]]; then
         test_file=${test_dir}/${type}.ctl
         if [[ -s ${test_file} ]]; then
            $NCP ${test_file} ./${type}.ctl
            have_ctl=1
         elif [[ -s ${test_file}.${Z} ]]; then
            $NCP ${test_file}.${Z} ./${type}.ctl.${Z}
            have_ctl=1
         fi
      fi
      adate=`$NDATE +${CYCLE_INTERVAL} $cdate`
      cdate=$adate
   done

   ${UNCOMPRESS} *.${Z}

   #-------------------------------------------------------------------
   #  Get the number of levels for this $type
   #-------------------------------------------------------------------
   line=`cat ${type}.ctl | grep title`
   nlev=`echo $line|gawk '{print $4}'`
   echo levels = $nlev

   #-------------------------------------------------------------------
   #  Cut out the iuse flags from the ctl file and dump them
   #  into the level.txt file for make_base executable to access
   #-------------------------------------------------------------------
   gawk '/iuse/{print $8}' ${type}.ctl >> level.txt

   #-------------------------------------------------------------------
   #  Copy the executable and run it 
   #------------------------------------------------------------------
   out_file=${type}.base
   $NCP ${OZN_DE_EXEC}/oznmon_make_base.x ./

   nfiles=`ls -1 ${type}*ieee_d | wc -l` 

cat << EOF > input
 &INPUT
  satname='${type}',
  nlev=${nlev},
  nfile=${nfiles},
  out_file='${out_file}',
 /
EOF

   ./oznmon_make_base.x < input > stdout.${type}.base

   #-------------------------------------------------------------------
   #  Copy base file back to $tmpdir 
   #-------------------------------------------------------------------
   $NCP $out_file ${tmpdir}/.

   #-------------------------------------------------------------------
   #  Clean up
   #-------------------------------------------------------------------
   cd $tmpdir

done


#-------------------------------------------------------------------
#  Pack all basefiles into a tar file and move it to $TANKverf/info.
#  If a SINGLE_SAT was supplied at the command line then copy the
#  existing $basefile and add/replace the requested sat, leaving
#  all others in the $basefile unchanged.
#-------------------------------------------------------------------
if [[ ! -d ${OZN_STATS_TANKDIR}/info ]]; then
   mkdir -p ${OZN_STATS_TANKDIR}/info
fi

basefile=gdas_oznmon_base.tar

if [[ $SINGLE_SAT -eq 0 ]]; then
   tar -cvf ${basefile} *.base

else
   newbase=$tmpdir/newbase
   mkdir $newbase
   cd $newbase

   #---------------------------------------
   #  copy over existing $basefile
   #
   if [[ -s ${OZN_STATS_TANKDIR}/info/${basefile} ]]; then
      $NCP ${OZN_STATS_TANKDIR}/info/${basefile} ./${basefile} 
   elif [[ -s ${HOMEgdas_ozn}/fix/${basefile} ]]; then
      $NCP ${HOMEgdas_ozn}/fix/${basefile} ./${basefile} 
   fi

   tar -xvf ${basefile}
   rm ${basefile}

   #----------------------------------------------------------------------
   #  copy new *.base file from $tmpdir and build new $basefile (tar file)
   #
   cp -f $tmpdir/*.base .
   tar -cvf ${basefile} *.base
   mv -f ${basefile} $tmpdir/.
   cd $tmpdir

fi

#---------------------------------------------
#  Remove the old version of the $basefile
#
if [[ -e ${OZN_STATS_TANKDIR}/info/${basefile} || -e ${OZN_STATS_TANKDIR}/info/${basefile}.${Z} ]]; then
   rm -f ${OZN_STATS_TANKDIR}/info/${basefile}*
fi


$NCP ${basefile} ${OZN_STATS_TANKDIR}/info/.

#-------------------------------------------------------------------
#  Clean up $tmpdir
#-------------------------------------------------------------------
cd ..
#rm -rf $tmpdir

exit
