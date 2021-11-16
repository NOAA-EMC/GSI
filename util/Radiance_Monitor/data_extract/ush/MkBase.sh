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
  echo "Usage:  MkBase.sh suffix [--sat SAT/INSTRUMENT --run gdas|gfs] " 
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $TANKverf/stats directory."
  echo ""
  echo "            -s,--sat SAT/INSTRUMENT (optional) limits the action of"
  echo "              MkBase.sh to processing only this specified source."
  echo "              Not using --sat means all satellite sources will be"
  echo "              included in the new base file." 
  echo ""
  echo "            -r,--run gdas|gfs (optional) specifies the run.  Use this"
  echo "              if TANK_USE_RUN=1 in the parm/RadMon_user_settings file"
}

nargs=$#
if [[ $nargs -lt 1 || $nargs -gt 5 ]]; then
   usage
   exit 1
fi

while [[ $# -ge 1 ]]
do
   key="$1"
   echo $key

   case $key in
      -s|--sat)
         export SATYPE="$2"
         shift # past argument
      ;;
      -r|--run)
         export RUN="$2"
         shift # past argument
      ;;
      *)
         #any unspecified key is RADMON_SUFFIX
         export RADMON_SUFFIX=$key
      ;;
   esac

   shift
done

echo "RADMON_SUFFIX = $RADMON_SUFFIX"
echo "RUN           = $RUN"
echo "SATYPE        = $SATYPE"
satlen=`echo ${#SATYPE}`
echo "satlen        = $satlen"

SINGLE_SAT=0
if [[ $satlen -gt 0 ]]; then
   SINGLE_SAT=1
fi

this_file=`basename $0`
this_dir=`dirname $0`

#--------------------------------------------------------------------
# Get the area (glb/rgn) for this suffix
#--------------------------------------------------------------------
RAD_AREA=${RAD_AREA:-glb}
area=$RAD_AREA
echo $area

#------------------------------------------------------------------
# Set environment variables.
#-------------------------------------------------------------------
top_parm=${this_dir}/../../parm

export RADMON_CONFIG=${RADMON_CONFIG:-${top_parm}/RadMon_config}
if [[ -s ${RADMON_CONFIG} ]]; then
   . ${RADMON_CONFIG}
else
   echo "Unable to source ${RADMON_CONFIG} file"
   exit 2
fi

if [[ -s ${RADMON_USER_SETTINGS} ]]; then
   . ${RADMON_USER_SETTINGS}
else
   echo "Unable to source ${RADMON_USER_SETTINGS} file"
   exit 2
fi



REGIONAL_RR=${REGIONAL_RR:-0}
echo "REGIONAL_RR   = $REGIONAL_RR"
echo "CYCLE_INTERVAL = $CYCLE_INTERVAL"

#-------------------------------------------------------------------
#  Set dates
#    BDATE is beginning date for the 30/60 day range
#    EDATE is ending date for 30/60 day range (always use 00 cycle) 
#-------------------------------------------------------------------
echo "TANKverf = $TANKverf"
EDATE=`${DE_SCRIPTS}/nu_find_cycle.pl --cyc 1 --dir ${TANKverf} --run $RUN`
echo $EDATE

sdate=`echo $EDATE|cut -c1-8`
EDATE=${sdate}00
BDATE=`$NDATE -1080 $EDATE`
BDATE=`$NDATE -336 $EDATE`

echo EDATE = $EDATE
echo BDATE = $BDATE

tmpdir=${STMP_USER}/base_${RADMON_SUFFIX}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

#-------------------------------------------------------------------
#  If no single sat source was supplied at the command line then 
#  find or build $SATYPE list for this data source.
#-------------------------------------------------------------------
if [[ $SINGLE_SAT -eq 0 ]]; then
   if [[ -e ${TANKverf}/info/SATYPE.txt ]]; then
      SATYPE=`cat ${TANKverf}/info/SATYPE.txt`
   else
      PDY=`echo $EDATE|cut -c1-8`
      CYC=`echo $EDATE|cut -c9-10`

      if [[ $TANK_USE_RUN -eq 1 ]]; then
         testdir=${TANKverf}/${RUN}.${PDY}/${CYC}/radmon
      else
         testdir=${TANKverf}/radmon.${PDY}
      fi
      echo "testdir = $testdir"

      if [[ -d ${testdir} ]]; then
         test_list=`ls ${testdir}/angle.*${EDATE}.ieee_d*`
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

echo SATYPE = $SATYPE
echo TANK_USE_RUN = $TANK_USE_RUN

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
   nfiles=0
   while [[ $cdate -le $EDATE ]]; do
      echo $cdate >> cycle_hrs.txt
      adate=`$NDATE +${CYCLE_INTERVAL} $cdate`
      cdate=$adate
      nfiles=`expr $nfiles + 1`
   done


   #-------------------------------------------------------------------
   #  Copy the data files and ctl file to workdir
   #-------------------------------------------------------------------
   cdate=$BDATE
   while [[ $cdate -le $EDATE ]]; do
      if [[ $REGIONAL_RR -eq 1 ]]; then
         tdate=`$NDATE +6 $cdate`
         day=`echo $tdate | cut -c1-8 `
         hh=`echo $cdate | cut -c9-10`
         . ${IG_SCRIPTS}/rr_set_tz.sh $hh
      else
         day=`echo $cdate | cut -c1-8 `
      fi

      day=`echo $cdate | cut -c1-8 `
      cyc=`echo $cdate | cut -c9-10 `
     
      if [[ $TANK_USE_RUN -eq 1 ]]; then
         testday=${TANKverf}/${RUN}.${day}/${cyc}/radmon
      else
         testday=${TANKverf}/radmon.${day}
      fi
      echo "testday = $testday"

      if [[ -d ${testday} ]]; then
         if [[ $REGIONAL_RR -eq 1 ]]; then
            test_file=${testday}/${rgnHH}.time.${type}.${cdate}.ieee_d.${rgnTM}
         else
            test_file=${testday}/time.${type}.${cdate}.ieee_d
         fi

         if [[ -s $test_file ]]; then
            $NCP ${test_file} ./${type}.${cdate}.ieee_d
         elif [[ -s ${test_file}.${Z} ]]; then
            $NCP ${test_file}.${Z} ./${type}.${cdate}.ieee_d.${Z}
         fi
      fi

      adate=`$NDATE +${CYCLE_INTERVAL} $cdate`
      cdate=$adate
   done



   test_file=${testday}/time.${type}.ctl
 
   if [[ -s ${test_file} ]]; then
      $NCP ${test_file} ${type}.ctl
   elif [[ -s ${test_file}.${Z} ]]; then
      $NCP $test_file.${Z} ${type}.ctl.${Z}
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
   $NCP ${DE_EXEC}/radmon_mk_base.x ./make_base

cat << EOF > input
 &INPUT
  satname='${type}',
  n_chan=${nchan},
  nregion=1,
  nfile=${nfiles},
  date='${EDATE}',
  out_file='${out_file}',
 /
EOF

   ./make_base < input > stdout.${type}.base

   #-------------------------------------------------------------------
   #  Copy base file back to $tmpdir 
   #-------------------------------------------------------------------
   $NCP $out_file ${tmpdir}/.

   cd $tmpdir

done


#-------------------------------------------------------------------
#  Pack all basefiles into a tar file and move it to $TANKverf/info.
#  If a SINGLE_SAT was supplied at the command line then copy the
#  existing $basefile and add/replace the requested sat, leaving
#  all others in the $basefile unchanged.
#-------------------------------------------------------------------
if [[ ! -d ${TANKverf}/info ]]; then
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
   if [[ -e ${TANKverf}/info/${basefile} || -e ${TANKverf}/info/${basefile}.${Z} ]]; then
      $NCP ${TANKverf}/info/${basefile}* .
      if [[ -e ${basefile}.${Z} ]]; then
         $UNCOMPRESS ${basefile}.${Z}
      fi
      tar -xvf ${basefile}
   fi

   #  copy new *.base file from $tmpdir and build new $basefile (tar file)
   cp -f $tmpdir/*.base .
   tar -cvf ${basefile} *.base
   mv -f ${basefile} $tmpdir/.
   cd $tmpdir

fi

#  Remove the old version of the $basefile
if [[ -e ${TANKverf}/info/${basefile} || -e ${TANKverf}/info/${basefile}.${Z} ]]; then
   rm -f ${TANKverf}/info/${basefile}*
fi


$NCP ${basefile} ${TANKverf}/info/.

#-------------------------------------------------------------------
#  Clean up $tmpdir
#-------------------------------------------------------------------
cd ..
rm -rf $tmpdir

exit
