#!/bin/sh -l

#-----------------------------------------------------------------------
#  OznMon_Plt.sh
#
#  Main plot script for OznMon.
#
#  Usage:
#
#    OznMon_Plt.sh OZNMON_SUFFIX [-p|pdate yyyymmddcc] [-r|run gdas|gfs]
#
#	OZNMON_SUFFIX = data source identifier which matches data 
#		  	in the TANKverf/stats directory.
#       -p --pdate    = specified cycle to plot.  If not specified the
#			last available date will be plotted.
#	-r --run      = $RUN value, gdas|gfs, default is gdas.
#       -c1|--comp1   = define first source to plot as comparison (time
#			   series plots only)
#       -c2|--comp2   = define second source to plot as comparison (time
#			   series plots only)
#
#	NOTE:  Both COMP1 and COMP2 have to be defined to 
#	       generate comparison plots as part of the COMP1
#	       source's time plots.
#-----------------------------------------------------------------------

function usage {
  echo " "
  echo "Usage:  OznMon_Plt.sh OZNMON_SUFFIX "
  echo "            OZNMON_SUFFIX is data source identifier which matches data in "
  echo "              the $TANKverf/stats directory."
  echo "            -p | --pdate yyyymmddcc to specify the cycle to be plotted."
  echo "              If unspecified the last available date will be plotted."
  echo "            -r | --run  the gdas|gfs run to be plotted, gdas is default"
  echo "            -c1| --comp1 first instrument/sat source to plotted as a comparision"
  echo "            -c2| --comp2 first instrument/sat source to plotted as a comparision"
  echo " "
}

echo start OznMon_Plt.sh
set -ax

nargs=$#
echo nargs = $nargs



while [[ $# -ge 1 ]]
do
   key="$1"
   echo $key

   case $key in
      -p|--pdate)
         PDATE="$2"
         shift # past argument
      ;;
      -r|--run)
         export RUN="$2"
         shift # past argument
      ;;
      -c1|--comp1)
	 export COMP1="$2"
         shift # past argument
      ;;
      -c2|--comp2)
	 export COMP2="$2"
         shift # past argument
      ;;
      *)
         #any unspecified key is OZNMON_SUFFIX
         export OZNMON_SUFFIX=$key
      ;;
   esac

   shift
done

if [[ $nargs -lt 0 || $nargs -gt 9 ]]; then
   usage
   exit 1
fi

if [[ $OZNMON_SUFFIX = "" ]]; then
   echo ""
   echo "ERROR:  OZNMON_SUFFIX not specified in input"
   echo ""
   usage
   exit 2
fi


if [[ ${#RUN} -le 0 ]]; then
   echo "setting RUN to gdas"
   export RUN=gdas 
fi

echo "OZNMON_SUFFIX = $OZNMON_SUFFIX"
echo "PDATE         = $PDATE"
echo "RUN           = $RUN"


export DO_COMP=0
if [[ ${#COMP1} > 0 && ${#COMP2} > 0 ]]; then
   export DO_COMP=1
fi


this_file=`basename $0`
this_dir=`dirname $0`

#--------------------------------------------------
# source verison, config, and user_settings files
#--------------------------------------------------
top_parm=${this_dir}/../../parm


oznmon_version_file=${oznmon_version:-${top_parm}/OznMon.ver}
if [[ -s ${oznmon_version_file} ]]; then
   . ${oznmon_version_file}
   echo "able to source ${oznmon_version_file}"
else
   echo "Unable to source ${oznmon_version_file} file"
   exit 2
fi

oznmon_user_settings=${oznmon_user_settings:-${top_parm}/OznMon_user_settings}
if [[ -s ${oznmon_user_settings} ]]; then
   . ${oznmon_user_settings}
   echo "able to source ${oznmon_user_settings}"
else
   echo "Unable to source ${oznmon_user_settings} file"
   exit 4
fi

oznmon_config=${oznmon_config:-${top_parm}/OznMon_config}
if [[ -s ${oznmon_config} ]]; then
   . ${oznmon_config}
   echo "able to source ${oznmon_config}"
else
   echo "Unable to source ${oznmon_config} file"
   exit 3
fi


#--------------------------------------------------------------------
#  Check for my monitoring use.  Abort if running on prod machine.
#--------------------------------------------------------------------
if [[ RUN_ONLY_ON_DEV =  1 ]]; then
   is_prod=`${OZN_IG_SCRIPTS}/onprod.sh`
   if [[ $is_prod = 1 ]]; then
      exit 10
   fi
fi


#--------------------------------------------------------------------
#  Specify TANKDIR for this suffix
#--------------------------------------------------------------------
if [[ $GLB_AREA -eq 1 ]]; then
   export TANKDIR=${OZN_TANKDIR}/stats/${OZNMON_SUFFIX}
else
   export TANKDIR=${OZN_TANKDIR}/stats/regional/${OZNMON_SUFFIX}
fi

#--------------------------------------------------------------------
#  Set up OZN_IMGN_TANKDIR
#--------------------------------------------------------------------
if [[ ! -d ${OZN_IMGN_TANKDIR} ]]; then
   mkdir -p ${OZN_IMGN_TANKDIR}
fi


#--------------------------------------------------------------------
#  If PDATE wasn't specified as an argument then plot the last
#  available cycle.
#--------------------------------------------------------------------
if [[ ${#PDATE} -le 0 ]]; then
   echo "PDATE not specified:  setting PDATE using last cycle"
   PDATE=`${OZN_IG_SCRIPTS}/find_cycle.pl -run ${RUN} -cyc 1 -dir ${TANKDIR}`
else
   echo "PDATE was specified:  $PDATE"
fi

export PDATE=$PDATE
export PDY=`echo $PDATE|cut -c1-8`
export cyc=`echo $PDATE|cut -c9-10`

#--------------------------------------------------------------------
#  Create the WORKDIR and link the data files to it
#--------------------------------------------------------------------
export WORKDIR=${STMP_USER}/${OZNMON_SUFFIX}/${RUN}/oznmon/IG.${PDY}.${cyc}
if [[ -d $WORKDIR ]]; then
  rm -rf $WORKDIR
fi
mkdir $WORKDIR
cd $WORKDIR

#--------------------------------------------------------------------
#  Plot scripts are plot_time.sh and plot_horiz.sh.  The plot_time.sh
#  script calls plot_summary.sh.  The plot_time & plot_horiz are
#  submitted jobs.
#
#  All plot_* scripts call transfer.sh.  We'll handle that like the
#  other monitors.
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#  export SATYPE
#  
#  For the moment we can just load the 
#  gdas.v2.0.0/fix/gdas_oznmon_satype.txt file.  Eventually DE will
#  need to compare actual files vs this list (or an updated one in 
#  TANKDIR/info like RadMon.
#
#  Update the search order to use the local copy in $TANKDIR/info
#  if available.
#--------------------------------------------------------------------
if [[ -e ${TANKDIR}/info/gdas_oznmon_satype.txt ]]; then
   export SATYPE=${SATYPE:-`cat ${TANKDIR}/info/${RUN}_oznmon_satype.txt`}
else
   export SATYPE=${SATYPE:-`cat ${HOMEgdas_ozn}/fix/${RUN}_oznmon_satype.txt`}
fi


${OZN_IG_SCRIPTS}/mk_horiz.sh
${OZN_IG_SCRIPTS}/mk_time.sh
${OZN_IG_SCRIPTS}/mk_summary.sh

if [[ $DO_DATA_RPT -eq 1 ]]; then
   ${OZN_IG_SCRIPTS}/mk_err_rpt.sh
fi

echo "end OznMon_Plt.sh"
exit
