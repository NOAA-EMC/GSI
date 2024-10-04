#!/bin/sh

#--------------------------------------------------------------------
#
#  CMon_Install_html.sh 
#
#  This is the html installation for the Conventional Data Monitor
#  (Cmon) package.  
#
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#  usage
#--------------------------------------------------------------------
function usage {
  echo " "
  echo "Usage:  CMon_Install_html.sh [--o] suffix"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $C_TANKDIR/stats directory."
  echo "	    The --o flag can be used if this installation is"
  echo " 	    designed for operational rather than experimental data"
}


#--------------------------------------------------------------------
#  CMon_IG.sh begins here
#--------------------------------------------------------------------

echo "Begin CMon_IG.sh"


#------------------------------
#  process command line input
#
OPTIND=1
nargs=$#
echo "nargs = $nargs"

if [[ $nargs -lt 1 || $nargs -gt 2 ]]; then
   usage
   exit 1
fi

this_dir=`dirname $0`

verbose=0
operational=0
while getopts "vo" opt; do
    echo "opt = $opt"
    case "$opt" in
    v)  verbose=1
	;;
    o)  operational=1
        ;;
    esac
done

shift $((OPTIND-1))

[ "$1" = "--" ] && shift

CMON_SUFFIX=$@

echo "operational=$operational, verbose=$verbose, suffix=$CMON_SUFFIX, Leftovers: $@"

if [[ $verbose -eq 1 ]]; then
   set -ax
fi



#--------------------------------------------------------------------
# Run config files to load environment variables to pick up WEB* 
# definitions.
#--------------------------------------------------------------------
top_parm=${this_dir}/../../parm

cmon_version_file=${cmon_version:-${top_parm}/CMon.ver}
if [[ -s ${cmon_version_file} ]]; then
   . ${cmon_version_file}
   echo "able to source ${cmon_version_file}"
else
   echo "Unable to source ${cmon_version_file} file"
   exit 3
fi

cmon_config=${cmon_config:-${top_parm}/CMon_config}
if [[ -s ${cmon_config} ]]; then
   . ${cmon_config}
   echo "able to source ${cmon_config}"
else
   echo "Unable to source ${cmon_config} file"
   exit 4
fi

echo "WEBSVR  = $WEBSVR"
echo "WEBUSER = $WEBUSER"
echo "WEBDIR  = $WEBDIR"

#--------------------------------------------------------------
#  Create a temporary working directory.
#
workdir=${C_STMP_USER}/${CMON_SUFFIX}_html

if [[ -d $workdir ]]; then
   rm -rf $workdir
fi

mkdir -p $workdir
cd $workdir

#--------------------------------------------------------------------
#  copy over the html files
#--------------------------------------------------------------------
cp -f ${C_IG_HTML}/*.html .


#--------------------------------------------------------------------
#  Replace "_SUFFIX_" in html files with $CMON_SUFFIX
#--------------------------------------------------------------------
html_files="index.html horzbody.html index_hist.html index_horz.html index_time.html index_vert.html intro_hist.html intro_horz.html intro_time.html intro_vert.html"

for file in $html_files; do
   echo "processing file $file"
   sed -i -e "s/_SUFFIX_/$CMON_SUFFIX/g" $file
done


if [[ $operational -eq 1 ]]; then
   ex_files="index.html "
   echo "replacing Experimental with Operational"
   for file in $ex_files; do
      echo "processing file $file"
      sed -i -e "s/Experimental/Operational/g" $file
   done
fi

echo "C_IMGNDIR = $C_IMGNDIR"
#-----------------------
#  move html files to C_IMGNDIR
#
if [[ ! -d ${C_IMGNDIR} ]]; then
   mkdir -p ${C_IMGNDIR}
fi

all_files=`ls *.html`
for file in $all_files; do
   $NCP ${file} ${C_IMGNDIR}/${file}
done


#---------------------------------------------------
# if on wcoss then cd $imgndir and do the rsync here
#

if [[ $MY_MACHINE = "wcoss" ]]; then
   if [[ ${C_IMGNDIR} != "/" ]]; then      # sanity check to avoid serious embarrassment
      echo "rsync is go"
      ssh ${WEBUSER}@${WEBSVR}.ncep.noaa.gov "mkdir ${WEBDIR}/${CMON_SUFFIX}"

      /usr/bin/rsync -ave ssh  ${C_IMGNDIR}/ \
         ${WEBUSER}@${WEBSVR}.ncep.noaa.gov:${WEBDIR}/${CMON_SUFFIX}
   fi
fi
 
#
##--------------------------------------------------------------------
##  Create workdir and cd to it
##--------------------------------------------------------------------
#
#export C_PLOT_WORKDIR=${C_PLOT_WORKDIR:-${C_STMP_USER}/plot_cmon_${CMON_SUFFIX}}
#rm -rf $C_PLOT_WORKDIR
#mkdir -p $C_PLOT_WORKDIR
#cd $C_PLOT_WORKDIR
#
#
#
#
##--------------------------------------------------------------------
## Clean up and exit
##cd $tmpdir
##cd ../
##rm -rf $tmpdir

echo "End CMon_Install_html.sh"
exit
