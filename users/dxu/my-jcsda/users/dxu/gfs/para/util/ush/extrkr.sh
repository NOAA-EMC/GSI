#!/bin/ksh

export PS4=' + extrkr.sh line $LINENO: '

set +x
echo "TIMING: Time at beginning of extrkr.sh for pert= $pert is `date`"
set -x

set +x
##############################################################################
echo " "
echo "------------------------------------------------"
echo "xxxx - Track vortices in model GRIB output"
echo "------------------------------------------------"
echo "History: Mar 1998 - Marchok - First implementation of this new script."
echo "         Apr 1999 - Marchok - Modified to allow radii output file and"
echo "                              to allow reading of 4-digit years from"
echo "                              TC vitals file."
echo "         Oct 2000 - Marchok - Fixed bugs: (1) copygb target grid scanning mode"
echo "                              flag had an incorrect value of 64 (this prevented"
echo "                              NAM, NGM and ECMWF from being processed correctly);" 
echo "                              Set it to 0.  (2) ECMWF option was using the "
echo "                              incorrect input date (today's date instead of "
echo "                              yesterday's)."
echo "         Jan 2001 - Marchok - Hours now listed in script for each model and "
echo "                              passed into program.  Script included to process"
echo "                              GFDL & Ensemble data.  Call to DBN included to "
echo "                              pass data to OSO and the Navy.  Forecast length"
echo "                              extended to 5 days for GFS & MRF."
echo "         Aug 2005 - Marchok - Added ability to process ECMWF ensemble, ECMWF"
echo "                              hires out to 240 (every 12h), CMC hires, CMC"
echo "                              ensemble, GFS extended from 126h to 180h."
echo "         May 2006 - Wobus -   For 2006 NCEP ensemble implementation, changed"
echo "                              directory names."
echo "         Jun 2006 - Marchok - Changed handling of NCEP ensemble files beyond"
echo "                              180h.  These are now 1-deg instead of 2.5-deg,"
echo "                              so there is no longer a need to interpolate "
echo "                              down to 1-deg for these files.  Also, changed"
echo "                              the COM directory for CMC."
echo " "
echo "                    In the event of a crash, you can contact Tim "
echo "                    Marchok at GFDL at (609) 452-6534 or timothy.marchok@noaa.gov"
echo " "
echo "Current time is: `date`"
echo " "
##############################################################################
set -x

##############################################################################
#
#    FLOW OF CONTROL
#
# 1. Define data directories and file names for the input model 
# 2. Process input starting date/cycle information
# 3. Update TC Vitals file and select storms to be processed
# 4. Cut apart input GRIB files to select only the needed parms and hours
# 5. Execute the tracker
# 6. Copy the output track files to various locations
#
##############################################################################

########################################
msg="has begun for ${cmodel} at ${CYL}z"
postmsg "$jlogfile" "$msg"
########################################

# This script runs the hurricane tracker using operational GRIB model output.  
# This script makes sure that the data files exist, it then pulls all of the 
# needed data records out of the various GRIB forecast files and puts them 
# into one, consolidated GRIB file, and then runs a program that reads the TC 
# Vitals records for the input day and updates the TC Vitals (if necessary).
# It then runs gettrk, which actually does the tracking.
# 
# Environmental variable inputs needed for this scripts:
#  PDY   -- The date for data being processed, in YYYYMMDD format
#  CYL   -- The numbers for the cycle for data being processed (00, 06, 12, 18)
#  cmodel -- Model being processed (gfs, mrf, ukmet, ecmwf, nam, ngm, ngps,
#                                   gdas, gfdl, ens (ncep ensemble), ensm (ncep
#                                   ensemble run off of the mean fields)
#  envir -- 'prod' or 'test'
#  SENDCOM -- 'YES' or 'NO'
#  stormenv -- This is only needed by the tracker run for the GFDL model.
#              'stormenv' contains the name/id that is used in the input
#              grib file names.
#  pert  -- This is only needed by the tracker run for the NCEP ensemble.
#           'pert' contains the ensemble member id (e.g., n2, p4, etc.)
#           which is used as part of the grib file names.
#
# For testing script interactively in non-production set following vars:
#     gfsvitdir  - Directory for GFS Error Checked Vitals
#     namvitdir  - Directory for NAM Error Checked Vitals
#     gltrkdir   - Directory for output tracks
#     homesyndir - Directory with syndir scripts/exec/fix 
#     archsyndir - Directory with syndir scripts/exec/fix 
#

qid=$$
#----------------------------------------------#
#   Get input date information                 #
#----------------------------------------------#

export PDY=${PDY:-$1}
export CYL=${CYL:-${cyc:-$2}}
export CYCLE=t${CYL}z
export cmodel=${cmodel:-$3}
export jobid=${jobid:-testjob}
export envir=${envir:-test}
export SENDCOM=${SENDCOM:-NO}
export PARAFLAG=${PARAFLAG:-NO}
export DISK_TRAK=${DISK_TRAK:-/global/save}
export TRKDATA=${TRKDATA:-$DATA}
export COMDIR=${COMDIR:-""}
export ATCFdir=${ATCFdir:-$COMDIR/com/tpc/${envir}/atcf}

export flag_pgb=${flag_pgb:-f}

export DATA=${DATA:-/ptmpp1/$LOGNAME/trakout}
export NWPROD=${NWPROD:-/nwprod}
if [ ! -d $DATA ]
then
   mkdir -p $DATA
   cd $DATA
   $NWPROD/util/ush/setup.sh
fi
cd $DATA

if [ ${PARAFLAG} = 'YES' ]
then 
  $NWPROD/util/ush/setup.sh
else
#TM take out this else part for operations.....
  $NWPROD/util/ush/setup.sh
fi

if [ ${#PDY} -eq 0 -o ${#CYL} -eq 0 -o ${#cmodel} -eq 0 ]
then
  set +x
  echo
  echo "Something wrong with input data.  One or more input variables has length 0"
  echo "PDY= ${PDY}, CYL= ${CYL}, cmodel= ${cmodel}"
  echo "EXITING...."
  set -x
  err_exit " FAILED ${jobid} -- BAD INPUTS AT LINE $LINENO IN TRACKER SCRIPT - ABNORMAL EXIT"
else
  set +x
  echo " "
  echo " #-----------------------------------------------------------------#"
  echo " At beginning of tracker script, the following imported variables "
  echo " are defined: "
  echo "   PDY ................................... $PDY"
  echo "   CYL ................................... $CYL"
  echo "   CYCLE ................................. $CYCLE"
  echo "   cmodel ................................ $cmodel"
  echo "   jobid ................................. $jobid"
  echo "   envir ................................. $envir"
  echo "   SENDCOM ............................... $SENDCOM"
  echo " "
  set -x
fi

syy=`echo ${PDY} | cut -c3-4`
smm=`echo ${PDY} | cut -c5-6`
sdd=`echo ${PDY} | cut -c7-8`
shh=${CYL}
symd=`echo ${PDY} | cut -c3-8`
syyyy=`echo ${PDY} | cut -c1-4`

export gfsvitdir=${gfsvitdir:-$COMDIR/com/gfs/prod/gfs.$PDY}
export namvitdir=${namvitdir:-$COMDIR/com/nam/prod/nam.$PDY}
export gltrkdir=${gltrkdir:-$COMDIR/com/hur/${envir}/global}

export homesyndir=${homesyndir:-$NWPROD/util}
export exectrkdir=${exectrkdir:-${homesyndir}/exec}
export ushtrkdir=${ushtrkdir:-${homesyndir}/ush}
export archsyndir=${archsyndir:-$COMDIR/com/arch/prod/syndat}

export CENT=`echo ${PDY} | cut -c1-2`

if [ -s $NWPROD/util/exec/wgrib ]
then
  wgrib=$NWPROD/util/exec/wgrib
else
  set +x
  echo " "
  echo "!!! ERROR: wgrib is not available, script will crash.  Exiting...."
  echo " "
  set -x
  err_exit " FAILED ${jobid} -- line= $LINENO IN TRACKER SCRIPT - ABNORMAL EXIT"
fi

wgrib_parmlist=" HGT:850 HGT:700 UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 SurfaceU SurfaceV ABSV:850 ABSV:700 MSLET:MSL "
wgrib_egrep_parmlist="HGT:850|HGT:700|UGRD:850|UGRD:700|UGRD:500|VGRD:850|VGRD:700|VGRD:500|UGRD:10 m |VGRD:10 m |ABSV:850|ABSV:700|PRMSL:MSL"
wgrib_ec_hires_parmlist=" GH:850 GH:700 U:850 U:700 U:500 V:850 V:700 V:500 10U:sfc 10V:sfc MSL:sfc "

export maxtime=65    # Max number of forecast time levels

#----------------------------------------------------------------#
#
#    --- Define data directories and data file names ---
#               
# Convert the input model to lowercase letters and check to see 
# if it's a valid model, and assign a model ID number to it.  
# This model ID number is passed into the Fortran program to 
# let the program know what set of forecast hours to use in the 
# ifhours array.  Also, set the directories for the operational 
# input GRIB data files and create templates for the file names.
# While only 1 of these sets of directories and file name 
# templates is used during a particular run of this script, 
# "gfsvitdir" is used every time, because that is the directory 
# that contains the error-checked TC vitals file that Steve Lord 
# produces, and so it is included after the case statement.
#
# NOTE: The varible PDY is now defined within the J-Jobs that
# call this script.  Therefore there is no reason to do this
# here.
#
# NOTE: The script that processes the ECMWF data defines PDY as
# the current day, and in this script we need PDY to be 
# yesterday's date (for the ecmwf ONLY).  So instead, the ecmwf
# script will pass the variable PDYm1 to this script, and in the
# case statement below we change that to PDY.
#
# NOTE: Do NOT try to standardize this script by changing all of 
# these various data directories' variable names to be all the 
# same, such as "datadir".  As you'll see in the data cutting 
# part of this script below, different methods are used to cut 
# apart different models, thus it is important to know the 
# difference between them....
#----------------------------------------------------------------#

cmodel=`echo ${cmodel} | tr "[A-Z]" "[a-z]"`

case ${cmodel} in 

  gfs) set +x                                       ;
       echo " "; echo " ++ operational GFS chosen"  ;
       echo " "                                     ;
       set -x                                       ;
       gfsdir=/com/gfs/prod/gfs.${PDY}              ;
       gfsgfile=gfs.t${CYL}z.master.grbf            ;
       gfsifile=gfs.t${CYL}z.master.grbif           ;
       COM=/com/gfs/${envir}/gfs.${PDY}             ;
       fcstlen=180                                  ;
       fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78 
                 84 90 96 102 108 114 120 126 132 138 144  
                 150 156 162 168 174 180  99  99  99  99   
                 99  99  99  99  99  99  99  99  99  99    
                 99  99  99  99  99  99  99  99  99  99    
                 99  99  99  99  99  99  99  99  99  99' ;
       atcfnum=15                                   ;
       atcfname="avno"                              ;
       atcfout="avn"                                ;
       modtyp='global'                              ;
       model=1                                     ;;

  gfs00) set +x                                       ;
       echo " "; echo " ++ operational GFS chosen"  ;
       echo " "                                     ;
       set -x                                       ;
       gfsdir=/com/gfs/prod/gfs.${PDY}              ;
       gfsgfile=gfs.t${CYL}z.master.grbf            ;
       gfsifile=gfs.t${CYL}z.master.grbif           ;
       COM=/com/gens/${envir}/gefs.${PDY}/$cyc/init                ;
       [[ ! -d $COM ]] && mkdir -p $COM                 ;
       fcstlen=00                                   ;
       fcsthrs=' 00 99 99 99 99 99 99 99 99 99 99 99 99 99
                 99 99 99  99  99  99  99  99  99  99  99
                  99  99  99  99  99  99  99  99  99  99
                 99  99  99  99  99  99  99  99  99  99
                 99  99  99  99  99  99  99  99  99  99
                 99  99  99  99  99  99  99  99  99  99' ;
       atcfnum=15                                   ;
       atcfname="zgfs"                              ;
       atcfout="zgfs"                                ;
       modtyp='global'                              ;
       model=1                                     ;;

  mrf) set +x                                       ;
       echo " "; echo " ++ operational MRF chosen"  ;
       echo " "                                     ;
       set -x                                       ;
       # Operational MRF is now defunct, but leave this in 
       # here in case we need to do historical MRF cases.
       mrfdir=/com/mrf/prod/mrf.${PDY}              ;
       mrfgfile=drfmr.t${CYL}z.pgrbf                ;
       mrfifile=drfmr.t${CYL}z.pgrbif               ;
       COM=/com/mrf/${envir}/mrf.${PDY}             ;
       fcstlen=168                                  ;
       fcsthrs=' 00 12 24 36 48 60 72 84 96 108 120 132 144  
                 156 168 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99   
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99   
                 99 99 99 99 99 99 99 99 99 99'     ;
       atcfnum=16                                   ;
       atcfname="mrfo"                              ;
       atcfout="mrf"                                ;
       modtyp='global'                              ;
       model=2                                     ;;

  para) set +x                                      ;
       echo " "; echo " ++ GFS para chosen"         ;
       echo " "                                     ;
       set -x                                       ;
#       paradir=  ---> value of paradir is exported in calling script.
#       paragfile= .... See para section below ....
#       paraifile= .... See para section below ....
       COM=${COMOUT}                                ;
       fcstlen=180                                  ;
       para_def_fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78
                 84 90 96 102 108 114 120 126 132 138 144
                 150 156 162 168 174 180  99  99  99  99
                 99  99  99  99  99  99  99  99  99  99
                 99  99  99  99  99  99  99  99  99  99
                 99  99  99  99  99  99  99  99  99  99' ;
       fcsthrs=${FCSTHRS:-para_def_fcsthrs}         ;
       atcfnum=71                                   ;

       atcfname="${TRACKID}"                        ;
       atcfout="${TRACKID}"                         ;
       modtyp='global'                              ;
       cdump=${CDUMP:-gfs}                          ;
       model=99                                    ;;

  ukmet) set +x                                         ; 
       echo " "; echo " ++ operational UKMET chosen"    ;
       echo " "                                         ;
       set -x                                           ;
       ukmetdir=/com/mrf/prod/ukmet.${PDY}              ;
       ukmetgfile=ukmet.t${CYL}z.ukmet                  ;
       ukmetifile=ukmet.t${CYL}z.ukmeti                 ;
       COM=/com/mrf/${envir}/ukmet.${PDY}               ;
       fcstlen=72                                       ;
       fcsthrs=' 00 12 24 36 48 60 72 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99' ;
       atcfnum=17                                       ;
       atcfname="ukx "                                  ;
       atcfout="ukx"                                    ;
       modtyp='global'                                  ;
       model=3                                         ;;

  ecmwf) set +x                                         ;
       echo " "; echo " ++ operational high-res ECMWF chosen"    ;
       echo " "                                         ;
       set -x                                           ;
       ecmwfdir=/dcom/us007003/${PDY}/wgrbbul/ecmwf     ;
       ecmwfgfile=                                      ;
       ecmwfifile=                                      ;
       COM=/com/mrf/${envir}/ecmwf.${PDY}               ;
       [[ ! -d $COM ]] && mkdir -p $COM                 ;
       fcstlen=240                                      ;
       fcsthrs=' 00 12 24 36 48 60 72 84 96 108 120 132 144 
                 156 168 180 192 204 216 228 240 99 99 99   
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99  
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99  
                 99 99 99 99 99 99 99 99 99 99 99 99 99' ;
       atcfnum=19                                       ;
       atcfname="emx "                                  ;
       atcfout="emx"                                    ;
       modtyp='global'                                  ;
       SENDTRACKER=NO                                   ;
       SENDDBN=NO                                       ;
       model=4                                         ;;

  ngm) set +x                                       ;
       echo " "; echo " ++ operational NGM chosen"  ;
       echo " "                                     ;
       set -x                                       ;
       ngmdir=/com/ngm/prod/ngm.${PDY}              ;
       ngmgfile=ngm.${CYCLE}.pgrb.f                 ;
       ngmifile=ngm.${CYCLE}.pgrbif                 ;
       COM=/com/ngm/${envir}/ngm.${PDY}             ;
       fcstlen=48                                   ;
       fcsthrs=' 00 06 12 18 24 30 36 42 48 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99';
       atcfnum=75                                   ;
       atcfname="ngm "                              ;
       atcfout="ngm"                                ;
       modtyp='regional'                            ;
       model=5                                     ;;

  nam) set +x                                             ;
       echo " "; echo " ++ operational Early NAM chosen"  ;
       echo " "                                           ;
       set -x                                             ;
       namdir=/com/nam/prod/nam.${PDY}                    ;
       namgfile=nam.t${CYL}z.awip32                       ;
       namifile=nam.t${CYL}z.awip32i                      ;
       COM=/com/nam/${envir}/nam.${PDY}                   ;
       fcstlen=84
       fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78 
                 84 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99';
       atcfnum=73                                         ;
       atcfname="nam "                                    ;
       atcfout="nam"                                      ;
       modtyp='regional'                                  ;
       model=6                                           ;;

  ngps) set +x                                          ;
       echo " "; echo " ++ operational NOGAPS chosen"   ;
       echo " "                                         ;
       set -x                                           ;
       ngpsdir=/com/fnmoc/${envir}/nogaps.${PDY}        ;
       ngpsgfile=nogaps_${PDY}${CYL}f                   ;
       ngpsifile=                                       ;
       COM=/com/fnmoc/${envir}/nogaps.${PDY}            ;
       fcstlen=144                                      ;
       fcsthrs=' 00 12 24 36 48 60 72 84 96 108 120 132 144 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99  
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99  
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99  
                 99 99 99 99 99 99 99 99 99 99' ;
       atcfnum=29                                       ;
       atcfname="ngx "                                  ;
       atcfout="ngx"                                    ;
       modtyp='global'                                  ;
       model=7                                         ;; 

  gdas) set +x                                          ;
       echo " "; echo " ++ operational GDAS chosen"     ;
       echo " "                                         ;
       set -x                                           ;
       gdasdir=/com/gfs/prod/gdas.${PDY}                ;
       gdasgfile=gdas1.t${CYL}z.pgrbf                   ;
       gdasifile=gdas1.t${CYL}z.pgrbif                  ;
       COM=/com/gfs/${envir}/gdas.${PDY}                ;
       fcstlen=9                                        ;
       fcsthrs=' 00 03 06 09 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99' ;
       atcfnum=72                                       ;
       atcfname="gdas"                                  ;
       atcfout="gdas"                                   ;
       modtyp='global'                                  ;
       model=19                                         ;;

  gfdl) set +x                                          ;
       echo " "; echo " ++ operational GFDL chosen"     ;
       echo " "                                         ;
       set -x                                           ;
       gfdldir=${gfdldir:-/com/hur/prod/hur.${PDY}${CYL}}  ;
       gfdlgfile=${stormenv}.${PDY}${CYL}.grib6th.f     ;
       gfdlifile=${stormenv}.${PDY}${CYL}.grib6th.if    ;
       COM=/com/hur/${envir}/hur.${PDY}${CYL}           ;
       fcstlen=126                                      ;
       fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78 
                 84 90 96 102 108 114 120 126 99 99 99 99  
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99';
       stormid=${atcfid}                                ;
       atcfnum=81                                       ;
       atcfname="gfdt"                                  ;
       atcfout="gfdt"                                   ;
       modtyp='regional'                                ;
       model=9                                         ;;

  ens) set +x                                           ;
       echo " "; echo " ++ operational NCEP ensemble member ${pert} chosen"     ;
       pert=` echo ${pert} | tr '[A-Z]' '[a-z]'`        ;
       PERT=` echo ${pert} | tr '[a-z]' '[A-Z]'`        ;
       echo " "                                         ;
       set -x                                           ;
       ensdir=/com/gens/prod/gefs.${PDY}/$cyc/pgrba     ;
       ensgfile=ge${pert}.t${CYL}z.pgrbaf               ;
       ensifile=ge${pert}.t${CYL}z.pgrbaif              ;
       COM=${COM:-/com/gens/${envir}/gefs.${PDY}/$cyc/track} ;
       [[ ! -d $COM ]] && mkdir -p $COM                 ;
       fcstlen=240                                      ;
       fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78
                 84 90 96 102 108 114 120 126 132 138 144
                 150 156 162 168 174 180 186 192 198 204 210
                 216 222 228 234 240  99  99  99  99  99  99
                  99  99  99  99  99  99  99  99  99  99  99
                  99  99  99  99  99  99  99';
       atcfnum=91                                       ;
       pert_posneg=` echo "${pert}" | cut -c1-1`        ;
       pert_num=`    echo "${pert}" | cut -c2-3`        ;
       atcfname="a${pert_posneg}${pert_num}"            ;
       atcfout="a${pert_posneg}${pert_num}"             ;
       modtyp='global'                                  ;
       SENDTRACKER=NO                                   ;
       SENDDBN=NO                                       ;
       model=10                                        ;;

  ece) set +x                                           ;
       echo " "; echo " ++ ECMWF ensemble member ${pert} chosen"     ;
       pert=` echo ${pert} | tr '[A-Z]' '[a-z]'`        ;
       PERT=` echo ${pert} | tr '[a-z]' '[A-Z]'`        ;
       echo " "                                         ;
       set -x                                           ;
       [ $CYL -eq 12 ] && PDY=${PDYm1}                  ;
       syy=`echo ${PDY} | cut -c3-4`                    ;
       smm=`echo ${PDY} | cut -c5-6`                    ;
       sdd=`echo ${PDY} | cut -c7-8`                    ;
       shh=${CYL}                                       ;
       symd=`echo ${PDY} | cut -c3-8`                   ;
       ecedir=/com/mrf/prod/wsr.${PDY}                  ;
       ecegfile=                                        ;
       eceifile=                                        ;
       COM=/com/mrf/${envir}/wsr.${PDY}                 ;
       fcstlen=240                                      ;
       fcsthrs=' 00 12 24 36 48 60 72 84 96 108 120 132 144   
                 156 168 180 192 204 216 228 240 99 99 99 99  
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99' ;
       atcfnum=91                                       ;
       pert_posneg=` echo "${pert}" | cut -c1-1`        ;
       pert_num=`    echo "${pert}" | cut -c2-3`        ;
       atcfname="e${pert_posneg}${pert_num}"            ;
       atcfout="e${pert_posneg}${pert_num}"             ;
       modtyp='global'                                  ;
       SENDTRACKER=NO                                   ;
       SENDDBN=NO                                       ;
       model=11                                        ;;

  sref) set +x                                          ;
       echo " "; echo " ++ operational sref tracking has been chosen ";
       echo " This script is not set up to run for SREF yet."
       echo " This script code is just a placeholder for future use.";
       echo " "                                         ;
       set -x                                           ;
       model=13                                         ;
       exit 99                                         ;;

  ensm) set +x                                          ;
       echo " "; echo " ++ operational ensemble tracking run off of "; 
       echo " mean fields has been chosen...."          ;
       echo " "                                         ;
       set -x                                           ;
       ensmdir=/com/mrf/prod/ens.${PDY}                 ;
       ensmgfile=ensstat.t${CYL}z.                      ;
       ensmifile=ensstat.t${CYL}z.                      ;
       COM=/com/mrf/${envir}/ens.${PDY}                 ;
       fcstlen=180                                      ;
       fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78 
                 84 90 96 102 108 114 120 126 132 138 144  
                 150 156 162 168 174 180 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99' ;
       atcfnum=91                                       ;
       atcfname="ammn"                                  ;
       atcfout="ammn"                                   ;
       modtyp='global'                                  ;
       SENDTRACKER=NO                                   ;
       SENDDBN=NO                                       ;
       model=14                                        ;;

  cmc) set +x                                           ;
       echo " "; echo " ++ operational Canadian global model chosen"   ;
       echo " "                                         ;
       set -x                                           ;
       cmcdir=/dcom/us007003/${PDY}/wgrbbul/cmc         ;
       cmcgfile=cmc_${PDY}${CYL}f                       ;
       cmcifile=nonexistant                             ;
       COM=/com/gens/prod/cmce.${PDY}/${CYL}/track      ;
       [[ ! -d $COM ]] && mkdir -p $COM                 ;
       fcstlen=144                                      ;
       fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78 
                 84 90 96 102 108 114 120 126 132 138 144  
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99' ;
       atcfnum=39                                       ;
       atcfname="cmc "                                  ;
       atcfout="cmc"                                    ;
       modtyp='global'                                  ;
       SENDTRACKER=NO                                   ;
       SENDDBN=NO                                       ;
       model=15                                        ;;

  cens) set +x                                            ;
       echo " "; echo " ++ Canadian ensemble member ${pert} chosen";
       pert=` echo ${pert} | tr '[A-Z]' '[a-z]'`        ;
       PERT=` echo ${pert} | tr '[a-z]' '[A-Z]'`        ;
       echo " "                                         ;
       set -x                                           ;
       ccedir=/com/gens/${envir}/cmce.${PDY}/${CYL}/pgrba  ;
       ccegfile=cmc_ge${pert}.t${CYL}z.pgrbaf           ;
       cceifile=does_not_exist                          ;
       COM=/com/gens/prod/cmce.${PDY}/${CYL}/track      ;
       [[ ! -d $COM ]] && mkdir -p $COM                 ;
       fcstlen=240                                      ;
       fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78
                 84 90 96 102 108 114 120 126 132 138 144
                 150 156 162 168 174 180 186 192 198 204 210
                 216 222 228 234 240  99  99  99  99  99  99
                  99  99  99  99  99  99  99  99  99  99  99
                  99  99  99  99  99  99  99';
       atcfnum=91                                       ;
       pert_posneg=` echo "${pert}" | cut -c1-1`        ;
       pert_num=`    echo "${pert}" | cut -c2-3`        ;
       atcfname="c${pert_posneg}${pert_num}"            ;
       atcfout="c${pert_posneg}${pert_num}"             ;
       modtyp='global'                                  ;
       SENDTRACKER=NO                                   ;
       SENDDBN=NO                                       ;
       model=16                                        ;;

  hwrf) set +x                                            ;
       echo " "; echo " ++ Hurricane WRF (HWRF) chosen"   ;
       echo " "                                           ;
       set -x                                             ;
       hwrfdir=/ptmp/wx20tm/wrfdat/${PDY}${CYL}           ;
       hwrfgfile=wrf.latlon.${PDY}${CYL}                  ;
       hwrfifile=wrf.latlon.${PDY}${CYL}.i                ;
       COM=/com/hwrf/${envir}/hwrf.${PDY}                 ;
       fcstlen=120                                        ;
       fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78  
                 84 90 96 102 108 114 120 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99  
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99  
                 99 99 99 99 99 99 99 99 99 99'           ;
       atcfnum=83                                         ;
       atcfname="hwrf"                                    ;
       atcfout="hwrf"                                     ;
       modtyp='regional'                                  ;
       model=17                                          ;;

  hens) set +x                                          ;
       echo " "; echo " ++ HWRF ensemble member ${pert} chosen" ;
       echo " This script is not set up to run for HWRF ensemble yet. ";
       echo " This script code is just a placeholder for future use."  ;
       pert=` echo ${pert} | tr '[A-Z]' '[a-z]'`        ;
       PERT=` echo ${pert} | tr '[a-z]' '[A-Z]'`        ;
       echo " "                                         ;
       set -x                                           ;
       hensdir=  --- not yet available ---              ;
       hensgfile=                                       ;
       hensifile=                                       ;
       COM=                                             ;
       fcstlen=120                                      ;
       fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78  
                 84 90 96 102 108 114 120 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99  
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99  
                 99 99 99 99 99 99 99 99 99 99'         ;
       atcfnum=91                                       ;
       pert_posneg=` echo "${pert}" | cut -c1-1`        ;
       pert_num=`    echo "${pert}" | cut -c2-3`        ;
       atcfname="h${pert_posneg}${pert_num}"            ;
       atcfout="h${pert_posneg}${pert_num}"             ;
       modtyp='regional'                                ;
       model=18                                         ;
       exit 99                                         ;;

  hdas) set +x                                          ;
       echo " "; echo " ++ operational HDAS chosen"     ;
       echo " "                                         ;
       set -x                                           ;
       hdasdir=/com/gfs/prod/gdas.${PDY}                ;
       hdasgfile=gdas1.t${CYL}z.pgrbf                   ;
       hdasifile=gdas1.t${CYL}z.pgrbif                  ;
       COM=/com/gfs/${envir}/gdas.${PDY}                ;
       fcstlen=9                                        ;
       fcsthrs=' 00 03 06 09 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99'            ;
       atcfnum=72                                       ;
       atcfname="hdas"                                  ;
       atcfout="hdas"                                   ;
       modtyp='regional'                                ;
       model=19                                        ;;

  gfs_enr) set +x                                       ;
       echo " "                                         ;
       echo " ++ operational off-cycle (relocation) ensemble member ${pert} chosen";
       pert=` echo ${pert} | tr '[A-Z]' '[a-z]'`        ;
       PERT=` echo ${pert} | tr '[a-z]' '[A-Z]'`        ;
       echo " "                                         ;
       set -x                                           ;
       ensrdir=/com/gens/prod/gefs.${PDY}/$cyc/pgrba                 ;
       ensrgfile=ge${pert}.t${CYL}z.pgrbaf              ;
       ensrifile=ge${pert}.t${CYL}z.pgrbaif             ;
       if [[ "$cyc" = "$cyc_fcst" ]]; then
         ensrgsuffix=                     ;
       else
         ensrgsuffix=.cycfs${cyc_fcst}                     ;
       fi
       COM=/com/gens/${envir}/gefs.${PDY}/$cyc/track                ;
       fcstlen=6                                        ;
       fcsthrs=' 00 06 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99 99 99 99 99 99 
                 99 99 99 99 99 99 99 99 99'            ;
       atcfnum=72                                       ;
       pert_posneg=` echo "${pert}" | cut -c1-1`        ;
       pert_num=`    echo "${pert}" | cut -c2-3`        ;
       if (( $cyc_fcst == 00 )); then
         atcfname="w${pert_posneg}${pert_num}"          ;
         atcfout="w${pert_posneg}${pert_num}"           ;
       elif (( $cyc_fcst == 06 )); then
         atcfname="x${pert_posneg}${pert_num}"          ;
         atcfout="x${pert_posneg}${pert_num}"           ;
       elif (( $cyc_fcst == 12 )); then
         atcfname="y${pert_posneg}${pert_num}"          ;
         atcfout="y${pert_posneg}${pert_num}"           ;
       elif (( $cyc_fcst == 18 )); then
         atcfname="z${pert_posneg}${pert_num}"          ;
         atcfout="z${pert_posneg}${pert_num}"           ;
       fi
       modtyp='global'                                  ;
       SENDTRACKER=NO                                   ;
       SENDDBN=NO                                       ;
       model=20                                        ;;

  *) set +x; echo " "; echo " !!! Model selected is not recognized."             ;
     echo " Model= ---> ${cmodel} <--- ..... Please submit the script again...."  ;
     echo " ";  set -x;
     err_exit " FAILED ${jobid} -- UNKNOWN cmodel IN TRACKER SCRIPT - ABNORMAL EXIT";;

esac

#---------------------------------------------------------------#
#
#      --------  TC Vitals processing   --------
#
# Check Steve Lord's operational tcvitals file to see if any 
# vitals records were processed for this time by his system.  
# If there were, then you'll find a file in /com/gfs/prod/gfs.yymmdd 
# with the vitals in it.  Also check the raw TC Vitals file in
# /com/arch/prod/syndat , since this may contain storms that Steve's 
# system ignored (Steve's system will ignore all storms that are 
# either over land or very close to land);  We still want to track 
# these inland storms, AS LONG AS THEY ARE NHC STORMS (don't 
# bother trying to track inland storms that are outside of NHC's 
# domain of responsibility -- we don't need that info).
# UPDATE 5/12/98 MARCHOK: The script is updated so that for the
#   global models, the gfs directory is checked for the error-
#   checked vitals file, while for the regional models, the 
#   nam directory is checked for that file.
#--------------------------------------------------------------#

# First check to see if the vitals file is in gfsvitdir or not.  If 
# it's not, then run Hua-Lu's ftp script to get the file from one
# of the other machines.  If it's still not there, then no big 
# deal; this script will exit just a little further down once it
# realizes there are not any storms to process.

d6ago_ymdh=` $NWPROD/util/exec/ndate -6 ${PDY}${CYL}`
d6ago_4ymd=` echo ${d6ago_ymdh} | cut -c1-8`
d6ago_ymd=` echo ${d6ago_ymdh} | cut -c3-8`
d6ago_hh=`  echo ${d6ago_ymdh} | cut -c9-10`
d6ago_str="${d6ago_ymd} ${d6ago_hh}00"

d6ahead_ymdh=` $NWPROD/util/exec/ndate 6 ${PDY}${CYL}`
d6ahead_4ymd=` echo ${d6ahead_ymdh} | cut -c1-8`
d6ahead_ymd=` echo ${d6ahead_ymdh} | cut -c3-8`
d6ahead_hh=`  echo ${d6ahead_ymdh} | cut -c9-10`
d6ahead_str="${d6ahead_ymd} ${d6ahead_hh}00"

if [ ${modtyp} = 'global' ]
then

  synvitdir=${synvitdir:-$COMDIR/com/gfs/prod/gfs.${PDY}}
  synvitfile=${synvitfile:-gfs.t${CYL}z.syndata.tcvitals.tm00}
  synvit6ago_dir=${synvit6ago_dir:-$COMDIR/com/gfs/prod/gfs.${d6ago_4ymd}}
  synvit6ago_file=${synvit6ago_file:-gfs.t${d6ago_hh}z.syndata.tcvitals.tm00}
  synvit6ahead_dir=${synvit6ahead_dir:-$COMDIR/com/gfs/prod/gfs.${d6ahead_4ymd}}
  synvit6ahead_file=${synvit6ahead_file:-gfs.t${d6ahead_hh}z.syndata.tcvitals.tm00}

else
      
  synvitdir=${synvitdir:-$COMDIR/com/nam/prod/nam.${PDY}}
  synvitfile=${synvitfile:-nam.t${CYL}z.syndata.tcvitals.tm00}
  synvit6ago_dir=${synvit6ago_dir:-$COMDIR/com/nam/prod/nam.${d6ago_4ymd}}
  synvit6ago_file=${synvit6ago_file:-nam.t${d6ago_hh}z.syndata.tcvitals.tm00}
  synvit6ahead_dir=${synvit6ahead_dir:-$COMDIR/com/nam/prod/nam.${d6ahead_4ymd}}
  synvit6ahead_file=${synvit6ahead_file:-nam.t${d6ahead_hh}z.syndata.tcvitals.tm00}

fi

set +x
echo " "
echo "              -----------------------------"
echo " "
echo " Now sorting and updating the TC Vitals file.  Please wait...."
echo " "
set -x

dnow_str="${symd} ${CYL}00"

if [ -s ${synvitdir}/${synvitfile} -o\
     -s ${synvit6ago_dir}/${synvit6ago_file} -o\
     -s ${synvit6ahead_dir}/${synvit6ahead_file} ]
then
  grep "${d6ago_str}" ${synvit6ago_dir}/${synvit6ago_file}        \
                  >${DATA}/tmpsynvit.${atcfout}.${PDY}${CYL}
  grep "${dnow_str}"  ${synvitdir}/${synvitfile}                  \
                 >>${DATA}/tmpsynvit.${atcfout}.${PDY}${CYL}
  grep "${d6ahead_str}" ${synvit6ahead_dir}/${synvit6ahead_file}  \
                 >>${DATA}/tmpsynvit.${atcfout}.${PDY}${CYL}
else
  set +x
  echo " "
  echo " There is no (synthetic) TC vitals file for ${CYL}z in ${synvitdir},"
  echo " nor is there a TC vitals file for ${d6ago_hh}z in ${synvit6ago_dir}."
  echo " nor is there a TC vitals file for ${d6ahead_hh}z in ${synvit6ahead_dir},"
  echo " Checking the raw TC Vitals file ....."
  echo " "
  set -x
fi

# Take the vitals from Steve Lord's /com/gfs/prod tcvitals file,
# and cat them with the NHC-only vitals from the raw, original
# /com/arch/prod/synda_tcvitals file.  Do this because the nwprod
# tcvitals file is the original tcvitals file, and Steve runs a
# program that ignores the vitals for a storm that's over land or
# even just too close to land, and for tracking purposes for the
# US regional models, we need these locations.  Only include these
# "inland" storm vitals for NHC (we're not going to track inland 
# storms that are outside of NHC's domain of responsibility -- we 
# don't need that info).  
# UPDATE 5/12/98 MARCHOK: nawk logic is added to screen NHC 
#   vitals such as "91L NAMELESS" or "89E NAMELESS", since TPC 
#   does not want tracks for such storms.

#grep "${d6ago_str}" ${archsyndir}/syndat_tcvitals.${CENT}${syy}   | \
#      grep -v TEST | nawk 'substr($0,6,1) !~ /[8]/ {print $0}' \
#      >${DATA}/tmprawvit.${atcfout}.${PDY}${CYL}
#grep "${dnow_str}"  ${archsyndir}/syndat_tcvitals.${CENT}${syy}   | \
#      grep -v TEST | nawk 'substr($0,6,1) !~ /[8]/ {print $0}' \
#      >>${DATA}/tmprawvit.${atcfout}.${PDY}${CYL}
#grep "${d6ahead_str}" ${archsyndir}/syndat_tcvitals.${CENT}${syy} | \
#      grep -v TEST | nawk 'substr($0,6,1) !~ /[8]/ {print $0}' \
#      >>${DATA}/tmprawvit.${atcfout}.${PDY}${CYL}

grep "${d6ago_str}" ${archsyndir}/syndat_tcvitals.${CENT}${syy}   | \
      grep -v TEST | gawk 'substr($0,6,1) !~ /[8]/ {print $0}' \
      >${DATA}/tmprawvit.${atcfout}.${PDY}${CYL}
grep "${dnow_str}"  ${archsyndir}/syndat_tcvitals.${CENT}${syy}   | \
      grep -v TEST | gawk 'substr($0,6,1) !~ /[8]/ {print $0}' \
      >>${DATA}/tmprawvit.${atcfout}.${PDY}${CYL}
grep "${d6ahead_str}" ${archsyndir}/syndat_tcvitals.${CENT}${syy} | \
      grep -v TEST | gawk 'substr($0,6,1) !~ /[8]/ {print $0}' \
      >>${DATA}/tmprawvit.${atcfout}.${PDY}${CYL}

#PRODTEST#
#PRODTEST# Use the next couple lines to test the tracker on the SP.
#PRODTEST# These next couple lines use data from a test TC Vitals file that 
#PRODTEST# I generate.  When you are ready to test this system, call me and
#PRODTEST# I'll create one for the current day, and then uncomment the next
#PRODTEST# couple lines in order to access the test vitals file.
#
#ttrkdir=/nfsuser/g01/wx20tm/trak/prod/data
#ttrkdir=/nfsuser/g01/wx20tm/trak/prod/scripts
#grep "${dnow_str}" ${ttrkdir}/tcvit.test >>${DATA}/tmprawvit.${atcfout}.${PDY}${CYL}


# IMPORTANT:  When "cat-ing" these files, make sure that the vitals
# files from the "raw" TC vitals files are first in order and Steve's
# TC vitals files second.  This is because Steve's vitals file has
# been error-checked, so if we have a duplicate tc vitals record in
# these 2 files (very likely), program supvit.x below will
# only take the last vitals record listed for a particular storm in
# the vitals file (all previous duplicates are ignored, and Steve's
# error-checked vitals records are kept).

cat ${DATA}/tmprawvit.${atcfout}.${PDY}${CYL} ${DATA}/tmpsynvit.${atcfout}.${PDY}${CYL} \
        >${DATA}/vitals.${atcfout}.${PDY}${CYL}

# If we are doing the processing for the GFDL model, then we want
# to further cut down on which vitals we allow into this run of the
# tracker.  The reason is that this program will be called from 
# each individual run for a storm, so the grib files will be 
# specific to each storm.  So if 4 storms are being run at a 
# particular cycle, then this script is run 4 separate times from
# within the GFDL_POST job.

if [ ${cmodel} = 'gfdl' ]; then
  grep -i ${stormid} ${COMIN}/${ATCFNAME}.vitals.${syy}${smm}${sdd}${shh} >${DATA}/tmpvit
  mv ${DATA}/tmpvit ${DATA}/vitals.${atcfout}.${PDY}${CYL}
fi

#--------------------------------------------------------------#
# Now run a fortran program that will read all the TC vitals
# records for the current dtg and the dtg from 6h ago, and
# sort out any duplicates.  If the program finds a storm that
# was included in the vitals file 6h ago but not for the current
# dtg, this program updates the 6h-old first guess position
# and puts these updated records as well as the records from
# the current dtg into a temporary vitals file.  It is this
# temporary vitals file that is then used as the input for the
# tracking program.
#--------------------------------------------------------------#

ymdh6ago=` $NWPROD/util/exec/ndate -6 ${PDY}${CYL}`
syy6=`echo ${ymdh6ago} | cut -c3-4`
smm6=`echo ${ymdh6ago} | cut -c5-6`
sdd6=`echo ${ymdh6ago} | cut -c7-8`
shh6=`echo ${ymdh6ago} | cut -c9-10`
symd6=${syy6}${smm6}${sdd6}

ymdh6ahead=` $NWPROD/util/exec/ndate 6 ${PDY}${CYL}`
syyp6=`echo ${ymdh6ahead} | cut -c3-4`
smmp6=`echo ${ymdh6ahead} | cut -c5-6`
sddp6=`echo ${ymdh6ahead} | cut -c7-8`
shhp6=`echo ${ymdh6ahead} | cut -c9-10`
symdp6=${syyp6}${smmp6}${sddp6}

echo "&datenowin   dnow%yy=${syy}, dnow%mm=${smm},"       >${DATA}/suv_input.${atcfout}.${PDY}${CYL}
echo "             dnow%dd=${sdd}, dnow%hh=${CYL}/"      >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}
echo "&date6agoin  d6ago%yy=${syy6}, d6ago%mm=${smm6},"  >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}
echo "             d6ago%dd=${sdd6}, d6ago%hh=${shh6}/"  >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}
echo "&date6aheadin  d6ahead%yy=${syyp6}, d6ahead%mm=${smmp6},"  >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}
echo "               d6ahead%dd=${sddp6}, d6ahead%hh=${shhp6}/"  >>${DATA}/suv_input.${atcfout}.${PDY}${CYL}

numvitrecs=`cat ${DATA}/vitals.${atcfout}.${PDY}${CYL} | wc -l`
if [ ${numvitrecs} -eq 0 ]
then
  set +x
  echo " "
  echo "!!! NOTE -- There are no vitals records for this time period."
  echo "!!! File ${DATA}/vitals.${atcfout}.${PDY}${CYL} is empty."
  echo "!!! It could just be that there are no storms for the current"
  echo "!!! time.  Please check the dates and submit this job again...."
  echo " "
  set -x
  exit 1
fi

# - - - - - - - - - - - - -
# Before running the program to read, sort and update the vitals,
# first run the vitals through some awk logic, the purpose of 
# which is to convert all the 2-digit years into 4-digit years.
# Beginning 4/21/99, NHC and JTWC will begin sending the vitals
# with 4-digit years, however it is unknown when other global
# forecasting centers will begin using 4-digit years, thus we
# need the following logic to ensure that all the vitals going
# into supvit.f have uniform, 4-digit years in their records.
#
# 1/8/2000: sed code added by Tim Marchok due to the fact that 
#       some of the vitals were getting past the syndata/qctropcy
#       error-checking with a colon in them; the colon appeared
#       in the character immediately to the left of the date, which
#       was messing up the "(length($4) == 8)" statement logic.
# - - - - - - - - - - - - -

sed -e "s/\:/ /g"  ${DATA}/vitals.${atcfout}.${PDY}${CYL} > ${DATA}/tempvit
mv ${DATA}/tempvit ${DATA}/vitals.${atcfout}.${PDY}${CYL}

awk '
{
  yycheck = substr($0,20,2)
  if ((yycheck == 20 || yycheck == 19) && (length($4) == 8)) {
    printf ("%s\n",$0)
  }
  else {
    if (yycheck >= 0 && yycheck <= 50) {
      printf ("%s20%s\n",substr($0,1,19),substr($0,20))
    }
    else {
      printf ("%s19%s\n",substr($0,1,19),substr($0,20))
    }
  }
} ' ${DATA}/vitals.${atcfout}.${PDY}${CYL} >${DATA}/vitals.${atcfout}.${PDY}${CYL}.y4

mv ${DATA}/vitals.${atcfout}.${PDY}${CYL}.y4 ${DATA}/vitals.${atcfout}.${PDY}${CYL}

export pgm=supvit
. ./prep_step

#export XLFUNIT_31=${DATA}/vitals.${atcfout}.${PDY}${CYL}       
#export XLFUNIT_51=${DATA}/vitals.upd.${atcfout}.${PDY}${CYL}  
ln -fs ${DATA}/vitals.${atcfout}.${PDY}${CYL} fort.31

msg="$pgm start for $atcfout at ${CYL}z"
postmsg "$jlogfile" "$msg"

${exectrkdir}/supvit <${DATA}/suv_input.${atcfout}.${PDY}${CYL}
suvrcc=$?

if [ ${suvrcc} -eq 0 ]
then
  mv fort.51 ${DATA}/vitals.upd.${atcfout}.${PDY}${CYL}
  msg="$pgm end for $atcfout at ${CYL}z completed normally"
  postmsg "$jlogfile" "$msg"
else
  set +x
  echo " "
  echo "!!! ERROR -- An error occurred while running supvit.x, "
  echo "!!! which is the program that updates the TC Vitals file."
  echo "!!! Return code from supvit.x = ${suvrcc}"
  echo "!!! model= ${atcfout}, forecast initial time = ${PDY}${CYL}"
  echo "!!! Exiting...."
  echo " "
  set -x
  err_exit " FAILED ${jobid} - ERROR RUNNING SUPVIT IN TRACKER SCRIPT- ABNORMAL EXIT"
fi

#------------------------------------------------------------------#
# Now select all storms to be processed, that is, process every
# storm that's listed in the updated vitals file for the current
# forecast hour.  If there are no storms for the current time,
# then exit.
#------------------------------------------------------------------#

numvitrecs=`cat ${DATA}/vitals.upd.${atcfout}.${PDY}${CYL} | wc -l`
if [ ${numvitrecs} -eq 0 ]
then
  set +x
  echo " "
  echo "!!! NOTE -- There are no vitals records for this time period "
  echo "!!! in the UPDATED vitals file."
  echo "!!! It could just be that there are no storms for the current"
  echo "!!! time.  Please check the dates and submit this job again...."
  echo " "
  set -x
  exit 1
fi

set +x
echo " "
echo " *--------------------------------*"
echo " |        STORM SELECTION         |"
echo " *--------------------------------*"
echo " "
set -x

ict=1
while [ $ict -le 15 ]
do
  stormflag[${ict}]=3
  let ict=ict+1
done

dtg_current="${symd} ${CYL}00"
stormmax=` grep "${dtg_current}" ${DATA}/vitals.upd.${atcfout}.${PDY}${CYL} | wc -l`

if [ ${stormmax} -gt 15 ]
then
  stormmax=15
fi

sct=1
while [ ${sct} -le ${stormmax} ]
do
  stormflag[${sct}]=1
  let sct=sct+1
done


#-----------------------------------------------------------------#
#
#         ------  CUT APART INPUT GRIB FILES  -------
#
# For the selected model, cut apart the GRIB input files in order
# to pull out only the variables that we need for the tracker.  
# Put these selected variables from all forecast hours into 1 big 
# GRIB file that we'll use as input for the tracker.
# 
# The wgrib utility (/nwprod/util/exec/wgrib) is used to cut out 
# the needed parms for the GFS, MRF, GDAS, UKMET and NOGAPS files.
# The utility /nwprod/util/exec/copygb is used to interpolate the 
# NGM (polar stereographic) and NAM (Lambert Conformal) data from 
# their grids onto lat/lon grids.  Note that while the lat/lon 
# grid that I specify overlaps into areas that don't have any data 
# on the original grid, Mark Iredell wrote the copygb software so 
# that it will mask such "no-data" points with a bitmap (just be 
# sure to check the lbms in your fortran program after getgb).
#-----------------------------------------------------------------#

set +x
echo " "
echo " -----------------------------------------"
echo "   NOW CUTTING APART INPUT GRIB FILES TO "
echo "   CREATE 1 BIG GRIB INPUT FILE "
echo " -----------------------------------------"
echo " "
set -x

gix=$NWPROD/util/exec/grbindex
cgb=$NWPROD/util/exec/copygb

regflag=`grep NHC ${DATA}/vitals.upd.${atcfout}.${PDY}${CYL} | wc -l`

# ----------------------------
#   Process NGM, if selected
# ----------------------------
  
if [ ${model} -eq 5 ]
then

  grid='255 0 151 71 70000 190000 128 0000 340000 1000 1000 0'

  if [ ${regflag} -eq 0 ]
  then
    set +x
    echo " "
    echo " !!! NGM model has been selected, but there are no storms in the"
    echo " !!! TC Vitals file that can be processed.  That is, there are no"
    echo " !!! Vitals records from NHC.  The vitals records that are in the"
    echo " !!! updated vitals file must be from another cyclone forecast "
    echo " !!! center, and the NGM domain does not extend to any "
    echo " !!! region other than that covered by NHC.  Exiting....."
    set -x
    exit 1
  fi

  if [ -s ${DATA}/ngmlatlon.pgrb.${PDY}${CYL} ]
  then
    rm ${DATA}/ngmlatlon.pgrb.${PDY}${CYL}
  fi

  for fhour in ${fcsthrs}
  do
 
    if [ ${fhour} -eq 99 ]
    then
      continue
    fi

    if [ ! -s ${ngmdir}/${ngmgfile}${fhour} ]
    then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! NGM File missing: ${ngmdir}/${ngmgfile}${fhour}"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      set -x
      continue
    fi
    if [ -s ${ngmdir}/${ngmifile}${fhour} ]
    then
      x1=${ngmdir}/${ngmifile}${fhour}
    else
      if [ -s ${DATA}/tmpngmixfile ]; then rm ${DATA}/tmpngmixfile; fi
      $gix ${ngmdir}/${ngmgfile}${fhour} ${DATA}/tmpngmixfile
      x1=${DATA}/tmpngmixfile
    fi

    set +x
    echo " "
    echo " Extracting NGM GRIB data for forecast hour = $fhour"
    echo " "
    set -x

    g1=${ngmdir}/${ngmgfile}${fhour}       
   
    $cgb -g"$grid" -k'2*-1 104 -1 33 100 850' $g1 $x1 ${DATA}/ngmllu850.grb.f${fhour};  rcc1=$?
    $cgb -g"$grid" -k'2*-1 104 -1 33 100 700' $g1 $x1 ${DATA}/ngmllu700.grb.f${fhour};  rcc2=$?
    $cgb -g"$grid" -k'2*-1 104 -1 33 100 500' $g1 $x1 ${DATA}/ngmllu500.grb.f${fhour};  rcc3=$?
    $cgb -g"$grid" -k'2*-1 104 -1 33 105 10'  $g1 $x1 ${DATA}/ngmllu10m.grb.f${fhour};  rcc4=$?
    $cgb -g"$grid" -k'2*-1 104 -1 41 100 850' $g1 $x1 ${DATA}/ngmllav850.grb.f${fhour}; rcc5=$?
    $cgb -g"$grid" -k'2*-1 104 -1 41 100 700' $g1 $x1 ${DATA}/ngmllav700.grb.f${fhour}; rcc6=$?
    $cgb -g"$grid" -k'2*-1 104 -1  7 100 850' $g1 $x1 ${DATA}/ngmllz850.grb.f${fhour};  rcc7=$?
    $cgb -g"$grid" -k'2*-1 104 -1  7 100 700' $g1 $x1 ${DATA}/ngmllz700.grb.f${fhour};  rcc8=$?
    $cgb -g"$grid" -k'2*-1 104 -1  2 102 0'   $g1 $x1 ${DATA}/ngmllmslp.grb.f${fhour};  rcc9=$?

    if [ $rcc1 -eq 134 -o $rcc2 -eq 134 -o $rcc3 -eq 134 -o $rcc4 -eq 134 -o $rcc5 -eq 134 -o \
         $rcc6 -eq 134 -o $rcc7 -eq 134 -o $rcc8 -eq 134 -o $rcc9 -eq 134 ]
    then
      set +x
      echo " "
      echo "!!! ERROR using $cgb to interpolate ngm data.  We will stop execution because"
      echo "!!! some variables may have been copied okay, while some obviously have not, "
      echo "!!! and that could lead to unreliable results from the tracker.  Check to make"
      echo "!!! sure you've allocated enough memory for this job.  Exiting....."
      echo " "
      set -x
      err_exit " FAILED ${jobid} - ERROR INTERPOLATING NGM DATA IN TRACKER SCRIPT - ABNORMAL EXIT"
    fi

    cat ${DATA}/ngmllu850.grb.f${fhour} ${DATA}/ngmllu700.grb.f${fhour} \
        ${DATA}/ngmllu500.grb.f${fhour} ${DATA}/ngmllz850.grb.f${fhour} \
        ${DATA}/ngmllz700.grb.f${fhour} ${DATA}/ngmllmslp.grb.f${fhour} \
        ${DATA}/ngmllav850.grb.f${fhour} ${DATA}/ngmllav700.grb.f${fhour} \
        ${DATA}/ngmllu10m.grb.f${fhour} \
        >>${DATA}/ngmlatlon.pgrb.${PDY}${CYL}

  done

  $gix ${DATA}/ngmlatlon.pgrb.${PDY}${CYL} ${DATA}/ngmlatlon.pgrb.ix.${PDY}${CYL}
  gribfile=${DATA}/ngmlatlon.pgrb.${PDY}${CYL}
  ixfile=${DATA}/ngmlatlon.pgrb.ix.${PDY}${CYL}

fi


# ----------------------------------
#   Process NAM, if selected
# ----------------------------------

if [ ${model} -eq 6 ]
then

  grid='255 0 301 141 70000 190000 128 0000 340000  500  500 0'

  if [ ${regflag} -eq 0 ]; then
    set +x
    echo " "
    echo " !!! NAM model has been selected, but there are no storms in the"
    echo " !!! TC Vitals file that can be processed.  That is, there are no"
    echo " !!! Vitals records from NHC.  The vitals records that are in the"
    echo " !!! updated vitals file must be from another cyclone forecast "
    echo " !!! center, and the NAM domain does not extend to any "
    echo " !!! region other than that covered by NHC.  Exiting....."
    set -x
    exit 1
  fi

  if [ -s ${DATA}/namlatlon.pgrb.${PDY}${CYL} ]; then 
    rm ${DATA}/namlatlon.pgrb.${PDY}${CYL}
  fi

  for fhour in ${fcsthrs}
  do

    if [ ${fhour} -eq 99 ]
    then
      continue
    fi

    if [ ! -s ${namdir}/${namgfile}${fhour}.tm00 ]
    then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! NAM File missing: ${namdir}/${namgfile}${fhour}.tm00"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      set -x
      continue
    fi
    if [ -s ${namdir}/${namifile}${fhour} ]
    then
      x1=${namdir}/${namifile}${fhour}
    else
      if [ -s ${DATA}/tmpnamixfile ]; then rm ${DATA}/tmpnamixfile; fi
      $gix ${namdir}/${namgfile}${fhour}.tm00 ${DATA}/tmpnamixfile
      x1=${DATA}/tmpnamixfile
    fi

    set +x
    echo " "
    echo " Extracting NAM GRIB data for forecast hour = $fhour"
    echo " "
    set -x
  
    g1=${namdir}/${namgfile}${fhour}.tm00
     
    $cgb -g"$grid" -k'4*-1 33 100 850' $g1 $x1 ${DATA}/namllu850.grb.f${fhour};   rcc1=$?
    $cgb -g"$grid" -k'4*-1 33 100 700' $g1 $x1 ${DATA}/namllu700.grb.f${fhour};   rcc2=$?
    $cgb -g"$grid" -k'4*-1 33 100 500' $g1 $x1 ${DATA}/namllu500.grb.f${fhour};   rcc3=$?
    $cgb -g"$grid" -k'4*-1 33 105 10'  $g1 $x1 ${DATA}/namllu10m.grb.f${fhour};   rcc4=$?
    $cgb -g"$grid" -k'4*-1 41 100 850' $g1 $x1 ${DATA}/namllav850.grb.f${fhour};  rcc5=$?
    $cgb -g"$grid" -k'4*-1 41 100 700' $g1 $x1 ${DATA}/namllav700.grb.f${fhour};  rcc6=$?
    $cgb -g"$grid" -k'4*-1  7 100 850' $g1 $x1 ${DATA}/namllz850.grb.f${fhour};   rcc7=$?
    $cgb -g"$grid" -k'4*-1  7 100 700' $g1 $x1 ${DATA}/namllz700.grb.f${fhour};   rcc8=$?
    $cgb -g"$grid" -k'4*-1  2 102   0' $g1 $x1 ${DATA}/namllmslp.grb.f${fhour};   rcc9=$?

    if [ $rcc1 -eq 134 -o $rcc2 -eq 134 -o $rcc3 -eq 134 -o $rcc4 -eq 134 -o $rcc5 -eq 134 -o \
         $rcc6 -eq 134 -o $rcc7 -eq 134 -o $rcc8 -eq 134 -o $rcc9 -eq 134 ]
    then
      set +x
      echo " "
      echo "!!! ERROR using $cgb to interpolate nam data.  We will stop execution because"
      echo "!!! some variables may have been copied okay, while some obviously have not, "
      echo "!!! and that could lead to unreliable results from the tracker.  Check to make"
      echo "!!! sure you've allocated enough memory for this job.  Exiting...."
      echo " "
      set -x
      err_exit " FAILED ${jobid} - ERROR INTERPOLATING NAM DATA IN TRACKER SCRIPT - ABNORMAL EXIT"
    fi

    cat ${DATA}/namllu850.grb.f${fhour} ${DATA}/namllu700.grb.f${fhour} \
        ${DATA}/namllu500.grb.f${fhour} ${DATA}/namllz850.grb.f${fhour} \
        ${DATA}/namllz700.grb.f${fhour} ${DATA}/namllmslp.grb.f${fhour} \
        ${DATA}/namllav850.grb.f${fhour} ${DATA}/namllav700.grb.f${fhour} \
        ${DATA}/namllu10m.grb.f${fhour} \
        >>${DATA}/namlatlon.pgrb.${PDY}${CYL}
  
  done

  $gix ${DATA}/namlatlon.pgrb.${PDY}${CYL} ${DATA}/namlatlon.pgrb.ix.${PDY}${CYL}
  gribfile=${DATA}/namlatlon.pgrb.${PDY}${CYL}
  ixfile=${DATA}/namlatlon.pgrb.ix.${PDY}${CYL}

fi

# ----------------------------------
#   Process GFDL, if selected
# ----------------------------------

# The GFDL GRIB grid is already a lat/lon grid, however it uses a scanning 
# mode flag of 64, which means the data starts in the south and goes north.
# The tracker needs the data to start in the north and go south, so we will
# use copygb to flip the data in the grid.  The other thing that copygb 
# will do here is make the new file have wider boundaries around all 4 
# sides of the grid.  This is done because the GFDL grid is a regional grid,
# and we need to have that buffer of null, bitmapped data around a 
# regional grid.  The north-south extent of the domain is always the same,
# so we will hardwire the new latitude bounds in the "grid=" statement
# (adding an extra ~5.0 degrees both north and south), but we need to wgrib
# the 00h file to get the longitude bounds, and then modify them.  The 
# northern and southern boundaries are fixed in that "grid=" statement 
# because those boundaries do NOT change from run to run; the integration
# grid only changes in the east/west direction.

if [ ${model} -eq 9 ]
then

  origwestlon=`$wgrib -V ${gfdldir}/${gfdlgfile}00 | grep long | head -1 | awk '{printf ("%d",$2*1000)}'`
  origeastlon=`$wgrib -V ${gfdldir}/${gfdlgfile}00 | grep long | head -1 | awk '{printf ("%d",$4*1000)}'`
  let newwestlon=origwestlon-5000
  let neweastlon=origeastlon+5000

#  grid="255 0 171 171 70000 ${newwestlon} 128 -15000 ${neweastlon} 500  500 0"
#  grid="255 0 255 255 69833 ${newwestlon} 128 -14833 ${neweastlon} 333  333 0"
  grid="255 0 511 511 69917 ${newwestlon} 128 -14917 ${neweastlon} 167  167 0"


  if [ ${regflag} -eq 0 ]; then
    set +x
    echo " "
    echo " !!! GFDL model has been selected, but there are no storms in the"
    echo " !!! TC Vitals file that can be processed.  That is, there are no"
    echo " !!! Vitals records from NHC.  The vitals records that are in the"
    echo " !!! updated vitals file must be from another cyclone forecast "
    echo " !!! center, and the GFDL domain does not extend to any "
    echo " !!! region other than that covered by NHC.  Exiting....."
    set -x
    exit 0
  fi

  if [ -s ${DATA}/gfdl.${stormenv}.pgrb.${PDY}${CYL} ]; then
    rm ${DATA}/gfdl.${stormenv}.pgrb.${PDY}${CYL}
  fi

  for fhour in ${fcsthrs}
  do

    if [ ${fhour} -eq 99 ]
    then
      continue
    fi

    if [ ! -s ${gfdldir}/${gfdlgfile}${fhour} ]
    then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! GFDL File missing: ${gfdldir}/${gfdlgfile}${fhour}" 
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" 
      set -x 
      continue
    fi
    if [ -s ${gfdldir}/${gfdlifile}${fhour} ]
    then
      x1=${gfdldir}/${gfdlifile}${fhour}
    else
      if [ -s ${DATA}/tmpgfdlixfile ]; then rm ${DATA}/tmpgfdlixfile; fi
      $gix ${gfdldir}/${gfdlgfile}${fhour} ${DATA}/tmpgfdlixfile
      x1=${DATA}/tmpgfdlixfile
    fi

    set +x
    echo " "
    echo " Extracting GFDL GRIB data for forecast hour = $fhour"
    echo " "
    set -x

    g1=${gfdldir}/${gfdlgfile}${fhour}

    $cgb -g"$grid" -k'4*-1 33 100 850' $g1 $x1 ${DATA}/gfdlu850.grb.f${fhour};   rcc1=$?
    $cgb -g"$grid" -k'4*-1 33 100 700' $g1 $x1 ${DATA}/gfdlu700.grb.f${fhour};   rcc2=$?
    $cgb -g"$grid" -k'4*-1 33 100 500' $g1 $x1 ${DATA}/gfdlu500.grb.f${fhour};   rcc3=$?
    $cgb -g"$grid" -k'4*-1 33 105  35' $g1 $x1 ${DATA}/gfdlu35m.grb.f${fhour};   rcc4=$?
    $cgb -g"$grid" -k'4*-1  7 100 850' $g1 $x1 ${DATA}/gfdlz850.grb.f${fhour};   rcc7=$?
    $cgb -g"$grid" -k'4*-1  7 100 700' $g1 $x1 ${DATA}/gfdlz700.grb.f${fhour};   rcc8=$?
    $cgb -g"$grid" -k'4*-1  2 102   0' $g1 $x1 ${DATA}/gfdlmslp.grb.f${fhour};   rcc9=$?

    if [ $rcc1 -eq 134 -o $rcc2 -eq 134 -o $rcc3 -eq 134 -o $rcc4 -eq 134 -o \
         $rcc7 -eq 134 -o $rcc8 -eq 134 -o $rcc9 -eq 134 ]
    then
      set +x
      echo " "
      echo "!!! ERROR using $cgb to interpolate gfdl data.  We will stop execution because"
      echo "!!! some variables may have been copied okay, while some obviously have not, "
      echo "!!! and that could lead to unreliable results from the tracker.  Check to make"
      echo "!!! sure you've allocated enough memory for this job.  Exiting...."
      echo " "
      set -x
      err_exit " FAILED ${jobid} - ERROR INTERPOLATING GFDL DATA IN TRACKER SCRIPT - ABNORMAL EXIT"
    fi

    cat ${DATA}/gfdlu850.grb.f${fhour} ${DATA}/gfdlu700.grb.f${fhour} \
        ${DATA}/gfdlu500.grb.f${fhour} ${DATA}/gfdlz850.grb.f${fhour} \
        ${DATA}/gfdlz700.grb.f${fhour} ${DATA}/gfdlmslp.grb.f${fhour} \
        ${DATA}/gfdlu35m.grb.f${fhour} \
        >>${DATA}/gfdl.${stormenv}.pgrb.${PDY}${CYL}
 
  done

  $gix ${DATA}/gfdl.${stormenv}.pgrb.${PDY}${CYL} ${DATA}/gfdl.${stormenv}.pgrb.ix.${PDY}${CYL}
  gribfile=${DATA}/gfdl.${stormenv}.pgrb.${PDY}${CYL}
  ixfile=${DATA}/gfdl.${stormenv}.pgrb.ix.${PDY}${CYL}

fi


# ------------------------------
#   Process ECMWF, if selected
# ------------------------------

# As of Summer, 2005, ECMWF is now sending us high res (1-degree) data on
# a global grid with 12-hourly resolution out to 240h.  Previously, we 
# only got their data on a low res (2.5-degree) grid, from 35N-35S, with
# 24-hourly resolution out to only 168h.

if [ ${model} -eq 4 ]
then

  if [ -s ${DATA}/ecgribfile.${PDY}${CYL} ]
  then
    rm ${DATA}/ecgribfile.${PDY}${CYL}
  fi

  immddhh=`echo ${PDY}${CYL}| cut -c5-`
  ict=0

  for fhour in ${fcsthrs}
  do
    
    if [ ${fhour} -eq 99 ]
    then
      continue
    fi

    let fhr=ict*12
    echo "fhr= $fhr"
    fmmddhh=` $NWPROD/util/exec/ndate ${fhr} ${PDY}${CYL} | cut -c5- `
    ec_hires_orig=ecens_DCD${immddhh}00${fmmddhh}001
      
    if [ ! -s ${ecmwfdir}/${ec_hires_orig} ]
    then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! ECMWF File missing: ${ecmwfdir}/${ec_hires_orig}"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " "
      set -x
      continue
    fi
      
    ecfile=${ecmwfdir}/${ec_hires_orig}
    $wgrib -s $ecfile >ec.ix

    for parm in ${wgrib_ec_hires_parmlist}
    do
      grep "${parm}" ec.ix | $wgrib -s $ecfile -i -grib -append \
                              -o ${DATA}/ecgribfile.${PDY}${CYL}
    done

    let ict=ict+1

  done

  $gix ${DATA}/ecgribfile.${PDY}${CYL} ${DATA}/ecixfile.${PDY}${CYL}
  gribfile=${DATA}/ecgribfile.${PDY}${CYL}
  ixfile=${DATA}/ecixfile.${PDY}${CYL}

fi


# ------------------------------
#   Process GFS, if selected
# ------------------------------
  
if [ ${model} -eq 1 ]
then

  if [ -s ${DATA}/gfsgribfile.${PDY}${CYL} ]
  then 
    rm ${DATA}/gfsgribfile.${PDY}${CYL}
  fi

  for fhour in ${fcsthrs}
  do

    if [ ${fhour} -eq 99 ]
    then
      continue
    fi

    if [ ! -s ${gfsdir}/${gfsgfile}${fhour} ]
    then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! GFS File missing: ${gfsdir}/${gfsgfile}${fhour}"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " "
      set -x
      continue
    fi

    gfile=${gfsdir}/${gfsgfile}${fhour}
    $wgrib -s $gfile >gfs.ix

    for parm in ${wgrib_parmlist} 
    do
      case ${parm} in
        "SurfaceU")     
          grep "UGRD:10 m " gfs.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${DATA}/master.gfsgribfile.${PDY}${CYL} ;;
        "SurfaceV")
          grep "VGRD:10 m " gfs.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${DATA}/master.gfsgribfile.${PDY}${CYL} ;;
                 *) 
          grep "${parm}" gfs.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${DATA}/master.gfsgribfile.${PDY}${CYL} ;;
      esac 

    done

  done
  $cgb -g4 -i2 -x ${DATA}/master.gfsgribfile.${PDY}${CYL} ${DATA}/gfsgribfile.${PDY}${CYL}  
  $gix ${DATA}/gfsgribfile.${PDY}${CYL} ${DATA}/gfsixfile.${PDY}${CYL}
  gribfile=${DATA}/gfsgribfile.${PDY}${CYL}
  ixfile=${DATA}/gfsixfile.${PDY}${CYL}

fi


# ------------------------------
#   Process MRF, if selected
# ------------------------------
  
if [ ${model} -eq 2 ]
then

  if [ -s ${DATA}/mrfgribfile.${PDY}${CYL} ]
  then 
    rm ${DATA}/mrfgribfile.${PDY}${CYL}
  fi

  for fhour in ${fcsthrs}
  do

    if [ ${fhour} -eq 99 ]
    then
      continue
    fi

    if [ ! -s ${mrfdir}/${mrfgfile}${fhour} ]
    then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! MRF File missing: ${mrfdir}/${mrfgfile}${fhour}"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " "
      set -x
      continue
    fi

    gfile=${mrfdir}/${mrfgfile}${fhour}
    $wgrib -s $gfile >mrf.ix

    for parm in ${wgrib_parmlist}
    do
      case ${parm} in
        "SurfaceU")
          grep "UGRD:10 m " mrf.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${DATA}/mrfgribfile.${PDY}${CYL} ;;
        "SurfaceV")
          grep "VGRD:10 m " mrf.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${DATA}/mrfgribfile.${PDY}${CYL} ;;
                 *)
          grep "${parm}" mrf.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${DATA}/mrfgribfile.${PDY}${CYL} ;;
      esac
    done

  done

  $gix ${DATA}/mrfgribfile.${PDY}${CYL} ${DATA}/mrfixfile.${PDY}${CYL}
  gribfile=${DATA}/mrfgribfile.${PDY}${CYL}
  ixfile=${DATA}/mrfixfile.${PDY}${CYL}

fi

# ------------------------------
#   Process GFS parallel
# ------------------------------

if [ ${model} -eq 99 ]
then

  if [ -s ${DATA}/paragribfile.${PDY}${CYL} ]
  then
    rm ${DATA}/paragribfile.${PDY}${CYL}
  fi

  for fhour in ${fcsthrs}
  do

    if [ ${fhour} -eq 99 ]
    then
      continue
    fi

    psuffix=${psuffix:-""}
    if [ ! -s ${paradir}/pgb${flag_pgb}${fhour}.$cdump.${PDY}${CYL}$psuffix ]
    then
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! PARA File missing: ${paradir}/pgb${flag_pgb}${fhour}.$cdump.${PDY}${CYL}"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " "
      set -x
      continue
    fi

    gfile=${paradir}/pgb${flag_pgb}${fhour}.$cdump.${PDY}${CYL}$psuffix
    $wgrib -s $gfile >para.ix

    for parm in ${wgrib_parmlist}
    do
      case ${parm} in
        "SurfaceU")
          grep "UGRD:10 m " para.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${DATA}/paragribfile.${PDY}${CYL} ;;
        "SurfaceV")
          grep "VGRD:10 m " para.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${DATA}/paragribfile.${PDY}${CYL} ;;
                 *)
          grep "${parm}" para.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${DATA}/paragribfile.${PDY}${CYL} ;;
      esac

    done

  done

# If using master grid file, follow production procedure and do copygb from master to grid4
  if [ ${flag_pgb} = 'm' ] ; then
    mv ${DATA}/paragribfile.${PDY}${CYL} ${DATA}/master.gfsgribfile.${PDY}${CYL}
    $cgb -g4 -i2 -x ${DATA}/master.gfsgribfile.${PDY}${CYL} ${DATA}/gfsgribfile.${PDY}${CYL}
    cp ${DATA}/gfsgribfile.${PDY}${CYL} ${DATA}/paragribfile.${PDY}${CYL}
  fi
##
  $gix ${DATA}/paragribfile.${PDY}${CYL} ${DATA}/paraixfile.${PDY}${CYL}
  gribfile=${DATA}/paragribfile.${PDY}${CYL}
  ixfile=${DATA}/paraixfile.${PDY}${CYL}

fi


# --------------------------------------------------
#   Process NCEP Ensemble perturbation, if selected
# --------------------------------------------------

if [ ${model} -eq 10 ]
then

  if [ -s ${DATA}/ens${pert}gribfile.${PDY}${CYL} ]
  then
    rm ${DATA}/ens${pert}gribfile.${PDY}${CYL}
  fi

  for fhour in ${fcsthrs}
  do

    if [ ${fhour} -eq 99 ]
    then
      continue
    fi

    if [ ! -s ${ensdir}/${ensgfile}${fhour} ]
    then           
      set +x       
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! ENSEMBLE ${PERT} File missing: ${ensdir}/${ensgfile}${fhour}"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " "     
      set -x       
      continue
    fi      

    gfile=${ensdir}/${ensgfile}${fhour}
    $wgrib -s $gfile >ens.ix
      
    for parm in ${wgrib_parmlist}
    do
      case ${parm} in
        "SurfaceU")
          grep "UGRD:10 m " ens.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${DATA}/ens${pert}gribfile.${PDY}${CYL} ;;
        "SurfaceV")
          grep "VGRD:10 m " ens.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${DATA}/ens${pert}gribfile.${PDY}${CYL} ;;
                 *)
          grep "${parm}" ens.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${DATA}/ens${pert}gribfile.${PDY}${CYL} ;;
      esac

    done

  done

  $gix ${DATA}/ens${pert}gribfile.${PDY}${CYL} ${DATA}/ens${pert}ixfile.${PDY}${CYL}
  gribfile=${DATA}/ens${pert}gribfile.${PDY}${CYL}
  ixfile=${DATA}/ens${pert}ixfile.${PDY}${CYL}

fi


# -------------------------------
#   Process Ensemble Mean track 
#   from Ensemble mean (ensstat) 
#   fields, if selected
# -------------------------------

if [ ${model} -eq 14 ]
then
    
  if [ -s ${DATA}/ensmgribfile.${PDY}${CYL} ]
  then
    rm ${DATA}/ensmgribfile.${PDY}${CYL}
  fi

  ensm_parmlist="z850 z700 u850 u500 v850 v500 u10m v10m prmsl"
    
  for parm in ${ensm_parmlist}
  do
    
    if [ ! -s ${ensmdir}/${ensmgfile}${parm}hr ]
    then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! Ensemble ensstat file missing for $parm: ${ensmdir}/${ensmgfile}${parm}hr"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " "
      set -x
      continue
    fi

    efile=${ensmdir}/${ensmgfile}${parm}hr
    $wgrib -PDS10 $efile | awk '$0 ~ / 0 0 0 0 1 5 0 1 / {print $0}' | \
          awk -FPDS10= '{print $1}' | \
          $wgrib -i $efile -grib -append -o ${DATA}/ensmgribfile.${PDY}${CYL}

  done

  $gix ${DATA}/ensmgribfile.${PDY}${CYL} ${DATA}/ensmixfile.${PDY}${CYL}
  gribfile=${DATA}/ensmgribfile.${PDY}${CYL}
  ixfile=${DATA}/ensmixfile.${PDY}${CYL}

fi


# -------------------------------------
#   Process GDAS, if selected
# -------------------------------------

if [ ${model} -eq 8 ]
then

  if [ -s ${DATA}/gdasgribfile.${PDY}${CYL} ]
  then
    rm ${DATA}/gdasgribfile.${PDY}${CYL}
  fi

  for fhour in ${fcsthrs}
  do

    if [ ${fhour} -eq 99 ]
    then
      continue
    fi

    if [ ! -s ${gdasdir}/${gdasgfile}${fhour} ]
    then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! GDAS File missing: ${gdasdir}/${gdasgfile}${fhour}"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      set -x
      continue
    fi

    gfile=${gdasdir}/${gdasgfile}${fhour}
    $wgrib -s $gfile >gdas.ix

    for parm in ${wgrib_parmlist}
    do
      case ${parm} in
        "SurfaceU")
          grep "UGRD:10 m " gdas.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${DATA}/gdasgribfile.${PDY}${CYL} ;;
        "SurfaceV")
          grep "VGRD:10 m " gdas.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${DATA}/gdasgribfile.${PDY}${CYL} ;;
                 *)
          grep "${parm}" gdas.ix | $wgrib -s $gfile -i -grib -append    \
                                  -o ${DATA}/gdasgribfile.${PDY}${CYL} ;;
      esac
    done

  done

  $gix ${DATA}/gdasgribfile.${PDY}${CYL} ${DATA}/gdasixfile.${PDY}${CYL}
  gribfile=${DATA}/gdasgribfile.${PDY}${CYL}
  ixfile=${DATA}/gdasixfile.${PDY}${CYL}

fi

# ------------------------------
#   Process UKMET, if selected
# ------------------------------
  
if [ ${model} -eq 3 ]
then

  if [ -s ${DATA}/ukmetgribfile.${PDY}${CYL} ]
  then 
    rm ${DATA}/ukmetgribfile.${PDY}${CYL}
  fi

  wgrib_parmlist=' HGT:850 HGT:700 UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 UGRD:sfc VGRD:sfc ABSV:850 ABSV:700 PRMSL:MSL '

  for fhour in ${fcsthrs}
  do

    if [ ${fhour} -eq 99 ]
    then
      continue
    fi

    if [ ! -s ${ukmetdir}/${ukmetgfile}${fhour} ]
    then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! UKMET File missing: ${ukmetdir}/${ukmetgfile}${fhour}"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " "
      set -x
      continue
    fi

    gfile=${ukmetdir}/${ukmetgfile}${fhour}
    $wgrib -s $gfile >ukmet.ix

    for parm in ${wgrib_parmlist}
    do
      grep "${parm}" ukmet.ix | $wgrib -s $gfile -i -grib -append \
                             -o ${DATA}/ukmetgribfile.${PDY}${CYL}
    done

  done

  $gix ${DATA}/ukmetgribfile.${PDY}${CYL} ${DATA}/ukmetixfile.${PDY}${CYL}
  gribfile=${DATA}/ukmetgribfile.${PDY}${CYL}
  ixfile=${DATA}/ukmetixfile.${PDY}${CYL}

fi


# ------------------------------
#   Process NOGAPS, if selected
# ------------------------------

if [ ${model} -eq 7 ]
then

  if [ -s ${DATA}/ngpsgribfile.${PDY}${CYL} ]
  then
    rm ${DATA}/ngpsgribfile.${PDY}${CYL}
  fi

  for fhour in ${fcsthrs}
  do

    if [ ${fhour} -eq 99 ]
    then
      continue
    fi

    vhour=${fhour}
    if [ ${fhour} -lt 100 ]; then
      vhour=0${fhour}
    fi

    if [ ! -s ${ngpsdir}/${ngpsgfile}${vhour} ]
    then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! NOGAPS File missing: ${ngpsdir}/${ngpsgfile}${vhour}"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " "
      echo " !!! Due to missing NOGAPS file, execution is ending...."
      echo " "
      set -x  
#      err_exit " FAILED ${jobid} - MISSING NOGAPS FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
      exit 1
    fi

    if [ $fhour = '00' ]
    then
      vtstring=":anl:"
    else
      vtstring="${fhour}hr"
    fi

    gfile=${ngpsdir}/${ngpsgfile}${vhour}
    $wgrib -s $gfile >ngps.ix

    for parm in ${wgrib_parmlist}
    do
      case ${parm} in
        "SurfaceU")
          grep "UGRD:10 m " ngps.ix | grep ${vtstring} | \
            $wgrib -s $gfile -i -grib -append -o ${DATA}/ngpsgribfile.${PDY}${CYL} ;;
        "SurfaceV")
          grep "VGRD:10 m " ngps.ix | grep ${vtstring} | \
            $wgrib -s $gfile -i -grib -append -o ${DATA}/ngpsgribfile.${PDY}${CYL} ;;
                 *)
          grep "${parm}" ngps.ix | grep ${vtstring} | \
            $wgrib -s $gfile -i -grib -append -o ${DATA}/ngpsgribfile.${PDY}${CYL} ;;
      esac
    done

  done

  $gix ${DATA}/ngpsgribfile.${PDY}${CYL} ${DATA}/ngpsixfile.${PDY}${CYL}
  gribfile=${DATA}/ngpsgribfile.${PDY}${CYL}
  ixfile=${DATA}/ngpsixfile.${PDY}${CYL}

fi

# --------------------------------------------------
#   Process ECMWF Ensemble perturbation, if selected
# --------------------------------------------------

if [ ${model} -eq 11 ]
then
    
  if [ -s ${DATA}/ece${pert}gribfile.${PDY}${CYL} ]
  then
    rm ${DATA}/ece${pert}gribfile.${PDY}${CYL}
  fi
    
  if [ ${pert_posneg} = 'n' ]; then
    posneg=2
  elif [ ${pert_posneg} = 'p' ]; then
    posneg=3
  elif [ ${pert_posneg} = 'c' ]; then
    # low-res control
    posneg=1
  else
    set +x
    echo " "
    echo "!!! ERROR: ECMWF PERT ID NOT RECOGNIZED"
    echo "!!! pert_posneg= ${pert_posneg}"
    echo " "
    set -x
    exit 8
  fi
    
  pnum=${pert_num}
  if [ ${pnum} -lt 10 ]; then
    pnum=` echo $pnum | cut -c2-2`
  fi

  if [ ${pnum} -eq 0 ]; then
    # low-res control
    pnum=2
  fi
    
  pert_grep_str=" 0 0 0 1 ${posneg} ${pnum} 1 "

  glo=${DATA}/ece.lores.cut.${PDY}${CYL}
  xlo=${DATA}/ece.lores.cut.${PDY}${CYL}.i

  if [ -s ${glo} ]; then rm ${glo}; fi
  if [ -s ${xlo} ]; then rm ${xlo}; fi

  grid="255 0 360 181 90000 0000 128 -90000 -1000 1000 1000 0"

  # This next part simply uses wgrib to parse out
  # the member records for each variable from each
  # respective enspost file.

  for var in u500 v500 u850 v850 mslp
  do
    
    ecegfile=enspost.t${CYL}z.${var}

    if [ ! -s ${ecedir}/${ecegfile} ]
    then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! ECMWF ENSEMBLE POST File missing: ${ecedir}/${ecegfile}"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " "
      set -x
      continue
    fi
      
    ece_orig=${ecedir}/${ecegfile}
      
    $wgrib -PDS10 ${ece_orig} | grep "${pert_grep_str}" | \
          awk -F:TR= '{print $1}'                       | \
          $wgrib -i ${ece_orig} -grib -append -o ${glo}
      
  done
     
  # ECMWF data are 2.5-degree data, so for consistency
  # with the NCEP ensemble data, we now use copygb to
  # interpolate down to 1-degree.  The -g3 in the copygb
  # statement is for grid 3, a 1x1 global grid (AVN).
      
  ${gix} ${glo} ${xlo}
  gfile=${DATA}/ece${pert}gribfile.${PDY}${CYL}
  $cgb -g"${grid}" -a ${glo} ${xlo} ${gfile}
      
  $gix ${DATA}/ece${pert}gribfile.${PDY}${CYL} ${DATA}/ece${pert}ixfile.${PDY}${CYL}
  gribfile=${DATA}/ece${pert}gribfile.${PDY}${CYL}
  ixfile=${DATA}/ece${pert}ixfile.${PDY}${CYL}
      
fi    

# --------------------------------------
#   Process SREF, if selected
# --------------------------------------

if [ ${model} -eq 13 ]
then
  echo " "
  echo "SREF, for future use...."
  echo " "
  exit 9
fi

    
# ------------------------------------------------------
#   Process Canadian (CMC) hi-res deterministic, if selected
# ------------------------------------------------------

if [ ${model} -eq 15 ]
then
    
  if [ -s ${DATA}/cmcgribfile.${PDY}${CYL} ]
  then
    rm ${DATA}/cmcgribfile.${PDY}${CYL}
  fi
    
  for fhour in ${fcsthrs}
  do
    
    if [ ${fhour} -lt 99 ]; then
      fhour=0${fhour}
    fi
      
    if [ ${fhour} -eq 99 ]
    then
      continue
    fi
      
    if [ ! -s ${cmcdir}/${cmcgfile}${fhour} ]
    then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! CMC File missing: ${cmcdir}/${cmcgfile}"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " "
      echo " !!! Due to missing CMC file, execution is ending...."
      echo " "
      set -x
#      err_exit " FAILED ${jobid} - MISSING CMC FILE IN TRACKER SCRIPT - ABNORMAL EXIT"
      exit 1
    fi
      
    gfile=${cmcdir}/${cmcgfile}${fhour}
    $wgrib -s $gfile >cmc.ix

    if [ $fhour = '000' ]
    then
      vtstring=":anl:"
    else
      if [ ${fhour} -gt 99 ]; then
        vtstring="${fhour}hr"
      fi
      if [ ${fhour} -lt 10 ]; then
        vthour=` echo $fhour | cut -c3-3`
        vtstring="${vthour}hr"
      fi
      if [ ${fhour} -ge 10 -a ${fhour} -lt 99 ]; then
        vthour=` echo $fhour | cut -c2-3`
        vtstring="${vthour}hr"
      fi
    fi  
    
    for parm in ${wgrib_parmlist}
    do
      case ${parm} in
        "SurfaceU")
          grep "UGRD:1.00000 (ETA level)" cmc.ix | grep ${vtstring} | \
            $wgrib -s $gfile -i -grib -append -o ${DATA}/cmcgribfile.${PDY}${CYL} ;;
        "SurfaceV")
          grep "VGRD:1.00000 (ETA level)" cmc.ix | grep ${vtstring} | \
            $wgrib -s $gfile -i -grib -append -o ${DATA}/cmcgribfile.${PDY}${CYL} ;;
                 *)
          grep "${parm}" cmc.ix | grep ${vtstring} | \
            $wgrib -s $gfile -i -grib -append -o ${DATA}/cmcgribfile.${PDY}${CYL} ;;
      esac
    done
    
  done
    
  $gix ${DATA}/cmcgribfile.${PDY}${CYL} ${DATA}/cmcixfile.${PDY}${CYL}
  gribfile=${DATA}/cmcgribfile.${PDY}${CYL}
  ixfile=${DATA}/cmcixfile.${PDY}${CYL}

fi  

# ------------------------------------------------------
#   Process Canadian Ensemble perturbation, if selected
# ------------------------------------------------------

if [ ${model} -eq 16 ]
then
    
  if [ -s ${DATA}/cce${pert}gribfile.${PDY}${CYL} ]
  then
    rm ${DATA}/cce${pert}gribfile.${PDY}${CYL}
  fi
    
  for fhour in ${fcsthrs}
  do
    
    if [ ${fhour} -eq 99 ]
    then
      continue
    fi
      
    if [ ! -s ${ccedir}/${ccegfile}${fhour} ]
    then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! CANADIAN ENSEMBLE ${PERT} File missing: ${ccedir}/${ccegfile}${fhour}"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " "
      set -x
      continue
    fi
      
    gfile=${ccedir}/${ccegfile}${fhour}
    $wgrib -s $gfile >ens.ix
      
    for parm in ${wgrib_parmlist}
    do
      case ${parm} in
        "SurfaceU")
          grep "UGRD:10 m " ens.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${DATA}/cce${pert}gribfile.${PDY}${CYL} ;;
        "SurfaceV")
          grep "VGRD:10 m " ens.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${DATA}/cce${pert}gribfile.${PDY}${CYL} ;;
                 *)
          grep "${parm}" ens.ix | $wgrib -s $gfile -i -grib -append \
                                  -o ${DATA}/cce${pert}gribfile.${PDY}${CYL} ;;
      esac
      
    done
      
  done
     
  $gix ${DATA}/cce${pert}gribfile.${PDY}${CYL} ${DATA}/cce${pert}ixfile.${PDY}${CYL}
  gribfile=${DATA}/cce${pert}gribfile.${PDY}${CYL}
  ixfile=${DATA}/cce${pert}ixfile.${PDY}${CYL}

fi    

# --------------------------------------------
#   Process Hurricane WRF (HWRF), if selected
# --------------------------------------------

if [ ${model} -eq 17 ]
then
    
  grid='255 0 301 141 70000 190000 128 0000 340000  500  500 0'

  if [ ${regflag} -eq 0 ]; then
    set +x
    echo " "
    echo " !!! HWRF model has been selected, but there are no storms in the"
    echo " !!! TC Vitals file that can be processed.  That is, there are no"
    echo " !!! Vitals records from NHC.  The vitals records that are in the"
    echo " !!! updated vitals file must be from another cyclone forecast "
    echo " !!! center, and the HWRF domain does not extend to any "
    echo " !!! region other than that covered by NHC.  Exiting....."
    set -x
    exit 1
  fi
    
  if [ -s ${DATA}/hwrflatlon.pgrb.${PDY}${CYL} ]; then
    rm ${DATA}/hwrflatlon.pgrb.${PDY}${CYL}
  fi

#  for fhour in ${fcsthrs}
#  do
#
#    if [ ${fhour} -eq 99 ]
#    then
#      continue
#    fi
#
#    if [ ! -s ${hwrfdir}/${hwrfgfile}${fhour}.tm00 ]
#    then
#      set +x
#      echo " "
#      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
#      echo " !!! HWRF File missing: ${hwrfdir}/${hwrfgfile}${fhour}.tm00"
#      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
#      set -x
#      continue
#    fi
#    if [ -s ${hwrfdir}/${hwrfifile}${fhour} ]
#    then
#      x1=${hwrfdir}/${hwrfifile}${fhour}
#    else
#      if [ -s ${DATA}/tmphwrfixfile ]; then rm ${DATA}/tmphwrfixfile; fi
#      $gix ${hwrfdir}/${hwrfgfile}${fhour} ${DATA}/tmphwrfixfile
#      x1=${DATA}/tmphwrfixfile
#    fi
#
#    set +x
#    echo " "
#    echo " Extracting HWRF GRIB data for forecast hour = $fhour"
#    echo " "
#    set -x
#
#    g1=${hwrfdir}/${hwrfgfile}${fhour}.tm00

    g1=${hwrfdir}/${hwrfgfile}
    x1=${DATA}/tmphwrfixfile
    $gix $g1 $x1

    $cgb -g"$grid" -k'4*-1 33 100 850' $g1 $x1 ${DATA}/hwrfllu850.grb.f${fhour};   rcc1=$?
    $cgb -g"$grid" -k'4*-1 33 100 700' $g1 $x1 ${DATA}/hwrfllu700.grb.f${fhour};   rcc2=$?
    $cgb -g"$grid" -k'4*-1 33 100 500' $g1 $x1 ${DATA}/hwrfllu500.grb.f${fhour};   rcc3=$?
    $cgb -g"$grid" -k'4*-1 33 105 10'  $g1 $x1 ${DATA}/hwrfllu10m.grb.f${fhour};   rcc4=$?
    $cgb -g"$grid" -k'4*-1 41 100 850' $g1 $x1 ${DATA}/hwrfllav850.grb.f${fhour};  rcc5=$?
    $cgb -g"$grid" -k'4*-1 41 100 700' $g1 $x1 ${DATA}/hwrfllav700.grb.f${fhour};  rcc6=$?
    $cgb -g"$grid" -k'4*-1  7 100 850' $g1 $x1 ${DATA}/hwrfllz850.grb.f${fhour};   rcc7=$?
    $cgb -g"$grid" -k'4*-1  7 100 700' $g1 $x1 ${DATA}/hwrfllz700.grb.f${fhour};   rcc8=$?
    $cgb -g"$grid" -k'4*-1  2 102   0' $g1 $x1 ${DATA}/hwrfllmslp.grb.f${fhour};   rcc9=$?

    if [ $rcc1 -eq 134 -o $rcc2 -eq 134 -o $rcc3 -eq 134 -o $rcc4 -eq 134 -o $rcc5 -eq 134 -o \
         $rcc6 -eq 134 -o $rcc7 -eq 134 -o $rcc8 -eq 134 -o $rcc9 -eq 134 ]
    then
      set +x
      echo " "
      echo "!!! ERROR using $cgb to interpolate HWRF data.  We will stop execution because"
      echo "!!! some variables may have been copied okay, while some obviously have not, "
      echo "!!! and that could lead to unreliable results from the tracker.  Check to make"
      echo "!!! sure you've allocated enough memory for this job.  Exiting...."
      echo " "
      set -x
      err_exit " FAILED ${jobid} - ERROR INTERPOLATING HWRF DATA IN TRACKER SCRIPT - ABNORMAL EXIT"
    fi

    cat ${DATA}/hwrfllu850.grb.f${fhour} ${DATA}/hwrfllu700.grb.f${fhour} \
        ${DATA}/hwrfllu500.grb.f${fhour} ${DATA}/hwrfllz850.grb.f${fhour} \
        ${DATA}/hwrfllz700.grb.f${fhour} ${DATA}/hwrfllmslp.grb.f${fhour} \
        ${DATA}/hwrfllav850.grb.f${fhour} ${DATA}/hwrfllav700.grb.f${fhour} \
        ${DATA}/hwrfllu10m.grb.f${fhour} \
        >>${DATA}/hwrflatlon.pgrb.${PDY}${CYL}

#  done

  $gix ${DATA}/hwrflatlon.pgrb.${PDY}${CYL} ${DATA}/hwrflatlon.pgrb.ix.${PDY}${CYL}
  gribfile=${DATA}/hwrflatlon.pgrb.${PDY}${CYL}
  ixfile=${DATA}/hwrflatlon.pgrb.ix.${PDY}${CYL}

fi


# --------------------------------------------------------------
#   Process Hurricane WRF (HWRF) DAS system (HDAS), if selected
# --------------------------------------------------------------

if [ ${model} -eq 19 ]
then
    
  grid='255 0 301 141 70000 190000 128 0000 340000  500  500 0'

  if [ ${regflag} -eq 0 ]; then
    set +x
    echo " "
    echo " !!! HWRF DAS has been selected, but there are no storms in the"
    echo " !!! TC Vitals file that can be processed.  That is, there are no"
    echo " !!! Vitals records from NHC.  The vitals records that are in the"
    echo " !!! updated vitals file must be from another cyclone forecast "
    echo " !!! center, and the HWRF domain does not extend to any "
    echo " !!! region other than that covered by NHC.  Exiting....."
    set -x
    exit 1
  fi
    
  if [ -s ${DATA}/hdaslatlon.pgrb.${PDY}${CYL} ]; then
    rm ${DATA}/hdaslatlon.pgrb.${PDY}${CYL}
  fi

#  for fhour in ${fcsthrs}
#  do
#
#    if [ ${fhour} -eq 99 ]
#    then
#      continue
#    fi
#
#    if [ ! -s ${hdasdir}/${hdasgfile}${fhour}.tm00 ]
#    then
#      set +x
#      echo " "
#      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
#      echo " !!! HWRF DAS File missing: ${hdasdir}/${hdasgfile}${fhour}.tm00"
#      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
#      set -x
#      continue
#    fi
#    if [ -s ${hdasdir}/${hdasifile}${fhour} ]
#    then
#      x1=${hdasdir}/${hdasifile}${fhour}
#    else
#      if [ -s ${DATA}/tmphdasixfile ]; then rm ${DATA}/tmphdasixfile; fi
#      $gix ${hdasdir}/${hdasgfile}${fhour} ${DATA}/tmphdasixfile
#      x1=${DATA}/tmphdasixfile
#    fi
#
#    set +x
#    echo " "
#    echo " Extracting HDAS GRIB data for forecast hour = $fhour"
#    echo " "
#    set -x
#
#    g1=${hdasdir}/${hdasgfile}${fhour}.tm00

    g1=${hdasdir}/${hdasgfile}
    x1=${DATA}/tmphdasixfile
    $gix $g1 $x1

    $cgb -g"$grid" -k'4*-1 33 100 850' $g1 $x1 ${DATA}/hdasllu850.grb.f${fhour};   rcc1=$?
    $cgb -g"$grid" -k'4*-1 33 100 700' $g1 $x1 ${DATA}/hdasllu700.grb.f${fhour};   rcc2=$?
    $cgb -g"$grid" -k'4*-1 33 100 500' $g1 $x1 ${DATA}/hdasllu500.grb.f${fhour};   rcc3=$?
    $cgb -g"$grid" -k'4*-1 33 105 10'  $g1 $x1 ${DATA}/hdasllu10m.grb.f${fhour};   rcc4=$?
    $cgb -g"$grid" -k'4*-1 41 100 850' $g1 $x1 ${DATA}/hdasllav850.grb.f${fhour};  rcc5=$?
    $cgb -g"$grid" -k'4*-1 41 100 700' $g1 $x1 ${DATA}/hdasllav700.grb.f${fhour};  rcc6=$?
    $cgb -g"$grid" -k'4*-1  7 100 850' $g1 $x1 ${DATA}/hdasllz850.grb.f${fhour};   rcc7=$?
    $cgb -g"$grid" -k'4*-1  7 100 700' $g1 $x1 ${DATA}/hdasllz700.grb.f${fhour};   rcc8=$?
    $cgb -g"$grid" -k'4*-1  2 102   0' $g1 $x1 ${DATA}/hdasllmslp.grb.f${fhour};   rcc9=$?

    if [ $rcc1 -eq 134 -o $rcc2 -eq 134 -o $rcc3 -eq 134 -o $rcc4 -eq 134 -o $rcc5 -eq 134 -o \
         $rcc6 -eq 134 -o $rcc7 -eq 134 -o $rcc8 -eq 134 -o $rcc9 -eq 134 ]
    then
      set +x
      echo " "
      echo "!!! ERROR using $cgb to interpolate HDAS data.  We will stop execution because"
      echo "!!! some variables may have been copied okay, while some obviously have not, "
      echo "!!! and that could lead to unreliable results from the tracker.  Check to make"
      echo "!!! sure you've allocated enough memory for this job.  Exiting...."
      echo " "
      set -x
      err_exit " FAILED ${jobid} - ERROR INTERPOLATING HDAS DATA IN TRACKER SCRIPT - ABNORMAL EXIT"
    fi
      
    cat ${DATA}/hdasllu850.grb.f${fhour} ${DATA}/hdasllu700.grb.f${fhour} \
        ${DATA}/hdasllu500.grb.f${fhour} ${DATA}/hdasllz850.grb.f${fhour} \
        ${DATA}/hdasllz700.grb.f${fhour} ${DATA}/hdasllmslp.grb.f${fhour} \
        ${DATA}/hdasllav850.grb.f${fhour} ${DATA}/hdasllav700.grb.f${fhour} \
        ${DATA}/hdasllu10m.grb.f${fhour} \
        >>${DATA}/hdaslatlon.pgrb.${PDY}${CYL}
      
#  done
      
  $gix ${DATA}/hdaslatlon.pgrb.${PDY}${CYL} ${DATA}/hdaslatlon.pgrb.ix.${PDY}${CYL}
  gribfile=${DATA}/hdaslatlon.pgrb.${PDY}${CYL}
  ixfile=${DATA}/hdaslatlon.pgrb.ix.${PDY}${CYL}
      
fi    

# ----------------------------------------------
#
#   *** Ensemble RELOCATION ONLY follows....***
#
#   Process Ensemble perturbation, if selected
# ----------------------------------------------

if [ ${model} -eq 20 ]; then

  if [ -s ${TRKDATA}/ensr${pert}gribfile.${PDY}${CYL} ]; then
     rm ${TRKDATA}/ensr${pert}gribfile.${PDY}${CYL}
  fi
        
  for fhour in ${fcsthrs}
  do

    if [ ${fhour} -eq 99 ]; then
      continue
    fi
           
    if [ ! -s ${ensrdir}/${ensrgfile}${fhour}$ensrgsuffix ]; then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! "
      echo " !!! ENSEMBLE RELOCATION ${PERT} File missing:               "
      echo " !!!          ${ensrdir}/${ensrgfile}${fhour}$ensrgsuffix    "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! "
      echo " "
      set -x  
      continue
    fi
           
    gfile=${ensrdir}/${ensrgfile}${fhour}$ensrgsuffix
    $wgrib -s $gfile >ensr.ix

    for parm in ${wgrib_parmlist}
    do
      case ${parm} in
        "SurfaceU")
           grep "UGRD:10 m " ensr.ix | $wgrib -s $gfile -i -grib -append \
                               -o ${TRKDATA}/ensr${pert}gribfile.${PDY}${CYL} ;;
        "SurfaceV")
           grep "VGRD:10 m " ensr.ix | $wgrib -s $gfile -i -grib -append \
                               -o ${TRKDATA}/ensr${pert}gribfile.${PDY}${CYL} ;;
                 *)
           grep "${parm}" ensr.ix | $wgrib -s $gfile -i -grib -append \
                               -o ${TRKDATA}/ensr${pert}gribfile.${PDY}${CYL} ;;
      esac
            
    done
            
  done  
         
  $gix ${TRKDATA}/ensr${pert}gribfile.${PDY}${CYL} ${TRKDATA}/ensr${pert}ixfile.${PDY}${CYL}
  gribfile=${TRKDATA}/ensr${pert}gribfile.${PDY}${CYL}
  ixfile=${TRKDATA}/ensr${pert}ixfile.${PDY}${CYL}
            
fi       


set +x
echo "TIMING: Time in extrkr.sh after gribcut for pert= $pert is `date`"
set -x

#------------------------------------------------------------------------#
#                         Now run the tracker                            #
#------------------------------------------------------------------------#

ist=1
while [ $ist -le 15 ]
do
  if [ ${stormflag[${ist}]} -ne 1 ]
  then
    set +x; echo "Storm number $ist NOT selected for processing"; set -x
  else
    set +x; echo "Storm number $ist IS selected for processing...."; set -x
  fi
  let ist=ist+1
done

# Load the forecast hours for this particular model into an array 
# that will be passed into the executable via a namelist....

ifh=1
while [ $ifh -le ${maxtime} ]
do
  fh[${ifh}]=` echo ${fcsthrs} | awk '{print $n}' n=$ifh`
  let ifh=ifh+1
done

namelist=${DATA}/input.${atcfout}.${PDY}${CYL}
ATCFNAME=` echo "${atcfname}" | tr '[a-z]' '[A-Z]'`
  
echo "&datein inp%byy=${syy},inp%bmm=${smm},inp%bdd=${sdd},"    >${namelist}
echo "        inp%bhh=${shh}, inp%model=${model}/"             >>${namelist}
echo "&stormlist stswitch = ${stormflag[1]},${stormflag[2]},"  >>${namelist}
echo "      ${stormflag[3]},${stormflag[4]},${stormflag[5]},"  >>${namelist}
echo "      ${stormflag[6]},${stormflag[7]},${stormflag[8]},"  >>${namelist}
echo "    ${stormflag[9]},${stormflag[10]},${stormflag[11]},"  >>${namelist}
echo "   ${stormflag[12]},${stormflag[13]},${stormflag[14]},"  >>${namelist}
echo "   ${stormflag[15]}/"                                    >>${namelist}
echo "&fhlist itmphrs = ${fh[1]},${fh[2]},${fh[3]},"           >>${namelist}
echo "      ${fh[4]},${fh[5]},${fh[6]},${fh[7]},"              >>${namelist}
echo "      ${fh[8]},${fh[9]},${fh[10]},${fh[11]},"            >>${namelist}
echo "      ${fh[12]},${fh[13]},${fh[14]},"                    >>${namelist}
echo "      ${fh[15]},${fh[16]},${fh[17]},"                    >>${namelist}
echo "      ${fh[18]},${fh[19]},${fh[20]},"                    >>${namelist}
echo "      ${fh[21]},${fh[22]},${fh[23]},"                    >>${namelist}
echo "      ${fh[24]},${fh[25]},${fh[26]},"                    >>${namelist}
echo "      ${fh[27]},${fh[28]},${fh[29]},"                    >>${namelist}
echo "      ${fh[30]},${fh[31]},${fh[32]},"                    >>${namelist}
echo "      ${fh[33]},${fh[34]},${fh[35]},"                    >>${namelist}
echo "      ${fh[36]},${fh[37]},${fh[38]},"                    >>${namelist}
echo "      ${fh[39]},${fh[40]},${fh[41]},"                    >>${namelist}
echo "      ${fh[42]},${fh[43]},${fh[44]},"                    >>${namelist}
echo "      ${fh[45]},${fh[46]},${fh[47]},"                    >>${namelist}
echo "      ${fh[48]},${fh[49]},${fh[50]},"                    >>${namelist}
echo "      ${fh[51]},${fh[52]},${fh[53]},"                    >>${namelist}
echo "      ${fh[54]},${fh[55]},${fh[56]},"                    >>${namelist}
echo "      ${fh[57]},${fh[58]},${fh[59]},"                    >>${namelist}
echo "      ${fh[60]},${fh[61]},${fh[62]},"                    >>${namelist}
echo "      ${fh[63]},${fh[64]},${fh[65]}/"                    >>${namelist}
echo "&atcfinfo atcfnum=${atcfnum},atcfname='${ATCFNAME}'/"    >>${namelist}

export pgm=gettrk
. ./prep_step

ln -s -f ${gribfile}                                    fort.11
ln -s -f ${DATA}/vitals.upd.${atcfout}.${PDY}${shh}     fort.12
ln -s -f ${ixfile}                                      fort.31
ln -s -f ${DATA}/trak.${atcfout}.all.${PDY}${CYL}       fort.61
ln -s -f ${DATA}/trak.${atcfout}.atcf.${PDY}${CYL}      fort.62
ln -s -f ${DATA}/trak.${atcfout}.radii.${PDY}${CYL}     fort.63
ln -s -f ${DATA}/trak.${atcfout}.atcfunix.${PDY}${CYL}  fort.64

set +x
echo " "
echo " -----------------------------------------------"
echo "           NOW EXECUTING TRACKER......"
echo " -----------------------------------------------"
echo " "
set -x

msg="$pgm start for $atcfout at ${CYL}z"
postmsg "$jlogfile" "$msg"

#${exectrkdir}/gettrk <${namelist}
GETTRKEXEC=${GETTRKEXEC:-$exectrkdir/gettrk}

$GETTRKEXEC <${namelist}
gettrk_rcc=$?

set +x
echo "TIMING: Time in extrkr.sh after gettrk for pert= $pert is `date`"
set -x

#--------------------------------------------------------------#
# Now copy the output track files to different directories
#--------------------------------------------------------------#

set +x
echo " "
echo " -----------------------------------------------"
echo "    NOW COPYING OUTPUT TRACK FILES TO COM  "
echo " -----------------------------------------------"
echo " "
set -x

if [ ${gettrk_rcc} -eq 0 ]; then

  msg="$pgm end for $atcfout at ${CYL}z completed normally"
  postmsg "$jlogfile" "$msg"

# Copy atcf files to NHC archives. We'll use Steve Lord's original script,
# distatcf.sh, to do this, and that script requires the input atcf file to
# have the name "attk126", so first copy the file to that name, then call
# the distatcf.sh script.  After that's done, then copy the full 0-72h
# track into the /com/hur/prod/global track archive file.

  if [ ${SENDCOM} = 'YES' ]
  then

    gltrakarch=${gltrakarch:-${gltrkdir}/tracks.${syy}}
    tmtrakarch=${tmtrakarch:-$DISK_TRAK/wx20tm/trak/prod/tracks.all.${syy}}

    glatcfarch=${glatcfarch:-${gltrkdir}/tracks.atcf.${syy}}
    tmatcfarch=${tmatcfarch:-$DISK_TRAK/wx20tm/trak/prod/tracks.atcf.${syy}}

    glatuxarch=${glatuxarch:-${gltrkdir}/tracks.atcfunix.${syy}}
    tmatuxarch=${tmatuxarch:-$DISK_TRAK/wx20tm/trak/prod/tracks.atcfunix.${syy}}

    glradarch=${glradarch:-${gltrkdir}/tracks.radii.${syy}}
    tmradarch=${tmradarch:-$DISK_TRAK/wx20tm/trak/prod/tracks.radii.${syy}}

    cat ${DATA}/trak.${atcfout}.all.${PDY}${CYL}   >>${gltrakarch}
    #cat ${DATA}/trak.${atcfout}.all.${PDY}${CYL}   >>${tmtrakarch}

    cat ${DATA}/trak.${atcfout}.atcf.${PDY}${CYL}  >>${glatcfarch}
    #cat ${DATA}/trak.${atcfout}.atcf.${PDY}${CYL}  >>${tmatcfarch}

    cat ${DATA}/trak.${atcfout}.atcfunix.${PDY}${CYL}  >>${glatuxarch}
    #cat ${DATA}/trak.${atcfout}.atcfunix.${PDY}${CYL}  >>${tmatuxarch}

    cat ${DATA}/trak.${atcfout}.radii.${PDY}${CYL} >>${glradarch}
    #cat ${DATA}/trak.${atcfout}.radii.${PDY}${CYL} >>${tmradarch}


    if [ ${PARAFLAG} = 'YES' ]
    then
      echo " "
    else
      if [ ${cmodel} = 'gfdl' ]
      then
        cp ${DATA}/trak.${atcfout}.atcfunix.${PDY}${CYL} ${COM}/${stormenv}.${PDY}${CYL}.trackeratcfunix
      else
        if [ ${cmodel} = 'gdas' -o ${cmodel} = 'hdas' ]
        then
          cp ${DATA}/trak.${atcfout}.all.${PDY}${CYL}      ${COM}/${atcfout}.t${CYL}z.cyclone.track
        fi
        cp ${DATA}/trak.${atcfout}.atcfunix.${PDY}${CYL} ${COM}/${atcfout}.t${CYL}z.cyclone.trackatcfunix
      fi

      tmscrdir=$DISK_TRAK/wx20tm/trak/prod

      tmtrakstat=${tmscrdir}/tracker.prod.status
      echo "${atcfout} tracker completed okay for ${PDY}${CYL}" >>${tmtrakstat}

      export SENDDBN=${SENDDBN:-YES}
      export SENDTRACKER=${SENDTRACKER:-NO}
      if [ ${SENDDBN} = 'YES' -o ${SENDTRACKER} = 'YES' ]
      then
        if [ ${cmodel} = 'gfdl' ]
        then
          $DBNROOT/bin/dbn_alert ATCFUNIX GFS_NAVY $job ${COM}/${stormenv}.${PDY}${CYL}.trackeratcfunix
        else
          if [ $cmodel != 'ece' -a $cmodel != 'cens' -a $cmodel != 'emx' -a $cmodel != 'ens' -a $cmodel != 'gfs_enr' ]; then
            $DBNROOT/bin/dbn_alert ATCFUNIX GFS_NAVY $job ${COM}/${atcfout}.t${CYL}z.cyclone.trackatcfunix
          fi
        fi
      fi

      # ------------------------------------------
      # Cat atcfunix files to storm trackers files
      # ------------------------------------------
      #
      # We need to parse apart the atcfunix file and distribute the forecasts to 
      # the necessary directories.  To do this, first sort the atcfunix records 
      # by forecast hour (k6), then sort again by ocean basin (k1), storm number (k2)
      # and then quadrant radii wind threshold (k12).  Once you've got that organized 
      # file, break the file up by putting all the forecast records for each storm 
      # into a separate file.  Then, for each file, find the corresponding atcfunix 
      # file in the storm trackers directory and dump the atcfunix records for that storm 
      # in there.  NOTE: Only do this if the model run is NOT for the CMC or 
      # ECMWF ensemble.  The reason is that we do NOT want to write out the individual 
      # member tracks to the atcfunix file.  We only want to write out the ensemble
      # mean track to the atcfunix file, and the mean track is calculated and written
      # out in a separate script.


      if [ $cmodel != 'ece' -a $cmodel != 'cens' -a $cmodel != gfs_enr ]; then

        if [ ${cmodel} = 'gfdl' ]
        then
          auxfile=${COM}/${stormenv}.${PDY}${CYL}.trackeratcfunix
        else
          auxfile=${DATA}/trak.${atcfout}.atcfunix.${PDY}${CYL}
        fi

        sort -k6 ${auxfile} | sort -k1 -k2 -k12  >atcfunix.sorted

        old_string="XX, XX"

        ict=0
        while read unixrec
        do
          storm_string=` echo "${unixrec}" | cut -c1-6`
          if [ "${storm_string}" = "${old_string}" ]
          then
            echo "${unixrec}" >>atcfunix_file.${ict}
          else
            let ict=ict+1
            echo "${unixrec}"  >atcfunix_file.${ict}
            old_string="${storm_string}"
          fi
        done <atcfunix.sorted

        if [ $ict -gt 0 ]
        then
          mct=0
          while [ $mct -lt $ict ]
          do
            let mct=mct+1
            at=` head -1 atcfunix_file.$mct | cut -c1-2 | tr '[A-Z]' '[a-z]'`
            NO=` head -1 atcfunix_file.$mct | cut -c5-6`

            if [ ! -d $ATCFdir/${at}${NO}${syyyy} ]
            then
                mkdir -p $ATCFdir/${at}${NO}${syyyy}
            fi
            cat atcfunix_file.$mct >>$ATCFdir/${at}${NO}${syyyy}/ncep_a${at}${NO}${syyyy}.dat
            set +x
            echo " "
            echo "+++ Adding records to  TPC ATCFUNIX directory: $ATCFdir/${at}${NO}${syyyy}/ncep_${at}${NO}${syyyy}"
            echo " "
            set -x
          done
        fi

      fi

    fi

  fi

else

  if [ ${PARAFLAG} = 'YES' ]
  then
    echo " "
  else
    tmtrakstat=$DISK_TRAK/wx20tm/trak/prod/tracker.prod.status
    echo "ERROR: ${atcfout} tracker FAILED for ${PDY}${CYL}" >>${tmtrakstat}
  fi

  set +x
  echo " "
  echo "!!! ERROR -- An error occurred while running gettrk.x, "
  echo "!!! which is the program that actually gets the track."
  echo "!!! Return code from gettrk.x = ${gettrk_rcc}"
  echo "!!! model= ${atcfout}, forecast initial time = ${PDY}${CYL}"
  echo "!!! Exiting...."
  echo " "
  set -x
  err_exit " FAILED ${jobid} - ERROR RUNNING GETTRK IN TRACKER SCRIPT- ABNORMAL EXIT"

fi
