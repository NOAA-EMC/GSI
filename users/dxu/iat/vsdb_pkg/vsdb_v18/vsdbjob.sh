#!/bin/ksh
set -x

##---------------------------------------------------------------------------
## Modified version of vsdbjob_submit.sh for use in NCEP/EMC GFS para_config 
## to do verification while forecast is running.
## Fanglin Yang, 01Jan2010
##---------------------------------------------------------------------------

## variables read in from vrfy.sh and/or para_config
export DATEST=${1:-20080701}                         ;#forecast starting date
export DATEND=${2:-20080801}                         ;#forecast ending date
export vlength=${3:-384}                             ;#forecast length in hour
export cycle=${4:-"00"}                              ;#forecast cycle
export exp1name=${5:-"pre13a"}                       ;#experiment names
export VSDB_START_DATE=${6:-$VSDB_START_DATE}        ;#map starting date
export anl_type=${7:-${anltype:-gfs}}                ;#analysis type for verification: gfs, gdas, ecmwf, manl or canl
export gfs_cyc=${8:-${gfs_cyc:-1}}                   ;#number of GFS cycles, 1-->00Z, 4-->00Z 06Z 12Z and 18Z        
export exp1dir=${ARCDIR1:-/global/hires/glopara/archive} ;#online archive of current exp
export scppgb=${SCP_PGB:-"NO"}                       ;#whether of not to scp pgb files from CLIENT
export sfcvsdb=${sfcvsdb:-"YES"}                      ;#include the group of surface variables

##
##-------------------------------------------------------------------
##-------------------------------------------------------------------

MAKEVSDBDATA=${VSDB_STEP1:-NO}           ;#To create VSDB date

MAKEMAPS=${VSDB_STEP2:-NO}               ;#To make AC and RMS maps

CONUSPLOTS=${VSDB_STEP2:-NO}             ;#To make precip verification plots 

CONUSDATA=${VRFYPRCP:-NO}                ;#To compute precip threat skill scores

#----------------------------------------------------------------------
export machine=${machine:-ZEUS}                                  ;#IBM or ZEUS                  
export machine=$(echo $machine|tr '[a-z]' '[A-Z]')
export ACCOUNT=${ACCOUNT:-GFS-T2O}                               ;#ibm computer ACCOUNT task
export CUE2RUN=${CUE2RUN:-shared}                                ;#dev or devhigh or 1
export CUE2FTP=${CUE2FTP:-$CUE2RUN}                              ;#queue for data transfer
export GROUP=${GROUP:-g01}                                       ;#account group       
export webhost=${webhost:-"emcrzdm.ncep.noaa.gov"}               ;#host for web display
export webhostid=${webhostid:-$LOGNAME}                          ;#id of webhost
export ftpdir=${WEBDIR:-/home/people/emc/www/htdocs/gmb/$webhostid}/vsdb
export doftp=${SEND2WEB:-"NO"}                                   ;#whether or not to sent maps to ftpdir
export vsdbsave=${vsdbsave:-/stmp/$LOGNAME/VSDB/vsdb_data}       ;#place where vsdb database is saved

myhost=`echo $(hostname) |cut -c 1-1 `

if [ $myhost = c -o $myhost = s -o $machine = IBM ]; then
 export vsdbhome=${vsdbhome:-/global/save/wx24fy/VRFY/vsdb}    ;#script home, do not change
 export GNOSCRUB=${GNOSCRUB:-/global/noscrub}          ;#archive directory
 export STMP=${STMP:-/stmp}                            ;#temporary directory
 export PTMP=${PTMP:-/ptmp}                            ;#temporary directory

 export obdata=/climate/save/wx24fy/obdata             ;#observation data for making 2dmaps
 export gstat=/global/shared/stat                      ;#global stats directory
 export gfsvsdb=/climate/save/wx24fy/VRFY/vsdb_data    ;#operational gfs vsdb database
 export canldir=$gstat/canl                            ;#consensus analysis directory
 export ecmanldir=/global/shared/stat/ecm              ;#ecmwf analysis directory
 export OBSPCP=$gstat/OBSPRCP                          ;#observed precip for verification
 export gfswgnedir=$gstat/wgne1                        ;#operational gfs precip QPF scores
 export gfsfitdir=/climate/save/wx23ss                 ;#Suru operational model fit-to-obs database
 export SUBJOB=$vsdbhome/bin/sub_ibm                   ;#script for submitting batch jobs
 export NWPROD=$vsdbhome/nwprod                        ;#common utilities and libs included in /nwprod
 export GRADSBIN=/usrx/local/grads/bin                 ;#GrADS executables
 export IMGCONVERT=/usrx/local/im_beta/bin/convert     ;#image magic converter
 export FC=/usr/bin/xlf90                              ;#fortran compiler
 export FFLAG=" "                                      ;#fortran compiler options

elif [ $machine = ZEUS ]; then
 export vsdbhome=${vsdbhome:-/scratch2/portfolios/NCEPDEV/global/save/Fanglin.Yang/VRFY/vsdb} ;#script home, do not change
 export GNOSCRUB=${GNOSCRUB:-/scratch2/portfolios/NCEPDEV/global/noscrub}        ;#archive directory
 export STMP=${STMP:-/scratch2/portfolios/NCEPDEV/stmp}                          ;#temporary directory
 export PTMP=${PTMP:-/scratch2/portfolios/NCEPDEV/ptmp}                          ;#temporary directory

 export obdata=/scratch2/portfolios/NCEPDEV/global/save/Fanglin.Yang/obdata      ;#observation data for making 2dmaps
 export gstat=/scratch2/portfolios/NCEPDEV/global/noscrub/stat  ;#global stats directory
 export gfsvsdb=$gstat/vsdb_data                            ;#operational gfs vsdb database
 export canldir=$gstat/canl                                 ;#consensus analysis directory
 export ecmanldir=$gstat/ecm                                ;#ecmwf analysis directory
 export OBSPCP=$gstat/OBSPRCP                               ;#observed precip for verification
 export gfswgnedir=$gstat/wgne1                             ;#operational gfs precip QPF scores
 export gfsfitdir=$gstat/surufits                           ;#Suru operational model fit-to-obs database
 export SUBJOB=$vsdbhome/bin/sub_zeus                       ;#script for submitting batch jobs
 export NWPROD=$vsdbhome/nwprod                             ;#common utilities and libs included in /nwprod
 export GRADSBIN=/apps/grads/2.0.a9/bin                     ;#GrADS executables
 export IMGCONVERT=/usr/bin/convert                         ;#image magic converter
 export FC=/apps/intel/composerxe-2011.4.191/composerxe-2011.4.191/bin/intel64/ifort ;#intel compiler
 export FFLAG="-O2 -convert big_endian -FR"                 ;#intel compiler options
elif [ $machine = JET ]; then
 export vsdbhome=${vsdbhome:-/pan2/projects/gnmip/Fanglin.Yang/VRFY/vsdb}   ;#script home, do not change
 export GNOSCRUB=${GNOSCRUB:-/pan2/projects/gnmip/$LOGNAME/noscrub} ;#temporary directory                  
 export STMP=${STMP:-/pan2/projects/gnmip/$LOGNAME/ptmp}            ;#temporary directory                          
 export PTMP=${PTMP:-/pan2/projects/gnmip/$LOGNAME/ptmp}            ;#temporary directory                          

 export obdata=/pan2/projects/gnmip/Fanglin.Yang/VRFY/obdata    ;#observation data for making 2dmaps
 export gstat=/pan2/projects/gnmip/Fanglin.Yang/VRFY/stat       ;#global stats directory              
 export gfsvsdb=$gstat/vsdb_data                            ;#operational gfs vsdb database
 export canldir=$gstat/canl                                 ;#consensus analysis directory
 export ecmanldir=$gstat/ecm                                ;#ecmwf analysis directory
 export OBSPCP=$gstat/OBSPRCP                               ;#observed precip for verification
 export gfswgnedir=$gstat/wgne1                             ;#operational gfs precip QPF scores
 export gfsfitdir=$gstat/surufits                           ;#Suru operational model fit-to-obs database
 export SUBJOB=$vsdbhome/bin/sub_jet                        ;#script for submitting batch jobs
 export NWPROD=$vsdbhome/nwprod                             ;#common utilities and libs included in /nwprod
 export GRADSBIN=/opt/grads/2.0.a2//bin/grads               ;#GrADS executables       
 export IMGCONVERT=/usr/bin/convert                         ;#image magic converter
 export FC=/opt/intel/Compiler/11.1/072//bin/intel64/ifort  ;#intel compiler
 export FFLAG="-O2 -convert big_endian -FR"                 ;#intel compiler options

elif [ $myhost = t -o $machine = WCOSS ]; then
 export vsdbhome=${vsdbhome:-/global/save/Fanglin.Yang/VRFY/vsdb}    ;#script home, do not change
 export GNOSCRUB=${GNOSCRUB:-/global/noscrub}          ;#archive directory
 export STMP=${STMP:-/stmp}                            ;#temporary directory
 export PTMP=${PTMP:-/ptmp}                            ;#temporary directory

 export obdata=/global/save/Fanglin.Yang/obdata        ;#observation data for making 2dmaps
 export gstat=/global/noscrub/Fanglin.Yang/stat        ;#global stats directory
 export gfsvsdb=$gstat/vsdb_data                       ;#operational gfs vsdb database
 export canldir=$gstat/canl                            ;#consensus analysis directory
 export ecmanldir=$gstat/ecm                           ;#ecmwf analysis directory
 export OBSPCP=$gstat/OBSPRCP                          ;#observed precip for verification
 export gfswgnedir=$gstat/wgne1                        ;#operational gfs precip QPF scores
 export gfsfitdir=$gstat/surufits                      ;#Suru operational model fit-to-obs database
 export SUBJOB=$vsdbhome/bin/sub_wcoss                 ;#script for submitting batch jobs
 export CUE2FTP=transfer                               ;#data transfer queue 
 export NWPROD=$vsdbhome/nwprod                        ;#common utilities and libs included in /nwprod
 export GRADSBIN=/usrx/local/GrADS/2.0.2/bin           ;#GrADS executables
 export IMGCONVERT=/usrx/local/ImageMagick/6.8.3-3/bin/convert ;#image magic converter
 export FC=/usrx/local/intel/composer_xe_2011_sp1.11.339/bin/intel64/ifort    ;#intel compiler
 export FFLAG="-O2 -convert big_endian -FR"            ;#fortran compiler options
fi

if [ $gfs_cyc = 1 ]; then
 export vhrlist=${vhrlist:-"$cycle"}            ;#verification hours for each day
 export fcyclist="$cycle"                       ;#forecast cycles to be included in stats computation
 export cyc2runvsdb="$cycle"                    ;#cycle to run vrfy which will generate vsdb data for all cycles of the day
elif [ $gfs_cyc = 2 ]; then
 export vhrlist=${vhrlist:-"00 12 "}            ;#verification hours for each day
 export fcyclist="00 12"                        ;#forecast cycles to be included in stats computation
 export cyc2runvsdb=12                          ;#cycle to run vrfy which will generate vsdb data for all cycles of the day
elif [ $gfs_cyc = 4 ]; then
 export vhrlist=${vhrlist:-"00 06 12 18"}       ;#verification hours for each day
 export fcyclist="00 06 12 18"                  ;#forecast cycles to be included in stats computation
 export cyc2runvsdb=18                          ;#cycle to run vrfy which will generate vsdb data for all cycles of the day
else
 echo "gfs_cyc must be 1, 2 or 4, quit vsdbjob"                                          
 exit
fi

if [ $cycle != $cyc2runvsdb ]; then 
 MAKEVSDBDATA=NO 
 MAKEMAPS=NO 
fi
if [ $cycle != 00 -a $cycle != 12 ]; then 
 CONUSPLOTS=NO
 CONUSDATA=NO
fi
 

### --------------------------------------------------------------
###   make vsdb database
      if [ $MAKEVSDBDATA = YES ] ; then
### --------------------------------------------------------------
export fcyclist="$fcyclist"                         ;#all fcst cycles to be included in verification
export expnlist=$exp1name                           ;#experiment names 
export expdlist=$exp1dir                            ;#exp online archive directories
export complist=$(hostname)                         ;#computers where experiments are run
export dumplist=".gfs."                             ;#file format pgb${asub}${fhr}${dump}${yyyymmdd}${cyc}

export anl_type=$anl_type                           ;#analysis type for verification: gfs, gdas or canl
export DATEST=$DATEST                               ;#verification starting date
export DATEND=$DATEND                               ;#verification ending date
export vlength=$vlength                             ;#forecast length in hour
export asub=${asub:-a}                              ;#string in pgb anal file after pgb, say, pgbanl, pgbhnl 
export fsub=${fsub:-f}                              ;#string in pgb fcsy file after pgb, say, pgbf06, pgbh06

if [ ! -d $vsdbhome ]; then
 echo "$vsdbhome does not exist "
 exit
fi
if [ ! -d $expdlist ]; then
 echo "$expdlist does not exist "
 exit
fi

export rundir=${rundir:-$STMP/$LOGNAME/vsdb_exp}
#export listvar1=fcyclist,vhrlist,expnlist,expdlist,complist,dumplist,DATEST,DATEND,vlength,rundir
#export listvar2=machine,anl_type,scppgb,sfcvsdb,canldir,ecmanldir,vsdbsave,vsdbhome,gd,NWPROD
#export listvar="$listvar1,$listvar2"

${vsdbhome}/verify_exp_step1.sh

### --------------------------------------------------------------
      fi                                       
### --------------------------------------------------------------


 
### --------------------------------------------------------------
###   make AC and RMSE maps            
      if [ $MAKEMAPS = YES ] ; then
### --------------------------------------------------------------
#
export mdlist=${mdlist:-"gfs $exp1name"}        ;#experiment names, up to 10                                     
export fcyclist="$fcyclist"                     ;#forecast cycles to show on map 
export DATEST=${VSDB_START_DATE:-$DATEST}       ;#map starting date  starting date to show on map
export DATEND=$DATEND                           ;#verification ending date to show on map
export vlength=$vlength                         ;#forecast length in hour to show on map
export maptop=${maptop:-10}                     ;#can be set to 10, 50 or 100 hPa for cross-section maps
export maskmiss=${maskmiss:-1}                  ;#remove missing data from all models to unify sample size, 0-->NO, 1-->Yes

set -A namelist $mdlist
export rundir=${rundir:-/stmp/$LOGNAME/vsdb_exp}/${namelist[1]}

${vsdbhome}/verify_exp_step2.sh
### --------------------------------------------------------------
    fi
### --------------------------------------------------------------


### --------------------------------------------------------------
###   make CONUS precip plots
      if [ $CONUSPLOTS = YES ] ; then
### --------------------------------------------------------------
export expnlist=$mdlist                                             ;#experiment names, up to 6 
export expdlist=${expd_list:-"$exp1dir $exp1dir $exp1dir $exp1dir $exp1dir $exp1dir"}    ;#precip stats online archive dirs
export complist=${comp_list:-"$(hostname) $(hostname) $(hostname) $(hostname) $(hostname) $(hostname) "}  ;#computers where experiments are run

export cycle=$cycle                                       ;#cycle to make QPF plots 
export DATEST=$DATEST                                     ;#forecast starting date to show on map
export DATEND=$(echo $($NWPROD/util/exec/ndate -${VBACKUP_PRCP:-00} ${DATEND}00 ) |cut -c1-8 )
export rundir=${rundir:-/stmp/$LOGNAME}/rain
export scrdir=${vsdbhome}/precip                  
export vhour=${vhour:-180}                                 ;#verification length in hour
                                                                                                                           
${scrdir}/plot_pcp.sh
### --------------------------------------------------------------
      fi
### --------------------------------------------------------------
                                                                                                                           

### --------------------------------------------------------------
###   compute precip threat score stats over CONUS
      if [ $CONUSDATA = YES ] ; then
### --------------------------------------------------------------
export cycle=$cycle                                 ;#cycle to generate QPF stats data
export expnlist=$exp1name                           ;#experiment names 
export expdlist=`dirname $COMROT`                   ;#exp online archive directories
export complist=$(hostname)                         ;#computers where experiments are run
export dumplist=".gfs."                             ;#file format pgb${asub}${fhr}${dump}${yyyymmdd}${cyc}
export DATEST=`$NWPROD/util/exec/ndate -${VBACKUP_PRCP:-00} ${DATEST}00 |cut -c 1-8 ` ;#verification starting date
export DATEND=`$NWPROD/util/exec/ndate -${VBACKUP_PRCP:-00} ${DATEND}00 |cut -c 1-8 ` ;#verification starting date

export ftyplist="flx"                               ;#file types: pgb or flx
export dumplist=".gfs."                             ;#file format ${ftyp}f${fhr}${dump}${yyyymmdd}${cyc}
export ptyplist="PRATE"                             ;#precip types in GRIB: PRATE or APCP
export bucket=${bucket:-6}                          ;#accumulation bucket in hours. bucket=0 -- continuous accumulation
export fhout=6                                      ;#forecast output frequency in hours
export vhour=${vhour:-180}                           ;#verification length in hour
export ARCDIR=${ARCDIR1:-$GNOSCRUB/$LOGNAME/archive} ;#directory to save stats data
export rundir=${rundir:-$STMP}/mkup_precip          ;#temporary running directory
export scrdir=${vsdbhome}/precip

#export listvar1=expnlist,expdlist,complist,ftyplist,dumplist,ptyplist,bucket,fhout,cyclist,vhour
#export listvar2=machine,DATEST,DATEND,ARCDIR,rundir,scrdir,OBSPCP,mapdir,scppgb,NWPROD
#export listvar="$listvar1,$listvar2"

${scrdir}/mkup_rain_stat.sh  
### --------------------------------------------------------------
      fi
### --------------------------------------------------------------


exit

