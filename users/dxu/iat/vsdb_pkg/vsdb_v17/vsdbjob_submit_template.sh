#!/bin/ksh
set -ux

#------------------------------------------
# Set up flag to run each step: YES/NO
#------------------------------------------
# step 1 : To create VSDB date
MAKEVSDBDATA=${ENV_MAKEVSDBDATA}  

# step 2 : To make AC and RMS maps 
MAKEMAPS=${ENV_MAKEMAPS}         

# step 3 : To generate precip verification stats
CONUSDATA=${ENV_CONUSDATA}      

# step 4 : To make precip verification maps
CONUSPLOTS=${ENV_CONUSPLOTS}   

# step 5 : To make fit-to-obs maps
FIT2OBS=${ENV_FIT2OBS}        

# step 6 : To make maps of lat-lon distributions and zonal-mean corss-sections. 
MAPS2D=${ENV_MAPS2D}

#-----------------------------------------------------
# Specify machine : zeus, jibb, etc. 
#-----------------------------------------------------
export machine=${ENV_MACHINE}
export machine=$(echo $machine|tr '[a-z]' '[A-Z]')
myhome=`pwd`
set -a;. ${myhome}/setup_envs_template.sh $machine 
if [ $? -ne 0 -o $rc -gt 0 ]; then exit; fi
set -ux

#-----------------------------------------------------
# Top-level setting: web dir
#-----------------------------------------------------
export mapdir=$STMP/$LOGNAME/web               
if [ ! -d $mapdir ]; then
   mkdir -p $mapdir ; cd $mapdir ||exit
   tar xvf ${vsdbhome}/vsdb_exp_webpage.tar 
fi


#------------------------------
# current computer host name
#------------------------------
chost=$(hostname)      

### --------------------------------------------------------------
### step 1:
###   make vsdb database
### --------------------------------------------------------------
if [ $MAKEVSDBDATA = YES ] ; then
   # Create run dir with step name
   . ${myhome}/makeRunDir.sh step1

   #1. Input dir :
   myarch=${ENV_1_MYARCH}

   #2. Output  dir :
   #  $vsdbsave ( set in setup_envs.sh)

   #3. Running directory
   export rundir=$tmpdir/stats

   #4. Case configuration
   export expdlist="$myarch $myarch"      # <== input dir
   export expnlist=${ENV_1_EXPNLIST}
   export fcyclist=${ENV_1_FCYCLIST}
   export complist="$chost  $chost "
   export dumplist=${ENV_1_DUMPLIST}
   export vhrlist=${ENV_1_VHRLIST}
   export DATEST=${ENV_1_DATEST}
   export DATEND=${ENV_1_DATEND}
   export vlength=${ENV_1_VLENGTH}


   export listvar1=fcyclist,expnlist,expdlist,complist,dumplist,vhrlist,DATEST,DATEND,vlength,rundir
   export listvar2=machine,anl_type,scppgb,sfcvsdb,canldir,ecmanldir,vsdbsave,vsdbhome,gd,NWPROD
   export listvar="$listvar1,$listvar2"

   ## pgb files must be saved as $expdlist/$expnlist/pgbf${fhr}${cdump}${yyyymmdd}${cyc}
   if [ $batch = YES ]; then
      $SUBJOB -e $listvar -a $ACCOUNT  -q $CUE2RUN -g $GROUP -p 1/1/N -r 2048/1 -t 6:00:00 \
      -j vstep1 -o $tmpdir/vstep1.out  ${vsdbhome}/verify_exp_step1.sh
   else
      ${vsdbhome}/verify_exp_step1.sh 1>${tmpdir}/vstep1.out 2>&1
   fi

fi                                       


### --------------------------------------------------------------
### step 2:
###   make AC and RMSE maps            
### --------------------------------------------------------------
if [ $MAKEMAPS = YES ] ; then
   # Create run dir with step name
   . ${myhome}/makeRunDir.sh step2

   #1. Input dir :
   #   $gfsvsdb (OPTIONAL, used if exp is gfs, set in setup_envs.sh) 
   #   $vsdbsave (set in setup_envs.sh)

   #2. Output  dir :
   #   $mapdir/allmodel

   #3. Running directory and score directory
   export rundir=$tmpdir/acrms$$
   export scoredir=$rundir/score

   #4. Case configuration 
   export fcycle=${ENV_2_FCYCLE}
   export mdlist=${ENV_2_MDLIST}
   export vsdblist="$vsdbsave $vsdbsave"
   export vhrlist=${ENV_2_VHRLIST}
   export DATEST=${ENV_2_DATEST}
   export DATEND=${ENV_2_DATEND}
   export vlength=${ENV_2_VLENGTH}
   export maptop=${ENV_2_MAPTOP}
   export maskmiss=1         

   ${vsdbhome}/verify_exp_step2.sh  1>${tmpdir}/vstep2.out 2>&1 

   ##--wait 3 hours for all stats to be created and then generate scorecard 
   if [ ${scorecard:-NO} = YES ]; then
      if [ $batch = YES ]; then
	 listvar=SDATE,EDATE,mdlist,webhostid,webhost,ftpdir,doftp,rundir,scoredir,vsdbhome,mapdir
	 $SUBJOB -e $listvar -a $ACCOUNT  -q $CUE2FTP -g $GROUP -p 1/1/S -r 1024/1 -t 1:00:00 -w +0300 \
	 -j scorecard -o $rundir/score.out  ${vsdbhome}/run_scorecard.sh   
      else
	 sleep 10800
	 ${vsdbhome}/run_scorecard.sh  1>$rundir/score.out 2>&1 
      fi
   fi
fi



### --------------------------------------------------------------
### step 3:
###   compute precip threat score stats over CONUS   
### --------------------------------------------------------------
if [ $CONUSDATA = YES ] ; then
   # Create run dir with step name
   . ${myhome}/makeRunDir.sh step3

   #1. Input dir :
   #   $OBSPCP (REQUIRED data, set in setup_envs.sh)
   export COMROT=${ENV_3_COMROT}

   #2. Output dir :
   export ARCDIR=$GNOSCRUB

   #3. Running directory
   export rundir=$tmpdir/mkup_precip           

   #4. Case configuration
   export expdlist="$COMROT $COMROT"          # <== input dir
   export expnlist=${ENV_3_EXPNLIST}
   export hpsslist="/NCEPDEV/hpssuser/g01/wx24fy/WCOSS /NCEPDEV/hpssuser/g01/wx24fy/WCOSS" 
   export complist="$chost  $chost "         
   export ftyplist=${ENV_3_FTYPLIST}
   export dumplist=${ENV_3_DUMPLIST}
   export ptyplist=${ENV_3_PTYPLIST}
   export bucket=${ENV_3_BUCKET}
   export fhout=${ENV_3_FHOUT}
   export cycle=${ENV_3_CYCLE}
   export DATEST=${ENV_3_DATEST}
   export DATEND=${ENV_3_DATEND}

   #5. Precip source directory
   export scrdir=${vsdbhome}/precip                  
														     
   export listvar1=expnlist,expdlist,hpsslist,complist,ftyplist,dumplist,ptyplist,bucket,fhout,cycle
   export listvar2=machine,DATEST,DATEND,ARCDIR,rundir,scrdir,OBSPCP,mapdir,scppgb,NWPROD
   export listvar="$listvar1,$listvar2"

   if [ $batch = YES ]; then
      $SUBJOB -e $listvar -a $ACCOUNT  -q $CUE2RUN -g $GROUP -p 1/1/S -r 1024/1 -t 06:00:00  \
      -j mkup_rain_stat.sh -o $tmpdir/mkup_rain_stat.out ${scrdir}/mkup_rain_stat.sh
   else
      ${scrdir}/mkup_rain_stat.sh  1>${tmpdir}/mkup_rain_stat.out 2>&1       
   fi
fi


### --------------------------------------------------------------
### step 4:
###   make CONUS precip skill score maps 
### --------------------------------------------------------------
if [ $CONUSPLOTS = YES ] ; then
   # Create run dir with step name
   . ${myhome}/makeRunDir.sh step4

   #1. Input dir :
   #  $gstat/wgne1 ( OPTIONAL, used if exp = gfs , set in setup_envs.sh)
   export ARCDIR=$GNOSCRUB

   #2. Output dir :
   #  $mapdir/rain ( set up above)

   #3. Running dir :
   export rundir=$tmpdir/plot_pcp

   #4. Case configuration
   export expdlist="$ARCDIR $ARCDIR"              # <== input dir
   export expnlist=${ENV_4_EXPNLIST}
   export complist="$chost  $chost "               
   export cyclist=${ENV_4_CYCLIST}
   export DATEST=${ENV_4_DATEST}
   export DATEND=${ENV_4_DATEND}

   #5. precip source directory
   export scrdir=${vsdbhome}/precip                  
														  
   export listvar1=expnlist,expdlist,complist,cyclist,DATEST,DATEND,rundir,scrdir
   export listvar2=doftp,webhost,webhostid,ftpdir,scppgb,gstat,NWPROD,mapdir,GRADSBIN
   export listvar3=vsdbhome,SUBJOB,ACCOUNT,GROUP,CUE2RUN,CUE2FTP
   export listvar="$listvar1,$listvar2,$listvar3"

   if [ $batch = YES ]; then
      $SUBJOB -e $listvar -a $ACCOUNT  -q $CUE2RUN -g $GROUP -p 1/1/S -r 2048/1 -t 01:00:00  \
      -j plot_pcp -o $tmpdir/plot_pcp.out ${scrdir}/plot_pcp.sh
   else
      ${scrdir}/plot_pcp.sh 1>${tmpdir}/plot_pcp.out 2>&1 
   fi
fi
														  

### --------------------------------------------------------------
### step 5:
###   make fit-to-obs plots
### --------------------------------------------------------------
if [ $FIT2OBS = YES ] ; then
   # Create run dir with step name
   . ${myhome}/makeRunDir.sh step5

   #1. Input dir :
   # $gfsfitdir ( OPTIONAL, used if exp is fnl, set in setup_envs.sh)
   fitdir=${ENV_5_FITDIR}

   #2. Output  dir :
   #  $mapdir/fits ( set up above)

   #3. Running directory:
   export rundir=$tmpdir/fit

   #4. Case configuration
   export expdlist="$fitdir $fitdir"      # <== input 
   export expnlist=${ENV_5_EXPNLIST}
   export complist="$chost  $chost "     
   export endianlist=${ENV_5_ENDIANLIST}
   export cycle=${ENV_5_CYCLE}
   export oinc_f2o=${ENV_5_OINC_F2O}
   export finc_f2o=${ENV_5_FINC_F2O}
   export fmax_f2o=${ENV_5_FMAX_F2O}
   export DATEST=${ENV_5_DATEST}
   export DATEND=${ENV_5_DATEND}

   #5. fit2obs source directory:
   export scrdir=${vsdbhome}/fit2obs

   ${scrdir}/fit2obs.sh 1>${tmpdir}/fit2obs.out 2>&1 
fi


### --------------------------------------------------------------
### step 6:
###   make maps of lat-lon distributions and zonal-mean cross-sections
### --------------------------------------------------------------
if [ $MAPS2D = YES ] ; then
   # Create run dir with step name
   . ${myhome}/makeRunDir.sh step6

   #1. Input dir :
   # $gstat/gfs ( OPTIONAL, used if exp = gfs, set in setup_envs.sh)
   # $obdata ( REQUIRED data )
   export myarch=${ENV_6_MYARCH}

   #2. Output  dir :
   # $mapdir/2D 

   #3. Running directory 
   export rundir=$tmpdir/2dmaps

   #4. Case configuration
   export expdlist="$myarch  $myarch"     ### <== input dir
   export expnlist=${ENV_6_EXPNLIST}
   export complist="$chost  $chost " 
   export dumplist=${ENV_6_DUMPLIST}
   export fdlist=${ENV_6_FDLIST}
   export cycle=${ENV_6_CYCLE}
   export DATEST=${ENV_6_DATEST}
   export ndays=${ENV_6_NDAYS}
   export nlev=${ENV_6_NLEV}
   export grid=${ENV_6_GRID}
   export pbtm=${ENV_6_PBTM}
   export ptop=${ENV_6_PTOP}
   export latlon="-90 90 0 360"          ;#map area lat1, lat2, lon1 and lon2

   export listvara=machine,gstat,expnlist,expdlist,complist,dumplist,cycle,DATEST,ndays,nlev,grid,pbtm,ptop,latlon
   export listvarb=rundir,mapdir,obdata,webhost,webhostid,ftpdir,doftp,NWPROD,APRUN,vsdbhome,GRADSBIN
   export listvarc=SUBJOB,ACCOUNT,GROUP,CUE2RUN,CUE2FTP

   export odir=0
   for fcstday in $fdlist ; do
      export odir=`expr $odir + 1 `
      export fcst_day=$fcstday
      export listvar=$listvara,$listvarb,$listvarc,odir,fcst_day,fhlist$fcst_day
      if [ $batch = YES ]; then
	 $SUBJOB -e $listvar -a $ACCOUNT  -q $CUE2RUN -g $GROUP -p 1/1/N -r 4096/1 -t 6:00:00 \
	 -j map2d$odir -o $tmpdir/2dmaps${odir}.out  ${vsdbhome}/plot2d/maps2d_new.sh
      else
	 ${vsdbhome}/plot2d/maps2d_new.sh  1>${tmpdir}/2dmaps${odir}.out 2>&1 &
      fi
   done
fi                                       


exit

