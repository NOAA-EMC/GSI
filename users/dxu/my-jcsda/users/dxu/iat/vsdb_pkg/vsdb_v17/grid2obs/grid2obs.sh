#!/bin/ksh
set -x

##----for CCS batch job------------------------
###@ account_no = GFS-MTN
###@ class = 1  
###@ output = g2o.out
###@ error = g2o.out
###@ job_type = serial
###@ job_name = g2o
###@ parallel_threads=1
###@ task_affinity = core(1)
###@ wall_clock_limit = 03:00:00
###@ node_resources = ConsumableMemory(106 GB)
###@ node_usage = not_shared
###@ queue
##------------------------------------------

##-----for WCOSS batch job-----------------
#BSUB -a poe
#BSUB -e g2o.out
#BSUB -o g2o.out
#BSUB -J g2o
#BSUB -network type=sn_all:mode=US
#BSUB -q dev
#BSUB -n 1
#BSUB -R span[ptile=1]
#BSUB -R affinity[core]
#BSUB -x
#BSUB -W 6:00
export MP_EUIDEVELOP=min
export KMP_STACKSIZE=2048m
export MPICH_ALLTOALL_THROTTLE=0
export MP_SINGLE_THREAD=yes
export MP_SHARED_MEMORY=no
export MP_MPILIB=mpich2
export MP_LABELIO=yes
export MP_STDOUTMODE=ordered
##-----------------------------------------


#----------------------------------------------------------------
# November 2011, Fanglin Yang
# 1. This script creates grid2obs verification statistics in vsdb format, including 
#    both surface and upper air observations. It is aimed to 1) compare the diurnal 
#    cycles of area-mean forecasts and  ground observations of SLP, T2m, RH2m and 
#    wind speed at 10m over CONUS and its subregions and, 2) compare area-mean 
#    vertical distributions of bias and RMSE of upper air ADPUPA (e.g. rawinsonde, 
#    pibals and profilers) and AIRCAR (ACARS) observations.  
#    Helin Wei provided his scripts for producing surface stats.  Perry Shafran 
#    provided the operational version of the gridtobs Fortran code. Fanglin Yang 
#    rewrote the scripts to make them applicable for processing restropestive
#    experiments for all forecast cycles with any given output frequency.
# 2. The script uses nam/ndas prepbufr for surafce fits over the NAM(G104) 
#    subregions and gdas/gfs prepbufr for upper-air fit over global subregions.
# 3. This script must be run as a batch job because verf_gridtobs_prepfits demands
#    large ConsumableMemory.
#----------------------------------------------------------------
#
export listvar=${listvar:-""}
export vsdbsave=${vsdbsave:-/stmpd2/$LOGNAME/vsdb_data}                      ;#place where vsdb database is saved
#export vsdbsave=${vsdbsave:-/global/save/$LOGNAME/vrfygfs/vsdb_data}      ;#place where vsdb database is saved
export vsdbhome=${vsdbhome:-/global/save/Fanglin.Yang/VRFY/vsdb}           ;#script home

export cyclist=${cyclist:-"00 06 12 18"}                               ;#forecast cycles 
export expnlist=${expnlist:-"gfs prd12q3k"}                             ;#experiment names
export expdlist=${expdlist:-"/global/hires/glopara/archive /global/hires/glopara/archive"}   ;#experiment data directory
export hpssdirlist=${hpssdirlist:-"/NCEPDEV/1year/hpsspara/runhistory/glopara /NCEPDEV/1year/hpsspara/runhistory/glopara"}  ;#hpss archive directory
export complist=${complist:-"tide tide"}                               ;#computers where experiments are run
export dumplist=${dumplist:-".gfs. .gfs."}                             ;#file format pgb${fsub}${fhr}${dump}${yyyymmdd}${cyc}

export DATEST=${1:-${DATEST:-20111129}}                                ;#verification starting date
export DATEND=${2:-${DATEND:-20111210}}                                ;#verification ending date
export vlength=${vlength:-168}                                         ;#verification length in hours
export vsdbair=${vsdbair:-"YES"}                                       ;#run upperair raobs verification
export vsdbsfc=${vsdbsfc:-"YES"}                                       ;#run sfc verification
export fhoutair=${fhoutair:-${fhout:-6}}                               ;#forecast output frequency in hours for raobs vrfy
export fhoutsfc=${fhoutsfc:-${fhout:-6}}                               ;#forecast output frequency in hours for sfc vrfy

export gdtype=${gdtype:-3}                                             ;#fcst data resolution, 2-2.5deg, 3-1deg., 4-0.5deg
export fsub=${fsub:-f}                                                 ;# string in pgb fcst file after pg
export scppgb=${scppgb:-"NO"}                                          ;#copy pgb files from other machine?
export NWPROD=${NWPROD:-/global/save/Fanglin.Yang/VRFY/vsdb/nwprod}
export SUBJOB=${SUBJOB:-$vsdbhome/bin/sub_wcoss}                       ;#script for submitting batch jobs
export ndate=${ndate:-$NWPROD/util/exec/ndate}
export rundir=${rundir:-/stmpd2/$LOGNAME/g2o}
export ACCOUNT=${ACCOUNT:-GFS-MTN}                                     ;#ibm computer ACCOUNT task
export CUE2RUN=${CUE2RUN:-dev}                                         ;#dev or devhigh or 1
export CUE2FTP=${CUE2FTP:-transfer}                                    ;#queue for data transfer
export GROUP=${GROUP:-g01}                                             ;#account group
export HPSSTAR=${HPSSTAR:-/u/Fanglin.Yang/bin/hpsstar}                 ;#account group
export memory=${memory:-2048}
export share=${share:-N}
if [ $CUE2RUN = dev_shared ]; then export memory=1024; export share=S; fi
export gdas_prepbufr_arch=${gdas_prepbufr_arch:-/global/noscrub/Fanglin.Yang/prepbufr/gdas} ;#ops gdas prepbufr archive

export grid2obshome=${vsdbhome}/grid2obs  
export g2osave=${vsdbsave}/grid2obs
#----------------------------------------------------------------
#----------------------------------------------------------------
export batch=${batch:-YES}

NDAT=${NDAT:-0}     ;#number of days to verify for each batch job
export DAYBEG=${DAYBEG:-$DATEST}
if [ $batch = YES ]; then
 export DAYEND=`echo $($ndate +$(expr ${NDAT} \* 24) ${DAYBEG}00 ) |cut -c 1-8`
else
 export DAYEND=$DATEND
fi  

#-- specify verification hours in a day 
if [ $fhoutair -eq 12 ]; then
 export vhlistair="00 12"
elif [ $fhoutair -eq 6 ]; then
 export vhlistair="00 06 12 18"
elif [ $fhoutair -eq 3 ]; then
 export vhlistair="00 03 06 09 12 15 18 21"
else
 echo " fhoutair=$fhoutair hours is not supported"
 export vsdbair="NO"
fi
if [ $fhoutsfc -eq 12 ]; then
 export vhlistsfc="00 12"
elif [ $fhoutsfc -eq 6 ]; then
 export vhlistsfc="00 06 12 18"
elif [ $fhoutsfc -eq 3 ]; then
 export vhlistsfc="00 03 06 09 12 15 18 21"
else
 echo " fhoutsfc=$fhoutsfc hours is not supported"
 export vsdbsfc="NO"
fi
if [ $vsdbsfc = "NO" -a $vsdbair = "NO" ]; then exit; fi
fhout=$fhoutsfc
if [ $fhoutair -lt $fhoutsfc ]; then fhout=$fhoutair; fi


vlength=$((vlength/24*24))
myhost=`echo $(hostname) |cut -c 1-1 `
nexp=`echo $expnlist |wc -w`
n=0 ; for runn in $expnlist ; do n=$((n+1)) ; expname[n]=$runn ; done
n=0 ; for rund in $expdlist ; do n=$((n+1)) ; expdir[n]=$rund  ; done
n=0 ; for comp in $complist ; do n=$((n+1)) ; compname[n]=$comp  ; done
n=0 ; for dump in $dumplist ; do n=$((n+1)) ; dumpname[n]=$dump  ; done
n=0 ; for hpssd in $hpssdirlist ; do n=$((n+1)) ; hpssname[n]=$hpssd  ; done
echo ${expname[n]} ${expdir[n]} ${compname[n]} ${dumpname[n]}


#----------------------------------
nn=1; while [ $nn -le $nexp ] ; do
for cyc in $cyclist; do
#----------------------------------
export comrot=$rundir/${cyc}Z
mkdir -p $comrot ; cd $comrot || exit 8 

# ---- prepare forecast data, and gdas prepbufr data ----
  export exp=${expname[nn]}                        ;#exp name
         expcap=`echo $exp |tr "[a-z]" "[A-Z]" `   ;#name in capital 
  export exp_dir=${expdir[nn]}/${exp}              ;#exp directory
  export CLIENT=${compname[nn]}                    ;#computer used
  export cdump=${dumpname[nn]:-".gfs."}            ;#file dump format
  export hpssdir=${hpssname[nn]:-/NCEPDEV/1year/hpsspara/runhistory/glopara}  ;#tape archive directory
  myclient=`echo $CLIENT |cut -c 1-1 `
  if [ $myhost = $myclient ]; then
   if [ $myhost = "t" ]; then export CLIENT="gyre" ;fi
   if [ $myhost = "g" ]; then export CLIENT="tide" ;fi
  fi
  comout=$comrot/$exp
  mkdir -p $comout; cd $comout || exit 8
  #----------------------------------------------------------
  # copy forecast data to temporary directory following standard name convention
   vlength24=`expr $vlength + 24 `
   loop=$($ndate -$vlength24 ${DAYBEG}${cyc} )
   while [ $loop -le ${DAYEND}${cyc} ] ; do
    IDAY=`echo $loop |cut -c 1-8`
    fcyc=`echo $loop |cut -c 9-10`
    if [  ! -s pgbf${vlength}.${exp}.${loop} ]; then
      if [ $exp = gfs ]; then
        $grid2obshome/scripts/get_opsgfs_data.sh $comout $exp $loop $vlength $fhout "$vhlistair"
      else 
        $grid2obshome/scripts/get_paragfs_data.sh $comout $exp $loop $vlength $fhout "$vhlistair"
      fi
    fi
    loop=$($ndate +24 $loop)
   done


# ---- Create VSDB database ----
cd $comrot
CDATE=${DAYBEG}
while [ $CDATE -le ${DAYEND} ]; do


#--surface fits to obs over NAM(G104) area
if [ $vsdbsfc = "YES" ]; then
 for vhour in $vhlistsfc ; do
   export vdate=${CDATE}${vhour}
   $grid2obshome/scripts/grid2obssfc.fits.sh ${exp} $vdate $vlength $cyc $comrot ${comrot}/${exp} 
 done

 cat ${exp}_sfc_${CDATE}*.vdb >${exp}_sfc_${CDATE}.vsdb
 sed s/CTLF/"$expcap"/g ${exp}_sfc_${CDATE}.vsdb > temp
 mv temp ${exp}_sfc_${CDATE}.vsdb
 mkdir -p ${g2osave}/${cyc}Z/$exp
 cp -p ${exp}_sfc_${CDATE}.vsdb ${g2osave}/${cyc}Z/$exp/.                    
fi


#--upper-air fits to obs over the globe        
if [ $vsdbair = "YES" ]; then
 for vhour in $vhlistair ; do
   export vdate=${CDATE}${vhour}
   $grid2obshome/scripts/grid2obsair.fits.sh ${exp} $vdate $vlength $cyc $comrot ${comrot}/${exp} 
 done

 cat ${exp}_air_${CDATE}*.vdb >${exp}_air_${CDATE}.vsdb
 sed s/CTLF/"$expcap"/g ${exp}_air_${CDATE}.vsdb > temp
 mv temp ${exp}_air_${CDATE}.vsdb
 mkdir -p ${g2osave}/${cyc}Z/$exp
 cp -p ${exp}_air_${CDATE}.vsdb ${g2osave}/${cyc}Z/$exp/.                    
fi


 CDATE=$(echo $($ndate +24 ${CDATE}00 ) |cut -c 1-8 )
done

#----------------------------------
done    ;#end of cyclist
 nn=`expr $nn + 1 `
done    ;#end of explist
#----------------------------------


#--submit next batch job for next NDAT days of verification
cd  $grid2obshome
export DAYBEG=$(echo $($ndate +24 ${DAYEND}00 ) |cut -c 1-8 )
if [ $DAYBEG -le $DATEND ]; then
 jobname=g2o${expname[1]}${DAYBEG}            
 jobout=$rundir/g2o${expname[1]}${DAYBEG}.out
 $SUBJOB -e DAYBEG,DATEND,listvar,$listvar -a $ACCOUNT -q $CUE2RUN -g $GROUP -p 1/1/$share  -r $memory/1 -t 6:00:00  -j $jobname -o $jobout $0   
fi

exit


