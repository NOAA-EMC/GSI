#!/bin/ksh
set -x

##---------------------------------------------------------------------------------
## grid-to-obs driver for submitting jobs to create g2o 
## stats, to make graphics, and to upload to web servers.
## Fanglin Yang, NCEP/EMC, August 2013
##----------------------------------------------------------------------------------
## notes: 
##  1) creating stats is a slow process and has to be run in batch mode. 
##     it takes 10 to 20 minutes on WCOSS to finish one forecast case. 
##  2) It takes grib files as input.  The files should contain near surface 
##     variables and upper-air variables on isobaric layers. They can be
##     at any given forecast output frequency and on any regular lat-lon gird.
##  3) The verification includes both surface and upper air observations. 
##     It is aimed to compare forecasts against a) ground observations of SLP,
##     T2m, RH2m and wind speed at 10m over CONUS and its subregions and, 
##     b) upper-air observations of T, wind, RH and Q from ADPUPA (rawinsonde, 
##     pibals and profilers) and AIRCAR (ACARS) over the globle, NH, SH and 
##     the tropics.  
##  4) The script uses nam/ndas prepbufr for surafce fits over the NAM(G104)
##     subregions and gdas/gfs prepbufr for upper-air fit over global subregions.
##  5) Perry Shafran provided the operational version of the gridtobs Fortran 
##     code. Helin Wei provided his scripts for producing surface stats. Fanglin 
##     Yang added options to the Fortran code to run verification over larger 
##     global domains, and wrote the shell and GrADS scripts for producing 
##     upper-air stats and for making graphics. This toll is applicable for 
##     processing GFS operational and parallel forecasts for all forecast cycles. 
##---------------------------------------------------------------------------------

G2OSTATS=YES      ;#for making verification stats
G2OPLOTS=NO       ;#for making graphics, set to YES after G2OSTAT finishes

export machine=WCOSS                                      ;#ZEUS or WCOSS

chost=`echo $(hostname)|cut -c 1-1`
if [ $machine = WCOSS ]; then
export NOSCRUB=/global/noscrub          ;#noscrub directory                 
export vsdbsave=$NOSCRUB/$LOGNAME/archive/vsdb_data         ;#place where vsdb database is saved
export opsvsdb=/global/save/Fanglin.Yang/vrfygfs/vsdb_data  ;#operational model grid-to-obs data base
export vsdbhome=/global/save/Fanglin.Yang/VRFY/vsdb         ;#verify source code and scripts
export gdas_prepbufr_arch=/global/noscrub/Fanglin.Yang/prepbufr/gdas ;#ops gdas prepbufr archive
export NWPROD=$vsdbhome/nwprod                              ;#utilities in nwprod
export ACCOUNT=GFS-MTN                                      ;#ibm computer ACCOUNT task
export CUE2RUN=dev                                          ;#account type (dev, devhigh, or 1) to run 
export CUE2FTP=transfer                                     ;#account for data transfer                 
export GROUP=g01                                            ;#account group
export HPSSTAR=/u/Fanglin.Yang/bin/hpsstar                  ;#hpsstar                              
export SUBJOB=$vsdbhome/bin/sub_wcoss                       ;#script for submitting batch jobs
export rundir=/stmpd2/$LOGNAME/g2o                            ;#running directory
export FC=/usrx/local/intel/composer_xe_2011_sp1.11.339/bin/intel64/ifort ;#fortran compiler

elif [ $machine = ZEUS ]; then
export NOSCRUB=/scratch2/portfolios/NCEPDEV/global/noscrub  ;#noscrub directory                 
export vsdbsave=$NOSCRUB/$LOGNAME/archive/vsdb_data         ;#place where vsdb database is saved
export opsvsdb=/scratch2/portfolios/NCEPDEV/global/noscrub/stat/vsdb_data ;#operational model grid-to-obs data base
export vsdbhome=/scratch2/portfolios/NCEPDEV/global/save/Fanglin.Yang/VRFY/vsdb  ;#verify source code and scripts
export gdas_prepbufr_arch=/scratch2/portfolios/NCEPDEV/global/noscrub/stat/prepbufr/gdas ;#ops gdas prepbufr archive
export NWPROD=$vsdbhome/nwprod                              ;#utilities in nwprod
export ACCOUNT=glbss                                        ;#computer ACCOUNT task
export CUE2RUN=batch                                        ;#account type (dev, devhigh, or 1) to run 
export CUE2FTP=service                                      ;#account for data transfer                 
export GROUP=g01                                            ;#account group
export SUBJOB=$vsdbhome/bin/sub_zeus                        ;#script for submitting batch jobs
export HPSSTAR=/home/Fanglin.Yang/bin/hpsstar_zeus          ;#hpsstar                              
export rundir=/scratch2/portfolios/NCEPDEV/stmp/$LOGNAME/g2o  ;#running directory
export FC=/apps/intel/composerxe-2011.4.191/composerxe-2011.4.191/bin/intel64/ifort ;#fortran compiler
fi
export memory=10240; export share=N
if [ $CUE2RUN = dev_shared ]; then export memory=1024; export share=S; fi
mkdir -p $rundir


#============================
#---produce g2o vsdb database
if [ ${G2OSTATS:-NO} = YES ]; then
#============================
export cyclist="00 06 12 18"                    ;#forecast cycles
export expnlist="prexp1 prexp2"                 ;#experiment names
export expdlist="$NOSCRUB/$LOGNAME/archive $NOSCRUB/$LOGNAME/archive"
export hpssdirlist="/NCEPDEV/hpssuser/g01/wx20rt/WCOSS /NCEPDEV/1year/hpsspara/runhistory/glopara"
export dumplist=".gfs. .gfs."  
export fhoutair="6"                         ;#forecast output frequency in hours for raobs vrfy
export fhoutsfc="3"                         ;#forecast output frequency in hours for sfc vrfy
export gdtype="3"                           ;#pgb file resolution, 2 for 2.5-deg and 3 for 1-deg
export vsdbsfc="YES"                        ;#run sfc verification
export vsdbair="YES"                        ;#run upper-air verification
export DATEST=20130701                      ;#verification starting date
export DATEND=20130801                      ;#verification ending date
export batch=YES                            ;#to run jobs in batch mode

listvar1=vsdbhome,vsdbsave,cyclist,expnlist,expdlist,hpssdirlist,dumplist,fhoutair,fhoutsfc,,vsdbsfc,vsdbair,gdtype
listvar2=NWPROD,SUBJOB,ACCOUNT,CUE2RUN,CUE2FTP,GROUP,DATEST,DATEND,rundir,HPSSTAR,gdas_prepbufr_arch,batch
export listvar=$listvar1,$listvar2
JJOB=${vsdbhome}/grid2obs/grid2obs.sh
if [ $batch = YES ]; then
 $SUBJOB -e listvar,$listvar -a $ACCOUNT  -q $CUE2RUN -g $GROUP -p 1/1/$share -r $memory/1 \
        -t 6:00:00 -j g2ogfs -o $rundir/g2ogfs.out $JJOB 
else
 $JJOB 1> $rundir/g2ogfs.out 2>&1 
fi
#-------
fi
#-------

#============================
# make g2o maps
if [ ${G2OPLOTS:-NO} = YES ]; then
#===========================

export mdlist="gfs prexp1 prexp2"          ;#experiment names, up to 10
export cyclist="00"                        ;#forecast cycles to verify
export vlength=168                         ;#forecast length in hour
export fhoutair="6"                        ;#forecast output frequency in hours for raobs vrfy
export fhoutsfc="3"                        ;#forecast output frequency in hours for sfc vrfy
export DATEST=20130701                     ;#verification starting date
export DATEND=20130801                     ;#verification ending date
export maskmiss=1                          ;#remove missing data from all runs, 0-->NO, 1-->Yes
export obairtype=ADPUPA                    ;#uppair observation type, ADPUPA or ANYAIR
export plotair="YES"                        ;#make upper plots
export plotsfc="YES"                       ;#make sfc plots


export webhost=emcrzdm.ncep.noaa.gov       ;#host for web display
export webhostid=$LOGNAME                  ;#login id on webhost
export ftpdir=/home/people/emc/www/htdocs/gmb/$webhostid/vsdb ;#where maps are displayed on webhost
export doftp="YES"                                            ;#whether or not sent maps to ftpdir

sh +x ${vsdbhome}/grid2obs/grid2obs_plot.sh

#-------
fi
#-------

exit


