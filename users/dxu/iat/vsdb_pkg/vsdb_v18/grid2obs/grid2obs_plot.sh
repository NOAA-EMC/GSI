#!/bin/ksh
set -x

#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#  Fanglin Yang, November 2011
#  This script make graphics using grid2obs verification statistics saved 
#  in vsdb format. It is aimed to compare 1) surface observations of T2m, 
#  RH2m amd wind at 10m over the Continental United Stats and the Alaska 
#  region, and 2) upper-air observations of T, Q, RH and Wind from rawinsonde, 
#  dropsonde, and profilers over the global and its subregions. 
#----------------------------------------------------------------------
#----------------------------------------------------------------------

export mdlist=${mdlist:-"gfs prexp"}                           ;#experiment names, up to 10
export cyclist=${cyclist:-"00" }                               ;#forecast cycle to be verified
export DATEST=${DATEST:-20130501}                              ;#forecast starting date
export DATEND=${DATEND:-20130801}                              ;#forecast ending date
export vlength=${vlength:-168}                                 ;#forecast length in hour
export fhoutair=${fhoutair:-${fhout:-6}}                       ;#forecast output frequency in hours for raobs vrfy
export fhoutsfc=${fhoutsfc:-${fhout:-6}}                       ;#forecast output frequency in hours for sfc vrfy
export obairtype=${obairtype:-ADPUPA}                          ;#uppair observation type, ADPUPA or ANYAIR
export plotair=${plotair:-"YES"}                               ;#make upper plots                         
export plotsfc=${plotsfc:-"YES"}                               ;#make sfc plots                         
export batch=${batch:-"YES"}                                   ;#run in batch mode                      


export vsdbhome=${vsdbhome:-/global/save/Fanglin.Yang/VRFY/vsdb}             ;#script home
export vsdbsave=${vsdbsave:-/global/save/$LOGNAME/vrfygfs/vsdb_data}         ;#where vsdb stats are saved
export opsvsdb=${opsvsdb:-/global/save/Fanglin.Yang/vrfygfs/vsdb_data}       ;#operational model  vsdb data base        
export webhost=${webhost:-"emcrzdm.ncep.noaa.gov"}                           ;#login id on webhost         
export webhostid=${webhostid:-"wx24fy"}                                      ;#login id on webhost         
export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$webhostid/vsdb}     ;#where maps are  displayed
export doftp=${doftp:-"YES"}                                                 ;#whether or not sent maps to ftpdir 
export rundir=${rundir:-/stmpd2/$LOGNAME/g2oplot$$}                            ;#temporary workplace
export locwebdir=${locwebdir:-$rundir/web}                                   ;#local web directory               
export mapdir=$locwebdir/g2o                                                 ;#place where maps are saved locally
export maskmiss=${maskmiss:-1}            ;#remove missing data from all models to unify sample size, 0-->NO, 1-->Yes
export copymap=${copymap:-"YES"}                                             ;#whether or not to copy maps to mapdir
export ACCOUNT=${ACCOUNT:-GFS-MTN}                                           ;#ibm computer ACCOUNT task
export CUE2RUN=${CUE2RUN:-dev_shared}                                        ;#dev or devhigh or 1
export CUE2FTP=${CUE2FTP:-transfer}                                          ;#data transfer queue
export GROUP=${GROUP:-g01}                                                   ;#account group     
export NWPROD=${NWPROD:-/global/save/Fanglin.Yang/VRFY/vsdb/nwprod}          ;#utilities and libs included in /nwprod
export SUBJOB=${SUBJOB:-$vsdbhome/bin/sub_wcoss}                             ;#script for submitting batch jobs
export FC=${FC:-/usrx/local/intel/composer_xe_2011_sp1.11.339/bin/intel64/ifort}  ;#compiler
export FFLAG=${FFLAG:-"-O2 -convert big_endian -FR"}                              ;#compiler options
export cputime=${cputime:-6:00:00}                                           ;#CPU time hh:mm:ss to run each batch job
export memory=${memory:-2048}
export share=${share:-N}
if [ $CUE2RUN = dev_shared ]; then export memory=1024; export share=S; fi
mkdir -p $rundir $locwebdir $mapdir

#--------------------------------------
#--------------------------------------
export sdate=$DATEST                                 ;#start of verification date
export edate=$DATEND                                 ;#end of verification date
export sorcdir=$vsdbhome/grid2obs
export vsdb_data=${vsdbsave}/grid2obs

y1=`echo $sdate |cut -c 1-4 `
m1=`echo $sdate |cut -c 5-6 `
d1=`echo $sdate |cut -c 7-8 `
y2=`echo $edate   |cut -c 1-4 `
m2=`echo $edate   |cut -c 5-6 `
d2=`echo $edate   |cut -c 7-8 `
ndays=`$vsdbhome/map_util/days.sh -a $y2 $m2 $d2 - $y1 $m1 $d1`
export ndays=`expr $ndays + 1 `
export vlength=$((vlength/24*24))

# ------------------------------------------------------------------------------
cd $locwebdir ||exit
tar xvf ${sorcdir}/g2o_webpage.tar     
if [ $doftp = "YES" -a $machine = WCOSS ]; then
ssh -q -l $webhostid ${webhost} " ls -l ${ftpdir}/g2o "
if [ $? -ne 0 ]; then
 ssh -q -l $webhostid ${webhost} " mkdir -p ${ftpdir}/g2o "
 scp -q ${sorcdir}/g2o_webpage.tar  ${webhostid}@${webhost}:${ftpdir}/.
 ssh -q -l $webhostid ${webhost} "cd ${ftpdir} ; tar -xvf g2o_webpage.tar "
 ssh -q -l $webhostid ${webhost} "rm ${ftpdir}/g2o_webpage.tar "
fi
fi
cd $rundir || exit 8
rm g2oair*.out g2osfc*.out

##make sympoblic link to operational grid2obs vsdb database in case operational scores are used for comparison
if [ $vsdbsave != "$opsvsdb" ]; then
 mkdir -p $vsdb_data/00Z  $vsdb_data/06Z $vsdb_data/12Z $vsdb_data/18Z
 ln -fs $opsvsdb/grid2obs/00Z/gfs $vsdb_data/00Z/gfs
 ln -fs $opsvsdb/grid2obs/06Z/gfs $vsdb_data/06Z/gfs
 ln -fs $opsvsdb/grid2obs/12Z/gfs $vsdb_data/12Z/gfs
 ln -fs $opsvsdb/grid2obs/18Z/gfs $vsdb_data/18Z/gfs
fi

export listvar1=edate,ndays,vsdb_data,sorcdir,ftpdir,doftp,mapdir,copymap,webhost,webhostid,maskmiss,vlength
export listvar2=SUBJOB,ACCOUNT,CUE2RUN,CUE2FTP,GROUP,FC,FFLAG,NWPROD
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#  make plots of fit2obs surface varibales (T2m, RH2m, 10-m Wind Speed)

if [ $plotsfc = YES ]; then
export levlist="SFC"             
export vnamlist="T RH VWND DPT TCLD SLP"             
bigregion="west east NA MN SSSG LGS NN SPL AK"

for regname in $bigregion ; do
  case $regname in
    west)  export reglist="NWC SWC GRB NMT SMT SWD NPL SPL"   
           export regdef="CONUS West"                     ;;
    east)  export reglist="APL NEC SEC MDW LMV GMC"           
           export regdef="CONUS East"                     ;;
      NA)  export reglist="NEC APL"                           
           export regdef="CONUS Northeast"                 ;;
      MN)  export reglist="MDW NPL"                           
           export regdef="N. Plains and Mid-West"  ;;
    SSSG)  export reglist="SWC SWD SMT GRB"                   
           export regdef="CONUS Southwest"                 ;;
     LGS)  export reglist="LMV GMC SEC"                       
           export regdef="CONUS Southeast"                 ;;
      NN)  export reglist="NWC NMT"                           
           export regdef="CONUS Northwest"                 ;;
     SPL)  export reglist="SPL"                               
           export regdef="S. Plains"            ;;
      AK)  export reglist="NAK SAK"                           
           export regdef="Alaska"                          ;;
       *)  echo " $regname not defined in $0 "; exit ;;
  esac  

  export fhout=$fhoutsfc
  export regname=$regname
  export tmpdir=$rundir/sfc/$regname                   
  mkdir -p $tmpdir
  export listvar=$listvar1,$listvar2,mdlist,cyclist,levlist,vnamlist,reglist,regdef,tmpdir,regname,fhout
 if [ $batch = YES ]; then
  $SUBJOB -e listvar,$listvar -a $ACCOUNT  -q $CUE2RUN -g $GROUP -p 1/1/$share -r $memory/1 -t $cputime -j g2osfc$regname -o $rundir/g2osfc${regname}.out  ${sorcdir}/scripts/g2o_sfcmap.sh $regname
  if [ $? -ne 0 ]; then ${sorcdir}/scripts/g2o_sfcmap.sh $regname ; fi
 else
  ${sorcdir}/scripts/g2o_sfcmap.sh $regname  &
 fi
done
fi

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#  make plots of fit2obs upper air varibales (T, RH, Vector Wind)

if [ $plotair = YES ]; then
export vnamlist="T Q VWND RH"
export levlist="P1000 P925 P850 P700 P500 P400 P300 P250 P200 P150 P100 P50"             
#export levlist="P1000 P925 P850 P700 P500 P400 P300 P250 P200 P150 P100 P50 P20 P10"             

#regioncode="gglb g236 gnh gsh gtrp geur gasi gafr gsa gna gaus"
regioncode="gglb g236 gnh gsh gtrp"
for regname in $regioncode ; do
  case $regname in
    gglb)  export reglist="GGLB"                              
           export regdef="Globe"      ;;
    g236)  export reglist="G236"                              
           export regdef="CONUS"      ;;
     gnh)  export reglist="GNH"                               
           export regdef="NH"         ;;
     gsh)  export reglist="GSH"                               
           export regdef="SH"         ;;
    gtrp)  export reglist="GTRP"                               
           export regdef="Tropics"    ;;
    geur)  export reglist="GEUR"                               
           export regdef="Europe"     ;;
    gasi)  export reglist="GASI"                               
           export regdef="Asia"     ;;
    gafr)  export reglist="GAFR"                               
           export regdef="Africa"     ;;
     gna)  export reglist="GNA"                                
           export regdef="N. America"  ;;
     gsa)  export reglist="GSA"                                
           export regdef="S. America"  ;;
    gaus)  export reglist="GAUS"                                
           export regdef="Australia"  ;;
       *)  echo " $regname not defined in $0 "; exit ;;
  esac  

  export fhout=$fhoutair
  export regname=$regname
  export tmpdir=$rundir/air/$regname                   
  mkdir -p $tmpdir
  export listvar=$listvar1,$listvar2,mdlist,cyclist,levlist,vnamlist,reglist,regdef,tmpdir,regname,fhout,obairtype
 if [ $batch = YES ]; then
  $SUBJOB -e listvar,$listvar -a $ACCOUNT  -q $CUE2RUN -g $GROUP -p 1/1/$share -r $memory/1 -t $cputime -j g2oair$regname -o $rundir/g2oair${regname}.out  ${sorcdir}/scripts/g2o_airmap.sh $regname
  if [ $? -ne 0 ]; then ${sorcdir}/scripts/g2o_airmap.sh $regname ; fi
 else
  ${sorcdir}/scripts/g2o_airmap.sh $regname &
 fi
#---------------
done  ;#regname
#---------------
fi

exit




