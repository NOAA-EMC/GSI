#!/bin/ksh
set -x

##--find and/or create siganl and sigges in binary format
##--Fanglin Yang, March 2015
date

export cyclist=${cyclist:-"00 06 12 18"}    ;#forecast cycles to verify
export exp=${exp:-"gfs"}                    ;#exp name
export expdir=${expdir:-"/global/noscrub/emc.glopara/global"}
##export hpssdir=${hpssdir:-"/5year/NCEPDEV/emc-global/$LOGNAME/WCOSS"}
export hpssdir=${hpssdir:-"/NCEPPROD/hpssprod/runhistory"}
export dump=${dump:-".gdas."}               ;#file format siganl${dum}${cdate} and sigges${dump}$cdate
export comp=${comp:-tide}                   ;#computers where exp data are archived
export CDATE1=${CDATE1:-20141201}           ;#starting verifying date 
export CDATE2=${CDATE2:-20141206}           ;#ending verifying date
export JCAP_bin=${JCAP_bin:-254}            ;#binary file res
export nlev=${nlev:-64}                     ;#sig file vertical layers, fixed for 64-L GFS

export mkctl=${mkctl:-YES}                  ;#whether or not to create grads ctl file      
export DATEST=${DATEST:-$CDATE1}            ;#starting date for grads ctl file
export ndays=${ndays:-6}                    ;#total number of days 

export vsdbhome=${vsdbhome:-/global/save/Fanglin.Yang/VRFY/vsdb}
export machine=${machine:-WCOSS}     
export NWPROD=${NWPROD:-$vsdbhome/nwprod}     
export ndate=${ndate:-$NWPROD/util/exec/ndate}
export s2g=${s2g:-$NWPROD/util/exec/ss2ggx}
export SUBJOB=${SUBJOB:-$vsdbhome/bin/sub_wcoss}
export ACCOUNT=${ACCOUNT:-GFS-MTN}
export CUE2RUN=${CUE2RUN:-dev}
export CUE2FTP=${CUE2FTP:-transfer}
export GROUP=${GROUP:-g01}
export batch=${batch:-YES}
if [ $machine = WCOSS ]; then
 export HPSSTAR0=/nwprod/util/ush/hpsstar
elif [ $machine = ZEUS ]; then
 export HPSSTAR0=/home/Fanglin.Yang/bin/hpsstar_zeus
fi
export HPSSTAR=${HPSSTAR:-$HPSSTAR0}



export OMP_NUM_THREADS=4
export MP_SHARED_MEMORY=no
export KMP_STACKSIZE=2048m
#---------------------------------------------------------------------------------

if [ $JCAP_bin = 1534 ]; then
 lonb=3072; latb=1536; dx=0.117188  ; dy=0.117264
elif [ $JCAP_bin = 1148 ]; then
 lonb=2304; latb=1152; dx=0.156250  ; dy=0.156386
elif [ $JCAP_bin = 574 ]; then
 lonb=1760; latb=880;  dx=0.204545  ; dy=0.204778
elif [ $JCAP_bin = 382 ]; then
 lonb=1152; latb=576;  dx=0.312500  ; dy=0.313043
elif [ $JCAP_bin = 254 ]; then
 lonb=768;  latb=384;  dx=0.468750  ; dy=0.469974
elif [ $JCAP_bin = 126 ]; then
 lonb=384;  latb=190;  dx=0.937500  ; dy=0.952381
elif [ $JCAP_bin = 62 ]; then
 lonb=192;  latb=94;   dx=1.875000  ; dy=1.935484
else
 echo " JCAP_bin=$JCAP_bin not supported, exit"
 exit 
fi
idrt=0  ;##0->lat-lon grid

rundir=${rundir:-/ptmpd2/$LOGNAME/2dmapssig}
ctldir=${ctldir:-$rundir/ctl}
if [ ! -s $rundir ]; then mkdir -p $rundir ; fi
if [ ! -s $ctldir ]; then mkdir -p $ctldir ; fi
cd ${rundir} || exit 8


#------------------------------
CLIENT=$comp              
myhost=`echo $(hostname) |cut -c 1-1 `
myclient=`echo $CLIENT |cut -c 1-1 `

export datadir=$rundir/$exp
if [ ! -s $datadir ]; then mkdir -p $datadir ;fi
cd $datadir ||exit 8

#/////////////////////////////////
for cyc in $cyclist; do
#/////////////////////////////////

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
sdate=${CDATE1}${cyc}
edate=${CDATE2}${cyc}             
while [ $sdate -le $edate ]; do
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 indir=${expdir}/${exp}
 ina=siganl$dump$sdate
 ing=sigges$dump$sdate
 oua=siganl$dump${sdate}.${lonb}.${latb}.bin
 oug=sigges$dump${sdate}.${lonb}.${latb}.bin

 #----------------------------
 ##first check online archives
 #----------------------------
 if [ $myhost = $myclient -o $machine != WCOSS ]; then
  if [ -s $indir/$oua ]; then
   ln -fs $indir/$oua .
  else
   if [ -s $indir/$ina ]; then $s2g $indir/$ina $oua tmpa.ctl $idrt $lonb $latb ;fi 
  fi
  if [ -s $indir/$oug ]; then
   ln -fs $indir/$oug .
  else
   if [ -s $indir/$ing ]; then $s2g $indir/$ing $oug tmpg.ctl $idrt $lonb $latb ;fi 
  fi
 elif [ $machine = WCOSS ]; then
  scp -pB ${LOGNAME}@${CLIENT}:$indir/$oua $oua          
  if [ $? -ne 0 ]; then 
   scp -pB ${LOGNAME}@${CLIENT}:$indir/$ina $ina          
   if [ $? -eq 0 ]; then $s2g $ina $oua tmpa.ctl $idrt $lonb $latb ;fi 
  fi
  scp -pB ${LOGNAME}@${CLIENT}:$indir/$oug $oug          
  if [ $? -ne 0 ]; then 
   scp -pB ${LOGNAME}@${CLIENT}:$indir/$ing $ing          
   if [ $? -eq 0 ]; then $s2g $ing $oug tmpg.ctl $idrt $lonb $latb ;fi 
  fi
 fi

 #-----------------------------------------------------------
 #if oua and oug still not produced , check hpss archive
 if [ ! -s $oua ]; then 
 #-----------------------------------------------------------
  if [ $dump = .gfs.  ]; then DMPA=gfs;  DMPB=gfs   ; fi
  if [ $dump = .gdas. ]; then DMPA=gdas; DMPB=gdas1 ; fi

  #....operational GFS.........
  if [ $exp = gfs ]; then
  #............................
   yyyy=`echo $sdate | cut -c1-4`
   mm=`  echo $sdate | cut -c5-6`
   dd=`  echo $sdate | cut -c7-8`
   tmpdir=$datadir/hpssget$$; mkdir -p $tmpdir
   rm -f hpssget$$.sh 
   hpssfile=$hpssdir/rh${yyyy}/${yyyy}${mm}/${yyyy}${mm}${dd}/com_gfs_prod_${DMPA}.${sdate}.tar 
cat <<EOF1 >hpssget$$.sh
   cd $tmpdir
   rm -f ${DMPB}.t${cyc}z.sanl  ${DMPB}.t${cyc}z.sgesprep
   $HPSSTAR get $hpssfile ./${DMPB}.t${cyc}z.sanl ./${DMPB}.t${cyc}z.sgesprep
   mv ${DMPB}.t${cyc}z.sanl     $datadir/$ina
   mv ${DMPB}.t${cyc}z.sgesprep $datadir/$ing
EOF1
   chmod a+x hpssget$$.sh
   if [ $batch = NO ]; then 
    ./hpssget$$.sh 
   else
    $SUBJOB -a $ACCOUNT -q $CUE2FTP -g $GROUP -p 1/1/S -t 0:30:00 -j hpssget$$ -o hpssget$$.out $datadir/hpssget$$.sh 
   fi

  #.....EMC GFS PARALLEL.......
  else
  #............................
   rm -f hpssget$$.sh
cat <<EOF2 >hpssget$$.sh
   cd $datadir
   $HPSSTAR get $hpssdir/$exp/${sdate}${DMPA}.tar $ina $ing
EOF2
   chmod a+x hpssget$$.sh
   if [ $batch = NO ]; then 
    ./hpssget$$.sh 
   else
    $SUBJOB -a $ACCOUNT -q $CUE2FTP -g $GROUP -p 1/1/S -t 0:30:00 -j hpssget$$ -o hpssget$$.out $datadir/hpssget$$.sh 
   fi
  #............................
  fi
  #............................

  ##wait for 31 minutes for HPSSTAR job to complete
  nsleep=0; tsleep=30;  msleep=61 
  while test ! -s $ina -a $nsleep -lt $msleep;do
   sleep $tsleep; nsleep=`expr $nsleep + 1`
  done
  if [ -s $ina ]; then $s2g $ina $oua tmpa.ctl $idrt $lonb $latb ;fi 
  if [ -s $ing ]; then $s2g $ing $oug tmpg.ctl $idrt $lonb $latb ;fi 
  cp -p $oua $expdir/$exp/.
  cp -p $oug $expdir/$exp/.
 #-----------------------------------------------------------
 fi
 #-----------------------------------------------------------
  
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 sdate=`$ndate +24 $sdate`
done
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#/////////////////////////////////
done  ;# cycle
#/////////////////////////////////



#-----------------------------------
##create grads ctl file
if [ $mkctl = YES ]; then
#-----------------------------------
set -A cycname  $cyclist
set -A mlist none Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
yeara=`echo $DATEST |cut -c 1-4`
mona=`echo $DATEST |cut -c 5-6`
daya=`echo $DATEST |cut -c 7-8`
cyca=${cycname[0]}
mona=${mlist[$mona]}

ncyc=`echo $cyclist |wc -w`
int=`expr 24 \/ $ncyc `
nc=`expr $ndays \* $ncyc `

cat >$ctldir/${exp}_anl.ctl <<EOF
dset ${datadir}/siganl${dump}%y4%m2%d2%h2.${lonb}.${latb}.bin   
options yrev
format template
undef -9.99E+33
xdef  $lonb linear    0.000000    $dx           
ydef  $latb linear  -90.000000    $dy             
zdef  $nlev linear 1 1
tdef $nc linear ${cyca}Z${daya}${mona}${yeara} ${int}hr
vars    8 
PS    1    99 surface pressure (Pa)
T    $nlev 99 temperature (K)
Q    $nlev 99 specific humidity (kg/kg)
RH   $nlev 99 relative humidity 
U    $nlev 99 zonal wind (m/s)
V    $nlev 99 meridional wind (m/s)
O3   $nlev 99 Ozone (kg/kg)
CLW  $nlev 99 Cloud Water (kg/kg)
endvars
EOF


cat >$ctldir/${exp}_ges.ctl <<EOF
dset ${datadir}/sigges${dump}%y4%m2%d2%h2.${lonb}.${latb}.bin   
options yrev
format template
undef -9.99E+33
xdef  $lonb linear    0.000000    $dx           
ydef  $latb linear  -90.000000    $dy             
zdef  $nlev linear 1 1
tdef $nc linear ${cyca}Z${daya}${mona}${yeara} ${int}hr
vars    8 
PS    1    99 surface pressure (Pa)
T    $nlev 99 temperature (K)
Q    $nlev 99 specific humidity (kg/kg)
RH   $nlev 99 relative humidity 
U    $nlev 99 zonal wind (m/s)
V    $nlev 99 meridional wind (m/s)
O3   $nlev 99 Ozone (kg/kg)
CLW  $nlev 99 Cloud Water (kg/kg)
endvars
EOF
#-----------------------------------
fi
#-----------------------------------

date

export logfile=${logfile:-$rundir/log.ss2gg$CDATE1}
echo "complete $exp $CDATE1 to $CDATE2" > $logfile                        
exit
