#!/bin/ksh
set -x

## combine analyses of multiple experiments to create a single analysis
# Fanglin Yang, Nov 2010 

DATEST=${DATEST:-20090701}
DATEND=${DATEND:-20090701} 

cyclist=${cyclist:-"00"}                        ;#cycle to verify
expnlist=${expnlist:-"pru12r pre13d"}            ;#experiment names
expdlist=${expdlist:-"/global/hires/glopara/archive /global/hires/glopara/archive"}  ;#exp directories
complist=${complist:-"stratus cirrus"}          ;#computers where experiments are run
cdump=${cdump:-".gfs."}
scrdir=${vsdbhome:-/global/save/wx24fy/VRFY/vsdb}                                                
manldir=${manldir:-/stmp/$LOGNAME/vsdb_exp/manl}                          

export NWPROD=${NWPROD:-/nwprod}
export cpygb=${cpygb:-$NWPROD/util/exec/copygb}
export ndate=${ndate:-$NWPROD/util/exec/ndate}

rundir=${manldir}/tmp                                         
mkdir -p $manldir $rundir; cd $rundir  ||exit 8

myhost=`echo $(hostname) |cut -c 1-1 `
nexp=`echo $expnlist |wc -w`
n=0 ; for runn in $expnlist ; do n=$((n+1)) ; expname[n]=$runn ; done
n=0 ; for rund in $expdlist ; do n=$((n+1)) ; expdir[n]=$rund  ; done
n=0 ; for comp in $complist ; do n=$((n+1)) ; compname[n]=$comp  ; done

npts=10512                  ;#fix input pgbanl files resolution 144*73
#---------------------------------------
for cyc in $cyclist ; do
#---------------------------------------

sdate=$DATEST 
#---------------------------------------
while [ $sdate -le $DATEND ]; do
#---------------------------------------

nn=1
while [ $nn -le $nexp ]; do
rm -f input$nn 
  export CLIENT=${compname[nn]}   
  myclient=`echo $CLIENT |cut -c 1-1 `
  export filename=${expdir[nn]}/${expname[nn]}/pgbanl${cdump}${sdate}${cyc}
  if [ -s $filename ]; then
   ${cpygb} -g2 -x  $filename input$nn
  elif [ $myclient != $myhost ]; then
   rm input${nn}x
   scp $LOGNAME@${CLIENT}:${filename} input${nn}x
   ${cpygb} -g2 -x input${nn}x input$nn
  else
   echo "$filename does not exist"
   exit
  fi
nn=`expr $nn + 1 `
done

output=$manldir/pgbanl.${sdate}${cyc}
rm outtmp* 

for var in HGT TMP UGRD VGRD ; do 
for lev in 1000 975 950 925 900 850 800 750 700 650 600 550 500 450 400 350 300 250 200 150 100 70 50 30 20 10; do
  if [ $var = "HGT" ]; then  kpds5=7; kpds6=100; kpds7=$lev ; fi
  if [ $var = "TMP" ]; then  kpds5=11; kpds6=100; kpds7=$lev ; fi
  if [ $var = "UGRD" ]; then  kpds5=33; kpds6=100; kpds7=$lev ; fi
  if [ $var = "VGRD" ]; then  kpds5=34; kpds6=100; kpds7=$lev ; fi

  $scrdir/manl/mean_anl.exe $nexp $kpds5 $kpds6 $kpds7 $npts 
  mv outtmp outtmp_$var$lev
done
done

rm $output
cat outtmp* >$output 


tdate=` $ndate +24 ${sdate}00`
sdate=`echo $tdate |cut -c 1-8`
done   # date

#---------------------------------------
done   # cyc 
#---------------------------------------

exit


