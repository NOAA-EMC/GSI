#!/bin/ksh
set -x

export list="$listvar"
export SCRIPTS=$FITDIR/scripts
export SORC=$FITDIR/sorc
export GSCRIPTS=$FITDIR/grads
export CTLS=$FITDIR/ctls
export cue=${cue:-1}
export GROUP=${GROUP:-g01}

export ctldir=$tmpdir/$exp2/ctl
export PROUT=$tmpdir/$exp2/prout

export NWPROD=${NWPROD:-/nwprod}
export GRADSBIN=${GRADSBIN:-/apps/grads/2.0.1/bin} 
export stnmap=${GRADSBIN:-/usrx/local/grads/bin}/stnmap
export IMGCONVERT=${IMGCONVERT:-convert}
export ndate=${ndate:-$NWPROD/util/exec/ndate}
export nhour=${nhour:-$NWPROD/util/exec/nhour}
export SUBJOB=${SUBJOB:-/u/wx20mi/bin/sub}
export rundir=${rundir:-/stmp/$LOGNAME/fit}
export mapdir=${mapdir:-$rundir/web}

if [ ! -d $ctldir ] ; then
  mkdir -p $ctldir
fi
export vt2dir=$tmpdir/$exp2/vert
if [ ! -d $vt2dir ] ; then
  mkdir -p $vt2dir
fi
export vt1dir=$tmpdir/$exp2/vert/$exp1
if [ ! -d $vt1dir ] ; then
  mkdir -p $vt1dir
fi
export hz2dir=$tmpdir/$exp2/horiz
if [ ! -d $hz2dir ] ; then
  mkdir -p $hz2dir
fi
export hz1dir=$tmpdir/$exp2/horiz/$exp1
if [ ! -d $hz1dir ] ; then
  mkdir -p $hz1dir
fi

echo $sdate
export sdate12=` $ndate  12 $sdate`
export edate12=` $ndate -12 $edate`

yy=`echo $sdate | cut -c1-4`
mm=`echo $sdate | cut -c5-6`
dd=`echo $sdate | cut -c7-8`
hh=`echo $sdate | cut -c9-10`
mon=`$SCRIPTS/cmon.sh $mm`
hts00=${hh}z${dd}${mon}${yy}
echo "00z horiz plot start date $hts00"

nhours=` $nhour $edate $sdate`
ndays=`expr $nhours \/ 24`
echo "ndays is $ndays"

####### minday should be at least 75% (perc) of the time 
export perc=75
echo "perc is $perc"
export minday=`expr $ndays \* $perc \/ 100`
echo "minday is $minday"

yy=`echo $sdate12 | cut -c1-4`
mm=`echo $sdate12 | cut -c5-6`
dd=`echo $sdate12 | cut -c7-8`
hh=`echo $sdate12 | cut -c9-10`
mon=`$SCRIPTS/cmon.sh $mm`
hts12=${hh}z${dd}${mon}${yy}
echo "12z horiz plot start date $hts12"

leglist='legf00af06_0z legf00af06_12z legf00af06 legf12af36 legf24af48'
cd $ctldir
exx1=$(echo $exp1|cut -c 1-4)
exx2=$(echo $exp2|cut -c 1-4)
for leg in $leglist
do
ifile=$GSCRIPTS/${leg}_proto
file=$leg
> tmp
/bin/cp $ifile tmp
sed "s/Exp1/$exx1/g" tmp | sed "s/Exp2/$exx2/g" > $file
cat $file
done
###################################################################
#    Make time-averaged vertical fits plot file
###################################################################
if [[ $vcomp -eq 1 ]] ; then

for exp in $exp1 $exp2
do

#if [ ${exp} = "fnc" -o ${exp} = "fnl" -o ${exp} = "gfs" -o ${exp} = "ecm" -o ${exp} = "cdas" -o ${exp} = "avn" -o ${exp} = "gdas" -o ${exp} = "ukm" ]; then
# if [ $exp = $exp1 ]; then
#  export expdir=$dir1/fits/$exp
#  export pvdir=$vt1dir
# fi
# if [ $exp = $exp2 ]; then
#  export expdir=$dir2/fits/$exp
#  export pvdir=$vt2dir
# fi
#else
 if [ $exp = $exp1 ]; then
  export expdir=$dir1/fits
  export pvdir=$vt1dir
 fi
 if [ $exp = $exp2 ]; then
  export expdir=$dir2/fits
  export pvdir=$vt2dir
 fi
#fi

cd $pvdir

incr=24

fcslist='00 06'
for fcshr in $fcslist
do
for cycle in 00z 12z
do
if [ $cycle = "00z" ] ; then
pss=$sdate
pse=$edate
fi
if [ $cycle = "12z" ] ; then
pss=$sdate12
pse=$edate12
fi
$SCRIPTS/suruplot2_lnx.sh $exp $fcshr $cycle $pss $pse $incr $pvdir $expdir
done
done

fcslist='12 24 36 48'
for fcshr in $fcslist
do
pss=$sdate
pse=$edate
if [ $fcshr = "12" ] ; then
pss=$sdate12
pse=$edate12
fi
if [ $fcshr = "36" ] ; then
pss=$sdate12
pse=$edate12
fi
$SCRIPTS/suruplot_lnx.sh $exp $fcshr $pss $pse $incr $pvdir $expdir
done

##  finish exp-loop
done

###exit ###jsw

##  finish vcomp-section
fi

if [[ $vplots -eq 1 ]] ; then

for exp in $exp1 $exp2 ;  do
export exp

export ps00=$sdate
export pe00=$edate
export ps12=$sdate12
export pe12=$edate12
export pdir=$tmpdir/$exp2/maps/vert
export idir=$vt2dir
if [ $exp = $exp1 ] ; then
 export pdir=$tmpdir/$exp2/maps/vert/$exp1
 export idir=$vt1dir
fi
if [ ! -d $pdir ] ; then
  mkdir -p $pdir
fi

export webdir=$rzdmdir/fits/vert/$exp
export localdir=$mapdir/fits/vert/$exp
# mkdir -p $localdir

echo "exp is $exp"
echo "pdir is $pdir"
echo "webdir is $webdir"
echo "idir is $idir"

export listvar=ps00,pe00,ps12,pe12,exp,idir,pdir,web,webdir,localdir,SCRIPTS,GSCRIPTS,CTLS,namstr,webmch,webid,GRADSBIN,stnmap,IMGCONVERT
$SUBJOB -e $listvar -a $task  -q $cue -g $GROUP -p 1/1/S -r 512/1 -t 3:00:00 -o $PROUT/vertplot_$exp.out \
  $SCRIPTS/vertplot.new

done

fi
if [[ $vcplots -eq 1 ]] ; then

fcstlist='1 2 3 4'

for fcst in $fcstlist
do

export idir1=$vt1dir
export idir2=$vt2dir

if [ $fcst = 1 ] ; then
export fcs1='00.00z'
export fcs2='06.00z'
export sdir=f00af06_0z
export psdate=$sdate
export pedate=$edate
fi
if [ $fcst = 2 ] ; then
export fcs1='00.12z'
export fcs2='06.12z'
export sdir=f00af06_12z
export psdate=$sdate12
export pedate=$edate12
fi
if [ $fcst = 3 ] ; then
export fcs1=12
export fcs2=36
export sdir=f${fcs1}af${fcs2}
export psdate=$sdate12
export pedate=$edate12
fi
if [ $fcst = 4 ] ; then
export fcs1=24
export fcs2=48
export sdir=f${fcs1}af${fcs2}
export psdate=$sdate
export pedate=$edate
fi
export pdir=$tmpdir/$exp2/maps/vert/$sdir
if [ ! -d $pdir ] ; then
  mkdir -p $pdir
fi
export webdir=$rzdmdir/fits/vert/${exp1}-${exp2}
export localdir=$mapdir/fits/vert/${exp1}-${exp2}
# mkdir -p $localdir

export listvar=psdate,pedate,exp1,exp2,fcs1,fcs2,idir1,idir2,pdir,web,webdir,localdir,SCRIPTS,GSCRIPTS,CTLS,namstr,webmch,webid,ctldir,GRADSBIN,stnmap,IMGCONVERT
$SUBJOB -e $listvar -a $task  -q $cue -g $GROUP -p 1/1/S -r 512/1 -t 3:00:00 -o $PROUT/vertplot_$sdir.out \
  $SCRIPTS/vertplot.cdas

done

fi

###################################################################
##  submit job to make time plots
###################################################################
#### make ctl files
if [[ $mctl -eq 1 ]] ; then
yy=`echo $sdate | cut -c1-4`
mm=`echo $sdate | cut -c5-6`
dd=`echo $sdate | cut -c7-8`
mon=`$SCRIPTS/cmon.sh $mm`
timedate=${dd}${mon}${yy}
echo $timedate
typelist='raob sfc acar acft'
fcslist='00 06 12 24 36 48'
cd $ctldir
for exp in $exp1 $exp2
do
#if [ ${exp} = "fnc" -o ${exp} = "fnl" -o ${exp} = "gfs" -o ${exp} = "ecm" -o ${exp} = "cdas" -o ${exp} = "avn" -o ${exp} = "gdas" -o ${exp} = "ukm" ]; then
# if [ $exp = $exp1 ]; then dir=$dir1/fits/$exp ;fi
# if [ $exp = $exp2 ]; then dir=$dir2/fits/$exp ;fi
#else
 if [ $exp = $exp1 ]; then dir=$dir1/fits; fi
 if [ $exp = $exp2 ]; then dir=$dir2/fits; fi
#fi

if [ $exp = $exp1 ]; then endian=${endian1:-big}; fi
if [ $exp = $exp2 ]; then endian=${endian2:-big}; fi
endnew=${endian}_endian

for type in $typelist
do
for fcs in $fcslist
do
name=f${fcs}.$type
ifile=$CTLS/${name}_proto.ctl
ofile=${exp}.${name}.ctl
> tmp
sed "s/date/$timedate/g" $ifile | sed "s?dir?$dir?g"  | sed "s?ENDIAN?$endnew?g"  > $ofile
done
cat $ofile
done

done
fi

if [[ $tplots -eq 1 ]] ; then

for exp in timeout timevrt $exp1 $exp2 ; do
export exp exp1 exp2

export oinc_f2o=${oinc_f2o:-24}      # increment (hours) between observation verify times for timeout plots
export finc_f2o=${finc_f2o:-12}      # increment (hours) between forecast lengths for timeout plots
export fmax_f2o=${fmax_f2o:-48}      # max forecast length to show for timeout plots
export webdir=$rzdmdir/fits/time/$exp
export localdir=$mapdir/fits/time/$exp; mkdir -p $localdir
export ptdir=$tmpdir/$exp2/maps/time/$exp; mkdir -p $ptdir

if [ $exp = timeout ]; then
 export pdir=$ptdir;  mkdir -p $pdir; echo "exp is $exp"
 export listvar=quan,edate,edate12,sdate,sdate12,var,exp1,exp2,pdir,web,webdir,localdir,SCRIPTS,GSCRIPTS,ctldir,namstr,webmch,webid,GRADSBIN,IMGCONVERT,oinc_f2o,finc_f2o,fmax_f2o,expnlist,expdlist,SORC,CTLS,ndate
 $SUBJOB -e $listvar -a $task  -q $cue -g $GROUP -p 1/1/S -r 512/1 -t 3:00:00 -o $PROUT/timeout.newb.out $SCRIPTS/timeout.newb
 continue
fi

if [ $exp = timevrt ]; then
 export pdir=$ptdir;  mkdir -p $pdir; echo "exp is $exp"
 export listvar=quan,edate,edate12,sdate,sdate12,var,exp1,exp2,pdir,web,webdir,localdir,SCRIPTS,GSCRIPTS,ctldir,namstr,webmch,webid,GRADSBIN,IMGCONVERT,oinc_f2o,finc_f2o,fmax_f2o,expnlist,expdlist,SORC,CTLS,ndate
 $SUBJOB -e $listvar -a $task  -q $cue -g $GROUP -p 1/1/S -r 512/1 -t 3:00:00 -o $PROUT/timevrt.newb.out $SCRIPTS/timevrt.newb
 continue
fi

for var in tmp hgt wnd moi ps
do
export pdir=$ptdir/$var;  mkdir -p $pdir; echo "exp is $exp"
export listvar=edate,edate12,sdate,sdate12,var,exp,pdir,web,webdir,localdir,SCRIPTS,GSCRIPTS,ctldir,namstr,webmch,webid,GRADSBIN,stnmap,IMGCONVERT,ndate
$SUBJOB -e $listvar -a $task  -q $cue -g $GROUP -p 1/1/S -r 512/1 -t 3:00:00 -o $PROUT/time${var}_$exp.out \
$SCRIPTS/time${var}.newb
done

export pdir=$ptdir/acar; mkdir -p $pdir
export listvar=edate,edate12,sdate,sdate12,var,exp,pdir,web,webdir,localdir,SCRIPTS,GSCRIPTS,ctldir,namstr,webmch,webid,GRADSBIN,stnmap,IMGCONVERT,ndate
$SUBJOB -e $listvar -a $task  -q $cue -g $GROUP -p 1/1/S -r 512/1 -t 3:00:00 -o $PROUT/timeacar_$exp.out \
$SCRIPTS/timeacar.newb

export pdir=$ptdir/acft;  mkdir -p $pdir
export listvar=edate,edate12,sdate,sdate12,var,exp,pdir,web,webdir,localdir,SCRIPTS,GSCRIPTS,ctldir,namstr,webmch,webid,GRADSBIN,stnmap,IMGCONVERT,ndate
$SUBJOB -e $listvar -a $task  -q $cue -g $GROUP -p 1/1/S -r 512/1 -t 3:00:00 -o $PROUT/timeacft_$exp.out \
$SCRIPTS/timeacft.newb

done

fi

if [[ $tcplots -eq 1 ]] ; then

fcstlist='1 2 3'
for fcst in $fcstlist
do

if [ $fcst = 1 ] ; then
export fcs1=00
export fcs2=06
fi
if [ $fcst = 2 ] ; then
export fcs1=12
export fcs2=36
fi
if [ $fcst = 3 ] ; then
export fcs1=24
export fcs2=48
fi
export sdir=f${fcs1}af${fcs2}
export webdir=$rzdmdir/fits/time/$sdir
export localdir=$mapdir/fits/time/$sdir
# mkdir -p $localdir

varlist='tmp hgt wnd moi ps'  
for var in $varlist
do
export pdir=$tmpdir/$exp2/maps/time/$sdir/$var
if [ ! -d $pdir ] ; then
  mkdir -p $pdir
fi
export listvar=edate,edate12,sdate,sdate12,var,exp1,exp2,fcs1,fcs2,pdir,web,webdir,localdir,SCRIPTS,GSCRIPTS,ctldir,namstr,webmch,webid,GRADSBIN,stnmap,IMGCONVERT
$SUBJOB -e $listvar -a $task  -q $cue -g $GROUP -p 1/1/S -r 512/1 -t 3:00:00 -o $PROUT/time${var}_$sdir.out \
  $SCRIPTS/time$var.cdas
done

export pdir=$tmpdir/$exp2/maps/time/$sdir/acar
if [ ! -d $pdir ] ; then
  mkdir -p $pdir
fi
export listvar=edate,edate12,sdate,sdate12,var,exp1,exp2,fcs1,fcs2,pdir,web,webdir,localdir,SCRIPTS,GSCRIPTS,ctldir,namstr,webmch,webid,GRADSBIN,stnmap,IMGCONVERT
$SUBJOB -e $listvar -a $task  -q $cue -g $GROUP -p 1/1/S -r 512/1 -t 3:00:00 -o $PROUT/timeacar_$sdir.out \
  $SCRIPTS/timeacar.cdas

export pdir=$tmpdir/$exp2/maps/time/$sdir/acft
if [ ! -d $pdir ] ; then
  mkdir -p $pdir
fi
export listvar=edate,edate12,sdate,sdate12,var,exp1,exp2,fcs1,fcs2,pdir,web,webdir,localdir,SCRIPTS,GSCRIPTS,ctldir,namstr,webmch,webid,GRADSBIN,stnmap,IMGCONVERT
$SUBJOB -e $listvar -a $task  -q $cue -g $GROUP -p 1/1/S -r 512/1 -t 3:00:00 -o $PROUT/timeacft_$sdir.out  \
$SCRIPTS/timeacft.cdas

done

fi

###################################################################
##  submit job to make horiz plots
###################################################################
if [[ $hcomp -eq 1 ]] ; then

incr=24

for exp in $exp1 $exp2
do

##  start 00z-loop
for dir in anl fcs
do

#if [ ${exp} = "fnc" -o ${exp} = "fnl" -o ${exp} = "gfs" -o ${exp} = "ecm" -o ${exp} = "cdas" -o ${exp} = "avn" -o ${exp} = "gdas" -o ${exp} = "ukm" ]; then
# if [ $exp = $exp1 ] ; then
#  horizdir=$dir1/horiz/$exp/$dir
#  outdir=$hz1dir
# fi
# if [ $exp = $exp2 ] ; then
#  horizdir=$dir2/horiz/$exp/$dir
#  outdir=$hz2dir
# fi
#else
 if [ $exp = $exp1 ] ; then
  horizdir=$dir1/horiz/$dir
  outdir=$hz1dir
 fi
 if [ $exp = $exp2 ] ; then
  horizdir=$dir2/horiz/$dir
  outdir=$hz2dir
 fi
#fi
cd $outdir


hzname=adpupa.mand
outfile=$outdir/adpupa.$dir.$sdate.$edate
$SCRIPTS/havgfit.sh $sdate $edate $horizdir $hzname $outfile $incr

for name in adpsfc sfcshp
do
hzname=$name
outfile=$outdir/$name.$dir.$sdate.$edate
imas=1
iwnd=0
$SCRIPTS/sfcfit.sh $sdate $edate $horizdir $hzname $outfile $imas $iwnd $incr
done

for dat in adpupa adpsfc sfcshp
do
ofile=${dat}00.$dir.ctl
> $ofile
cp $CTLS/${dat}00_tm.ctl $ofile
> out
#sed "s?date?$hts00?g" $ofile | sed "s?dir?$dir?g" | sed "s?file?$sdate.$edate?g"  | sed "s?ENDIAN?$endnew?g" > out
sed "s?date?$hts00?g" $ofile | sed "s?dir?$dir?g" | sed "s?file?$sdate.$edate?g"   > out
cp out $ofile
cat $ofile
$stnmap -i $ofile
done

##  finish 00z-loop
done

##  start 12z-loop
for dir in fcs
do

hzname=adpupa.mand
outfile=$outdir/adpupa.$dir.$sdate12.$edate12
$SCRIPTS/havgfit.sh $sdate12 $edate12 $horizdir $hzname $outfile $incr

for name in adpsfc sfcshp
do
hzname=$name
outfile=$outdir/$name.$dir.$sdate12.$edate12
imas=1
iwnd=0
$SCRIPTS/sfcfit.sh $sdate12 $edate12 $horizdir $hzname $outfile $imas $iwnd $incr
done

for dat in adpupa adpsfc sfcshp
do
ofile=${dat}12.$dir.ctl
> $ofile
cp $CTLS/${dat}12_tm.ctl $ofile
> out
#sed "s?date?$hts12?g" $ofile | sed "s?dir?$dir?g" | sed "s?file?$sdate12.$edate12?g"  | sed "s?ENDIAN?$endnew?g" > out
sed "s?date?$hts12?g" $ofile | sed "s?dir?$dir?g" | sed "s?file?$sdate12.$edate12?g"  > out
cp out $ofile
cat $ofile
$stnmap -i $ofile
done

##  finish 12z-loop
done

##  finish experiment-loop
done

##  finish hcomp-section
fi

if [[ $hplots -eq 1 ]] ; then

### for joint maps...
export exp1dir=$hz1dir
export exp2dir=$hz2dir
export webdir=$rzdmdir/fits/horiz/$exp2
export localdir=$mapdir/fits/horiz/$exp2
# mkdir -p $localdir
export pdir=$tmpdir/$exp2/maps/horiz/f00plots
if [ ! -d $pdir ] ; then
  mkdir -p $pdir
fi
export listvar=edate,edate12,sdate,sdate12,minday,exp1dir,exp2dir,exp1,exp2,pdir,GSCRIPTS,SCRIPTS,web,webdir,localdir,namstr,webmch,webid,GRADSBIN,stnmap,IMGCONVERT
$SUBJOB -e $listvar -a $task  -q $cue -g $GROUP -p 1/1/S -r 512/1 -t 3:00:00 -o $PROUT/f00plots_$exp2.out \
  $SCRIPTS/f00plots.cdas
export pdir=$tmpdir/$exp2/maps/horiz/f12plots
if [ ! -d $pdir ] ; then
  mkdir -p $pdir
fi
export listvar=edate,edate12,sdate,sdate12,minday,exp1dir,exp2dir,exp1,exp2,pdir,GSCRIPTS,SCRIPTS,web,webdir,localdir,namstr,webmch,webid,GRADSBIN,stnmap,IMGCONVERT
$SUBJOB -e $listvar -a $task  -q $cue -g $GROUP -p 1/1/S -r 512/1 -t 3:00:00 -o $PROUT/f12plots_$exp2.out \
  $SCRIPTS/f12plots.cdas

### for indivdual maps...
for exp in $exp1 $exp2
do
export exp
export expdir=$hz2dir
if [ $exp = $exp1 ] ; then
 export expdir=$hz1dir
fi

export webdir=$rzdmdir/fits/horiz/$exp
export localdir=$mapdir/fits/horiz/$exp
# mkdir -p $localdir

export pdir=$tmpdir/$exp2/maps/horiz/horizab
if [ $exp = $exp1 ] ; then
export pdir=$tmpdir/$exp2/maps/horiz/horizab/$exp1
fi
if [ ! -d $pdir ] ; then
  mkdir -p $pdir
fi
export listvar=edate,edate12,sdate,sdate12,minday,expdir,exp,pdir,GSCRIPTS,SCRIPTS,web,webdir,localdir,namstr,webmch,webid,GRADSBIN,stnmap,IMGCONVERT
$SUBJOB -e $listvar -a $task  -q $cue -g $GROUP -p 1/1/S -r 512/1 -t 3:00:00 -o $PROUT/horizab_$exp.out \
  $SCRIPTS/horizab.cdas

export pdir=$tmpdir/$exp2/maps/horiz/horizrab
if [ $exp = $exp1 ] ; then
export pdir=$tmpdir/$exp2/maps/horiz/horizrab/$exp1
fi
if [ ! -d $pdir ] ; then
  mkdir -p $pdir
fi
export listvar=edate,edate12,sdate,sdate12,minday,expdir,exp,pdir,GSCRIPTS,SCRIPTS,web,webdir,localdir,namstr,webmch,webid,GRADSBIN,stnmap,IMGCONVERT
$SUBJOB -e $listvar -a $task  -q $cue -g $GROUP -p 1/1/S -r 512/1 -t 3:00:00 -o $PROUT/horizrab_$exp.out \
  $SCRIPTS/horizrab.cdas

export pdir=$tmpdir/$exp2/maps/horiz/sfcab
if [ $exp = $exp1 ] ; then
export pdir=$tmpdir/$exp2/maps/horiz/sfcab/$exp1
fi
if [ ! -d $pdir ] ; then
  mkdir -p $pdir
fi
export listvar=edate,edate12,sdate,sdate12,minday,expdir,exp,pdir,GSCRIPTS,SCRIPTS,web,webdir,localdir,namstr,webmch,webid,GRADSBIN,stnmap,IMGCONVERT
$SUBJOB -e $listvar -a $task  -q $cue -g $GROUP -p 1/1/S -r 512/1 -t 3:00:00 -o $PROUT/sfcab_$exp.out \
  $SCRIPTS/sfcab.cdas

done

fi
