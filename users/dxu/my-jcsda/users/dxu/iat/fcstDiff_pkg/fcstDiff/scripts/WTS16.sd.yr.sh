#!/bin/sh
set -x

# ---------------------------------------------
#  die-off curves 
# ----------------------------------------

chmmdd=${ndate_dir}/ndate
exedir=${STMP}/$LOGNAME/annual
tmpdir=${exedir}/WTS16.sd.yr
rm -r -f $tmpdir;  mkdir -p $tmpdir

sdir=$FHOME/annual/WTS16yr
sdir2=$FHOME/monthly/WTS16
archdir=${SGLOBAL}/vrfy

#-------------------------------------
cd $tmpdir  ||exit 8


if [ $# -lt 8 ]; then
   echo "  The number of input arguments less then 8 "
   echo "  specify start,end date (yyyymmdd), models; first 2 to be compared "
   echo "   standard: s x e k n m; but for year, use  s no e k n m"
   exit 8
fi
stymdy2k=$1
edymdy2k=$2
mod1=$3
mod2=$4
mod3=$5
mod4=$6
mod5=$7
mod6=$8
numi=$#
numm=` expr $numi - 2 `
lev1=1000;lev2=500;lev3=850;lev4=200
echo  "models are "$mod1 $mod2 $mod3 $mod4 $mod5 $mod6
echo  "levels are "$lev1 $lev2 $lev3 $lev4 
echo  "number of models is " $numm


# save start day, get no. of days:
ds=$stymdy2k
de=$edymdy2k
jds=${ds}00
 echo  "jds is "$jds
jde=${de}00
 echo  "jde is "$jde
 nhrs=`/nwprod/util/exec/nhour $jde $jds `
 echo  "nhrs is "$nhrs
 ndays=`expr "(" $nhrs "/" 24 ")" + 1`
 echo  "ndays is "$ndays


stymd=$stymdy2k
day=1
while [ $day -le $ndays ]; do
  cp $archdir/SCORESa.${stymd}12 SCORESa12.$stymd
  cp $archdir/SCORESa.${stymd}06 SCORESa06.$stymd
  cp $archdir/SCORESa.${stymd}18 SCORESa18.$stymd
  cp $archdir/SCORESa.${stymd}00 SCORESa00.$stymd
  cp $archdir/SCORESe.${stymd}12 SCORESe.$stymd
  cp $archdir/SCORESx.$stymd .

day=` expr $day + 1 `
stymd=`$chmmdd +24 ${stymd}00 | cut -c1-8 `
 echo "stymd= "$stymd
done

tmpdir3=${tmpdir}/test
rmdir -r $tmpdir3; mkdir -p $tmpdir3

#make directories to accum daily scores, each model, level
diearch=${tmpdir}/dataarch
rm -r -f $diearch; mkdir -p $diearch

# make directories for model/level
   
#for lev in 1000 500 850 200
 for lev in $lev1 $lev2 $lev3 $lev4
do
   for mod in $mod1 $mod2 $mod3 $mod4 $mod5 $mod6
do 
      rm -r -f $diearch/${mod}${lev}
      mkdir -p $diearch/${mod}${lev}
done
done

 xlf  $sdir2/wts16.f -o $tmpdir/wts16.x -O -qsource -qxref=full -qattr=full -qcheck 

#istymd=$stymd
istymdy2k=$stymdy2k

day=1

# ---   loop through verifying days --------------------------|

#while [ $stymd -le edymd ]; do
while [ $day -le $ndays ]; do

iymdy2k=$stymdy2k
iymd=` expr $iymdy2k - 19000000 `
#iymd=$stymd
echo " the date is set to $iymdy2k or $iymd "

#  ---- loop through all models for each day -----------|
#    s=mrf e=ecmwf n=fnoc a=AVN extended z,x,y,v, and w are parallels           

   for mod in $mod1 $mod2 $mod3 $mod4 $mod5 $mod6
do

tmpdir2=${tmpdir}/$mod
rm -r  $tmpdir2
mkdir -p $tmpdir2
cd $tmpdir2 ||exit 8

# zero all indicators, then set to 1 if model available...
im=0
# substitute a for s to verify afainst "aviation" run, not GDAS
#  use modd as a dummy for mod only in SCORES
modd=$mod
if [ "$mod" = "s" ];then modd="a";fi
#if [ -s $archdir/SCORES$modd.$iymd -o -s $archdir/SCORES$modd.$iymdy2k ]; then
if [ -s $archdir/SCORES$modd.${iymd}00 -o -s $archdir/SCORES$modd.${iymdy2k}00 ]; then
  im=1
fi

if [ "$mod" = "no" ];then im=0;fi
#if [ "$mod" -eq "no" ];then im=0;fi

rm stdin
echo "$iymdy2k $istymdy2k" >>stdin  
echo "$im " >>stdin
echo "$mod" >>stdin
cat stdin

if [ $stymdy2k -lt 19990000 ]; then
    ln -fs $archdir/SCORES$modd.${iymd}00        fort.31
else
    ln -fs $archdir/SCORES$modd.${iymdy2k}00     fort.31
fi

ln -fs acz1000n                        fort.51
ln -fs acz1000s                        fort.52
ln -fs acz500n                         fort.53
ln -fs acz500s                         fort.54
ln -fs rmsz1000n                       fort.55
ln -fs rmsz1000s                       fort.56
ln -fs rmszc1000n                      fort.57
ln -fs rmszc1000s                      fort.58
ln -fs rmsz500n                        fort.59
ln -fs rmsz500s                        fort.60
ln -fs rmszc500n                       fort.61
ln -fs rmszc500s                       fort.62

ln -fs acv850                          fort.63
ln -fs rmsv850                         fort.64
ln -fs rmsvc850                        fort.65
ln -fs mse850                          fort.66
ln -fs acv200                          fort.67
ln -fs rmsv200                         fort.68
ln -fs rmsvc200                        fort.69
ln -fs mse200                          fort.70
ln -fs mze1000n                        fort.71
ln -fs mze1000s                        fort.72
ln -fs mze500n                         fort.73
ln -fs mze500s                         fort.74

ln -fs test500zn                       fort.81
ln -fs test500zs                       fort.82
ln -fs test1000zn                      fort.83
ln -fs test1000zs                      fort.84
ln -fs test500zmn                      fort.85
ln -fs test500zms                      fort.86
ln -fs testrms5zn                      fort.88
ln -fs testrms5zs                      fort.89

# display day number to watch progress of job interactively
  if [ "$mod" = "s" ]; then
 echo "|||||||||||||||||||||||||||"
 echo "                           "
 echo  "    DAY NUMBER     "$day 
 echo "                           "
 echo "|||||||||||||||||||||||||||"
sleep 1
  fi

$tmpdir/wts16.x <stdin    
#---------------------------------------
  for lev in $lev1 $lev2
do
    for hem in n s
do
  cat acz$lev$hem     >>  $diearch/${mod}${lev}/acz$hem.$edymdy2k
  cat rmsz$lev$hem    >>  $diearch/${mod}${lev}/rmsz$hem.$edymdy2k  
  cat rmszc$lev$hem   >>  $diearch/${mod}${lev}/rmszc$hem.$edymdy2k  
  cat mze$lev$hem     >>  $diearch/${mod}${lev}/mze$hem.$edymdy2k  
done
done

  for lev in $lev3 $lev4
do
  cat acv$lev         >>  $diearch/${mod}${lev}/acv.$edymdy2k
  cat rmsv$lev        >>  $diearch/${mod}${lev}/rmsv.$edymdy2k
  cat rmsvc$lev       >>  $diearch/${mod}${lev}/rmsvc.$edymdy2k
  cat mse$lev         >>  $diearch/${mod}${lev}/mse.$edymdy2k
done

# new - collect daily formatted results for each model
  cat test500zn       >>  $tmpdir3/${mod}total500zn
  cat test500zs       >>  $tmpdir3/${mod}total500zs
  cat test1000zn      >>  $tmpdir3/${mod}total1000zn
  cat test1000zs      >>  $tmpdir3/${mod}total1000zs
  cat testrms5zn      >>  $tmpdir3/${mod}totalrms5zn
  cat testrms5zs      >>  $tmpdir3/${mod}totalrms5zs
  cat test500zmn      >>  $tmpdir3/${mod}total500zmn
  cat test500zms      >>  $tmpdir3/${mod}total500zms

# end model loop ----------------------------|
done

day=` expr $day + 1 `

stymdy2k=`$chmmdd +24 ${stymdy2k}00 | cut -c1-8 `
 echo "stymdy2k= "$stymdy2k

# end day loop ------------------------------------|
done

#do acdiegrads,rmsmediegrads passing first and last day, ndays and models:

       echo "-------------------------${sdir}/rmsdie16 ----------------------"
#export rundir=$tmpdir; $sdir/rmsdie16.yr.sh  $istymdy2k $edymdy2k $ndays $mod1 $mod2 $mod3 $mod4 $mod5 $mod6
       echo "-------------------------${sdir}/acdie16 ----------------------"
#export rundir=$tmpdir; $sdir/acdie16.sd.yr.sh $istymdy2k $edymdy2k $ndays $mod1 $mod2 $mod3 $mod4 $mod5 $mod6
export rundir=$tmpdir; /global/save/wx23dc/scripts/acdie16.sd.yr.sh $istymdy2k $edymdy2k $ndays $mod1 $mod2 $mod3 $mod4 $mod5 $mod6


exit
