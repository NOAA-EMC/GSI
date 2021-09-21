#!/bin/sh
#date of first radstat file
bdate=2020100100
#date of last radstat file
edate=2020101418
#instrument name, as it would appear in the title of a diag file
#instr=airs_aqua
#instr=cris-fsr_n20
instr=iasi_metop-b
#location of radstat file
exp=v16rt2
diagdir=/scratch1/NCEPDEV/da/${USER}/archive/${exp}
#working directory
wrkdir=/scratch1/NCEPDEV/stmp4/${USER}/corr_obs_${instr}
#location the covariance matrix is saved to
savdir=$diagdir
#FOV type- 0 for all, 1 for sea, 2 for land, 3 for snow,
#4 for mixed (recommended to use 0 for mixed)
#5 for ice and 6 for snow and ice combined (recommended when using ice)
type=1
#cloud 1 for clear FOVs, 2 for clear channels
cloud=2
#absolute value of the maximum allowable sensor zenith angle (degrees)
angle=30
#option to output the channel wavenumbers
wave_out=.false.
#option to output the observation errors
err_out=.false.
#option to output the correlation matrix
corr_out=.false.
#condition number to recondition Rcov.  Set <0 to not recondition
kreq=-200
#inflation factors, for regular channels, surface channels and water vapor channels
#infl is applied to all channels if using binary files or a MW instrument
#set factors equal to 1 to not inflate, or if this channel group is not assimilated
infl=1.0
inflsurf=1.0
inflwv=1.0
#method to recondition:  1 for trace method, 2 for Weston's second method
method=1
#method to compute covariances: 1 for Hollingsworth-Lonnberg, 2 for Desroziers
cov_method=2
#maximum time between observations in a pair, in minutes
time_sep=1.0
#bin size for obs pairs in km
bsize=1
#bin center, in km, needed for Hollingsworth-Lonnberg
bcen=80
#channel set choice:  0 to only use active channels, 1 to use all channels
chan_set=0
#Have the radstats already been processed? 1 for yes, 0 for no
radstats_processed=1
#netcdf or binary diag files-0 for binary, 1 for netcdf
netcdf=1
ndate=/scratch1/NCEPDEV/da/Kristen.Bathmann/Analysis_util/ndate
#ndate=/gpfs/dell2/emc/modeling/noscrub/Kristen.Bathmann/ndate

####################

cdate=$bdate
[ ! -d ${wrkdir} ] && mkdir ${wrkdir}
[ ! -d ${savdir} ] && mkdir ${savdir}
cp ../../exec/cov_calc $wrkdir
nt=0
ntt=0
cd $wrkdir
while [[ $cdate -le $edate ]] ; do
   while [[ ! -f $diagdir/radstat.gdas.$cdate ]] ; do 
     cdate=`$ndate +06 $cdate`
     if [ $cdate -ge $edate ] ; then
        break
     fi
   done
   if [ $netcdf -gt 0 ] ; then
      fil=${cdate}.nc4
   else
      fil=${cdate}
   fi
   nt=`expr $nt + 1`
   if [ $nt -lt 10 ] ; then
      fon=000$nt
   elif [ $nt -lt 100 ] ; then
      fon=00$nt
   elif [ $nt -lt 1000 ] ; then
      fon=0$nt
   else
      fon=$nt
   fi
   if [ $radstats_processed -lt 1 ] ; then
      if [ ! -f danl_${fon} ];
      then
         cp $diagdir/radstat.gdas.$cdate .
         tar --extract --file=radstat.gdas.${cdate} diag_${instr}_ges.${fil}.gz diag_${instr}_anl.${fil}.gz
         gunzip *.gz
         rm radstat.gdas.$cdate
         if [ -f diag_${instr}_ges.${fil} ];
         then
            mv diag_${instr}_anl.${fil} danl_${fon}
            mv diag_${instr}_ges.${fil} dges_${fon}
         else
            nt=`expr $nt - 1`
         fi
         ntt=$nt
      fi
   else
      if [ -f danl_${fon} ] ; then
         ntt=`expr $ntt + 1`
      fi
   fi
   cdate=`$ndate +06 $cdate`
done
./cov_calc <<EOF
$ntt $type $cloud $angle $instr $wave_out $err_out $corr_out $kreq $infl $inflsurf $inflwv $method $cov_method $chan_set $time_sep $bsize $bcen $netcdf
EOF
stype=sea
if [ $type -eq 0 ] ; then
   stype=glb
elif [ $type -eq 2 ] ; then
   stype=land
elif [ $type -eq 3 ] ; then
   stype=snow
elif [ $type -eq 4 ] ; then
   stype=mixed
elif [ $type -eq 5 ] ; then
   stype=ice
elif [ $type -eq 6 ] ; then
   stype=snow_ice
fi
mv Rcov_$instr Rcov_${instr}_${stype}
[ -f Rcorr_$instr ] && mv Rcorr_$instr Rcorr_${instr}_${stype}
[ -f wave_$instr ] && mv wave_$instr wave_${instr}_${stype}
[ -f err_$instr ] && mv err_$instr err_${instr}_${stype}

cp Rcov_${instr}_${stype} $savdir

[ -f Rcorr_${instr}_${stype} ] && cp Rcorr_${instr}_${stype} $savdir
[ -f wave_${instr}_${stype} ] && cp wave_${instr}_${stype} $savdir
[ -f err_${instr}_${stype} ] && cp err_${instr}_${stype} $savdir
exit 0
