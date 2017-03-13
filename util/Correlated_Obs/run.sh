#!/bin/sh
#date of first radstat file
bdate=2014040700
#date of last radstat file
edate=2014041718
#instrument name, as it would appear in the title of a diag file
#instr=airs_aqua
instr=iasi_metop-b
#location of radstat file
exp=prCtl
diagdir=/scratch4/NCEPDEV/da/noscrub/${USER}/archive/${exp}
working directory
#wrkdir=/scratch4/NCEPDEV/stmp4/${USER}/corr_obs
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
#option to output the assigned observation errors
err_out=.false.
#option to output the correlation matrix
corr_out=.false.
#condition number to recondition Rcov.  Set <0 to not recondition
kreq=-150
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

ndate=/scratch4/NCEPDEV/da/save/Michael.Lueken/nwprod/util/exec/ndate
####################

cdate=$bdate
[ ! -d ${wrkdir} ] && mkdir ${wrkdir}
[ ! -d ${savdir} ] && mkdir ${savdir}
cp cov_calc $wrkdir
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
         tar --extract --file=radstat.gdas.${cdate} diag_${instr}_ges.${cdate}.gz diag_${instr}_anl.${cdate}.gz
         gunzip *.gz
         rm radstat.gdas.$cdate
         if [ -f diag_${instr}_ges.${cdate} ];
         then
            mv diag_${instr}_anl.${cdate} danl_${fon}
            mv diag_${instr}_ges.${cdate} dges_${fon}
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
$ntt $type $cloud $angle $instr $wave_out $err_out $corr_out $kreq $method $cov_method $chan_set $time_sep $bsize $bcen
EOF

cp Rcov_$instr $savdir

[ -f Rcorr_$instr ] && cp Rcorr_$instr $savdir
[ -f wave_$instr ] && cp wave_$instr $savdir
[ -f err_$instr ] && cp err_$instr $savdir
exit 0
