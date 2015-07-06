#!/bin/sh
#date of first radstat file
#bdate=2013111100
bdate=2013102500
#date of last radstat file
#edate=bdate
edate=2013111500
#instrument name, as it would appear in the title of a diag file
instr=airs_aqua
#instr=airs_aqua
#location of radstat file
exp=prexoz
diagdir=/scratch1/portfolios/NCEPDEV/da/noscrub/Kristen.Bathmann/archive/${exp}
#working directory
wrkdir=/scratch2/portfolios/NCEPDEV/stmp/Kristen.Bathmann/desroziers_${exp}_${bdate}
#location the covariance matrix is saved to
savdir=$diagdir
#type- 0 for all, 1 for sea, 2 for land
type=1
#cloud -0 for all (cloudy and clear) radiances, 1 for clear FOVs, 2 for clear channels
cloud=1
#absolute value of the maximum allowable sensor zenith angle (degrees)
angle=20
#The name of the outputted covariance file
fileout=Rcov_$instr
#Name of file that contains the wavenumbers of the channels of $instr
fileout1=wave_$instr
#Name of file that contains the assumed obs error of the channels of $instr
fileout2=err_$instr
 ndate=/scratch1/portfolios/NCEPDEV/da/save/Michael.Lueken/nwprod/util/exec/ndate

####################

cdate=$bdate
[ ! -d ${wrkdir} ] && mkdir ${wrkdir}
[ ! -d ${savdir} ] && mkdir ${savdir}
cp cov_calc $wrkdir
nt=0
cd $wrkdir
while [[ $cdate -le $edate ]] ; do
   while [[ ! -f $diagdir/radstat.gdas.$cdate ]] ; do 
     cdate=`$ndate +06 $cdate`
     if [ $cdate -gt $edate ] ; then
        break
     fi
   done
   nt=`expr $nt + 1`
   if [ $nt -lt 10 ] ; then
      fon=000$nt
   elif [ $nt -lt 100 ] ; then
      fon=00$nt
   elif [ $nt -lt 1000] ; then
      fon=0$nt
   else
      fon=$nt
   fi
   if [ ! -f danl_${fon} ];
   then
      cp $diagdir/radstat.gdas.$cdate .
      tar -xvf radstat.gdas.$cdate
      gunzip *.gz
      rm radstat.gdas.$cdate
      if [ -f diag_${instr}_anl.${cdate} ];
      then
         mv diag_${instr}_anl.${cdate} danl_${fon}
         mv diag_${instr}_ges.${cdate} dges_${fon}
      else
         nt=`expr $nt - 1`
      fi
      rm diag*
   fi
   cdate=`$ndate +06 $cdate`
done
./cov_calc <<EOF
$nt $type $cloud $angle $fileout $fileout1 $fileout2
EOF

cp $fileout $savdir
cp $fileout1 $savdir
cp $fileout2 $savdir
cp ${fileout}_corr $savdir
