#!/bin/ksh
set -x

# get operatinal GFS forecast and analysis data 
# in /com or from HPSS archive          

comout=${1:-/stmp/$LOGNAME/g2o/00Z/gfs}
exp=${2:-gfs}
cdate=${3:-2011050100}                    ;#forecast cycle         
vlength=${4:-120}                        ;#verification end hour of forecast
fhout=${5:-6}                            ;#output frequency
vhlist=${6:-"00 06 12 18"}               ;#prepbufr from gdas analysis cycles

IDAY=`echo $cdate |cut -c 1-8`
yyyy=`echo $cdate |cut -c 1-4 `
mm=`echo $cdate |cut -c 5-6 `
dd=`echo $cdate |cut -c 7-8 `
fcyc=`echo $cdate |cut -c 9-10 `

hpsstar=${HPSSTAR:-/u/Fanglin.Yang/bin/hpsstar}
NWPROD=${NWPROD:-/nwprod}
chost=`echo $(hostname) |cut -c 1-1`
gdas_prepbufr_arch=${gdas_prepbufr_arch:-/global/noscrub/Fanglin.Yang/prepbufr/gdas}
exp_dir=${exp_dir:-/global/noscrub/emc.glopara/global/$exp}       
if [ ! -s $comout ]; then mkdir -p $comout ;fi
cd $comout  ||exit 8

#--first try to get real-time ops data online at /com or at /exp_dir
errgfs=0
ffcst=00
vlength1=$vlength
if [ $vlength -gt 192 ]; then vlength1=192 ; fi
while [ $ffcst -le $vlength1 ] ; do
   fileina=/com/gfs/prod/gfs.$IDAY/gfs.t${fcyc}z.pgrbf${ffcst}
   fileinb=$exp_dir/pgbf${ffcst}.$exp.${IDAY}${fcyc}
   fileout=pgbf${ffcst}.${exp}.${IDAY}${fcyc}
   if [ -s $fileina ]; then
    ln -fs $fileina $fileout
   elif [ -s $fileinb ]; then
    ln -fs $fileinb $fileout
   else
    errgfs=1
   fi
   ffcst=`expr $ffcst + $fhout `
   if [ $ffcst -lt 10 ]; then ffcst=0$ffcst ; fi
done

 errgfs2=0
 ffcst=204
if [ $vlength -gt 192 ]; then
 while [ $ffcst -le $vlength ] ; do
   filein=/com/gfs/prod/gfs.$IDAY/gfs.t${fcyc}z.pgrb2f${ffcst}
   fileout=pgbf${ffcst}.${exp}.${IDAY}${fcyc}
   $NWPROD/util/exec/cnvgrib -g21 $filein $fileout
   if [ $? -ne 0 ]; then errgfs2=1 ; fi
   ffcst=`expr $ffcst + 12 `
 done
fi


errgdas=0
for vcyc in $vhlist; do
  fileina=/com/gfs/prod/gdas.$IDAY/gdas1.t${vcyc}z.prepbufr
  fileinb=${gdas_prepbufr_arch}/prepbufr.gdas.${IDAY}${vcyc} 
  fileout=prepbufr.gdas.${IDAY}${vcyc}
  if [ -s $fileina ]; then
    ln -fs $fileina $fileout
  elif [ -s $fileinb ]; then
    ln -fs $fileinb $fileout
  else
   errgdas=1
  fi
done


#--get data from HPSS archive if they do not exist online
newhpssdir=/NCEPPROD/hpssprod/runhistory/rh${yyyy}/${yyyy}${mm}/${yyyy}${mm}${dd}          ;#2.5-deg, bufr etc
newhpssdir1=/NCEPPROD/1year/hpssprod/runhistory/rh${yyyy}/${yyyy}${mm}/${yyyy}${mm}${dd}   ;#1-deg, 0.5-deg pgb files
newhpssdir2=/NCEPPROD/2year/hpssprod/runhistory/rh${yyyy}/${yyyy}${mm}/${yyyy}${mm}${dd}   ;#sigma, sfc flux etc

if [ $errgfs -ne 0 ]; then
rm pgbflist
>pgbflist
ffcst=00        
while [ $ffcst -le $vlength1 ] ; do
 echo ./gfs.t${fcyc}z.pgrbf${ffcst} >>pgbflist
 ffcst=`expr $ffcst + $fhout `
 if [ $ffcst -lt 10 ]; then ffcst=0$ffcst ; fi
done
#--extract data and rename files 
$hpsstar get $newhpssdir1/com_gfs_prod_gfs.${cdate}.pgrb.tar `cat pgbflist `
if [ $? -ne 0 ]; then
 rm gethpss.sh
cat > gethpss.sh <<EOF
 cd $comout
 $hpsstar get $newhpssdir1/com_gfs_prod_gfs.${cdate}.pgrb.tar \`cat pgbflist \`
EOF
 chmod a+x gethpss.sh
 $SUBJOB -a $ACCOUNT -q $CUE2FTP -p 1/1/S -r 1024/1 -t 00:30:00 -j gethpss \
   -o gethpss.out $comout/gethpss.sh
 nsleep=0; tsleep=30; msleep=40
 while test ! -s gfs.t${fcyc}z.pgrbf$vlength1 -a $nsleep -lt $msleep;do
   sleep $tsleep ; nsleep=`expr $nsleep + 1`
 done
fi
ffcst=00
while [ $ffcst -le $vlength1 ] ; do
 mv gfs.t${fcyc}z.pgrbf${ffcst} pgbf${ffcst}.${exp}.$cdate
 ffcst=`expr $ffcst + $fhout `
 if [ $ffcst -lt 10 ]; then ffcst=0$ffcst ; fi
done
fi

#---GFS 2nd segment, 12 hourly frequency frequency
if [ $errgfs2 -ne 0 -a $vlength -gt 192 ]; then
rm pgbflist
>pgbflist
ffcst=204       
while [ $ffcst -le $vlength ] ; do
 echo ./gfs.t${fcyc}z.pgrb2f${ffcst} >>pgbflist
 ffcst=`expr $ffcst + 12 `
done
#--extract data and rename files 
$hpsstar get $newhpssdir1/com_gfs_prod_gfs.${cdate}.pgrb0p5.tar `cat pgbflist `
ffcst=204
while [ $ffcst -le $vlength ] ; do
 $NWPROD/util/exec/cnvgrib -g21 gfs.t${fcyc}z.pgrb2f${ffcst} pgbf${ffcst}.${exp}.$cdate
 ffcst=`expr $ffcst + 12 `
done
fi


#-get gdas prepbufr files 
if [ $errgdas -ne 0 ]; then
 for vcyc in $vhlist; do
  if [ ! -s prepbufr.gdas.${yyyy}${mm}${dd}${vcyc} ]; then
   $hpsstar get ${newhpssdir}/com_gfs_prod_gdas.${yyyy}${mm}${dd}${vcyc}.tar   ./gdas1.t${vcyc}z.prepbufr
   mv gdas1.t${vcyc}z.prepbufr prepbufr.gdas.${yyyy}${mm}${dd}${vcyc}
  fi
 done
fi

exit
