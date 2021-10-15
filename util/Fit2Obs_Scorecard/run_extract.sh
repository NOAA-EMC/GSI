#!/bin/sh
#This program reads the noscrub/archive/exp/fits/fit* files 
#and puts them into a format that matlab can read
#Kristen Bathmann
#2018

ndate=/scratch1/NCEPDEV/da/Kristen.Bathmann/Analysis_util/ndate
wrkdir=/scratch1/NCEPDEV/stmp4/Kristen.Bathmann/fitdo2
ddir=/scratch1/NCEPDEV/da/Kristen.Bathmann/archive

ctl=wopr
exp=v161rt1
bdate=2021040200
edate=2021052000
##############################################
rm a.out
ifort -convert big_endian extract.f90
cdate=$bdate
[ ! -d ${wrkdir} ] && mkdir ${wrkdir}
cp a.out $wrkdir
cd $wrkdir
ctldir=${ddir}/${ctl}/fits
expdir=${ddir}/${exp}/fits
ten=10
onehundred=100
onethousand=1000
fh="00 06 24 48 72 96 120"

ch=( $fh )
nhr=${#ch[@]}
instr="acar acft raob surf"

#check for missing files
nmiss=0
dmiss=()
cdate=$bdate
while [[ $cdate -le $edate ]] ; do
  nmiss=0
  for ins in $instr ; do
    for var in $fh ; do
      if  [[ ! -f ${ctldir}/f${var}.${ins}.${cdate} ]] ; then
       nmiss=1
      fi
      if [[ ! -s ${ctldir}/f${var}.${ins}.${cdate} ]] ; then 
       nmiss=1
      fi
      if  [[ ! -f ${expdir}/f${var}.${ins}.${cdate} ]] ; then
       nmiss=1
      fi
      if  [[ ! -s ${expdir}/f${var}.${ins}.${cdate} ]] ; then
       nmiss=1
      fi
    done
  done
  if [[ $nmiss -eq 0 ]] ; then
     dincl+=("${cdate}")
  fi
  cdate=`$ndate +24 $cdate`
done
#now copy the files, skipping any dates missing fit files
for ins in $instr ; do
  nt=0
  for var in $fh ; do
    for cdate in "${dincl[@]}" ; do
        nt=`expr $nt + 1`
        if [ $nt -lt $ten ] ; then
          cp ${ctldir}/f${var}.${ins}.${cdate} c${ins}_000${nt}
          cp ${expdir}/f${var}.${ins}.${cdate} e${ins}_000${nt}
        elif [ $nt -lt $onehundred ] ; then
          cp ${ctldir}/f${var}.${ins}.${cdate} c${ins}_00${nt}
          cp ${expdir}/f${var}.${ins}.${cdate} e${ins}_00${nt}
        elif [ $nt -lt $onethousand ] ; then
          cp ${ctldir}/f${var}.${ins}.${cdate} c${ins}_0${nt}
          cp ${expdir}/f${var}.${ins}.${cdate} e${ins}_0${nt}
        else
          cp ${ctldir}/f${var}.${ins}.${cdate} c${ins}_{nt}
          cp ${expdir}/f${var}.${ins}.${cdate} e${ins}_{nt}
        fi
    done
  done
done
ntt=$nt
./a.out << EOF
$ntt $nhr
EOF
rm a.out
