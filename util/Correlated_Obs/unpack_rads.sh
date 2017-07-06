
cd $wrkdir
jobdir=jobdir_$start_nt
[ ! -d ${jobdir} ] && mkdir ${jobdir}
cd $jobdir
one=1
nt=$((start_nt-one))
cdate=$bdate

while [[ $cdate -le $edate ]] ; do
   while [[ ! -f $diagdir/radstat.gdas.$cdate ]] ; do
     cdate=`$ndate +06 $cdate`
     if [ $cdate -gt $edate ] ; then
        break
     fi
   done
   nt=$((nt + one))
   if [ $nt -lt 10 ] ; then
      fon=000$nt
   elif [ $nt -lt 100 ] ; then
      fon=00$nt
   elif [ $nt -lt 1000 ] ; then
      fon=0$nt
   else
      fon=$nt
   fi
   if [ ! -f danl_${fon} ];
   then
      cp $diagdir/radstat.gdas.${cdate} .
      tar --extract --file=radstat.gdas.${cdate} diag_${instr}_ges.${cdate}.gz diag_${instr}_anl.${cdate}.gz
      gunzip *.gz
      rm radstat.gdas.${cdate}
      if [ -f diag_${instr}_ges.${cdate} ];
      then
         mv diag_${instr}_anl.${cdate} danl_${fon}
         mv diag_${instr}_ges.${cdate} dges_${fon}
      else
         nt=$((nt - one))
      fi
   fi
   cdate=`$ndate +06 $cdate`
done
cp danl* $wrkdir
cp dges* $wrkdir
exit 0
