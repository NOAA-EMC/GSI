

if [ ! -z "$NP" ] ; then
   export OMP_NUM_THREADS=$NP
fi
cdate=$bdate
nt=0
one=1
cd $wrkdir
while [[ $nt -le $ntot ]] ; do
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
   if [ ! -f dges_${fon} ];
   then
      nt=$((nt-one))
      break
   fi
   
done

./cov_calc <<EOF
$nt $type $cloud $angle $instr $wave_out $err_out $corr_out $kreq $infl $inflsurf $inflwv $method $cov_method $chan_set $time_sep $bsize $bcen $netcdf
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
