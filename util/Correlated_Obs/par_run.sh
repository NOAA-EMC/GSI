

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
$nt $type $cloud $angle $instr $wave_out $err_out $corr_out $kreq $method $cov_method $chan_set $time_sep $bsize $bcen
EOF

cp Rcov_$instr $savdir
[ -f Rcorr_$instr ] && cp Rcorr_$instr $savdir
[ -f wave_$instr ] && cp wave_$instr $savdir
[ -f err_$instr ] && cp err_$instr $savdir
exit 0
