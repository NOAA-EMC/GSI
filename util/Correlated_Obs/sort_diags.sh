
cd $wrkdir
nt=1
one=1

while [[ $nt -lt $ntot ]] ; do
   if [ $nt -lt 10 ] ; then
      fon=000$nt
   elif [ $nt -lt 100 ] ; then
      fon=00$nt
   elif [ $nt -lt 1000 ] ; then
      fon=0$nt
   else
      fon=$nt
   fi
   nd=$nt
   fonn=$fon
   while [ ! -f danl_${fonn} ] || [ ! -f dges_${fonn} ] ; do
      if [ $nd -lt 10 ] ; then
         fonn=000$nd
      elif [ $nd -lt 100 ] ; then
         fonn=00$nd
      elif [ $nd -lt 1000 ] ; then
         fonn=0$nd
      else
         fonn=$nd
      fi
      nd=$(( nd+one ))
      if [ $nd -gt $ntot ] ; then
         break
      fi
   done
   if [ $nd -gt $ntot ] ; then
      break
   fi
   if [ $nd -gt $nt ] ; then
      mv danl_${fonn} danl_${fon}
      mv dges_${fonn} dges_${fon}
   fi
   nt=$(( nt + one))
    
done

