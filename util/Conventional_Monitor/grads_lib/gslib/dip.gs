function name(arg)
*
* run dt variable scale 
* assumes all numbers are positive
* makes [0,d] white
*
d=subwrd(arg,1)
rev=0
if (d < 0)
  d = -d
  rev=1
endif
say 'interval is ' d
if (d = '' | d = 'def')
   exit 0
endif


line='set clevs '
i=1
while(i <= 9)
  line=line ' ' i*d
  i=i+1
endwhile
say line
line
if (rev = 0)
   say 'set ccols  99 22 23 24 25 27 29 32 35 38'
   'set ccols  99 22 23 24 25 27 29 32 35 38'
else
   'set ccols  38 35 32 29 27 25 24 23 22 99'
   say 'set ccols  38 35 32 29 27 25 24 23 22 99'
endif
exit 0
