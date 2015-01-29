function name(arg)
*
* run dt variable scale base
*
d=subwrd(arg,1)
b=subwrd(arg,2)

if (d = '' | d = 'def')
   exit 0
endif
if (b = '' | b = 'def')
  'set cint ' d
   exit 0
endif


i=-6
if (b != 0)
  i=-5
endif
line='set clevs '
while(i <= 6)
  if (b != 0)
     line=line ' ' i*d+b
  endif
  if (b = 0 & i != 0)
     line=line ' ' i*d+b
  endif
  i=i+1
endwhile
say line
line
say 'set ccols  49 47 45 44 43 42 99 22 23 24 25 27 29'
'set ccols  49 47 45 44 43 42 99 22 23 24 25 27 29'
exit 0
