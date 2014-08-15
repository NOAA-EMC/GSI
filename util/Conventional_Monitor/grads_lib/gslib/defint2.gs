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


i=-4
if (b != 0)
  i=-3
endif
line='set clevs '
while(i <= 4)
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
'set ccols  45 44 43 42 99 22 23 24 25'
exit 0
