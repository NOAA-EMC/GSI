function name(arg)
*
* run dt scale base (contour)
* scale = contour interval
* base = white value
* optional contour=use black
*
d=subwrd(arg,1)
b=subwrd(arg,2)
contour=subwrd(arg,3)

if (d = '' | d = 'def')
   exit 0
endif
if (b = '' | b = 'def')
   'set cint ' d
   exit 0
endif


i=-5
if (b != 0)
  i=-4
endif
line='set clevs '
while(i <= 5)
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
if (contour = 'contour')
   'set ccols  98 98 98 98 98 98 98 98 98 98 98'
else
   'set ccols  47 45 44 43 42 99 22 23 24 25 27'
endif

exit 0
