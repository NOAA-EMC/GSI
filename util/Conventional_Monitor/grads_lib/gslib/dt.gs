function name(arg)
*
* run dt variable scale base
*
variable=subwrd(arg,1)
d=subwrd(arg,2)
b=subwrd(arg,3)

if (d = '')
   d=0
endif
if (b = '')
   b=0
endif

say d

'set background 1'
'set line 0'
'set annot 0'
'set map 0'
'set xlopts 0'
'set ylopts 0'

* 'clear'
'run clearX'
'set grads off'
'set ccolor 0'
'set gxout shaded'

'run rgbset.gs'
i=-9
if (b != 0)
  i=-8
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
'set ccols  49 47 45 44 43 42 41 1 21 22 23 24 25 27 29'

'd ' variable
'set line 0'
'run cbarb'
say  arg ' plotted'

'set ccolor 2'
exit 0
