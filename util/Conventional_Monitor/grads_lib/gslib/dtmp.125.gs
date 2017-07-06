function name(arg)

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

'set clevs  -1.125 -1 -.875 -.75 -.625 -.5 -.375 -.25 -.125 .125 .25 .375 .5 .625 .75 .875 1 1.125'
'set ccols  49 48 47 46 45 44 43 42 41 1 21 22 23 24 25 26 27 28 29'

'd ' arg
'set line 0'
'run cbarb'
say  arg ' plotted'

'set ccolor 2'
exit 0
