function name(arg)

'set background 1'
'set line 0'
'set annot 0'
'set map 0'
'set xlopts 0'
'set ylopts 0'

'run clearX'
'set grads off'
'set ccolor 0'
'set gxout shaded'

'run rgbset.gs'

'set clevs  -2.5 -2 -1.5 -1 -0.5 0.5 1 1.5 2 2.5'
'set ccols  49 47 45 43 41 1 21 23 25 27 29'

'd ' arg
'set line 0'
'run cbarb'
say  arg ' plotted'
'set ccolor 2'

exit 0
/* 'set clevs  -9 -8 -7 -6 -5 -4 -3 -2 -1 1  2  3  4  5  6  7  8  9' */
