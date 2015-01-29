function name(arg)
*
* run dt variable scale base
*
v=subwrd(arg,1)
s=subwrd(arg,2)
b=subwrd(arg,3)

'set gxout shaded'
'set cterp off'
'defint ' s ' ' b
'd ' v

'cbar98.gs'

'set gxout contour'
'set clevs'
'set ccolor 98'
'defint ' s ' ' b
'set ccols 98'
'd ' v
'set gxout shaded'
