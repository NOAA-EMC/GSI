*
* clears page only if it is a full screen
*
exit 0
'query gxinfo'
rec2 = sublin(result,2)
rec3 = sublin(result,3)
rec4 = sublin(result,4)
xdim = subwrd(rec2,4)
ydim = subwrd(rec2,6)

if ( xdim < ydim )
  small=xdim
  big=ydim
else
  small=ydim
  big=xdim
endif

say ' page size in clearX is ' xdim ' ' ydim
if (small = 8.5 & big = 11)
  say 'clearing 1'
  'clear'
endif
exit 0
