function name(arg)
*
* run page position
*  position = top bottom left or right
*             q1 q2
*             q3 q4
*   (q=quadrent)
*  set virtual page
*
* 
*  v1.1.1
* bigger margins 
*
'set vpage off'
'query gxinfo'
rec2 = sublin(result,2)
xlo = 0
xhi = subwrd(rec2,4)
ylo = 0
yhi = subwrd(rec2,6)

if (yhi > xhi)
*  xlo=0.5
*  xhi=xhi-0.5
*  ylo = yhi - (xhi - xlo)
*  yhi=yhi-1
*  ylo=ylo-1
   ylo=1
   yhi=yhi-1
else
  xhi=xhi-1.25
  xlo=xlo+1.25
endif

xmid = 0.5 * (xlo + xhi)
ymid = 0.5 * (ylo + yhi)

if ( arg = 'left' )
  'set vpage ' xlo ' ' xmid ' ' ylo ' ' yhi
endif
if ( arg = 'right' )
  'set vpage ' xmid ' ' xhi ' ' ylo ' ' yhi
endif

if ( arg = 'top' )
  'set vpage ' xlo ' ' xhi ' ' ymid ' ' yhi
endif
if ( arg = 'bottom' )
  'set vpage ' xlo ' ' xhi ' ' ylo ' ' ymid
endif

if ( arg = 'q1' )
  'set vpage ' xlo ' ' xmid ' ' ymid ' ' yhi
endif
if ( arg = 'q2' )
  'set vpage ' xmid ' ' xhi ' ' ymid ' ' yhi
endif
if ( arg = 'q3' )
  'set vpage ' xlo ' ' xmid ' ' ylo ' ' ymid
endif
if ( arg = 'q4' )
  'set vpage ' xmid ' ' xhi ' ' ylo ' ' ymid
endif
if ( arg = '' )
  'set vpage ' xlo ' ' xhi ' ' ylo ' ' yhi
endif

ix=substr(arg,1,1)
iy=substr(arg,2,1)
nx=substr(arg,4,1)
ny=substr(arg,5,1)

if (nx >= 1 & nx <= 9 & ny >= 1 & ny <= 9) 
  x0 = xlo + (xhi - xlo) * (ix - 1) / nx
  x1 = xlo + (xhi - xlo) * (ix    ) / nx
  y0 = ylo + (yhi - ylo) * (iy - 1) / ny
  y1 = ylo + (yhi - ylo) * (iy    ) / ny
  'set vpage ' x0 ' ' x1 ' ' y0 ' ' y1
  say 'set vpage ' x0 ' ' x1 ' ' y0 ' ' y1
endif

'set grads off'

