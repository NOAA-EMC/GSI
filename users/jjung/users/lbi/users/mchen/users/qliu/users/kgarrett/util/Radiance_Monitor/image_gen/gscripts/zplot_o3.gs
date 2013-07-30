'open pgbf06_gfs.ctl'
'open pgbanl_gfs.ctl'
'open pgbf06_prx.ctl'
'open pgbanl_prx.ctl'

'run /nfsuser/g01/emcsrc/wx20rt/web/gscripts/define_o3.gs'

xsize=x600
ysize=y700

'set t 2 5'
'query time'
date1=subwrd(result,3)
date2=subwrd(result,5)

'set mproj scaled'
'set t 1'
'set lon 180'
'set lat -95 95'
'clear'


* Get min/max limits for plot
'set gxout stat'
'd zgfsanl0'
 lin8=sublin(result,8)
 min1=subwrd(lin8,4)
 max1=subwrd(lin8,5)

*'d zprxanl0'
* lin8=sublin(result,8)
* min2=subwrd(lin8,4)
* max2=subwrd(lin8,5)
min2=min1
max2=max1

if (min1<=min2)
   ymin=min1
endif
if (min2<min1)
   ymin=min2
endif
ymin=0.95*ymin
if (ymin<0)
   ymin=0
endif
***set to fixed minimum value
***ymin=140

if (max1>=max2) 
   ymax=max1
endif
if (max2>max1)
   ymax=max2
endif
ymax=1.05*ymax
if (ymax>500) 
   ymax=500
endif

*Draw plot
'set gxout line'
'set vpage 0.0 8.5 5.0 10.5'
'set grads off'
'set axlim 'ymin' 'ymax

'set gxout line'
'set cmark 0'
'set cstyle 2'
'set ccolor 2'
'set cthick 10'
'd zgfsges0'

'set cmark 0'
'set cstyle 1'
'set ccolor 4'
'set cthick 10'
'd zgfsanl0'

'set cmark 0'
'set cstyle 2'
'set ccolor 4'
'set cthick 10'
*'d zprxges0'

'set cmark 0'
'set cstyle 1'
'set ccolor 4'
'set cthick 10'
*'d zprxanl0'

'draw ylab total ozone (DU)'
'set vpage off'

'set string 1 l 6'
'set strsiz 0.12 0.12'
'draw string 6.5 9.5 guess = red'
'draw string 6.5 9.3 analy = blue'
'draw string 6.5 9.0 guess = dash'
'draw string 6.5 8.8 analy = solid'


'set gxout stat'
'd zgfsinc0'
 lin8=sublin(result,8)
 min1=subwrd(lin8,4)
 max1=subwrd(lin8,5)

*'d zprxinc0'
* lin8=sublin(result,8)
* min2=subwrd(lin8,4)
* max2=subwrd(lin8,5)
min2=min1
max2=max1

if (min1<=min2)
   ymin=min1
endif
if (min2<min1)
   ymin=min2
endif
if (ymin<-100)
   ymin=-100
endif

if (max1>=max2) 
   ymax=max1
endif
if (max2>max1)
   ymax=max2
endif
if (ymax>100) 
   ymax=100
endif

'set gxout line'


'set vpage 0.0 8.5 0.0 5.5'
'set grads off'
'set axlim 'ymin' 'ymax

'set cmark 0'
'set ccolor 2'
'set cthick 10'
'd zgfsinc0'

'set cmark 0'
'set ccolor 4'
'set cthick 10'
*'d zprxinc0'
'draw ylab total ozone increment (DU)'
'set vpage off'

'draw string 5.7 4.5 GFS increment = red'
*'draw string 5.7 4.3 PRX increment = blue'

'set strsiz 0.15 0.15'
'draw string 0.5 10.5 Zonal mean total ozone'
'draw string 0.5 10.2 Valid:  'date1' to 'date2

'printim zonalo3.png 'xsize' 'ysize' white'

'quit'
