* Script to plot given bias correction term for given satellite instrument
* 
* Two arguments are expected
*    plotfile = satellite id (name and number ... e.g., msu.014 = noaa-14 msu)

function plotsummary (args)

plotfile=subwrd(args,1)
xsize=subwrd(args,3)
ysize=subwrd(args,4)
platform=plotfile

say 'process plotfile 'plotfile
*'open 'plotfile'.ctl'

'q file'
lin1=sublin(result,1)
satnam=subwrd(lin1,4)
satnum=subwrd(lin1,5)
nlev=subwrd(lin1,6)
*say 'nlev='nlev

nfield=3
field.1=count
field.2=omg
field.3=cpen

title.1="number of observations passing quality control"
title.2="obs - ges"
title.3="contribution to penalty"

color.1=7
color.2=4
color.3=3
color.4=2


* Set time
'set t last'
'query time'
date1=subwrd(result,3)
*say 'date1='date1

'q dims'
lin5=sublin(result,5)
tlast=subwrd(lin5,9)
t1day=tlast-3
t7days=tlast-27
*say 'tlast,t1day,t7days='tlast' 't1day' 't7days


'clear'
'set grads off'
'set x 1 'nlev
'set y 1'

'set string 1 l 5'
'set strsiz 0.12 0.12'
'set xlopts 1 4 0.12'
'set ylopts 1 4 0.13'
  
'clear'
'set mproj off'

i=1
while (i<=nfield)

* Counts plot
if (field.i = "count")
   y1=8.1
   t1=t1day
   t2=tlast
   'set t 't1' 't2
   'define avg0=const('field.i',0,-u)'
   'set gxout stat'
   'd avg0'
   rec8=sublin(result,8)
   minvar=subwrd(rec8,4)
   maxvar=subwrd(rec8,5)

   ymin=minvar
   ymax=maxvar

   imiss=0
   if (ymin=0 & ymax=0)
      imiss=1
   endif

   yrange=ymax-ymin
   dy=0.1*yrange
   ymin=ymin-dy
   ymax=ymax+dy

   'set parea 0.7 7.4 'y1' 'y1+2.0
   'set grads off'
   'set gxout line'
   'set xlint 20'
   if (nlev<25) 
      'set xlint 1'
   endif
   'set vrange 'ymin' 'ymax    
   'set ccolor 4'
   'set cmark 0'
   'set cthick 6'
   tt=t1
   ic=1
   while (tt<=t2)
      'set t 'tt      
      'q time'
      cycle=subwrd(result,3)
      date=substr(cycle,1,8)
      'set ccolor 'color.ic
      'set cmark 0'
      'd avg0'
      'set strsiz 0.12'
      'set string 'color.ic' l 6'
      'draw string 7.45 'y1+2.1-ic*0.2' 'date
*      say 'tt,ic='tt' 'ic' 'date
      tt=tt+1
      ic=ic+1
   endwhile
   size=5/nlev
   if (size<0.05)
      size=0.05
   endif
   'set strsiz 'size' 0.1'
*   say 'nlev='nlev' string size='size
   ic=1
   while (ic<=nlev)
      'q gr2xy 'ic' 'ymin
      xpos=subwrd(result,3)
      ypos=subwrd(result,6)
      '!rm -f info.txt'
      '!cat 'plotfile'.ctl |grep "'ic', level" > info.txt'
      result=read(info.txt)
      rc=sublin(result,1)
      iuse=0
      if (rc = 0)
         info=sublin(result,2)
         level=subwrd(info,5)
         iuse=subwrd(info,8)
         error=subwrd(info,11)
      endif
      result=close(info.txt)
      flag=3
      if (iuse<0)
         flag=2
      endif
*      say 'ic,xpos,ypos,iuse,flag='ic' 'xpos' 'ypos' 'iuse' 'flag
      'set string 'flag' c 6'
*     'draw string 'xpos+0.13' 'ypos+0.05' `3L`0 '
*     'draw string 'xpos+0.0' 'ypos+0.05' `3L`0 '
      'draw string 'xpos+size/2.2' 'ypos+0.05' `3L`0 '
      ic=ic+1
   endwhile
   'set parea off'

   'set strsiz 0.12'
   'set string 3 l 6'
   'draw string 7.45 'y1+0.30' `3L`0 assim'
   'set string 2 l 6'
   'draw string 7.45 'y1+0.15' `3L`0 no assim'

   'set strsiz 0.12'
   'set string 1 c 6'
   'draw string 4.25 'y1+2.1' 'title.i

endif


* obs - ges plot
* say 'i,field.i='i' 'field.i
if (field.i = "omg")
   y1=y1-2.5
   'set t 'tlast
   t1=tlast
   t2=tlast
   'define avg1=ave(avg'field.i',t='t1',t='t2')'
   'define avg1=const(avg1,0,-u)'
*   say 't1,t2='t1' 't2
   'set gxout stat'
   ymin=999
   ymax=-999
   ic=1
   while (ic<=nlev)
      'set x 'ic
      'd avg1'
      rec8=sublin(result,8)
      minvar=subwrd(rec8,4)
      maxvar=subwrd(rec8,5)
      if (minvar>-10 & minvar<10)
         if (minvar<ymin)
            ymin=minvar
         endif
      endif
      if (maxvar>-10 & maxvar<10)
         if (maxvar>ymax)
            ymax=maxvar
         endif
      endif
*      say 'ic,minvar,maxvar,ymin,ymax='ic' 'minvar' 'maxvar' 'ymin' 'ymax
      ic=ic+1
   endwhile
   'set x 1 'nlev


   t1=t7days
   t2=tlast
   'define avg2=ave(avg'field.i',t='t1',t='t2')'
   'define avg2=const(avg2,0,-u)'
*   say 't1,t2='t1' 't2
   'set gxout stat'
   ic=1
   while (ic<=nlev)
      'set x 'ic
      'd avg2'
      rec8=sublin(result,8)
      minvar=subwrd(rec8,4)
      maxvar=subwrd(rec8,5)
      if (minvar>-10 & minvar<10)
         if (minvar<ymin)
            ymin=minvar
         endif
      endif
      if (maxvar>-10 & maxvar<10)
         if (maxvar>ymax)
            ymax=maxvar
         endif
      endif
*      say 'ic,minvar,maxvar,ymin,ymax='ic' 'minvar' 'maxvar' 'ymin' 'ymax
      ic=ic+1
   endwhile
   'set x 1 'nlev

   t1=1
   t2=tlast
   'define avg3=ave(avg'field.i',t='t1',t='t2')'
   'define avg3=const(avg3,0,-u)'
*   say 't1,t2='t1' 't2
   'set gxout stat'
   ic=1
   while (ic<=nlev)
      'set x 'ic
      'd avg3'
      rec8=sublin(result,8)
      minvar=subwrd(rec8,4)
      maxvar=subwrd(rec8,5)
      if (minvar>-10 & minvar<10)
         if (minvar<ymin)
            ymin=minvar
         endif
      endif
      if (maxvar>-10 & maxvar<10)
         if (maxvar>ymax)
            ymax=maxvar
         endif
      endif
*      say 'ic,minvar,maxvar,ymin,ymax='ic' 'minvar' 'maxvar' 'ymin' 'ymax
      ic=ic+1
   endwhile
   'set x 1 'nlev

   t1=tlast
   t2=tlast
   'define sdv1=ave(sdv'field.i',t='t1',t='t2')'
   'define sdv1=const(sdv1,0,-u)'
   'set gxout stat'
   ic=1
   while (ic<=nlev)
      'set x 'ic
      'd sdv1'
      rec8=sublin(result,8)
      minvar=subwrd(rec8,4)
      maxvar=subwrd(rec8,5)
      if (minvar>-10 & minvar<10)
         if (minvar<ymin)
            ymin=minvar
         endif
      endif
      if (maxvar>-10 & maxvar<10)
         if (maxvar>ymax)
            ymax=maxvar
         endif
      endif
*      say 'ic,minvar,maxvar,ymin,ymax='ic' 'minvar' 'maxvar' 'ymin' 'ymax
      ic=ic+1
   endwhile
   'set x 1 'nlev


   t1=t7days
   t2=tlast
   'define sdv2=ave(sdv'field.i',t='t1',t='t2')'
   'define sdv2=const(sdv2,0,-u)'
*   say 't1,t2='t1' 't2
   'set gxout stat'
   ic=1
   while (ic<=nlev)
      'set x 'ic
      'd sdv2'
      rec8=sublin(result,8)
      minvar=subwrd(rec8,4)
      maxvar=subwrd(rec8,5)
      if (minvar>-10 & minvar<10)
         if (minvar<ymin)
            ymin=minvar
         endif
      endif
      if (maxvar>-10 & maxvar<10)
         if (maxvar>ymax)
            ymax=maxvar
         endif
      endif
*      say 'ic,minvar,maxvar,ymin,ymax='ic' 'minvar' 'maxvar' 'ymin' 'ymax
      ic=ic+1
   endwhile
   'set x 1 'nlev

   t1=1
   t2=tlast
   'define sdv3=ave(sdv'field.i',t='t1',t='t2')'
   'define sdv3=const(sdv3,0,-u)'
*   say 't1,t2='t1' 't2
   'set gxout stat'
   ic=1
   while (ic<=nlev)
      'set x 'ic
      'd sdv3'
      rec8=sublin(result,8)
      minvar=subwrd(rec8,4)
      maxvar=subwrd(rec8,5)
      if (minvar>-10 & minvar<10)
         if (minvar<ymin)
            ymin=minvar
         endif
      endif
      if (maxvar>-10 & maxvar<10)
         if (maxvar>ymax)
            ymax=maxvar
         endif
      endif
*      say 'ic,minvar,maxvar,ymin,ymax='ic' 'minvar' 'maxvar' 'ymin' 'ymax
      ic=ic+1
   endwhile
   'set x 1 'nlev

   if (ymax = ymin) 
      ymin=-1
      ymax=1
   endif
   yrange=ymax-ymin
   dy=0.1*yrange
   ymin=ymin-dy
   ymax=ymax+dy
*   say 'ymin,ymax='ymin' 'ymax

   'set parea 0.7 7.4 'y1' 'y1+2.0
   'set grads off'
   'set gxout line'
   'set xlint 20'
   if (nlev<25) 
      'set xlint 1'
   endif
   'set vrange 'ymin' 'ymax    
   'set ccolor 4'
   'set cmark 0'
   'set cthick 6'

   color.1=2
   color.2=4
   color.3=6
   color.4=5
   color.5=3
   color.6=7
   day.1='1 cyc avg'
   day.2='7 day avg'
   day.3='month avg'
   day.4='1 cyc sdv'
   day.5='7 day sdv'
   day.6='month sdv'
   ii=0
   ic=1
   nc=6
   while (ic<=nc)
      'set ccolor 'color.ic
      'set cmark 0'
      if (ic<=3) 
         'set cstyle 1'
         'd avg'ic
         fact=2.1
      endif
      if (ic>3)
         ii=ii+1
         'set cstyle 2'
         'd sdv'ii
         fact=2.0
      endif
      'set strsiz 0.12'
      'set string 'color.ic' l 6'
      'draw string 7.45 'y1+fact-ic*0.2' 'day.ic
      ic=ic+1
   endwhile
   'set ccolor 1'
   'set cstyle 1'
   'set cmark 0'
   'd avg2-avg2'
   'set parea off'

   'set strsiz 0.12 0.12'
   'set string 1 c 6'
   'draw string 4.25 'y1+2.1' 'title.i
endif


* Contribution to penalty
if (field.i = "cpen")
   y1=y1-2.5
   'set t 'tlast
   t1=tlast
   t2=tlast
   'define avg1=ave('field.i',t='t1',t='t2')'
   'define avg1=const(avg1,0,-u)'
*   say 't1,t2='t1' 't2
   'set gxout stat'
   'd avg1'
   rec8=sublin(result,8)
   minvar=subwrd(rec8,4)
   maxvar=subwrd(rec8,5)
   ymin=minvar
   ymax=maxvar

   t1=t1day
   t2=tlast
   'define avg2=ave('field.i',t='t1',t='t2')'
   'define avg2=const(avg2,0,-u)'
*   say 't1,t2='t1' 't2
   'set gxout stat'
   'd avg2'
   rec8=sublin(result,8)
   minvar=subwrd(rec8,4)
   maxvar=subwrd(rec8,5)
   if (minvar<ymin)
      ymin=minvar
   endif
   if (maxvar>ymax)
      ymax=maxvar
   endif

   t1=t7days
   t2=tlast
   'define avg3=ave('field.i',t='t1',t='t2')'
   'define avg3=const(avg3,0,-u)'
*   say 't1,t2='t1' 't2
   'set gxout stat'
   'd avg3'
   rec8=sublin(result,8)
   minvar=subwrd(rec8,4)
   maxvar=subwrd(rec8,5)
   if (minvar<ymin)
      ymin=minvar
   endif
   if (maxvar>ymax)
      ymax=maxvar
   endif

   t1=1
   t2=tlast
   'define avg4=ave('field.i',t='t1',t='t2')'
   'define avg4=const(avg4,0,-u)'
*   say 't1,t2='t1' 't2
   'set gxout stat'
   'd avg4'
   rec8=sublin(result,8)
   minvar=subwrd(rec8,4)
   maxvar=subwrd(rec8,5)
   if (minvar<ymin)
      ymin=minvar
   endif
   if (maxvar>ymax)
      ymax=maxvar
   endif

   yrange=ymax-ymin
   dy=0.1*yrange
   ymin=ymin-dy
   ymax=ymax+dy

   'set parea 0.7 7.4 'y1' 'y1+2.0
   'set grads off'
   'set gxout line'
   'set xlint 20'
   if (nlev<25) 
      'set xlint 1'
   endif
   'set vrange 'ymin' 'ymax    
   'set ccolor 4'
   'set cmark 0'
   'set cthick 6'

   color.1=2
   color.2=3
   color.3=4
   color.4=5
   day.1='1 cycle'
   day.2='1 day avg'
   day.3='7 day avg'
   day.4='month avg'
   ic=1
   nc=4
   while (ic<=nc)
      'set ccolor 'color.ic
      'set cmark 0'
      'd avg'ic
      'set strsiz 0.12'
      'set string 'color.ic' l 6'
      'draw string 7.45 'y1+2.1-ic*0.2' 'day.ic
      ic=ic+1
   endwhile
   'set parea off'

   'set strsiz 0.12 0.12'
   'set string 1 c 6'
   'draw string 4.25 'y1+2.1' 'title.i
endif

i=i+1
endwhile

'set string 1 l 6'
'set strsiz 0.15 0.15'
'draw string 0.2 10.80 platform:  'plotfile
'draw string 0.2 10.55 valid   :  'date1
'set string 1 c 6'
'draw string 4.05 2.5  l  e  v  e  l      n  u  m  b  e  r'

outfile=plotfile'.summary.png'
'printim 'outfile' 'xsize' 'ysize' white'

return
endfile

function digs(string,num)
  nc=0
  pt=""
  while(pt = "")
    nc=nc+1
    zzz=substr(string,nc,1)
    if(zzz = "." | zzz = ""); break; endif
  endwhile
  end=nc+num
  str=substr(string,1,end)
return str

