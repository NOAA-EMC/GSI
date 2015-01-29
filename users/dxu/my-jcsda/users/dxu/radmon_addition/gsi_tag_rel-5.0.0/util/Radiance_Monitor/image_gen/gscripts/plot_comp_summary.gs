* Script to plot given bias correction term for given satellite instrument
* 
* Two arguments are expected
*    plotfile = satellite id (name and number ... e.g., msu.014 = noaa-14 msu)
*
*  this is an experimental version using the new time .ieee_d files for oex only.
*

function plotsummary (args)

plotfile=subwrd(args,1)
sub_avg=0
suffix1=subwrd(args,2)
suffix2=subwrd(args,3)
xsize=subwrd(args,4)
ysize=subwrd(args,5)
platform=plotfile

say 'process plotfile 'plotfile

'q file'
lin1=sublin(result,1)
satnam=subwrd(lin1,4)
satnum=subwrd(lin1,5)
nchan=subwrd(lin1,6)

nfield=4
field.1=count
field.2=total
field.3=omgbc
field.4=penalty

title.1="number of observations passing quality control"
title.2="total bias correction (K)"
title.3="ges_(w/bias cor) - obs (K)"
title.4="contribution to penalty"

color.1=7
color.2=4
color.3=3
color.4=2

red    = 2
blue   = 4
green  = 3
ltblue = 5
yellow = 8
ygreen = 10

* Determine number of channels
'q file'
lin1=sublin(result,1)
nchan=subwrd(lin1,6)

say 'nchan='nchan
*say 'nregion='nregion

* Set time
'set t 1 last'
'query time'
date1=subwrd(result,3)
date2=subwrd(result,5)
say 'date1='date1

'q dims'
lin5=sublin(result,5)
tfirst=subwrd(lin5,11)
tlast=subwrd(lin5,13)
t1day=tlast-3
t30days=tlast-119
*t30days=tlast-79
t30days=1
*say 'tlast,t1day,t30days='tlast' 't1day' 't30days

*
*  Determine number of days in plot (4 cycles per day)
*
rslt=tlast-tfirst
if (rslt > 4)
  mrslt=math_mod(rslt, 4)
  ndays=(rslt-mrslt)/4
else
  ndays=1
endif
say 'rslt,mrslt,ndays = 'rslt' 'mrslt' 'ndays


'set t last'

'clear'
'set grads off'
'set x 1 'nchan
'set y 1'

'set string 1 l 5'
'set strsiz 0.12 0.12'
'set xlopts 1 4 0.12'
'set ylopts 1 4 0.13'
  
'clear'
'set mproj off'

i=1
while (i<=nfield)

*****************************
* Counts plot
*****************************
if (field.i = "count")
   y1=8.1

   'set t 'tlast
   t1=t30days
   t2=tlast
   'define avg1=ave(count.1, t='t1',t='t2')'
   'define avg1=const(avg1,0,-u)'
   'define avg2=ave(count.2, t='t1',t='t2')'
   'define avg2=const(avg2,0,-u)'

   t1=t1day
   t2=tlast
   'set t 't1' 't2
*   'define avg0=const('field.i',0,-u)'
   'define avg0=const(count.1,0,-u)'
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
   if (nchan<20) 
      'set xlint 1'
   endif
   'set vrange 'ymin' 'ymax    
   'set ccolor 4'
   'set cmark 0'
   'set cthick 6'
   tt=t1
   ic=1
*   while (tt<=t2)
*      'set t 'tt      
*      'q time'
*      cycle=subwrd(result,3)
*      date=substr(cycle,1,8)
*      'set ccolor 'color.ic
*      'set cmark 0'
*      'd avg0'
*      'set strsiz 0.12'
*      'set string 'color.ic' l 6'
*      'draw string 7.45 'y1+2.1-ic*0.2' 'date
*      say 'tt,ic='tt' 'ic' 'date
*      tt=tt+1
*      ic=ic+1
*   endwhile
  'set ccolor 'blue
  'set cmark 0'
  'd avg1'
  'set strsiz 0.12'
  'set string 'blue' l 6'
  'draw string 7.45 'y1+2.1-0.2' 'suffix1

  'set ccolor 'green
  'set cmark 0'
  'd avg2'
  'set strsiz 0.12'
  'set string 'green' l 6'
  'draw string 7.45 'y1+2.1-0.4' 'suffix2

   size=5/nchan
   if (size<0.05)
      size=0.05
   endif
   'set strsiz 'size' 0.1'
*   say 'nchan='nchan' string size='size
   ic=1
   while (ic<=nchan)
      'q gr2xy 'ic' 'ymin
      xpos=subwrd(result,3)
      ypos=subwrd(result,6)
      '!rm -f info.txt'
      '!cat 'suffix1'.'plotfile'.ctl |grep "'ic', channel" > info.txt'
      result=read(info.txt)
      rc=sublin(result,1)
      iuse=0
      if (rc = 0)
         info=sublin(result,2)
         channel=subwrd(info,5)
         iuse=subwrd(info,8)
         error=subwrd(info,11)
         wavelength=subwrd(info,14)
         freq=subwrd(info,17)
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

*****************************
* Total bias correction plot
*****************************
if (field.i = "total")
   ymin=999
   ymax=-999
   y1=y1-2.5
   'set x 1 'nchan
   'set t 'tlast
   'set x 1 'nchan

   t1=t30days
   t2=tlast
   'define avgtotal31=ave(total.1/count.1,t='t1',t='t2')'
   'define avgtotal31=const(avgtotal31,0,-u)'

   'define avgtotal32=ave(total.2/count.2,t='t1',t='t2')'
   'define avgtotal32=const(avgtotal32,0,-u)'

   'set gxout stat'
   ic=1
   while (ic<=nchan)
      'set x 'ic
      'd avgtotal31'
      rec8=sublin(result,8)
      rec11=sublin(result,11)
      minvar=subwrd(rec8,4)
      maxvar=subwrd(rec8,5)
      if( minvar < ymin )
         ymin=minvar
      endif
      if( maxvar > ymax )
         ymax=maxvar
      endif

      'd avgtotal32'
      rec8=sublin(result,8)
      rec11=sublin(result,11)
      minvar=subwrd(rec8,4)
      maxvar=subwrd(rec8,5)
      if( minvar < ymin )
         ymin=minvar
      endif
      if( maxvar > ymax )
         ymax=maxvar
      endif

*      say 'total, 30 days, minvar,maxvar,ymin,ymax='minvar' 'maxvar' 'ymin' 'ymax
      ic=ic+1
   endwhile

   'set x 1 'nchan

   yrange=ymax-ymin
   dy=0.1*yrange
   ymin=ymin-dy
   ymax=ymax+dy

   'set parea 0.7 7.4 'y1' 'y1+2.0
   'set grads off'
   'set gxout line'
   'set xlint 20'
   if (nchan<20) 
      'set xlint 1'
   endif
   'set vrange 'ymin' 'ymax    
   'set ccolor 4'
   'set cmark 0'
   'set cthick 6'

   color.1=2
   color.2=3
*   color.3=4

*   day.1='1 cycle'
*   day.2='1 day avg'
*   day.3=ndays' day avg'
*   if (sub_avg=1)
*      ic=1
*   else
      ic=3
*   endif
*   nc=3
*   while (ic<=nc)
      'set ccolor 'blue
      'set cmark 0'
      say 'drawing avgtotal'
      'd avgtotal31'

      'set cmark 0'
      'set ccolor 'green
      'd avgtotal32'

      'set strsiz 0.12'
      'set string 'blue' l 6'
      'draw string 7.45 'y1+2.1-3*0.2' 'suffix1
      'set string 'green' l 6'
      'draw string 7.45 'y1+2.1-4*0.2' 'suffix2
      ic=ic+1
*   endwhile
   'set ccolor 1'
   'set cstyle 1'
   'set cmark 0'
   'd avgtotal31-avgtotal31'
   'set parea off'

   'set strsiz 0.12 0.12'
   'set string 1 c 6'
   'draw string 4.25 'y1+2.1' 'title.i
endif


*****************************
* Ges(w/bias cor) - obs plot
*****************************
if (field.i = "omgbc")
   say 'determining omgbc'
   y1=y1-2.5
   
   ymin=999
   ymax=-999

   'set x 1 'nchan
   t1=t30days
   t2=tlast

*  calculate avg and sdv for suffix 1
   'define avgcnt=ave(count.1,t='t1',t='t2')'
   'define avgomgbc=ave(omgbc.1,t='t1',t='t2')'
   'define avgomgbc2=ave(omgbc2.1,t='t1',t='t2')'
   'define avgcnt1=ave(count.1-1,t='t1',t='t2')'

   'define rterm1=1/avgcnt'
   'define avg2=rterm1*avgomgbc'

   'define rterm2=1/(avgcnt1)'
   'define svar=(abs(avgcnt*avgomgbc2-avgomgbc*avgomgbc))*rterm1*rterm2'
   'define sdv1=sqrt(svar)'
   'define sdv1=const(sdv1,0,-u)'

   'define avgomgbc1=avg2'
   'define avgomgbc1=const(avgomgbc1,0,-u)'

*  calculate avg and sdv for suffix 2
   'define avgcnt=ave(count.2,t='t1',t='t2')'
   'define avgomgbc=ave(omgbc.2,t='t1',t='t2')'
   'define avgomgbc2=ave(omgbc2.2,t='t1',t='t2')'
   'define avgcnt1=ave(count.2-1,t='t1',t='t2')'

   'define rterm1=1/avgcnt'
   'define avg2=rterm1*avgomgbc'

   'define rterm2=1/(avgcnt1)'
   'define svar=(abs(avgcnt*avgomgbc2-avgomgbc*avgomgbc))*rterm1*rterm2'
   'define sdv2=sqrt(svar)'
   'define sdv2=const(sdv2,0,-u)'

   'define avgomgbc2=avg2'
   'define avgomgbc2=const(avgomgbc2,0,-u)'

*   say 't1,t2='t1' 't2

**************************************
*  Determine y range
**************************************
   'set gxout stat'
   ic=1
   while (ic<=nchan)
      'set x 'ic
      'd avgomgbc1'
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

      'd avgomgbc2'
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
     
      ic=ic+1
   endwhile
   'set x 1 'nchan

   if (ymax = ymin) 
      ymin=-1
      ymax=1
   endif
   yrange=ymax-ymin
   dy=0.1*yrange
   ymin=ymin-dy
   ymax=ymax+dy

   'set parea 0.7 7.4 'y1' 'y1+2.0
   'set grads off'
   'set gxout line'
   'set xlint 20'
   if (nchan<20) 
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

   day.1='1 cyc avg'
   day.2='avg ' % 'suffix1'
   day.3='1 cyc sdv'
   day.4='sdv ' % 'suffix1'

**************************************
*  plot data
**************************************
say 'plotting omgbc'
   'set ccolor 'blue 
   'set cmark 0'
   'set cstyle 1'
   'd avgomgbc1'
   'set strsiz 0.12'
   'set string 'blue' l 6'
   'draw string 7.45 'y1+2.1-0.2' avg 'suffix1

   'set ccolor 'ltblue 
   'set cmark 0'
   'set cstyle 2'
   'd sdv1'
   'set strsiz 0.12'
   'set string 'ltblue' l 6'
   'draw string 7.45 'y1+2.1-0.4' sdv 'suffix1

   'set ccolor 'green 
   'set cmark 0'
   'set cstyle 1'
   'd avgomgbc2'
   'set strsiz 0.12'
   'set string 'green' l 6'
   'draw string 7.45 'y1+2.1-0.7' avg 'suffix2

   'set ccolor 'ygreen 
   'set cmark 0'
   'set cstyle 2'
   'd sdv2'
   'set strsiz 0.12'
   'set string 'ygreen' l 6'
   'draw string 7.45 'y1+2.1-0.9' sdv 'suffix2

*   ii=0
*   ic=1
*   nc=4
*   while (ic<=nc)
*      'set ccolor 'color.ic
*      'set cmark 0'
*      if (ic<=2) 
*         if (ic=2 | sub_avg=1)
*            'set cstyle 1'
*            'd avgomgbc1'
*            fact=2.1
*         endif
*      endif
*      if (ic>2)
*         ii=ii+1
*         'set cstyle 2'
*         if (ii>1 | sub_avg=1)
*            'd sdv1'
*            fact=2.0
*         endif
*      endif
*      'set strsiz 0.12'
*      'set string 'color.ic' l 6'
*      if (sub_avg=1 | ic=2 | ic=4)
*         'draw string 7.45 'y1+fact-ic*0.2' 'day.ic
*      endif
*      ic=ic+1
*   endwhile

*  draw 0 line in black 
   'set ccolor 1'
   'set cstyle 1'
   'set cmark 0'
   'd avgomgbc1-avgomgbc1'
   'set parea off'

   'set strsiz 0.12 0.12'
   'set string 1 c 6'
   'draw string 4.25 'y1+2.1' 'title.i
endif


* Contribution to penalty
if (field.i = "penalty")
   ymin=999
   ymax=-999

   y1=y1-2.5
   'set t 'tlast

   t1=t30days
   t2=tlast
   'define avg31=ave(penalty.1/count.1,t='t1',t='t2')'
   'define avg31=const(avg31,0,-u)'
   'set gxout stat'
   'd avg31'
   rec8=sublin(result,8)
   minvar=subwrd(rec8,4)
   maxvar=subwrd(rec8,5)
   if (minvar<ymin)
      ymin=minvar
   endif
   if (maxvar>ymax)
      ymax=maxvar
   endif

   'define avg32=ave(penalty.2/count.2,t='t1',t='t2')'
   'define avg32=const(avg32,0,-u)'
   'set gxout stat'
   'd avg32'
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
   if (nchan<20) 
      'set xlint 1'
   endif
   'set vrange 'ymin' 'ymax    
   'set ccolor 4'
   'set cmark 0'
   'set cthick 6'

*  color.1=2
*  color.2=3
*  color.3=4
*   day.1='1 cycle'
*  day.2='1 day avg'
*  day.3=ndays' day avg'
*  if (sub_avg=1)
*     ic=1
*  else
*     ic=3
*  endif
*  nc=3
*  while (ic<=nc)
      'set ccolor 'blue
      'set cmark 0'
      'd avg31'
      'set strsiz 0.12'
      'set string 'blue' l 6'
      'draw string 7.45 'y1+2.1-3*0.2' 'suffix1

      'set ccolor 'green
      'set cmark 0'
      'd avg32'
      'set strsiz 0.12'
      'set string 'green' l 6'
      'draw string 7.45 'y1+2.1-4*0.2' 'suffix2

*      ic=ic+1
*  endwhile

   'set parea off'

   'set strsiz 0.12 0.12'
   'set string 1 c 6'
   'draw string 4.25 'y1+2.1' 'title.i
endif

i=i+1
endwhile

'set string 1 l 6'
'set strsiz 0.15 0.15'
'draw string 0.2 10.80 platform:  'plotfile' , 'suffix1' , 'suffix2
'draw string 0.2 10.55 valid   :  'date1' - 'date2
'set string 1 c 6'
'draw string 4.05 0.2 c  h  a  n  n  e  l      n  u  m  b  e  r'

outfile=plotfile'.comp.png'
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

