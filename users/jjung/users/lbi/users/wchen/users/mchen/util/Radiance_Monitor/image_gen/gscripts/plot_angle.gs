* Script to plot given bias correction term for given satellite instrument
* 
* Two arguments are expected
*    plotfile = satellite id (name and number ... e.g., msu.014 = noaa-14 msu)
*    field  = field to plot  (valid strings are:  count total fixang lapse lapse2 const scangl clw

function plottime (args)

plotfile=subwrd(args,1)
field=subwrd(args,2)
xsize=subwrd(args,3)
ysize=subwrd(args,4)
platform=plotfile

say 'process 'field' from 'plotfile

debug=0

'q file'
lin1=sublin(result,1)
satnam=subwrd(lin1,4)
satnum=subwrd(lin1,5)
nchan=subwrd(lin1,6)

if (field = count)
 type="number of observations"
endif
if (field = omgnbc)
 type="ges_(w/o bias cor) - obs (K)"
endif
if (field = total)
 type="total bias correction (K)"
endif
if (field = omgbc)
 type="ges_(w/bias cor) - obs (K)"
endif
if (field = penalty)
 type="contribution to penalty"
endif

* Determine number of channels and regions
'q file'
lin1=sublin(result,1)
nchan=subwrd(lin1,6)
lin5=sublin(result,5)
nregion=subwrd(lin5,9)


*say 'nchan='nchan
*say 'nregion='nregion

* Set time
'set t 1'
'query time'
date1=subwrd(result,3)
date2=subwrd(result,5)

*say 'date1='date1
*say 'date2='date2

region=1
while (region<=nregion)

*say 'top of region loop with region='region

'!rm -f area.txt'
'!cat 'plotfile'.ctl |grep "region= 'region' " > area.txt'
result=read(area.txt)
rc=sublin(result,1)
area="uknown"
if (rc = 0)
   info=sublin(result,2)
   area=substr(info,14,60)
endif
result=close(area.txt)

'!cat 'plotfile'.ctl |grep "n= 'region' " > area.txt'
*say 'area = 'area


'clear'
'set grads off'
'set missconn on'
'set lon -55 55'
if (plotfile="goes.11" | plotfile="goes.12")
   'set lon 0 65'
endif
if (plotfile="goesimg.008" | plotfile="goesimg.010" | plotfile="goesimg.012")
   'set lon 0 65'
endif
'set z 'region
'set mproj off'

'set string 1 l 5'
'set strsiz 0.12 0.12'
'set xlopts 1 4 0.12'
'set ylopts 1 4 0.13'

fr=0
i=1
chn=1
nt=3
while (chn<=nchan)
*   say 'top of channel loop with chn='chn
   'set y 'chn
   if (field != "count" & field != "penalty")
      it=1
      nt=3
      while (it<=nt)
         'define avg'it'=avg'field'(t='it')'
         'define sdv'it'=sdv'field'(t='it')'
         it=it+1
      endwhile
   endif
   if (field = "count")
      it=1
      nt=3
      while (it<=nt)  
         'define avg'it'='field'(t='it')'
         it=it+1
      endwhile
      'set gxout stat'
      'd avg3'
      rec14=sublin(result,14)
      avgsdv=subwrd(rec14,2)
      'define sdv3='field'(t=3)'
      'set gxout line'
   endif
   if (field = "penalty")
      it=1
      nt=3
      while (it<=nt)
         'define avg'it'='field'(t='it')'
         it=it+1
      endwhile
      'set gxout stat'
      'd avg3'
      rec14=sublin(result,14)
      avgsdv=subwrd(rec14,2)
      'define sdv3='field'(t=3)'
      'set gxout line'
   endif


   chi=chn
   if (i=1) 
      'clear'
      y1=7.3
      clo=chn
      clast=clo+3
   endif
   if (i>1 & i<4) 
      y1=y1-2.4
   endif
   if (i=4) 
      y1=y1-2.4
   endif

   minvar0= 9999
   maxvar0=-9999
   minsdv0= 9999
   maxsdv0=-9999

   'set gxout stat'
   it=1
   while (it<=nt)
      'd avg'it
      rec7=sublin(result,7)
      rec8=sublin(result,8)
      rec11=sublin(result,11)
      valvar=subwrd(rec7,8)
      minvar=subwrd(rec8,4)
      maxvar=subwrd(rec8,5)
      avgvar=subwrd(rec11,2)
*      say ' avg'it' min,max,avg='minvar','maxvar','avgvar

      if (field != "count" & field != "penalty") 
         'd sdv'it
         rec7=sublin(result,7)
         rec8=sublin(result,8)
         rec11=sublin(result,11)
         valsdv=subwrd(rec7,8)
         minsdv=subwrd(rec8,4)
         maxsdv=subwrd(rec8,5)
         avgsdv=subwrd(rec11,2)
*         say 'sdv'it' min,max,avg='minsdv','maxsdv','avgsdv
      endif

      if (field = "omgnbc")
         'set z 1'
*        NOTE:  multiply by -1 since fixang is o-g.  we plot g-o.
         'd -1*fixang(t=1)'
         rec7=sublin(result,7)
         rec8=sublin(result,8)
         valfix=subwrd(rec7,8)
         minfix=subwrd(rec8,4)
         maxfix=subwrd(rec8,5)
         if (minfix<minvar)
            minvar=minfix
         endif
         if (maxfix>maxvar)
            maxvar=maxfix
         endif
*        NOTE:  multiply by -1 since satang is o-g.  we plot g-o.
         'd -1*satang(t=1)'
         rec7=sublin(result,7)
         rec8=sublin(result,8)
         valsat=subwrd(rec7,8)
         minsat=subwrd(rec8,4)
         maxsat=subwrd(rec8,5)
         if (minsat<minvar)
            minvar=minsat
         endif
         if (maxsat>maxvar)
            maxvar=maxsat
         endif
         'set z 'region
      endif

      'd penalty'
      rec8=sublin(result,8)
      rec11=sublin(result,11)
      ratio=subwrd(rec11,2)
*      say 'sdv'it' ratio='ratio

 
      if (field = "count" | field = "penalty")
         minsdv=minvar
         maxsdv=maxvar
         valsdv=valvar
      endif

*      say 'valvar,valsdv='valvar' 'valsdv
      if (minvar<minvar0 & valvar!=0)
         minvar0=minvar
      endif
      if (maxvar>maxvar0 & valvar!=0)
         maxvar0=maxvar
      endif
      if (minsdv<minsdv0 & valsdv!=0)
         minsdv0=minsdv
      endif
      if (maxsdv>maxsdv0 & valsdv!=0)
         maxsdv0=maxsdv
      endif

      it=it+1
   endwhile

   '!rm -f info.txt'
   '!cat 'plotfile'.ctl |grep "'chn', channel" > info.txt'
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
*   say 'channel,iuse,error,freq,wavelength = 'channel', 'iuse', 'error', 'freq', 'wavelength
   
   'set strsiz 0.12 0.12'
   'set string 1 l 6'
   'draw string 0.1 'y1+2.4' channel 'channel
   'draw string 0.1 'y1+2.2' `3X`0 'digs(ratio,4)
   'draw string 0.1 'y1+2.0' f 'freq' GHz'
   'draw string 0.1 'y1+1.8' `3l`0 'wavelength' `3m`0m'
*   'set string 4 l 6'
   if (field != "count" & field != "penalty")
      'set string 4 l 6'
      'draw string 0.1 'y1+1.5' 30d avg: 'digs(avgvar,2)
      'set string 2 l 6'
      'draw string 0.1 'y1+1.3' 30d sdv: 'digs(avgsdv,2)
   endif
   if (field = "count")
      'set string 4 l 6'
      'draw string 0.1 'y1+1.5' 30d avg: 'digs(avgvar,1)
      'set string 2 l 6'
      'draw string 0.1 'y1+1.3' 30d sdv: 'digs(avgsdv,1)
   endif
   if (field = "penalty")
      'set string 4 l 6'
      'draw string 0.1 'y1+1.5' 30d avg: 'digs(avgvar,2)
      'set string 2 l 6'
      'draw string 0.1 'y1+1.3' 30d sdv: 'digs(avgsdv,2)
   endif

   if (iuse<0) 
      'set string 3 l 6'
      'draw string 0.1 'y1+1.05' CHANNEL 'channel
      'set string 9 l 6'
      'draw string 0.1 'y1+0.85' ** IS NOT **'
      'set string 3 l 6'
      'draw string 0.1 'y1+0.65' ASSIMILATED'
   endif

   ymin=minvar0
   if (minsdv0<ymin)
      ymin=minsdv0
   endif

   ymax=maxvar0
   if (maxsdv0>ymax)
      ymax=maxsdv0
   endif

   yrange=ymax-ymin
   dy=0.1*yrange
   ymin=ymin-dy
   ymax=ymax+dy


   'set vpage 0.0 8.5 'y1' 'y1+3.2
   'set grads off'
   'set gxout line'

   'set vrange 'ymin' 'ymax    
   'set cthick 8'
   'set cmark 0'
   'set cstyle 1'
   'set ccolor 7'
   'd avg1'
   'set cmark 0'
   'set cstyle 1'
   'set ccolor 3'
   'd avg2'
   'set cmark 0'
   'set cstyle 1'
   'set ccolor 4'
   'd avg3'

   if (field != "count" & field != "penalty")
      'set cmark 0'
      'set cstyle 2'
      'set ccolor 7'
      'd sdv1'
      'set cmark 0'
      'set cstyle 2'
      'set ccolor 3'
      'd sdv2'
      'set cmark 0'
      'set cstyle 2'
      'set ccolor 2'
      'd sdv3'
   endif

   if (field = "omgnbc")
      'set z 1'
      'set cmark 0'
      'set cstyle 2'
      'set ccolor 1'
      'd -1*fixang(t=1)'
      'set cmark 0'
      'set cstyle 3'
      'set ccolor 1'
      'd -1*satang(t=1)'
      'set z 'region
   endif

   if (i=4 | chn=nchan)
      'draw xlab look angle (degrees)'
   endif
   'set vpage off'

   i=i+1
   if (i=5 | chn=nchan)
      fr=fr+1
      'set string 1 l 6'
      'set strsiz 0.15 0.15'
      'draw string 0.2 10.80 platform:  'plotfile
      'draw string 0.2 10.55 region  :  'area
      'draw string 0.2 10.30 variable:  'type
      'draw string 0.2 10.05 valid   :  'date1
      'set strsiz 0.12 0.12'
      'set string 7 r 6'
      'draw string 8.3 10.80 yellow: 1d'
      'set string 3 r 6'
      'draw string 8.3 10.60 green: 7d'
      if (field = "omgnbc" | field = "total" | field = "omgbc")
         'set string 4 r 6'
         'draw string 7.5 10.4 blue, '
         'set string 2 r 6'
         'draw string 8.3 10.40 red: 30d'
         'set string 4 r 6'
         'draw string 7.4 10.20 solid=avg, '
         'set string 2 r 6'
         'draw string 8.3 10.20 dash=sdv'
      endif
      if (field = "count" | field = "penalty")
         'set string 4 r 6'
         'draw string 8.42 10.4 blue: 30d '
      endif
      if (field = "omgnbc")
         'set string 1 r 6'
         'draw string 8.3 9.95 black: fixed'
      endif
      outfile=plotfile'.'field'_region'region'_fr'fr'.png'
      'printim 'outfile' 'xsize' 'ysize' white' 
*      say 'output to file 'outfile
      if (debug=1) 
         say 'press any key to continue
         pull var
      endif
      i=1
   endif
   chn=chn+1
endwhile

region=region+1
endwhile

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

