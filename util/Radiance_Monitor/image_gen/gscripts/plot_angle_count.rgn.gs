* Script to plot given bias correction term for given satellite instrument
*    Data is global, meaning it contains multimple regions
* 
* Two arguments are expected
*    plotfile = satellite id (name and number ... e.g., msu.014 = noaa-14 msu)
*    field  = field to plot  (valid strings are:  count penalty total fixang lapse lapse2 const scangl clw

function plotangle (args)

plotfile=subwrd(args,1)
field=subwrd(args,2)
sub_avg=subwrd(args,3)
xsize=subwrd(args,4)
ysize=subwrd(args,5)
platform=plotfile

say 'process 'field' from 'plotfile

debug=0

if (field = count)
 type="number of observations"
endif
if (field = penalty)
 type="contribution to penalty"
endif


*
* Determine number of channels and regions
*
'q file'
lin1=sublin(result,1)
satnam=subwrd(lin1,4)
satnum=subwrd(lin1,5)
nchan=subwrd(lin1,6)
*lin5=sublin(result,5)
*nregion=subwrd(lin5,9)
nregion=1

'!rm -f xsize.txt'
'!cat 'plotfile'.ctl |grep "xdef" > xsize.txt'
result=read(xsize.txt)
rc=sublin(result,1)
if (rc = 0)
   info=sublin(result,2)
   nx=subwrd(info,2)
   xs=subwrd(info,4)
   xe1=subwrd(info,5)
endif
result=close(xsize.txt)

xe=xs+xe1*nx


say 'nchan='nchan
say 'nregion='nregion

*
* Set time
*
'set t 1 last'
'query time'
date1=subwrd(result,3)
date2=subwrd(result,5)

say 'date1='date1
say 'date2='date2

'q dims'
lin5=sublin(result,5)
tfirst=subwrd(lin5,11)
tlast=subwrd(lin5,13)
t1day=tlast-3
t7days=tlast-27
*t30days=tlast-119
t30days=1
say 'tlast,t1day,t7days,t30days='tlast' 't1day' 't7days' 't30days


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


*
* For count some sat/instruments have zero counts at the beginning and/or
* end of their scan positions.  We don't want to plot any of these zeros.
* Find the first non-zero count on either end and use that range to 
* define a new grid, rcount, from count.  Use rcount for all plotting and
* calculations.
*

'set y 1 'nchan
'set z 1'

new_xs=xs
new_xe=xe
'set gxout stat'

it=new_xs
done=0
while (it<=new_xe & done=0)
   'set lon 'it' 'it
   'd ave(count, t='t30days', t='tlast')'
   rec11=sublin(result,11)
   avgvar=subwrd(rec11,2)
   say ' it, avgvar = 'it', 'avgvar

   if (avgvar > 0.0 )
      done = 1
   else
      it=it+1
   endif
endwhile
new_xs=it
say 'new_xs = 'new_xs


it=new_xe-1
done=0
say 'it = 'it
while (it>=new_xs & done=0)
   'set lon 'it' 'it

   'd ave(count, t='t30days', t='tlast')'
   rec11=sublin(result,11)
   avgvar=subwrd(rec11,2)
   say ' it, avgvar = 'it', 'avgvar
   if (avgvar > 0.0 )
      done=1
   else
      it=it-1
   endif
endwhile
new_xe=it

'set lon 'new_xs' 'new_xe
'set t 't30days' 'tlast
'define rcount=count'


'set lon 'xs' 'xe
'set t 'tlast

region=1
while (region<=nregion)

   say 'top of region loop with region='region

   '!rm -f area.txt'
   '!cat 'plotfile'.ctl |grep "region= 'region' " > area.txt'
   result=read(area.txt)
   rc=sublin(result,1)
   area="unknown"
   if (rc = 0)
      info=sublin(result,2)
      area=substr(info,14,60)
   endif
   result=close(area.txt)

   'clear'
   'set grads off'
   'set missconn on'
   'set lon 'xs' 'xe
   'set z 'region
   'set mproj off'


   'set z 'region
   'set string 1 l 5'
   'set strsiz 0.12 0.12'
   'set xlopts 1 4 0.12'
   'set ylopts 1 4 0.13'

   fr=0
   i=1
   chn=1
   nt=3
   while (chn<=nchan)
      say 'top of channel loop with chn='chn
      'set y 'chn
      if (field = "count")

         if (sub_avg=1)
            'define avg1=ave(rcount,t='t1day',t='tlast')'
            'define avg2=ave(rcount,t='t7days',t='tlast')'
         endif
         'define avg3=ave(rcount,t='t30days',t='tlast')'

         'set gxout stat'
         'd avg3'

         rec14=sublin(result,14)
         avgsdv=subwrd(rec14,2)
         'define sdv3='field'(t=3)'
         'set gxout line'
      endif

      if (field = "penalty")

         if (sub_avg=1)
            'define avg1=ave(penalty/rcount,t='t1day',t='tlast')'
            'define avg2=ave(penalty/rcount,t='t7days',t='tlast')'
         endif
         'define avg3=ave(penalty/rcount,t='t30days',t='tlast')'

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
      if (sub_avg=0)
         it=3
      else
         it=1
      endif

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
*         say ' avg'it' min,max,avg='minvar','maxvar','avgvar

         'd penalty/rcount'
         rec8=sublin(result,8)
         rec11=sublin(result,11)
         ratio=subwrd(rec11,2)
         say 'sdv'it' ratio='ratio

         if (field = "count" | field = "penalty")
            minsdv=minvar
            maxsdv=maxvar
            valsdv=valvar
         endif


*         say 'valvar,valsdv='valvar' 'valsdv
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
*      say 'channel,iuse,error,freq,wavelength = 'channel', 'iuse', 'error', 'freq', 'wavelength
   
      'set strsiz 0.12 0.12'
      'set string 1 l 6'
      'draw string 0.1 'y1+2.4' channel 'channel
      'draw string 0.1 'y1+2.2' `3X`0 'digs(ratio,4)
      'draw string 0.1 'y1+2.0' f 'freq' GHz'
      'draw string 0.1 'y1+1.8' `3l`0 'wavelength' `3m`0m'
      if (field != "count" & field != "penalty")
         'set string 4 l 6'
         'draw string 0.1 'y1+1.5' 'ndays'd avg: 'digs(avgvar,2)
         'set string 2 l 6'
         'draw string 0.1 'y1+1.3' 'ndays'd sdv: 'digs(avgsdv,2)
      endif
      if (field = "count")
         'set string 4 l 6'
         'draw string 0.1 'y1+1.5' 'ndays'd avg: 'digs(avgvar,1)
         'set string 2 l 6'
         'draw string 0.1 'y1+1.3' 'ndays'd sdv: 'digs(avgsdv,1)
      endif
      if (field = "penalty")
         'set string 4 l 6'
         'draw string 0.1 'y1+1.5' 'ndays'd avg: 'digs(avgvar,2)
         'set string 2 l 6'
         'draw string 0.1 'y1+1.3' 'ndays'd sdv: 'digs(avgsdv,2)
      endif

      if (iuse<=0) 
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
      if (sub_avg=1)
         'd avg1'
         'set cmark 0'
         'set cstyle 1'
         'set ccolor 3'
         'd avg2'
      endif
      'set cmark 0'
      'set cstyle 1'
      'set ccolor 4'
      'd avg3'

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
         if (nregion>1)
            'draw string 0.2 10.55 region  :  'area
         endif
         'draw string 0.2 10.30 variable:  'type
         'draw string 0.2 10.05 valid   :  'date1' - 'date2
         'set strsiz 0.12 0.12'
         'set string 7 r 6'
         if (sub_avg=1)
            'draw string 8.3 10.80 yellow: 1d'
            'set string 3 r 6'
            'draw string 8.3 10.60 green: 7d'
         endif
         if (field = "omgnbc" | field = "total" | field = "omgbc")
            'set string 4 r 6'
            'draw string 7.5 10.4 blue, '
            'set string 2 r 6'
            'draw string 8.3 10.40 red: 'ndays'd'
            'set string 4 r 6'
            'draw string 7.4 10.20 solid=avg, '
            'set string 2 r 6'
            'draw string 8.3 10.20 dash=sdv'
         endif
         if (field = "fixang" | field = "lapse" | field = "lapse2" | field = "const" | field = "scangl" | field = "clw")
            'set string 4 r 6'
            'draw string 7.5 10.4 blue, '
            'set string 2 r 6'
            'draw string 8.3 10.40 red: 'ndays'd'
            'set string 4 r 6'
            'draw string 7.4 10.20 solid=avg, '
            'set string 2 r 6'
            'draw string 8.3 10.20 dash=sdv'
         endif

         if (field = "count" | field = "penalty")
            'set string 4 r 6'
            'draw string 8.42 10.4 blue: 'ndays'd '
         endif
         if (field = "omgnbc")
            'set string 1 r 6'
            'draw string 8.3 9.95 black: fixed'
         endif
         outfile=plotfile'.'field'_fr'fr'.png'
         'printim 'outfile' 'xsize' 'ysize' white' 
         say 'output to file 'outfile
         if (debug=1) 
            say 'press any key to continue'
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

