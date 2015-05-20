* Script to plot given bias correction term for given satellite instrument
* 
* Two arguments are expected
*    plotfile = satellite id (name and number ... e.g., msu.014 = noaa-14 msu)
*    field  = field to plot  (valid strings are:  count total fixang lapse lapse2 const scangl clw


function plotangle (args)

plotfile=subwrd(args,1)
field=subwrd(args,2)
plot_all_regions=subwrd(args,3)
sub_avg=subwrd(args,4)
xsize=subwrd(args,5)
ysize=subwrd(args,6)
platform=plotfile

say 'plot_all_Regions = 'plot_all_regions

say 'process 'field' from 'plotfile
*say 'using fixed1'

debug=0

'q file'
lin1=sublin(result,1)
satnam=subwrd(lin1,4)
satnum=subwrd(lin1,5)
nchan=subwrd(lin1,6)

if (field = omgnbc)
 type="ges_(w/o bias cor) - obs (K)"
endif
if (field = total)
 type="total bias correction (K)"
endif
if (field = omgbc)
 type="ges_(w/bias cor) - obs (K)"
endif
if (field = fixang)
 type="fixed angle correction (K)"
endif
if (field = lapse)
 type="lapse rate correction (K)"
endif
if (field = lapse2)
 type="(lapse rate)**2 correction (K)"
endif
if (field = const)
 type="mean correction (K)"
endif
if (field = scangl)
 type="scan angle correction (K)"
endif
if (field = clw)
 type="cloud liquid water correction (K)"
endif
if (field = cos)
 type="Cos for SSMIS"
endif
if (field = sin)
 type="Sin for SSMIS"
endif
if (field = emiss)
 type="Emissivity sensitivity term"
endif
if (field = ordang4)
 type="4th order angle term"
endif
if (field = ordang3)
 type="3rd order angle term"
endif
if (field = ordang2)
 type="2nd order angle term"
endif
if (field = ordang1)
 type="1st order angle term"
endif


*
* Determine number of channels and regions
*
'q file'
lin1=sublin(result,1)
nchan=subwrd(lin1,6)
lin5=sublin(result,5)
*
*  If plot_all_regions is 0 (false) then set to plot region 1 (global) only
*
nregion=subwrd(lin5,9)

if (plot_all_regions = 0)
   nregion=1
endif

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

xe=xs+xe1*(nx-1)
say 'nx, xs, xe1= 'nx','xs','xe1
'set lon 'xs' 'xe



*say 'nchan='nchan
*say 'nregion='nregion

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
say 'tlast='tlast

t1day=tlast-3
t7days=tlast-27
*t30days=tlast-119
t30days=1
say 'tlast,t1day,t7days,t30days='tlast' 't1day' 't7days' 't30days

if (debug=1) 
   say 'press any key to continue'
   pull var
endif

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
* For omgnbc we plot the satang.  But it shouldn't be plotted for 
* scan positions (the x or lon variable) for which there is no data.
* Working from both ends, throw out any scan positions that have a 
* 0 count for the 30 day average.  Define the new grid, fixed, from
* satang, using the appropriately reduced scan positions.
*
'set t last'

if (field = "omgnbc")
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

   'set y 1 'nchan
   'set z 1'
   'set lon 'new_xs' 'new_xe
   'define satang1=fixang(t='tlast')/count(t='tlast')'
   'define fixed1=-1*satang1(t='tlast')'
endif
say ' new_xs, new_xe ='new_xs','new_xe


'set lon 'xs' 'xe

if (sub_avg = 1)
   'define cnt1=ave(count, t='t1day', t='tlast')' 
   'define avg=ave('field', t='t1day', t='tlast')'
   'define avgs=ave('field'_2, t='t1day', t='tlast')'
   'define rterm1=1/cnt1'
   'define rterm2=1/ave(count-1, t='t1day', t='tlast')'
   'define svar=(abs(cnt1*avgs-avg*avg))*rterm1*rterm2'

   'define avg1=ave('field'/count, t='t1day', t='tlast')'
   'define avg1=maskout(avg1,avg1-0)'
   'define sdv1=sqrt(svar)'

   'define cnt1=ave(count, t='t7days', t='tlast')' 
   'define avg=ave('field', t='t7days', t='tlast')'
   'define avgs=ave('field'_2, t='t7days', t='tlast')'
   'define rterm1=1/cnt1'
   'define rterm2=1/ave(count-1, t='t7days', t='tlast')'
   'define svar=(abs(cnt1*avgs-avg*avg))*rterm1*rterm2'

   'define avg2=ave('field'/count, t='t7days', t='tlast')'
   'define sdv2=sqrt(svar)'
endif

'define cnt1=ave(count, t='t30days', t='tlast')' 
'define avg=ave('field', t='t30days', t='tlast')'
'define avgs=ave('field'_2, t='t30days', t='tlast')'
'define rterm1=1/cnt1'
'define rterm2=1/ave(count-1, t='t30days', t='tlast')'
'define svar=(abs(cnt1*avgs-avg*avg))*rterm1*rterm2'

'define avg3=ave('field'/count, t='t30days', t='tlast')'
'define sdv3=sqrt(svar)'

'undefine cnt1'
'undefine avg'
'undefine avgs'
'undefine rterm1'
'undefine rterm2'
'undefine svar'



region=1
while (region<=nregion)

say 'top of region loop with region='region

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
*say 'area = 'area
say 'xs = 'xs
say 'xe = 'xe


'clear'
'set grads off'
'set missconn on'
'set lon 'xs' 'xe
'set z 'region
'set mproj off'

'set string 1 l 5'
'set strsiz 0.11 0.11'
'set xlopts 1 4 0.11'
'set ylopts 1 2 0.09'

*if (sub_avg = 1)
*   'define cnt1=ave(count, t='t1day', t='tlast')' 
*   'define avg=ave('field', t='t1day', t='tlast')'
*   'define avgs=ave('field'_2, t='t1day', t='tlast')'
*   'define rterm1=1/cnt1'
*   'define rterm2=1/ave(count-1, t='t1day', t='tlast')'
*   'define svar=(abs(cnt1*avgs-avg*avg))*rterm1*rterm2'
*
*   'define avg1=ave('field'/count, t='t1day', t='tlast')'
*   'define avg1=maskout(avg1,avg1-0)'
*   'define sdv1=sqrt(svar)'
*
*   'define cnt1=ave(count, t='t7days', t='tlast')' 
*   'define avg=ave('field', t='t7days', t='tlast')'
*   'define avgs=ave('field'_2, t='t7days', t='tlast')'
*   'define rterm1=1/cnt1'
*   'define rterm2=1/ave(count-1, t='t7days', t='tlast')'
*   'define svar=(abs(cnt1*avgs-avg*avg))*rterm1*rterm2'
*
*   'define avg2=ave('field'/count, t='t7days', t='tlast')'
*   'define sdv2=sqrt(svar)'
*endif
*
*'define cnt1=ave(count, t='t30days', t='tlast')' 
*'define avg=ave('field', t='t30days', t='tlast')'
*'define avgs=ave('field'_2, t='t30days', t='tlast')'
*'define rterm1=1/cnt1'
*'define rterm2=1/ave(count-1, t='t30days', t='tlast')'
*'define svar=(abs(cnt1*avgs-avg*avg))*rterm1*rterm2'
*
*'define avg3=ave('field'/count, t='t30days', t='tlast')'
*'define sdv3=sqrt(svar)'
*
*'undefine cnt1'
*'undefine avg'
*'undefine avgs'
*'undefine rterm1'
*'undefine rterm2'
*'undefine svar'

fr=0
i=1
chn=1
nt=3
while (chn<=nchan)
*   say 'top of channel loop with chn='chn
   'set y 'chn

   chi=chn
   if (i=1) 
      'clear'
      y1=7.65
   endif
   if (i>1 & i<4) 
      y1=y1-2.35
   endif
   if (i=4) 
      y1=y1-2.35
   endif

   minvar0= 9999
   maxvar0=-9999
   minsdv0= 9999
   maxsdv0=-9999

   'set gxout stat'
   if (sub_avg=1)
      it=1
   else
      it=3
   endif
   while (it<=nt)
      'd avg'it
      rec7=sublin(result,7)
      rec8=sublin(result,8)
      rec11=sublin(result,11)
      valvar=subwrd(rec7,8)
      minvar=subwrd(rec8,4)
      maxvar=subwrd(rec8,5)
      avgvar=subwrd(rec11,2)
      say ' avg'it' min,max,avg='minvar','maxvar','avgvar

         'd sdv'it
         rec7=sublin(result,7)
         rec8=sublin(result,8)
         rec11=sublin(result,11)
         valsdv=subwrd(rec7,8)
         minsdv=subwrd(rec8,4)
         maxsdv=subwrd(rec8,5)
         avgsdv=subwrd(rec11,2)
         say 'sdv'it' min,max,avg='minsdv','maxsdv','avgsdv

      if (field = "omgnbc")
         'd fixed1'
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

      'd penalty/count'
      rec8=sublin(result,8)
      rec11=sublin(result,11)
      ratio=subwrd(rec11,2)
*      say 'sdv'it' ratio='ratio
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
   say 'channel,iuse,error,freq,wavelength = 'channel', 'iuse', 'error', 'freq', 'wavelength
   
   'set strsiz 0.12 0.12'
   'set string 1 l 6'
   'draw string 0.1 'y1+2.0' channel 'channel
   'draw string 0.1 'y1+1.8' `3X`0 'digs(ratio,4)
   'draw string 0.1 'y1+1.6' f 'freq' GHz'
   'draw string 0.1 'y1+1.4' `3l`0 'wavelength' `3m`0m'
   'set string 4 l 6'
   'draw string 0.1 'y1+1.1' 'ndays'd avg: 'digs(avgvar,2)
   'set string 2 l 6'
   'draw string 0.1 'y1+0.9' 'ndays'd sdv: 'digs(avgsdv,2)
   if (iuse<=0) 
      'set string 3 l 6'
      'draw string 0.1 'y1+0.7' CHANNEL 'channel
      'set string 9 l 6'
      'draw string 0.1 'y1+0.5' ** IS NOT **'
      'set string 3 l 6'
      'draw string 0.1 'y1+0.3' ASSIMILATED'
   endif

   y2=y1
   y3=y2+1.2
   'set parea 2.1 7.6 'y2' 'y3
   'set grads off'
   'set datawarn off'
   'set xlab on'
   'set ylpos 0 l'
    yrange=maxvar0-minvar0
    dy=0.1*yrange
    ymin=minvar0-dy
    ymax=maxvar0+dy
    yrange=ymax-ymin
   'set gxout line'
   'set vrange 'ymin' 'ymax    
   aymin=ymin
    if(ymin <0)
      aymin=-ymin
    endif
    if( aymin >0.0001)
    'set ylab %.4f'
    endif
    dy=0.2*yrange
    'set ylint 'dy
   'set cthick 8'
   if (sub_avg=1)
      'set cmark 0'
      'set cstyle 1'
      'set ccolor 7'
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
   if( field !="omgnbc")
     if(ymax >0  & ymin <0)
       ' set ccolor  1'
       'set cmark 0'
       'd avg3*0'
     endif
   endif
   if (field = "omgnbc")
      'set cmark 0'
      'set cstyle 3'
      'set ccolor 1'
      'd fixed1'
      'set z 'region
   endif

   'set parea off'

      y2=y3+0.05
      y3=y2+0.85
      'set parea 2.1 7.6 'y2' 'y3
      'set z 'region
   'set grads off'
   'set datawarn off'
   'set xlab off'
   'set ylpos 0 r'
   yrange=maxsdv0-minsdv0
   dy=0.1*yrange
    ymin=minsdv0-dy
    ymax=maxsdv0+dy
    yrange=ymax-ymin
    dy=0.2*yrange
   'set gxout line'
   'set vrange 'ymin' 'ymax
   'set ylint 'dy
   if( ymin >0.0001)
      'set ylab %.4f'
   endif

   if( sub_avg=1)
      'set cmark 0'
      'set cstyle 2'
      'set ccolor 7'
      'd sdv1'
      'set cmark 0'
      'set cstyle 2'
      'set ccolor 3'
      'd sdv2'
   endif
   'set cmark 0'
   'set cstyle 2'
   'set ccolor 2'
   'd sdv3'

*  Add page lables
*
   if (i=4 | chn=nchan)
      'set string 1 l 6'
      'set strsiz 0.15 0.15'
      'draw string 4.0 0.18 look angle (degrees)'
   endif
   'set parea off'

   i=i+1
   if (i=5 | chn=nchan)
      fr=fr+1
      'set string 1 l 6'
      'set strsiz 0.15 0.15'
      'draw string 0.2 10.80 platform:  'plotfile
      'draw string 0.2 10.55 region  :  'area
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
*      endif
*      if (field = "fixang" | field = "lapse" | field = "lapse2" | field = "const" | field = "scangl" | field = "clw" | field = "cos" | field = "sin" | field = "emiss" | field = ""ordang4" | field = "ordang3" | field = "ordang2" | field = "ordang1")
      else
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
      outfile=plotfile'.'field'_region'region'_fr'fr'.png'
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

