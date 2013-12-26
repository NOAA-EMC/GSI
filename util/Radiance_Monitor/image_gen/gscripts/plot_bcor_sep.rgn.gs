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

*plotfile=goes.010
*field=lapse
*xsize=x750
*ysize=y700
*platform=goes.010

*say 'process 'field' from 'plotfile
*'open 'plotfile'.ctl'

debug=0

'q file'
lin1=sublin(result,1)
satnam=subwrd(lin1,4)
satnum=subwrd(lin1,5)
nchan=subwrd(lin1,6)

if (field = total)
 type="total bias correction (K)"
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
 type="cos for SSMIS"
endif
if (field = sin)
 type="sin for SSMIS"
endif
if (field = emiss)
 type="emissivity sensitivity"
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


* Determine number of channels
'q file'
lin1=sublin(result,1)
nchan=subwrd(lin1,6)
lin5=sublin(result,5)


*say 'nchan='nchan

* Set time
'set t 1 last'
'query time'
date1=subwrd(result,3)
date2=subwrd(result,5)

*say 'date1='date1
*say 'date2='date2



'clear'
'set grads off'
'set y 1'

'set string 1 l 5'
'set strsiz 0.11 0.11'
'set xlopts 1 4 0.11'
'set ylopts 1 2 0.09'

'define avg=avg'field
'define sdv=sdv'field

fr=0
i=1
chn=1
while (chn<=nchan)
*   say 'top of channel loop with chn='chn
   'set x 'chn
   chi=chn
   if (i=1) 
      'clear'
      y1=7.65
   endif
   if (i>1 & i<4) 
      y1=y1-2.45
   endif
   if (i=4) 
      y1=y1-2.45
   endif

   'set gxout stat'
   'd avg'
   rec7=sublin(result,7)
   rec8=sublin(result,8)
   rec11=sublin(result,11)
   valvar=subwrd(rec7,8)
   minvar=subwrd(rec8,4)
   maxvar=subwrd(rec8,5)
   avgvar=subwrd(rec11,2)
*   say 'avg'var' min,max,avg='minvar','maxvar','avgvar','valvar
      'd sdv'
      rec7=sublin(result,7)
      rec8=sublin(result,8)
      rec11=sublin(result,11)
      valsdv=subwrd(rec7,8)
      minsdv=subwrd(rec8,4)
      maxsdv=subwrd(rec8,5)
      avgsdv=subwrd(rec11,2)
*      say 'sdv'var' min,max,avg='minsdv','maxsdv','avgsdv','valsdv

   'd penalty'
   rec8=sublin(result,8)
   rec11=sublin(result,11)
   ratio=subwrd(rec11,2)


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
   'draw string 0.1 'y1+2.0' channel 'channel
   'draw string 0.1 'y1+1.8' `3X`0 'digs(ratio,4)
   'draw string 0.1 'y1+1.6' f 'freq' GHz'
   'draw string 0.1 'y1+1.4' `3l`0 'wavelength' `3m`0m'
   'set string 4 l 6'
   'draw string 0.1 'y1+1.1' avg: 'digs(avgvar,4)
   'set string 2 l 6'
   'draw string 0.1 'y1+0.9' sdv: 'digs(avgsdv,4)
   if (iuse<0)
      'set string 3 l 6'
      'draw string 0.1 'y1+0.7' CHANNEL 'channel
      'set string 9 l 6'
      'draw string 0.1 'y1+0.5' ** IS NOT **'
      'set string 3 l 6'
      'draw string 0.1 'y1+0.3' ASSIMILATED'
   endif

   
   y2=y1
   y3=y2+1.2
   'set parea 2.1 7.8 'y2' 'y3
   'set grads off'
   'set datawarn off'
   'set tlsupp year'
   'set ylpos 0 l'
    yrange=maxvar-minvar
    dy=0.1*yrange
    ymin=minvar-dy
    ymax=maxvar+dy
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
*    'set yaxis 'ymin' 'ymax
*    'set ylab %.4f'
*    yr=(ymax-ymin)/2
*    ymid=ymin+yr
*    ym1=ymin+yr*0.5
*    ym2=ymid+yr*0.5
*    say ' ymid='ymid
*    say ' ym1='ym1
*    say ' ym2='ym2
*   'set ylevs 'ymin' 'ym1' 'ymid' 'ym2' 'ymax
   'set ccolor 4'
   'set cmark 1'
   'd avg'
   if(ymax >0  & ymin <0)
   ' set ccolor  1'
   'set cmark 0'
    'd avg*0'
    endif 
   'set parea off'
y2=y3+0.05
   y3=y2+0.85
   'set parea 2.1 7.8 'y2' 'y3
   'set grads off'
   'set datawarn off'
   'set tlsupp year'
   'set tlsupp month'
   'set ylpos 0 r'
    yrange=maxsdv-minsdv
    dy=0.1*yrange
    ymin=minsdv-dy
    ymax=maxsdv+dy
    yrange=ymax-ymin
   'set gxout line'
   'set vrange 'ymin' 'ymax
*    'set yaxis 'ymin' 'ymax
    if( ymin >0.0001)
    'set ylab %.4f'
    endif
*    yr=(ymax-ymin)/2
*    ymid=ymin+yr
*    ym1=ymin+yr*0.5
*    ym2=ymid+yr*0.5
*    say ' ymid='ymid
*    say ' ym1='ym1
*    say ' ym2='ym2
*   'set ylevs 'ymin' 'ym1' 'ymid' 'ym2' 'ymax
    ymid=0.2*yrange
    'set ylint 'ymid
   'set ccolor 2'
   'set cmark 1'
   'd sdv'
   'set parea off'

   i=i+1
   if (i=5 | chn=nchan)
      fr=fr+1
      'set string 1 l 6'
      'set strsiz 0.15 0.15'
      'draw string 0.2 10.80 platform:  'plotfile
      'draw string 0.2 10.55 variable:  'type
      'draw string 0.2 10.30 valid   :  'date1' to 'date2
      outfile=plotfile'.'field'_fr'fr'.png'
      'printim 'outfile' 'xsize' 'ysize' white'
*      say 'output to file 'outfile
      if (debug=1) 
         say 'press any key to continue'
         pull var
      endif
      i=1
   endif
   chn=chn+1
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

