* script to plot given bias correction term for given satellite instrument
* 
* Two arguments are expected
*    plotfile = satellite id (name and number ... e.g., msu.014 = noaa-14 msu)
*    field  = field to plot  (valid strings are:  count total fixang lapse lapse2 const scangl clw

function plottime (args)

*'reinit'
plotfile1=subwrd(args,1)
plotfile2=subwrd(args,2)
platform=subwrd(args,3)
field=penalty
xsize=subwrd(args,4)
ysize=subwrd(args,5)

*plotfile1=con
*plotfile2=exp
*platform=ssmi_f13
*field=count
*xsize=x750
*ysize=y700

*say 'process 'field' from 'plotfile
*'open 'plotfile'.ctl'

debug=0

'q file'
lin1=sublin(result,1)
satnam=subwrd(lin1,4)
satnum=subwrd(lin1,5)
nchan=subwrd(lin1,6)
say 'nchan='nchan

if (field = count)
 type="number of observations(con-exp)"
endif
if (field = omgnbc)
 type="ges_(w/o bias cor) - obs (K)(con-exp)"
endif
if (field = total)
 type="total bias correction (K)(con-exp)"
endif
if (field = omgbc)
 type="ges_(w/bias cor) - obs (K)(con-exp)"
endif
if (field = penalty)
 type="contribution to penalty(con-exp)"
endif


* Determine number of channels and regions
'q file'
lin1=sublin(result,1)
nchan=subwrd(lin1,6)
lin5=sublin(result,5)
nregion=subwrd(lin5,6)


*say 'nchan='nchan
*say 'nregion='nregion

* Set time
'set t 1 last'
'query time'
date1=subwrd(result,3)
date2=subwrd(result,5)

*say 'date1='date1
*say 'date2='date2

region=1
while (region<=nregion)

*say 'top of region loop with region='region

'!rm -f area.txt'
'!cat 'plotfile1'_'platform'.ctl |grep "region= 'region' " > area.txt'
result=read(area.txt)
rc=sublin(result,1)
area="uknown"
if (rc = 0)
   info=sublin(result,2)
   area=substr(info,14,60)
endif
result=close(area.txt)
*say 'area = 'area


'clear'
'set grads off'
'set y 'region

'set string 1 l 5'
'set strsiz 0.12 0.12'
'set xlopts 1 4 0.12'
'set ylopts 1 4 0.13'

fr=0
i=1
chn=1
while (chn<=nchan)
*   say 'top of channel loop with chn='chn
   'set x 'chn
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

   '!rm -f info.txt'
   '!cat 'plotfile1'_'platform'.ctl |grep "'chn', channel" > info.txt'
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
   'set string 4 l 6'

   if (iuse<0) 
      'set string 3 l 6'
      'draw string 0.1 'y1+1.1' CHANNEL 'channel
      'set string 9 l 6'
      'draw string 0.1 'y1+0.9' ** IS NOT **'
      'set string 3 l 6'
      'draw string 0.1 'y1+0.7' ASSIMILATED'
   endif

   'set vpage 0.0 8.5 'y1' 'y1+3.2
   'set grads off'
   'set gxout line'

   'set ccolor 4'
   'd abs(penalty.1)-abs(penalty.2)'
   ' set ccolor  1'
   'set cmark 0'
    'd penalty.1*0'
   'set vpage off'

   i=i+1
   if (i=5 | chn=nchan)
      fr=fr+1
      'set string 1 l 6'
      'set strsiz 0.15 0.15'
      'draw string 0.2 10.80 platform:  'platform
      'draw string 0.2 10.55 region  :  'area
      'draw string 0.2 10.30 variable:  'type
      'draw string 0.2 10.05 valid   :  'date1' to 'date2
      outfile=platform'.'field'_region'region'_fr'fr'.png'
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

