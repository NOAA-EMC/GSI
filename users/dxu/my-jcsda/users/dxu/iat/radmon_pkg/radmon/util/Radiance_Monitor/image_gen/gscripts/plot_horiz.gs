*-----------------------------------------------------------------------------
* plot_horiz_dev.gs
*
* Script to plot horizontal maps of given field for given satellite instrument
* 
* Two arguments are expected
*    plotfile = satellite id (name and number ... e.g., msu.014 = noaa-14 msu)
*    field  = field to plot  (valid strings are:  obs, cor, obsges, obsnbc)
*
* Log
*  3/2010  safford  modified plot area for sndrd*
*  3/2010  safford  modified coloring and spectrum display for sndrd* 
*-----------------------------------------------------------------------------

function plottime (args)

plotfile=subwrd(args,1)
field=subwrd(args,2)
xsize=subwrd(args,3)
ysize=subwrd(args,4)
platform=plotfile

*say 'plotfile='plotfile
*say 'field='field
*say 'xsize,ysize='xsize', 'ysize

*'open 'plotfile'.ctl'

debug=0

'q file'
lin1=sublin(result,1)
satnam=subwrd(lin1,4)
satnum=subwrd(lin1,5)
nchan=subwrd(lin1,6)
sndr=substr(satnam, 1, 5)
say 'satnam = 'satnam

if (field = obs)
 type="observation (K)"
endif
if (field = cor)
 type="bias correction (K)"
endif
if (field = obsges)
 type="ges_(w/bias cor) - obs (K)"
endif
if (field = obsnbc)
 type="ges_(w/o bias cor) - obs (K)"
endif

* Set (lat,lon) box to plot
lon1=0
lon2=360
lat1=-90
lat2=90
if (satnam = goes)
   if (satnum = 8 | satnum = 12)
      lon1=260
      lon2=330
      lat1=0
      lat2=60
   endif
   if (satnum = 10)
      lon1=180
      lon2=250
      lat1=0
      lat2=60
   endif
endif
if (satnam = seviri)
   lon1=-80
   lon2=80
   lat1=-60
   lat2=60
endif
if (sndr = sndrd)
  lon1=100
  lon2=360
  lat1=-10
  lat2=80
endif
   
'set display color white'
'clear'

'set t 1'
'set lon 'lon1' 'lon2
'set lat 'lat1' 'lat2
'set mproj scaled'

xlo.1=0.1
xlo.2=5.6
xlo.3=0.1
xlo.4=5.6

xhi.1=5.4
xhi.2=10.9
xhi.3=5.4
xhi.4=10.9

ylo.1=4.1
ylo.2=4.1
ylo.3=0.1
ylo.4=0.1

yhi.1=8.1
yhi.2=8.1
yhi.3=4.1
yhi.4=4.1

mxchn=nchan
chn=1
while (chn<=mxchn)
 'clear'
 var=field''chn
 total=0

 nframe=4
 i=1
 while (i<=nframe)

    'set t 'i
    'query time'
    date1=subwrd(result,3)
 
    'set gxout stat'
    'd 'var
    lin7=sublin(result,7)
    lin8=sublin(result,8)
    lin11=sublin(result,11)
    lin13=sublin(result,13)
    valid=subwrd(lin7,8)
    min=subwrd(lin8,4)
    max=subwrd(lin8,5)
    avg=subwrd(lin11,2)
    sdv=subwrd(lin13,2)

*----------------------------------------------------
*   scale the units interval based on the sdv
*   and rounded to 0.05, 0.1, 0.2, 0.5 or a whole
*   number (runit) for larger sdvs 
*----------------------------------------------------
    avg_int=math_nint(avg)
    runit=math_nint((sdv*4)/14)
    interval=0.5

    if( sdv < 0.1 )
       interval=0.05
    else
       if( sdv < 0.2 )
          interval=0.1
       else
          if( sdv < 0.5 )
             interval=0.2
          else
             if( runit > 0 )
                interval=runit
             endif
          endif
       endif
    endif

    c1=avg_int-(6*interval)
    c2=avg_int-(5*interval)
    c3=avg_int-(4*interval)
    c4=avg_int-(3*interval)
    c5=avg_int-(2*interval)
    c6=avg_int-interval
    c7=avg_int
    c8=avg_int+interval
    c9=avg_int+(2*interval)
    c10=avg_int+(3*interval)
    c11=avg_int+(4*interval)
    c12=avg_int+(5*interval)
    c13=avg_int+(6*interval)
    c14=avg_int+(7*interval) 

*    say 'var='var', i='i
*    say 'valid,min,max,avg,sdv='valid', 'min', 'max', 'avg', 'sdv

    if (valid !=0 )
       'set vpage 'xlo.i' 'xhi.i' 'ylo.i' 'yhi.i
       'set grads off'
       'set gxout stnmark'
       'set cmark 3'

       if (sndr = sndrd)
             'set rgb 16 80 30 225'
             'set rgb 17 245 95 50'
             'set clevs 'c1' 'c2' 'c3' 'c4' 'c5' 'c6' 'c7' 'c8' 'c9' 'c10' 'c11' 'c12' 'c13' 'c14
             'set ccols 9 14 16 4 11 5 13 3 10 7 12 8 17 2 6'
       endif

       'set digsize 0.1 0.1'
       if (satnam = goes)
          'set digsize 0.2 0.2'
       endif
       'd 'var
       'cbarnew.gs'
       'draw map'
       'set vpage off'

       'set string 1 l 6'
       'set strsiz 0.12 0.12'
       'draw string 'xlo.i+0.3' 'yhi.i-0.25' cnt,avg,sdv= 'valid', 'digs(avg,5)', 'digs(sdv,5)
       'draw string 'xlo.i+0.3' 'yhi.i-0.5' VT: 'date1
    endif
    if (valid = 0)
       'set vpage 'xlo.i' 'xhi.i' 'ylo.i' 'yhi.i
       'set grads off'
       'set gxout stnmark'
       'set cmark 3'

       if (field = obs)
          if (sndr = sndrd)
             'set rgb 16 80 30 225'
             'set rgb 17 245 95 50'
             'set clevs 'c1' 'c2' 'c3' 'c4' 'c5' 'c6' 'c7' 'c8' 'c9' 'c10' 'c11' 'c12' 'c13' 'c14
             'set ccols 9 14 16 4 11 5 13 3 10 7 12 8 17 2 6'
          endif
       endif

       'set digsize 0.1 0.1'
       if (satnam = goes)
          'set digsize 0.2 0.2'
       endif
       'd 'var
       'cbarnew.gs'
       'draw map'
       'set vpage off'

       'set string 1 l 6'
       'set strsiz 0.12 0.12'
       'draw string 'xlo.i+0.3' 'yhi.i-0.25' cnt,avg,sdv= 'valid', 'digs(avg,5)', 'digs(sdv,5)
       'draw string 'xlo.i+0.3' 'yhi.i-0.5' VT: 'date1
    endif


 total=total+valid    
 i=i+1
 endwhile

   '!rm -f info.txt'
   '!cat 'plotfile'.ctl |grep " 'chn', channel" > info.txt'
   result=read(info.txt)
   rc=sublin(result,1)
   iuse=0
   if (rc = 0)
      info=sublin(result,2)
      chan=subwrd(info,5)
      iuse=subwrd(info,8)
      error=subwrd(info,11)
      wave=subwrd(info,14)
      freq=subwrd(info,17)
   endif
   result=close(info.txt)
*   say 'chan,iuse,error,freq,wave = 'chan', 'iuse', 'error', 'freq', 'wave

 if (total != 0)
    'set strsiz 0.15 0.15'
    'set string 1 r 6'
    'draw string 10.8 8.3 frequency: 'freq' GHz'
    'draw string 10.8 8.1 wavelength: 'wave' `3m`0m'
    'set string 1 l 6'
    'set strsiz 0.15 0.15'
    if (iuse>0) 
       'draw string 0.2 8.3 platform:  'satnam' 'satnum
    endif
    if (iuse<=0) 
       'set string 2 l 6'
       'draw string 0.2 8.3 platform:  'satnam' 'satnum' (NOT ASSIMILATED)'
    endif
    'set string 1 l 6'
    'draw string 0.2 8.1 variable:  channel 'chan' 'type
*    say 'iuse='iuse
*	say plotfile'.'field'_'chn'.png'
    'printim 'plotfile'.'field'_'chn'.png 'xsize' 'ysize' white'
*    'enable print 'plotfile'.'field'_'chn
*    'print'
*    'disable print'
 endif
 if (total = 0)
    'set strsiz 0.15 0.15'
    'set string 1 r 6'
    'draw string 10.8 8.3 frequency: 'freq' GHz'
    'draw string 10.8 8.1 wavelength: 'wave' `3m`0m'
    'set string 1 l 6'
    'set strsiz 0.15 0.15'
    if (iuse>0)
       'draw string 0.2 8.3 platform:  'satnam' 'satnum
    endif
    if (iuse<=0)
       'set string 2 l 6'
       'draw string 0.2 8.3 platform:  'satnam' 'satnum' (NOT ASSIMILATED)'
    endif
    'set string 1 l 6'
    'draw string 0.2 8.1 variable:  channel 'chan' 'type
    'printim 'plotfile'.'field'_'chn'.png 'xsize' 'ysize
 endif


  if (debug = 1)
    say 'hit enter to continue'
    pull var
  endif

 chn=chn+1

endwhile

*'close 1'
*'quit'
return

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

function satinfo(var1,info)
  'q file'
  rec6=sublin(result,6)
  nvar=subwrd(rec6,5)
  i=1
  ii=0 
  while (i<=nvar)
    'q file'
     lin=sublin(result,6+i)
     var2=subwrd(lin,1)
     if (var1=var2)
       ii=i+6
       break
     endif
     i=i+1
  endwhile
  str=sublin(result,ii)
  if (info='channel')
     var2=subwrd(str,6)
  endif
  if (info='error') 
     var2=subwrd(str,8)
  endif
  if (info='iuse')
     var2=subwrd(str,10)
  endif
  if (info='wavelength')
     var2=subwrd(str,12)
  endif
  if (info='frequency')
     var2=subwrd(str,14)
  endif
return var2
