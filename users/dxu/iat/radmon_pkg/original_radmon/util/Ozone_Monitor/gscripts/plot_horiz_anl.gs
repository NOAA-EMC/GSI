* Script to plot horizontal maps of given field for given satellite instrument
* 
* Two arguments are expected
*    plotfile = satellite id (name and number ... e.g., sbuv2_n17 = noaa-17 sbuv/2)
*    field  = field to plot  (valid strings are:  obs, ges, obsges)

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
nlev=subwrd(lin1,6)

if (field = obs)
 type="observation "
endif
if (field = ges)
 type=" analysis "
endif
if (field = obsges)
 type="obs - anl "
endif
if (field = sza)
 type="solar zenith angle"
endif
if (field = fovn)
 type="field of view number"
endif

* Set (lat,lon) box to plot
lon1=0
lon2=360
lat1=-90
lat2=90
   
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

mxlev=nlev
levn=1
while (levn<=mxlev)
 'clear'
 var=field''levn
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
 
*    say 'var='var', i='i
*    say 'valid,min,max,avg,sdv='valid', 'min', 'max', 'avg', 'sdv

    if (valid !=0 )
       'set vpage 'xlo.i' 'xhi.i' 'ylo.i' 'yhi.i
       'set grads off'
       'set gxout stnmark'
       'set cmark 3'
       'set digsize 0.1 0.1'
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
       'set digsize 0.1 0.1'
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
   '!cat 'plotfile'.ctl |grep " 'levn', level" > info.txt'
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
*   say 'levn,iuse,error = 'levn', 'iuse', 'error'

 if (total != 0)
    'set strsiz 0.15 0.15'
    'set string 1 r 6'
    'set string 1 l 6'
    'set strsiz 0.15 0.15'
    if (iuse>0) 
       'draw string 0.2 8.3 platform:  'satnam' 'satnum
    endif
    if (iuse<=0) 
       'set string 2 l 6'
       'draw string 0.2 8.3 platform:  'satnam' 'satnum' (NOT ASSIMILATED)'
    endif
    if ( level > 0.0 )
       'set string 1 l 6'
       'draw string 0.2 8.1 variable:  'type
       'set string 1 l 6'
       'draw string 7.5 8.1 level'levn' (pressure:' level'mb)'
    endif
    if ( level <= 0.0 )
       'set string 1 l 6'
       'draw string 0.2 8.1 variable:  'type
       'set string 1 l 6'
       'draw string 7.5 8.1 level'levn' (pressure: total)'
    endif
*    say 'iuse='iuse
*	say plotfile'.'field'_'levn'.png'
    'printim 'plotfile'.'field'_'levn'.png 'xsize' 'ysize' white'
*    'enable print 'plotfile'.'field'_'levn
*    'print'
*    'disable print'
 endif
 if (total = 0)
    'set strsiz 0.15 0.15'
    'set string 1 r 6'
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
    'draw string 0.2 8.1 variable:  level 'levn' 'type
    'printim 'plotfile'.'field'_'levn'.png 'xsize' 'ysize
 endif


  if (debug = 1)
    say 'hit enter to continue'
    pull var
  endif

 levn=levn+1

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
  if (info='level')
     var2=subwrd(str,6)
  endif
  if (info='error') 
     var2=subwrd(str,8)
  endif
  if (info='iuse')
     var2=subwrd(str,10)
  endif
return var2
