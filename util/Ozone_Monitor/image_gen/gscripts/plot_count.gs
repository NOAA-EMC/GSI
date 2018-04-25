* Script to plot given bias correction term for given satellite instrument
* 
* Two arguments are expected
*    plotfile = satellite id (name and number ... e.g., msu.014 = noaa-14 msu)
*    field  = field to plot  (valid strings are:  count total fixang lapse lapse2 const scangl clw

*'reinit'
function plottime (args)

plotfile=subwrd(args,1)
field=subwrd(args,2)
xsize=subwrd(args,3)
ysize=subwrd(args,4)
platform=plotfile

*say 'process 'field' from 'plotfile
*'open 'plotfile'.ctl'

debug=0

'q file'
lin1=sublin(result,1)
satnam=subwrd(lin1,4)
satnum=subwrd(lin1,5)
nlev=subwrd(lin1,6)

if (field = count)
 type="number of observations"
endif

* Determine number of levels and regions
'q file'
lin1=sublin(result,1)
nlev=subwrd(lin1,6)
lin5=sublin(result,5)
nregion=subwrd(lin5,6)


*say 'nlev='nlev
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


'clear'
'set grads off'
'set y 'region

'set string 1 l 5'
'set strsiz 0.12 0.12'
'set xlopts 1 4 0.12'
'set ylopts 1 4 0.13'

fr=0
i=1
levn=1
while (levn<=nlev)
*   say 'top of level loop with levn= 'levn
   'set x 'levn
      'define avg='field
      'set gxout stat'
      'd avg'
      rec14=sublin(result,14)
      avgsdv=subwrd(rec14,2)
      'define sdv='field
      'set gxout line'
   levi=levn
   if (i=1) 
      'clear'
      y1=7.3
      clo=levn
      clast=clo+3
   endif
   if (i>1 & i<4) 
      y1=y1-2.4
   endif
   if (i=4) 
      y1=y1-2.4
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

      minsdv=minvar
      maxsdv=maxvar
      valsdv=valvar

   '!rm -f info.txt'
   '!cat 'plotfile'.ctl |grep "'levn', level" > info.txt'
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
*   say 'level,iuse,error = 'level', 'iuse', 'error'
   
   'set strsiz 0.12 0.12'
   'set string 1 l 6'
   'draw string 0.1 'y1+2.4' pressure 'level
   'draw string 0.1 'y1+2.2' level 'levn
   'set string 4 l 6'
   'draw string 0.1 'y1+1.5' avg: 'digs(avgvar,4)
   'set string 2 l 6' 
   'draw string 0.1 'y1+1.3' sdv: 'digs(avgsdv,4)
   if (iuse<=0) 
      'set string 3 l 6'
      'draw string 0.1 'y1+1.1' PRESSURE 'level
      'set string 9 l 6'
      'draw string 0.1 'y1+0.9' ** IS NOT **'
      'set string 3 l 6'
      'draw string 0.1 'y1+0.7' ASSIMILATED'
   endif

   if (valvar != 0) 
      ymin=minvar
      ymax=maxvar
   endif
   if (valvar = 0)
      ymin=0
      ymax=0
   endif

   if (minsdv<ymin & valsdv!=0)
      ymin=minsdv
   endif
   if (maxsdv>ymax & valvar!=0)
      ymax=maxsdv
   endif

   yrange=ymax-ymin
   dy=0.1*yrange
   ymin=ymin-dy
   ymax=ymax+dy


   'set vpage 0.0 8.5 'y1' 'y1+3.2
   'set grads off'
   'set gxout line'

   'set vrange 'ymin' 'ymax    
   'set ccolor 4'
   'd avg'
   'set vpage off'

   i=i+1
   if (i=5 | levn=nlev)
      fr=fr+1
      'set string 1 l 6'
      'set strsiz 0.15 0.15'
      'draw string 0.2 10.80 platform:  'plotfile
      'draw string 0.2 10.55 region  :  'area
      'draw string 0.2 10.30 variable:  'type
      'draw string 0.2 10.05 valid   :  'date1' to 'date2
      outfile=plotfile'.'field'_region'region'_fr'fr'.png'
      'printim 'outfile' 'xsize' 'ysize' white'
*      say 'output to file 'outfile
      if (debug=1) 
         say 'press any key to continue'
         pull var
      endif
      i=1
   endif
   levn=levn+1
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

