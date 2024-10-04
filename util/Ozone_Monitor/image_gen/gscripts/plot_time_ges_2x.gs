* Script to plot given bias correction term for given satellite instrument
* 
* Expected arguments:
*    net      = $NET value, or the identifying data source (e.g. GFS|fv3rt1)
*    run      = $RUN value, typically gfs|gdas
*    plotfile1 = satellite id (name and number ... e.g., msu.014 = noaa-14 msu)
*    plotfile2 = satellite id (name and number ... e.g., msu.014 = noaa-14 msu)
*    field    = field to plot  (valid strings are:  cnt total fixang lapse lapse2 const scangl clw
*    xsize    = horiz size of plotted image
*    ysize    = vert size of plotted image

*'reinit'
function plottime (args)

net=subwrd(args,1)
run=subwrd(args,2)
plotfile1=subwrd(args,3)
plotfile2=subwrd(args,4)
field=subwrd(args,5)
xsize=subwrd(args,6)
ysize=subwrd(args,7)
*platform1=plotfile1
*platform2=plotfile2

say 'process 'field' from 'plotfile1
say 'process 'field' from 'plotfile2
* 'open 'plotfile1'.ges.ctl'
* 'open 'plotfile2'.ges.ctl'

debug=0

'q file'
lin1=sublin(result,1)
satnam=subwrd(lin1,4)
satnum=subwrd(lin1,5)
nlev=subwrd(lin1,6)

if (field = cnt)
 type="number of observations"
endif
if (field = omg )
 type="obs-ges"
endif
if (field = cpen )
 type="contribution to penalty"
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
'!cat 'plotfile1'.ges.ctl |grep "region= 'region' " > area.txt'
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
'set strsiz 0.11 0.11'
'set xlopts 1 4 0.11'
'set ylopts 1 2 0.09'

fr=0
i=1
levn=1
while (levn<=nlev)
*   say 'top of level loop with levn= 'levn
*
*-----------------------------------------------------
*  Note:  here avg and sdv are being (mis)used in 
*   the cases of cnt and cpen.  They are not avg
*   and sdv values, but someone was being cute and
*   rather than add another if( ) case, decided all
*   cases could use avg and sdv when actually 
*   plotting.  That, predictably, took a while to 
*   understand.  I'd fix it but ultimately I'm 
*   going to do away with GrADS plots in favour of
*   using javascript to make things on-the-fly.
*   So for now think of this note as a sort of
*
*     "Warning:  Logical Minefield" 
*
*-----------------------------------------------------
   'set x 'levn
   if (field != "cnt" & field != "cpen")
      'define avg1=avg'field'.1'
      'define sdv1=sdv'field'.1'
      'define avg2=avg'field'.2'
      'define sdv2=sdv'field'.2'
   endif
   if (field = "cnt" | field = "cpen")
      'define avg1='field'.1'
      'define sdv1='field'.1'
      'define avg2='field'.2'
      'define sdv2='field'.2'


      'set gxout stat'
      'd avg1'
      rec14=sublin(result,14)
      avgsdv1=subwrd(rec14,2)
   endif
   levi=levn
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
   'd avg1'
   rec7=sublin(result,7)
   rec8=sublin(result,8)
   rec11=sublin(result,11)
   valvar1=subwrd(rec7,8)
   minvar1=subwrd(rec8,4)
   maxvar1=subwrd(rec8,5)
   avgvar1=subwrd(rec11,2)

   'set gxout stat'
   'd avg2'
   rec7=sublin(result,7)
   rec8=sublin(result,8)
   rec11=sublin(result,11)
   valvar2=subwrd(rec7,8)
   minvar2=subwrd(rec8,4)
   maxvar2=subwrd(rec8,5)
   avgvar2=subwrd(rec11,2)

   minvar=minvar1 
   if (minvar2 <= minvar)
      minvar=minvar2 
   endif
*   say 'minvar1, minvar1, minvar = 'minvar1', 'minvar2', 'minvar

   maxvar=maxvar1 
   if (maxvar2 >= maxvar)
      maxvar=maxvar2 
   endif
*   say 'maxvar1, maxvar1, maxvar = 'maxvar1', 'maxvar2', 'maxvar

   say 'avg1'field' min,max,avg1='minvar1','maxvar1','avgvar1','valvar1

   if (field != "cnt" & field != "cpen")
      'set gxout stat'
      'd sdv1'
      rec7=sublin(result,7)
      rec8=sublin(result,8)
      rec11=sublin(result,11)
      valsdv1=subwrd(rec7,8)
      minsdv1=subwrd(rec8,4)
      maxsdv1=subwrd(rec8,5)
      avgsdv1=subwrd(rec11,2)

      'set gxout stat'
      'd sdv2'
      rec7=sublin(result,7)
      rec8=sublin(result,8)
      rec11=sublin(result,11)
      valsdv2=subwrd(rec7,8)
      minsdv2=subwrd(rec8,4)
      maxsdv2=subwrd(rec8,5)
      avgsdv2=subwrd(rec11,2)

      minsdv=minsdv1 
      if (minsdv2 <= minsdv)
         minsdv=minsdv2 
      endif

      maxsdv=maxsdv1 
      if (maxsdv2 >= maxsdv)
         maxsdv=maxsdv2 
      endif
*      say 'sdv'var' min,max,avg1='minsdv','maxsdv','avgsdv
   endif

   if (field = "cnt" | field = "cpen")
      minsdv=minvar
      maxsdv=maxvar
*      valsdv=valvar1
   endif

   '!rm -f info.txt'
   '!cat 'plotfile1'.ges.ctl |grep "'levn', level" > info.txt'
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
   if ( level > 0.0 )
      'draw string 0.1 'y1+1.5' pressure 'level
   endif

   if ( level = 0.0 )
      'draw string 0.1 'y1+1.5' pressure total'
   endif

   'set string 1 l 6'

   'draw string 0.1 'y1+1.3' level 'levn
   'set string 4 l 6'
   'draw string 0.1 'y1+1.1' avg: 'digs(avgvar1,14)
*   'set string 2 l 6'
   'draw string 0.1 'y1+0.9' sdv: 'digs(avgsdv1,14)

   if (iuse<=0)
      'set string 9 l 6'
      'draw string 0.1 'y1+0.5' ** IS NOT **'
      'set string 3 l 6'
      'draw string 0.1 'y1+0.3' ASSIMILATED'
   endif

   'set parea 2.1 7.8 'y1' 'y1+1.2
   'set grads off'
   'set tlsupp year'
   'set ylpos 0 r'
   yrange=maxvar-minvar
   dy=0.2*yrange
   ymin=minvar-dy
   ymax=maxvar+dy
   yrange=ymax-ymin
   'set gxout line'
   'set vrange 'ymin' 'ymax
*   say 'minvar, dy, maxvar, yrange,ymin,ymax = 'minvar','dy','maxvar','yrange','ymin','ymax


   if (field != "cnt" & field != "cpen")
      aymin=ymin
      if(ymin <0)
        aymin=-ymin
      endif
      if( aymin >0.0001)
        'set ylab %.4f'
      endif
      dy=0.2*yrange
      'set ylint 'dy

*     'set yaxis 'ymin' 'ymax
*     'set ylab %.4f'
*     yr=(ymax-ymin)/2
*     ymid=ymin+yr
*     ym1=ymin+yr*0.5
*     ym2=ymid+yr*0.5
*     say ' ymid='ymid
*     say ' ym1='ym1
*     say ' ym2='ym2
*    'set ylevs 'ymin' 'ym1' 'ymid' 'ym2' 'ymax
   endif
   'set ccolor 4'
   'set cmark 1'
   'd avg1'
   'set ccolor 2'
   'set cmark 1'
   'd avg2'
   'set parea off'

   if (field != "cnt" & field != "cpen")
      'set parea 2.1 7.8 'y1+1.25' 'y1+2.1 
      'set grads off'
      'set tlsupp year'
      'set tlsupp month'
      'set timelab off'
      'set ylpos 0 r'
      yrange=maxsdv-minsdv
      dy=0.1*yrange
      ymin=minsdv-dy
      ymax=maxsdv+dy
      yrange=ymax-ymin

      'set gxout line'
      'set vrange 'ymin' 'ymax
*     'set yaxis 'ymin' 'ymax
      if( ymin >0.0001)
         'set ylab %.4f'
      endif

*     yr=(ymax-ymin)/2
*     ymid=ymin+yr
*     ym1=ymin+yr*0.5
*     ym2=ymid+yr*0.5
*     say ' ymid='ymid
*     say ' ym1='ym1
*     say ' ym2='ym2
*     'set ylevs 'ymin' 'ym1' 'ymid' 'ym2' 'ymax

      ymid=0.2*yrange
      'set ylint 'ymid

      'set ccolor 4'
      'set cmark 1'
      'd sdv1'
      'set ccolor 2'
      'set cmark 1'
      'd sdv2'

      'set parea off'
   endif

   i=i+1
   if (i=5 | levn=nlev)
      fr=fr+1
      'set string 1 l 6'
      'set strsiz 0.15 0.15'
      'draw string 0.2 10.80 Net,run :  'net', 'run

      'draw string 0.2 10.55 platform:  '
      'set string 4 l 6'
      'draw string 1.5 10.55 'plotfile1
      'set string 2 l 6'
      'draw string 3.5 10.55 'plotfile2

      'set string 1 l 6'
      'draw string 0.2 10.30 region  :  'area
      'draw string 0.2 10.05 variable :  'type
      'draw string 0.2  9.80 valid   :  'date1' to 'date2
      outfile=plotfile1'.ges.'field'_region'region'_fr'fr'.png'
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

