* Script to plot horizontal maps of given field for given satellite instrument
* 
* Two arguments are expected
*    plotfile = satellite id (name and number ... e.g., msu.014 = noaa-14 msu)
*    field  = field to plot  (valid strings are:  obs, cor, obsges)

*function plothist (args)

plotfile=PLOTFILE
xsize=XSIZE
ysize=YSIZE
cdate=SDATE
*plotfile=t120
*xsize=x800
*ysize=y600
*platform=plotfile

*say 'plotfile='plotfile
*say 'field='field
*say 'xsize,ysize='xsize', 'ysize

'open  ges_'plotfile'.ctl'
'open anl_'plotfile'.ctl'

debug=0

'q file'
lin1=sublin(result,1)
dtype=subwrd(lin1,4)
cycle=subwrd(lin1,5)
say 'dtype=' dtype
say 'cycle='cycle 
histplot(dtype,cycle,plotfile,xsize,ysize,cdate)
if (debug = 1)
    say 'hit enter to continue'
    pull var
  endif

return

   
function histplot(satnam,satnum,plotfile,xsize,ysize,cdate)
'set display color white'
'clear'

i=1
y1=2
       
while( i<=2)

  if (i=1)
   cycle='ges'
  else
   cycle='anl'
  endif

 'run setvpage 1 'y1' 3 2 0.85'
       'set strsiz 0.18'
       'set string 2 bl 5'
      'set gxout scatter'
       if(i =1) 
       'd rmark.1;hist1.1'
       else
        'd rmark.2;hist1.2'
       endif
       rc=read(fileout)
       all=sublin(rc,2)
       no=subwrd(all,1)
       novqc=subwrd(all,2)
       nogros=subwrd(all,3)
       std=subwrd(all,4)
       mean=subwrd(all,5)
       'draw string 4.2 7.5  no.:'no 
       'draw string 4.2 7.0  std:'std 
       'draw string 4.2 6.5  mean:'mean 
       'draw string 0.7 7.5  no.rej. by vqc:'
       'draw string 0.7 7.0 'novqc
       'draw string 0.7 6.5  no.rej. by gross:'
       'draw string 0.7 6.0 'nogros 
       'draw title  'plotfile':'cycle',all data on 'cdate

    'run setvpage 2 'y1' 3 2 0.85'
       'set strsiz 0.18'
       'set string 2 bl 5'
      'set gxout scatter'
       'set digsiz 0.15'
       if(i =1)
       'd rmark.1;hist2.1'
       else
        'd rmark.2;hist2.2'
       endif
       rc=read(fileout)
       all=sublin(rc,2)
       no=subwrd(all,1)
       novqc=subwrd(all,2)
       nogros=subwrd(all,3)
       std=subwrd(all,4)
       mean=subwrd(all,5)
       'draw string 4.2 7.5  no.:'no 
       'draw string 4.2 7.0  std:'std 
       'draw string 4.2 6.5  mean:'mean 
       'draw string 0.7 7.5  no.rej. by vqc:'
       'draw string 0.7 7.0 'novqc
       'draw string 0.7 6.5  no.rej. by gross:'
       'draw string 0.7 6.0 'nogros 
       'draw title  'plotfile':'cycle',data rejected by OIQC'
    'run setvpage 3 'y1' 3 2 0.85'
       'set strsiz 0.18'
       'set string 2 bl 5'
       'set gxout scatter'
       'set digsiz 0.15'
       if(i =1)
       'd rmark.1;hist3.1'
       else
        'd rmark.2;hist3.2'
       endif
       rc=read(fileout)
       all=sublin(rc,2)
       no=subwrd(all,1)
       novqc=subwrd(all,2)
       nogros=subwrd(all,3)
       std=subwrd(all,4)
       mean=subwrd(all,5)
       'draw string 4.2 7.5  no.:'no 
       'draw string 4.2 7.0  std:'std 
       'draw string 4.2 6.5  mean:'mean 
       'draw string 0.7 7.5  no.rej. by vqc:'
       'draw string 0.7 7.0 'novqc
       'draw string 0.7 6.5  no.rej. by gross:'
       'draw string 0.7 6.0 'nogros 

       'draw title  'plotfile':'cycle',monitored data' 
  i=i+1
  y1=y1-1
 endwhile


    'printim 'plotfile'_hist.png 'xsize' 'ysize' white'


return

