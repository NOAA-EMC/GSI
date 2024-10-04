* Script to plot all gnorm values from a given GSI run
* 
*

function plotallgnorms (args)

source=subwrd(args,1)
date=subwrd(args,2)
xsize=subwrd(args,3)
ysize=subwrd(args,4)

say 'process source 'source

*nchan=252

'query file'
line5=sublin(result,5)
nchan=subwrd(line5,3)
tlast=subwrd(line5, 12)
say 'tlast = 'tlast
last.1=29
last.2=28
last.3=27
last.4=26

'set t 29'
'define ave1=ave(allgnorm,t=1,t=29)'

* Set time
'set t 'last.4' 'last.1
'query time'
title.1=subwrd(result,3)
title.2=subwrd(result,5)

*'set t 2'
*'query time'
*date2=subwrd(result,3)
*title.2='date2'

*'set t 3'
*'query time'
*date3=subwrd(result,3)
*title.3='date3'

*'set t 4'
*'query time'
*date4=subwrd(result,3)
*title.4='date4'

*'set t 1'

'clear'
'set grads off'
'set x 1 'nchan
'set y 1'

'set string 1 l 5'
'set strsiz 0.12 0.12'
'set xlopts 1 4 0.12'
'set ylopts 1 4 0.13'
  
'clear'
'set mproj off'

*
*  Plot the 4 cycles
*
v1=-13
v2=1
y1=8.1

   'set parea 0.6 8.3 'y1' 'y1+2.5
   'set xlint 10'
   'set vrange 'v1' 'v2
   'set gxout line'

   'set t 'last.4
   'q time'
   cycle=subwrd(result,3)
   date=substr(cycle,1,8)
   cyc=substr(cycle,1,2)
   clr=gcolor(cyc)

   'set ccolor 'clr
   'set cmark 0'
   'd log(allgnorm)'

   'set strsiz 0.12'
   'set string 'clr' l 6'
   'draw string 7.45 6.5 'date


   'set t 'last.3
   'q time'
   cycle=subwrd(result,3)
   date=substr(cycle,1,8)
   cyc=substr(cycle,1,2)
   clr=gcolor(cyc)

   'set ccolor 'clr
   'set cmark 0'
   'd log(allgnorm)'

   'set strsiz 0.12'
   'set string 'clr' l 6'
   'draw string 7.45 6.7 'date

   'set t 'last.2
   'q time'
   cycle=subwrd(result,3)
   date=substr(cycle,1,8)
   cyc=substr(cycle,1,2)
   clr=gcolor(cyc)
   'set ccolor 'clr
   'set cmark 0'
   'd log(allgnorm)'

   'set strsiz 0.12'
   'set string 'clr' l 6'
   'draw string 7.45 6.9 'date

   'set t 'last.1
   'q time'
   cycle=subwrd(result,3)
   date=substr(cycle,1,8)
   cyc=substr(cycle,1,2)
   clr=gcolor(cyc)

   'set ccolor 'clr
   'set cmark 0'
   'd log(allgnorm)'

   'set strsiz 0.12'
   'set string 'clr' l 6'
   'draw string 7.45 7.1 'date

   'set ccolor 1'
   'set cmark 0'
   'd log( ave1 )'

   'set strsiz 0.12'
   'set string 1 l 6'
   'draw string 7.45 6.3 7day mean'

   'set strsiz 0.2'
   'set string 1 c 6 0'
   'draw string 5.75 'y1'+2.0  'title.1' - 'title.2' 'source'   gnorm '


'set strsiz 0.15 0.15'
'set string 1 c 6 0'
'draw string 6.05 0.2   I  t  e  r  a  t  i  o  n    N  u  m  b  e  r'

'set string 1 c 6 90'
'draw string 1 4   l   o   g   ( g   n   o   r   m )'
'set string 1 c 6 0'

outfile=source'.4cycle.gnorms.png'
'printim 'outfile' 'xsize' 'ysize' white'

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

function gcolor(string)
  if ( string = 00 )
     clr=4
  endif
  if ( string = 06 )
     clr=12
  endif
  if ( string = 12 )
     clr=2
  endif
  if ( string = 18 )
     clr=3
  endif
return clr
