* Script to plot all reduction values from a given GSI run
* 
*

function plotallreduction (args)

source=subwrd(args,1)
pdate=subwrd(args,2)
xsize=subwrd(args,3)
ysize=subwrd(args,4)

say 'process source, date 'source' , 'date

*
*  color assignments by cycle
*
*  00 = dk blue
*  06 = dk yellow
*  12 = red
*  18 = green
*
pcolor=3
cyc=substr(pdate,9,10)
if (cyc = 00) 
   pcolor=4
endif
if (cyc = 06)
   pcolor=12
endif
if (cyc = 12)
   pcolor=2
endif


'q file'
line5=sublin(result,5)
nchan=subwrd(line5,3)
tlast=subwrd(line5,12)
say 'nchan,tlast = 'nchan' 'tlast
*nchan=252

* Set time
'set t last'
'query time'
title.1=subwrd(result,3)

'clear'
'set grads off'
'set x 1 'nchan
'set y 1'
'define ave1=ave(reduct,t=1,t=29,4)'

'set string 1 l 5'
'set strsiz 0.12 0.12'
'set xlopts 1 4 0.12'
'set ylopts 1 4 0.13'
  
'clear'
'set mproj off'

*
*  Plot cycle
*
v1=0
v2=2
y1=8.1

   'set parea 0.6 8.3 'y1' 'y1+2.5
   'set xlint 10'
   'set vrange 'v1' 'v2
   'set gxout line'
   'set ccolor 'pcolor
   'set cmark 0'

   'set t last'
   'd reduct'
   'q time'
   cycle=subwrd(result,3)
*   date=substr(cycle,1,8)
   date=cycle
   'set strsiz 0.12'
   'set string 'pcolor' l 6'
   'draw string 8.0 7.1 'date

   'set t 1 29'
   'set ccolor 1'
   'set cmark 0'
   'd ave1'
   'set strsiz 0.12'
   'set string 1 l 6'
   'draw string 8.0 6.9  7day mean of 'cyc'z cycles'


   'set strsiz 0.2'
   'set string 1 c 6 0'
   'draw string 5.75 'y1'+2.0   'title.1'   'source'   reduction '

'set strsiz 0.15 0.15'
'set string 1 c 6 0'
'draw string 6.05 0.2   I  t  e  r  a  t  i  o  n    N  u  m  b  e  r'

'set string 1 c 6 90'
'draw string 1 4  R   e   d   u   c   t   i   o   n       V   a   l   u   e '

cyc=substr(date,1,2)

outfile=source'.'pdate'.reduction.png'
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

