*   the program plot surface pressure time series:
*   data no, bias, rms,penalty and qc penalty


'reinit'
'open ps_stas_1.ctl' 
'open ps_stas_2.ctl' 
*'open ps_stas_3.ctl' 
'set grads off'
debug=0

'set t 1 last'
'q time'
dmy=sublin(result,1)
ti=subwrd(dmy,5)
say ti


hh=substr(ti,1,2)
dd=substr(ti,4,2)

'q file'
size=sublin(result,5)
ixc=subwrd(size,3)
say ixc
iyc=3
ix=1
while(ix <=ixc)
'!rm -f info.txt'
'!cat ps_stas_1.ctl |grep "'ix' dtype=" > info.txt'
result=read(info.txt)
   rc=sublin(result,1)
   iuse=0
   if (rc = 0)
      info=sublin(result,2)
      stype=subwrd(info,6)
      iuse=subwrd(info,8)
   endif
   result=close(info.txt)

   say 'stype='stype
iy=1
 while(iy <=iyc)

say 'while loop iy  ix= 'ix ' iy=' iy

*!plottime(ix,iy,stype,hh,dd)

*     if(debug=1)
*         say 'press aiy key to continue'
*         pull var
*      endif


*iy=iy+1
*endwhile
*ix=ix+1
*endwhile

*function plottime(ix,iy,stype,hh,dd)

'set string 1 l 5'
'set strsiz 0.12 0.12'
'set xlopts 1 4 0.12'
'set ylopts 1 4 0.13'

'clear'
'set mproj off'

nfield=5
field.2.1=bias.1
field.2.2=bias.2
field.2.3=bias.3
field.3.1=rms.1
field.3.2=rms.2
field.3.3=rms.3
field.4.1=rat.1
field.4.2=rat.2
field.4.3=rat.3
field.5.1=qcrat.1
field.5.2=qcrat.2
field.5.3=qcrat.3

title.1="number of observations"
title.2="bias: obs-ges(mb)"
title.3="rms (mb)"
title.4="contributionon to penalty"
title.5="contribution to penalty after qc "

*  for the count
* Set time
'set t 1 last'
'query time'
'set y 'iy
'set x 'ix
'set z 1'
 'set gxout stat'
   'd count.1'
   rec8=sublin(result,8)
   minvar1=subwrd(rec8,4)
   maxvar1=subwrd(rec8,5)
 'd count.3'
 rec8=sublin(result,8)
   minvar=subwrd(rec8,4)
   maxvar=subwrd(rec8,5)
   if(minvar > minvar1)
    minvar=minvar1
   endif
   if(maxvar1 > maxvar)
    maxvar=maxvar1
   endif
  if(maxvar <1.0) ; iy=iy+1 ; say "continue " ix " " iy ; continue ; endif
   maxvar=maxvar+20
   minvar=minvar-20 

'set parea 1.0 8.0 9.0 10.6'
'set gxout line'
'set t 1 last'
'set y 'iy
'set x 'ix
'set z 1'
*'set axlim 'minvar' 'maxvar
*'set yaxis ''minvar' 'maxvar' 'dy
  'set vrange 'minvar' 'maxvar
  'set ccolor 1'
  'set cmark 1'
  'd count.1'
  'set ccolor 2'
  'set cmark 2' 
  'd count.2'
  'set ccolor 3'
  'set cmark 3'
  'd count.3'
if(iy =1)
'draw string 1.5 10.7 ps'stype'(assimilated):'title.1' on 'dd''hh
endif
if(iy=2)
'draw string 1.5 10.7 ps'stype'(rejected by gross check):'title.1' on 'dd''hh
endif
if(iy=3) 
'draw string 1.5 10.7 ps'stype'(monitored):'title.1' on 'dd''hh
endif


nf=2

while(nf <=nfield)
y1=8.5-(nf-2)*2.1
y2=y1-1.6
ystring=y1+0.1
say ' y1='y1
say ' y2='y2
say ' ystring='ystring
'set t 1 last'
'set z 1'
'set x 'ix
'set y 'iy
'set gxout stat'
   'd 'field.nf.1
   rec8=sublin(result,8)
   maxvar1=subwrd(rec8,5)
   minvar1=subwrd(rec8,4)
  'd 'field.nf.3
   rec8=sublin(result,8)
   maxvar=subwrd(rec8,5)
   minvar=subwrd(rec8,4)
   if(minvar > minvar1)
    minvar=minvar1
   endif
   if(maxvar1 > maxvar)
    maxvar=maxvar1
   endif
   say ' 'minvar
   say ' 'maxvar
   yrange=maxvar-minvar
   dy=0.1*yrange
   minvar=minvar-dy
   maxvar=maxvar+dy
   say ' 'minvar
   say ' 'maxvar
'set parea 1.0 8.0 'y2' 'y1 
'set gxout line'
'set t 1 last'
'set datawarn off'
'set tlsupp year'
'set grads off'
'set y 'iy
'set x 'ix
'set z 1' 
*'set axlim 'minvar' 'maxvar
*'set yaxis 'minvar' 'maxvar' 'dy
'set vrange 'minvar' 'maxvar
  'set ccolor 1' 
  'set cmark 1'
  'd  'field.nf.1
   'set ccolor 2'
  'set cmark 2'
  'd  'field.nf.2
   'set ccolor 3'
  'set cmark 3'
  'd  'field.nf.3
if(iy =1)
'draw string '1.5' 'ystring' ps'stype'(assimilated):'title.nf' on 'dd''hh
endif
if(iy=2)
'draw string '1.5' 'ystring' ps'stype'(rejected):'title.nf' on 'dd''hh
endif
if(iy=3)
'draw string '1.5' 'ystring' ps'stype'(monitoed):'title.nf' on 'dd''hh
endif

nf=nf+1
endwhile

if(iy =1)
outfile='ps'stype'-'dd''hh'.png'
endif
if(iy =2)
outfile='ps'stype'_rej-'dd''hh'.png'
endif
if(iy =3)
outfile='ps'stype'_mon-'dd''hh'.png'
endif
'printim 'outfile' x800 y700 white'
if(debug=1)
say 'enter'
pull
endif

iy=iy+1
endwhile
ix=ix+1
endwhile

