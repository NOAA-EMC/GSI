******
*--- initialize

'reinit'

hour=HOUR
dday=DDAY
dtype=DTYPE

'rgbset2'
'set background 99'
'clear'
'open 'dtype'_stas_ges.ctl'
'open 'dtype'_stas_anl.ctl'
*'open 'dtype'_stas_int.ctl'
'set grads off'
debug=0

'set t last'
'q time'
ti=sublin(result,1)
dmy=subwrd(ti,3)
say dmy
hh=substr(dmy,1,2)
dd=substr(dmy,4,2)

if(dd !=dday | hh !=hour); say 'wrong hour or day'; exit; endif
'q file'
size=sublin(result,5)
nxc=subwrd(size,3)
nyc=subwrd(size,6)
nzc=subwrd(size,9)

iy=1

while(iy <=nyc)
ix=1
while(ix <=nxc)
'!rm -f info.txt'
'!cat 'dtype'_stas_ges.ctl |grep "'ix' dtype=" > info.txt'
result=read(info.txt)
   rc=sublin(result,1)
   iuse=0
   if (rc = 0)
      info=sublin(result,2)
      stype=subwrd(info,6)
      subtype=subwrd(info,8)
      iuse=subwrd(info,10)
   endif
result=close(info.txt)
  if( iuse=-1 )
   datause='mon.'
  else
   datause='used'
  endif
  

plot_vert(ix,iy,nzc,dmy,dtype,stype,subtype,datause,iuse,debug)


ix=ix+1
endwhile
iy=iy+1
endwhile


function plot_vert(ix,iy,nzc,dmy,dtype,stype,subtype,datause,iuse,debug)

'page'
'clear'


field.1.1=count1.1
field.1.2=count1.2
field.1.3=count1.3
field.2.1=count_vqc1.1
field.2.2=count_vqc1.2
field.2.3=count_vqc1.3
field.3.1=count2.1
field.3.2=count2.2
field.3.3=count2.3
field.4.1=count3.1
field.4.2=count3.2
field.4.3=count3.3

title.1="no assi."
title.2="no rej. by VQC"
title.3="no rej. by GC"
title.4="no monitored"

nfield=4
nf=1
xp=1
yp=2
while(nf <=nfield)

say 'nf='nf

'run setvpage  'xp' 'yp' 2  2 0.9'
 'set annot 98'
'set line 98'
'set map 98'
'set xlopts 98'
'set ylopts 98'
 'set datawarn off'
 'set t last'
 'set x 'ix
 say 'x='ix
 'set y 'iy
 'set z 1'
 'define vmax=max('field.nf.1',z=2,z='nzc')'
'd vmax'
say result
cmax=subwrd(result,4)
say 'cmax='cmax
* if(cmax <1.0)
*   iy=iy+1
*   'page'
*   'clear'
*    say "continue " ix " " iy
*    continue
* endif
cmax=cmax+10
say 'cmax='cmax
 'set z 2 'nzc
 'set vrange 0 'cmax
   'set ccolor 98'
  'd 'field.nf.1
  'set ccolor 2'
  'd 'field.nf.2
*  'set ccolor 3'
*  'd 'field.nf.2
'draw title 'dtype''stype'-'subtype'('datause'):'title.nf' on 'dmy

if( nf =2 );xp=xp+1;endif;
yp=yp-1
if(yp =0 ); yp=2;endif;

nf=nf+1
endwhile

outfile=dtype''stype'-'subtype'_count_vert_region'iy'.png'

'printim 'outfile' x800 y650 white'
if(debug=1)
say 'enter'
pull
endif

