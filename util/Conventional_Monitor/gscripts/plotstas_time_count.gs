*   the program plot surface pressure time series:
*   data count: no assimilated, no rejected by gross check, no rejected
*   by variational qc,  no monitored


'reinit'
dtype=DTYPE
'open 'dtype'_stas_ges.ctl'
'open 'dtype'_stas_anl.ctl'
*'open 'dtype'_stas_int.ctl'
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
iyc=subwrd(size,6)
izc=subwrd(size,9)
say ixc
say 'iyc=' iyc

ix=1
while(ix <=ixc)
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

iy=1

while(iy <=iyc)

say 'iy=' iy
'!rm -f area.txt'
if ( iy <10)
'!cat 'dtype'_stas_ges.ctl |grep "region=  'iy' " > area.txt'
else
'!cat 'dtype'_stas_ges.ctl |grep "region= 'iy' " > area.txt'
endif
result=read(area.txt)
rc=sublin(result,1)
area="uknown"
if (rc = 0)
   info=sublin(result,2)
   area=substr(info,14,25)
endif
result=close(area.txt)
*say 'area = 'area
iz=1

while(iz <=izc)

if (dtype='q')
if(iz =1);levz='2000-0mb';endif
if(iz =2);levz='>=1000mb';endif
if(iz =3);levz='999-950mb';endif
if(iz =4);levz='949-900mb';endif
if(iz =5);levz='899-850mb';endif
if(iz =6);levz='849-800mb';endif
if(iz =7);levz='799-750mb';endif
if(iz =8);levz='749-700mb';endif
if(iz =9);levz='699-600mb';endif
if(iz =10);levz='599-500mb';endif
if(iz =11);levz='499-400mb';endif
if(iz =12);levz='399-300mb';endif
if(iz =13);levz='299-0mb';endif
else
if(iz =1);levz='2000-0mb';endif
if(iz =2);levz='>=1000mb';endif
if(iz =3);levz='999-900mb';endif
if(iz =4);levz='899-800mb';endif
if(iz =5);levz='799-600mb';endif
if(iz =6);levz='599-400mb';endif
if(iz =7);levz='399-300mb';endif
if(iz =8);levz='299-250mb';endif
if(iz =9);levz='249-200mb';endif
if(iz =10);levz='199-150mb';endif
if(iz =11);levz='149-100mb';endif
if(iz =12);levz='99-50mb';endif
if(iz =13);levz='49-0mb';endif
endif


plottime(ix,iy,iz,dtype,hh,dd,area,stype,subtype,iuse,levz,debug)


iz=iz+1
if( dtype='q'  | dtype='t')
   if(stype >=180 & iz >1);break;endif
else
   if(stype >=280 & iz >1 );break;endif
endif

endwhile
iy=iy+1
endwhile
ix=ix+1
endwhile

function plottime(ix,iy,iz,dtype,hh,dd,area,stype,subtype,iuse,levz,debug)

'clear'

nfield=4
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

title.1="assi. no."
title.2="no. rej. by VQC"
title.3="no. rej. by GC"
title.4="no. monitored"

nf=1
while(nf <=nfield)
y1=10.6-(nf-1)*2.5
y2=y1-1.8
ystring=y1+0.1
say ' y1='y1
say ' y2='y2
say ' ystring='ystring
'set t 1 last'
'query time'
'set y 'iy
'set x 'ix
'set z 'iz
 'set gxout stat'
   'd 'field.nf.1
   rec8=sublin(result,8)
   minvar1=subwrd(rec8,4)
   maxvar1=subwrd(rec8,5)
 'd 'field.nf.2
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
'set z 'iz
*'set axlim 'minvar' 'maxvar
*'set yaxis 'minvar' 'maxvar' 'dy
'set vrange 'minvar' 'maxvar
  'set ccolor 1'
  'set cmark 0'
  'd  'field.nf.1
   'set ccolor 2'
  'set cmark 1'
  'd  'field.nf.2
*   'set ccolor 3'
*  'set cmark 2'
*  'd  'field.nf.3
*  if(iuse = 1);datause='used';endif
  if(iuse = -1)
   datause='mon.'
  else
   datause='used'
  endif

  
'draw string '1.1' 'ystring' 'dtype''stype'-'subtype'('datause'):'title.nf' at 'area' for 'levz
'set line 1 1'
'draw line 1.1 0.6 1.4 0.6'
'draw string 1.5 0.55  init. outloop'
'set line 2 1'
'draw line 3.1 0.6 3.4 0.6'
'draw string 3.5 0.55  final outloop'
*'set line 3 1'
*'draw line 5.1 0.6 5.4 0.6'
*'draw string 5.5 0.55   second outloop'
nf=nf+1
endwhile

outfile=dtype''stype'-'subtype'_count_region'iy'_lev'iz'.png'

'printim 'outfile' x800 y700 white'
if(debug=1)
say 'enter'
pull
endif

return
