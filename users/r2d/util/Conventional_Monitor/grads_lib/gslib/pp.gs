function main(args)

datopt=subwrd(args,1)
lats=subwrd(args,2)
latn=subwrd(args,3)
tinc=subwrd(args,4)

if(lats = '' | latn = '')
  lats=-7.5
  latn=7.5
endif

if(tinc='')
  tinc=0
endif

if(datopt='')
  datopt='ng'
endif
if(lats < 0)
  latstt=-lats%'S'
else
  latstt=lats%'N'
endif

if(lats = 0)
  latstt='EQ'
endif

if(latn < 0)
  latntt=latn%'S'
else
  latntt=latn%'N'
endif

if(latn = 0)
  latntt='EQ'
endif

if(datopt='ng')
  varopt='ppt'
  datfile='ng.102912.012200.precip'
  tt='NOGAPS 12h precip (mm/day) 'latstt'-'latntt' 92102900 --> 93012200'
endif

if(datopt='gpcp')
  varopt='gpp'
  datfile='/mf_disk/fiorino/data/gpcp/gpcp.nogoes.9206.9212'
  datfile='../../data/gpcp/gpcp.nogoes.9206.9212'
  tt='Prelim GPCP pentads (mm/day) 'latstt'-'latntt' 92102900 --> 93012200'
endif

d1=ofile(datfile'.ctl')
'say d1 = 'd1

'enable print wprint.gm'

pvar='tloop(ave(pp1,t-'tinc',t+'tinc'))'

shdopt='grfill'
shdopt='shaded'
csmthopt='on'

if(tinc=0)
  plevspp='5 10 15 20 25'
  pcolspp='0 4 3 2 1'
endif

if(tinc > 2)
  plevspp='5 10 15 20 25'
  pcolspp='0 4 3 2 1'
endif

xl1v=0
xr1v=8.5
yb1v=0.0
yt1v=10.0

xl1=1.25
xr1=7.5
yb1=0.50
yt1=9.75

'set dfile 'd1
'set time 00Z1oct92 00Z1mar93'
'set y 1'
'set lon 60 230'
'q dims'
*
*	define the average precip
*

if(datopt='gpcp')
  'define pp1=ave('varopt',lat='lats',lat='latn')'
endif

if(datopt='ng')
  'define pp1=ave('varopt'*20,lat='lats',lat='latn')'
  'define pp1=smth9(pp1)'
endif

'set vpage 'xl1v' 'xr1v' 'yb1v' 'yt1v
'set parea 'xl1' 'xr1' 'yb1' 'yt1
'set grads off'

'set yflip on'

'set gxout 'shdopt
'set csmooth 'csmthopt
*'set clevs 'plevspp
*'set ccols 'pcolspp
'set cmin 3'
'set cmax 30'
'set cint 3'
'set rbrange 3 30'
'set rbcols 9 4 5 3 7 2 6 1'
'd 'pvar
'run cbar.gs'

'set gxout contour'
'set csmooth 'csmthopt
'set cthick 8'
*'set clevs 'plevspp
'set cmin 3'
'set cmax 30'
'set cint 3'
'd 'pvar

'set vpage off'
ttsiz='0.13'
rc=plotitle(xl1v,xr1v,yb1v,10.25,ttsiz,tt)

ttsiz='0.10'
if(tinc='0')
  tt2='no time ave'
else
  tincout=tinc/2
  tt2='time ave +/- 'tincout' day'
endif

rc=plotitle(xl1v,xr1v,yb1v,9.90,ttsiz,tt2)

if(bchopt='y');'quit';endif

function plotitle(xl,xr,yb,yt,siz,title)
*
*  plot title
*
  xs=xl+(xr-xl)*0.5
  ys=yt+0.15
  'set strsiz 'siz
  'set string 1 c 6'
  'draw string 'xs' 'ys' 'title
return

function ofile (fname)
  'query files'
  i = 0
  while (1)  
    if (subwrd(result,1)='No')       
      ret = 0
      break;
    endif
    rec = sublin(result,i*3+2)
    if (rec='') 
      ret = 0;
      break; 
    endif
    if (subwrd(rec,2)=fname)
      rec = sublin(result,i*3+1)
      ret = subwrd(rec,2)
      break;
    endif
    i = i + 1
  endwhile
  if (ret=0) 
    'open 'fname
    if (rc>0) 
      say "Error opening "fname
      return (0)
    endif
    rec = sublin(result,2)
    ret = subwrd(rec,8)
  endif
  return (ret)

