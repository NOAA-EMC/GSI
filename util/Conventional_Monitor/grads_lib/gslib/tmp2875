function main(args)

say 'args = 'args

datadir='\grads\data\'
basin='w'
datatype='sf'

dtgb=92100200
dtge=92100400
dtginc=24

*
*     open the files
*

dtg0=dtgb
i=0
while(dtg0<=dtge)

  i=i+1
  dname=basin%datatype

  if(substr(dtg0,3,2)<=9) 
    dname=dname%substr(dtg0,4,5)
  endif

  if(substr(dtg0,3,2)='10') 
    dname=dname%a%substr(dtg0,5,4)
  endif
  if(substr(dtg0,3,2)='11') 
    dname=dname%b%substr(dtg0,5,4)
  endif
  if(substr(dtg0,3,2)='12') 
    dname=dname%c%substr(dtg0,5,4)
  endif
  
  dname=dname%'.ctl'

  df.dtg0=ofile(datadir%dname)
  say 'df.'dtg0' = 'df.dtg0' from 'datadir%dname
  dtg0=incdtgh(dtg0,dtginc)

endwhile

if(df.dtge!=0)
  'set dfile 'df.dtge
endif

tb=1
te=7
tinc=2
dt=12

'set mpdset mres'
'set mpdset lowres'
'set map 1 1 7'
'set lat 00 60'
'set lon 100 180'
t0=tb

xl1v='0.0'
xr1v='11.00'
yb1v='0.00'
yt1v='8.50'

xl1='1.0'
xr1='10.0'
yb1='0.50'
yt1='7.00'

setpage(xl1v,xr1v,yb1v,yt1v,xl1,xr1,yb1,yt1)

var=vort

dvar='t'
while(t0<=te)

  'set t 't0

  time=curtime()
  result=curdtgh()
  idtg=subwrd(result,1)
  cdtg=subwrd(result,2)
  
  hour=(t0-1)*dt
  tt1='NOGAPS 'var' 'cdtg' t = 'hour
  tt2=''
*
*     look for analysis data if time is le 0
*

  if(t<=0)

   'set

  endif

  if(dvar='t') 
    toptitle(xl1,xr1,yt1,yb1,tt1,tt2)

    'define spd=mag(us,vs)'
    'define vort=hcurl(us,vs)*1.0e5'

    dtype=vort

    if(dtype='stream')
    'set csmooth on'
    'set gxout shaded'
    'set clevs 15 30'
    'set ccols 0 4 2'
    'd spd'

    'set gxout stream'
    'set strmden 2'
    'd us;vs;spd'

    'set gxout contour'
    'set clevs 10 15 30'
    'set cthick 10'
    'set ccols 1 1 1'
    'd spd'
    endif

    if(dtype='vort')
      'set cint 2.0'
      'd vort'
    endif

  else
    loopinst(xl1,xr1,yt1,yb1)
  endif

  t0=loopctrl(t0,tinc,dvar)

endwhile


*
*ffffffffffffffffff     function definitions      ffffffffffffffffff
*

function setpage(xlv,xrv,ybv,ytv,xl,xr,yb,yt)
  setv='set vpage 'xlv' 'xrv' 'ybv' 'ytv
  setv
  setp='set parea 'xl' 'xr' 'yb' 'yt
  setp
  'set grads off'
return

function loopinst(xl,xr,yt,yb)
  xs=xl+(xr-xl)*0.5
  ys=yt-0.25
  'set strsiz 0.25'
  'set string 1 c 6'
  t1=loop instructions
  'draw string 'xs' 'ys' 't1
return


function toptitle(xl,xr,yt,yb,t1,t2)
  xs=xl+(xr-xl)*0.5
  ys=yt+0.25
  'set strsiz 0.25'
  'set string 1 c 6'
  'draw string 'xs' 'ys' 't1
*  'set string 0 c 6'
*  'set strsiz 0.10'
*  'draw string 'xs' 10.65 't2
*  say 'ending toptitle'
return

function loopctrl(t0,tinc,dvar)
  while(1)
   pull keycmd
   if(keycmd='c'|keycmd='h'|heycmd='?')
     'c'
     dvar='f'
   endif
   if(keycmd='f')
     'c'
     t0=t0+tinc
     break
   endif
   if(keycmd='b')
     'c'
     t0=t0-tinc
     break
   endif
   if(keycmd='q')
     'c'
     t0=99999
     break
   endif

endwhile
return(t0)

function curtime
  'q time'
  t1=subwrd(result,3)
  hr=substr(t1,1,3)
  da=substr(t1,4,2)
  mo=substr(t1,6,3)
  yr=substr(t1,9,4)
return (hr' 'da' 'mo' 'yr)

function curdtgh
*
*  convert current time to dtg 
*
  moname='JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC'
  'q time'
  t1=subwrd(result,3)
  iyr=substr(t1,11,2)
  nmo=substr(t1,6,3)
  ida=substr(t1,4,2)
  ihr=substr(t1,1,2)
  i=1
  while (nmo!=subwrd(moname,i));i=i+1;endwhile
  imo=i
  idtg=iyr*1000000+imo*10000+ida*100+ihr
  cdtg=iyr%imo%ida%ihr

return (idtg' 'cdtg)

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

function incdtgh(dtgh,inc)
*
*  increment a dtg by inc hours
*  RESTRICTIONS!!  
*  (1)  inc > 0
*  (2)  inc < 24
*
  moname='JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC'
  monday='31 28 31 30 31 30 31 31 30 31 30 31'

  iyr=substr(dtgh,1,2)*1
  imo=substr(dtgh,3,2)*1
  ida=substr(dtgh,5,2)*1
  ihr=substr(dtgh,7,2)*1

  if(mod(iyr,4)=0) 
    monday='31 29 31 30 31 30 31 31 30 31 30 31'
  endif

  ihr=ihr+inc
*  say 'ihr = 'ihr
*  say 'ida = 'ida

  if(ihr>=24)
    ihr=ihr-24
    ida=ida+1
  endif

*  say 'new ihr = 'ihr' new ida = 'ida' imo = 'imo

  if(ida > subwrd(monday,imo))
    ida=ida-subwrd(monday,imo)
*    say 'inside check ida = 'ida' monday = 'subwrd(monday,imo)
    imo=imo+1
  endif

  if(ida <= 0)
    imo=imo-1
    ida=subwrd(monday,imo)-ida+1
  endif

  if(imo>=13)
    imo=imo-12
    iyr=iyr+1
  endif

if(imo<10);imo='0'imo;endif
if(ida<10);ida='0'ida;endif
if(ihr<10);ihr='0'ihr;endif

return (iyr%imo%ida%ihr)

function int(i0)
  i=0
  while(i<12)
    i=i+1
    if(substr(i0,i,1)='.') 
      i0=substr(i0,1,i-1)
      break
    endif
  endwhile
return(i0)

function mod(i0,inc)
  if(inc!=0) 
    imod=int(i0/inc)
  else
    imod=int(i0/1)
  endif 
  imod=i0-imod*inc
return(imod)

