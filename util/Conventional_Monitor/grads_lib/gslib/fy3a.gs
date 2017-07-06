script='[FY3a]'
*
*       open files
*
datadir='/mf_disk/fiorino/grads/data/'
d1=ofile(datadir'amip_f_6h.ctl')

d2=ofile(datadir'amip_y_6h.ctl')
say 'd1,d2 = 'd1' 'd2

prntfile='fy.gd'

if (d1=0 | d2=0) 
  say 'Error opening files'
  return
endif
*
*       define the grid functions?
*
  defunc='y'
*       grey shade call done or use color?

*
*       set time interval of the loop using month dtg
*
dtghbeg='79011100'
dtghend='79011500'
dtghinc='24'
*
*       averaging in time
*
tave='n'
taveinc='1'
'set grads off'
'set csmooth on'

*
*       loop and printing options
*
looper='on'
printer='off'

if(looper!='on'&printer='on');'enable print 'prntfile;endif
*
*       convert ending dtgh to a number for checking
*
ndtghend=ndtgh(dtghend)

pltrgn='usa'
*pltrgn='nhm'
*pltrgn='global'
*pltrgn='westpac'

pltvar='slp'
pltvar='phi500'
*pltvar='gtmp'
*pltvar='slp'

*
*  default map projection
*
dpmprj='scaled'

dp1var=pltvar

*
* westpac
*
if(pltrgn='westpac')
  lats='-60'
  latn='60'
  lonw='0'
  lone='220'
 dpz='0'
endif
*
* austalian region
*
if(pltrgn='aus') 
  lats='-60'
  latn='0'
  lonw='80'
  lone='180'
  dpz='0'
endif
*
* global
*
if(pltrgn='global')
  lats='-60'
  latn='60'
  lonw='-20'
  lone='340'
  dpz='0'
endif

*
* nhem
*
if(pltrgn='nhm')
  lats='30'
  latn='90'
  lonw='-270'
  lone='90'
  dpmprj='nps'
endif

if(pltrgn='usa')
  lats='10'
  latn='90'
  lonw='-180'
  lone='0'
  dpz='0'
  dpmprj='nps'
endif


dpmpst='mres'
dpmpst='lowres'

if(pltvar='slp')
  dp1var='slp'
  dpcint='4.0'
  dpdcint='4.0'
  dp1units='mb'
endif

if(pltvar='phi500')

  dp1var='geophi'
  dp1cint='60.0'
  dpdcint='60.0'
  dp1units='m'

  sl1='5160.0'
  sl2='5400.0'
  sl3='5640.0'
  sl4='5760.0'

  sc0='1'
  sc1='4'
  sc2='1'
  sc3='2'
  sc4='1'

  sl1d='-120.0'
  sl2d='0.0'
  sl3d='0.0'
  sl4d='120.0'

  sc0d='4'
  sc1d='1'
  sc2d='1'
  sc3d='1'
  sc4d='2'

  cl1='2.5'
  cl2='5.0'
  cl3='10.0'
  cl4='20.0'
  cl5='40.0'
  cl6='80.0'
  cl7='80.0'
  cl8='80.0'

  cl1d='-20.0'
  cl2d='-10.0'
  cl3d='-5.0'
  cl4d='-2.5'
  cl5d='2.5'
  cl6d='5.0'
  cl7d='10.0'
  cl8d='20.0'

  t1='Interannual Comparison of SGCM SLP'

endif

if(pltvar='gtmp')
  dp1var=pltvar
  dpcint='5.0'
  dpdcint='1.0'
  dp1units='mb'
endif

if(pltvar='preacc')
  dp1var='preacc'
  dp1units='mm/day'
  dp2var='prcpsk'
endif

dpmod1='Cray 2'
dpmod2='Cray Y-MP'
dpmod3='Difference'



xl1v='0.0'
xr1v='8.5'
yb1v='7.25'
yt1v='10.50'

xl1='1.0'
xr1='8.0'
yb1='0.25'
yt1='3.00'

xl2v=xl1v
xr2v=xr1v
yb2v='4.00'
yt2v='7.25'

xl2=xl1
xr2=xr1
yb2=yb1
yt2=yt1

xl3v=xl1v
xr3v=xr1v
yb3v='0.75'
yt3v='4.00'

xl3=xl1
xr3=xr1
yb3=yb1
yt3=yt1

'set lat 'lats' 'latn 
'set lon 'lonw' 'lone
'set z 'dpz
'set mpdset 'dpmpst
'set mproj 'dpmprj
'set map 0 1 6'
'set grid off'
'set annot 0 6'
if(looper='on')
  'set dbuff on'
else
  'set dbuff off'
endif

cdtgh=dtghbeg

'set background 1'
'set line 0'
'c'

while (1) 
*
*  set the mon/yr/da/hr dtgn
*
  'set time 'dtghcur(cdtgh)
  'q time'
  say 'current time = 'result
  'set cthick 6'
  'set rbcols auto' 
  'set rbrange 5120 5760'
*
*  basic contour/shade plots
* 
   'set grads off'
 
   if(defunc='y') 

    if(tave='y')
      'define fld1=ave('dp1var'.'d1',t-'taveinc',t+'taveinc',-b)'
      'define fld2=ave('dp1var'.'d2',t-'taveinc',t+'taveinc',-b)'
    else
      'define fld1='dp1var'.'d1
      'define fld2='dp1var'.'d2
    endif

    'define fldd='fld1'-'fld2
    
  endif

    var1='fld1'
    var2='fld2'
    vard='fldd'

    
    setpage(xl1v,xr1v,yb1v,yt1v,xl1,xr1,yb1,yt1)

    shdlevs(var1,sl1,sl2,sl3,sl4,sc0,sc1,sc2,sc3,sc4)

    if(pltvar='preacc')
      cntrlevs(var1,cl1,cl2,cl3,cl4,cl5,cl6,cl7,cl8)
    else
      cntrint(var1,dp1cint)
    endif

    plotitle(xl1,xr1,yb1,yt1,dpmod1)
 

    setpage(xl2v,xr2v,yb2v,yt2v,xl2,xr2,yb2,yt2)
    shdlevs(var2,sl1,sl2,sl3,sl4,sc0,sc1,sc2,sc3,sc4)

    if(pltvar='preacc')
      cntrlevs(var2,cl1,cl2,cl3,cl4,cl5,cl6,cl7,cl8)
    else
      cntrint(var2,dp1cint)
    endif

    plotitle(xl2,xr2,yb2,yt2,dpmod2)

*
*       difference 
*

*   'set rbcols 6 8 7 3 5 4 9'
    'set rbcols 5 11 3 14 2'
    'set rbrange -120 120'
   
    setpage(xl3v,xr3v,yb3v,yt3v,xl3,xr3,yb3,yt3)
    shdlevs(vard,sl1d,sl2d,sl3d,sl4d,sc0d,sc1d,sc2d,sc3d,sc4d)

    if(pltvar='preacc')
      cntrlevs(vard,cl1d,cl2d,cl3d,cl4d,cl5d,cl6d,cl7d,cl8d)
    else
      cntrint(vard,dpdcint)
    endif

    plotitle(xl3,xr3,yb3,yt3,dpmod3)

    time=curtime()
    tt=substr(time,1,3)

    if(tave='y') 
      tavelen=2*taveinc+1
      t1=tavelen'-month ave SGCM 'pltvar' about t= 'curtime()
    else
      t1='SGCM 'pltvar' t= 'curtime()
    endif

    t2=dp1units' 'script

    toptitle(xl1,xr1,t1,t2)

  if(looper='on') 
    swap
  else
    pull keycmd
    say 'keycmd = 'keycmd
    if(keycmd='c');'clear';endif
    if(keycmd='p' &  printer='on')
      'print'
      'clear'
    endif

  endif

*
*       increment daily dtg and check if done
*
  cdtgh=incdtgh(cdtgh,dtghinc)

  say 'XXXXXXXXX'cdtgh

  if(cdtgh>dtghend);break;endif

*
endwhile
*
*       ALL DONE
*
say 'all done folk!!!'

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

function toptitle(xl,xr,t1,t2)
  'set vpage off'
  xs=xl+(xr-xl)*0.5
  'set strsiz 0.125'
  'set string 0 c 6'
  'draw string 'xs' 10.85 't1
  'set string 0 c 6'
  'set strsiz 0.10'
  'draw string 'xs' 10.65 't2
  say 'ending toptitle'
return

function plotitle(xl,xr,yb,yt,title)
*
*  plot title
*
  xs=xl+(xr-xl)*0.5
  ys=yt+0.15
  'set strsiz 0.10'
  'set string 0 c 6'
  'draw string 'xs' 'ys' 'title
return

function shdlevs(var,c1,c2,c3,c4,s0,s1,s2,s3,s4)
  'set gxout shaded'
  'set clevs 'c1' 'c2' 'c3' 'c4
  'set ccols 's0' 's1' 's2' 's3' 's4
  'd 'var
return

function cntrint(var,cint)
* 
*  real contour plot
*
  'set ccolor rainbow'
*  'set ccolor revrain'
  'set cthick 6'
  'set cint 'cint
  'set gxout contour'

  'd 'var
return

function cntrlevs(var,c1,c2,c3,c4,c5,c6,c7,c8)
* 
*  real contour plot
*
  'set ccolor rainbow'
  'set cthick 6'
  'set clevs 'c1' 'c2' 'c3' 'c4' 'c5' 'c6' 'c7' 'c8
  'set gxout contour'
  'd 'var
return

function curtime
  say 'in currtime'
  'q time'
  t1=subwrd(result,3)
  hr=substr(t1,1,3)
  da=substr(t1,4,2)
  mo=substr(t1,6,3)
  yr=substr(t1,9,4)
  say 'out currtime'
return (hr' 'da' 'mo' 'yr)

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
return (iyr*1000000+imo*10000+ida*100+ihr)

function dtghcur(dtgh)
*
*  convert dtg to GrADS time
*
  moname='JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC'
  iyr=substr(dtgh,1,2)*1
  imo=substr(dtgh,3,2)*1
  ida=substr(dtgh,5,2)*1
  ihr=substr(dtgh,7,2)*1

  nmo=subwrd(moname,imo)
  imo=i
return (ihr%'Z'ida%nmo%iyr)

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

  ihr=ihr+inc
  say 'ihr = 'ihr
  say 'ida = 'ida

  if(ihr>=24)
    ihr=ihr-24
    ida=ida+1
  endif

  say 'new ihr = 'ihr' new ida = 'ida' imo = 'imo

  if(ida > subwrd(monday,imo))#`?
    ida=ida-subwrd(monday,imo)
    say 'inside check ida = 'ida' monday = 'subwrd(monday,imo)
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

function ndtgh(cdtgh)
  dtghnum=substr(cdtgh,1,2^R
#`{)*100000+substr(cdtgh,3,2)*10000+substr(cdtgh,5,2)*100+substr(cdtgh,7,2)
return(dtghnum)
typhoon:/mf_disk/fiorino/grads/apps/chaos 38 > #`{t( {
