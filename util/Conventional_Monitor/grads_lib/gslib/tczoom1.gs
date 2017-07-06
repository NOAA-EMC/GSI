script='[/fnoc_grib/tczoom]'
*
*	open files
*
datadir=''
d1=ofile(datadir'r20_92071312.ctl')
say 'd1 = 'd1

prntfile='fnoc.gd'
'set csmooth on'

if (d1=0 | d2=0 | v1=0 | v2=0 | v3=0) 
  say 'Error opening files'
  return
endif
*
*	loop and printing options
*
looper='off'
printer='off'
zoom='y'
if(looper!='on'&printer='on');'enable print 'prntfile;endif

*
*	convert ending dtg to a number for checking
*

pltvar='r20'
lats='-5.0'
latn='40.0'
lone='80.0'
lonw='180.0'

dp1var=pltvar

dpmpst='mres'
dpmpst='lowres'

if(pltvar='r20')

  dp1var='r20'
  dpcint='3.0'
  dpdcint='0.25'
  dp1units='m/sec'

  sl1='0.0'
  sl2='0.0'
  sl3='10.0'
  sl4='15.0'

  sc0='22'
  sc1='0'
  sc2='0'
  sc3='26'
  sc4='35'

  sl1d='-0.5'
  sl2d='0.0'
  sl3d='0.0'
  sl4d='0.5'

  sc0d='22'
  sc1d='0'
  sc2d='0'
  sc3d='0'
  sc4d='35'

  t1='NOGAPS V3.3 92071312'

endif

dpmod1='FNOC 925 mb wind'

xl1v='0.0'
xr1v='11.0'
yb1v='0.00'
yt1v='8.00'

xl1='1.0'
xr1='10.0'
yb1='0.5'
yt1='7.75'

xl2v=xl1v
xr2v=xr1v
yb2v='0.00'
yt2v='4.00'

xl2=xl1
xr2=xr1
yb2=yb1
yt2=yt1

'set lat 'lats' 'latn 
'set lon 'lone' 'lonw
'set z 1'
time='1'
'set t 'time

if(looper='on')
  'set dbuff on'
else
  'set dbuff off'
endif

cdtgm=dtgmbeg

while (1) 
*
*  set the mon/yr dtg
*
*
*  basic contour/shade plots
* 
   'set lat 'lats' 'latn 
   'set lon 'lone' 'lonw
   'set z 1'
   'set t 'time

   'set dfile 'd1

   'define vort=hcurl(r20,r21)*1e5'
   'define vmag=mag(r20,r21)'

    setpage(xl1v,xr1v,yb1v,yt1v,xl1,xr1,yb1,yt1)
 
    'set gxout shaded'
    'set clevs 1 3 5'
    'set ccols 0 4 3 2 '
    'd vort'
    'set gxout stream'
    'd r20;r21;vmag'
    'run cbar.gs'

*    plotgen('vort','1.0',tave,taveinc)
*    shadegen('vort',tave,taveinc,sl1,sl2,sl3,sl4,sc0,sc1,sc2,sc3,sc4)
    
    plotitle(xl1,xr1,yb1,yt1,'streamlines; rel vort shaded')
    t2=dp1units' 'script
    toptitls(xl1,xr1,t1,t2)

  if(zoom='y') 
     ret1=getlola()
     say 'ret1 = 'ret1
     dlatzoom=20.0
     dlonzoom=20.0
     lat0=subwrd(ret1,1)
     lon0=subwrd(ret1,2)
     latnz=lat0+dlatzoom/2
     latsz=lat0-dlatzoom/2
     lonez=lon0-dlonzoom/2
     lonwz=lon0+dlonzoom/2
 
      say lat0' 'lon0
     'set lat 'latsz' 'latnz
     'set lon 'lonez' 'lonwz
     'c'
      setpage(xl1v,xr1v,yb1v,yt1v,xl1,xr1,yb1,yt1)

      'set gxout shaded'
      'set clevs 15 30 45'
      'set ccols 0 4 3 2 '
      'd vmag*1.94'
      'run cbar.gs'
      'set gxout contour'
      'd vmag*1.94'

      'set gxout stream'
      'd r20;r21;vort'

      plotitle(xl1,xr1,yb1,yt1,'TC circ (wind speed in knots)')
      t2=dp1units' 'script
      toptitls(xl1,xr1,t1,t2)


*    plotgen(pltvar,dpcint,tave,taveinc)
*    plotitle(xl1,xr1,yb1,yt1,dpmod1)
   
  endif

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
*  	increment monthly dtg and check if done
*
  time=time+1
  if(time>3);break;endif
*
endwhile
*
*	ALL DONE
*
'set vpage off'
say 'all done folk!!!'

*
*ffffffffffffffffff	function definitions      ffffffffffffffffff
*

function getlola
  'q bpos'
  x=subwrd(result,3)
  y=subwrd(result,4)
  'q gxinfo'
  say result
  xpos=sublin(result,3)
  ypos=sublin(result,4)
  say xpos
  say ypos
  x1=subwrd(xpos,4)
  y1=subwrd(ypos,4)
  x2=subwrd(xpos,6)
  y2=subwrd(ypos,6)
  say x1' 'x2' 'y1' 'y2

  'q dims'
  lonpos=sublin(result,2)
  latpos=sublin(result,3)
  lat1=subwrd(latpos,6)
  lat2=subwrd(latpos,8)
  lon1=subwrd(lonpos,6)
  lon2=subwrd(lonpos,8)
  say latpos
  say lat1' 'lat2
  say lonpos
  say lon1' 'lon2
  dlat=lat2-lat1
  dlon=lon2-lon1
  say dlat' 'dlon
  dy=y2-y1
  dx=x2-x1
  say dy' 'dx
  
  lat=lat1+(dlat/dy)*(y-y1)
  say 'lat = 'lat

  lon=lon1+(dlon/dx)*(x-x1)
  say 'lat = 'lat

  res=lat' 'lon

return(res)

function setpage(xlv,xrv,ybv,ytv,xl,xr,yb,yt)
  setv='set vpage 'xlv' 'xrv' 'ybv' 'ytv
  setv
  setp='set parea 'xl' 'xr' 'yb' 'yt
  setp
  'set grads off'
return

function toptitls(xl,xr,t1,t2)
  'set vpage off'
  xs=xl+(xr-xl)*0.5
  'set strsiz 0.125'
  'set string 1 c 5'
  'draw string 'xs' 8.35 't1
  'set string 1 c 5'
  'set strsiz 0.10'
  'draw string 'xs' 8.15 't2
  say 'ending toptitls'
return

function plotprcp(xl,xr,yb,yt,var,tave,taveinc,title)
  'set parea 'xl' 'xr' 'yb' 'yt
  'set ccolor rainbow'
  'set gxout shaded'
  'set clevs 5.0 10.0 20.0'
  'set ccols 0 9 3 2'

  if(tave='y') 
    'd ave('var',t-'taveinc',t+'taveinc',-b)'
  else
   'd 'var
  endif

  'set ccolor rainbow'
  'set ccolor 1'
  'set gxout contour'
  'set clevs 2.5 5.0 10.0 20.0' 

  if(tave='y') 
    'd ave('var',t-'taveinc',t+'taveinc',-b)'
  else
   'd 'var
  endif

  xs=xl+(xr-xl)*0.5
  ys=yt+0.15
  'set strsiz 0.10'
  'set string 1 c 4'
  'draw string 'xs' 'ys' 'title
return

function plotitle(xl,xr,yb,yt,title)
*
*  plot title
*
  xs=xl+(xr-xl)*0.5
  ys=yt+0.15
  'set strsiz 0.10'
  'set string 1 c 6'
  'draw string 'xs' 'ys' 'title
return

function shadegen(var,tave,taveinc,c1,c2,c3,c4,s0,s1,s2,s3,s4)
  'set gxout shaded'
  'set clevs 'c1' 'c2' 'c3' 'c4
  'set ccols 's0' 's1' 's2' 's3' 's4
  if(tave!='y') 
    'd 'var
  else 
    'd ave('var',t-'taveinc',t+'taveinc',-b)'
  endif
return

function plotgen(var,cint,tave,taveinc)
* 
*  real contour plot
*
  'set ccolor rainbow'
  'set cint 'cint
  'set gxout contour'
  'set cthick 4'

  if(tave!='y') 
    'd 'var
  else 
    'd ave('var',t-'taveinc',t+'taveinc',-b)'
  endif
return

function plotdif(xl,xr,yb,yt,var1,var2,cint,tave,taveinc,title)
  'set parea 'xl' 'xr' 'yb' 'yt
* 
*  trick to get the map on subsequent contour plots after
*  first on a single page
*
  'set gxout shaded'
  'set clevs 0'
  'set ccols 0 0'
  'd 'var1
*
*  real contour plot
*
  'set ccolor rainbow'
  'set cint 'cint
  'set gxout contour'

  if(tave!='y') 
    'd 'var1'-'var2
  else 
    'd ave('var1'-'var2',t-'taveinc',t+'taveinc',-b)'
  endif

*
*  plot title
*
  xs=xl+(xr-xl)*0.5
  ys=yt+0.15
  'set strsiz 0.10'
  'set string 1 c 4'
  'draw string 'xs' 'ys' 'title
return

function curtime
  say 'in currtime'
  'q time'
  t1=subwrd(result,3)
  mo=substr(t1,6,3)
  yr=substr(t1,9,4)
  say 'out currtime'
return (mo' 'yr)

function moinc(bt,moinc)
*
*  increment a month/year dtg by a month
*
  moname='jan feb mar apr may jun jul aug sep oct nov dec'
  curyr=substr(bt,4,2)
  curmo=substr(bt,1,3)
  i=1
  while (curmo!=subwrd(moname,i));i=i+1;endwhile
  i=i+moinc
  if(i>12) 
    curyr=curyr+1
    i=i-12
  endif
  monew=subwrd(moname,i)%curyr
return(monew)

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

function curdtgm
*
*  convert current time to monthly dtg 
*
  moname='JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC'
  'q time'
  t1=subwrd(result,3)
  iyr=substr(t1,11,2)
  nmo=substr(t1,6,3)
  ida=substr(t1,4,2)
  i=1
  while (nmo!=subwrd(moname,i));i=i+1;endwhile
  imo=i
return (iyr*100+imo)

function dtgmcur(dtgm)
*
*  convert dtg to GrADS time
*
  moname='JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC'
  iyr=substr(dtgm,1,2)*1
  imo=substr(dtgm,3,2)*1

  nmo=subwrd(moname,imo)
  imo=i
return (nmo%iyr)

function incdtgm(dtgm,inc)
*
*  increment a monthdtg by inc months 
*  RESTRICTIONS!!  
*  (1)  inc < one year
*
  moname='JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC'

  iyr=substr(dtgm,1,2)*1
  imo=substr(dtgm,3,2)*1

  imo=imo+inc
  say 'new imo = 'imo

  if(imo>=13)
    imo=imo-12
    iyr=iyr+1
  endif

return (iyr*100+imo)

function ndtgm(dtgm)
  dtgnum=substr(dtgm,1,2)*100+substr(dtgm,3,2)*1
return(dtgnum)
