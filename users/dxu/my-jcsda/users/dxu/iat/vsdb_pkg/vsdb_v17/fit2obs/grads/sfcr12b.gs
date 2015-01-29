function windt(args)
ts=subwrd(args,1)
te=subwrd(args,2)
pdir=subwrd(args,3)
minday=subwrd(args,4)
exp1dir=subwrd(args,5)
exp2dir=subwrd(args,6)
gdir=subwrd(args,7)
exp1=subwrd(args,8)
exp2=subwrd(args,9)
namstr=subwrd(args,10)
say "minday "minday

value=0

vs=1
ve=2

*** fcs=1 is 12-hr forecast; fcs=2 is 36-hr forecast
fcss=1
fcse=2

us=1
gl=0

if(us=1)
lats=25
latn=55
lonw=235
lone=295
endif

if(gl=1)
lats=-90
latn=90
lonw=0
lone=360
endif

*********************************************************************************
fcsx=fcss
while (fcsx <= fcse)
say fcsx
*
'reinit'
'set display color white'
'clear'
'run 'gdir'/rgbset.gs'
*
if(us=1)
gridfile=gdir'/grid1deg.ctl'
maskfile=gdir'/mask.1deg.ctl'
endif
if(gl=1)
gridfile=gdir'/grid.ctl'
maskfile=gdir'/mask.ieee.ctl'
endif
say gridfile
say maskfile
*
exp1sfc=exp1dir'/adpsfc12.fcs.ctl'
say exp1sfc
exp1shp=exp1dir'/sfcshp12.fcs.ctl'
say exp1shp
exp2sfc=exp2dir'/adpsfc12.fcs.ctl'
say exp2sfc
exp2shp=exp2dir'/sfcshp12.fcs.ctl'
say exp2shp
*
if(fcsx=1)
titfcs='12-HR'
fcsgr='12'
f='f'
endif
if(fcsx=2)
titfcs='36-HR'
fcsgr='36'
f='a'
endif

say titfcs

'open 'gridfile
'open 'maskfile
'open 'exp1sfc
'open 'exp1shp
'open 'exp2sfc
'open 'exp2shp
*
var=p
vartit='Surface Pressure'
units='mb'
say var
say vartit
*
'set vpage 0 8.5 0 11'
*
'set string 4 tl 6'
'set strsiz 0.1'
'draw string 0.12 0.12 'namstr
*
'set string 2 tl 6'
'set strsiz 0.15'
'draw string 1 10.8 'vartit' 'titfcs' RMSE in 'units
'draw string 2 10.6 from 'ts'-'te
*
*grfile=pdir'/'var'.f'fcsgr'.us.rmse.gr'
*say grfile
*'enable print 'grfile
giffile=pdir'/'var'.f'fcsgr'.us.rmse.gif'
say giffile
*
vx=vs
while (vx<=ve)
*
if(vx=1)
fl1=3
fl2=4
fcstit=exp1
endif
*
if (vx=2)
'set vpage 0 8.5 0 5.4'
fl1=5
fl2=6
fcstit=exp2
endif
*
say fcstit
say fl1
say fl2
*
'set lat 'lats' 'latn
'set lon 'lonw' 'lone
if(us=1)
'set poli on'
'set mpdset hires'
endif

'set gxout stat'
'd maskout(p'f'e.'fl1'(time='ts'),pc.'fl1'(time='ts')-'minday')'
say result
line=sublin(result,7)
sfccnt=subwrd(line,8)
line=sublin(result,11)
word=subwrd(line,4)
sfcrms=digs(word,2)
'd maskout(p'f'e.'fl2'(time='ts'),pc.'fl2'(time='ts')-'minday')'
say result
line=sublin(result,7)
shpcnt=subwrd(line,8)
line=sublin(result,11)
word=subwrd(line,4)
shprms=digs(word,2)
*
'set vpage 0 8.5 0 11'
'set string 1 tl 6'
'set strsiz 0.15'
if(vx=1)
'draw string 1 10.4 'fcstit'-OBS : Land Station Count 'sfccnt' RMS 'sfcrms
'draw string 1 10.2 'fcstit'-OBS : Ocean Station Count 'shpcnt' RMS 'shprms
'set vpage 0 8.5 5.4 10.8'
endif
if(vx=2)
'draw string 1 5.2 'fcstit'-OBS : Land Station RMS 'sfcrms
'draw string 1 5.0 'fcstit'-OBS : Ocean Station RMS 'shprms
'set vpage 0 8.5 0 5.4'
endif
*
'set gxout shaded'
'set grads off'
'set grid off'
*
** over land
'set clevs .4 .8 1.2 1.6  2 2.4 2.8 3.2 3.6  4'
'set ccols 71 72  73  74 75  21  22  23  24 25 26'
'd maskout(oacres(grid.1(t=1),maskout(p'f'e.'fl1'(time='ts'),pc.'fl1'(time='ts')-'minday')),mask.2(t=1)-0.9)'
*
** over ocean
'set clevs .4 .8 1.2 1.6  2 2.4 2.8 3.2 3.6  4'
'set ccols 71 72  73  74 75  21  22  23  24 25 26'
'd maskout(oacres(grid.1(t=1),maskout(p'f'e.'fl2'(time='ts'),pc.'fl2'(time='ts')-'minday')),0.9-mask.2(t=1))'
*
if(vx=1)
'run 'gdir'/cbarnew.gs'
endif
*
'set gxout contour'
** over land
'set cthick 4'
'set ccolor 1'
'set cint 1'
'd maskout(oacres(grid.1(t=1),maskout(mpo.'fl1'(time='ts'),mpc.'fl1'(time='ts')-'minday')),mask.2(t=1)-0.9)'
'set vpage 0 8.5 0 11'
'set string 1 tl 6 90'
'set strsiz 0.15'
'draw string 0.2 2 Isolines are Mean Sea Level Pressure from actual observations'
'set string 1 tl 6 0'
*
vx=vx+1
endwhile
*
'printim 'giffile' gif x550 y720'
*'print'
*'disable print'
*
fcsx=fcsx+1
endwhile
'quit'
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

