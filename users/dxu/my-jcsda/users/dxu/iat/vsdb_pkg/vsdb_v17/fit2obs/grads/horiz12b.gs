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

*** fcs=1 is 12-hr forecast; =2 is 36-hr forecast
fcss=1
fcse=2

***  var=1 is T850; =2 is T700; =3 is T200; =4 is Z500; =5 is W200; =6 is W850; =7 is Q925; =8 is Q850
vars=1
vare=8
*
***  reg=1 is USA; reg=2 is Europe; reg=3 is Asia
regs=1
rege=3
*
vs=1
ve=2
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
gridfile=gdir'/grid1deg.ctl'
say gridfile
*
exp1file=exp1dir'/adpupa12.fcs.ctl'
say exp1file
exp2file=exp2dir'/adpupa12.fcs.ctl'
say exp2file
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
*
say titfcs

'open 'gridfile
'open 'exp1file
'open 'exp2file
*
reg=regs
while (reg<=rege)
*
if(reg=1)
lats=25
latn=55
lonw=235
lone=295
titreg='us'
endif

if(reg=2)
lats=35
latn=65
lonw=-20
lone=40
titreg='eu'
endif

if(reg=3)
lats=5
latn=45
lonw=65
lone=145
titreg='as'
endif
*
varx=vars
while(varx<=vare)
*
if(varx=1)
var=t
vartit='Temp'
levx=850
units='Celsius'
endif
*
if(varx=2)
var=t
vartit='Temp'
levx=700
units='Celsius'
endif
*
if(varx=3)
var=t
vartit='Temp'
levx=200
units='Celsius'
endif
*
if(varx=4)
var=z
vartit='Height'
levx=500
units='meters'
endif
*
if(varx=5)
var=w
vartit='Wind Speed'
levx=850
units='m/sec'
endif
*
if(varx=6)
var=w
vartit='Wind Speed'
levx=200
units='m/sec'
endif
*
if(varx=7)
var=q
vartit='Moisture'
levx=925
units='g/Kg'
endif
*
if(varx=8)
var=q
vartit='Moisture'
levx=850
units='g/Kg'
endif
*
say varx
say var
say vartit
say levx
*
'set vpage 0 8.5 0 11'
*
'set string 0 tl 6'
'set strsiz 0.1'
'draw string 0.12 0.12 'namstr
*
'set string 2 tl 6'
'set strsiz 0.15'
'draw string 1 10.8 'vartit' 'levx' mb 'titfcs' BIAS in 'units
'draw string 2 10.6 from 'ts'-'te
*
'set string 1 tl 6 90'
'set strsiz 0.15'
'draw string 0.2 3 Black Isolines are from actual observations'
'set string 1 tl 6 0'
*
*grfile=pdir'/'var''levx'.f'fcsgr'.'titreg'.bias.gr'
*say grfile
*'enable print 'grfile
giffile=pdir'/'var''levx'.f'fcsgr'.'titreg'.bias.gif'
say giffile
*
vx=vs
while (vx<=ve)
*
if(vx=1)
fl=2
fcstit=exp1
endif
*
if (vx=2)
'set vpage 0 8.5 0 5.4'
fl=3
fcstit=exp2
endif
*
say fcstit
say f
say fl
*
'set lat 'lats' 'latn
'set lon 'lonw' 'lone
'set lev 'levx
'set poli on'
'set mpdset hires'
*
'set gxout stat'
if (var=t)
'd maskout(t'f'.'fl'(time='ts')-to.'fl'(time='ts'),tc.'fl'(time='ts')-'minday')'
endif
if (var=z)
'd maskout(z'f'.'fl'(time='ts')-zo.'fl'(time='ts'),zc.'fl'(time='ts')-'minday')'
endif
if (var=w)
'd maskout(w'f's.'fl'(time='ts'),wc.'fl'(time='ts')-'minday')'
endif
if (var=q)
'd maskout(q'f'.'fl'(time='ts')-qo.2(time='ts'),qc.'fl'(time='ts')-'minday')'
endif
say result
line=sublin(result,7)
count=subwrd(line,8)
line=sublin(result,11)
word=subwrd(line,4)
rmse=digs(word,2)
*
'set vpage 0 8.5 0 11'
'set string 1 tl 6'
'set strsiz 0.15'
if(vx=1)
'draw string 1 10.3 'fcstit'-OBS : Station Count 'count' RMSE of mean 'rmse
'set vpage 0 8.5 5.4 10.8'
endif
if(vx=2)
'draw string 1 5 'fcstit'-OBS : Station Count 'count' RMSE of mean 'rmse
'set vpage 0 8.5 0 5.4'
endif
*
'set gxout shaded'
'set grads off'
'set grid off'
if (var=t)
'set clevs -5 -4 -3 -2.5 -2 -1.5 -1 -.5  0 .5  1 1.5  2 2.5  3  4  5'
'set ccols 49 48 47  46 45   44 43  42 41 61  62 63  64 65 66 67 68 69'
'd oacres(grid.1(t=1),maskout(t'f'.'fl'(time='ts')-to.'fl'(time='ts'),tc.'fl'(time='ts')-'minday'))'
endif
if (var=z)
'set clevs -60 -50 -40 -30 -20 -15 -10 -5  0  5 10 15 20 30 40 50 60'
'set ccols  49  48  47  46  45  44  43 42 41 61 62 63 64 65 66 67 68 69'
'd oacres(grid.1(t=1),maskout(z'f'.'fl'(time='ts')-zo.'fl'(time='ts'),zc.'fl'(time='ts')-'minday'))'
endif
if (var=w)
if (levx=200)
'set clevs -25 -20 -15 -10 -8 -6 -4 -2  0  2  4  6  8 10 15 20 25'
'set ccols  49  48  47  46 45 44 43 42 41 61 62 63 64 65 66 67 68 69'
endif
if (levx=500)
'set clevs -25 -20 -15 -10 -8 -6 -4 -2  0  2  4  6  8 10 15 20 25'
'set ccols  49  48  47  46 45 44 43 42 41 61 62 63 64 65 66 67 68 69'
endif
if (levx=850)
'set clevs -25 -20 -15 -10 -8 -6 -4 -2  0  2  4  6  8 10 15 20 25'
'set ccols  49  48  47  46 45 44 43 42 41 61 62 63 64 65 66 67 68 69'
endif
'd oacres(grid.1(t=1),maskout(w'f's.'fl'(time='ts'),wc.'fl'(time='ts')-'minday'))'
endif
if (var=q)
'set clevs -5 -4 -3 -2 -1.6 -1.2 -.8 -.4  0 .4 .8 1.2 1.6  2  3  4  5'
'set ccols 49 48 47 46   45   44  43  42 41 61 62  63  64 65 66 67 68 69'
'd oacres(grid.1(t=1),maskout(q'f'.'fl'(time='ts')-qo.'fl'(time='ts'),qc.'fl'(time='ts')-'minday'))'
endif
if(vx=1)
'run 'gdir'/cbarnew.gs'
endif
*
'set gxout contour'
'set ccolor 1'
if (var=t)
'set cint 2'
'd oacres(grid.1(t=1),maskout(to.'fl'(time='ts')-273.16,tc.'fl'(time='ts')-'minday'))'
endif
if (var=z)
'set cint 20'
'd oacres(grid.1(t=1),maskout(zo.'fl'(time='ts'),zc.'fl'(time='ts')-'minday'))'
endif
if (var=w)
if(levx=850)
'set cint 2'
endif
if(levx=200)
'set cint 4'
endif
'd oacres(grid.1(t=1),maskout(mag(uo.'fl'(time='ts'),vo.'fl'(time='ts')),wc.'fl'(time='ts')-'minday'))'
endif
if (var=q)
'set cint 1'
'd oacres(grid.1(t=1),maskout(qo.'fl'(time='ts'),qc.'fl'(time='ts')-'minday'))'
endif
*
vx=vx+1
endwhile
*
'printim 'giffile' gif x650 y700'
*'print'
*'disable print'
'clear'
*
varx=varx+1
endwhile
*
reg=reg+1
endwhile
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
