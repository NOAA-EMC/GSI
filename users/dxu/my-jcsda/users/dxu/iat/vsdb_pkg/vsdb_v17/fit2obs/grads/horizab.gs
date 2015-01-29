function windt(args)
ts=subwrd(args,1)
te=subwrd(args,2)
ts12=subwrd(args,3)
pdir=subwrd(args,4)
minday=subwrd(args,5)
regs=subwrd(args,6)
rege=subwrd(args,7)
expdir=subwrd(args,8)
gdir=subwrd(args,9)
exp=subwrd(args,10)
namstr=subwrd(args,11)

***  var=1 is T850; =2 is T700; =3 is T200; =4 is Z500; =5 is W200; =6 is W850; =7 is Q925; =8 is Q850
vars=1
vare=8
*
*********************************************************************************
'set display color white'
'clear'
'run 'gdir'/rgbset.gs'
*
gridfile=gdir'/grid1deg.ctl'
say gridfile
'open 'gridfile
anl=expdir'/adpupa00.anl.ctl'
say anl
'open 'anl
fcs00=expdir'/adpupa00.fcs.ctl'
say fcs00
'open 'fcs00
fcs12=expdir'/adpupa12.fcs.ctl'
say fcs12
'open 'fcs12
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
'draw string 1 10.8 'vartit' 'levx' mb BIAS in 'units
'draw string 2 10.6 from 'ts'-'te
*
*grfile=pdir'/'var''levx'.all.'titreg'.bias.gr'
*say grfile
*'enable print 'grfile
giffile=pdir'/'var''levx'.all.'titreg'.bias.gif'
say giffile
*
fcsx=1
while (fcsx <= 6)
say fcsx
*
if(fcsx=1)
titfcs='ANALYSIS'
f='a'
fl=2
tsx=ts
endif
if(fcsx=2)
titfcs='6-hr GUESS'
f='f'
fl=2
tsx=ts
endif
if(fcsx=3)
titfcs='12-HR FORECAST'
f='f'
fl=4
tsx=ts12
endif
if(fcsx=4)
titfcs='24-HR FORECAST'
f='f'
fl=3
tsx=ts
endif
if(fcsx=5)
titfcs='36-HR FORECAST'
f='a'
fl=4
tsx=ts12
endif
if(fcsx=6)
titfcs='48-HR FORECAST'
f='a'
fl=3
tsx=ts
endif
*
fcstit=exp
*
say titfcs
say f
say fcstit
say fl
*
x1=getx1(fcsx)
x2=getx2(fcsx)
y1=gety1(fcsx)
y2=gety2(fcsx)
say x1
say x2
say y1
say y2
*
'set vpage 'x1' 'x2' 'y1' 'y2
*
'set lat 'lats' 'latn
'set lon 'lonw' 'lone
'set xlopts 1 4 0.15'
'set ylopts 1 4 0.15'
'set clopts 1 4 0.12'
'set lev 'levx
'set poli on'
'set mpdset hires'
*
'set gxout stat'
if (var=t)
'd maskout(t'f'.'fl'(time='tsx')-to.'fl'(time='tsx'),tc.'fl'(time='tsx')-'minday')'
endif
if (var=z)
'd maskout(z'f'.'fl'(time='tsx')-zo.'fl'(time='tsx'),zc.'fl'(time='tsx')-'minday')'
endif
if (var=w)
'd maskout(w'f's.'fl'(time='tsx'),wc.'fl'(time='tsx')-'minday')'
endif
if (var=q)
'd maskout(q'f'.'fl'(time='tsx')-qo.'fl'(time='tsx'),qc.'fl'(time='tsx')-'minday')'
endif
say result
line=sublin(result,7)
count=subwrd(line,8)
line=sublin(result,11)
word=subwrd(line,4)
rmse=digs(word,2)
*
'set gxout shaded'
'set grads off'
'set grid off'
if (var=t)
'set clevs -5 -4 -3 -2.5 -2 -1.5 -1 -.5  0 .5  1 1.5  2 2.5  3  4  5'
'set ccols 49 48 47  46 45   44 43  42 41 61  62 63  64 65 66 67 68 69'
'd oacres(grid.1(t=1),maskout(t'f'.'fl'(time='tsx')-to.'fl'(time='tsx'),tc.'fl'(time='tsx')-'minday'))'
endif
if (var=z)
'set clevs -60 -50 -40 -30 -20 -15 -10 -5  0  5 10 15 20 30 40 50 60'
'set ccols  49  48  47  46  45  44  43 42 41 61 62 63 64 65 66 67 68 69'
'd oacres(grid.1(t=1),maskout(z'f'.'fl'(time='tsx')-zo.'fl'(time='tsx'),zc.'fl'(time='tsx')-'minday'))'
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
'd oacres(grid.1(t=1),maskout(w'f's.'fl'(time='tsx'),wc.'fl'(time='tsx')-'minday'))'
endif
if (var=q)
'set clevs -5 -4 -3 -2 -1.6 -1.2 -.8 -.4  0 .4 .8 1.2 1.6  2  3  4  5'
'set ccols 49 48 47 46   45   44  43  42 41 61 62  63  64 65 66 67 68 69'
'd oacres(grid.1(t=1),maskout(q'f'.'fl'(time='tsx')-qo.'fl'(time='tsx'),qc.'fl'(time='tsx')-'minday'))'
endif
'run 'gdir'/cbarnew.gs'
'draw title 'fcstit' 'titfcs' RMSE OF TIME MEAN 'rmse
*
if(fcsx=1)
'set gxout contour'
'set ccolor 1'
if (var=t)
'set cint 2'
'd oacres(grid.1(t=1),maskout(to.'fl'(time='tsx')-273.16,tc.'fl'(time='tsx')-'minday'))'
endif
if (var=z)
'set cint 20'
'd oacres(grid.1(t=1),maskout(zo.'fl'(time='tsx'),zc.'fl'(time='tsx')-'minday'))'
endif
if (var=w)
if(levx=850)
'set cint 2'
endif
if(levx=200)
'set cint 4'
endif
'd oacres(grid.1(t=1),maskout(mag(uo.'fl'(time='tsx'),vo.'fl'(time='tsx')),wc.'fl'(time='tsx')-'minday'))'
endif
if (var=q)
'set cint 1'
'd oacres(grid.1(t=1),maskout(qo.'fl'(time='tsx'),qc.'fl'(time='tsx')-'minday'))'
endif
endif
*
fcsx=fcsx+1
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
function getx1(fcsx)
if(fcsx=1);x1=0.;endif;
if(fcsx=2);x1=0.;endif;
if(fcsx=3);x1=0.;endif;
if(fcsx=4);x1=4.25;endif;
if(fcsx=5);x1=4.25;endif;
if(fcsx=6);x1=4.25;endif;
return x1
function getx2(fcsx)
if(fcsx=1);x2=4.25;endif;
if(fcsx=2);x2=4.25;endif;
if(fcsx=3);x2=4.25;endif;
if(fcsx=4);x2=8.5;endif;
if(fcsx=5);x2=8.5;endif;
if(fcsx=6);x2=8.5;endif;
return x2
function gety1(fcsx)
if(fcsx=1);y1=7.2;endif;
if(fcsx=2);y1=3.6;endif;
if(fcsx=3);y1=0.;endif;
if(fcsx=4);y1=7.2;endif;
if(fcsx=5);y1=3.6;endif;
if(fcsx=6);y1=0.;endif;
return y1
function gety2(fcsx)
if(fcsx=1);y2=10.8;endif;
if(fcsx=2);y2=7.2;endif;
if(fcsx=3);y2=3.6;endif;
if(fcsx=4);y2=10.8;endif;
if(fcsx=5);y2=7.2;endif;
if(fcsx=6);y2=3.6;endif;
return y2
