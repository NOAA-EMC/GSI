function windt(args)
ts=subwrd(args,1)
te=subwrd(args,2)
ts12=subwrd(args,3)
pdir=subwrd(args,4)
minday=subwrd(args,5)
expdir=subwrd(args,6)
gdir=subwrd(args,7)
exp=subwrd(args,8)
namstr=subwrd(args,9)

var=p
vartit='Surface Pressure'
units='mb'
*********************************************************************************
'reinit'
'set display color white'
'clear'
'run 'gdir'/rgbset.gs'
*
lats=25
latn=55
lonw=235
lone=295
titreg='us'
*
gridfile=gdir'/grid1deg.ctl'
maskfile=gdir'/mask.1deg.ctl'
say gridfile
'open 'gridfile
say maskfile
'open 'maskfile
*
asfc=expdir'/adpsfc00.anl.ctl'
say asfc
'open 'asfc
ashp=expdir'/sfcshp00.anl.ctl'
say ashp
'open 'ashp
f00sfc=expdir'/adpsfc00.fcs.ctl'
say f00sfc
'open 'f00sfc
f00shp=expdir'/sfcshp00.fcs.ctl'
say f00shp
'open 'f00shp
f12sfc=expdir'/adpsfc12.fcs.ctl'
say f12sfc
'open 'f12sfc
f12shp=expdir'/sfcshp12.fcs.ctl'
say f12shp
'open 'f12shp
*
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
'draw string 1 10.8 'vartit' BIAS in 'units
'draw string 2 10.6 from 'ts'-'te
*
*grfile=pdir'/'var'.all.'titreg'.bias.gr'
*say grfile
*'enable print 'grfile
giffile=pdir'/'var'.all.'titreg'.bias.gif'
say giffile
*
fcsx=1
while (fcsx <= 6)
say fcsx
*
if(fcsx=1)
titfcs='ANALYSIS'
f='a'
fl1=3
fl2=4
tsx=ts
endif
if(fcsx=2)
titfcs='6-hr GUESS'
f='f'
fl1=3
fl2=4
tsx=ts
endif
if(fcsx=3)
titfcs='12-HR FCST'
f='f'
fl1=7
fl2=8
tsx=ts12
endif
if(fcsx=4)
titfcs='24-HR FCST'
f='f'
fl1=5
fl2=6
tsx=ts
endif
if(fcsx=5)
titfcs='36-HR FCST'
f='a'
fl1=7
fl2=8
tsx=ts12
endif
if(fcsx=6)
titfcs='48-HR FCST'
f='a'
fl1=5
fl2=6
tsx=ts
endif
*
fcstit=exp
*
say titfcs
say f
say fcstit
say fl1
say fl2
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
'set poli on'
'set mpdset hires'
*
'set gxout stat'
'd maskout(p'f'.'fl1'(time='tsx')-po.'fl1'(time='tsx'),pc.'fl1'(time='tsx')-'minday')'
say result
line=sublin(result,7)
sfccnt=subwrd(line,8)
line=sublin(result,11)
word=subwrd(line,4)
sfcrms=digs(word,2)
'd maskout(p'f'.'fl2'(time='tsx')-po.'fl2'(time='tsx'),pc.'fl2'(time='tsx')-'minday')'
say result
line=sublin(result,7)
shpcnt=subwrd(line,8)
line=sublin(result,11)
word=subwrd(line,4)
shprms=digs(word,2)
*
'set gxout shaded'
'set grads off'
'set grid off'
*
** over land
'set clevs -4 -3 -2.5 -2 -1.6 -1.2 -.8 -.4  0 .4 .8 1.2 1.6  2 2.5  3  4'
'set ccols 49 48   47 46   45   44  43  42 41 61 62  63  64 65  66 67 68 69'
'd maskout(oacres(grid.1(t=1),maskout(p'f'.'fl1'(time='tsx')-po.'fl1'(time='tsx'),pc.'fl1'(time='tsx')-'minday')),mask.2(t=1)-0.9)'
*
** over ocean
'set clevs -4 -3 -2.5 -2 -1.6 -1.2 -.8 -.4  0 .4 .8 1.2 1.6  2 2.5  3  4'
'set ccols 49 48   47 46   45   44  43  42 41 61 62  63  64 65  66 67 68 69'
'd maskout(oacres(grid.1(t=1),maskout(p'f'.'fl2'(time='tsx')-po.'fl2'(time='tsx'),pc.'fl2'(time='tsx')-'minday')),0.9-mask.2(t=1))'
*
'run 'gdir'/cbarnew.gs'
'draw title 'fcstit' 'titfcs' LAND : 'sfcrms' OCEAN 'shprms 
*
if(fcsx=1)
'set gxout contour'
** over land
'set cthick 4'
'set ccolor 1'
'set cint 1'
'd maskout(oacres(grid.1(t=1),maskout(mpo.'fl1'(time='tsx'),mpc.'fl1'(time='tsx')-'minday')),mask.2(t=1)-0.9)'
endif
*
fcsx=fcsx+1
endwhile
*
'printim 'giffile' gif x550 y720'
*'print'
*'disable print'
'quit'
*
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

