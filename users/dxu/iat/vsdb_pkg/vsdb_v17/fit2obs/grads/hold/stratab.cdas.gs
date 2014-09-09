function vert(args)
ts=subwrd(args,1)
te=subwrd(args,2)
pdir=subwrd(args,3)
gdir=subwrd(args,4)
exp1=subwrd(args,5)
exp2=subwrd(args,6)
fcs1=subwrd(args,7)
fcs2=subwrd(args,8)
idir1=subwrd(args,9)
idir2=subwrd(args,10)
namstr=subwrd(args,11)

say "exp1 is "exp1
say "exp2 is "exp2
say "fcs1 is "fcs1
say "fcs2 is "fcs2

**  var: 1=temp; 2=wind; =3 is height
vars=1
vare=4
*
* reg: 1=GL; 2=NH; 3=SH; 4=TR; 5=NA; 6=EU; =7 is AS
regs=2
rege=5
*
* sub: 1=ADPUPA
subs=1
sube=1
*
file=idir1'/'exp1'.f'fcs1'.raob.ctl'
say file
'open 'file
file=idir1'/'exp1'.f'fcs2'.raob.ctl'
say file
'open 'file
file=idir2'/'exp2'.f'fcs1'.raob.ctl'
say file
'open 'file
file=idir2'/'exp2'.f'fcs2'.raob.ctl'
say file
'open 'file
*
sub=subs
while (sub<= sube)
titsub=getsub(sub)
gsub=getgsub(sub)
*
varx=vars
while (varx<= vare)
var=getvar(varx)
say var
titvar=gettit(varx)
say titvar
xmax=getxmax(varx)
say xmax
xmin=getxmin(varx)
say xmin
xlintx=getlint(varx)
say xlintx
if(varx=4);leve=300;else;leve=10;endif;levs=1000
*
'reset'
'set display color white'
'clear'
*
*grfile=pdir'/s'var'.f'fcs1'.all.'gsub'.gr'
*say grfile
*'enable print 'grfile
giffile=pdir'/s'var'.f'fcs1'.all.'gsub'.gif'
say giffile
*
'set vpage 0 11 0 8.5'
'set strsiz 0.1'
'set string 4 tl 6'
*'draw string 0.12 0.12 'namstr
*
'set strsiz 0.15'
'set string 4 bl 6'
'draw string 6 0.12 'ts' - 'te
*
if(fcs1=00.00z)
'set string 1 bl 6'
'draw string 2 0.12 Anl for 00z'
'set string 2 bl 6'
'draw string 4 0.12 Ges for 00z'
endif
if(fcs1=00.12z)
'set string 1 bl 6'
'draw string 2 0.12 Anl for 12z'
'set string 2 bl 6'
'draw string 4 0.12 Ges for 12z'
endif
if(fcs1=12)
'set string 1 bl 6'
'draw string 2 0.12 12-hr fcst'
'set string 2 bl 6'
'draw string 4 0.12 36-hr fcst'
endif
if(fcs1=24)
'set string 1 bl 6'
'draw string 2 0.12 24-hr fcst'
'set string 2 bl 6'
'draw string 4 0.12 48-hr fcst'
endif
*
'set strsiz 0.25'
'set string 4 tl 6'
'draw string 4.2 8.3 'titvar
*
reg=regs
while (reg<=rege)
say reg
titreg=getreg(reg)
say titreg
*
xminp=getxminp(reg)
xmaxp=getxmaxp(reg)
yminp=getyminp(reg)
ymaxp=getymaxp(reg)
say xminp
say xmaxp
say yminp
say ymaxp
'set parea 'xminp' 'xmaxp' 'yminp' 'ymaxp
*
'set x 'reg
'set y 'sub
'set lev 'levs' 'leve
'set time 'ts
'define zero=0.'
'set zlog on'
*
'set grads off'
'set grid on'
'set xlopts 1 4 0.15'
'set ylopts 1 4 0.15'
'set xlint 'xlintx
*
'set cstyle 1'
'set ccolor 1'
'set cmark 0'
'set cthick 6'
'set axlim 'xmin' 'xmax
'd zero'
*
'set dfile 1'
'set time 'ts
'set cstyle 1'
'set ccolor 1'
'set cmark 0'
'set cthick 4'
'set axlim 'xmin' 'xmax
'd 'var'rmse.1'
'set cstyle 1'
'set ccolor 1'
'set cmark 0'
'set cthick 4'
'set axlim 'xmin' 'xmax
'd 'var'bias.1'
*
'set dfile 2'
'set time 'ts
'set cstyle 1'
'set ccolor 2'
'set cmark 0'
'set cthick 4'
'set axlim 'xmin' 'xmax
'd 'var'rmse.2'
'set cstyle 1'
'set ccolor 2'
'set cmark 0'
'set cthick 4'
'set axlim 'xmin' 'xmax
'd 'var'bias.2'
*
'set dfile 3'
'set time 'ts
'set cstyle 5'
'set ccolor 1'
'set cmark 0'
'set cthick 6'
'set axlim 'xmin' 'xmax
'd 'var'rmse.3'
'set cstyle 5'
'set ccolor 1'
'set cmark 0'
'set cthick 6'
'set axlim 'xmin' 'xmax
'd 'var'bias.3'
*
'set dfile 4'
'set time 'ts
'set cstyle 5'
'set ccolor 2'
'set cmark 0'
'set cthick 6'
'set axlim 'xmin' 'xmax
'd 'var'rmse.4'
'set cstyle 5'
'set ccolor 2'
'set cmark 0'
'set cthick 6'
'set axlim 'xmin' 'xmax
'd 'var'bias.4'
*
if(reg>3)
'draw xlab     'exp1' solid   'exp2' dotted'
endif
*
'draw title 'titreg
*
reg=reg+1
endwhile
*
'printim 'giffile' gif x700 y650'
*'print'
*'disable print'
*say 'type in c to continue or quit to exit'
*pull corquit
*corquit
*
varx=varx+1
endwhile
*
sub=sub+1
endwhile
'quit'
function getreg(reg)
if(reg=1);titreg='Global';endif;
if(reg=2);titreg='North';endif;
if(reg=3);titreg='South';endif;
if(reg=4);titreg='Tropics';endif;
if(reg=5);titreg='North America';endif;
if(reg=6);titreg='Europe';endif;
if(reg=7);titreg='Asia';endif;
return titreg
function getvar(varx)
if(varx=1);var=t;endif;
if(varx=2);var=w;endif;
if(varx=3);var=z;endif;
if(varx=4);var=q;endif;
return var
function gettit(varx)
if(varx=1);titvar='TEMPERATURE';endif;
if(varx=2);titvar='VECTOR WIND';endif;
if(varx=3);titvar='HEIGHT';endif;
if(varx=4);titvar='MOISTURE';endif;
return titvar
function getxmin(varx)
if(varx=1);xmin=-2;endif;
if(varx=2);xmin=-5;endif;
if(varx=3);xmin=-30;endif;
if(varx=4);xmin=-1;endif;
return xmin
function getxmax(varx)
if(varx=1);xmax=5;endif;
if(varx=2);xmax=12;endif;
if(varx=3);xmax=150;endif;
if(varx=4);xmax=3;endif;
return xmax
function getlint(varx)
if(varx=1);xlintx=1;endif;
if(varx=2);xlintx=1;endif;
if(varx=3);xlintx=30;endif;
if(varx=4);xlintx=0.5;endif;
return xlintx
function getsub(sub)
if(sub=1);titsub='RAOBS';endif;
if(sub=2);titsub='ADPSFC';endif;
if(sub=3);titsub='SFCSHP';endif;
if(sub=4);titsub='AIRCFT';endif;
if(sub=5);titsub='AIRCAR';endif;
if(sub=6);titsub='SATWND';endif;
if(sub=7);titsub='SATEMP';endif;
if(sub=8);titsub='PROFLR';endif;
return titsub
function getgsub(sub)
if(sub=1);gsub='adp';endif;
if(sub=2);gsub='sfc';endif;
if(sub=3);gsub='shp';endif;
if(sub=4);gsub='acft';endif;
if(sub=5);gsub='acar';endif;
if(sub=6);gsub='satw';endif;
if(sub=7);gsub='satt';endif;
if(sub=8);gsub='prfl';endif;
return gsub
function getxminp(reg)
if(reg=2);xminp=1;endif;
if(reg=3);xminp=6.;endif;
if(reg=4);xminp=1.;endif;
if(reg=5);xminp=6.;endif;
return xminp
function getxmaxp(reg)
if(reg=2);xmaxp=5.5;endif;
if(reg=3);xmaxp=10.5;endif;
if(reg=4);xmaxp=5.5;endif;
if(reg=5);xmaxp=10.5;endif;
return xmaxp
function getyminp(reg)
if(reg=2);yminp=4.75;endif;
if(reg=3);yminp=4.75;endif;
if(reg=4);yminp=1.;endif;
if(reg=5);yminp=1.;endif;
return yminp
function getymaxp(reg)
if(reg=2);ymaxp=8.;endif;
if(reg=3);ymaxp=8.;endif;
if(reg=4);ymaxp=4.25;endif;
if(reg=5);ymaxp=4.25;endif;
return ymaxp
