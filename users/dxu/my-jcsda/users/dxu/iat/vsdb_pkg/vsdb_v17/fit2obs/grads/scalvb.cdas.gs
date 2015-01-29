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
ctldir=subwrd(args,12)

say "exp1 is "exp1
say "exp2 is "exp2
say "fcs1 is "fcs1
say "fcs2 is "fcs2
say "gdir is "gdir

**  var: 1=temp; 2=wind; =3 is height; =4 is moisture
vars=1
vare=4
*
* reg: 1=GL; 2=NH; 3=SH; 4=TR; 5=NA; 6=EU; =7 is AS
regs=1
rege=7
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
say "var "var
titvar=gettit(varx)
say "titvar "titvar
xmax=getxmax(varx)
say "xmax "xmax
xmin=getxmin(varx)
say "xmin "xmin
xlintx=getlint(varx)
say "xlintx "xlintx
if(varx=4);leve=300;else;leve=10;endif;levs=1000
*
reg=regs
while (reg<= rege)
titreg=getreg(reg)
greg=getgreg(reg)
*
'set display color white'
'clear'
*
*grfile=pdir'/t'var'.f'fcs1'.'greg'.'gsub'.gr'
*say grfile
*'enable print 'grfile
giffile=pdir'/t'var'.f'fcs1'.'greg'.'gsub'.gif'
say giffile
*
'reset'
*
'set vpage 0 11 0 8.5'
'set strsiz 0.1'
'set string 4 tl 6'
*'draw string 0.12 0.12 'namstr
*
'set x 'reg
'set y 'sub
'set lev 'levs' 'leve
'set time 'ts
'define zero=0.'
*
'set vpage 0 9 0 8.5'
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
'draw xlab     'exp1' solid   'exp2' dotted'
*
ftg1=substr(fcs1,1,2);ftg2=substr(fcs2,1,2)
'draw title f'ftg1'/f'ftg2' 'titreg' 'titvar' Fits to 'titsub' \ 'ts' - 'te
*
*'set vpage 0 11 0 8.5'
*if(fcs1=00.00z)
*'run 'gdir'/linesmpos.gs 'ctldir'/legf00af06_0z 0.06 4 1.66 6.2'
*endif
*if(fcs1=00.12z)
*'run 'gdir'/linesmpos.gs 'ctldir'/legf00af06_12z 0.06 4 1.66 6.2'
*endif
*if(fcs1=12)
*'run 'gdir'/linesmpos.gs 'ctldir'/legf12af36 0.06 4 1.66 6.2'
*endif
*if(fcs1=24)
*'run 'gdir'/linesmpos.gs 'ctldir'/legf24af48 0.06 4 1.66 6.2'
*endif

'set vpage 7 11 0 8.5'
'set grads off'
'set grid off'
'set dfile 1'
'set time 'ts
'set ccolor 1'
'd 'var'cnt.1/1000'
'draw title 'titreg' \ Data Counts'
'draw xlab (in thousands)'
*
'printim 'giffile' gif x700 y650'
'clear'
*
reg=reg+1
endwhile
*
varx=varx+1
endwhile
*
sub=sub+1
endwhile
'quit'
function getreg(reg)
if(reg=1);titreg='Global';endif;
if(reg=2);titreg='NH';endif;
if(reg=3);titreg='SH';endif;
if(reg=4);titreg='TROPICS';endif;
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
if(varx=1);titvar='Temp';endif;
if(varx=2);titvar='Vector Wind';endif;
if(varx=3);titvar='Height';endif;
if(varx=4);titvar='Moisture';endif;
return titvar
function getxmin(varx)
if(varx=1);xmin=-1;endif;
if(varx=2);xmin=-2;endif;
if(varx=3);xmin=-10;endif;
if(varx=4);xmin=-1;endif;
return xmin
function getxmax(varx)
if(varx=1);xmax=3;endif;
if(varx=2);xmax=12;endif;
if(varx=3);xmax=60;endif;
if(varx=4);xmax=3;endif;
return xmax
function getlint(varx)
if(varx=1);xlintx=1;endif;
if(varx=2);xlintx=1;endif;
if(varx=3);xlintx=10;endif;
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
function getgreg(reg)
if(reg=1);greg='gl';endif;
if(reg=2);greg='nh';endif;
if(reg=3);greg='sh';endif;
if(reg=4);greg='tr';endif;
if(reg=5);greg='na';endif;
if(reg=6);greg='eu';endif;
if(reg=7);greg='as';endif;
return greg
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
