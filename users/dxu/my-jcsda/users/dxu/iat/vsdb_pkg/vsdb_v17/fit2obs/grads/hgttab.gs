function windt(args)
ts00=subwrd(args,1)
te00=subwrd(args,2)
ts12=subwrd(args,3)
te12=subwrd(args,4)
pdir=subwrd(args,5)
gdir=subwrd(args,6)
exp=subwrd(args,7)
mean=subwrd(args,8)
ctldir=subwrd(args,9)
namstr=subwrd(args,10)
*
var=z
titvar='Height'
*
regs=1
rege=7

subs=1
sube=1
*
levs=1
leve=4
*
sub=subs
while (sub<=sube)
titsub=getsub(sub)
gsub=getgsub(sub)

levx=levs
while (levx<=leve)
level=getlev(levx)
*
if(level=1000)
ymin=0
ymax=50
endif
if(level=500)
ymin=0
ymax=50
endif
if(level=200)
ymin=0
ymax=60
endif
if(level=30)
ymin=0
ymax=150
endif
*
file=ctldir'/'exp'.f00.raob.ctl'
say file
'open 'file
'open 'ctldir'/'exp'.f06.raob.ctl'
'open 'ctldir'/'exp'.f12.raob.ctl'
'open 'ctldir'/'exp'.f24.raob.ctl'
'open 'ctldir'/'exp'.f36.raob.ctl'
'open 'ctldir'/'exp'.f48.raob.ctl'
*
'reset'
'set display color white'
'clear'
*
*grfile=pdir'/'var''level'.all.'gsub'.gr'
*say grfile
*'enable print 'grfile
giffile=pdir'/'var''level'.all.'gsub'.gif'
say giffile
*
'set vpage 0 8.5 0 11'
'set strsiz 0.1'
'set string 4 tl 6'
'draw string 0.12 0.12 'namstr
*
'set strsiz 0.15'
'set string 2 tl 6'
'draw string 0.1 10.8 'exp' 'titvar' 'level' mb RMS Fit to 'titsub' 'ts00' - 'te00
*
'run 'gdir'/linesmpos.gs 'gdir'/legexp 6 0.5 8 2.2'
*
reg=regs
while (reg<=rege)
titreg=getreg(reg)
*
xminp=getxmin(reg)
xmaxp=getxmax(reg)
yminp=getymin(reg)
ymaxp=getymax(reg)
*
'set vpage 'xminp' 'xmaxp' 'yminp' 'ymaxp
'set grads off'
'set x 'reg
'set y 'sub
'set lev 'level
'set time 'ts00' 'te00
'set xlopts 1 4 0.15'
'set ylopts 1 4 0.15'
'set cstyle 1'
'set ccolor 1'
'set cmark 1'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt('var'fs.1+'var'os.1-2*'var'fo.1)'
'set cstyle 1'
'set ccolor 2'
'set cmark 6'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt('var'fs.2+'var'os.2-2*'var'fo.2)'
'set cstyle 1'
'set ccolor 3'
'set cmark 3'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt('var'fs.3+'var'os.3-2*'var'fo.3)'
'set cstyle 1'
'set ccolor 4'
'set cmark 4'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt('var'fs.4+'var'os.4-2*'var'fo.4)'
'set cstyle 1'
'set ccolor 5'
'set cmark 5'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt('var'fs.5+'var'os.5-2*'var'fo.5)'
'set cstyle 1'
'set ccolor 6'
'set cmark 2'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt('var'fs.6+'var'os.6-2*'var'fo.6)'
'draw title 'exp' 'titreg
*
if(mean=1)
'set dfile 1'
'set t 1'
'define tcnt=ave('var'cnt.1,time='ts00',time='te00')'
'define num1=ave('var'fs.1*'var'cnt.1,time='ts00',time='te00')'
'define num2=ave('var'os.1*'var'cnt.1,time='ts00',time='te00')'
'define num3=ave('var'fo.1*'var'cnt.1,time='ts00',time='te00')'
'define num1=num1/tcnt'
'define num2=num2/tcnt'
'define num3=num3/tcnt'
'define score=sqrt(num1+num2-2*num3)'
'd score'
say result
line=sublin(result,1)
word=subwrd(line,4)
f00=digs(word,2)
'set dfile 2'
'set t 1'
'define tcnt=ave('var'cnt.2,time='ts00',time='te00')'
'define num1=ave('var'fs.2*'var'cnt.2,time='ts00',time='te00')'
'define num2=ave('var'os.2*'var'cnt.2,time='ts00',time='te00')'
'define num3=ave('var'fo.2*'var'cnt.2,time='ts00',time='te00')'
'define num1=num1/tcnt'
'define num2=num2/tcnt'
'define num3=num3/tcnt'
'define score=sqrt(num1+num2-2*num3)'
'd score'
say result
line=sublin(result,1)
word=subwrd(line,4)
f06=digs(word,2)
'set dfile 3'
'set t 1'
'define tcnt=ave('var'cnt.3,time='ts12',time='te12')'
'define num1=ave('var'fs.3*'var'cnt.3,time='ts12',time='te12')'
'define num2=ave('var'os.3*'var'cnt.3,time='ts12',time='te12')'
'define num3=ave('var'fo.3*'var'cnt.3,time='ts12',time='te12')'
'define num1=num1/tcnt'
'define num2=num2/tcnt'
'define num3=num3/tcnt'
'define score=sqrt(num1+num2-2*num3)'
'd score'
say result
line=sublin(result,1)
word=subwrd(line,4)
f12=digs(word,2)
'set dfile 4'
'set t 1'
'define tcnt=ave('var'cnt.4,time='ts00',time='te00')'
'define num1=ave('var'fs.4*'var'cnt.4,time='ts00',time='te00')'
'define num2=ave('var'os.4*'var'cnt.4,time='ts00',time='te00')'
'define num3=ave('var'fo.4*'var'cnt.4,time='ts00',time='te00')'
'define num1=num1/tcnt'
'define num2=num2/tcnt'
'define num3=num3/tcnt'
'define score=sqrt(num1+num2-2*num3)'
'd score'
say result
line=sublin(result,1)
word=subwrd(line,4)
f24=digs(word,2)
'set dfile 5'
'set t 1'
'define tcnt=ave('var'cnt.5,time='ts12',time='te12')'
'define num1=ave('var'fs.5*'var'cnt.5,time='ts12',time='te12')'
'define num2=ave('var'os.5*'var'cnt.5,time='ts12',time='te12')'
'define num3=ave('var'fo.5*'var'cnt.5,time='ts12',time='te12')'
'define num1=num1/tcnt'
'define num2=num2/tcnt'
'define num3=num3/tcnt'
'define score=sqrt(num1+num2-2*num3)'
'd score'
say result
line=sublin(result,1)
word=subwrd(line,4)
f36=digs(word,2)
'set dfile 6'
'set t 1'
'define tcnt=ave('var'cnt.6,time='ts00',time='te00')'
'define num1=ave('var'fs.6*'var'cnt.6,time='ts00',time='te00')'
'define num2=ave('var'os.6*'var'cnt.6,time='ts00',time='te00')'
'define num3=ave('var'fo.6*'var'cnt.6,time='ts00',time='te00')'
'define num1=num1/tcnt'
'define num2=num2/tcnt'
'define num3=num3/tcnt'
'define score=sqrt(num1+num2-2*num3)'
'd score'
say result
line=sublin(result,1)
word=subwrd(line,4)
f48=digs(word,2)
*
'set vpage 0 8.5 0 11'
'set strsiz 0.12'
xpos=xminp+0.3
ypos=ymaxp-0.8
'set string 6 tl 4'
'draw string 'xpos' 'ypos' 'f48
ypos=ymaxp-1.0
'set string 5 tl 4'
'draw string 'xpos' 'ypos' 'f36
ypos=ymaxp-1.2
'set string 4 tl 4'
'draw string 'xpos' 'ypos' 'f24
ypos=ymaxp-1.4
'set string 3 tl 4'
'draw string 'xpos' 'ypos' 'f12
ypos=ymaxp-1.6
'set string 2 tl 4'
'draw string 'xpos' 'ypos' 'f06
ypos=ymaxp-1.8
'set string 1 tl 4'
'draw string 'xpos' 'ypos' 'f00
endif
*
reg=reg+1
endwhile
*
'printim 'giffile' gif x550 y720'
*'print'
*'disable print'
*
levx=levx+1
endwhile
*
sub=sub+1
endwhile
*
'quit'
function getreg(reg)
if(reg=1);titreg='Global';endif;
if(reg=2);titreg='North Hemis';endif;
if(reg=3);titreg='South Hemis';endif;
if(reg=4);titreg='Tropics';endif;
if(reg=5);titreg='North America';endif;
if(reg=6);titreg='Europe';endif;
if(reg=7);titreg='Asia';endif;
return titreg
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
function getlev(levx)
if(levx=1);level=1000;endif;
if(levx=2);level=500;endif;
if(levx=3);level=200;endif;
if(levx=4);level=30;endif;
return level
function getxmin(reg)
if(reg=1);xminp=0.;endif;
if(reg=2);xminp=4.;endif;
if(reg=3);xminp=0.;endif;
if(reg=4);xminp=4.;endif;
if(reg=5);xminp=0.;endif;
if(reg=6);xminp=4.;endif;
if(reg=7);xminp=0.;endif;
return xminp
function getxmax(reg)
if(reg=1);xmaxp=4.5;endif;
if(reg=2);xmaxp=8.5;endif;
if(reg=3);xmaxp=4.5;endif;
if(reg=4);xmaxp=8.5;endif;
if(reg=5);xmaxp=4.5;endif;
if(reg=6);xmaxp=8.5;endif;
if(reg=7);xmaxp=4.5;endif;
return xmaxp
function getymin(reg)
if(reg=1);yminp=8.1;endif;
if(reg=2);yminp=8.1;endif;
if(reg=3);yminp=5.4;endif;
if(reg=4);yminp=5.4;endif;
if(reg=5);yminp=2.7;endif;
if(reg=6);yminp=2.7;endif;
if(reg=7);yminp=0.;endif;
return yminp
function getymax(reg)
if(reg=1);ymaxp=10.8;endif;
if(reg=2);ymaxp=10.8;endif;
if(reg=3);ymaxp=8.1;endif;
if(reg=4);ymaxp=8.1;endif;
if(reg=5);ymaxp=5.4;endif;
if(reg=6);ymaxp=5.4;endif;
if(reg=7);ymaxp=2.7;endif;
return ymaxp
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
