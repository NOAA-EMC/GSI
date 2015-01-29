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

var=w
titvar='Vector Wind'
*
**  reg=1 is gl; reg=2 is nh; reg=3 is sh; reg=4 is tr; tr=5 is na; tr=6 is eu; as=asia
regs=1
rege=7
*
**  sub=1 is adpupa
subs=1
sube=1
*
**  =1 is 850 mb; =2 is 700 mb ; =3 is 500 mb; =4 is 200 mb; =5 is 30 mb
levs=1
leve=5
*
reg=regs
while (reg<=rege)
titreg=getreg(reg)
greg=getgreg(reg)
*
sub=subs
while (sub<=sube)
titsub=getsub(sub)
gsub=getgsub(sub)
*
levx=levs
while (levx<=leve)
level=getlev(levx)
*
if(level=850)
ymin=0
ymax=10
endif
if(level=700)
ymin=0
ymax=10
endif
if(level=500)
ymin=0
ymax=12
endif
if(level=200)
ymin=0
ymax=15
endif
if(level=30)
ymin=0
ymax=12
endif
*
ctlfile=ctldir'/'exp'.f00.raob.ctl'
say "ctlfile= "ctlfile
'open 'ctlfile
'open 'ctldir'/'exp'.f06.raob.ctl'
'open 'ctldir'/'exp'.f12.raob.ctl'
'open 'ctldir'/'exp'.f24.raob.ctl'
'open 'ctldir'/'exp'.f36.raob.ctl'
'open 'ctldir'/'exp'.f48.raob.ctl'
*
'set display color white'
'clear'
*
*grfile=pdir'/'var''level'.'greg'.'gsub'.gr'
*say grfile
*'enable print 'grfile
giffile=pdir'/'var''level'.'greg'.'gsub'.gif'
say giffile
*
'reset'
*
'set vpage 0 11 0 8.5'
'set strsiz 0.1'
'set string 4 tl 6'
'draw string 0.12 0.12 'namstr
*
'set vpage 0 11 2 8.5'
'set grads off'
'set x 'reg
'set y 'sub
'set lev 'level
'set time 'ts00' 'te00
'set cstyle 1'
'set ccolor 1'
'set cmark 1'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt(uvf.1+uvo.1-2*uv.1)'
'set cstyle 1'
'set ccolor 2'
'set cmark 6'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt(uvf.2+uvo.2-2*uv.2)'
'set cstyle 1'
'set ccolor 3'
'set cmark 3'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt(uvf.3+uvo.3-2*uv.3)'
'set cstyle 1'
'set ccolor 4'
'set cmark 4'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt(uvf.4+uvo.4-2*uv.4)'
'set cstyle 1'
'set ccolor 5'
'set cmark 5'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt(uvf.5+uvo.5-2*uv.5)'
'set cstyle 1'
'set ccolor 6'
'set cmark 2'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt(uvf.6+uvo.6-2*uv.6)'
'draw title 'exp' 'titreg' 'titvar' 'level' mb RMS Fit to 'titsub' \ 'ts00' - 'te00
*
if(mean=1)
'set dfile 1'
'set t 1'
'define tcnt=ave(wcnt.1,time='ts00',time='te00')'
'define num1=ave(uvf.1*wcnt.1,time='ts00',time='te00')'
'define num2=ave(uvo.1*wcnt.1,time='ts00',time='te00')'
'define num3=ave(uv.1*wcnt.1,time='ts00',time='te00')'
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
'define tcnt=ave(wcnt.2,time='ts00',time='te00')'
'define num1=ave(uvf.2*wcnt.2,time='ts00',time='te00')'
'define num2=ave(uvo.2*wcnt.2,time='ts00',time='te00')'
'define num3=ave(uv.2*wcnt.2,time='ts00',time='te00')'
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
'define tcnt=ave(wcnt.3,time='ts12',time='te12')'
'define num1=ave(uvf.3*wcnt.3,time='ts12',time='te12')'
'define num2=ave(uvo.3*wcnt.3,time='ts12',time='te12')'
'define num3=ave(uv.3*wcnt.3,time='ts12',time='te12')'
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
'define tcnt=ave(wcnt.4,time='ts00',time='te00')'
'define num1=ave(uvf.4*wcnt.4,time='ts00',time='te00')'
'define num2=ave(uvo.4*wcnt.4,time='ts00',time='te00')'
'define num3=ave(uv.4*wcnt.4,time='ts00',time='te00')'
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
'define tcnt=ave(wcnt.5,time='ts12',time='te12')'
'define num1=ave(uvf.5*wcnt.5,time='ts12',time='te12')'
'define num2=ave(uvo.5*wcnt.5,time='ts12',time='te12')'
'define num3=ave(uv.5*wcnt.5,time='ts12',time='te12')'
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
'define tcnt=ave(wcnt.6,time='ts00',time='te00')'
'define num1=ave(uvf.6*wcnt.6,time='ts00',time='te00')'
'define num2=ave(uvo.6*wcnt.6,time='ts00',time='te00')'
'define num3=ave(uv.6*wcnt.6,time='ts00',time='te00')'
'define num1=num1/tcnt'
'define num2=num2/tcnt'
'define num3=num3/tcnt'
'define score=sqrt(num1+num2-2*num3)'
'd score'
say result
line=sublin(result,1)
word=subwrd(line,4)
f48=digs(word,2)
'set vpage 0 11 0 8.5'
'set strsiz 0.12'
'set string 1 tl 4'
'draw string 0.2 8.2 'exp
'set string 6 tl 4'
'draw string 0.2 8.0 'f48
'set string 5 tl 4'
'draw string 0.2 7.8 'f36
'set string 4 tl 4'
'draw string 0.2 7.6 'f24
'set string 3 tl 4'
'draw string 0.2 7.4 'f12
'set string 2 tl 4'
'draw string 0.2 7.2 'f06
'set string 1 tl 4'
'draw string 0.2 7.0 'f00
endif
*
'set vpage 0 11 0 8.5'
'run 'gdir'/linesmpos.gs 'gdir'/legexp 0.06 4 1.66 6.2'
*
'set vpage 0 11 0 3'
'set grads off'
'set grid off'
'set t 1'
'define mean=ave(wcnt.1/100,time='ts00',time='te00')'
'set gxout stat'
'd mean'
line=sublin(result,8)
word=subwrd(line,4)
'set gxout bar'
'set barbase 'word
'set baropts outline'
'set time 'ts00' 'te00
'set ccolor 1'
'd wcnt.1/100'
'draw ylab Data Counts \ (in hundreds)'
*
'printim 'giffile' gif x720 y550'
*'print'
*'disable print'
*
levx=levx+1
endwhile
*
sub=sub+1
endwhile
*
reg=reg+1
endwhile
*
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
function getgreg(reg)
if(reg=1);greg='gl';endif;
if(reg=2);greg='nh';endif;
if(reg=3);greg='sh';endif;
if(reg=4);greg='tr';endif;
if(reg=5);greg='na';endif;
if(reg=6);greg='eu';endif;
if(reg=7);greg='as';endif;
return greg
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
if(levx=1);level=850;endif;
if(levx=2);level=700;endif;
if(levx=3);level=500;endif;
if(levx=4);level=200;endif;
if(levx=5);level=30;endif;
return level
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
