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
**  =1 is 1000 mb; =2 is 700 mb; =3 is 300 mb
levs=1
leve=3
*
levx=levs
while (levx<=leve)
level=getlev(levx)
titlev=getlevt(levx)
*
if(level=1000)
ymin=0
ymax=10
endif
if(level=700)
ymin=0
ymax=12
endif
if(level=300)
ymin=0
ymax=15
endif
*
'open 'ctldir'/'exp'.f00.acar.ctl'
'open 'ctldir'/'exp'.f06.acar.ctl'
'open 'ctldir'/'exp'.f12.acar.ctl'
'open 'ctldir'/'exp'.f24.acar.ctl'
'open 'ctldir'/'exp'.f36.acar.ctl'
'open 'ctldir'/'exp'.f48.acar.ctl'
*
'set display color white'
'clear'
*
*grfile=pdir'/'var''level'.na.acar.gr'
*say grfile
*'enable print 'grfile
giffile=pdir'/'var''level'.na.acar.gif'
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
'set x 1'
'set y 1'
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
'draw title 'exp' N. America 'titvar' 'titlev' mb RMS Fit to ACARS \ 'ts00' - 'te00
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
'set string 1 tl 6'
'draw string 0.2 8.2 'exp
'set string 6 tl 6'
'draw string 0.2 8.0 'f48
'set string 5 tl 6'
'draw string 0.2 7.8 'f36
'set string 4 tl 6'
'draw string 0.2 7.6 'f24
'set string 3 tl 6'
'draw string 0.2 7.4 'f12
'set string 2 tl 6'
'draw string 0.2 7.2 'f06
'set string 1 tl 6'
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
*say 'type in c to continue or quit to exit'
*pull corquit
*corquit
*
levx=levx+1
endwhile
*
'quit'
function getlev(levx)
if(levx=1);level=1000;endif;
if(levx=2);level=700;endif;
if(levx=3);level=300;endif;
return level
function getlevt(levx)
if(levx=1);titlev='1000-700';endif;
if(levx=2);titlev='700-300';endif;
if(levx=3);titlev='300-150';endif;
return titlev
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
