function windt(args)
ts00=subwrd(args,1)
te00=subwrd(args,2)
ts12=subwrd(args,3)
te12=subwrd(args,4)
pdir=subwrd(args,5)
gdir=subwrd(args,6)
exp1=subwrd(args,7)
exp2=subwrd(args,8)
fcs1=subwrd(args,9)
fcs2=subwrd(args,10)
ctldir=subwrd(args,11)
namstr=subwrd(args,12)
*
var=t
titvar='Temperature'
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
ymax=4
endif
if(level=700)
ymin=0
ymax=4
endif
if(level=300)
ymin=0
ymax=4
endif

ctlfile=ctldir'/'exp1'.f'fcs1'.acar.ctl'
say ctlfile
'open 'ctlfile
ctlfile=ctldir'/'exp1'.f'fcs2'.acar.ctl'
say ctlfile
'open 'ctlfile
ctlfile=ctldir'/'exp2'.f'fcs1'.acar.ctl'
say ctlfile
'open 'ctlfile
ctlfile=ctldir'/'exp2'.f'fcs2'.acar.ctl'
say ctlfile
'open 'ctlfile
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
'set cmark 0'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt('var'fs.1+'var'os.1-2*'var'fo.1)'
'set cstyle 1'
'set ccolor 2'
'set cmark 0'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt('var'fs.2+'var'os.2-2*'var'fo.2)'
'set cstyle 5'
'set ccolor 1'
'set cmark 0'
'set cthick 6'
'set axlim 'ymin' 'ymax
'd sqrt('var'fs.3+'var'os.3-2*'var'fo.3)'
'set cstyle 5'
'set ccolor 2'
'set cmark 0'
'set cthick 6'
'set axlim 'ymin' 'ymax
'd sqrt('var'fs.4+'var'os.4-2*'var'fo.4)'
'draw title N. America 'titvar' 'titlev' mb RMS Fit to ACARS \ 'ts00' - 'te00
*
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
f00a=digs(word,2)
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
f06a=digs(word,2)
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
f00b=digs(word,2)
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
f06b=digs(word,2)
*
'set vpage 0 11 0 8.5'
'set strsiz 0.12'
'set string 1 tl 6'
'draw string 0.2 8.2 'exp1
'set string 2 tl 6'
'draw string 0.2 8.0 'f06a
'set string 1 tl 6'
'draw string 0.2 7.8 'f00a
'set string 1 tl 6'
'draw string 0.8 8.2 'exp2
'set string 2 tl 6'
'draw string 0.8 8.0 'f06b
'set string 1 tl 6'
'draw string 0.8 7.8 'f00b
*
'set vpage 0 11 0 8.5'
'run 'gdir'/linesmpos.gs 'ctldir'/legf'fcs1'af'fcs2' 0.06 4 1.66 6.2'
*
'set vpage 0 11 0 3'
'set grads off'
'set grid off'
'set t 1'
'define mean=ave('var'cnt.1/100,time='ts00',time='te00')'
'set gxout stat'
'd mean'
line=sublin(result,8)
word=subwrd(line,4)
'set gxout bar'
'set barbase 'word
'set baropts outline'
'set time 'ts00' 'te00
'set ccolor 1'
'd 'var'cnt.1/100'
'draw ylab Data Counts \ (in hundreds)'
*
'printim 'giffile' gif x700 y650'
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
