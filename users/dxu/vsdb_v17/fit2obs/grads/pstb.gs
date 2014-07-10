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

var=p
titvar='Surface Pressure'
*
**  reg=1 is gl; reg=2 is nh; reg=3 is sh; reg=4 is tr; tr=5 is na; tr=6 is eu; as=asia
regs=1
rege=7
*
**  sub=1 is adpsfc; sub=2 is sfcshp
subs=1
sube=2
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
ymin=0
ymax=5
*
'open 'ctldir'/'exp'.f00.sfc.ctl'
'open 'ctldir'/'exp'.f06.sfc.ctl'
'open 'ctldir'/'exp'.f12.sfc.ctl'
'open 'ctldir'/'exp'.f24.sfc.ctl'
'open 'ctldir'/'exp'.f36.sfc.ctl'
'open 'ctldir'/'exp'.f48.sfc.ctl'
*
'set display color white'
'clear'
*
*grfile=pdir'/'var'.'greg'.'gsub'.gr'
*say grfile
*'enable print 'grfile
giffile=pdir'/'var'.'greg'.'gsub'.gif'
say giffile
*
'reset'
*
'set vpage 0 11 0 8.5'
'set strsiz 0.1'
'set string 4 tl 6'
'draw string 0.12 0.12 'namstr
*
'set x 'reg
'set y 'sub
*
'set vpage 0 11 2 8.5'
'set grads off'
'set x 'reg
'set y 'sub
'set time 'ts00' 'te00
'set cstyle 1'
'set ccolor 1'
'set cmark 1'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt(pfs.1+pos.1-2*pfo.1)'
'set cstyle 1'
'set ccolor 2'
'set cmark 6'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt(pfs.2+pos.2-2*pfo.2)'
'set cstyle 1'
'set ccolor 3'
'set cmark 3'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt(pfs.3+pos.3-2*pfo.3)'
'set cstyle 1'
'set ccolor 4'
'set cmark 4'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt(pfs.4+pos.4-2*pfo.4)'
'set cstyle 1'
'set ccolor 5'
'set cmark 5'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt(pfs.5+pos.5-2*pfo.5)'
'set cstyle 1'
'set ccolor 6'
'set cmark 2'
'set cthick 4'
'set axlim 'ymin' 'ymax
'd sqrt(pfs.6+pos.6-2*pfo.6)'
'draw title 'exp' 'titreg' 'titsub' 'titvar' RMS Fit \ 'ts00' - 'te00
*
if(mean=1)
'set dfile 1'
'set t 1'
'define tcnt=ave(pcnt.1,time='ts00',time='te00')'
'define num1=ave(pfs.1*pcnt.1,time='ts00',time='te00')'
'define num2=ave(pos.1*pcnt.1,time='ts00',time='te00')'
'define num3=ave(pfo.1*pcnt.1,time='ts00',time='te00')'
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
'define tcnt=ave(pcnt.2,time='ts00',time='te00')'
'define num1=ave(pfs.2*pcnt.2,time='ts00',time='te00')'
'define num2=ave(pos.2*pcnt.2,time='ts00',time='te00')'
'define num3=ave(pfo.2*pcnt.2,time='ts00',time='te00')'
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
'define tcnt=ave(pcnt.3,time='ts12',time='te12')'
'define num1=ave(pfs.3*pcnt.3,time='ts12',time='te12')'
'define num2=ave(pos.3*pcnt.3,time='ts12',time='te12')'
'define num3=ave(pfo.3*pcnt.3,time='ts12',time='te12')'
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
'define tcnt=ave(pcnt.4,time='ts00',time='te00')'
'define num1=ave(pfs.4*pcnt.4,time='ts00',time='te00')'
'define num2=ave(pos.4*pcnt.4,time='ts00',time='te00')'
'define num3=ave(pfo.4*pcnt.4,time='ts00',time='te00')'
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
'define tcnt=ave(pcnt.5,time='ts12',time='te12')'
'define num1=ave(pfs.5*pcnt.5,time='ts12',time='te12')'
'define num2=ave(pos.5*pcnt.5,time='ts12',time='te12')'
'define num3=ave(pfo.5*pcnt.5,time='ts12',time='te12')'
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
'define tcnt=ave(pcnt.6,time='ts00',time='te00')'
'define num1=ave(pfs.6*pcnt.6,time='ts00',time='te00')'
'define num2=ave(pos.6*pcnt.6,time='ts00',time='te00')'
'define num3=ave(pfo.6*pcnt.6,time='ts00',time='te00')'
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
'define mean=ave(pcnt.1/1000,time='ts00',time='te00')'
'set gxout stat'
'd mean'
line=sublin(result,8)
word=subwrd(line,4)
'set gxout bar'
'set barbase 'word
'set baropts outline'
'set time 'ts00' 'te00
'set ccolor 1'
'd pcnt.1/1000'
'draw ylab Data Counts \ (in thousands)'
*
'printim 'giffile' gif x720 y550'
*'print'
*'disable print'
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
if(sub=1);titsub='LAND';endif;
if(sub=2);titsub='OCEAN';endif;
return titsub
function getgsub(sub)
if(sub=1);gsub='sfc';endif;
if(sub=2);gsub='shp';endif;
return gsub
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
