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
xpix=subwrd(args,11)
ypix=subwrd(args,12)

* open the fit files

'open 'ctldir'/'exp'.f00.satw.ctl'
'open 'ctldir'/'exp'.f06.satw.ctl'
'open 'ctldir'/'exp'.f12.satw.ctl'
'open 'ctldir'/'exp'.f24.satw.ctl'
'open 'ctldir'/'exp'.f36.satw.ctl'
'open 'ctldir'/'exp'.f48.satw.ctl'

* cycle rms and bias

rmsd='uvo+uvf-2*uv';bias=spd

varn=1; while (varn<=2)
if(varn=1);var=w ;endif
if(varn=2);var=wb;endif

* cycle regions  reg=1 is gl; reg=2 is nh; reg=3 is sh; reg=4 is tr; tr=5 is na; tr=6 is eu; as=asia

regs=1; rege=5; regx=regs; while (regx<=rege); reg=getreg(regx); titreg=getregt(reg); greg=getgreg(reg)

* cycle levels 

levs=1; leve=3; levx=levs; while (levx<=leve); level=getlev(levx)

if(var=w); titvar='Vector Wind'; titlab='RMS Fits to SATWND'
if(level=low); ymin=0; ymax=10; endif
if(level=mid); ymin=0; ymax=12; endif
if(level=hih); ymin=0; ymax=15; endif
endif  

if(var=wb); titvar='Wind Speed'; titlab='BIAS f-o to SATWND'
if(level=low); ymin=-4; ymax=4; endif
if(level=mid); ymin=-4; ymax=4; endif
if(level=hih); ymin=-8; ymax=8; endif
endif  

if(level=low); titlev="( >=900 )mb"  ; endif
if(level=mid); titlev="(900-400)mb"; endif
if(level=hih); titlev="( <=400 )mb"; ; endif

grfile=pdir'/'var''level'.'greg'.satwnd.gif'
say grfile

'set display color white'
'clear';'reset'

'set vpage off'           
'set strsiz 0.1'
'set string 0 tl 6'
'draw string 0.12 0.15 'namstr

'set vpage 0 11 2 8.5'
'set grads off'
'set x 'reg
'set y 1'
'set z 'levx     
'set time 'ts00' 'te00

* plot the rms or bias time series 

file=1;while(file<=6)
'set dfile 'file;'set axlim 'ymin' 'ymax
if(file=1);'set cstyle 1';'set ccolor 1';'set cmark 1';'set cthick 4';endif
if(file=2);'set cstyle 1';'set ccolor 2';'set cmark 6';'set cthick 4';endif
if(file=3);'set cstyle 1';'set ccolor 3';'set cmark 3';'set cthick 4';endif
if(file=4);'set cstyle 1';'set ccolor 4';'set cmark 4';'set cthick 4';endif
if(file=5);'set cstyle 1';'set ccolor 5';'set cmark 5';'set cthick 4';endif
if(file=6);'set cstyle 1';'set ccolor 6';'set cmark 2';'set cthick 4';endif
if(var=w );'d sqrt('rmsd')';endif
if(var=wb);'d 'bias;endif
file=file+1;endwhile

'draw title 'exp' 'titreg' 'titvar' 'titlev' 'titlab' \ 'ts00' - 'te00   

* compute the mean values of the time series

if(mean=1)
file=1;while(file<=6); 'set dfile 'file;'set t 1'
'define acnt=ave(wcnt,time='ts00',time='te00')'
if(var=w );'define score=sqrt(ave(('rmsd')*wcnt,time='ts00',time='te00')/acnt)';endif
if(var=wb);'define score=    (ave(('bias')*wcnt,time='ts00',time='te00')/acnt)';endif
'd score'; say result
line=sublin(result,1)
word=subwrd(line,4)
if(file=1);f00=digs(word,2);endif
if(file=2);f06=digs(word,2);endif
if(file=3);f12=digs(word,2);endif
if(file=4);f24=digs(word,2);endif
if(file=5);f36=digs(word,2);endif
if(file=6);f48=digs(word,2);endif
file=file+1; endwhile

* display the mean values of the time series

'set vpage off'  
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

* add the data legends

'run 'gdir'/linesmpos.gs 'gdir'/legexp 0.06 4 1.66 6.2'

* add the data counts for the time series plots

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

* write the grads metafile

'printim 'grfile' gif x'xpix' y'ypix 

* end of loops over level,region,statistic

levx=levx+1
endwhile
*
regx=regx+1
endwhile
*
varn=varn+1
endwhile
*
'quit'
function getreg(regx)
if(regx=1);reg=1;endif;
if(regx=2);reg=2;endif;
if(regx=3);reg=3;endif;
if(regx=4);reg=4;endif;
if(regx=5);reg=7;endif;
return reg
function getregt(reg)
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
function getlev(levx)
if(levx=1);level=low;endif;
if(levx=2);level=mid;endif;
if(levx=3);level=hih;endif;
return level
function getlevt(levx)
if(levx=1);titlev='low cld ';endif;
if(levx=2);titlev='mid cld ';endif;
if(levx=3);titlev='high cld';endif;
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
