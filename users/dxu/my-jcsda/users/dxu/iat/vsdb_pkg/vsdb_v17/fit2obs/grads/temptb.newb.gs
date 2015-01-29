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

'open 'ctldir'/'exp'.f00.raob.ctl'
'open 'ctldir'/'exp'.f06.raob.ctl'
'open 'ctldir'/'exp'.f12.raob.ctl'
'open 'ctldir'/'exp'.f24.raob.ctl'
'open 'ctldir'/'exp'.f36.raob.ctl'
'open 'ctldir'/'exp'.f48.raob.ctl'

* cycle T rms and bias

sub=1; qsub=adp; tsub=ADPUPA; count=tcnt; rmsd='tos+tfs-2*tfo'; bias='tf-to'

varn=1; while (varn<=2)
if(varn=1);var=t ;endif
if(varn=2);var=tb;endif

* cycle regions  reg=1 is gl; reg=2 is nh; reg=3 is sh; reg=4 is tr; tr=5 is na; tr=6 is eu; as=asia

regs=1; rege=7; regx=regs; while (regx<=rege); reg=getreg(regx); titreg=getregt(reg); greg=getgreg(reg)

* cycle levels 

levs=1; leve=15; levx=levs; while (levx<=leve); level=getlev(levx); titlev=level' MB'

if(varn=1); titvar='Temperature'; titlab='RMS Fits to 'tsub          
if(level>=10 ); ymin=0; ymax=4.0; endif
if(level>=200); ymin=0; ymax=4.0; endif
if(level>=500); ymin=0; ymax=4.0; endif
if(level>=700); ymin=0; ymax=4.0; endif
if(level>=850); ymin=0; ymax=4.0; endif
endif

if(varn=2); titvar='Temperature'; titlab='BIAS f-o to 'tsub
if(level>=10 ); ymin=-2; ymax=2; endif
if(level>=200); ymin=-2; ymax=2; endif
if(level>=500); ymin=-2; ymax=2; endif
if(level>=700); ymin=-2; ymax=2; endif
if(level>=850); ymin=-2; ymax=2; endif
endif

* setup the plot parameters

grfile=pdir'/'var''level'.'greg'.'qsub'.gif'
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
'set lev 'level  
'set time 'ts00' 'te00
'define zero=0'

* plot the rms or bias time series 

file=1;while(file<=6)
'set dfile 'file;'set axlim 'ymin' 'ymax
if(file=1);'set cstyle 1';'set ccolor 1';'set cmark 1';'set cthick 4';endif
if(file=2);'set cstyle 1';'set ccolor 2';'set cmark 6';'set cthick 4';endif
if(file=3);'set cstyle 1';'set ccolor 3';'set cmark 3';'set cthick 4';endif
if(file=4);'set cstyle 1';'set ccolor 4';'set cmark 4';'set cthick 4';endif
if(file=5);'set cstyle 1';'set ccolor 5';'set cmark 5';'set cthick 4';endif
if(file=6);'set cstyle 1';'set ccolor 6';'set cmark 2';'set cthick 4';endif
if(varn=1);'d sqrt('rmsd')';endif
if(varn=2);'d 'bias;endif
file=file+1;endwhile

if(varn=2);'set cstyle 1';'set ccolor 1';'set cmark 0';'set cthick 4';'d zero';endif
'draw title 'exp' 'titreg' 'titvar' 'titlev' 'titlab' \ 'ts00' - 'te00   

* compute the mean values of the time series

if(mean=1)
file=1;while(file<=6); 'set dfile 'file;'set t 1'
'define acnt=ave('count',time='ts00',time='te00')'
if(varn=1);'define score=sqrt(ave(('rmsd')*'count',time='ts00',time='te00')/acnt)';endif
if(varn=2);'define score=    (ave(('bias')*'count',time='ts00',time='te00')/acnt)';endif
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

* draw the plot legend 

'set vpage off'            
'run 'gdir'/linesmpos.gs 'gdir'/legexp 0.06 4 1.66 6.2'

* add the data counts for the time series plots

'set vpage 0 11 0 3'
'set grads off'
'set grid off'
'set t 1'
'define mean=ave('count'.1/100,time='ts00',time='te00')'
'set gxout stat'
'd mean'
line=sublin(result,8)
word=subwrd(line,4)
'set gxout bar'
'set barbase 'word
'set baropts outline'
'set time 'ts00' 'te00
'set ccolor 1'
'd 'count'.1/100'
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
if(regx=5);reg=5;endif;
if(regx=6);reg=6;endif;
if(regx=7);reg=7;endif;
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
function getlev(levx)
if(levx=1) ;return   20; endif;
if(levx=2) ;return   30; endif;
if(levx=3) ;return   50; endif;
if(levx=4) ;return   70; endif;
if(levx=5) ;return  100; endif;
if(levx=6) ;return  150; endif;
if(levx=7) ;return  200; endif;
if(levx=8) ;return  250; endif;
if(levx=9) ;return  300; endif;
if(levx=10);return  400; endif;
if(levx=11);return  500; endif;
if(levx=12);return  700; endif;
if(levx=13);return  850; endif;
if(levx=14);return  925; endif;
if(levx=15);return 1000; endif;
return xxx
