function windt(args)
ts00=subwrd(args,1)
te00=subwrd(args,2)
ts12=subwrd(args,3)
te12=subwrd(args,4)
pdir=subwrd(args,5)
gdir=subwrd(args,6)
exp1=subwrd(args,7)
exp2=subwrd(args,8)
ctldir=subwrd(args,9)
namstr=subwrd(args,10)
xpix=subwrd(args,11)
ypix=subwrd(args,12)
quan=subwrd(args,13)

* open the fit files

mean=1          

'open 'ctldir'/'exp1'.f00.raob.ctl'
'open 'ctldir'/'exp1'.f06.raob.ctl'
'open 'ctldir'/'exp1'.f12.raob.ctl'
'open 'ctldir'/'exp1'.f24.raob.ctl'
'open 'ctldir'/'exp1'.f36.raob.ctl'
'open 'ctldir'/'exp1'.f48.raob.ctl'

'open 'ctldir'/'exp2'.f00.raob.ctl'
'open 'ctldir'/'exp2'.f06.raob.ctl'
'open 'ctldir'/'exp2'.f12.raob.ctl'
'open 'ctldir'/'exp2'.f24.raob.ctl'
'open 'ctldir'/'exp2'.f36.raob.ctl'
'open 'ctldir'/'exp2'.f48.raob.ctl'

qsub=adp; tsub=RAOB; titexp=exp1'-'exp2

* cycle T,Q,Z,W  rms and bias

if(quan=1); var=t; titvar='Temperature'; count=tcnt; rmsd='tos+tfs-2*tfo'; bias='tf-to'; yrmsd=0.5; ybias=1.0; leve=15; endif
if(quan=2); var=q; titvar='Moisture   '; count=qcnt; rmsd='qos+qfs-2*qfo'; bias='qf-qo'; yrmsd=0.5; ybias=1.0; leve=15; endif
if(quan=3); var=z; titvar='Height     '; count=zcnt; rmsd='zos+zfs-2*zfo'; bias='zf-zo'; yrmsd=15.; ybias=15.; leve=15; endif
if(quan=4); var=w; titvar='Vector Wind'; count=wcnt; rmsd='uvo+uvf-2*uv' ; bias='spd'  ; yrmsd=2.0; ybias=15.; leve=15; endif

varn=1; while (varn<=2)
if(varn=1);varx=var   ; titlab='RMSD vs RAOB'; endif
if(varn=2);varx=var'b'; titlab='BIAS vs RAOB'; endif

* cycle levels 
levs=1; levx=levs; while (levx<=leve); level=getlev(levx); titlev=level' MB'

* setup page and add legend and name string

'set display color white'; 'clear';'reset'
grfile=pdir'/'varx''level'.all.'qsub'.gif'
say grfile

'set vpage off'
'set strsiz 0.1'
'set string 0 tl 6'
'draw string 4.75 0.30 'namstr

'set strsiz 0.13'
'set string 1 tl 6'
'draw string 0.1 10.9 'titexp' 'titvar' 'titlev' 'titlab' 'ts00' - 'te00
'run 'gdir'/linesmpos.gs 'gdir'/legexp 6 0.5 8 2.2'

* cycle regions  reg=1 is gl; reg=2 is nh; reg=3 is sh; reg=4 is tr; tr=5 is na; tr=6 is eu; as=asia

regs=1; rege=7; regx=regs; while (regx<=rege); reg=getreg(regx); titreg=getregt(reg); greg=getgreg(reg)

* locate the region plot on the real page

xminp=getxmin(reg)
xmaxp=getxmax(reg)
yminp=getymin(reg)
ymaxp=getymax(reg)
'set vpage 'xminp' 'xmaxp' 'yminp' 'ymaxp
'set grads off'
'set x 'reg
'set y 1'
'set lev 'level  
'set time 'ts12' 'te00

* find the range of the plot

*'set gxout stat'
file=6
if(varn=1)
 'set dfile 'file  ;'define fit1=sqrt('rmsd')'
 'set dfile 'file+6;'define fit2=sqrt('rmsd')'
else
 'set dfile 'file  ;'define fit1=abs('bias')'
 'set dfile 'file+6;'define fit2=abs('bias')'
endif
'd max(abs(fit1-fit2),time='ts12',time='te00')'
say result
line=sublin(result,2)
maxv=subwrd(line,4)*2.0
ymin='-'maxv;ymax=maxv
say ymin ymax
'set gxout line'


* plot the rms or bias time series 

file=1;while(file<=6); 'set axlim 'ymin' 'ymax
if(file=1);'set cstyle 1';'set ccolor 1';'set cmark 1';'set cthick 4';endif
if(file=2);'set cstyle 1';'set ccolor 2';'set cmark 6';'set cthick 4';endif
if(file=3);'set cstyle 1';'set ccolor 3';'set cmark 3';'set cthick 4';endif
if(file=4);'set cstyle 1';'set ccolor 4';'set cmark 4';'set cthick 4';endif
if(file=5);'set cstyle 1';'set ccolor 5';'set cmark 5';'set cthick 4';endif
if(file=6);'set cstyle 1';'set ccolor 6';'set cmark 2';'set cthick 4';endif
if(varn=1)
 'set dfile 'file  ;'define fit1=sqrt('rmsd')'
 'set dfile 'file+6;'define fit2=sqrt('rmsd')'
else
 'set dfile 'file  ;'define fit1=abs('bias')'
 'set dfile 'file+6;'define fit2=abs('bias')'
endif
'd fit1-fit2';file=file+1;endwhile

* draw zero line and the title

'define zero=0';'set cstyle 1';'set ccolor 1';'set cmark 0';'set cthick 4';'d zero'
'draw title 'titreg' 'titvar' 'level'mb '

* compute the mean values of the time series

if(mean=1)
file=1;while(file<=6)
'set time 'ts12' 'te00
if(varn=1)
 'set dfile 'file  ;'define fit1=sqrt('rmsd')'
 'set dfile 'file+6;'define fit2=sqrt('rmsd')'
else
 'set dfile 'file  ;'define fit1=abs('bias')'
 'set dfile 'file+6;'define fit2=abs('bias')'
endif
'set t 1'
'define acnt=ave('count',time='ts00',time='te00')'
'define score=(ave((fit1-fit2)*'count',time='ts00',time='te00')/acnt)'
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
'set strsiz 0.10'
xpos=xminp+0.75
ypos=ymaxp-0.8
'set string 6 tr 4'
'draw string 'xpos' 'ypos' 'f48
ypos=ymaxp-1.0
'set string 5 tr 4'
'draw string 'xpos' 'ypos' 'f36
ypos=ymaxp-1.2
'set string 4 tr 4'
'draw string 'xpos' 'ypos' 'f24
ypos=ymaxp-1.4
'set string 3 tr 4'
'draw string 'xpos' 'ypos' 'f12
ypos=ymaxp-1.6
'set string 2 tr 4'
'draw string 'xpos' 'ypos' 'f06
ypos=ymaxp-1.8
'set string 1 tr 4'
'draw string 'xpos' 'ypos' 'f00

endif

* end of loops over level,region,statistic

regx=regx+1
endwhile

* write the grads metafile
'printim 'grfile' gif x'xpix' y'ypix 

*
levx=levx+1
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
