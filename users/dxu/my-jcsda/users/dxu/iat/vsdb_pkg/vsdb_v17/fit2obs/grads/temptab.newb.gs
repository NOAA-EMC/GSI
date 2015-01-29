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

* cycle levels 

levs=1; leve=15; levx=levs; while (levx<=leve); level=getlev(levx); titlev=level' MB'

if(varn=1); titvar='Temperature'; titlab='RMS Fits to 'getsub(sub)
if(level>=10 ); ymin=0; ymax=4.0; endif
if(level>=200); ymin=0; ymax=4.0; endif
if(level>=500); ymin=0; ymax=4.0; endif
if(level>=700); ymin=0; ymax=4.0; endif
if(level>=850); ymin=0; ymax=4.0; endif
endif  

if(varn=2); titvar='Temperature'; titlab='BIAS f-o to 'getsub(sub)
if(level>=10 ); ymin=-2; ymax=2; endif
if(level>=200); ymin=-2; ymax=2; endif
if(level>=500); ymin=-2; ymax=2; endif
if(level>=700); ymin=-2; ymax=2; endif
if(level>=850); ymin=-2; ymax=2; endif
endif  

* setup the plot parameters

'reset';'set display color white';'clear'
grfile=pdir'/'var''level'.all.'qsub'.gif'
say grfile

'set vpage off'               
'set strsiz 0.1'
'set string 0 tl 6'
'draw string 4.75 0.30 'namstr

'set strsiz 0.13'
'set string 1 tl 6'
'draw string 0.1 10.9 'exp' 'titvar' 'titlev' 'titlab' 'ts00' - 'te00
*
'run 'gdir'/linesmpos.gs 'gdir'/legexp 6 0.5 8 2.2'

* cycle regions for all on one page

regs=1; rege=7; reg=regs; while (reg<=rege); titreg=getreg(reg)

xminp=getxmin(reg)
xmaxp=getxmax(reg)
yminp=getymin(reg)
ymaxp=getymax(reg)

'set vpage 'xminp' 'xmaxp' 'yminp' 'ymaxp
'set grads off'
'set x 'reg
'set y 'sub
'set lev 'level
'set time 'ts00' 'te00
'set xlopts 1 4 0.15'
'set ylopts 1 4 0.15'
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
'draw title 'exp' 'titreg

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

reg=reg+1; endwhile; 'printim 'grfile' gif x'xpix' y'ypix 

levx=levx+1; endwhile

varn=varn+1; endwhile

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
