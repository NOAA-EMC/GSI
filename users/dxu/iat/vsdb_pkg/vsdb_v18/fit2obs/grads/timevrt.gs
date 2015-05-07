function alloff(args)
var  = subwrd(args,1)
lev  = subwrd(args,2)
type = subwrd(args,3)
ntim = subwrd(args,4)
dat1 = subwrd(args,5); dat1=substr(dat1,1,8)
dat2 = subwrd(args,6); dat2=substr(dat2,1,8)
nvfy = subwrd(args,7)
nmdl = subwrd(args,8)

'reinit';* 'set font 1'

if(nmdl>=1); mdc.1=subwrd(args,8+1); 'open 'mdc.1'.dieoff.'type'.ctl'; say opened' 'mdc.1 ; endif
if(nmdl>=2); mdc.2=subwrd(args,8+2); 'open 'mdc.2'.dieoff.'type'.ctl'; say opened' 'mdc.2 ; endif
if(nmdl>=3); mdc.3=subwrd(args,8+3); 'open 'mdc.3'.dieoff.'type'.ctl'; say opened' 'mdc.3 ; endif
if(nmdl>=4); mdc.4=subwrd(args,8+4); 'open 'mdc.4'.dieoff.'type'.ctl'; say opened' 'mdc.4 ; endif
if(nmdl>=5); mdc.5=subwrd(args,8+5); 'open 'mdc.5'.dieoff.'type'.ctl'; say opened' 'mdc.5 ; endif
if(nmdl>=6); mdc.6=subwrd(args,8+6); 'open 'mdc.6'.dieoff.'type'.ctl'; say opened' 'mdc.6 ; endif
if(nmdl>=7); mdc.7=subwrd(args,8+7); 'open 'mdc.7'.dieoff.'type'.ctl'; say opened' 'mdc.7 ; endif
**if(nmdl>6); say 'too many inputs' ; 'quit' : endif

if(var=t);vtit="Temp";endif
if(var=q);vtit="SpHu";endif
if(var=w);vtit="Wind";endif
if(var=z);vtit="GeoZ";endif
if(var=p);vtit="Pres";endif

if(type='surf');vtyp='land';else;vtyp=type;endif

*-- define line styles and model names
  cst.1=1; cst.2=1; cst.3=1; cst.4=1; cst.5=1; cst.6=1; cst.7=1; cst.8=1; cst.9=1; cst.10=1
  cth.1=12; cth.2=12; cth.3=9; cth.4=9; cth.5=9; cth.6=9; cth.7=9; cth.8=9; cth.9=9; cth.10=9
  cma.1=0; cma.2=0; cma.3=0; cma.4=0; cma.5=0; cma.6=0; cma.7=0; cma.8=0; cma.9=0; cma.10=0
  cco.1=1; cco.2=4; cco.3=3; cco.4=2; cco.5=14; cco.6=13; cco.7=5; cco.8=6; cco.9=7; cco.10=15
  bar.1=30; bar.2=40; bar.3=50; bar.4=60; bar.5=70; bar.6=80; bar.7=90; bar.8=92; bar.9=94; bar.10=96

*-- loop over stats and regions
  time=1; while(time<12)
  trop=1; while(trop<=2)
  stat=1; while(stat<=2)
  if(stat=1); rms=var'rmse'; stit="RootMeanSquare(O-F)";endif
  if(stat=2); rms=var'bias'; stit="BiasMagnitude(O-F) ";endif
  bincor=var'cnt'
  reg=1; while(reg<=7)

  'clear';'reset'
  'set x 'reg
  'set y 1'      
  'set z 1 16'
  'set t 'time
  if(trop=1);'set zlog off';endif
  if(trop=2);'set zlog  on';endif
* if(type=raob);'set lev 'level(type,lev); endif
* 'set t 1 'nvfy; 'q dims';say 'dims 'result

*--set plot area
  xwd=7.0; ywd=5.0; yy=ywd/17
  xmin=0.8; xmax=xmin+xwd; ymin=5.0; ymax=ymin+ywd
  xt=xmin+0.3; xt1=xt+0.5; xt2=xt1+0.1; yt=ymax;*for legend
  titlx=xmin+0.5*xwd;  titly=ymax+0.35               ;*for tytle
  titlx2=xmin+0.5*xwd;  titly2=ymax+0.10             ;*for tytle
  xlabx=xmin+0.5*xwd;  xlaby=ymin-0.60
* 'set parea 'xmin' 'xmax' 'ymin' 'ymax

*--find maximum and minmum values
   cmax=-10000000.0; cmin=10000000.0
   i=1
   while (i <= nmdl)
    'set gxout stat'
    'd ('rms'.'i')'
    say rms' 'i' ' result
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if( b>1 )
     if(zmax > cmax); cmax=zmax; endif
     if(zmin < cmin); cmin=zmin; endif
    endif
   i=i+1
   endwhile

*  if ( substr(rms,2,4) != rms)   
*    cmin=substr(cmin,1,10)
*  else
*    cmin=0
*    cmin=substr(cmin,1,10)
*  endif
*
*  cint=10*(cmax-cmin)/100
*  if (cint = 0); cint=(cmax-cmin)/10; endif
*  if (cint = 0); cint=0.1*(cmax-cmin); endif
*  if (cint = 0); cint=0.01*(cmax-cmin)*10; endif
*  if (cint = 0); cint=0.001*(cmax-cmin)*100; endif
*  if (cint = 0); cint=0.0001*(cmax-cmin)*1000; endif
*  if (cint = 0); cint=0.00001*(cmax-cmin)*10000; endif
*  say 'cmin cmax cint 'cmin' 'cmax' 'cint

*--display the statistic for each experiment
   i=1
   while (i <= nmdl)
      'set strsiz 0.13 0.13'; yt=yt-yy
      'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
      'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i'  ' a

      'set gxout line'
      'set mproj off'
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
*     'set xlopts 1 6 0';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      if(cmin>0);cmin=-(cmin);endif                   
      if(cmax<0);cmax=-(cmax);endif                   
      'set vrange 'cmin ' 'cmax
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'set ylevs 1000 925 850 700 500 400 300 250 200 150 100 70 50 30 20 10'
      'd ('rms'.'i')'        
      if(i=1&stat=2)
        'set cstyle 2';'set cthick 6';'set cmark 0';'set ccolor 1'      
        'd ('rms'.'i')-('rms'.'i')'
      endif
   i=i+1
   endwhile

*--draw a title which describes this plot
  if(type=acar);reg=5;endif
  'draw title 'vtit' 'rms' 'getregt(reg)' 'gettime(time)' 'dat1'-'dat2


*--print the plot
 if(stat=2);vart=var'b';else;vart=var;endif
 if(trop=1);vart='t'vart;endif
 if(trop=2);vart='s'vart;endif
 leve=level(type,lev); rege=getgreg(reg); typn=type
 ftim=gettime(time)
 grfile=vart'.'rege'.'ftim'.gif'
 'printim 'grfile' x650 y700'
 say stat' 'grfile
 'set vpage off'

 reg=reg+1
 if(type=acar);reg=8;endif
 endwhile
 stat=stat+1
 endwhile
 trop=trop+1
 endwhile
 time=time+1
 endwhile

'quit'

function level(type,lev)
if(type=raob)
if(lev=1) ;return   20; endif;
if(lev=2) ;return   30; endif;
if(lev=3) ;return   50; endif;
if(lev=4) ;return   70; endif;
if(lev=5) ;return  100; endif;
if(lev=6) ;return  150; endif;
if(lev=7) ;return  200; endif;
if(lev=8) ;return  250; endif;
if(lev=9) ;return  300; endif;
if(lev=10);return  400; endif;
if(lev=11);return  500; endif;
if(lev=12);return  700; endif;
if(lev=13);return  850; endif;
if(lev=14);return  925; endif;
if(lev=15);return 1000; endif;
if(lev=16);return   10; endif;
return xxx
endif
if(type=acft|type=acar)
if(lev=1) ;return 1000; endif;
if(lev=2) ;return  700; endif;
if(lev=3) ;return  300; endif;
return xxx
endif
if(type=surf|type=ship); return ''; endif
return xxx  
function getregt(reg)
if(reg=1);titreg='GLOBAL';endif;
if(reg=2);titreg='NH';endif;
if(reg=3);titreg='SH';endif;
if(reg=4);titreg='TROPS';endif;
if(reg=5);titreg='NA';endif;
if(reg=6);titreg='EU';endif;
if(reg=7);titreg='ASIA';endif;
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
function gettime(tim)
if(tim=1);greg='00';endif;
if(tim=2);greg='12';endif;
if(tim=3);greg='24';endif;
if(tim=4);greg='36';endif;
if(tim=5);greg='48';endif;
if(tim=6);greg='60';endif;
if(tim=7);greg='72';endif;
if(tim=8);greg='84';endif;
if(tim=9);greg='96';endif;
if(tim=10);greg='108';endif;
if(tim=11);greg='120';endif;
return greg

