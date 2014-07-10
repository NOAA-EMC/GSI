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
**if(nmdl>7); say 'too many inputs' ; 'quit' : endif

if(var=t);vtit="Temp";endif
if(var=q);vtit="SpHu";endif
if(var=w);vtit="Wind";endif
if(var=z);vtit="GeoZ";endif
if(var=p);vtit="Pres";endif

if(type='surf');vtyp='land';else;vtyp=type;endif

*-- define line styles and model names
  cst.1=1; cst.2=1; cst.3=1; cst.4=1; cst.5=1; cst.6=1; cst.7=1; cst.8=1; cst.9=1; cst.10=1
  cth.1=12; cth.2=12; cth.3=9; cth.4=9; cth.5=9; cth.6=9; cth.7=9; cth.8=9; cth.9=9; cth.10=9
  cma.1=2; cma.2=0; cma.3=0; cma.4=0; cma.5=0; cma.6=0; cma.7=0; cma.8=0; cma.9=0; cma.10=0
  cco.1=1; cco.2=4; cco.3=3; cco.4=2; cco.5=14; cco.6=13; cco.7=5; cco.8=6; cco.9=7; cco.10=15
  bar.1=30; bar.2=40; bar.3=50; bar.4=60; bar.5=70; bar.6=80; bar.7=90; bar.8=92; bar.9=94; bar.10=96

*-- loop over stats and regions
  stat=1; while(stat<=2)
  if(stat=1); rms=var'rmse'; stit="RootMeanSquare(O-F)";endif
  if(stat=2); rms=var'bias'; stit="BiasMagnitude(O-F) ";endif
  bincor=var'cnt'
  reg=1; while(reg<=7)

  'clear';'reset'
  'set x 'reg
  'set y 1'      
  'set z 'lev 
  if(type=raob);'set lev 'level(type,lev); endif
  'set t 1 'nvfy; 'q dims';say 'dims 'result
  xwd=7.0; ywd=5.0; yy=ywd/17
  xmin=0.8; xmax=xmin+xwd; ymin=5.0; ymax=ymin+ywd
  xt=xmin+0.3; xt1=xt+0.5; xt2=xt1+0.1; yt=ymax;*for legend
  titlx=xmin+0.5*xwd;  titly=ymax+0.35               ;*for tytle
  titlx2=xmin+0.5*xwd;  titly2=ymax+0.10             ;*for tytle
  xlabx=xmin+0.5*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

*--find maximum and minmum values
   cmax=-10000000.0; cmin=10000000.0
   i=1
   while (i <= nmdl)
    'set gxout stat'
    'd abs('rms'.'i')'
    say rms' 'i' ' result
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if( b>1 )
     if(zmax > cmax); cmax=zmax; endif
     if(zmin < cmin); cmin=zmin; endif
    endif
   i=i+1
   endwhile

   if ( substr(rms,2,4) != rms)   
     cmin=substr(cmin,1,10)
   else
     cmin=0
     cmin=substr(cmin,1,10)
   endif

   cint=10*(cmax-cmin)/100
   if (cint = 0); cint=(cmax-cmin)/10; endif
   if (cint = 0); cint=0.1*(cmax-cmin); endif
   if (cint = 0); cint=0.01*(cmax-cmin)*10; endif
   if (cint = 0); cint=0.001*(cmax-cmin)*100; endif
   if (cint = 0); cint=0.0001*(cmax-cmin)*1000; endif
   if (cint = 0); cint=0.00001*(cmax-cmin)*10000; endif
   say 'cmin cmax cint 'cmin' 'cmax' 'cint

*--display the statistic for each experiment
   i=1
   while (i <= nmdl)
    'set gxout stat'      ;* first compute means and count good data numbers
    'd 'bincor'.'i            ;* number of records
    say bincor result
    ln=sublin(result,11); wd=subwrd(ln,2); a=wd;*substr(wd,1,3)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if ( a>0 & b>1 )
      'set strsiz 0.13 0.13'; yt=yt-yy
      'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
      'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i'  ' a

      'set gxout line'
      'set mproj off'
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 'cmin ' 'cmax; 'set ylint 'cint; 'set xlint 48'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'd abs('rms'.'i')'        
     endif
   i=i+1
   endwhile

*--draw a title which describes this plot
* 'set string 1 bc 6'
* 'set strsiz 0.13 0.13'; 'draw string 'titlx' 'titly' 'vtit' 'type' 'level(type,lev)' 'getregt(reg)' 'dat1'-'dat2
* 'set strsiz 0.13 0.13'; 'draw string 'titlx2' 'titly2' 'stit 

  if(type=acar);reg=5;endif
  'draw title 'vtit' 'vtyp' 'level(type,lev)' 'getregt(reg)' 'dat1'-'dat2

*---------------------------------------------------------
*--plot difference between others and the first
*---------------------------------------------------------
  ymin=ymin-4; ymax=ymin+3.5;*4
  'set parea 'xmin' 'xmax' 'ymin' 'ymax
  xlabx=xmin+0.45*xwd;  xlaby=ymin-0.60

*--find maximum and minmum values to determine y-axis labels
 cmax=-999.0; cmin=999.0
 i=2
 while (i <= nmdl)
   'set gxout stat'  ;* find good data records
   'd 'bincor'.'i            
   ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
   if ( b>1 )
     'set gxout stat'
     'd abs('rms'.'i')-abs('rms'.1)'
     say result
     range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
     ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
     if ( b>0 )
      if(zmax > cmax); cmax=zmax; endif
      if(zmin < cmin); cmin=zmin; endif
     endif
   endif
 i=i+1
 endwhile

*---------------------------------------------------------
* compute standard deviation of the difference between the
* first and each of the rest models for each forecast hour
*--plot the 5% conf interval of difference of means : F*SD/sqrt(N-1), F=1.96
*--Null Hypothesis: mean(AC1-AC2)=0, AC1-AC2 follows normal distribution.
*---------------------------------------------------------

  say 'making boxes'

  i=2; while (i <= nmdl)
  'set gxout stat'; 'd 'bincor'.'i ; say 'bincor 'i' 'result
  ln=sublin(result,11); wd=subwrd(ln,2); a=substr(wd,1,3)
  if ( a>0 )
     'define acdm=ave('rms'.'i'-'rms'.1,y=2,y='ntim')'
     'define std=sqrt(ave(('rms'.'i'-'rms'.1-acdm)*('rms'.'i'-'rms'.1-acdm),y=2,y='ntim'))'
     'define intvl=1.96*std/sqrt('bincor'.'i'-1)'

     'set gxout stat'; 'd intvl'
     range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
     ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
     if ( b>0 )
      if(zmax > cmax); cmax=zmax; endif
      if(zmin < cmin); cmin=zmin; endif
      zmin=-zmax
      if(zmin < cmin); cmin=zmin; endif
     endif

  endif
  i=i+1; endwhile

  cint=(cmax-cmin)*0.2

  i=2; while (i <= nmdl)
  'set gxout stat'; 'd 'bincor'.'i             
  ln=sublin(result,11); wd=subwrd(ln,2); a=substr(wd,1,3)
  if ( a>0 )
     'define acdm=ave('rms'.'i'-'rms'.1,y=2,y='ntim')'
     'define std=sqrt(ave(('rms'.'i'-'rms'.1-acdm)*('rms'.'i'-'rms'.1-acdm),y=2,y='ntim'))'
     'define intvl=1.96*std/sqrt('bincor'.'i'-1)'
     'set gxout bar'
     'set bargap 'bar.i
     'set baropts outline'
     'set ccolor 'cco.i
     'set cstyle 1'; 'set cthick 3'; 'set cmark 0'
     'set mproj off'
     'set display color white'; 'set grads off'; 'set grid on'
     'set xlopts 1 6 0.0';     'set ylopts 1 6 0.0'; 'set clopts 1 6 0.0'
     'set vrange 'cmin' 'cmax; 'set ylint 'cint;*** 'set xlint 48'
     'd -intvl;intvl'
  endif
  i=i+1; endwhile

*--plot the difference of all exps minus the first
 say 'plotting diffs'
 i=1
 while (i <= nmdl)
   'set gxout stat'  ;* find good data records
   'd 'bincor'.'i        
   ln=sublin(result,11); wd=subwrd(ln,2); a=substr(wd,1,3)
   if ( a>0 )
     'set gxout line'
     'set mproj off'
     'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
     'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
     'set vrange 'cmin' 'cmax; 'set ylint 'cint
     'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
     if(i=1); 'set cstyle 1'; 'set cthick 1'; 'set cmark 0'; 'set ccolor 1'; endif
     'set xlabs 0|24|48|72|96|120|'
     'd abs('rms'.'i')-abs('rms'.1)'         
   endif
 i=i+1
 endwhile

 'set string 1 bl 6'; say gr2xy
 'q w2xy 00Z01JAN2001 'cmin; xl=subwrd(result,3);'draw string 'xl-0.1' .75 00'
 'q w2xy 00z02jan2001 'cmin; xl=subwrd(result,3);'draw string 'xl-0.1' .75 24'
 'q w2xy 00z03jan2001 'cmin; xl=subwrd(result,3);'draw string 'xl-0.1' .75 48'
 'q w2xy 00z04jan2001 'cmin; xl=subwrd(result,3);'draw string 'xl-0.1' .75 72'
 'q w2xy 00z05jan2001 'cmin; xl=subwrd(result,3);'draw string 'xl-0.1' .75 96'
 'q w2xy 00z06jan2001 'cmin; xl=subwrd(result,3);'draw string 'xl-0.1' .75 120'
 'draw xlab Forecast Length'
  'draw title 'stit

*'set strsiz 0.14 0.14'
*'draw string 'xlabx' 'xlaby' Forecast Hour'
 'set string 1 bl 6'
 'set strsiz 0.14 0.14'
 'draw string 'xmin+0.2' 'ymax-0.2' Difference w.r.t. 'mdc.1
 'set string 1 bl 3'
 'set strsiz 0.11 0.11'
 if( nmdl > 1)
* 'draw string 'xmin+0.1' 'ymin+0.3' rms differences outside of outline bars '
* 'draw string 'xmin+0.1' 'ymin+0.1' are significant at the 95% confidence level'
 endif
*---------------------------------------------------------

 if(stat=2);vart=var'b';else;vart=var;endif
 leve=level(type,lev); rege=getgreg(reg); typn=type
 if(type=raob);typn=adp;endif
 if(type=surf);typn=sfc;endif
 if(type=ship);typn=shp;endif
 grfile=vart''leve'.'rege'.'typn'.gif'
 'printim 'grfile' x650 y700'
 say stat' 'grfile
 'set vpage off'

 reg=reg+1
 if(type=acar);reg=8;endif
 endwhile
 stat=stat+1
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

