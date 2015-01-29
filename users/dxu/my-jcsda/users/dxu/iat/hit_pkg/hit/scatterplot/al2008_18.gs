 'reinit'; 'set font 1'
 'run /u/wx24fy/bin/grads/white.gs'                
 'open  al2008.ctl'           

 'set x 1 '
 'set y 1 '
 'set t 1  92'

* 8 forecast hours for first model 
 'define an1=terr(x=1,y=1)'               
 'define an2=terr(x=2,y=1)'               
 'define an3=terr(x=3,y=1)'               
 'define an4=terr(x=4,y=1)'               
 'define an5=terr(x=5,y=1)'               
 'define an6=terr(x=6,y=1)'               
 'define an7=terr(x=7,y=1)'               
 'define an8=terr(x=8,y=1)'               

* 8 forecast hours for second model 
 'define bn1=terr(x=1,y=2)'               
 'define bn2=terr(x=2,y=2)'               
 'define bn3=terr(x=3,y=2)'               
 'define bn4=terr(x=4,y=2)'               
 'define bn5=terr(x=5,y=2)'               
 'define bn6=terr(x=6,y=2)'               
 'define bn7=terr(x=7,y=2)'               
 'define bn8=terr(x=8,y=2)'               

  cco.1=29; cco.2=25; cco.3=79; cco.4=75; cco.5=39; cco.6=35; cco.7=49; cco.8=45
  cma.1=3; cma.2=3; cma.3=3; cma.4=3; cma.5=3; cma.6=3; cma.7=3; cma.8=3
  fhr.1=00hr; fhr.2=12hr; fhr.3=24hr; fhr.4=36hr
  fhr.5=48hr; fhr.6=72hr; fhr.7=96hr; fhr.8=120hr

*---------------------------string/caption
 'set string 4 tc 6'
 'set strsiz 0.16 0.16'
 'draw string 4.2 10.2  2008 Atlantic Hurricane Track Errors (nm)'
 'draw string 4.2 9.9  01Jul2008 - 30Nov2008, GFS 00Z cycles'
*---------------------------set dimsnesion, page size and style
nframe=1
nframe2=1
xmin0=1;  xlen=7.0;  xgap=0.5
ymax0=9.4; ylen=-7.;  ygap=-0.5
dy1=0.2; dy2=0.1; ymark=ymax0
*
iframe=1
while ( iframe <= nframe )
  icx=1; if (iframe > nframe2); icx=2; endif
  xmin=xmin0+(icx-1)*(xlen+xgap)
  xmax=xmin+xlen
  icy=iframe; if (iframe > nframe2); icy=iframe-nframe2; endif
  ymax=ymax0+(icy-1)*(ylen+ygap)
  ymin=ymax+ylen
  titlx=xmin+0.10
  titly=ymax+0.07
* say xmin; say xmax; say ymin; say ymax
* 'set vpage 'xmin' 'xmax' 'ymin' 'ymax
  'set parea 'xmin' 'xmax' 'ymin' 'ymax
*
 'run /u/wx24fy/bin/grads/rgbset.gs'                  
 'set xlopts 1 4 0.16'
 'set ylopts 1 4 0.16'
 'set clopts 1 4 0.06'
 'set grid on'
 'set mproj scaled'
*'set mproj nps'
*
 
*--find minimum and maximum track errors
*--find case number and  mean track error for each fcst hour and each model      
tmax=0; tmin=800
j=1
while ( j <= 8 )
    'set gxout stat'
    'd an'%j
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > tmax); tmax=zmax; endif
    if(zmin < tmin); tmin=zmin; endif
    ln=sublin(result,11); wd=subwrd(ln,2); meana.j=substr(wd,1,4)
    ln=sublin(result,7); wd=subwrd(ln,8); casea.j=substr(wd,1,6)
   j=j+1
endwhile
j=1
while ( j <= 8 )
    'set gxout stat'
    'd bn'%j
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > tmax); tmax=zmax; endif
    if(zmin < tmin); tmin=zmin; endif
    ln=sublin(result,11); wd=subwrd(ln,2); meanb.j=substr(wd,1,4)
    ln=sublin(result,7); wd=subwrd(ln,8); caseb.j=substr(wd,1,6)
   j=j+1
endwhile
tmin=substr(tmin,1,3); tmax=substr(tmax,1,3)
say 'tmin tmax  'tmin' 'tmax
if(tmax > 800); tmax=800; endif
if(tmin < 0  ); tmin=0  ; endif



j=1
while ( j <= 8 )
 'set gxout scatter'
 'set grads off'
 'set vrange 'tmin' ' tmax
 'set vrange2 'tmin' ' tmax
*'set xlint 50'
*'set ylint 50'
 'set cthick 5'
 'set digsiz 0.2'

 'set ccolor 'cco.j
 'set cmark 'cma.j
 'd an'%j';bn'%j   

 'set strsiz 0.14 0.14'
 ymark=ymark-dy1; 'set line   'cco.j;        'draw mark 'cma.j ' 1.3 'ymark' 0.20'
* forecast hour, case number, mean track error
 ymark=ymark-dy2; 'set string 1 bl 5'; 'draw string 1.5 'ymark' 'fhr.j
 'draw string 2.3 'ymark' mx='meana.j
 'draw string 3.6 'ymark' my='meanb.j
 'draw string 4.9 'ymark' n='caseb.j

  j=j+1
endwhile


 'set line 1 1 6'
 'draw line 'xmin' 'ymin' 'xmax' 'ymax
 'draw xlab PRU11'
 'draw ylab PRU8'
*
  iframe=iframe+1
endwhile

*'run /u/wx24fy/bin/grads/cbarn.gs  0.9 0 4. 0.4'
 'printim al2008a_PRU11PRU8.gif gif '
 'set vpage off'
'quit'

