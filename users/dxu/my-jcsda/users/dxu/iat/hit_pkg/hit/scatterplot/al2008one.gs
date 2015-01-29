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
 'set strsiz 0.15 0.15'
 'draw string 4.2 10.4  2008 Atlantic Hurricane Track Errors (nm)'
 'draw string 4.2 10.1  01Jul2008 - 30Nov2008, GFS 00Z cycles'
*---------------------------set dimsnesion, page size and style
nframe=8
nframe2=4
xmin0=1;  xlen=3.0;  xgap=0.5
ymax0=9.8; ylen=-2.0;  ygap=-0.4
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
  titly=ymax-0.20
* say xmin; say xmax; say ymin; say ymax
* 'set vpage 'xmin' 'xmax' 'ymin' 'ymax
  'set parea 'xmin' 'xmax' 'ymin' 'ymax
*
 'run /u/wx24fy/bin/grads/rgbset.gs'                  
 'set xlopts 1 4 0.12'
 'set ylopts 1 4 0.12'
 'set grid on'
 'set mproj scaled'
*'set mproj nps'
*
 
*--find minimum and maximum track errors
*--find case number and  mean track error for each fcst hour and each model      
tmax=0; tmin=800
  'set gxout stat'
  'd an'%iframe
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > tmax); tmax=zmax; endif
    if(zmin < tmin); tmin=zmin; endif
    ln=sublin(result,11); wd=subwrd(ln,2); meana=substr(wd,1,4)
    ln=sublin(result,7); wd=subwrd(ln,8); casea=substr(wd,1,6)
  'set gxout stat'
  'd bn'%iframe
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > tmax); tmax=zmax; endif
    if(zmin < tmin); tmin=zmin; endif
    ln=sublin(result,11); wd=subwrd(ln,2); meanb=substr(wd,1,4)
    ln=sublin(result,7); wd=subwrd(ln,8); caseb=substr(wd,1,6)
tmin=substr(tmin,1,3); tmax=substr(tmax,1,3)
say 'tmin tmax  'tmin' 'tmax
if(tmax > 800); tmax=800; endif
if(tmin < 0  ); tmin=0  ; endif
dist=tmax-tmin
cint=10*substr(dist/50,1,2)
say 'tmin tmax cint 'tmin' 'tmax' 'cint

 'set gxout scatter'
 'set grads off'
 'set vrange 'tmin' 'tmax
 'set xlint 'cint
 'set vrange2 'tmin' 'tmax
 'set ylint 'cint
 'set cthick 5'
 'set digsiz 0.2'

 j=iframe
 'set ccolor 'cco.j
 'set cmark 'cma.j
 'd an'j';bn'j   

* forecast hour, case number, mean track error
 'set strsiz 0.14 0.14'
 'set string 1 bl 5'
 'draw string 'titlx'     'titly' 'fhr.j
 'draw string 'titlx+0.8' 'titly' n='caseb
 'draw string 'titlx'     'titly-0.2' mx='meana
 'draw string 'titlx'     'titly-0.4' my='meanb


 'set line 1 1 6'
 'draw line 'xmin' 'ymin' 'xmax' 'ymax
*
  iframe=iframe+1
endwhile

 'set string 1 tc 6'
 'set strsiz 0.17 0.17'
 'draw string 4.5 0.2 PRU11'
 'set string 1 tc 6 90'
 'set strsiz 0.17 0.17'
 'draw string 0.3 5.0 PRU8'

*'run /u/wx24fy/bin/grads/cbarn.gs  0.9 0 4. 0.4'
 'printim al2008b_PRU11PRU8.gif gif '
 'set vpage off'
'quit'

