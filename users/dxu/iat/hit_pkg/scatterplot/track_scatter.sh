#!/bin/sh
set -x

## hur_track.sh
## read track errors from tracks_al.t.txt which is manually modified from tracks_al.t.out 
## which is produced by hur_track.sh).  write out in binary format for GrADS

export trackcase=al2008
export DATST=01Jun2008
export DATND=30Nov2008
export cyc="GFS 00Z"
export basinyear="2008 Atlantic"

set -A model  nan PRU8 PRD09Q1O AVNO OFCL CLP5
export NMD=5         ;# number of total models
MD1=1; MD2=2         ;# models to be compared, eg. MD1=1-->PRU8 

export NHR=8         ;# number of total forecast hours: 00, 12, 24, 36, 48, 72, 96, 120
set -A fcsthr nan 00hr 12hr 24hr 36hr 48hr 72hr 96hr 120hr

#-------------------------------
cat >${trackcase}.f <<EOF
! read track errors, write out for making scatter plots

      integer, parameter :: nmd=$NMD           !number of models 
      integer, parameter :: nhr=$NHR           !forecast hours: 00, 12, 24, 36, 48, 72, 96, 120
      real*4         :: terr(nhr, nmd)
      character*4    :: run(nmd)
      character      :: subname(80)
      data bad /9999.0/

      open (1, file="${trackcase}.txt",form="formatted",status="old")
      open (2, file="${trackcase}.bin",form="unformatted",status="unknown")

      icount=0
 100  continue
      read(1,'(80a)',end=200) subname
        write(10,'(80a)') subname
      read(1,'(80a)',end=200) subname
        write(10,'(80a)') subname
      do m=1,nmd
      read(1,'(x,a,3x,8f7.1)',end=200) run(m),(terr(n,m),n=1,nhr)
        write(10,'(x,a,3x,8f7.1)') run(m),(terr(n,m),n=1,nhr)
      enddo 
      read(1,*,end=200)
        write(10,*)

      write(2)((terr(n,m),n=1,nhr),m=1,nmd)
      icount=icount+1
      goto 100

200   continue
      write(10,*)"icount=",icount
      write(99,*)icount

      stop
      end

EOF
#-------------------------------
xlf90 ${trackcase}.f
./a.out


#-------------------------------
export count=`head fort.99`
cat >${trackcase}.ctl <<EOF
DSET ^${trackcase}.bin
OPTIONS SEQUENTIAL
UNDEF 9999.0
TITLE track errors                  
XDEF $NHR  LINEAR 1 1
YDEF $NMD LINEAR 1 1   
ZDEF 1 linear 1 1
TDEF $count LINEAR 00Z01JAN1950 6hr
VARS 1
terr  0   99    track error
ENDVARS
EOF
#-------------------------------


#----------------------------------------------
#  including all hours of forecast on one plot
HR1=1; HR2=8         
cat >${trackcase}_${HR1}${HR2}.gs <<EOF
 'reinit'; 'set font 1'
 'run /u/wx24fy/bin/grads/white.gs'                
 'open  ${trackcase}.ctl'           

 'set x 1 '
 'set y 1 '
 'set t 1 $count'

* $NHR forecast hours for first model 
 'define an1=terr(x=1,y=$MD1)'               
 'define an2=terr(x=2,y=$MD1)'               
 'define an3=terr(x=3,y=$MD1)'               
 'define an4=terr(x=4,y=$MD1)'               
 'define an5=terr(x=5,y=$MD1)'               
 'define an6=terr(x=6,y=$MD1)'               
 'define an7=terr(x=7,y=$MD1)'               
 'define an8=terr(x=8,y=$MD1)'               

* $NHR forecast hours for second model 
 'define bn1=terr(x=1,y=$MD2)'               
 'define bn2=terr(x=2,y=$MD2)'               
 'define bn3=terr(x=3,y=$MD2)'               
 'define bn4=terr(x=4,y=$MD2)'               
 'define bn5=terr(x=5,y=$MD2)'               
 'define bn6=terr(x=6,y=$MD2)'               
 'define bn7=terr(x=7,y=$MD2)'               
 'define bn8=terr(x=8,y=$MD2)'               

  cco.1=29; cco.2=25; cco.3=79; cco.4=75; cco.5=39; cco.6=35; cco.7=49; cco.8=45
  cma.1=3; cma.2=3; cma.3=3; cma.4=3; cma.5=3; cma.6=3; cma.7=3; cma.8=3
  fhr.1=${fcsthr[1]}; fhr.2=${fcsthr[2]}; fhr.3=${fcsthr[3]}; fhr.4=${fcsthr[4]}
  fhr.5=${fcsthr[5]}; fhr.6=${fcsthr[6]}; fhr.7=${fcsthr[7]}; fhr.8=${fcsthr[8]}

*---------------------------string/caption
 'set string 4 tc 6'
 'set strsiz 0.16 0.16'
 'draw string 4.2 10.2  $basinyear Hurricane Track Errors (nm)'
 'draw string 4.2 9.9  $DATST - $DATND, ${cyc} cycles'
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
j=$HR1
while ( j <= $HR2 )
    'set gxout stat'
    'd an'%j
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > tmax); tmax=zmax; endif
    if(zmin < tmin); tmin=zmin; endif
    ln=sublin(result,11); wd=subwrd(ln,2); meana.j=substr(wd,1,4)
    ln=sublin(result,7); wd=subwrd(ln,8); casea.j=substr(wd,1,6)
   j=j+1
endwhile
j=$HR1
while ( j <= $HR2 )
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



j=$HR1
while ( j <= $HR2 )
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
 'draw xlab ${model[$MD1]}'
 'draw ylab ${model[$MD2]}'
*
  iframe=iframe+1
endwhile

*'run /u/wx24fy/bin/grads/cbarn.gs  0.9 0 4. 0.4'
 'printim ${trackcase}a_${model[$MD1]}${model[$MD2]}.gif gif '
 'set vpage off'
'quit'

EOF
#---------------------------------
grads -bcp "run ${trackcase}_${HR1}${HR2}.gs"



#----------------------------------------------
#  one forecast hour each , 8 plots on one page 
HR1=1; HR2=8         
cat >${trackcase}one.gs <<EOF
 'reinit'; 'set font 1'
 'run /u/wx24fy/bin/grads/white.gs'                
 'open  ${trackcase}.ctl'           

 'set x 1 '
 'set y 1 '
 'set t 1 $count'

* $NHR forecast hours for first model 
 'define an1=terr(x=1,y=$MD1)'               
 'define an2=terr(x=2,y=$MD1)'               
 'define an3=terr(x=3,y=$MD1)'               
 'define an4=terr(x=4,y=$MD1)'               
 'define an5=terr(x=5,y=$MD1)'               
 'define an6=terr(x=6,y=$MD1)'               
 'define an7=terr(x=7,y=$MD1)'               
 'define an8=terr(x=8,y=$MD1)'               

* $NHR forecast hours for second model 
 'define bn1=terr(x=1,y=$MD2)'               
 'define bn2=terr(x=2,y=$MD2)'               
 'define bn3=terr(x=3,y=$MD2)'               
 'define bn4=terr(x=4,y=$MD2)'               
 'define bn5=terr(x=5,y=$MD2)'               
 'define bn6=terr(x=6,y=$MD2)'               
 'define bn7=terr(x=7,y=$MD2)'               
 'define bn8=terr(x=8,y=$MD2)'               

  cco.1=29; cco.2=25; cco.3=79; cco.4=75; cco.5=39; cco.6=35; cco.7=49; cco.8=45
  cma.1=3; cma.2=3; cma.3=3; cma.4=3; cma.5=3; cma.6=3; cma.7=3; cma.8=3
  fhr.1=${fcsthr[1]}; fhr.2=${fcsthr[2]}; fhr.3=${fcsthr[3]}; fhr.4=${fcsthr[4]}
  fhr.5=${fcsthr[5]}; fhr.6=${fcsthr[6]}; fhr.7=${fcsthr[7]}; fhr.8=${fcsthr[8]}

*---------------------------string/caption
 'set string 4 tc 6'
 'set strsiz 0.15 0.15'
 'draw string 4.2 10.4  $basinyear Hurricane Track Errors (nm)'
 'draw string 4.2 10.1  $DATST - $DATND, ${cyc} cycles'
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
 'draw string 4.5 0.2 ${model[$MD1]}'
 'set string 1 tc 6 90'
 'set strsiz 0.17 0.17'
 'draw string 0.3 5.0 ${model[$MD2]}'

*'run /u/wx24fy/bin/grads/cbarn.gs  0.9 0 4. 0.4'
 'printim ${trackcase}b_${model[$MD1]}${model[$MD2]}.gif gif '
 'set vpage off'
'quit'

EOF
#---------------------------------
grads -bcp "run ${trackcase}one.gs"


exit
