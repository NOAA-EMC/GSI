#!/bin/ksh
set -x

## Fanglin Yang, Dec 2009
## compute precip threat skill scores
## make maps with significane tests
## applicable for 00Z or 12Z cycle forecasts


STYMD=${DATEST:-20080605}
EDYMD=${DATEND:-20081005}
cycle=${cycle:-00}
SDIR=${datdir:-/global/hires/glopara/archive}
EXPIDS=${expnlist:-"gfs pru12h"} 

export scrdir=${scrdir:-/global/save/wx24fy/VRFY/vsdb/precip}
export rundir=${rundir:-/ptmp/$LOGNAME/pvrfy}
mkdir -p $rundir; cd $rundir || exit 8; rm $rundir/*

affix=" "
if [ $cycle = "12" ]; then affix=12;  fi
STYMDC=`$scrdir/util/datestamp.sh $STYMD 0 `
EDYMDC=`$scrdir/util/datestamp.sh $EDYMD 0 `
nexp=`echo $EXPIDS | wc -w`  ;#count number of experiments, must be within 10
nexp1=`expr $nexp - 1 `
set -A expname none $EXPIDS

export NWPROD=${NWPROD:-/nwprod}
export ndate=${ndate:-$NWPROD/util/exec/ndate}
export GRADSBIN=${GRADSBIN:-/usrx/local/grads/bin}

#-------------------------------------------------
nid=0
for ID in $EXPIDS ; do
nid=`expr $nid + 1 `
rm filelist$nid ; touch filelist$nid
ymd=$STYMD; ndays=0
while [ $ymd -le $EDYMD ]; do
   ndays=`expr $ndays + 1 `
   echo " \"$SDIR/${ID}_rain_$ymd$affix\" "  >>filelist$nid
   ymd=`$ndate 24 $ymd\00 | cut -c1-8`
done
done
#-------------------------------------------------

## nregion: 1-211 CONUS, 2-211A, 3-211N, 4-211S, 
##          5-211E, 6-211G, 7-211W, 8-211R, 9-211M, 10-211F
nregion=${nregion:-1}
ntest=${ntest:-10000}     ;# number of monte carlo significane tests
ncat=9                   ;# precip intensity category, hard-wired, donot change
                          # 0.2 2 5 10 15 25 35 50 75 mm/24hrs

${scrdir}/exec/precip_score.x $ndays $nexp $nregion $ntest $cycle

if [ $cycle = "00" ]; then
 fhourlist="36 60 84"
 mv fort.21 f36daily.dat    
 mv fort.22 f60daily.dat    
 mv fort.23 f84daily.dat    
 mv fort.31 f36mean.dat 
 mv fort.32 f60mean.dat 
 mv fort.33 f84mean.dat 
 mv fort.41 f36std_montecarlo.dat 
 mv fort.42 f60std_montecarlo.dat 
 mv fort.43 f84std_montecarlo.dat 
 mv fort.51 f36dif_montecarlo.dat 
 mv fort.52 f60dif_montecarlo.dat 
 mv fort.53 f84dif_montecarlo.dat 
 mv fort.99 obcount.txt
elif [ $cycle = "12" ]; then
 fhourlist="24 48 72"
 mv fort.21 f24daily.dat    
 mv fort.22 f48daily.dat    
 mv fort.23 f72daily.dat    
 mv fort.31 f24mean.dat 
 mv fort.32 f48mean.dat 
 mv fort.33 f72mean.dat 
 mv fort.41 f24std_montecarlo.dat 
 mv fort.42 f48std_montecarlo.dat 
 mv fort.43 f72std_montecarlo.dat 
 mv fort.51 f24dif_montecarlo.dat 
 mv fort.52 f48dif_montecarlo.dat 
 mv fort.53 f72dif_montecarlo.dat 
 mv fort.99 obcount.txt
else
 echo " ${cycle}Z cycle not supported. exit"
 exit
fi


#------------- GrADS control files ------------------
for fhour in $fhourlist ; do

cat >f${fhour}daily.ctl <<EOF
dset ^f${fhour}daily.dat
options big_endian sequential 
undef -9999.0   
title daily-sample skill scores, y-models, x-category
xdef    $ncat linear 1 1
ydef    $nexp linear 1 1
zdef    1 linear    1.000  1.0000
tdef   $ndays linear   ${cycle}z$STYMDC     24hr
vars    3
bis   0 0 bias score
ets   0 0 equitable threat acore
obs   0 0 total obs counts
endvars
EOF

cat >f${fhour}mean.ctl <<EOF
dset ^f${fhour}mean.dat
options big_endian sequential 
undef -9999.0   
title  all-sample skill scores, x-category, y-models
xdef    $ncat linear 1 1
ydef    $nexp linear 1 1
zdef    1 linear    1.000  1.0000
tdef 1 linear   ${cycle}z$STYMDC     24hr
vars    8 
bis  0 0 all-sample bias score
ets  0 0 all-sample equitable threat acore
obt  0 0 observed grid counts above a given threshold
mbis  0 0 mean of daily bias score
mets  0 0 mean of equitable threat acore
vbis  0 0 standard deviation of daily bias score
vets  0 0 standard deviation of equitable threat acore
ngood 0 0 number of sampele days used to compute mean and standard deviation
endvars
EOF

cat >f${fhour}std_montecarlo.ctl <<EOF
dset ^f${fhour}std_montecarlo.dat
options big_endian sequential 
undef -9999.0   
title  x-category, y-models
xdef    $ncat linear 1 1
ydef    $nexp linear 1 1
zdef    1 linear    1.000  1.0000
tdef 1 linear   ${cycle}z$STYMDC     24hr
vars    2 
std_bis 0 0 standard devistion of bias score differences from Monte Carlo tests
std_ets 0 0 standard deviation of equitable threat score differences from Monte Carlo tests
endvars
EOF
 
cat >f${fhour}dif_montecarlo.ctl <<EOF
dset ^f${fhour}dif_montecarlo.dat
options big_endian sequential 
undef -9999.0   
title  x-category, z-monte carlo ntest, t-model
xdef    $ncat linear 1 1
ydef    1 linear     1 1
zdef    $ntest linear    1.000  1.0000
tdef $nexp1 linear   1jan1900   1dy        
vars    2 
dif_bis $ntest 0 bias score differences from Monte Carlo tests
dif_ets $ntest 0 equitable threat score differences from Monte Carlo tests
endvars
EOF
 
done
#------------- end of GrADS control files --------


#------------------------------------------------
#------------- Make maps -------------------------
for fhre in $fhourlist ; do
fhrs=`expr $fhre - 24 `
if [ $fhrs -lt 10 ]; then fhrs=0$fhrs; fi


#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#////////////////////////////////////////////////////////////
# PLOT 1:  All-sample ETS BIAS with significance test 

cat >etsbis_f${fhre}.gs <<EOF1 
'reinit'; 'set font 1'
'open f${fhre}mean.ctl'
'open f${fhre}std_montecarlo.ctl'

*-- define line styles 
  cst.1=1; cst.2=1; cst.3=1; cst.4=1; cst.5=1; cst.6=1; cst.7=1; cst.8=1; cst.9=1; cst.10=1
  cth.1=12; cth.2=12; cth.3=9; cth.4=9; cth.5=9; cth.6=9; cth.7=9; cth.8=9; cth.9=9; cth.10=9
  cma.1=2; cma.2=3; cma.3=6; cma.4=8; cma.5=7; cma.6=5; cma.7=4; cma.8=1; cma.9=9; cma.10=10
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=9; cco.7=5; cco.8=6; cco.9=7; cco.10=15
  bar.1=30; bar.2=40; bar.3=50; bar.4=60; bar.5=70; bar.6=80; bar.7=90; bar.8=92; bar.9=94; bar.10=96

*--model name
  mdc.1=${expname[1]}
  if($nexp>=2); mdc.2=${expname[2]} ;endif
  if($nexp>=3); mdc.3=${expname[3]} ;endif
  if($nexp>=4); mdc.4=${expname[4]} ;endif
  if($nexp>=5); mdc.5=${expname[5]} ;endif
  if($nexp>=6); mdc.6=${expname[6]} ;endif
  if($nexp>=7); mdc.7=${expname[7]} ;endif
  if($nexp>=8); mdc.8=${expname[8]} ;endif
  if($nexp>=9); mdc.9=${expname[9]} ;endif
  if($nexp>=10); mdc.10=${expname[10]} ;endif

*--observation counts
  count=read(obcount.txt)
  outrec=sublin(count,2)
  nc.1=subwrd(outrec,1); nc.2=subwrd(outrec,2); nc.3=subwrd(outrec,3)
  nc.4=subwrd(outrec,4); nc.5=subwrd(outrec,5); nc.6=subwrd(outrec,6)
  nc.7=subwrd(outrec,7); nc.8=subwrd(outrec,8); nc.9=subwrd(outrec,9)
*------------------------------


  xwd=4.4; ywd=4.0; yy=ywd/15
  'set parea 0 11.0 0 8.4'                    
  'set string 1 bc 6'; 'set strsiz 0.16 0.16'
  'draw string 5.5 8.3 CONUS Precip Skill Scores, f${fhrs}-f${fhre}, $STYMDC-$EDYMDC ${cycle}Z Cycle'
  'set string 1 bl 4'; 'set strsiz 0.11 0.11'
  'draw string 0.8 0.2 Differences outside of the hollow bars are 95% significant based on $ntest Monte Carlo Tests '

*---------------------------------------
*----first panel ETS--------------------
  xmin=0.8; xmax=xmin+xwd
  ymin=4.0; ymax=ymin+ywd
  yt=ymax-0.1; xt=xmin+0.6*xwd; xt1=xt+0.5; xt2=xt1+0.1
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

  'set x 1 $ncat' 
  'set y 1 '

*--find maximum and minmum values to determine y-axis labels
 cmax=-1.0; cmin=1.0
 i=1
 while (i <= $nexp)
    'set gxout stat'
    'd ets(y='%i')'
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > cmax); cmax=zmax; endif
    if(zmin < cmin); cmin=zmin; endif
 i=i+1
 endwhile
 cmin=substr(cmin,1,6); cmax=substr(cmax,1,6)
 if(cmax > 1); cmax=1; endif
 if(cmin < 0); cmin=0; endif
 dist=cmax-cmin 
 if (dist = 0); dist=1; endif
 cintp=10*substr(dist/40,1,4)
 if (cintp = 0); cintp=10*substr(dist/40,1,5); endif
 if (cintp = 0); cintp=10*substr(dist/40,1,6); endif

   i=1
   while (i <= $nexp)
   'set y 'i
      'set strsiz 0.15 0.15'; yt=yt-yy
      'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
      'set string 'cco.i' bl 6';  'draw string 'xt2' 'yt' 'mdc.i 

      'set gxout line'
      'set mproj off'
      'set display color white'; 'set missconn off';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.0';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 0 'cmax; 'set ylint 0.1'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'd ets(y='%i')'
   i=i+1
   endwhile
  'draw ylab Equitable Threat Score'

*--plot observed count
  'set string 4 bc 4'; 'set strsiz 0.09 0.09'
  j=1; while (j <= $ncat)
   xpos=xmin+(j-1)*xwd/8.0                   
   'draw string 'xpos' 'ymin+0.1' 'nc.j
   j=j+1
  endwhile

*---------------------------
* plot difference between others and the first
  ymin=ymin-3; ymax=ymin+3
  'set parea 'xmin' 'xmax' 'ymin' 'ymax
  xlabx=xmin+0.40*xwd;  xlaby=ymin-0.50
  xlabx1=xmin+0.51*xwd;  xlaby1=ymin-0.2                  

*--find maximum and minmum values to determine y-axis labels
 cmax=-1.0; cmin=1.0
 i=2
 while (i <= $nexp)
    'set gxout stat'
    'd ets(y='%i')-ets(y=1)'
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > cmax); cmax=zmax; endif
    if(zmin < cmin); cmin=zmin; endif
      'd 1.96*std_ets.2(y='%i')'
      range=sublin(result,9); zmax=subwrd(range,6); zmin=-zmax
      if(zmax > cmax); cmax=zmax; endif
      if(zmin < cmin); cmin=zmin; endif
 i=i+1
 endwhile
 cmin=substr(cmin,1,6); cmax=substr(cmax,1,6)
 if(cmax > 1); cmax=1; endif
 if(cmin < -1); cmin=-1; endif
 dist=cmax-cmin 
 if (dist = 0); dist=1; endif
 cintp=10*substr(dist/40,1,4)
 if (cintp = 0); cintp=10*substr(dist/40,1,5); endif
 if (cintp = 0); cintp=10*substr(dist/40,1,6); endif
 
*---------------------------------------------------------
* standard deviation of the difference between the 
* first and each of the rest models for each category  

 i=2
 while (i <= $nexp)
** plot the 5% conf interval of difference of means : 1.96*std
   'define intvl=1.96*std_ets.2(y='%i')'
*  'define intvl=maskout( 1.96*std_ets.2(y='%i'), ets(y='%i') )'
   'set gxout bar'
   'set bargap 'bar.i
   'set baropts outline'
   'set ccolor 'cco.i
   'set cstyle 1'; 'set cthick 3'; 'set cmark 0'
   'set mproj off'
   'set display color white'; 'set grads off'; 'set grid on'
   'set xlopts 1 6 0.0';     'set ylopts 1 6 0.0'; 'set clopts 1 6 0.0'
   'set vrange 'cmin' 'cmax; 'set ylint 'cintp
   'd -intvl;intvl'
**add missing bars when intvl is larger than cmax
   'set ccolor 'cco.i
   'define xa=maskout(intvl*0+0.999*'cmin',intvl+'cmin')'
   'define xb=maskout(intvl*0+0.999*'cmax',intvl-'cmax')'
   'set datawarn off'
   'd xa;xb'
 i=i+1
 endwhile

 i=1
 while (i <= $nexp)
     'set gxout line'
     'set mproj off'
     'set display color white'; 'set missconn off';     'set grads off'; 'set grid on'
     'set xlopts 1 6 0.0';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
     'set vrange 'cmin' 'cmax; 'set ylint 'cintp
     'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
     if(i=1); 'set cstyle 1'; 'set cthick 1'; 'set cmark 0'; 'set ccolor 1'; endif
     'd ets(y='%i')-ets(y=1)'
 i=i+1
 endwhile

 'set string 1 bc 5'
 'set strsiz 0.15 0.15'
 'draw string 'xlabx' 'xlaby' Threshold (mm/24hr)'
 'set string 1 bl 6'
 'set strsiz 0.14 0.14'
 'draw string 'xmin+0.2' 'ymax-0.2' Difference w.r.t. '${expname[1]}
 'set string 1 bl 3'
 'set strsiz 0.11 0.11'

*--add marks of precip intensity categories
  pi.1=0.2; pi.2=2; pi.3=5; pi.4=10; pi.5=15; pi.6=25; pi.7=35; pi.8=50; pi.9=75 
  'set string 1 bc 6'; 'set strsiz 0.12 0.12'
  j=1; while (j <= $ncat)
   xpos=xmin+(j-1)*xwd/8.0                   
   'draw string 'xpos' 'ymin-0.2' 'pi.j
   j=j+1
  endwhile




*---------------------------------------
*----2nd panel BIAS--------------------
  xmin=xmax+1.0; xmax=xmin+xwd
  ymin=4.0; ymax=ymin+ywd
  yt=ymax-0.1; xt=xmin+0.1*xwd; xt1=xt+0.5; xt2=xt1+0.1
  xlabx=xmin+0.45*xwd;  xlaby=ymin-0.50
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

  'set x 1 $ncat' 
  'set y 1 '

*--find maximum and minmum values to determine y-axis labels
 cmax=-999.0; cmin=999.0
 i=1
 while (i <= $nexp)
    'set gxout stat'
    'd bis(y='%i')'
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > cmax); cmax=zmax; endif
    if(zmin < cmin); cmin=zmin; endif
 i=i+1
 endwhile
 cmin=substr(cmin,1,6); cmax=substr(cmax,1,6)
 if(cmax > 3); cmax=3; endif
 if(cmax < 2); cmax=2; endif
 if(cmin < 0.48 ); cmin=0.48; endif
 dist=cmax-cmin 
 if (dist = 0); dist=1; endif
 cintp=10*substr(dist/40,1,4)
 if (cintp = 0); cintp=10*substr(dist/40,1,5); endif
 if (cintp = 0); cintp=10*substr(dist/40,1,6); endif


   i=1
   while (i <= $nexp)
   'set y 'i
      'set strsiz 0.15 0.15'; yt=yt-yy
      'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
      'set string 'cco.i' bl 6';  'draw string 'xt2' 'yt' 'mdc.i 

      'set gxout line'
      'set mproj off'
      'set display color white'; 'set missconn off';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.0';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
*     'set vrange 'cmin' 'cmax; 'set ylint 'cintp
      'set vrange 'cmin ' 'cmax; 'set ylint 0.5 '
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'd bis(y='%i')'
   i=i+1
   endwhile
  'draw ylab BIAS Score'

*--plot observed count
  'set string 4 bc 4'; 'set strsiz 0.09 0.09'
  j=1; while (j <= $ncat)
   xpos=xmin+(j-1)*xwd/8.0                   
   'draw string 'xpos' 'ymin+0.1' 'nc.j
   j=j+1
  endwhile

*---------------------------
* plot difference between others and the first
  ymin=ymin-3; ymax=ymin+3
  'set parea 'xmin' 'xmax' 'ymin' 'ymax
  xlabx=xmin+0.40*xwd;  xlaby=ymin-0.50
  xlabx1=xmin+0.51*xwd;  xlaby1=ymin-0.2                  

*--find maximum and minmum values to determine y-axis labels
 cmax=-99.0; cmin=99.0
 i=2
 while (i <= $nexp)
    'set gxout stat'
    'd bis(y='%i')-bis(y=1)'
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > cmax); cmax=zmax; endif
    if(zmin < cmin); cmin=zmin; endif
      'd 1.96*std_bis.2(y='%i')'
      range=sublin(result,9); zmax=subwrd(range,6); zmin=-zmax
      if(zmax > cmax); cmax=zmax; endif
      if(zmin < cmin); cmin=zmin; endif
 i=i+1
 endwhile
 cmin=substr(cmin,1,6); cmax=substr(cmax,1,6)
 if(cmax > 1.5); cmax=1.5; endif
 if(cmin < -1.5); cmin=-1.5; endif
 dist=cmax-cmin 
 if (dist = 0); dist=1; endif
 dist=cmax-cmin 
 cintp=10*substr(dist/40,1,4)
 if (cintp = 0); cintp=10*substr(dist/40,1,5); endif
 if (cintp = 0); cintp=10*substr(dist/40,1,6); endif

*---------------------------------------------------------
* standard deviation of the difference between the 
* first and each of the rest models for each category  

 i=2
 while (i <= $nexp)
**plot the 5% conf interval of difference of means : 1.96*std
*  'define intvl=1.96*std_bis.2(y='%i')'
   'define intvl=maskout( 1.96*std_bis.2(y='%i'), bis(y='%i') )'
   'set gxout bar'
   'set bargap 'bar.i
   'set baropts outline'
   'set ccolor 'cco.i
   'set cstyle 1'; 'set cthick 3'; 'set cmark 0'
   'set mproj off'
   'set display color white'; 'set grads off'; 'set grid on'
   'set xlopts 1 6 0.0';     'set ylopts 1 6 0.0'; 'set clopts 1 6 0.0'
   'set vrange 'cmin' 'cmax; 'set ylint 'cintp
   'd -intvl;intvl'
**add missing bars when intvl is larger than cmax
   'set ccolor 'cco.i
   'define xa=maskout(intvl*0+0.999*'cmin',intvl+'cmin')'
   'define xb=maskout(intvl*0+0.999*'cmax',intvl-'cmax')'
   'set datawarn off'
   'd xa;xb'
 i=i+1
 endwhile

 i=1
 while (i <= $nexp)
     'set gxout line'
     'set mproj off'
     'set display color white'; 'set missconn off';     'set grads off'; 'set grid on'
     'set xlopts 1 6 0.0';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
     'set vrange 'cmin' 'cmax; 'set ylint 'cintp
     'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
     if(i=1); 'set cstyle 1'; 'set cthick 1'; 'set cmark 0'; 'set ccolor 1'; endif
     'd bis(y='%i')-bis(y=1)'
 i=i+1
 endwhile

 'set string 1 bc 5'
 'set strsiz 0.15 0.15'
 'draw string 'xlabx' 'xlaby' Threshold (mm/24hr)'
 'set string 1 bl 6'
 'set strsiz 0.14 0.14'
 'draw string 'xmin+0.2' 'ymax-0.2' Difference w.r.t. '${expname[1]}
 'set string 1 bl 3'
 'set strsiz 0.11 0.11'

*--add marks of precip intensity categories
  'set string 1 bc 6'; 'set strsiz 0.12 0.12'
  j=1; while (j <= $ncat)
   xpos=xmin+(j-1)*xwd/8.0                   
   'draw string 'xpos' 'ymin-0.2' 'pi.j
   j=j+1
  endwhile

*----------------------------------------------
  'printim etsbis.f${fhre}.png png x800 y600'
  'set vpage off'
'quit'
EOF1
$GRADSBIN/grads -bcl "run etsbis_f${fhre}.gs" &
sleep 5
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#////////////////////////////////////////////////////////////




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#/////////////////////////////////////////////////////////////////////
# PLOT 2:  ETS, BIAS and COUNT for each caterory as a function of time
set -A intrain  none 0.2 2 5 10 15 25 35 50 75
set -A intrainc none 002 020 050 100 150 250 350 500 750
icat=1
while [ $icat -le $ncat ]; do
intp=${intrain[$icat]}
intpc=${intrainc[$icat]}

cat >etsbis.p${intpc}_${fhrs}-${fhre}.gs <<EOF1 
'reinit'; 'set font 1'
'open f${fhre}daily.ctl'

*-- define line styles 
  cst.1=1; cst.2=1; cst.3=1; cst.4=1; cst.5=1; cst.6=1; cst.7=1; cst.8=1; cst.9=1; cst.10=1
  cth.1=12; cth.2=12; cth.3=9; cth.4=9; cth.5=9; cth.6=9; cth.7=9; cth.8=9; cth.9=9; cth.10=9
  cma.1=2; cma.2=3; cma.3=6; cma.4=8; cma.5=7; cma.6=5; cma.7=4; cma.8=1; cma.9=9; cma.10=10
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=9; cco.7=5; cco.8=6; cco.9=7; cco.10=15
  bar.1=30; bar.2=40; bar.3=50; bar.4=60; bar.5=70; bar.6=80; bar.7=90; bar.8=92; bar.9=94; bar.10=96

*--model name
  mdc.1=${expname[1]}
  if($nexp>=2); mdc.2=${expname[2]} ;endif
  if($nexp>=3); mdc.3=${expname[3]} ;endif
  if($nexp>=4); mdc.4=${expname[4]} ;endif
  if($nexp>=5); mdc.5=${expname[5]} ;endif
  if($nexp>=6); mdc.6=${expname[6]} ;endif
  if($nexp>=7); mdc.7=${expname[7]} ;endif
  if($nexp>=8); mdc.8=${expname[8]} ;endif
  if($nexp>=9); mdc.9=${expname[9]} ;endif
  if($nexp>=10); mdc.10=${expname[10]} ;endif

*------------------------------
  xwd=7.0; ywd=3.0; yy=ywd/15
  'set parea 0 8.5 0 11'                    
  'set string 1 bc 6'; 'set strsiz 0.14 0.14'
  'draw string 4.3 10.7 CONUS Precip Skill Scores, $STYMDC-$EDYMDC ${cycle}Z Cycle'
  'draw string 4.3 10.4 f${fhrs}-f${fhre} Average, Threshold >= $intp mm/24hr '

*----first/top panel ETS--------------------
  xmin=1.0; xmax=xmin+xwd
  ymin=7.0; ymax=ymin+ywd
  yt=ymax-0.1; xt=xmin+0.1*xwd; xt1=xt+0.5; xt2=xt1+0.1
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

  'set x $icat' 
  'set t 1 $ndays' 
  'set y 1 '

*--find maximum and minmum values to determine y-axis labels
 cmax=-1.0; cmin=1.0
 i=1
 while (i <= $nexp)
    'set gxout stat'
    'd ets(y='%i')'
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > cmax); cmax=zmax; endif
    if(zmin < cmin); cmin=zmin; endif
 i=i+1
 endwhile
 cmin=substr(cmin,1,6); cmax=substr(cmax,1,6)
 if(cmax > 1); cmax=1; endif
 if(cmin < 0); cmin=0; endif
 dist=cmax-cmin 
 if (dist = 0); dist=1; endif
 cintp=10*substr(dist/40,1,4)
 if (cintp = 0); cintp=10*substr(dist/40,1,5); endif
 if (cintp = 0); cintp=10*substr(dist/40,1,6); endif

   i=1
   while (i <= $nexp)
   'set y 'i
      'set strsiz 0.15 0.15'; yt=yt-yy
      'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
      'set string 'cco.i' bl 6';  'draw string 'xt2' 'yt' 'mdc.i 

      'set gxout line'
      'set mproj off'
      'set display color white'; 'set missconn off';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.0';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 'cmin ' 'cmax; 'set ylint 0.1'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'd ets(y='%i')'
   i=i+1
   endwhile
  'draw ylab ET Score'


*----2nd/middle panel BIAS--------------------
  xmin=1.0; xmax=xmin+xwd
  ymin=3.8; ymax=ymin+ywd
  yt=ymax-0.1; xt=xmin+0.1*xwd; xt1=xt+0.5; xt2=xt1+0.1
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

  'set x $icat' 
  'set t 1 $ndays' 
  'set y 1 '

*--find maximum and minmum values to determine y-axis labels
 cmax=-999.0; cmin=999.0
 i=1
 while (i <= $nexp)
    'set gxout stat'
    'd bis(y='%i')'
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > cmax); cmax=zmax; endif
    if(zmin < cmin); cmin=zmin; endif
 i=i+1
 endwhile
 cmin=substr(cmin,1,6); cmax=substr(cmax,1,6)
 if(cmax > 3); cmax=3; endif
 if(cmin < 0); cmin=0; endif
 dist=cmax-cmin 
 if (dist = 0); dist=1; endif
 cintp=10*substr(dist/40,1,4)
 if (cintp = 0); cintp=10*substr(dist/40,1,5); endif
 if (cintp = 0); cintp=10*substr(dist/40,1,6); endif

   i=1
   while (i <= $nexp)
   'set y 'i
      'set strsiz 0.15 0.15'; yt=yt-yy
      'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
      'set string 'cco.i' bl 6';  'draw string 'xt2' 'yt' 'mdc.i 

      'set gxout line'
      'set mproj off'
      'set display color white'; 'set missconn off';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.0';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 'cmin' 'cmax; 'set ylint 0.5'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'd bis(y='%i')'
   i=i+1
   endwhile
  'draw ylab BIAS Score'


*----3rd/bottom panel OBS Count--------------------
  xmin=1.0; xmax=xmin+xwd
  ymin=0.6; ymax=ymin+ywd
  yt=ymax-0.1; xt=xmin+0.1*xwd; xt1=xt+0.5; xt2=xt1+0.1
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

  'set x $icat' 
  'set t 1 $ndays' 
  'set y 1 '

*--find maximum and minmum values to determine y-axis labels
 cmax=-999.0; cmin=999.0
 i=1
 while (i <= $nexp)
    'set gxout stat'
    'd obs(y='%i')'
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > cmax); cmax=zmax; endif
    if(zmin < cmin); cmin=zmin; endif
 i=i+1
 endwhile
 cmin=substr(cmin,1,7); cmax=substr(cmax,1,7)
 dist=cmax-cmin 
 if (dist = 0); dist=1; endif
 cintp=10*substr(dist/40,1,4)
 if (cintp = 0); cintp=10*substr(dist/40,1,5); endif
 if (cintp = 0); cintp=10*substr(dist/40,1,6); endif

   i=1
*  while (i <= $nexp)
   'set y 'i
      'set strsiz 0.15 0.15'; yt=yt-yy
      'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
      'set string 'cco.i' bl 6';  'draw string 'xt2' 'yt' 'mdc.i 

      'set gxout line'
      'set mproj off'
      'set display color white'; 'set missconn off';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.12';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 0 'cmax; 'set ylint 'cintp
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'd obs(y='%i')'
*  i=i+1
*  endwhile
  'draw ylab OBS Count'

*----------------------------------------------
  'printim etsbis.p${intpc}_${fhrs}-${fhre}.png png x600 y800'
  'set vpage off'
'quit'
EOF1
$GRADSBIN/grads  -bcp "run etsbis.p${intpc}_${fhrs}-${fhre}.gs" &
sleep 3


icat=`expr $icat + 1 `
done
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#////////////////////////////////////////////////////////////


#---------------------------------------------------------
done
#---------------------------------------------------------

exit
