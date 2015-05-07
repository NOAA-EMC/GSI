#!/bin/ksh
set -x

#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#  This script make graphics using grid2obs surface verification statistics
#  saved in vsdb format. It is aimed to compare the diurnal cycles of forecasts
#  and ground observations of T2m, RH2m and wind speed at 10m over CONUS
#  and its subregions. Plots compare scores from all centers for the same 
#  forecast cycle (e.g. 00Z)
#  Fanglin Yang, November 2011
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
## -- verification region
export regname=${regname:-${1:-west}}
export reglist=${reglist:-"NWC SWC GRB NMT SMT SWD NPL SPL"}
export regdef=${regdef:-"CONUS West"}                          

## -- verification dates
CDATE=$(date +%Y%m%d)
edate=${edate:-$CDATE}      ;#end of verification date
ndays=${ndays:-31}          ;#number of days back for verification
vlength=${vlength:-168}     ;#forecast length in hours to be verified
fhout=${fhout:-6}           ;#forecast output frequency in hours       


## -- verification parameters
export cyclist=${cyclist:-"00"}
export vnamlist=${vnamlist:-"T RH VWND"}
export mdlist=${mdlist:-"gfs prd12q3k"}
export levlist=${levlist:-"SFC"}

export webhost=${webhost:-"emcrzdm.ncep.noaa.gov"}
export webhostid=${webhostid:-"$LOGNAME"}
export vsdb_data=${vsdb_data:-/climate/save/wx24fy/VRFY/vsdb_data/grid2obs}
export sorcdir=${sorcdir:-/global/save/wx24fy/VRFY/vsdb/grid2obs}
export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$webhostid/vsdb} 
export doftp=${doftp:-"NO"}                                    
export copymap=${copymap:-"NO"}                                
export archmon=${archmon:-"NO"}                               
export tmpdir=${tmpdir:-/stmp/$LOGNAME/g2oplot/sfc}
export mapdir=${mapdir:-$tmpdir/maps}

export fnum=$((vlength/fhout))     ;#number of forecasts up to (vlength-fhout) hours
export xtick=$((fnum/8)) 

## remove missing data from all models to unify sample size, 0-->NO, 1-->Yes
export maskmiss=${maskmiss:-1}                                        

export ACCOUNT=${ACCOUNT:-GFS-MTN}                                           ;#ibm computer ACCOUNT task
export CUE2RUN=${CUE2RUN:-dev}                                               ;#dev or devhigh or 1
export CUE2FTP=${CUE2FTP:-transfer}                                          ;#data transfer queue
export GROUP=${GROUP:-g01}                                                   ;#account group
export NWPROD=${NWPROD:-/global/save/Fanglin.Yang/VRFY/vsdb/nwprod}          ;#utilities and libs included in /nwprod
export SUBJOB=${SUBJOB:-$vsdbhome/bin/sub_wcoss}                             ;#script for submitting batch jobs
export ndate=${ndate:-$NWPROD/util/exec/ndate}
export FC=${FC:-/usrx/local/intel/composer_xe_2011_sp1.11.339/bin/intel64/ifort}  ;#compiler
export FFLAG=${FFLAG:-"-O2 -convert big_endian -FR"}                              ;#compiler options

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
for vnam in $vnamlist; do
   export exedir=${tmpdir}/${vnam}
   if [ -s $exedir ]; then rm -rf $exedir; fi
   mkdir -p $exedir; cd $exedir || exit

for cyc  in $cyclist ; do
for lev  in $levlist ; do
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------

# -- generate output names 
  nhours=`expr $ndays \* 24 - 24`
  tmp=`$ndate -$nhours ${edate}00 `
  sdate=`echo $tmp | cut -c 1-8`
  outname1=${vnam}_${lev}_${regname}_${cyc}Z${sdate}${edate}
  yyyymm=`echo $edate |cut -c 1-6`                                
  outmon=${vnam}_${lev}_${regname}_${cyc}Z${yyyymm}

# -- search data for all models; write out binary data, create grads control file
  if [ $vnam = "VWND" ]; then
   $sorcdir/scripts/g2o_sfcmap_wind.sh $vnam $regname "$reglist" $lev $edate $ndays $cyc $vlength $fhout $outname1 $maskmiss "$mdlist"
  else
   $sorcdir/scripts/g2o_sfcmap_scal.sh $vnam $regname "$reglist" $lev $edate $ndays $cyc $vlength $fhout $outname1 $maskmiss "$mdlist"
  fi


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# -- create grads scripts, allows up to 10 models 
nmd=`echo $mdlist | wc -w`  ;#count number of models
set -A mdname $mdlist
set -A mdnamec `echo $mdlist |tr "[a-z]" "[A-Z]" `
#namedaily=${vnam}_${lev}_${regname}_${cyc}Z
namedaily=${vnam}_${lev}_${regname}


# ----- PLOT TYPE 1:  time series of anomaly correlations ----
cat >acz_${outname1}.gs <<EOF1 
'reinit'; 'set font 1'
'open ${outname1}.ctl'
mdc.1=${mdnamec[0]}
if($nmd >1); mdc.2=${mdnamec[1]} ;endif
if($nmd >2); mdc.3=${mdnamec[2]} ;endif
if($nmd >3); mdc.4=${mdnamec[3]} ;endif
if($nmd >4); mdc.5=${mdnamec[4]} ;endif
if($nmd >5); mdc.6=${mdnamec[5]} ;endif
if($nmd >6); mdc.7=${mdnamec[6]} ;endif
if($nmd >7); mdc.8=${mdnamec[7]} ;endif
if($nmd >8); mdc.9=${mdnamec[8]} ;endif
if($nmd >9); mdc.10=${mdnamec[9]} ;endif

*-- define line styles and model names 
  cst.1=1; cst.2=5; cst.3=3; cst.4=5; cst.5=5; cst.6=3; cst.7=5; cst.8=5; cst.9=5; cst.10=5
  cth.1=10; cth.2=10; cth.3=9; cth.4=9; cth.5=9; cth.6=9; cth.7=9; cth.8=9; cth.9=9; cth.10=9
  cma.1=0; cma.2=8; cma.3=6; cma.4=1; cma.5=2; cma.6=0; cma.7=4; cma.8=3; cma.9=4; cma.10=5
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=9; cco.7=5; cco.8=6; cco.9=7; cco.10=15
*------------------------
nf=1 ;*start from fcst00
while ( nf <  ${fnum} )
fhr=(nf-1)*${fhout}
*------------------------
  'c'
  'set x 1' 
  'set t 1 $ndays' 
  'set y '%nf 

  xwd=7.0; ywd=4.0; yy=ywd/16
  xmin=1.0; xmax=xmin+xwd; ymin=6.0; ymax=ymin+ywd
  xt=xmin+0.3; xt1=xt+0.5; xt2=xt1+0.1; yt=ymin+0.36*ywd ;*for legend  
  zxt=xt2+3.0; zxt1=zxt+0.5; zxt2=zxt1+0.1; zyt=ymin+0.36*ywd
  titlx=xmin+0.5*xwd;  titly=ymax+0.20                  ;*for tytle
  xlabx=xmin+0.45*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

*--find maximum and minmum values
*  cmax=-1.0; cmin=1.0
*  i=1
*  while (i <= $nmd)
*   'set gxout stat'
*   'd cor(x='%i')'
*   range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
*   if(zmax > cmax); cmax=zmax; endif
*   if(zmin < cmin); cmin=zmin; endif
*  i=i+1
*  endwhile
*  dist=cmax-cmin; cmin=cmin-0.9*dist; cmax=1.2*cmax        
*  if (cmin < -1.0); cmin=-1.0; endif
*  if (cmax > 1.0); cmax=1.0; endif
*  cmin=substr(cmin,1,3); cmax=substr(cmax,1,3); cint=0.1
*  say 'cmin cmax cint 'cmin' 'cmax' 'cint
   cmin=0.2; cmax=1.0; cint=0.1

   i=1
   while (i <= $nmd)
    'set gxout stat'  ;* first compute means and count good data numbers
    'd cor(x='%i')'
    ln=sublin(result,11); wd=subwrd(ln,2); mean=substr(wd,1,5)
    ln=sublin(result,7); wd=subwrd(ln,8); count=substr(wd,1,4)
    if ( count>0 )
      if( i <=5); 
        'set strsiz 0.13 0.13'; yt=yt-yy
        'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
        'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i'  ' mean'  'count
      else
        'set strsiz 0.13 0.13'; zyt=zyt-yy
        'set line 'cco.i' 'cst.i' 11'; 'draw line 'zxt' 'zyt' 'zxt1' 'zyt
        'set string 'cco.i' bl 6';     'draw string 'zxt2' 'zyt' 'mdc.i'  ' mean'  'count
      endif

      'set gxout line'
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
*     'set vrange 'cmin' 'cmax; 'set ylint 'cint; 'set xlint $xtick'
      'set vrange 0.2 1';        'set ylint 0.1'; 'set xlint $xtick'
      'd cor(x='%i')'
    endif
   i=i+1
   endwhile

  'set string 1 bc 7'
  'set strsiz 0.16 0.16'
  'draw string 'titlx' 'titly' Anomaly Correl: ${vnam} ${lev}, ${regdef}, ${cyc}Z cycle, fh'%fhr
  'set strsiz 0.15 0.15'
  'draw string 'xlabx' 'xlaby' Verification Date'

  'printim cor_fh'%fhr'_${namedaily}.png x800 y800'
  'set vpage off'
*--------
if (fhr<24 | $fhout > 6 )
 nf=nf+1
else
 nf=nf+2
endif
endwhile
*-------
'quit'
EOF1
##grads -bcp "run acz_${outname1}.gs"


# ----- PLOT TYPE 2:  Die-off plot for mean correlation over $ndays days----
ndaysp1=`expr $ndays + 1`
cat >cordieoff_${outname1}.gs <<EOF1 
'reinit'; 'set font 1'
'open ${outname1}.ctl'
mdc.1=${mdnamec[0]}
if($nmd >1); mdc.2=${mdnamec[1]} ;endif
if($nmd >2); mdc.3=${mdnamec[2]} ;endif
if($nmd >3); mdc.4=${mdnamec[3]} ;endif
if($nmd >4); mdc.5=${mdnamec[4]} ;endif
if($nmd >5); mdc.6=${mdnamec[5]} ;endif
if($nmd >6); mdc.7=${mdnamec[6]} ;endif
if($nmd >7); mdc.8=${mdnamec[7]} ;endif
if($nmd >8); mdc.9=${mdnamec[8]} ;endif
if($nmd >9); mdc.10=${mdnamec[9]} ;endif

*-- define line styles and model names 
  cst.1=1; cst.2=1; cst.3=1; cst.4=1; cst.5=1; cst.6=1; cst.7=1; cst.8=1; cst.9=1; cst.10=1
  cth.1=12; cth.2=12; cth.3=9; cth.4=9; cth.5=9; cth.6=9; cth.7=9; cth.8=9; cth.9=9; cth.10=9
  cma.1=0; cma.2=0; cma.3=0; cma.4=0; cma.5=0; cma.6=0; cma.7=0; cma.8=0; cma.9=0; cma.10=0
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=9; cco.7=5; cco.8=6; cco.9=7; cco.10=15
  bar.1=30; bar.2=40; bar.3=50; bar.4=60; bar.5=70; bar.6=80; bar.7=90; bar.8=92; bar.9=94; bar.10=96

  'set x 1' 
  'set t $ndaysp1' 
  'set y 1 ${fnum}'
  xwd=7.0; ywd=5.0; yy=ywd/17
  xmin=0.8; xmax=xmin+xwd; ymin=5.0; ymax=ymin+ywd
  xt=xmin+0.3; xt1=xt+0.5; xt2=xt1+0.1; yt=ymin+0.80*ywd ;*for legend  
  titlx=xmin+0.5*xwd;  titly=ymax+0.20                  ;*for tytle
  xlabx=xmin+0.45*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

*---------------------------------------------------------

   i=1
   while (i <= $nmd)
    'set gxout stat'  
    'd cor(x='%i')'  
    ln=sublin(result,7); wd=subwrd(ln,8); count=substr(wd,1,4)
    if ( count > 1 )
      'set strsiz 0.13 0.13'; yt=yt-yy
      'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
      'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i

      'set gxout line'
      'set mproj off'
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.0'
      if ($nmd <= 1); 'set xlopts 1 6 0.14' ; endif
      'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 0.21 1.0'; 'set ylint 0.1'; 'set xlint 12'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'd cor(x='%i')'  
    endif

   i=i+1
   endwhile

  'set string 1 bc 6'
  'set strsiz 0.13 0.13'
  'draw string 'titlx' 'titly' AC: ${vnam} ${lev}, ${regdef}, ${cyc}Z cyc, $sdate-$edate'
   xlabx=xmin+0.45*xwd;  xlaby=ymin-0.60
   if( $nmd <=1 ); 'draw string 'xlabx' 'xlaby' Forecast Hour' ;endif

*---------------------------------------------------------
if ( $nmd > 1 )
*---------------------------------------------------------
* plot AC difference between others and the first
  ymin=ymin-4; ymax=ymin+4
  'set parea 'xmin' 'xmax' 'ymin' 'ymax
  xlabx=xmin+0.45*xwd;  xlaby=ymin-0.60

*--find maximum and minmum values to determine y-axis labels
 cmax=-1.0; cmin=1.0
 i=2
 while (i <= $nmd)
  'set gxout stat'  
  'd cor(x='%i')' 
  ln=sublin(result,7); wd=subwrd(ln,8); count=substr(wd,1,4)
  if ( count > 1 )
    'set gxout stat'
    'd cor(x='%i')-cor(x=1)' 
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > cmax); cmax=zmax; endif
    if(zmin < cmin); cmin=zmin; endif
  endif
 i=i+1
 endwhile
 dist=cmax-cmin 
 if (dist = 0); dist=1; endif
 cmin=cmin-0.01*dist; cmax=cmax+0.01*dist
 cmin=substr(cmin,1,6); cmax=substr(cmax,1,6)
 cintp=10*substr(dist/40,1,4)
 if (cintp = 0); cintp=10*substr(dist/40,1,5); endif
 if (cintp = 0); cintp=10*substr(dist/40,1,6); endif

 
*---------------------------------------------------------
* compute standard deviation of the difference between the 
* first and each of the rest models for each forecast hour 

 i=2
 while (i <= $nmd)
 'set gxout stat'  
 'd cor(x='%i')'    
 ln=sublin(result,7); wd=subwrd(ln,8); count=substr(wd,1,4)
 if ( count > 1 )
   'define acdm=ave(cor(x='%i')-cor(x=1),t=1,t=$ndays)'
**      standard deviation of difference
   'define std=sqrt(ave((cor(x='%i')-cor(x=1)-acdm)*(cor(x='%i')-cor(x=1)-acdm),t=1,t=$ndays))'
**      Null Hypothesis: mean(AC1-AC2)=0, AC1-AC2 follows normal distribution.
**      plot the 5% conf interval of difference of means : F*SD/sqrt(N-1),
**      F=1.96 for infinite samples, F=2.0 for nsz=60, F=2.042 for nsz=30, F=2.228 for nsz=10
    if($ndays>=80);               'define intvl=1.960*std/sqrt($ndays-1)'  ;endif
    if($ndays>=40 & $ndays <80);  'define intvl=2.000*std/sqrt($ndays-1)'  ;endif
    if($ndays>=20 & $ndays <40);  'define intvl=2.042*std/sqrt($ndays-1)'  ;endif
    if($ndays<20);                'define intvl=2.228*std/sqrt($ndays-1)'  ;endif

   'set gxout bar'
   'set bargap 'bar.i
   'set baropts outline'
   'set ccolor 'cco.i
   'set cstyle 1'; 'set cthick 3'; 'set cmark 0'
   'set mproj off'
   'set display color white'; 'set grads off'; 'set grid on'
   'set xlopts 1 6 0.0';     'set ylopts 1 6 0.0'; 'set clopts 1 6 0.0'
   'set vrange 'cmin' 'cmax; 'set ylint 'cintp; 'set xlint 12'
   'd -intvl;intvl'
  endif
 i=i+1
 endwhile

 i=1
 while (i <= $nmd)
  'set gxout stat'  
  'd cor(x='%i')'  
  ln=sublin(result,7); wd=subwrd(ln,8); count=substr(wd,1,4)
  if ( count > 0 )
     'set gxout line'
     'set mproj off'
     'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
     'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
     'set vrange 'cmin' 'cmax; 'set ylint 'cintp; 'set xlint 12'
     'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
     if(i=1); 'set cstyle 1'; 'set cthick 1'; 'set cmark 0'; 'set ccolor 1'; endif
     'd cor(x='%i')-cor(x=1)'
  endif
 i=i+1
 endwhile

 'set strsiz 0.14 0.14'
 'draw string 'xlabx' 'xlaby' Forecast Hour'
 'set string 1 bl 6'
 'set strsiz 0.14 0.14'
 'draw string 'xmin+0.2' 'ymax-0.4' Difference w.r.t. 'mdc.1 
 'set string 1 bl 3'
 'set strsiz 0.11 0.11'
 if( $nmd > 1)
  'draw string 'xmin+0.1' 'ymin+0.3' AC differences outside of outline bars '
  'draw string 'xmin+0.1' 'ymin+0.1' are significant at the 95% confidence level'
 endif
*---------------------------------------------------------
endif
*---------------------------------------------------------
  'printim cordieoff_${namedaily}.png x700 y700'
  'set vpage off'
'quit'
EOF1
##grads -bcp "run cordieoff_${outname1}.gs"




# ----- PLOT TYPE 3:  time series of fcst and obs
cat >fo_${outname1}.gs <<EOF1 
'reinit'; 'set font 1'
'open ${outname1}.ctl'
mdc.1=${mdnamec[0]}
if($nmd >1); mdc.2=${mdnamec[1]} ;endif
if($nmd >2); mdc.3=${mdnamec[2]} ;endif
if($nmd >3); mdc.4=${mdnamec[3]} ;endif
if($nmd >4); mdc.5=${mdnamec[4]} ;endif
if($nmd >5); mdc.6=${mdnamec[5]} ;endif
if($nmd >6); mdc.7=${mdnamec[6]} ;endif
if($nmd >7); mdc.8=${mdnamec[7]} ;endif
if($nmd >8); mdc.9=${mdnamec[8]} ;endif
if($nmd >9); mdc.10=${mdnamec[9]} ;endif

*-- define line styles and model names 
* csto=1; cst.1=5; cst.2=5; cst.3=3; cst.4=5; cst.5=5; cst.6=3; cst.7=5; cst.8=5; cst.9=5; cst.10=5
  csto=1; cst.1=1; cst.2=1; cst.3=1; cst.4=1; cst.5=1; cst.6=1; cst.7=1; cst.8=1; cst.9=1; cst.10=1
  ctho=10;cth.1=10; cth.2=10; cth.3=9; cth.4=9; cth.5=9; cth.6=9; cth.7=9; cth.8=9; cth.9=9; cth.10=9
* cmao=0; cma.1=2; cma.2=8; cma.3=6; cma.4=1; cma.5=2; cma.6=0; cma.7=4; cma.8=3; cma.9=4; cma.10=5
  cmao=0; cma.1=0; cma.2=0; cma.3=0; cma.4=0; cma.5=0; cma.6=0; cma.7=0; cma.8=0; cma.9=0; cma.10=0
  ccoo=1; cco.1=2; cco.2=4; cco.3=3; cco.4=8; cco.5=9; cco.6=5; cco.7=6; cco.8=7; cco.9=15; cco.10=10
*------------------------
nf=1 ;*start from fcst00
while ( nf <  ${fnum} )
fhr=(nf-1)*${fhout}
*------------------------
  'c'
  'set x 1' 
  'set t 1 $ndays' 
  'set y '%nf 

  'define zero=0'

  xwd=7.0; ywd=5.0; yy=ywd/16
  xmin=1.0; xmax=xmin+xwd; ymin=5.0; ymax=ymin+ywd
  xt=xmin+0.3; xt1=xt+0.5; xt2=xt1+0.1; yt=ymax-0.01*ywd ;*for legend  
  zxt=xt2+3.0; zxt1=zxt+0.5; zxt2=zxt1+0.1; zyt=ymax-0.01*ywd
  titlx=xmin+0.5*xwd;  titly=ymax+0.20                  ;*for tytle
  xlabx=xmin+0.45*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

*--find maximum and minmum values
   cmax=-10000000.0; cmin=10000000.0
   i=1
   while (i <= $nmd)
    'set gxout stat'
    'd fcst(x='%i')-obs(x='%i')'
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    ln=sublin(result,7); wd=subwrd(ln,8); count=substr(wd,1,4)
    if( count>0 )
     if(zmax > cmax); cmax=zmax; endif
     if(zmin < cmin); cmin=zmin; endif
    endif
   i=i+1
   endwhile
   dist=cmax-cmin; cmax=cmax+0.05*dist
   cmin=substr(cmin,1,10); cmax=substr(cmax,1,10); cint=10*substr((cmax-cmin)/100,1,3)
   if (cint = 0); cint=substr((cmax-cmin)/10,1,3); endif
   if (cint = 0); cint=0.1*substr((cmax-cmin),1,3); endif
   if (cint = 0); cint=0.01*substr((cmax-cmin)*10,1,3); endif
   if (cint = 0); cint=0.001*substr((cmax-cmin)*100,1,3); endif
   if (cint = 0); cint=0.0001*substr((cmax-cmin)*1000,1,3); endif
   if (cint = 0); cint=0.00001*substr((cmax-cmin)*10000,1,3); endif
   say 'cmin cmax cint 'cmin' 'cmax' 'cint

   i=1
   while (i <= $nmd)
    'set gxout stat'  ;* first compute means and count good data numbers
    'd fcst(x='%i')-obs(x='%i')'
    ln=sublin(result,11); wd=subwrd(ln,2); mean=substr(wd,1,5)
    ln=sublin(result,7); wd=subwrd(ln,8); count=substr(wd,1,4)
    if ( count>0 )
      if( i <=4); 
        'set strsiz 0.13 0.13'; yt=yt-yy
        'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
        'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i'  'mean'  'count
      else
        'set strsiz 0.13 0.13'; zyt=zyt-yy
        'set line 'cco.i' 'cst.i' 11'; 'draw line 'zxt' 'zyt' 'zxt1' 'zyt
        'set string 'cco.i' bl 6';     'draw string 'zxt2' 'zyt' 'mdc.i'  'mean'  'count
      endif

      'set gxout line'
      'set display color white'; 'set missconn off';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set cstyle 'cst.i; 'set cthick 2'; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'set vrange 'cmin' 'cmax; 'set ylint 'cint; 'set xlint $xtick'
      'd fcst(x='%i')-obs(x='%i')'

      'set gxout line'
      'set display color white'; 'set missconn off';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set cstyle 'cst.i; 'set cthick 16'; 'set cmark 0'; 'set ccolor 'cco.i
      'set vrange 'cmin' 'cmax; 'set ylint 'cint; 'set xlint $xtick'
      'd tloop(ave(fcst(x='%i')-obs(x='%i'),t-15,t+15))'

      'set gxout line'
      'set cstyle 1'; 'set cthick 6'; 'set cmark 0'; 'set ccolor 1'
      'set vrange 'cmin' 'cmax; 'set ylint 'cint; 'set xlint $xtick'
      'd zero'                                            

    endif
   i=i+1
   endwhile

  'set string 1 bc 7'
  'set strsiz 0.16 0.16'
* 'draw string 'titlx' 'titly' ${vnam} ${lev}, ${regdef}, ${cyc}Z cycle, fh'%fhr
  'draw string 'titlx' 'titly' ${vnam} ${lev} Bias, ${regdef}, ${cyc}Z cycle, fh'%fhr
  'set strsiz 0.15 0.15'
  'draw string 'xlabx' 'xlaby' Verification Date'

  'printim fo_fh'%fhr'_${namedaily}.png x800 y800'
  'set vpage off'
*--------
if (fhr<24 | $fhout > 6 )
 nf=nf+1
else
 nf=nf+2
endif
endwhile
*-------
'quit'
EOF1
grads -bcp "run fo_${outname1}.gs"


# ----- PLOT TYPE 4:  mean obs and fcsts over $ndays days----
ndaysp1=`expr $ndays + 1`
cat >fom_${outname1}.gs <<EOF1 
'reinit'; 'set font 1'
'open ${outname1}.ctl'
mdc.1=${mdnamec[0]}
if($nmd >1); mdc.2=${mdnamec[1]} ;endif
if($nmd >2); mdc.3=${mdnamec[2]} ;endif
if($nmd >3); mdc.4=${mdnamec[3]} ;endif
if($nmd >4); mdc.5=${mdnamec[4]} ;endif
if($nmd >5); mdc.6=${mdnamec[5]} ;endif
if($nmd >6); mdc.7=${mdnamec[6]} ;endif
if($nmd >7); mdc.8=${mdnamec[7]} ;endif
if($nmd >8); mdc.9=${mdnamec[8]} ;endif
if($nmd >9); mdc.10=${mdnamec[9]} ;endif

*-- define line styles and model names 
  csto=1; cst.1=1; cst.2=1; cst.3=1; cst.4=1; cst.5=1; cst.6=1; cst.7=1; cst.8=1; cst.9=1; cst.10=1
  ctho=12;cth.1=12; cth.2=10; cth.3=9; cth.4=9; cth.5=9; cth.6=9; cth.7=9; cth.8=9; cth.9=9; cth.10=9
  cmao=0; cma.1=2; cma.2=8; cma.3=6; cma.4=1; cma.5=2; cma.6=0; cma.7=4; cma.8=3; cma.9=4; cma.10=5
  ccoo=1; cco.1=2; cco.2=3; cco.3=4; cco.4=8; cco.5=9; cco.6=5; cco.7=6; cco.8=7; cco.9=15; cco.10=10
          bar.1=30; bar.2=40; bar.3=50; bar.4=60; bar.5=70; bar.6=80; bar.7=90; bar.8=92; bar.9=94; bar.10=96

  'set x 1' 
  'set t $ndaysp1' 
  'set y 1 ${fnum}'
  xwd=7.0; ywd=5.0; yy=ywd/17; xx=xwd/5
  xmin=0.8; xmax=xmin+xwd; ymin=5.0; ymax=ymin+ywd
  xt=xmin+0.3; yt=ymin+0.95*ywd ;*for legend  
  titlx=xmin+0.5*xwd;  titly=ymax+0.20                  ;*for tytle
  xlabx=xmin+0.45*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax


*--find maximum and minmum values
   cmax=-10000000.0; cmin=10000000.0
    'set gxout stat'
    'd obs(x=1)'
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if( b>0 )
     if(zmax > cmax); cmax=zmax; endif
     if(zmin < cmin); cmin=zmin; endif
    endif
   i=1
   while (i <= $nmd)
    'set gxout stat'
    'd fcst(x='%i')'
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if( b>0 )
     if(zmax > cmax); cmax=zmax; endif
     if(zmin < cmin); cmin=zmin; endif
    endif
   i=i+1
   endwhile
   dist=cmax-cmin
   if(cmax>0)
     if( $vnam = SLP )
      cmax=1.001*cmax 
     else
      cmax=1.04*cmax 
     endif
   endif
   cmin=substr(cmin,1,10); cmax=substr(cmax,1,10); cint=10*substr((cmax-cmin)/100,1,3)
   if (cint = 0); cint=substr((cmax-cmin)/10,1,3); endif
   if (cint = 0); cint=0.1*substr((cmax-cmin),1,3); endif
   if (cint = 0); cint=0.01*substr((cmax-cmin)*10,1,3); endif
   if (cint = 0); cint=0.001*substr((cmax-cmin)*100,1,3); endif
   if (cint = 0); cint=0.0001*substr((cmax-cmin)*1000,1,3); endif
   if (cint = 0); cint=0.00001*substr((cmax-cmin)*10000,1,3); endif
   say 'cmin cmax cint 'cmin' 'cmax' 'cint

*---------------------------------------------------------
*first plot observations (assume obs are the same for all models/experiments)
   i=1
    'set gxout stat'  
    'd obs(x='%i')'
    ln=sublin(result,7); wd=subwrd(ln,8); count=substr(wd,1,4)
    if ( bo>0 )
      'set strsiz 0.14 0.14'; yt=yt; xt=xt; xt1=xt+0.3; xt2=xt1+0.1
      'set line 'ccoo' 'csto' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
*     'set string 'ccoo' bl 6';     'draw string 'xt2' 'yt' obs  'count
      'set string 'ccoo' bl 6';     'draw string 'xt2' 'yt' obs  '

      'set gxout line'
      'set mproj off'
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.0'
      if ($nmd <= 1); 'set xlopts 1 6 0.14' ; endif
      'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set cstyle 'csto; 'set cthick 'ctho; 'set cmark 'cmao; 'set ccolor 'ccoo
      'set vrange 'cmin' 'cmax; 'set ylint 'cint; 'set xlint 12'
      'd obs(x='%i')'
    endif

*then plot forecasts
   i=1
   while (i <= $nmd)
    'set gxout stat'  
    'd fcst(x='%i')'  
    ln=sublin(result,7); wd=subwrd(ln,8); count=substr(wd,1,4)
    if ( count > 1 )
      if (i < 4 )
       'set strsiz 0.13 0.13'; xt=xt+xx; xt1=xt+0.3; xt2=xt1+0.1
      else
       if(i=4); xt=xmin+0.3; yt=yt-yy; endif
       'set strsiz 0.13 0.13'; xt=xt+xx; xt1=xt+0.3; xt2=xt1+0.1
      endif
      'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
*     'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i'  'count
      'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i

      'set gxout line'
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.0'
      if ($nmd <= 1); 'set xlopts 1 6 0.14' ; endif
      'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'set vrange 'cmin' 'cmax; 'set ylint 'cint; 'set xlint 12'
      'd fcst(x='%i')'
    endif
   i=i+1
   endwhile

  'set string 1 bc 6'
  'set strsiz 0.13 0.13'
  'draw string 'titlx' 'titly' ${vnam} ${lev}, ${regdef}, ${cyc}Z Cycle, $sdate-$edate Mean '
   xlabx=xmin+0.45*xwd;  xlaby=ymin-0.60
   if( $nmd <=1 ); 'draw string 'xlabx' 'xlaby' Forecast Hour' ;endif

*---------------------------------------------------------
if ( $nmd > 0 )
*---------------------------------------------------------
* plot differences between forecasts and observations        
  ymin=ymin-4; ymax=ymin+4
  'set parea 'xmin' 'xmax' 'ymin' 'ymax
  xlabx=xmin+0.45*xwd;  xlaby=ymin-0.60

*--find maximum and minmum values to determine y-axis labels
 cmax=-10000000.0; cmin=10000000.0
 i=1
 while (i <= $nmd)
  'set gxout stat'  
  'd fcst(x='%i')' 
  ln=sublin(result,7); wd=subwrd(ln,8); count=substr(wd,1,4)
  if ( count > 1 )
    'set gxout stat'
    'd fcst(x='%i')-obs(x='%i')' 
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > cmax); cmax=zmax; endif
    if(zmin < cmin); cmin=zmin; endif
  endif
 i=i+1
 endwhile
 if ( cmax < 0); cmax=0;endif
 if ( cmin > 0); cmin=0;endif
 cmin=substr(cmin,1,10); cmax=substr(cmax,1,10)
 cintp=10*substr((cmax-cmin)/100,1,4)
 if (cintp = 0); cintp=substr((cmax-cmin)/10,1,4); endif
 if (cintp = 0); cintp=0.1*substr((cmax-cmin),1,4); endif
 if (cintp = 0); cintp=0.01*substr((cmax-cmin)*10,1,4); endif
 if (cintp = 0); cintp=0.001*substr((cmax-cmin)*100,1,4); endif
 if (cintp = 0); cintp=0.0001*substr((cmax-cmin)*1000,1,4); endif
 if (cintp = 0); cintp=0.00001*substr((cmax-cmin)*10000,1,4); endif

*---------------------------------------------------------
* compute standard deviation of the difference between the 
* first and each of the rest models for each forecast hour 

 i=1
 while (i <= $nmd)
 'set gxout stat'  
 'd fcst(x='%i')'    
 ln=sublin(result,7); wd=subwrd(ln,8); count=substr(wd,1,4)
 if ( count > 1 )
   'define acdm=ave(fcst(x='%i')-obs(x='%i'),t=1,t=$ndays)'
**      standard deviation of difference
   'define std=sqrt(ave((fcst(x='%i')-obs(x='%i')-acdm)*(fcst(x='%i')-obs(x='%i')-acdm),t=1,t=$ndays))'
**      Null Hypothesis: mean(AC1-AC2)=0, AC1-AC2 follows normal distribution.
**      plot the 5% conf interval of difference of means : F*SD/sqrt(N-1),
**      F=1.96 for infinite samples, F=2.0 for nsz=60, F=2.042 for nsz=30, F=2.228 for nsz=10
    if($ndays>=80);               'define intvl=1.960*std/sqrt($ndays-1)'  ;endif
    if($ndays>=40 & $ndays <80);  'define intvl=2.000*std/sqrt($ndays-1)'  ;endif
    if($ndays>=20 & $ndays <40);  'define intvl=2.042*std/sqrt($ndays-1)'  ;endif
    if($ndays<20);                'define intvl=2.228*std/sqrt($ndays-1)'  ;endif

   'set gxout bar'
   'set bargap 'bar.i
   'set baropts outline'
   'set ccolor 'cco.i
   'set cstyle 1'; 'set cthick 3'; 'set cmark 0'
   'set mproj off'
   'set display color white'; 'set grads off'; 'set grid on'
   'set xlopts 1 6 0.0';     'set ylopts 1 6 0.0'; 'set clopts 1 6 0.0'
   'set vrange 'cmin' 'cmax; 'set ylint 'cintp; 'set xlint 12'
   'd -intvl;intvl'
  endif
 i=i+1
 endwhile

 i=1
 while (i <= $nmd)
  'set gxout stat'  
  'd fcst(x='%i')'  
  ln=sublin(result,7); wd=subwrd(ln,8); count=substr(wd,1,4)
  if ( count > 0 )
     'set gxout line'
     'set mproj off'
     'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
     'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
     'set vrange 'cmin' 'cmax; 'set ylint 'cintp; 'set xlint 12'
     'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
*    if(i=1); 'set cstyle 1'; 'set cthick 1'; 'set cmark 0'; 'set ccolor 1'; endif
     'd fcst(x='%i')-obs(x='%i')'
  endif
 i=i+1
 endwhile

 'set strsiz 0.14 0.14'
 'draw string 'xlabx' 'xlaby' Forecast Hour'
 'set string 1 bl 6'
 'set strsiz 0.14 0.14'
 'draw string 'xmin+0.2' 'ymax-0.4' Difference w.r.t. obs'    
 'set string 1 bl 3'
 'set strsiz 0.11 0.11'
 if( $nmd > 1)
  'draw string 'xmin+0.1' 'ymin+0.3' Differences outside of outline bars '
  'draw string 'xmin+0.1' 'ymin+0.1' are significant at the 95% confidence level'
 endif
*---------------------------------------------------------
endif             
*---------------------------------------------------------
  'printim fom_${namedaily}.png x700 y700'
  'set vpage off'
'quit'
EOF1
grads -bcp "run fom_${outname1}.gs"
*---------------------------------------------------------



# ----- PLOT TYPE 5:  time series of rms  ----
cat >rms_${outname1}.gs <<EOF1 
'reinit'; 'set font 1'
'open ${outname1}.ctl'
mdc.1=${mdnamec[0]}
if($nmd >1); mdc.2=${mdnamec[1]} ;endif
if($nmd >2); mdc.3=${mdnamec[2]} ;endif
if($nmd >3); mdc.4=${mdnamec[3]} ;endif
if($nmd >4); mdc.5=${mdnamec[4]} ;endif
if($nmd >5); mdc.6=${mdnamec[5]} ;endif
if($nmd >6); mdc.7=${mdnamec[6]} ;endif
if($nmd >7); mdc.8=${mdnamec[7]} ;endif
if($nmd >8); mdc.9=${mdnamec[8]} ;endif
if($nmd >9); mdc.10=${mdnamec[9]} ;endif

*-- define line styles and model names 
  cst.1=1; cst.2=5; cst.3=3; cst.4=5; cst.5=5; cst.6=3; cst.7=5; cst.8=5; cst.9=5; cst.10=5
  cth.1=10; cth.2=10; cth.3=9; cth.4=9; cth.5=9; cth.6=9; cth.7=9; cth.8=9; cth.9=9; cth.10=9
  cma.1=0; cma.2=8; cma.3=6; cma.4=1; cma.5=2; cma.6=0; cma.7=4; cma.8=3; cma.9=4; cma.10=5
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=9; cco.7=5; cco.8=6; cco.9=7; cco.10=15
*------------------------
nf=1 ;*start from fcst00
while ( nf <  ${fnum} )
fhr=(nf-1)*${fhout}
*------------------------
  'c'
  'set x 1' 
  'set t 1 $ndays' 
  'set y '%nf 

  xwd=7.0; ywd=5.0; yy=ywd/16
  xmin=1.0; xmax=xmin+xwd; ymin=6.0; ymax=ymin+ywd
  xt=xmin+0.3; xt1=xt+0.5; xt2=xt1+0.1; yt=ymax-0.01*ywd ;*for legend  
  zxt=xt2+3.0; zxt1=zxt+0.5; zxt2=zxt1+0.1; zyt=ymax-0.01*ywd
  titlx=xmin+0.5*xwd;  titly=ymax+0.20                  ;*for tytle
  xlabx=xmin+0.45*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

*--find maximum and minmum values
   cmax=-10000000.0; cmin=10000000.0
   i=1
   while (i <= $nmd)
    'set gxout stat'
    'd rms(x='%i')'
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if( b>0 )
     if(zmax > cmax); cmax=zmax; endif
     if(zmin < cmin); cmin=zmin; endif
    endif
   i=i+1
   endwhile
   dist=cmax-cmin; cmax=1.04*cmax
   cmin=substr(cmin,1,10); cmax=substr(cmax,1,10); cint=10*substr((cmax-cmin)/100,1,3)
   if (cint = 0); cint=substr((cmax-cmin)/10,1,3); endif
   if (cint = 0); cint=0.1*substr((cmax-cmin),1,3); endif
   if (cint = 0); cint=0.01*substr((cmax-cmin)*10,1,3); endif
   if (cint = 0); cint=0.001*substr((cmax-cmin)*100,1,3); endif
   if (cint = 0); cint=0.0001*substr((cmax-cmin)*1000,1,3); endif
   if (cint = 0); cint=0.00001*substr((cmax-cmin)*10000,1,3); endif
   say 'cmin cmax cint 'cmin' 'cmax' 'cint


   i=1
   while (i <= $nmd)
    'set gxout stat'  ;* first compute means and count good data numbers
    'd rms(x='%i')'
    ln=sublin(result,11); wd=subwrd(ln,2); mean=substr(wd,1,5)
    ln=sublin(result,7); wd=subwrd(ln,8); count=substr(wd,1,4)
    if ( count>0 )
      if( i <=4); 
        'set strsiz 0.13 0.13'; yt=yt-yy
        'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
        'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i'  ' mean'  'count
      else
        'set strsiz 0.13 0.13'; zyt=zyt-yy
        'set line 'cco.i' 'cst.i' 11'; 'draw line 'zxt' 'zyt' 'zxt1' 'zyt
        'set string 'cco.i' bl 6';     'draw string 'zxt2' 'zyt' 'mdc.i'  ' mean'  'count
      endif

      'set gxout line'
      'set display color white'; 'set missconn off';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'set vrange 'cmin' 'cmax; 'set ylint 'cint; 'set xlint $xtick'
      'd rms(x='%i')'

    endif
   i=i+1
   endwhile

  'set string 1 bc 7'
  'set strsiz 0.16 0.16'
  'draw string 'titlx' 'titly' RMSE: ${vnam} ${lev}, ${regdef}, ${cyc}Z cycle, fh'%fhr
  'set strsiz 0.15 0.15'
  'draw string 'xlabx' 'xlaby' Verification Date'

  'printim rms_fh'%fhr'_${namedaily}.png x800 y800'
  'set vpage off'
*--------
 if (fhr<24 | $fhout > 6 )
  nf=nf+1
 else
  nf=nf+2
 endif
endwhile
*-------
'quit'
EOF1
grads -bcp "run rms_${outname1}.gs"
*---------------------------------------------------------


# ----- PLOT TYPE 6:  Die-off plot for mean rms over $ndays days----
ndaysp1=`expr $ndays + 1`
cat >rmsdieoff_${outname1}.gs <<EOF1 
'reinit'; 'set font 1'
'open ${outname1}.ctl'
mdc.1=${mdnamec[0]}
if($nmd >1); mdc.2=${mdnamec[1]} ;endif
if($nmd >2); mdc.3=${mdnamec[2]} ;endif
if($nmd >3); mdc.4=${mdnamec[3]} ;endif
if($nmd >4); mdc.5=${mdnamec[4]} ;endif
if($nmd >5); mdc.6=${mdnamec[5]} ;endif
if($nmd >6); mdc.7=${mdnamec[6]} ;endif
if($nmd >7); mdc.8=${mdnamec[7]} ;endif
if($nmd >8); mdc.9=${mdnamec[8]} ;endif
if($nmd >9); mdc.10=${mdnamec[9]} ;endif

*-- define line styles and model names 
  cst.1=1; cst.2=1; cst.3=1; cst.4=1; cst.5=1; cst.6=1; cst.7=1; cst.8=1; cst.9=1; cst.10=1
  cth.1=12; cth.2=12; cth.3=9; cth.4=9; cth.5=9; cth.6=9; cth.7=9; cth.8=9; cth.9=9; cth.10=9
  cma.1=0; cma.2=0; cma.3=0; cma.4=0; cma.5=0; cma.6=0; cma.7=0; cma.8=0; cma.9=0; cma.10=0
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=9; cco.7=5; cco.8=6; cco.9=7; cco.10=15
  bar.1=30; bar.2=40; bar.3=50; bar.4=60; bar.5=70; bar.6=80; bar.7=90; bar.8=92; bar.9=94; bar.10=96

  'set x 1' 
  'set t $ndaysp1' 
  'set y 1 ${fnum}'
  xwd=7.0; ywd=5.0; yy=ywd/17
  xmin=0.8; xmax=xmin+xwd; ymin=5.0; ymax=ymin+ywd
  xt=xmin+0.3; xt1=xt+0.5; xt2=xt1+0.1; yt=ymin+0.80*ywd ;*for legend  
  titlx=xmin+0.5*xwd;  titly=ymax+0.20                  ;*for tytle
  xlabx=xmin+0.45*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

*---------------------------------------------------------
*--find maximum and minmum values
   cmax=-10000000.0; cmin=10000000.0
   i=1
   while (i <= $nmd)
    'set gxout stat'
    'd rms(x='%i')'
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if( b>0 )
     if(zmax > cmax); cmax=zmax; endif
     if(zmin < cmin); cmin=zmin; endif
    endif
   i=i+1
   endwhile
   dist=cmax-cmin; cmax=1.04*cmax
   cmin=substr(cmin,1,10); cmax=substr(cmax,1,10); cint=10*substr((cmax-cmin)/100,1,3)
   if (cint = 0); cint=substr((cmax-cmin)/10,1,3); endif
   if (cint = 0); cint=0.1*substr((cmax-cmin),1,3); endif
   if (cint = 0); cint=0.01*substr((cmax-cmin)*10,1,3); endif
   if (cint = 0); cint=0.001*substr((cmax-cmin)*100,1,3); endif
   if (cint = 0); cint=0.0001*substr((cmax-cmin)*1000,1,3); endif
   if (cint = 0); cint=0.00001*substr((cmax-cmin)*10000,1,3); endif
   say 'cmin cmax cint 'cmin' 'cmax' 'cint


   i=1
   while (i <= $nmd)
    'set gxout stat'  
    'd rms(x='%i')'  
    ln=sublin(result,7); wd=subwrd(ln,8); count=substr(wd,1,4)
    if ( count > 1 )
      'set strsiz 0.13 0.13'; yt=yt-yy
      'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
      'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i

      'set gxout line'
      'set mproj off'
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.0'
      if ($nmd <= 1); 'set xlopts 1 6 0.14' ; endif
      'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 'cmin' 'cmax; 'set ylint 'cint; 'set xlint 12'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'd rms(x='%i')'  
    endif

   i=i+1
   endwhile

  'set string 1 bc 6'
  'set strsiz 0.13 0.13'
  'draw string 'titlx' 'titly' RMS: ${vnam} ${lev}, ${regdef}, ${cyc}Z cyc, $sdate-$edate'
   xlabx=xmin+0.45*xwd;  xlaby=ymin-0.60
   if( $nmd <=1 ); 'draw string 'xlabx' 'xlaby' Forecast Hour' ;endif

*---------------------------------------------------------
if ( $nmd > 1 )
*---------------------------------------------------------
* plot RMSE difference between others and the first
  ymin=ymin-4; ymax=ymin+4
  'set parea 'xmin' 'xmax' 'ymin' 'ymax
  xlabx=xmin+0.45*xwd;  xlaby=ymin-0.60

*--find maximum and minmum values to determine y-axis labels
 cmax=-10000000.0; cmin=10000000.0
 i=2
 while (i <= $nmd)
  'set gxout stat'
  'd rms(x='%i')'
  ln=sublin(result,7); wd=subwrd(ln,8); count=substr(wd,1,4)
  if ( count > 1 )
    'set gxout stat'
    'd rms(x='%i')-rms(x=1)'
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > cmax); cmax=zmax; endif
    if(zmin < cmin); cmin=zmin; endif
  endif
 i=i+1
 endwhile
 if ( cmax < 0); cmax=0;endif
 if ( cmin > 0); cmin=0;endif
 cmin=substr(cmin,1,10); cmax=substr(cmax,1,10)
 cintp=10*substr((cmax-cmin)/100,1,4)
 if (cintp = 0); cintp=substr((cmax-cmin)/10,1,4); endif
 if (cintp = 0); cintp=0.1*substr((cmax-cmin),1,4); endif
 if (cintp = 0); cintp=0.01*substr((cmax-cmin)*10,1,4); endif
 if (cintp = 0); cintp=0.001*substr((cmax-cmin)*100,1,4); endif
 if (cintp = 0); cintp=0.0001*substr((cmax-cmin)*1000,1,4); endif
 if (cintp = 0); cintp=0.00001*substr((cmax-cmin)*10000,1,4); endif

*---------------------------------------------------------
* compute standard deviation of the difference between the 
* first and each of the rest models for each forecast hour 

 i=2
 while (i <= $nmd)
 'set gxout stat'  
 'd rms(x='%i')'    
 ln=sublin(result,7); wd=subwrd(ln,8); count=substr(wd,1,4)
 if ( count > 1 )
   'define acdm=ave(cor(x='%i')-cor(x=1),t=1,t=$ndays)'
**      standard deviation of difference
   'define std=sqrt(ave((cor(x='%i')-cor(x=1)-acdm)*(cor(x='%i')-cor(x=1)-acdm),t=1,t=$ndays))'
**      Null Hypothesis: mean(AC1-AC2)=0, AC1-AC2 follows normal distribution.
**      plot the 5% conf interval of difference of means : F*SD/sqrt(N-1),
**      F=1.96 for infinite samples, F=2.0 for nsz=60, F=2.042 for nsz=30, F=2.228 for nsz=10
    if($ndays>=80);               'define intvl=1.960*std/sqrt($ndays-1)'  ;endif
    if($ndays>=40 & $ndays <80);  'define intvl=2.000*std/sqrt($ndays-1)'  ;endif
    if($ndays>=20 & $ndays <40);  'define intvl=2.042*std/sqrt($ndays-1)'  ;endif
    if($ndays<20);                'define intvl=2.228*std/sqrt($ndays-1)'  ;endif


   'set gxout bar'
   'set bargap 'bar.i
   'set baropts outline'
   'set ccolor 'cco.i
   'set cstyle 1'; 'set cthick 3'; 'set cmark 0'
   'set mproj off'
   'set display color white'; 'set grads off'; 'set grid on'
   'set xlopts 1 6 0.0';     'set ylopts 1 6 0.0'; 'set clopts 1 6 0.0'
   'set vrange 'cmin' 'cmax; 'set ylint 'cintp; 'set xlint 12'
   'd -intvl;intvl'
  endif
 i=i+1
 endwhile

 i=1
 while (i <= $nmd)
  'set gxout stat'  
  'd rms(x='%i')'  
  ln=sublin(result,7); wd=subwrd(ln,8); count=substr(wd,1,4)
  if ( count > 0 )
     'set gxout line'
     'set mproj off'
     'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
     'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
     'set vrange 'cmin' 'cmax; 'set ylint 'cintp; 'set xlint 12'
     'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
     if(i=1); 'set cstyle 1'; 'set cthick 1'; 'set cmark 0'; 'set ccolor 1'; endif
     'd rms(x='%i')-rms(x=1)'
  endif
 i=i+1
 endwhile

 'set strsiz 0.14 0.14'
 'draw string 'xlabx' 'xlaby' Forecast Hour'
 'set string 1 bl 6'
 'set strsiz 0.14 0.14'
 'draw string 'xmin+0.2' 'ymax-0.4' Difference w.r.t. 'mdc.1 
 'set string 1 bl 3'
 'set strsiz 0.11 0.11'
 if( $nmd > 1)
  'draw string 'xmin+0.1' 'ymin+0.3' RMSE differences outside of outline bars '
  'draw string 'xmin+0.1' 'ymin+0.1' are significant at the 95% confidence level'
 endif
*---------------------------------------------------------
endif             
*---------------------------------------------------------
  'printim rmsdieoff_${namedaily}.png x700 y700'
  'set vpage off'
'quit'
EOF1
grads -bcp "run rmsdieoff_${outname1}.gs"


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#--------------------
# remove whitesapce from.png files
# ImageMagic
#export PATH=$PATH:/usrx/local/imajik/bin
#export LIBPATH=/lib:/usr/lib
#export LIBPATH=$LIBPATH:/usrx/local/imajik/lib

chmod a+rw cor*.png fo*.png

#-------------------------------
  if [ $copymap = "YES" ]; then
    mkdir -p $mapdir/sfc
    cp fo*.png  $mapdir/sfc/.
    cp rms*.png  $mapdir/sfc/.
  fi
#------------------------------

## -- for daily rotatigng display
cat << EOF >ftpin
  binary
  promt
  cd $ftpdir/g2o/sfc
    mput fo*${namedaily}.png
    mput rms*${namedaily}.png
  quit
EOF
if [ $doftp = "YES" ]; then
cat <<EOF >ftpcard.sh
#!/bin/ksh
set -x
 cd $exedir
 sftp  ${webhostid}@${webhost} <ftpin
EOF
 chmod u+x ftpcard.sh
 sh +x $exedir/ftpcard.sh
 if [ $? -ne 0 ]; then
  $SUBJOB -a GFS-MTN -q $CUE2FTP -g $GROUP -p 1/1/S -t 0:30:00 -r 64/1 $exedir/ftpcard.sh
 fi
fi

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
done  ;# end lev
done  ;# end cyc
done  ;# end vnam
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------


exit

