#!/bin/ksh
set -x

#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#  This script make graphics using grid2obs upper air verification statistics
#  saved in vsdb format. It is aimed to compare vertical distributions of RMSE
#  and bias between forecasts and prepbufr ADPUPA observations (winds from rawinsonde, 
#  pibal and profilers and T, RH, and Q from rawinsonde and dropsonde), or ANYAIR 
#  observations (AIRCAR and AIRCFT etc),  over the globe and its subregions. 
#  Plots compare scores from all models/experiments for the same forecast cycle 
#  (e.g. 00Z)
#  Fanglin Yang, December 2011
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
## -- verification region
export regname=${regname:-${1:-g236}}
export reglist=${reglist:-"G236"} ;#G236,GNH,GSH,GTRP,GEUR,GASI,GAFR,GSA,GNA,GAUS,GGLB etc
export regdef=${regdef:-"CONUS"}                          

## -- verification dates
CDATE=$(date +%Y%m%d)
edate=${edate:-$CDATE}      ;#end of verification date
ndays=${ndays:-31}          ;#number of days back for verification
vlength=${vlength:-168}     ;#forecast length in hours to be verified
fhout=${fhout:-6}           ;#forecast output frequency in hours       
obtype=${obairtype:-ADPUPA} ;#observation type, ADPUPA or ANYAIR


## -- verification parameters
export cyclist=${cyclist:-"00"}
export vnamlist=${vnamlist:-"T Q RH VWND"}
export mdlist=${mdlist:-"gfs prd12q3k"}
export levlist0=${levlist:-"P1000 P925 P850 P700 P500 P400 P300 P250 P200 P150 P100 P50 P20 P10"}

export webhost=${webhost:-"emcrzdm.ncep.noaa.gov"}
export webhostid=${webhostid:-"$LOGNAME"}
export vsdb_data=${vsdb_data:-/climate/save/wx24fy/VRFY/vsdb_data/grid2obs}
export sorcdir=${sorcdir:-/global/save/wx24fy/VRFY/vsdb/grid2obs}
export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$webhostid/vsdb} 
export doftp=${doftp:-"YES"}                                    
export copymap=${copymap:-"NO"}                                
export tmpdir=${tmpdir:-/stmp/$LOGNAME/g2oplot/air}
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
if [ $obtype = ANYAIR ]; then
 export levlist0="P1000 P850 P700 P550 P400 P300 P250 P200 P150"
fi

for vnam in $vnamlist; do
   coeff=1.0; if [ $vnam = "Q" ]; then coeff=1.0 ;fi
   if [ $vnam = "Q" ]; then unit="(g/kg)" ;fi
   if [ $vnam = "T" ]; then unit="(K)" ;fi
   if [ $vnam = "RH" ]; then unit="(%)" ;fi
   if [ $vnam = "VWND" ]; then unit="(m/s)" ;fi

   export exedir=${tmpdir}/${vnam}
   if [ -s $exedir ]; then rm -rf $exedir; fi
   mkdir -p $exedir; cd $exedir || exit
   if [ $vnam = "RH" -o $vnam = "Q" ]; then
    export levlist="P1000 P925 P850 P700 P500 P400 P300"
   else
    export levlist="${levlist0}"
   fi
   nlev=`echo $levlist | wc -w` 

for cyc  in $cyclist ; do
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------

# -- generate output names 
  nhours=`expr $ndays \* 24 - 24`
  tmp=`$ndate -$nhours ${edate}00 `
  sdate=`echo $tmp | cut -c 1-8`
  outname1=${vnam}_${regname}_${cyc}Z${sdate}${edate}
  yyyymm=`echo $edate |cut -c 1-6`                                

# -- search data for all models; write out binary data, create grads control file
  if [ $vnam = "VWND" ]; then
   $sorcdir/scripts/g2o_airmap_wind.sh $obtype $vnam $regname "$reglist" "$levlist" $edate $ndays $cyc $vlength $fhout $outname1 $maskmiss "$mdlist"
  else
   $sorcdir/scripts/g2o_airmap_scal.sh $obtype $vnam $regname "$reglist" "$levlist" $edate $ndays $cyc $vlength $fhout $outname1 $maskmiss "$mdlist"
 fi

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# -- create grads scripts, allows up to 10 models 
nmd=`echo $mdlist | wc -w`  ;#count number of models
set -A mdname $mdlist
set -A mdnamec `echo $mdlist |tr "[a-z]" "[A-Z]" `
#namedaily=${vnam}_${regname}_${cyc}Z
namedaily=${vnam}_${regname}


#============================================
for var in rms bias ; do
#============================================

if [ $var = "rms" ];  then title="RMSE"         ;fi 
if [ $var = "bias" ]; then title="Bias"         ;fi 


# ----- PLOT TYPE 1:  maps of mean ${var} as a function of forecast time and pressure  ----
ndaysp1=`expr $ndays + 1`
cat >${var}mp_${outname1}.gs <<EOF1 
'reinit'; 'set font 1'
'run $sorcdir/grads/white.gs'
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

  nframe=$nmd
  xmin0=0.7;  xgap=0.2; xlen=3.5;  
  if ($nmd <= 1); xlen=7; endif
  ymax0=10.0; ygap=-0.1
               nframe2=1;  nframe3=2; ylen=-6.0
  if($nmd >2); nframe2=2;  nframe3=4; ylen=-4.2;endif
  if($nmd >4); nframe2=3;  nframe3=6; ylen=-2.8;endif
  if($nmd >6); nframe2=4;  nframe3=8; ylen=-2.1; endif
  if($nmd >8); nframe2=5;  nframe3=10; ylen=-1.7; endif

  'c'
  'set x 1'
  'set t $ndaysp1'
  'set y 1 ${fnum}'
  'set z 1 $nlev' 

*--find maximum and minmum values for first total field map
   cmax=-10000000.0; cmin=10000000.0
   i=1
   while (i <= $nmd)
    'set gxout stat'
    if ( $var = bias )
     'd ${coeff}*(fcst(x='%i')-obs(x='%i'))'
    else
     'd ${coeff}*${var}(x='%i')'
    endif
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > cmax); cmax=zmax; endif
    if(zmin < cmin); cmin=zmin; endif
   i=i+1
   endwhile
   dist=cmax-cmin; cmin=cmin-0.05*dist; cmax=cmax+0.05*dist
   cmin=substr(cmin,1,8); cmax=substr(cmax,1,8); cint=10*substr((cmax-cmin)/100,1,4)
   if (cint = 0); cint=substr((cmax-cmin)/10,1,4); endif
   if (cint = 0); cint=0.1*substr((cmax-cmin),1,4); endif
   if (cint = 0); cint=0.01*substr((cmax-cmin)*10,1,4); endif
   if (cint = 0); cint=0.001*substr((cmax-cmin)*100,1,4); endif
   if (cint = 0); cint=0.0001*substr((cmax-cmin)*1000,1,4); endif
   say 'cmin cmax cint 'cmin' 'cmax' 'cint
    bb1=cmin; bb2=cmin+cint; bb3=bb2+cint; bb4=bb3+cint; bb5=bb4+cint; bb6=bb5+cint
    bb7=bb6+cint; bb8=bb7+cint; bb9=bb8+cint; bb10=bb9+cint; bb11=bb10+cint

*--find maximum and minmum values for difference map
   cmax=-10000000.0; cmin=10000000.0
   i=2
   if( $var = bias ); i=1 ;endif
   while (i <= $nmd)
    'set gxout stat'
    if( $var = bias ) 
     'd ${coeff}*(fcst(x='%i')-obs(x='%i'))'
    else
     'd ${coeff}*(${var}(x='%i')-${var}(x=1))'
    endif
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if( b>0 )
     if(zmax > cmax); cmax=zmax; endif
     if(zmin < cmin); cmin=zmin; endif
    endif
   i=i+1
   endwhile
   if ( cmax >= -cmin); 
     cmin=-cmax
   else
     cmax=-cmin
   endif
** dist=cmax-cmin; cmin=cmin; cmax=cmax+0.1*dist
   cmin=substr(cmin,1,8); cmax=substr(cmax,1,8); 
   cintp=10*substr(cmax/50,1,4)
     if (cintp = 0); cintp=substr(cmax/5,1,4); endif
     if (cintp = 0); cintp=0.2*substr(cmax,1,4); endif
     if (cintp = 0); cintp=0.02*substr(cmax*10,1,4); endif
     if (cintp = 0); cintp=0.002*substr(cmax*100,1,4); endif
     if (cintp = 0); cintp=0.0002*substr(cmax*100,1,4); endif
     if (cintp = 0); cintp=0.00002*substr(cmax*100,1,4); endif
     if (cintp = 0); cintp=0.000002*substr(cmax*100,1,4); endif
   cp1=cintp; cp2=cp1+cintp; cp3=cp2+cintp; cp4=cp3+cintp; cp5=cp4+cintp
              cp6=cp5+cintp; cp7=cp6+cintp; cp8=cp7+cintp; cp9=cp8+cintp
   cm1=-cp1 ; cm2=-cp2     ; cm3=-cp3     ; cm4=-cp4     ; cm5=-cp5     
              cm6=-cp6     ; cm7=-cp7     ; cm8=-cp8     ; cm9=-cp9     
   say 'cmin cmax cintm cintp 'cmin' 'cmax' 'cintm' 'cintp


  i=1
  while ( i <= nframe )
  'set gxout stat'  ;*count good data numbers
   if( $var = bias ) 
    'd ${coeff}*(fcst(x='%i')-obs(x='%i'))'
   else
    'd ${coeff}*${var}(x='%i')'
   endif
  ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
  if ( b>0 )
    icx=1; if (i > nframe2); icx=2; endif
    if (i > nframe3); icx=3; endif
    if (i > nframe4); icx=4; endif
    xmin=xmin0+(icx-1)*(xlen+xgap)
    xmax=xmin+xlen
    icy=i; if (i > nframe2); icy=i-nframe2; endif
    if (i > nframe3); icy=i-nframe3; endif
    if (i > nframe4); icy=i-nframe4; endif
    ymax=ymax0+(icy-1)*(ylen+ygap)
    ymin=ymax+ylen
    titlx=xmin+0.15
    titly=ymax-0.3
    'set parea 'xmin' 'xmax' 'ymin' 'ymax

    'run $sorcdir/grads/rgbset.gs'
    'set xlopts 1 4 0.0'
    'set ylopts 1 4 0.0'
      if($nmd <=2)
        'set xlopts 1 4 0.11'
        if(i=1);'set ylopts 1 4 0.11';endif
      endif
      if($nmd >2 & $nmd <=4)
        if(i=2|i=$nmd);'set xlopts 1 4 0.11';endif
        if(i<=2);'set ylopts 1 4 0.11';endif
      endif
      if($nmd >4 & $nmd <=6)
        if(i=3|i=$nmd);'set xlopts 1 4 0.11';endif
        if(i<=3);'set ylopts 1 4 0.11';endif
      endif
      if($nmd >6 & $nmd <=8)
        if(i=4|i=$nmd);'set xlopts 1 4 0.11';endif
        if(i<=4);'set ylopts 1 4 0.10';endif
      endif
      if($nmd >=8)
        if(i=5|i=$nmd);'set xlopts 1 4 0.11';endif
        if(i<=5);'set ylopts 1 4 0.09';endif
      endif
    'set clopts 1 4 0.09'
    'set grid on'
    'set zlog on'
    'set mproj off'

    'set gxout shaded'
    'set grads off'
    'set clevs   'cm5' 'cm4' 'cm3' 'cm2' 'cm1' 0 'cp1' 'cp2' 'cp3' 'cp4' 'cp5
    if ( $var = bias | $var = rms )
     'set rbcols 39    37    36   35    34    32 62   64    65   66     67   69'
    else
     'set rbcols 69    67    66   65    64    62 32   34    35   36     37   39'
    endif
    if(i=1 & $var != bias );'set clevs   'bb1' 'bb2' 'bb3' 'bb4' 'bb5' 'bb6' 'bb7' 'bb8' 'bb9' 'bb10' 'bb11 ;endif
    if(i=1 & $var != bias );'set rbcols 41   42    43    44    45   46  47  48    49   55     56   57';endif
    'set ylevs 1000 925 850 700 500 400 300 250 200 150 100 50 20 10'              
*   'set ylevs 1000 850 700 500 400 300 200 100 50 20 10'              
    'set xlint 24'
    if( $var = bias )
     'd ${coeff}*(fcst(x='%i')-obs(x='%i'))'
    else
     if(i=1)
      'd ${coeff}*${var}(x='%i')'
     else
      'd ${coeff}*(${var}(x='%i')-${var}(x=1))'
     endif
    endif
*
    'set gxout contour'
    'set grads off'
    'set ccolor 1'
    'set clevs   'cm5' 'cm4' 'cm3' 'cm2' 'cm1' 0 'cp1' 'cp2' 'cp3' 'cp4' 'cp5
    if(i=1 & $var != bias );'set clevs   'bb1' 'bb2' 'bb3' 'bb4' 'bb5' 'bb6' 'bb7' 'bb8' 'bb9' 'bb10' 'bb11 ;endif
    if(i=1 & $var = msess );'set clevs     -0.1  0    0.1   0.2   0.3   0.4   0.5  0.6    0.7   0.8    0.9  '  ;endif
    'set clab on'
    if(i=1);'set clab forced';endif
    'set cstyle 3'
    if( $var = bias )
     'd ${coeff}*(fcst(x='%i')-obs(x='%i'))'
    else
     if(i=1)
      'd ${coeff}*${var}(x='%i')'
     else
      'd ${coeff}*(${var}(x='%i')-${var}(x=1))'
     endif
    endif

    'set string 1 bl 7'
    'set strsiz 0.16 0.16'
    if(i=1 | $var = bias )
     'draw string 'titlx' 'titly' 'mdc.i 
    else
     'draw string 'titlx' 'titly' 'mdc.i '-' mdc.1
    endif
  endif
  i=i+1
  endwhile

  'set string 1 bc 6'
  'set strsiz 0.15 0.15'
  'draw string 4.5 10.45 ${vnam} ${unit} ${title} over ${regdef}: fit to $obtype'
  'set strsiz 0.15 0.15'
  'draw string 4.5 10.20 ${cyc}Z Cycle $sdate-$edate Mean'
  'set string 1 bc 4'
  'set strsiz 0.14 0.14'
  'set strsiz 0.15 0.15'
  if($nmd >2)
    'draw string 4.8 0.7 Forecast Hour'
    'run $sorcdir/grads/cbarn.gs 0.95 0 4.5 0.25'
   else
    'draw string 4.3 3.5 Forecast Hour'
    'run $sorcdir/grads/cbarn.gs 0.95 0 4.5 2.90'
   endif

  'printim ${var}mp_${namedaily}.png x700 y700'
  'set vpage off'
'quit'
EOF1
grads -bcp "run ${var}mp_${outname1}.gs"


# ----- PLOT TYPE 2:  vertical profiles of mean $var for a given forecast hour
cat >${var}mpv_${outname1}${lev}.gs <<EOF1 
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
  cth.1=9; cth.2=9; cth.3=4; cth.4=1; cth.5=4; cth.6=1; cth.7=1; cth.8=1; cth.9=1; cth.10=1
  cma.1=0; cma.2=8; cma.3=6; cma.4=1; cma.5=2; cma.6=0; cma.7=3; cma.8=7; cma.9=4; cma.10=5
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=9; cco.7=5; cco.8=6; cco.9=7; cco.10=15

*------------------------
nf=1 ;*start from fcst00
while ( nf <  ${fnum} )
fhr=(nf-1)*${fhout}
*------------------------

  'c'
  'set x 1'
  'set t $ndaysp1'
  'set y '%nf 
  'set z 1 $nlev '
  'define zero=0'

  xwd=4.0; ywd=7.0; yy=ywd/17
  xmin=1.5; xmax=xmin+xwd; ymin=2.0; ymax=ymin+ywd
  xt=xmin+0.1; xt1=xt+0.5; xt2=xt1+0.1; yt=ymax-0.1 ;*for legend  
  zxt=xt2+3.0; zxt1=zxt+0.5; zxt2=zxt1+0.1; zyt=ymin+0.36*ywd
  titlx=xmin+0.7*xwd;  titly=ymax+0.65                  ;*for tytle
  titlx2=xmin+0.7*xwd;  titly2=ymax+0.30                  ;*for tytle
  xlabx=xmin+0.5*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

*--find maximum and minmum values
   cmax=-10000000.0; cmin=10000000.0
   i=1
   while (i <= $nmd)
    'set gxout stat'
    if ( $var = bias )
     'd ${coeff}*(fcst(x='%i')-obs(x='%i'))'
    else
     'd ${coeff}*${var}(x='%i')'
    endif
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if( b>0 )
     if(zmax > cmax); cmax=zmax; endif
     if(zmin < cmin); cmin=zmin; endif
    endif
   i=i+1
   endwhile
   if ( cmin > 0 ); cmin=0 ; endif
   if ( cmax < 0 ); cmax=0 ; endif
   say 'cmin cmax 'cmin' 'cmax

   i=1
   while (i <= $nmd)
    'set gxout stat'  ;* first compute means and count good data numbers
    if ( $var = bias )
     'd ${coeff}*(fcst(x='%i')-obs(x='%i'))'
    else
     'd ${coeff}*${var}(x='%i')'
    endif
    ln=sublin(result,11); wd=subwrd(ln,2); a=substr(wd,1,5)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if ( b>0 )
      if( i <=5);
        'set strsiz 0.15 0.15'; yt=yt-yy
        'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
        'set string 'cco.i' bl 9';     'draw string 'xt2' 'yt' 'mdc.i
      else
        'set strsiz 0.15 0.15'; zyt=zyt-yy
        'set line 'cco.i' 'cst.i' 11'; 'draw line 'zxt' 'zyt' 'zxt1' 'zyt
        'set string 'cco.i' bl 9';     'draw string 'zxt2' 'zyt' 'mdc.i
      endif

      'set gxout line'
      'set zlog on'
      'set ylevs 1000 925 850 700 500 400 300 250 200 150 100 50 20 10'              
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 'cmin' 'cmax
*     'set xlint $xtick'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      if ( $var = bias )
       'd ${coeff}*(fcst(x='%i')-obs(x='%i'))'
      else
       'd ${coeff}*${var}(x='%i')'
      endif
      'set cstyle 1'; 'set cthick 9'; 'set cmark 0'; 'set ccolor 7'
      if ( $var = bias ); 'd zero' ;endif
     endif
   i=i+1
   endwhile

  'set string 1 bc 7'
  'set strsiz 0.14 0.14'
  'draw string 'titlx' 'titly' ${vnam} ${unit} ${title} over ${regdef}: fit to $obtype'
  'set strsiz 0.14 0.14'
  'draw string 'titlx2' 'titly2' ${cyc}Z Cycle '%fhr'hr Fcst, $sdate-$edate Mean'
  'set strsiz 0.14 0.14'
  'draw string 'xlabx' 'xlaby' ${vnam} ${title}'
  'draw ylab Pressure (hPa)'   

**------------------------------
**plot counts of observations
  'set x 1'
  'set t $ndaysp1'
  'set y '%nf 
  'set z 1 $nlev '
  xmin=xmax+0.1; xmax=xmin+2; ymin=ymin; ymax=ymax
  xlabx=xmin+1;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

*--find max and min of number of obs (in thousands)          
   cmax=-100000.0; cmin=100000.0
   'set gxout stat'
   'd 0.001*${ndays}*pts(t=$ndaysp1)'
   range=sublin(result,9); zmin=subwrd(range,8); zmax=subwrd(range,6)
   ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
   if( b>0 )
    if(zmax > cmax); cmax=zmax; endif
    if(zmin < cmin); cmin=zmin; endif
   endif
   cmin=0 
   if ( cmax <=0 ); cmax=1 ; endif
   cmin=substr(cmin,1,10); cmax=substr(cmax,1,10); cint=10*substr((cmax-cmin)/50,1,3)
   if (cint = 0); cint=substr((cmax-cmin)/5,1,3); endif
   if (cint = 0); cint=0.2*substr((cmax-cmin),1,3); endif
   if (cint = 0); cint=0.02*substr((cmax-cmin)*10,1,3); endif
   if (cint = 0); cint=0.002*substr((cmax-cmin)*100,1,3); endif
   if (cint = 0); cint=0.0002*substr((cmax-cmin)*1000,1,3); endif
   if (cint = 0); cint=0.00002*substr((cmax-cmin)*10000,1,3); endif
   say 'cmin cmax cint 'cmin' 'cmax' 'cint

   'set gxout line'
   'set zlog on'
   'set ylevs 1000 925 850 700 500 400 300 250 200 150 100 50 20 10'              
   'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
   'set xlopts 1 6 0.14';     'set ylopts 1 6 0'; 'set clopts 1 6 0.0'
   'set vrange 'cmin' 'cmax
   'set xlint 'cint
   'set cstyle 1'; 'set cthick 9'; 'set cmark 2'; 'set ccolor 1'
   'd 0.001*${ndays}*pts(t=$ndaysp1)'
  'set string 1 bc 7'
  'set strsiz 0.12 0.12'
  'draw string 'xlabx' 'xlaby' Obs Count (x1000) '
**------------------------------

  'printim ${var}mpv_fhr'%fhr'_${namedaily}.png x800 y800'
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
grads -bcp "run ${var}mpv_${outname1}${lev}.gs"


#-------------------------------------------------------------------
#-------------------------------------------------------------------
#  The following are time series (line plots) for a specific forecast 
#  hour as a function of calendar day
for lev  in $levlist ; do
  levp=`echo $lev | sed "s?P??g"`
#-------------------------------------------------------------------

# ----- PLOT TYPE 3:  time series of ${var} Errors----
cat >${var}_${outname1}${lev}.gs <<EOF1 
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
  cth.1=9; cth.2=9; cth.3=4; cth.4=1; cth.5=4; cth.6=1; cth.7=1; cth.8=1; cth.9=1; cth.10=1
  cma.1=0; cma.2=8; cma.3=6; cma.4=1; cma.5=2; cma.6=0; cma.7=3; cma.8=7; cma.9=4; cma.10=5
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
  'set lev $levp '

  xwd=7.0; ywd=4.0; yy=ywd/17
  xmin=1.0; xmax=xmin+xwd; ymin=6.0; ymax=ymin+ywd
  xt=xmin+0.3; xt1=xt+0.5; xt2=xt1+0.1; yt=ymin+0.36*ywd ;*for legend  
  zxt=xt2+3.0; zxt1=zxt+0.5; zxt2=zxt1+0.1; zyt=ymin+0.36*ywd
  titlx=xmin+0.5*xwd;  titly=ymax+0.35                  ;*for tytle
  titlx2=xmin+0.5*xwd;  titly2=ymax+0.10                  ;*for tytle
  xlabx=xmin+0.5*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

*--find maximum and minmum values
   cmax=-10000000.0; cmin=10000000.0
   i=1
   while (i <= $nmd)
    'set gxout stat'
    if ( $var = bias )
     'd ${coeff}*(fcst(x='%i')-obs(x='%i'))'
    else
     'd ${coeff}*${var}(x='%i')'
    endif
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if( b>0 )
     if(zmax > cmax); cmax=zmax; endif
     if(zmin < cmin); cmin=zmin; endif
    endif
   i=i+1
   endwhile
   dist=cmax-cmin; cmin=cmin-0.5*dist; cmax=1.05*cmax
   cmin=substr(cmin,1,10); cmax=substr(cmax,1,10); cint=10*substr((cmax-cmin)/100,1,4)
   if (cint = 0); cint=substr((cmax-cmin)/10,1,4); endif
   if (cint = 0); cint=0.1*substr((cmax-cmin),1,4); endif
   if (cint = 0); cint=0.01*substr((cmax-cmin)*10,1,4); endif
   if (cint = 0); cint=0.001*substr((cmax-cmin)*100,1,4); endif
   if (cint = 0); cint=0.0001*substr((cmax-cmin)*1000,1,4); endif
   if (cint = 0); cint=0.00001*substr((cmax-cmin)*10000,1,4); endif
   say 'cmin cmax cint 'cmin' 'cmax' 'cint

   i=1
   while (i <= $nmd)
    'set gxout stat'  ;* first compute means and count good data numbers
    if ( $var = bias )
     'd ${coeff}*(fcst(x='%i')-obs(x='%i'))'
    else
     'd ${coeff}*${var}(x='%i')'
    endif
    ln=sublin(result,11); wd=subwrd(ln,2); a=substr(wd,1,5)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if ( b>0 )
      if( i <=5);
        'set strsiz 0.13 0.13'; yt=yt-yy
        'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
        'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i'  ' a'  'b
      else
        'set strsiz 0.13 0.13'; zyt=zyt-yy
        'set line 'cco.i' 'cst.i' 11'; 'draw line 'zxt' 'zyt' 'zxt1' 'zyt
        'set string 'cco.i' bl 6';     'draw string 'zxt2' 'zyt' 'mdc.i'  ' a'  'b
      endif

      'set gxout line'
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 'cmin' 'cmax; 'set ylint 'cint
      'set xlint $xtick'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      if ( $var = bias )
       'd ${coeff}*(fcst(x='%i')-obs(x='%i'))'
      else
       'd ${coeff}*${var}(x='%i')'
      endif
     endif
   i=i+1
   endwhile

  'set string 1 bc 7'
  'set strsiz 0.14 0.14'
  'draw string 'titlx' 'titly' ${lev} ${vnam} ${unit} ${title} over ${regdef}: fit to $obtype'
  'set strsiz 0.14 0.14'
  'draw string 'titlx2' 'titly2' ${cyc}Z Cycle '%fhr'hr Fcst'
  'set strsiz 0.15 0.15'
  'draw string 'xlabx' 'xlaby' Verification Date'

  'printim ${var}_fhr'%fhr'_${lev}${namedaily}.png x800 y800'
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
grads -bcp "run ${var}_${outname1}${lev}.gs"

#-------------------------------------------------------------------
done  ;# end lev for line plots
#-------------------------------------------------------------------

#=======================================
done   ;#variable cycle for making plots                    
#=======================================


chmod a+rw cor*.png fo*.png

#-------------------------------
  if [ $copymap = "YES" ]; then
    mkdir -p $mapdir/air/bias
    cp bias*.png  $mapdir/air/bias/.
    mkdir -p $mapdir/air/rms
    cp rms*.png  $mapdir/air/rms/.
  fi
#------------------------------

## -- for daily rotatigng display
cat << EOF >ftpin
  binary
  promt
  cd $ftpdir/g2o/air/bias
  mput bias*${namedaily}.png
  cd $ftpdir/g2o/air/rms
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
done  ;# end cyc
done  ;# end vnam
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------


exit


