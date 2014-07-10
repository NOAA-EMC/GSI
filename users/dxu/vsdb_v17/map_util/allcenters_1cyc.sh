#!/bin/ksh
set -x

#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#  1. VSDB stats are computed from anomalies relative to CDAS 30-year means (/nwprod/fix/cmean_1d.x)
#  2. Make AC plots that compare scores from all centers for the same forecast cycle (e.g. 00Z)
#  Fanglin Yang
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------

## -- verification dates
CDATE=$(date +%Y%m%d)
export edate=${edate:-${1:-$CDATE}}      ;#end of verification date
export ndays=${ndays:-${2:-31}}          ;#number of days back for verification
export fdays=${fdays:-${3:-10}}          ;#forecast length in days to be verified 


## -- verification parameters
export fcycle=${fcycle:-"00"}               ;#forecast cycles
export vhrlist=${vhrlist:-"00 06 12 18"}    ;#verification hours for each day
export vtype=${vtype:-anom}
export vnamlist=${vnamlist:-"HGT"}
export mdlist=${mdlist:-"gfs ecm cmc fno ukm cdas jma"}
export levlist=${levlist:-"P500"}
export reglist=${reglist:-"G2/NHX"}

export webhost=${webhost:-"emcrzdm.ncep.noaa.gov"}
export webhostid=${webhostid:-"$LOGNAME"}
export vsdb_data=${vsdb_data:-/climate/save/wx24fy/VRFY/vsdb_data}
export sorcdir=${sorcdir:-/global/save/wx24fy/VRFY/vsdb/map_util}
export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$webhostid/vsdb} 
export doftp=${doftp:-"YES"}                      ;#ftp maps to web site
export mapdir=${mapdir:-/stmp/$LOGNAME/vsdb_exp/maps}                          
export makemap=${makemap:-"YES"}                  ;#whether or not to make maps      
export copymap=${copymap:-"YES"}                  ;#copy maps to a central directory
export archmon=${archmon:-"NO"}                   ;# archive monthly means 
export rundir=${rundir:-/stmp/$LOGNAME/vsdb_stats0}
export xtick=`expr $ndays \/ 8 `
export scoredir=${scoredir:-$rundir/score}
if [ ! -s $scoredir ]; then mkdir -p $scoredir ;fi


##determine forecast output frequency required for verification
export nvhr=`echo $vhrlist |wc -w`     ;#number of verification hours
export fhout=`expr 24 \/ $nvhr `       ;#forecast output frequency
export vlength=`expr $fdays \* 24 `
export nfcst=`expr $vlength \/ $fhout + 1 `


## remove missing data from all models to unify sample size, 0-->NO, 1-->Yes
export maskmiss=${maskmiss:-1}                                        


export NWPROD=${NWPROD:-/nwprod}
export ndate=${ndate:-$NWPROD/util/exec/ndate}
export FC=${FC:-xlf90}
export FFLAG=${FFLAG:-" "}
export GRADSBIN=${GRADSBIN:-/usrx/local/grads/bin}
export imgconvert=${IMGCONVERT:-convert}                 

export vsdbhome=${vsdbhome:-/global/save/$LOGNAME/VRFY/vsdb}
export SUBJOB=${SUBJOB:-$vsdbhome/bin/sub_wcoss}
export ACCOUNT=${ACCOUNT:-GFS-MTN}
export CUE2RUN=${CUE2RUN:-shared}
export CUE2FTP=${CUE2FTP:-${CUE2RUN:-transfer}}
export GROUP=${GROUP:-g01}

##Create scorecard database
export scorecard=${scorecard:-"NO"}
scoretext=0; if [ $scorecard = "YES" ] ; then scoretext=1 ;fi

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
for vnam in $vnamlist; do
   if [ $vnam = "HGT_WV1/0-3" -o $vnam = "HGT_WV1/4-9" -o $vnam = "HGT_WV1/10-20" ]; then
     scoretext=0
   fi
   vnam1=`echo $vnam | sed "s?/??g" |sed "s?_WV1?WV?g"`
   export exedir=${rundir}/${vtype}/${vnam1}
   if [ ! -s ${rundir}/${vtype} ] ; then mkdir -p ${rundir}/${vtype} ; fi
   cd ${rundir}/${vtype}
   if [ -s ${vnam1} ]; then rm -rf ${vnam1} ; fi
   mkdir ${vnam1} ; cd $exedir || exit

for lev  in $levlist ; do
for reg  in $reglist ; do
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------

# -- generate output names 
  ncyc=`echo $fcycle | wc -w`            ;#count number of cycles
  nhours=`expr $ndays \* 24 - 24`
  tmp=`$ndate -$nhours ${edate}00 `
  sdate=`echo $tmp | cut -c 1-8`
  reg1=`echo $reg | sed "s?/??g"`
  outname1=${vnam1}_${lev}_${reg1}_${sdate}${edate}
  yyyymm=`echo $edate |cut -c 1-6`                                
  if [ $ncyc -gt 1 ]; then
   outmon=${vnam1}_${lev}_${reg1}_${yyyymm}
  else
   outmon=${vnam1}_${lev}_${reg1}_${fcycle}Z${yyyymm}
  fi

# -- search data for all models; write out binary data, create grads control file
  if [ $vnam = "WIND" ]; then
   $sorcdir/gen_wind.sh $vtype $vnam $reg $lev $edate $ndays "${fcycle}" $fdays $fhout $outname1 $maskmiss "$mdlist"
  else
   $sorcdir/gen_scal.sh $vtype $vnam $reg $lev $edate $ndays "${fcycle}" $fdays $fhout $outname1 $maskmiss "$mdlist"
  fi


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
if [ $makemap = "YES" ]; then
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# -- create grads scripts, allows up to 10 models 
nmd0=`echo $mdlist | wc -w`  ;#count number of models
nmd=`expr $nmd0 \* $ncyc `

set -A mdname0  $mdlist
set -A mdnamec0  `echo $mdlist |tr "[a-z]" "[A-Z]" `
set -A cycname  $fcycle

n=0; mn=0
while [ $n -lt $nmd0 ]; do
 m=0
 while [ $m -lt $ncyc ]; do
  if [ $ncyc -gt 1 ]; then
   mdname[mn]=${mdname0[n]}${cycname[m]}
   mdnamec[mn]=${mdnamec0[n]}${cycname[m]}
  else
   mdname[mn]=${mdname0[n]}
   mdnamec[mn]=${mdnamec0[n]}
  fi
  mn=`expr $mn + 1 `
  m=`expr $m + 1 `
 done
 n=`expr $n + 1 `
done

namedaily=${vnam1}_${lev}_${reg1}


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
  cth.1=10; cth.2=10; cth.3=9; cth.4=9; cth.5=9; cth.6=9; cth.7=9; cth.8=5; cth.9=9; cth.10=9
  cma.1=0; cma.2=8; cma.3=6; cma.4=1; cma.5=2; cma.6=3; cma.7=4; cma.8=7; cma.9=4; cma.10=5
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=5; cco.7=15; cco.8=9; cco.9=6; cco.10=7
*------------------------
fhour=0 ;*start from fcst00
while ( fhour <= ${vlength} )
if (fhour=24|fhour=72|fhour=120|fhour=144|fhour=192|fhour=240)
*------------------------
  'c'
  day=fhour/24
  laty=fhour/${fhout}+1
  'set x 1' 
  'set t 1 $ndays' 
  'set y '%laty 

  xwd=7.0; ywd=4.0; yy=ywd/16
  xmin=1.0; xmax=xmin+xwd; ymin=6.0; ymax=ymin+ywd
  xt=xmin+0.3; xtm=xt+0.25; xt1=xt+0.5; xt2=xt1+0.1; yt=ymin+0.36*ywd ;*for legend  
  zxt=xt2+3.0; zxtm=zxt+0.25; zxt1=zxt+0.5; zxt2=zxt1+0.1; zyt=ymin+0.36*ywd
  titlx=xmin+0.5*xwd;  titly=ymax+0.20                  ;*for tytle
  xlabx=xmin+0.45*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

*--find maximum and minmum values
   cmax=-1.0; cmin=1.0
   i=1
*  while (i <= $nmd)
   while (i <= 1)
    'set gxout stat'
    'd cor(x='%i')'
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > cmax); cmax=zmax; endif
    if(zmin < cmin); cmin=zmin; endif
   i=i+1
   endwhile
   dist=cmax-cmin; cmin=cmin-0.9*dist; cmax=1.2*cmax        
   if (cmin < -0.1)
     cmin=-0.1; cmin=substr(cmin,1,4) 
   else
     cmin=substr(cmin,1,3) 
   endif
   if (cmax > 1.0); cmax=1.0; endif
   cmax=substr(cmax,1,3)
   cint=0.1
   say 'cmin cmax cint 'cmin' 'cmax' 'cint
*    cmin=0.2; cmax=1.0

   i=1
   while (i <= $nmd)
    'set gxout stat'  ;* first compute means and count good data numbers
    'd cor(x='%i')'
    ln=sublin(result,11); wd=subwrd(ln,2); a=substr(wd,1,5); sc.i=subwrd(ln,2)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if ( b>0 )
      if( i <=5); 
        'set strsiz 0.13 0.13'; yt=yt-yy
        'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
        'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i'  ' a'  'b
        'draw mark 'cma.i' 'xtm' 'yt' 0.1'
      else
        'set strsiz 0.13 0.13'; zyt=zyt-yy
        'set line 'cco.i' 'cst.i' 11'; 'draw line 'zxt' 'zyt' 'zxt1' 'zyt
        'set string 'cco.i' bl 6';     'draw string 'zxt2' 'zyt' 'mdc.i'  ' a'  'b
        'draw mark 'cma.i' 'zxtm' 'zyt' 0.1'
      endif

      'set gxout line'
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'set vrange 'cmin' 'cmax; 'set ylint 'cint; 'set xlint $xtick'
*     'set vrange 0.2 1';        'set ylint 0.1'; 'set xlint $xtick'
      'd cor(x='%i')'
    endif
* Create verification scorecard text files
    if ( $scoretext = 1 )
    '${vsdbhome}/map_util/grads/fprintf.gs 'sc.i' score_cor_'${namedaily}'_'mdc.i'_day'%day'.txt %-7.6f'
    endif
   i=i+1
   endwhile

  'set string 1 bc 7'
  'set strsiz 0.16 0.16'
  if ( $ncyc >1 )
   'draw string 'titlx' 'titly' Anomaly Correl: ${vnam} ${lev} ${reg}, fh'%fhour
  else
   'draw string 'titlx' 'titly' Anomaly Correl: ${vnam} ${lev} ${reg} ${fcycle}Z, fh'%fhour
  endif
  'set strsiz 0.15 0.15'
  'draw string 'xlabx' 'xlaby' Verification Date'

  'printim cor_day'%day'_${namedaily}.png x800 y800'
  'set vpage off'
*--------
endif
fhour=fhour+${fhout}
endwhile
*-------
'quit'
EOF1
$GRADSBIN/grads -bcp "run acz_${outname1}.gs"


# ----- PLOT TYPE 2:  Die-off plot for mean correlation over $ndays days----
ndaysp1=`expr $ndays + 1`
fdaysp1=`expr $fdays + 1`
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
  cma.1=2; cma.2=0; cma.3=0; cma.4=0; cma.5=0; cma.6=0; cma.7=0; cma.8=0; cma.9=0; cma.10=0
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=5; cco.7=15; cco.8=9; cco.9=6; cco.10=7
  bar.1=30; bar.2=40; bar.3=50; bar.4=60; bar.5=70; bar.6=80; bar.7=90; bar.8=92; bar.9=94; bar.10=96

  'set x 1' 
  'set t $ndaysp1' 
  'set y 1 ${nfcst}'
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
    'd bincor(x='%i')'     ;* number of records
    ln=sublin(result,11); wd=subwrd(ln,2); a=substr(wd,1,3)
    if ( a>0 )
      'set strsiz 0.13 0.13'; yt=yt-yy
      'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
      'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i'  ' a

      'set gxout line'
      'set mproj off'
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.0';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 0.21 1.0'; 'set ylint 0.1'; 'set xlint 48'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'd cor(x='%i')'  
    endif

   i=i+1
   endwhile

  'set string 1 bc 6'
  'set strsiz 0.13 0.13'
  if ( $ncyc >1 )
   'draw string 'titlx' 'titly' AC: ${vnam} ${lev} ${reg}, $sdate-$edate '
  else
   'draw string 'titlx' 'titly' AC: ${vnam} ${lev} ${reg} ${fcycle}Z, $sdate-$edate '
  endif
  'set string 1 bl 3'
  'set strsiz 0.09 0.09'

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
  'd bincor(x='%i')'   ;*number of records 
  ln=sublin(result,11); wd=subwrd(ln,2); a=substr(wd,1,3)
  if ( a>0 )
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
 cintp=0
 if( dist > 0.004 ); cintp=10*substr(dist/40,1,4); endif
 if (cintp = 0 & dist > 0.0004 ); cintp=10*substr(dist/40,1,5); endif
 if (cintp = 0 & dist > 0.00004 ); cintp=10*substr(dist/40,1,6); endif

 
*---------------------------------------------------------
* compute standard deviation of the difference between the 
* first and each of the rest models for each forecast hour 

 i=2
 while (i <= $nmd)
 'set gxout stat'  
 'd bincor(x='%i')'     ;* number of records
 ln=sublin(result,11); wd=subwrd(ln,2); a=substr(wd,1,3)
 ln=sublin(result,7); ct=subwrd(ln,8)
 if ( a>0 )
   'define acdm=ave(cor(x='%i')-cor(x=1),t=1,t=$ndays)'
**      standard deviation of difference
   'define std=sqrt(ave((cor(x='%i')-cor(x=1)-acdm)*(cor(x='%i')-cor(x=1)-acdm),t=1,t=$ndays))'
   'define nsz=bincor(x='%i')'
**      Null Hypothesis: mean(AC1-AC2)=0, AC1-AC2 follows normal distribution. 
**      plot the 5% conf interval of difference of means : F*SD/sqrt(N-1), 
**      F=1.96 for infinite samples, F=2.0 for nsz=60, F=2.042 for nsz=30, F=2.228 for nsz=10   
   if(nsz>=80);            'define intvl=1.960*std/sqrt(nsz-1)'  ;endif
   if(nsz>=40 & nsz <80);  'define intvl=2.000*std/sqrt(nsz-1)'  ;endif
   if(nsz>=20 & nsz <40);  'define intvl=2.042*std/sqrt(nsz-1)'  ;endif
   if(nsz<20);             'define intvl=2.228*std/sqrt(nsz-1)'  ;endif

   'set gxout bar'
   'set bargap 'bar.i
   'set baropts outline'
   'set ccolor 'cco.i
   'set cstyle 1'; 'set cthick 3'; 'set cmark 0'
   'set mproj off'
   'set display color white'; 'set grads off'; 'set grid on'
   'set xlopts 1 6 0.0';     'set ylopts 1 6 0.0'; 'set clopts 1 6 0.0'
   'set vrange 'cmin' 'cmax; 'set ylint 'cintp; 'set xlint 48'
   'd -intvl;intvl'
**add missing bars when intvl is larger than cmax
   'set ccolor 'cco.i
   'define xa=maskout(intvl*0+0.999*'cmin',intvl+'cmin')'
   'define xb=maskout(intvl*0+0.999*'cmax',intvl-'cmax')'
   'set datawarn off'
   'd xa;xb'
  endif

** Create verification scorecard text files
  if ( $scoretext = 1 )
   'set gxout print'
   'set prnopts %10.4e 'ct
   'd intvl'
   ln=sublin(result,2)
   n=2
   while (n<=ct)
     tv.n=subwrd(ln,n)
     say tv.n
     '${vsdbhome}/map_util/grads/fprintf.gs 'tv.n' score_cor_conflimit_'${namedaily}'_'mdc.i'_day'n-1'.txt %-7.6f'
    n=n+1
   endwhile
  endif
 i=i+1
 endwhile

 i=1
 while (i <= $nmd)
  'set gxout stat'  
  'd bincor(x='%i')'     ;* number of records
  ln=sublin(result,11); wd=subwrd(ln,2); a=substr(wd,1,3)
  if ( a>0 )
     'set gxout line'
     'set mproj off'
     'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
     'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
     'set vrange 'cmin' 'cmax; 'set ylint 'cintp; 'set xlint 48'
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


  'printim cordieoff_${namedaily}.png x700 y700'
  'set vpage off'
'quit'
EOF1
$GRADSBIN/grads -bcp "run cordieoff_${outname1}.gs"


# ----- PLOT TYPE 3:  difference of AC, other models minus first model  ----
cat >cordiff_${outname1}.gs <<EOF1 
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

  'set x 1 ' 
  'set t 1 $ndays' 
  'set y 1 $nfcst' 

  nframe=$nmd
  xmin0=1.2;  xlen=3.0;  xgap=0.2
  ymax0=10.0; ygap=-0.1
               nframe2=1;  nframe3=2; ylen=-6.0
  if($nmd >2); nframe2=2;  nframe3=4; ylen=-4.2;endif
  if($nmd >4); nframe2=3;  nframe3=6; ylen=-2.8;endif
  if($nmd >6); nframe2=4;  nframe3=8; ylen=-2.1; endif
  if($nmd >8); nframe2=5;  nframe3=10; ylen=-1.7; endif

  i=1
  while ( i <= nframe )
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
        if(i=1);'set ylopts 1 4 0.12';endif
      endif
      if($nmd >2 & $nmd <=4)
        if(i=2|i=$nmd);'set xlopts 1 4 0.11';endif
        if(i<=2);'set ylopts 1 4 0.12';endif
      endif
      if($nmd >4 & $nmd <=6)
        if(i=3|i=$nmd);'set xlopts 1 4 0.11';endif
        if(i<=3);'set ylopts 1 4 0.11';endif
      endif
      if($nmd >6 & $nmd <=8)
        if(i=4|i=$nmd);'set xlopts 1 4 0.11';endif
        if(i<=4);'set ylopts 1 4 0.10';endif
      endif
      if($nmd >8)
        if(i=5|i=$nmd);'set xlopts 1 4 0.11';endif
        if(i<=5);'set ylopts 1 4 0.09';endif
      endif
    'set clopts 1 4 0.08'
    'set grid on'
    'set mproj off'

    'set gxout shaded'
    'set grads off'
    'set clevs   -0.3 -0.2 -0.1 -0.05 -0.01 -0.005 0 0.005 0.01 0.05 0.1 0.2 0.3'
*   'set rbcols 39   38   37   36    35    34    32 62   64   65   66   67   68 69'
    'set rbcols 69   67   66   65   64   62  32   34   35  36   37   39'
    if(i=1); 'set clevs    0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 0.95 0.97 ' ;endif
    if(i=1); 'set rbcols 31  33  35  37  39  42  43  45  47   48   49';endif
    'set xlevs 0 24 48 72 96 120 144 168 192 216 240 264 288 312 336 360 384'
    if($fdaysp1>12); 'set xlevs 0 48 96 144 192 240 288 336 384';endif
    'set ylint 5'
    if(i=1);'d cor(x=1)' ;endif
    if(i>1);'d cor(x='%i') - cor(x=1)' ;endif
*
    'set gxout contour'
    'set grads off'
    'set ccolor 1'
    'set clevs   -0.3 -0.2 -0.1 -0.05 -0.01 -0.005 0 0.005 0.01 0.05 0.1 0.2 0.3'
    if(i=1); 'set clevs    0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 0.95 0.97 0.99 ' ;endif
    'set clab forced'
    'set cstyle 1'
    if(i=1);'d cor(x=1)' ;endif
*   if(i>1);'d cor(x='%i') - cor(x=1)' ;endif

    'set string 1 bl 7'
    'set strsiz 0.14 0.14'
    if(i = 1); 'draw string 'titlx' 'titly' 'mdc.1; endif
    if(i > 1); 'draw string 'titlx' 'titly' 'mdc.i' - 'mdc.1; endif
  i=i+1
  endwhile

  'set string 1 bc 6'
  'set strsiz 0.15 0.15'
  if ( $ncyc >1 )
   'draw string 4.2 10.4 Anomaly Correlation: ${vnam} ${lev} ${reg}'
  else
   'draw string 4.2 10.4 Anomaly Correlation: ${vnam} ${lev} ${reg} ${fcycle}Z'
  endif
  'set string 1 bc 5'
  'set strsiz 0.13 0.13'
  'set strsiz 0.15 0.15'
  if($nmd >2)
    'draw string 4.3 0.7 Forecast Hour'
    'run $sorcdir/grads/cbarn.gs 0.95 0 4.1 0.25'
   else
    'draw string 4.3 3.5 Forecast Hour'
    'run $sorcdir/grads/cbarn.gs 0.95 0 4.1 2.90'
   endif

  'printim cordiff_${namedaily}.png x700 y700'
  'set vpage off'
'quit'
EOF1
$GRADSBIN/grads -bcp "run cordiff_${outname1}.gs"


# ----- PLOT TYPE 4:  frequency distribution of anomaly correlations ----
ndayfq=$ndays
if [ $ndayfq -gt 20 ]; then ndayfq=20; fi
nday05=`expr $ndayfq \/ 2 `
cat >freq_${outname1}.gs <<EOF1 
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

*-- define line styles and model names 
  cst.1=1; cst.2=1; cst.3=1; cst.4=1; cst.5=1; cst.6=1; cst.7=1; cst.8=1
  cth.1=9; cth.2=9; cth.3=9; cth.4=5; cth.5=5; cth.6=5; cth.7=5; cth.8=5
  cma.1=0; cma.2=0; cma.3=0; cma.4=0; cma.5=0; cma.6=0; cma.7=0; cma.8=0
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=9; cco.7=15; cco.8=6
*------------------------
*fhour=0 ;*start from fcst00
*while ( fhour <= ${vlength} )
fhour=120
while ( fhour <= 144 )
if (fhour=120|fhour=144)
*------------------------
  'c'
  day=fhour/24
  laty=fhour/${fhout}+1
  'set x 1'       
  'set y '%laty
  'set t $nday05 $ndayfq' 
  xwd=7.0; ywd=5.0; yy=ywd/17
  xmin=0.8; xmax=xmin+xwd; ymin=5.0; ymax=ymin+ywd
  xt=xmin+0.3; xt1=xt+0.5; xt2=xt1+0.1; yt=ymax;*for legend  
  titlx=xmin+0.5*xwd;  titly=ymax+0.20                  ;*for tytle
  xlabx=xmin+0.45*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

   i=1
   while (i <= $nmd)
    'set gxout stat'; 'd bincor(x='%i')'    ;* number of records
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,5)
    if ( b>0 )
      'set strsiz 0.13 0.13'; yt=yt-yy
      'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
      'set string 'cco.i' bl 6';        'draw string 'xt2' 'yt' 'mdc.i

      'set gxout line'
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 0 60';        'set ylint 10'
      'set xaxis 0.475 0.975 0.1'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'd 100*bincor(x='%i')'
    endif
   i=i+1
   endwhile

  'set string 1 bc 7'
  'set strsiz 0.16 0.16'
  if ( $ncyc >1 )
   'draw string 'titlx' 'titly' AC Freq: ${vnam} ${lev} ${reg}, Day '%day
  else
   'draw string 'titlx' 'titly' AC Freq: ${vnam} ${lev} ${reg} ${fcycle}Z, Day '%day
  endif
  'set strsiz 0.15 0.15'
  'draw string 'xlabx' 'xlaby' Anomaly Correlation,   ${sdate}-${edate}'

  'printim freq_day'%day'_${namedaily}.png x800 y800'
  'set vpage off'
*--------
endif
fhour=fhour+${fhout}
endwhile
*-------
'quit'
EOF1
$GRADSBIN/grads -bcp "run freq_${outname1}.gs"


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#--------------------
# remove whitesapce from.png files
# ImageMagic
export PATH=$PATH:/usrx/local/imajik/bin
export LIBPATH=/lib:/usr/lib
export LIBPATH=$LIBPATH:/usrx/local/imajik/lib

# nn=0
# while [ $nn -le $fdays ]; do
  for nn in 1 3 5 6 8 10; do
   $imgconvert -crop 0.2x0.2 cor_day${nn}_${namedaily}.png cor_day${nn}_${namedaily}.png
   #$imgconvert -crop 0.2x0.2 freq_day${nn}_${namedaily}.png freq_day${nn}_${namedaily}.png
#  nn=`expr $nn + 1 `
  done
  #$imgconvert -crop 0.2x0.2 cordiff_${namedaily}.png     cordiff_${namedaily}.png
  #$imgconvert -crop 0.2x0.2 cordieoff_${namedaily}.png   cordieoff_${namedaily}.png
  #$imgconvert -crop 0.2x0.2 freqmap_${namedaily}.png     freqmap_${namedaily}.png

chmod a+rw cor*.png
chmod a+rw freq*.png

#-------------------------------
  if [ $copymap = "YES" ]; then
    tmpdir=${mapdir}/allmodel/daily 
    mkdir -p $tmpdir/cor $tmpdir/dieoff $tmpdir/freq 
    cp cor*.png  $tmpdir/cor/.
    cp freq*.png  $tmpdir/freq/.
    cp cordieoff*.png  $tmpdir/dieoff/.
  fi
#------------------------------

## -- for daily rotatigng display
cat << EOF >ftp_$vnam1$lev$reg1
  binary
  promt
  cd $ftpdir/allmodel/daily/cor
    mput cor_day*_${namedaily}.png
    mput cordiff_${namedaily}.png
  cd $ftpdir/allmodel/daily/dieoff
    mput cordieoff_${namedaily}.png
  cd $ftpdir/allmodel/daily/freq
    mput freq_day*_${namedaily}.png
    mput freqmap_${namedaily}.png
  quit
EOF
if [ $doftp = "YES" -a $CUE2RUN = $CUE2FTP ]; then 
 sftp  ${webhostid}@${webhost} <ftp_$vnam1$lev$reg1
 if [ $? -ne 0 ]; then
  scp -rp cor_day*_${namedaily}.png   ${webhostid}@${webhost}:$ftpdir/allmodel/daily/cor/.
  scp -rp cordiff_${namedaily}.png    ${webhostid}@${webhost}:$ftpdir/allmodel/daily/cor/.
  scp -rp cordieoff_${namedaily}.png  ${webhostid}@${webhost}:$ftpdir/allmodel/daily/dieoff/.
  scp -rp freq_day*_${namedaily}.png  ${webhostid}@${webhost}:$ftpdir/allmodel/daily/freq/.  
  scp -rp freqmap_${namedaily}.png    ${webhostid}@${webhost}:$ftpdir/allmodel/daily/freq/.  
 fi
fi


## -- make monthly archive 
yyyy=`echo $edate |cut -c 1-4`                                
if [ $archmon = "YES" ]; then
# nn=0
# while [ $nn -le $fdays ]; do
  for nn in 1 3 5 6 8 10; do
   cp cor_day${nn}_${namedaily}.png  cor_day${nn}_${outmon}.png
   cp freq_day${nn}_${namedaily}.png freq_day${nn}_${outmon}.png
#  nn=`expr $nn + 1 `
  done
   cp cordiff_${namedaily}.png cordiff_${outmon}.png
   cp cordieoff_${namedaily}.png cordieoff_${outmon}.png 
   cp freqmap_${namedaily}.png   freqmap_${outmon}.png     
cat << EOF >ftpmon_$vnam1$lev$reg1
  binary
  promt
  cd $ftpdir/allmodel/arch_mon/cor/$yyyy
    mput cor_day*_${outmon}.png
    mput cordiff_${outmon}.png
    mput meancor*.txt
  cd $ftpdir/allmodel/arch_mon/dieoff/$yyyy
    mput cordieoff_${outmon}.png
  cd $ftpdir/allmodel/arch_mon/freq/$yyyy
    mput freq_day*_${outmon}.png
    mput freqmap_${outmon}.png
  quit
EOF
if [ $doftp = "YES" -a $CUE2RUN = $CUE2FTP ]; then 
 sftp  ${webhostid}@${webhost} <ftpmon_$vnam1$lev$reg1
 if [ $? -ne 0 ]; then
  scp -rp cor_day*_${outmon}.png  ${webhostid}@${webhost}:$ftpdir/allmodel/arch_mon/cor/$yyyy/.
  scp -rp cordiff_${outmon}.png   ${webhostid}@${webhost}:$ftpdir/allmodel/arch_mon/cor/$yyyy/.
  scp -rp meancor*.txt            ${webhostid}@${webhost}:$ftpdir/allmodel/arch_mon/cor/$yyyy/.
  scp -rp cordieoff_${outmon}.png ${webhostid}@${webhost}:$ftpdir/allmodel/arch_mon/dieoff/$yyyy/.
  scp -rp freq_day*_${outmon}.png ${webhostid}@${webhost}:$ftpdir/allmodel/arch_mon/freq/$yyyy/.  
  scp -rp freqmap_${outmon}.png   ${webhostid}@${webhost}:$ftpdir/allmodel/arch_mon/freq/$yyyy/.  
 fi
fi

fi    ;# end archmon
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

fi    ;# end makemap
done  ;# end reg
done  ;# end lev

#Copy scorecard text files
if [ $scorecard = "YES" ] ; then
  cp score_cor*.txt $scoredir
fi

done  ;# end vnam

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------



#-------------------------------------------------------
if [ $makemap = "YES" ]; then
#-------------------------------------------------------
# put HGT correlation dieoff maps for all waves together
set -A give $vnamlist
nva=`echo $vnamlist | wc -w`  ;#count number of variables
if [ ${give[0]} = "HGT" -a $nva -ge 4 ]; then
if [ ${give[3]} = "HGT_WV1/10-20" ]; then
  sleep 300
  topdir=${rundir}/${vtype}
  cd $topdir/HGTWV10-20 ||exit 8

  for lev  in $levlist ; do
  for reg  in $reglist ; do

    reg1=`echo $reg | sed "s?/??g"`
    tag=${lev}_${reg1}
    rm x1.png x2.png y1.png y2.png 
    $imgconvert $topdir/HGTWV10-20/cordieoff_HGTWV10-20_${tag}.png $topdir/HGTWV4-9/cordieoff_HGTWV4-9_${tag}.png  -rotate 90 -append x1.png
    $imgconvert $topdir/HGTWV0-3/cordieoff_HGTWV0-3_${tag}.png     $topdir/HGT/cordieoff_HGT_${tag}.png   -rotate 90 -append x2.png
    $imgconvert -rotate 270 -scale 70% x1.png y1.png
    $imgconvert -rotate 270 -scale 70% x2.png y2.png
    $imgconvert y2.png y1.png -append cordieoff_HGT_${tag}.png

chmod a+rw cordieoff_HGT*.png

#------------------------------------------------
  if [ $copymap = "YES" ]; then
    tmpdir=${mapdir}/allmodel/daily 
    mkdir -p $tmpdir/dieoff 
    cp *dieoff*.png  $tmpdir/dieoff/.
  fi
#-------------------------------------------------

cat << EOF >ftpdie_$lev$reg1
  binary
  cd $ftpdir/allmodel/daily/dieoff
  put cordieoff_HGT_${tag}.png 
  quit
EOF
if [ $doftp = "YES"  -a $CUE2RUN = $CUE2FTP ]; then 
 sftp  ${webhostid}@${webhost} <ftpdie_$$lev$reg1
 if [ $? -ne 0 ]; then
  scp -rp cordieoff_HGT_${tag}.png ${webhostid}@${webhost}:$ftpdir/allmodel/daily/dieoff/.
 fi
fi

if [ $archmon = "YES" ]; then
cat << EOF >ftpdiemon_$lev$reg1
  binary
  cd $ftpdir/allmodel/arch_mon/dieoff
  mkdir $yyyy
  cd $yyyy
  put cordieoff_HGT_${tag}${yyyymm}.png
  quit
EOF
if [ $doftp = "YES"  -a $CUE2RUN = $CUE2FTP ]; then 
 sftp  ${webhostid}@${webhost} <ftpdiemon_$lev$reg1 
 if [ $? -ne 0 ]; then
  scp -rp cordieoff_HGT_${tag}${yyyymm}.png ${webhostid}@${webhost}:$ftpdir/allmodel/arch_mon/dieoff/$yyyy/.
 fi
fi

fi

  done  ;# end reg
  done  ;# end lev
fi
fi
fi  ;#end makemap





#--------------------------------------------
##--send plots to web server using dedicated 
##--transfer node (required by NCEP WCOSS)
if [ $makemap = "YES" -a $doftp = "YES" -a $CUE2RUN != $CUE2FTP ]; then
#--------------------------------------------
cd ${rundir}
cat << EOF >ftpcard$$.sh 
#!/bin/ksh
set -x
for vnam in $vnamlist; do
 vnam1=\`echo \$vnam | sed "s?/??g" |sed "s?_WV1?WV?g"\`
 exedir=${rundir}/${vtype}/\$vnam1
 cd \$exedir
 for lev  in $levlist ; do
 for reg  in $reglist ; do
   reg1=\`echo \$reg | sed "s?/??g"\`
   if [ -s ftp_\$vnam1\$lev\$reg1 ]; then sftp  ${webhostid}@${webhost} <ftp_\$vnam1\$lev\$reg1 ; fi
   if [ -s ftpmon_\$vnam1\$lev\$reg1 ]; then sftp  ${webhostid}@${webhost} <ftpmon_\$vnam1\$lev\$reg1 ; fi
 done
 done
 if [ \$vnam1 = HGTWV10-20 ]; then
   for lev  in $levlist ; do
   for reg  in $reglist ; do
     reg1=\`echo \$reg | sed "s?/??g"\`
     if [ -s ftpdie_\$lev\$reg1 ]; then sftp  ${webhostid}@${webhost} <ftpdie_\$lev\$reg1 ; fi
     if [ -s ftpdiemon_\$lev\$reg1 ]; then sftp  ${webhostid}@${webhost} <ftpdiemon_\$lev\$reg1 ; fi
   done
   done
 fi
done
EOF
  chmod u+x $rundir/ftpcard$$.sh
  $SUBJOB -a $ACCOUNT -q $CUE2FTP -g $GROUP -p 1/1/S -t 0:30:00 -r 64/1 -j ftpcard -o ftpcard$$.out $rundir/ftpcard$$.sh
#--------------------------------------------
fi
#--------------------------------------------

date
exit

