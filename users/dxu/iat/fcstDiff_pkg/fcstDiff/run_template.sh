#!/bin/ksh
#----------------------------------------------------------
#                  verification
#  forecasts from a single cycle verified against multiple
#  analyses at corresponding forecast hour.
#               Fanglin Yang, feb 2008
#  Update history:
#  8/29/2014, Deyong Xu/RTI @NOAA/NESDIS/STAR/JCSDA
#     - Streamline the configuration by adding config file
#       to specify setting in one place.
#     - Consolidate sripts required for each experiment
#       into two: one for GFS model and one for ECMWF model.
#----------------------------------------------------------
set -x

# Call fcst diff configuration file. 
source ./fcstDiff_template.config

# Create run dir. 
rm -r $run_dir; mkdir -p $run_dir; cd $run_dir || exit 8

# Save output location where to extrac figures to generate PAR
echo ${run_dir}  > ${ENV_FCSTDIFF_PKG_DIR}/fcstDiff_data_loc

export yy=`echo $CDATE |cut -c 1-4 `
export mm=`echo $CDATE |cut -c 5-6 `
export dd=`echo $CDATE |cut -c 7-8 `

;#number of experiments
export nexp=`echo $explist |wc -w`        ;#number of experiments

;#number of panels on each plot, including multi-model mean/or two model differenc
export nplt=`expr $nexp + 1 `

set -A sname none $explist
set -A fname $fhrlist

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
for fhr in $fhrlist; do

## --find date of forecast hour,  get corresponding analysis 
export fhour=`echo $fhr |cut -c 2-5 `
fdate=`${ndate_dir}/ndate +$fhour ${CDATE}${cyc}`
anlcyc=`echo $fdate |cut -c 9-10 `
anlCDATE=`echo $fdate |cut -c 1-8 `
# Convert back to format like "02Aug2013"
anldate=`${help_srpt_dir}/datestamp.sh $anlCDATE -0 `

## convert source data to common grid and create grads control files
# 0-base array index
export index=0
for exp in $explist; do
  # 1-base index
  export index2=`expr $index + 1 `

  input_dir=${input_dir_arr[$index]}
  modelType=${modelTypeArr[$index]}
  para_list_1="${input_dir} ${run_dir} ${exp} ${cyc}    pgb${fhr} ${edate}   ${CDATE}    1" 
  para_list_2="${input_dir} ${run_dir} ${exp} ${anlcyc} pgbanl    ${anldate} ${anlCDATE} 1"
  # Add modelType to parameter list
  para_list_1="${para_list_1} ${modelType}"
  para_list_2="${para_list_2} ${modelType}"

  if [ "${modelTypeArr[$index]}" = 'gfs' || "${modelTypeArr[$index]}" = 'gdas' ]
  then 
     ${fcstDiff_dir}/ctl_gfs.sh $para_list_1
     export ctlfcst${index2}=${run_dir}/${exp}.t${cyc}z.pgb${fhr}.ctl

     ${fcstDiff_dir}/ctl_gfs.sh $para_list_2
     export ctlanl${index2}=${run_dir}/${exp}.t${anlcyc}z.pgbanl.ctl
  else
     ${fcstDiff_dir}/ctl_ecm.sh $para_list_1
     export ctlfcst${index2}=${run_dir}/${exp}.t${cyc}z.pgb${fhr}.ctl

     ${fcstDiff_dir}/ctl_ecm.sh $para_list_2
     export ctlanl${index2}=${run_dir}/${exp}.t${anlcyc}z.pgbanl.ctl
  fi  
  export index=`expr $index + 1 `
done


#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#for area in gb nh sh as eu na sa af; do
for area in ${arealist}; do

 if [ $area = "na" ];  then lat1=20 ; lat2=60; lon1=220; lon2=300; fi        ;# North America
 if [ $area = "nhe" ]; then lat1=0; lat2=90; lon1=-30  ; lon2=150; fi        ;# Eastern Northern Hemisphere
 if [ $area = "nhw" ]; then lat1=0; lat2=90; lon1=150  ; lon2=360; fi        ;# Western Northern Hemisphere
 if [ $area = "she" ]; then lat1=-90; lat2=0; lon1=-30  ; lon2=150; fi        ;# Eastern Northern Hemisphere
 if [ $area = "shw" ]; then lat1=-90; lat2=0; lon1=150  ; lon2=330; fi        ;# Western Northern Hemisphere

 if [ $area = "gb" ];  then lat1=-90; lat2=90; lon1=0  ; lon2=360; fi        ;# Entire Globe 
 if [ $area = "nh" ];  then lat1=20; lat2=80; lon1=0  ; lon2=360; fi        ;# Northern Hemisphere
 if [ $area = "sh" ];  then lat1=-90; lat2=-20; lon1=0  ; lon2=360; fi        ;# Southern Hemisphere
 if [ $area = "tr" ];  then lat1=-25; lat2=25; lon1=0  ; lon2=360; fi        ;# tropics
 if [ $area = "ua" ];  then lat1=10 ; lat2=75; lon1=-30; lon2=150 ; fi        ;# Eurasia
 if [ $area = "as" ];  then lat1=10 ; lat2=55; lon1=60 ; lon2=130; fi        ;# asian            
 if [ $area = "eu" ];  then lat1=35 ; lat2=70; lon1=-15; lon2=60 ; fi        ;# Europe            
 if [ $area = "sa" ];  then lat1=-55 ; lat2=13; lon1=275; lon2=330; fi        ;# South America       
 if [ $area = "af" ];  then lat1=-35; lat2=35; lon1=-25; lon2=60; fi        ;# Africa


#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# upper height and temperature
if [ $mapair = "yes" ]; then
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
for var in  TMPprs HGTprs UGRDprs VGRDprs;do
#for var in  HGTprs ;do
for lev in  1000 850 500 200; do

if [ $var = "TMPprs" ];   then varname="Temp (K)"           scal=1         trun=273.15  ; fi
if [ $var = "ABSVprs" ];  then varname="Vorticity"          scal=1000      trun=0.0     ; fi
if [ $var = "CLWMRprs" ]; then varname="Cloud Water (ppmg)" scal=1000000   trun=0.0     ; fi
if [ $var = "HGTprs" ];   then varname="HGT (m)"            scal=1         trun=0.0     ; fi
if [ $var = "O3MRprs" ];  then varname="O3 (ppmg)"          scal=1000000   trun=0.0     ; fi
if [ $var = "RHprs" ];    then varname="RH "                scal=1         trun=0.0     ; fi
if [ $var = "SPFHprs" ];  then varname="Q (k/kg)"           scal=1000      trun=0.0     ; fi
if [ $var = "UGRDprs" ];  then varname="U (m/s)"            scal=1         trun=0.0     ; fi
if [ $var = "VGRDprs" ];  then varname="V (m/s)"            scal=1         trun=0.0     ; fi
if [ $var = "VVELprs" ];  then varname="W (mb/hr)"          scal=24*36     trun=0.0     ; fi


cat >${var}${lev}_${area}${fhr}_t${cyc}z.gs <<EOF1 
'reinit'; 'set font 1'
'run ${help_srpt_dir}/initial.gs'
                 'open $ctlanl1'; 'open $ctlfcst1'
  if ($nexp >1); 'open $ctlanl2'; 'open $ctlfcst2'; endif
  if ($nexp >2); 'open $ctlanl3'; 'open $ctlfcst3'; endif
  if ($nexp >3); 'open $ctlanl4'; 'open $ctlfcst4'; endif
  if ($nexp >4); 'open $ctlanl5'; 'open $ctlfcst5'; endif
  if ($nexp >5); 'open $ctlanl6'; 'open $ctlfcst6'; endif
  if ($nexp >6); 'open $ctlanl7'; 'open $ctlfcst7'; endif

*-----
  'set lat $lat1 $lat2'
  'set lon $lon1 $lon2'
  'set lev $lev'
  'set t 1  '
* 'set time $anldate  '

*--multi-model means
  'define an${nplt}=0.'               ;*mean analysis
  'define fc${nplt}=0.'               ;*mean forecast
  'define sn${nplt}=0.'               ;*mean forecast-analyses

i=1; m=1; n=2 
while ( i <= ${nexp} )
  'define an'%i'=${scal}*${var}.'%m'(time=${anlcyc}Z${anldate})-${trun}'  
  'define fc'%i'=${scal}*${var}.'%n'(time=${cyc}Z${edate})-${trun}'  
  'define sn'%i'=fc'%i'-an'%i                                
  'define an${nplt}  =an${nplt}+an'%i'/${nexp} '
  'define fc${nplt}  =fc${nplt}+fc'%i'/${nexp} '
  'define sn${nplt}  =sn${nplt}+sn'%i'/${nexp} '
 i=i+1; m=m+2; n=n+2
endwhile
  'define fcdiff  =fc1-fc2'                  

*------------------------
*--find maximum and minmum values of analyses/forecast
   cmax=-10000000.0; cmin=10000000.0
   i=1
   while (i <= $nplt)
    'set gxout stat'
    'd an'%i 
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if( b>0 )
      if(zmax > cmax); cmax=zmax; endif
      if(zmin < cmin); cmin=zmin; endif
    endif
   i=i+1
   endwhile
   dist=cmax-cmin; cmin=cmin-0.1*dist; cmax=cmax+0.1*dist
   cmin=substr(cmin,1,5); cmax=substr(cmax,1,5); cint=10*substr((cmax-cmin)/100,1,3)
   if (cint = 0); cint=substr((cmax-cmin)/10,1,3); endif
   if (cint = 0); cint=0.1*substr((cmax-cmin),1,3); endif
   if (cint = 0); cint=0.01*substr((cmax-cmin)*10,1,3); endif
   say 'cmin cmax cint 'cmin' 'cmax' 'cint
    aa1=cmin; aa2=cmin+cint; aa3=aa2+cint; aa4=aa3+cint; aa5=aa4+cint; aa6=aa5+cint
    aa7=aa6+cint; aa8=aa7+cint; aa9=aa8+cint; aa10=aa9+cint; aa11=aa10+cint

*------------------------
*--find maximum and minmum values for difference map
   cmax=-10000000.0; cmin=10000000.0
   i=1
   while (i <= $nplt)
    'set gxout stat'
    'd sn'%i
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if( b>0 )
      if(zmax > cmax); cmax=zmax; endif
      if(zmin < cmin); cmin=zmin; endif
    endif
   i=i+1
   endwhile
   dist=cmax-cmin; cmin=cmin+0.1*dist; cmax=cmax-0.1*dist
   if ( $fhr = "f00" )
     cmin=cmin-2*dist
     cmax=cmax+2*dist
   endif
   if(cmin >=0); cmin=-0.1*dist;endif
   if(cmax <=0); cmax=0.1*dist; endif
   cmin=substr(cmin,1,4); cmax=substr(cmax,1,4);
   cintm=10*substr(cmin/50,1,3)
     if (cintm = 0); cintm=substr(cmin/5,1,3); endif
     if (cintm = 0); cintm=0.2*substr(cmin,1,3); endif
     if (cintm = 0); cintm=0.02*substr(cmin*10,1,3); endif
     if (cintm = 0); cintm=0.002*substr(cmin*100,1,3); endif
   cms=0.5*cintm; cm1=cintm; cm2=cm1+cintm; cm3=cm2+cintm; cm4=cm3+cintm; cm5=cm4+cintm
              cm6=cm5+cintm; cm7=cm6+cintm; cm8=cm7+cintm; cm9=cm8+cintm
   cintp=10*substr(cmax/50,1,3)
     if (cintp = 0); cintp=substr(cmax/5,1,3); endif
     if (cintp = 0); cintp=0.2*substr(cmax,1,3); endif
     if (cintp = 0); cintp=0.02*substr(cmax*10,1,3); endif
     if (cintp = 0); cintp=0.002*substr(cmax*100,1,3); endif
   cps=0.5*cintp; cp1=cintp; cp2=cp1+cintp; cp3=cp2+cintp; cp4=cp3+cintp; cp5=cp4+cintp
              cp6=cp5+cintp; cp7=cp6+cintp; cp8=cp7+cintp; cp9=cp8+cintp
   say 'cmin cmax cintm cintp 'cmin' 'cmax' 'cintm' 'cintp
*------------------------

  xmin0=0.9;  xlen=6.5;  xgap=0.2
  ymax0=9.4;  ylen=-2.5; ygap=-0.4; 
              nframe2=3;  nframe3=6
* if($nplt >2); nframe2=2;  nframe3=4; ylen=-4.0; endif
* if($nplt >4); nframe2=3;  nframe3=6; ylen=-2.6; endif
* if($nplt >6); nframe2=4;  nframe3=8; ylen=-1.8; endif

i=1
while ( i <= $nplt )
 'set gxout stat'
 'd sn'%i
 ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
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
    titlx=xmin+0.5
    titly=ymax+0.08
    'set parea 'xmin' 'xmax' 'ymin' 'ymax

    'run ${help_srpt_dir}/rgbset.gs'
    'set xlopts 1 4 0.0'
    'set ylopts 1 4 0.0'
      if($nplt <=3)
        if(i<=3);'set ylopts 1 4 0.12';endif
        if(i=3|i=$nplt);'set xlopts 1 4 0.12';endif
*       'set xlopts 1 4 0.13'
*       if(i=1);'set ylopts 1 4 0.13';endif
      endif
      if($nplt >3 & $nplt <=6)
        if(i=3|i=$nplt);'set xlopts 1 4 0.12';endif
        if(i<=3);'set ylopts 1 4 0.12';endif
      endif
      if($nplt >4 & $nplt <=6)
        if(i=3|i=$nplt);'set xlopts 1 4 0.11';endif
        if(i<=3);'set ylopts 1 4 0.11';endif
      endif
      if($nplt >=7)
        if(i=4|i=$nplt);'set xlopts 1 4 0.11';endif
        if(i<=4);'set ylopts 1 4 0.11';endif
      endif
    'set clopts 1 4 0.07'
    'set grid off'
*   'set zlog on'
    'set map 46 1 4'
    'set mproj scaled'
*   'set mpdset mres'
*   if ( $area = nh ); 'set mproj nps'; 'set mpvals -270 90 $lat1 $lat2'; 'set grid on'; endif
*   if ( $area = sh ); 'set mproj sps'; 'set mpvals -270 90 $lat1 $lat2'; 'set grid on'; endif

    'set gxout shaded'
    'set grads off'
    'set xlint 30'; 'set ylint 10'
    if($area = gb); 'set xlint 60'; 'set ylint 30';endif
    if($area = tr); 'set xlint 60'; 'set ylint 10';endif
    if($area = na | $area = eu | $area = as | $area = sa | $area = af); 'set xlint 10'; 'set ylint 10';endif
  if($var = "TMPprs" & $fhr = "f00")
    'set clevs  -8 -5 -3 -1 -0.5 0.5 1 3 5 8'
    'set rbcols 39    37    35    33     31    0    61    63  65     67   69'
  else
    'set clevs   'cm5' 'cm4' 'cm3' 'cm2' 'cm1' 'cp1' 'cp2' 'cp3' 'cp4' 'cp5
    'set rbcols 39    37    35    33     31    0    61    63  65     67   69'
  endif
    if( b>0 & i <=2); 'd sn'%i ;endif
    if( b>0 & i =3); 'd fcdiff' ;endif

    'set gxout contour'
    'set grads off'
    'set ccolor 15'
*   'set clab forced'
    'set cstyle 1'
    'set cthick 4'
    'set clopts 1 4 0.'
    'set clevs   'aa1' 'aa2' 'aa3' 'aa4' 'aa5' 'aa6' 'aa7' 'aa8' 'aa9' 'aa10' 'aa11 
*   if( b>0); 'd an'%i; endif 

    'set gxout contour'
    'set grads off'
    'set ccolor 1'
*   'set clab forced'
    'set cstyle 1'
    'set cthick 5'
    'set clopts 1 4 0.08'
    'set clevs   'aa1' 'aa2' 'aa3' 'aa4' 'aa5' 'aa6' 'aa7' 'aa8' 'aa9' 'aa10' 'aa11 
    if( b>0 & i<=2); 'd fc'%i; endif 
*
    'set string 59 bl 7'
    'set strsiz 0.17 0.17'
    if(i=1);'draw string 'titlx' 'titly ' ${sname[1]} ';endif
    if(i=2);'draw string 'titlx' 'titly ' ${sname[2]} ';endif
    if(i=3);'draw string 'titlx' 'titly ' ${sname[3]} ';endif
    if(i=4);'draw string 'titlx' 'titly ' ${sname[4]} ';endif
    if(i=5);'draw string 'titlx' 'titly ' ${sname[5]} ';endif
    if(i=6);'draw string 'titlx' 'titly ' ${sname[6]} ';endif
    if(i=7);'draw string 'titlx' 'titly ' ${sname[7]} ';endif
    if(i=${nplt});'draw string 'titlx' 'titly ' FCST: ${sname[1]} - ${sname[2]}';endif
i=i+1
endwhile

  'set string 4 bc 6'
  'set strsiz 0.14 0.14'
  'draw string 4.3 10.35 ${varname}, ${lev}hPa, ${CDATE}${cyc} Cycle, Fcst Hour ${fhr}'
  'draw string 4.3 10.1 Verification Time: ${fdate}'
  'set string 1 bc 4'
  'set strsiz 0.13 0.13'
  'draw string 4.3 9.8  Contour: FCST; Color: FCST-ANL'
  'set string 1 bc 6'
  'set strsiz 0.16 0.16'
  if($nexp >1)
    'run ${help_srpt_dir}/cbarn.gs 0.95 0 4.1 0.28'
   else
    'run ${help_srpt_dir}/cbarn.gs 0.95 0 4.1 4.70'
   endif

  'printim ${var}${lev}_${area}${fhr}_t${cyc}z.gif gif x700 y700'
  'set vpage off'
'quit'
EOF1
grads -bcp "run ${var}${lev}_${area}${fhr}_t${cyc}z.gs"

## --  daily rotatigng display
if [ $doftp = "YES" ]; then 

cat << EOF >ftpin
  binary
  cd /home/people/emc/www/htdocs/gmb/$LOGNAME/directory
  mkdir $ftpdir
  cd $ftpdir
    mput ${var}${lev}_${area}${fhr}_t${cyc}z.gif     
  quit
EOF
sftp emcrzdm.ncep.noaa.gov <ftpin 
fi


#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
done   ;# end of level
done   ;# end of var
fi
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# vector wind                            
if [ $mapuv = "yes" ]; then
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
export var=wind; export scal=1.0; export trun=0.
for lev in 1000 850 500 200 ; do

cat >${var}${lev}_${area}${fhr}_t${cyc}z.gs <<EOF1 
'reinit'; 'set font 1'
*'run ${help_srpt_dir}/white.gs'
                 'open $ctlanl1'; 'open $ctlfcst1'
  if ($nexp >1); 'open $ctlanl2'; 'open $ctlfcst2'; endif
  if ($nexp >2); 'open $ctlanl3'; 'open $ctlfcst3'; endif
  if ($nexp >3); 'open $ctlanl4'; 'open $ctlfcst4'; endif
  if ($nexp >4); 'open $ctlanl5'; 'open $ctlfcst5'; endif
  if ($nexp >5); 'open $ctlanl6'; 'open $ctlfcst6'; endif
  if ($nexp >6); 'open $ctlanl7'; 'open $ctlfcst7'; endif

*-----
  'set lat $lat1 $lat2'
  'set lon $lon1 $lon2'
  'set lev $lev'
  'set t 1 '
* 'set time $anldate '

*--multi-model means
  'define anu${nplt}=0.'               ;*mean analysis
  'define fcu${nplt}=0.'               ;*mean forecast
  'define snu${nplt}=0.'               ;*mean forecast-analyses
  'define anv${nplt}=0.'               ;*mean analysis
  'define fcv${nplt}=0.'               ;*mean forecast
  'define snv${nplt}=0.'               ;*mean forecast-analyses

i=1; m=1; n=2 
while ( i <= ${nexp} )
  'define anu'%i'=${scal}*ugrdprs.'%m'(time=${anlcyc}Z${anldate})-${trun}'  
  'define fcu'%i'=${scal}*ugrdprs.'%n'(time=${cyc}Z${edate})-${trun}'  
  'define snu'%i'=fcu'%i'-anu'%i                                
  'define anu${nplt}  =anu${nplt}+anu'%i'/${nexp} '
  'define fcu${nplt}  =fcu${nplt}+fcu'%i'/${nexp} '
  'define snu${nplt}  =snu${nplt}+snu'%i'/${nexp} '

  'define anv'%i'=${scal}*vgrdprs.'%m'(time=${anlcyc}Z${anldate})-${trun}'  
  'define fcv'%i'=${scal}*vgrdprs.'%n'(time=${cyc}Z${edate})-${trun}'  
  'define snv'%i'=fcv'%i'-anv'%i                                
  'define anv${nplt}  =anv${nplt}+anv'%i'/${nexp} '
  'define fcv${nplt}  =fcv${nplt}+fcv'%i'/${nexp} '
  'define snv${nplt}  =snv${nplt}+snv'%i'/${nexp} '
 i=i+1; m=m+2; n=n+2
endwhile


*------------------------
*--find maximum and minmum values of wind magnitude      
   cmax=-10000000.0; cmin=10000000.0
   i=1
    'set gxout stat'
    'd mag(fcu'%i',fcv'%i')'
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if( b>0 )
      if(zmax > cmax); cmax=zmax; endif
      if(zmin < cmin); cmin=zmin; endif
    endif
   dist=cmax-cmin; cmin=cmin+0.1*dist; cmax=cmax-0.1*dist
   cmin=substr(cmin,1,6); cmax=substr(cmax,1,6); cint=10*substr((cmax-cmin)/100,1,4)
   if (cint = 0); cint=substr((cmax-cmin)/10,1,4); endif
   if (cint = 0); cint=0.1*substr((cmax-cmin),1,4); endif
   if (cint = 0); cint=0.01*substr((cmax-cmin)*10,1,4); endif
   say 'cmin cmax cint 'cmin' 'cmax' 'cint
    aa1=cmin; aa2=cmin+cint; aa3=aa2+cint; aa4=aa3+cint; aa5=aa4+cint; aa6=aa5+cint
    aa7=aa6+cint; aa8=aa7+cint; aa9=aa8+cint; aa10=aa9+cint; aa11=aa10+cint

*------------------------
*--find maximum and minmum values for difference map
   cmax=-10000000.0; cmin=10000000.0
   i=1
   while (i <= $nplt)
    'set gxout stat'
    'd mag(fcu'%i',fcv'%i')- mag(anu'%i',anv'%i')'
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > cmax); cmax=zmax; endif
    if(zmin < cmin); cmin=zmin; endif
   i=i+2
   endwhile
   dist=cmax-cmin; cmin=cmin+0.2*dist; cmax=cmax-0.2*dist
   if(cmin >=0); cmin=-0.1*dist;endif
   if(cmax <=0); cmax=0.1*dist; endif
   cmin=substr(cmin,1,6); cmax=substr(cmax,1,6);
   cintm=10*substr(cmin/50,1,4)
     if (cintm = 0); cintm=substr(cmin/5,1,4); endif
     if (cintm = 0); cintm=0.2*substr(cmin,1,4); endif
     if (cintm = 0); cintm=0.02*substr(cmin*10,1,4); endif
     if (cintm = 0); cintm=0.002*substr(cmin*100,1,4); endif
   cms=0.5*cintm; cm1=cintm; cm2=cm1+cintm; cm3=cm2+cintm; cm4=cm3+cintm; cm5=cm4+cintm
              cm6=cm5+cintm; cm7=cm6+cintm; cm8=cm7+cintm; cm9=cm8+cintm
   cintp=10*substr(cmax/50,1,4)
     if (cintp = 0); cintp=substr(cmax/5,1,4); endif
     if (cintp = 0); cintp=0.2*substr(cmax,1,4); endif
     if (cintp = 0); cintp=0.02*substr(cmax*10,1,4); endif
     if (cintp = 0); cintp=0.002*substr(cmax*100,1,4); endif
   cps=0.5*cintp; cp1=cintp; cp2=cp1+cintp; cp3=cp2+cintp; cp4=cp3+cintp; cp5=cp4+cintp
              cp6=cp5+cintp; cp7=cp6+cintp; cp8=cp7+cintp; cp9=cp8+cintp
   say 'cmin cmax cintm cintp 'cmin' 'cmax' 'cintm' 'cintp
*------------------------

*------------------------
  xmin0=0.7;  xlen=3.5;  xgap=0.2
  ymax0=9.4;  ylen=-4.0; ygap=-0.4; 
              nframe2=1;  nframe3=2
  if($nplt >2); nframe2=2;  nframe3=4; ylen=-4.0; endif
  if($nplt >4); nframe2=3;  nframe3=6; ylen=-2.6; endif
  if($nplt >6); nframe2=4;  nframe3=8; ylen=-1.8; endif

i=1
while ( i <= $nplt )
 'set gxout stat'
 'd snu'%i
 ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
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
    titlx=xmin+0.5
    titly=ymax+0.08
    'set parea 'xmin' 'xmax' 'ymin' 'ymax

*    'run ${help_srpt_dir}/rgbset.gs'
    'set xlopts 1 4 0.0'
    'set ylopts 1 4 0.0'
      if($nplt <=2)
        'set xlopts 1 4 0.13'
        if(i=1);'set ylopts 1 4 0.13';endif
      endif
      if($nplt >2 & $nplt <=4)
        if(i=2|i=$nplt);'set xlopts 1 4 0.12';endif
        if(i<=2);'set ylopts 1 4 0.12';endif
      endif
      if($nplt >4 & $nplt <=6)
        if(i=3|i=$nplt);'set xlopts 1 4 0.11';endif
        if(i<=3);'set ylopts 1 4 0.11';endif
      endif
      if($nplt >=7)
        if(i=4|i=$nplt);'set xlopts 1 4 0.11';endif
        if(i<=4);'set ylopts 1 4 0.11';endif
      endif
    'set clopts 1 4 0.07'
    'set grid off'
*   'set zlog on'
    'set map 7 1 4'
    'set mproj scaled'
*   'set mpdset mres'
*   if ( $area = nh ); 'set mproj nps'; 'set mpvals -270 90 $lat1 $lat2'; 'set grid on'; endif
*   if ( $area = sh ); 'set mproj sps'; 'set mpvals -270 90 $lat1 $lat2'; 'set grid on'; endif

    'set gxout shaded'
    'set grads off'
    'set xlint 30'; 'set ylint 10'
    if($area = gb); 'set xlint 60'; 'set ylint 30';endif
    if($area = tr); 'set xlint 60'; 'set ylint 10';endif
    if($area = na | $area = eu | $area = as | $area = sa | $area = af); 'set xlint 10'; 'set ylint 10';endif
    'set clevs   'cm5' 'cm4' 'cm3' 'cm2' 'cm1' 'cp1' 'cp2' 'cp3' 'cp4' 'cp5
    'set rbcols 39    37    35    33     31    0    61    63  65     67   69'
    if( b>0 ); 'd mag(fcu'%i',fcv'%i')- mag(anu'%i',anv'%i')'; endif

    'set gxout vector'
    'set grads off'
    'set xlint 30'; 'set ylint 10'
    if($area = gb); 'set xlint 60'; 'set ylint 30';endif
    if($area = tr); 'set xlint 60'; 'set ylint 10';endif
    if($area = na | $area = eu | $area = as | $area = sa | $area = af); 'set xlint 10'; 'set ylint 10';endif
    'set arrscl 0.7 'aa11
    'set arrlab off'
    if( i=${nplt}); set arrlab on; endif
    'set ccolor 49'
    'set cstyle 1'
    'set cthick 4'
    if( b>0 )
      if($area = gb | $area = nh | $area = sh)
       'd skip(anu'%i',4,4);skip(anv'%i',4,4)' 
      else
       'd skip(anu'%i',2,2);skip(anv'%i',2,2)'
      endif
    endif

    'set gxout vector'
    'set grads off'
*   'set clab forced'
    'set arrscl 0.7 'aa11
    'set arrlab off'
    'set ccolor 1'
    'set cstyle 1'
    'set cthick 4'
    'set clopts 1 4 0.08'
    if( b>0 )
      if($area = gb | $area = nh | $area = sh)
       'd skip(fcu'%i',4,4);skip(fcv'%i',4,4)' 
      else
       'd skip(fcu'%i',2,2);skip(fcv'%i',2,2)'
      endif
    endif
*
    'draw map'
    'set string 59 bl 7'
    'set strsiz 0.17 0.17'
    if(i=1);'draw string 'titlx' 'titly ' ${sname[1]} ';endif
    if(i=2);'draw string 'titlx' 'titly ' ${sname[2]} ';endif
    if(i=3);'draw string 'titlx' 'titly ' ${sname[3]} ';endif
    if(i=4);'draw string 'titlx' 'titly ' ${sname[4]} ';endif
    if(i=5);'draw string 'titlx' 'titly ' ${sname[5]} ';endif
    if(i=6);'draw string 'titlx' 'titly ' ${sname[6]} ';endif
    if(i=7);'draw string 'titlx' 'titly ' ${sname[7]} ';endif
    if(i=${nplt});'draw string 'titlx' 'titly ' Multi_model Mean';endif
*   if(i=${nplt});'draw string 'titlx' 'titly ' FCST: GFS - ECM';endif
i=i+1
endwhile

  'set string 4 bc 6'
  'set strsiz 0.14 0.14'
  'draw string 4.3 10.35 Wind, ${lev}hPa, ${CDATE}${cyc} Cycle, Fcst Hour ${fhr}'
  'draw string 4.3 10.1 Verification Time: ${fdate}'
  'set string 1 bc 4'
  'set strsiz 0.13 0.13'
  'draw string 4.3 9.8  black: FCST; blue: ANL; shade: wind speed bias'
  'set string 1 bc 6'
  'set strsiz 0.16 0.16'
  if($nexp >1)
    'run ${help_srpt_dir}/cbarn.gs 0.95 0 4.3 0.28'
   else
    'run ${help_srpt_dir}/cbarn.gs 0.95 0 4.3 4.70'
   endif
  'set vpage off'
  'printim ${var}${lev}_${area}${fhr}_t${cyc}z.gif gif x700 y700'
'quit'
EOF1
grads -bcp "run ${var}${lev}_${area}${fhr}_t${cyc}z.gs"


## --  daily rotating display
if [ $doftp = "YES" ]; then 
cat << EOF >ftpin
  binary
  mkdir $ftpdir
  cd $ftpdir
    put ${var}${lev}_${area}${fhr}_t${cyc}z.gif     
  quit
EOF
sftp emcrzdm.ncep.noaa.gov <ftpin 
fi
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
done   ;# end of vector wind
fi
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# surface variables: sea-level pressure
if [ $mapsfc = "yes" ]; then
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#for var in  \
# GFLUXsfc  LHTFLsfc  PEVPRsfc SHTFLsfc  \
# PWATclm  CWATclm PRATEsfc CPRATsfc  \
# SOILW0_10cm  SOILW10_40cm  SOILW40_100cm  SOILW100_200cm  \
# TMP0_10cm  TMP10_40cm  TMP40_100cm  TMP100_200cm CNWATsfc  \
# WATRsfc WEASDsfc SNODsfc TOZNEclm  \
# TMAX2m TMIN2m TMPsfc  TMP2m  RH2m  SPFH2m \
# PRESsfc  PRMSLmsl HGTsfc  HPBLsfc HGTtrp   \
# UGWDsfc VGWDsfc UGRD10m VGRD10m
#do

for var in  PRMSLmsl; do

if [ $var = "UGRD10m" ];  then varname="U10m [m/s]"           trun=0   scal=1 ; fi
if [ $var = "VGRD10m" ];  then varname="V10m [m/s]"           trun=0   scal=1 ; fi
if [ $var = "UGWDsfc" ];  then varname="sfc U_GWD [N/m2]"           trun=0   scal=1 ; fi
if [ $var = "VGWDsfc" ];  then varname="sfc V_GWD [N/m2]"           trun=0   scal=1 ; fi
if [ $var = "WATRsfc" ];  then varname="Water runoff [g/m2]"           trun=0   scal=1000 ; fi
if [ $var = "WEASDsfc" ]; then varname="Accum. snow [Ton/m2]"          trun=0   scal=0.001 ; fi
if [ $var = "SNODsfc" ];  then varname="Snow depth [m]"                 trun=0   scal=1 ; fi
if [ $var = "TOZNEclm" ]; then varname="Column O3 [Dobson]"             trun=0   scal=1 ; fi
if [ $var = "TMAX2m" ];   then varname="Max T2m [K]"                    trun=273.15   scal=1 ; fi
if [ $var = "TMIN2m" ];   then varname="MIN T2m [K]"                    trun=273.15   scal=1 ; fi
if [ $var = "TMP2m" ];    then varname="T2m [K]"                        trun=273.15   scal=1 ; fi
if [ $var = "TMPsfc" ];   then varname="Skin Temp [K]"                  trun=273.15   scal=1 ; fi
if [ $var = "RH2m" ];     then varname="RH2m "                          trun=0   scal=1 ; fi
if [ $var = "SPFH2m" ];   then varname="Q2m [g/kg]"                     trun=0   scal=1000 ; fi
if [ $var = "GFLUXsfc" ]; then varname="Gnd Heat Flux [W/m2]"           trun=0   scal=1 ; fi
if [ $var = "LHTFLsfc" ];  then varname="Latent Heat Flux [W/m2]"        trun=0   scal=1 ; fi
if [ $var = "SHTFLsfc" ];  then varname="Sensible Heat Flux [W/m2]"      trun=0   scal=1 ; fi
if [ $var = "PEVPRsfc" ];  then varname="Poten Evap [W/m2]"              trun=0   scal=1 ; fi
if [ $var = "PWATclm" ];   then varname="Column Precip Water [kg/m2]"    trun=0   scal=1 ; fi
if [ $var = "CWATclm" ];   then varname="Column CLoud Water [kg/m2]"     trun=0   scal=1 ; fi
if [ $var = "PRATEsfc" ];  then varname="Precip Rate [mm/day]"           trun=0   scal=24*3600 ; fi
if [ $var = "CRATsfc" ];   then varname="Conv. Precip Rate [mm/day]"     trun=0   scal=24*3600 ; fi
if [ $var = "SOILW0_10cm" ];    then varname="Soil Moist 0-10cm"         trun=0   scal=100 ; fi
if [ $var = "SOILW10_40cm" ];   then varname="Soil Moist 10-40cm"        trun=0   scal=100 ; fi
if [ $var = "SOILW40_100cm" ];  then varname="Soil Moist 40-100cm"       trun=0   scal=100 ; fi
if [ $var = "SOILW100_200cm" ]; then varname="Soil Moist 100-200cm"      trun=0   scal=100 ; fi
if [ $var = "TMP0_10cm" ];      then varname="Soil Temp 0-10cm [K]"      trun=0   scal=1 ; fi
if [ $var = "TMP10_40cm" ];     then varname="Soil Temp 10-40cm [K]"     trun=0   scal=1 ; fi
if [ $var = "TMP40_100cm" ];    then varname="Soil Temp 40-100cm [K]"    trun=0   scal=1 ; fi
if [ $var = "TMP100_200cm" ];   then varname="Soil Temp 100-200cm [K]"   trun=0   scal=1 ; fi
if [ $var = "CNWATsfc" ];       then varname="Canopy Sfc Water [kg/m2]"  trun=0   scal=1 ; fi
if [ $var = "PRESsfc" ];        then varname="Sfc Pres [hPa]"            trun=0   scal=0.01 ; fi
if [ $var = "PRMSLmsl" ];       then varname="Sea-Level Pres [hPa]"      trun=0   scal=0.01 ; fi
if [ $var = "HGTsfc" ];         then varname="Sfc HGT [m]"               trun=0   scal=1 ; fi
if [ $var = "HPBLsfc" ];        then varname="PBL HGT [m]"               trun=0   scal=1 ; fi
if [ $var = "HGTtrp" ];         then varname="Tropopause HGT [m]"        trun=0   scal=1 ; fi


cat >${var}_${area}${fhr}_t${cyc}z.gs <<EOF1 
'reinit'; 'set font 1'
*'run ${help_srpt_dir}/white.gs'
                 'open $ctlanl1'; 'open $ctlfcst1'
  if ($nexp >1); 'open $ctlanl2'; 'open $ctlfcst2'; endif
  if ($nexp >2); 'open $ctlanl3'; 'open $ctlfcst3'; endif
  if ($nexp >3); 'open $ctlanl4'; 'open $ctlfcst4'; endif
  if ($nexp >4); 'open $ctlanl5'; 'open $ctlfcst5'; endif
  if ($nexp >5); 'open $ctlanl6'; 'open $ctlfcst6'; endif
  if ($nexp >6); 'open $ctlanl7'; 'open $ctlfcst7'; endif

*-----
  'set lat $lat1 $lat2'
  'set lon $lon1 $lon2'
  'set lev 1000'
  'set t 1 '
* 'set time $anldate '

*--multi-model means
  'define an${nplt}=0.'               ;*mean analysis
  'define fc${nplt}=0.'               ;*mean forecast
  'define sn${nplt}=0.'               ;*mean forecast-analyses

i=1; m=1; n=2 
while ( i <= ${nexp} )
  'define an'%i'=${scal}*${var}.'%m'(time=${anlcyc}Z${anldate})-${trun}'
  'define fc'%i'=${scal}*${var}.'%n'(time=${cyc}Z${edate})-${trun}'
* 'define an'%i'=${scal}*${var}.'%m'-${trun}'  
* 'define fc'%i'=${scal}*${var}.'%n'-${trun}'  
  'define sn'%i'=fc'%i'-an'%i                                
  'define an${nplt}  =an${nplt}+an'%i'/${nexp} '
  'define fc${nplt}  =fc${nplt}+fc'%i'/${nexp} '
  'define sn${nplt}  =sn${nplt}+sn'%i'/${nexp} '
 i=i+1; m=m+2; n=n+2
endwhile

*------------------------
*--find maximum and minmum values of analyses/forecast
   cmax=-10000000.0; cmin=10000000.0
   i=1
   while (i <= $nexp)
    'set gxout stat'
    'd an'%i 
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if( b>0 )
      if(zmax > cmax); cmax=zmax; endif
      if(zmin < cmin); cmin=zmin; endif
    endif
   i=i+1
   endwhile
   dist=cmax-cmin; cmin=cmin-0.1*dist; cmax=cmax+0.1*dist
   cmin=substr(cmin,1,4); cmax=substr(cmax,1,5); cint=10*substr((cmax-cmin)/100,1,3)
   if (cint = 0); cint=substr((cmax-cmin)/10,1,3); endif
   if (cint = 0); cint=0.1*substr((cmax-cmin),1,3); endif
   if (cint = 0); cint=0.01*substr((cmax-cmin)*10,1,3); endif
   say 'cmin cmax cint 'cmin' 'cmax' 'cint
    aa1=cmin; aa2=cmin+cint; aa3=aa2+cint; aa4=aa3+cint; aa5=aa4+cint; aa6=aa5+cint
    aa7=aa6+cint; aa8=aa7+cint; aa9=aa8+cint; aa10=aa9+cint; aa11=aa10+cint

*------------------------
*--find maximum and minmum values for difference map
   cmax=-10000000.0; cmin=10000000.0
   i=1
   while (i <= $nexp)
    'set gxout stat'
    'd sn'%i
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if( b>0 )
      if(zmax > cmax); cmax=zmax; endif
      if(zmin < cmin); cmin=zmin; endif
    endif
   i=i+1
   endwhile
   dist=cmax-cmin; cmin=cmin+0.1*dist; cmax=cmax-0.1*dist
   if(cmin >=0); cmin=-0.1*dist;endif
   if(cmax <=0); cmax=0.1*dist; endif
   cmin=substr(cmin,1,4); cmax=substr(cmax,1,4);
   cintm=10*substr(cmin/50,1,3)
     if (cintm = 0); cintm=substr(cmin/5,1,3); endif
     if (cintm = 0); cintm=0.2*substr(cmin,1,3); endif
     if (cintm = 0); cintm=0.02*substr(cmin*10,1,3); endif
     if (cintm = 0); cintm=0.002*substr(cmin*100,1,3); endif
   cms=0.5*cintm; cm1=cintm; cm2=cm1+cintm; cm3=cm2+cintm; cm4=cm3+cintm; cm5=cm4+cintm
              cm6=cm5+cintm; cm7=cm6+cintm; cm8=cm7+cintm; cm9=cm8+cintm
   cintp=10*substr(cmax/50,1,3)
     if (cintp = 0); cintp=substr(cmax/5,1,3); endif
     if (cintp = 0); cintp=0.2*substr(cmax,1,3); endif
     if (cintp = 0); cintp=0.02*substr(cmax*10,1,3); endif
     if (cintp = 0); cintp=0.002*substr(cmax*100,1,3); endif
   cps=0.5*cintp; cp1=cintp; cp2=cp1+cintp; cp3=cp2+cintp; cp4=cp3+cintp; cp5=cp4+cintp
              cp6=cp5+cintp; cp7=cp6+cintp; cp8=cp7+cintp; cp9=cp8+cintp
   say 'cmin cmax cintm cintp 'cmin' 'cmax' 'cintm' 'cintp
*------------------------

  xmin0=0.9;  xlen=3.5;  xgap=0.2
  ymax0=9.4;  ylen=-4.0; ygap=-0.4; 
              nframe2=1;  nframe3=2
  if($nplt >2); nframe2=2;  nframe3=4; ylen=-4.0; endif
  if($nplt >4); nframe2=3;  nframe3=6; ylen=-2.6; endif
  if($nplt >6); nframe2=4;  nframe3=8; ylen=-1.8; endif

i=1
while ( i <= $nplt )
 'set gxout stat'
 'd sn'%i
 ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
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
    titlx=xmin+0.5
    titly=ymax+0.08
    'set parea 'xmin' 'xmax' 'ymin' 'ymax

    'run ${help_srpt_dir}/rgbset.gs'
    'set xlopts 1 4 0.0'
    'set ylopts 1 4 0.0'
      if($nplt <=2)
        'set xlopts 1 4 0.13'
        if(i=1);'set ylopts 1 4 0.13';endif
      endif
      if($nplt >2 & $nplt <=4)
        if(i=2|i=$nplt);'set xlopts 1 4 0.12';endif
        if(i<=2);'set ylopts 1 4 0.12';endif
      endif
      if($nplt >4 & $nplt <=6)
        if(i=3|i=$nplt);'set xlopts 1 4 0.11';endif
        if(i<=3);'set ylopts 1 4 0.11';endif
      endif
      if($nplt >=7)
        if(i=4|i=$nplt);'set xlopts 1 4 0.11';endif
        if(i<=4);'set ylopts 1 4 0.11';endif
      endif
    'set clopts 1 4 0.07'
    'set grid off'
*   'set zlog on'
    'set map 46 1 4'
    'set mproj scaled'
*   'set mpdset mres'
*   if ( $area = nh ); 'set mproj nps'; 'set mpvals -270 90 $lat1 $lat2'; 'set grid on'; endif
*   if ( $area = sh ); 'set mproj sps'; 'set mpvals -270 90 $lat1 $lat2'; 'set grid on'; endif

    'set gxout shaded'
    'set grads off'
    'set xlint 30'; 'set ylint 10'
    if($area = gb); 'set xlint 60'; 'set ylint 30';endif
    if($area = tr); 'set xlint 60'; 'set ylint 10';endif
    if($area = na | $area = eu | $area = as | $area = sa | $area = af); 'set xlint 10'; 'set ylint 10';endif
    'set clevs   'cm5' 'cm4' 'cm3' 'cm2' 'cm1' 'cp1' 'cp2' 'cp3' 'cp4' 'cp5
    'set rbcols 39    37    35    33     31    0    61    63  65     67   69'
    if( b>0 ); 'd sn'%i ;endif

    'set gxout contour'
    'set grads off'
    'set ccolor 15'
*   'set clab forced'
    'set cstyle 1'
    'set cthick 4'
    'set clopts 1 4 0.'
    'set clevs   'aa1' 'aa2' 'aa3' 'aa4' 'aa5' 'aa6' 'aa7' 'aa8' 'aa9' 'aa10' 'aa11 
*   if( b>0); 'd an'%i; endif 

    'set gxout contour'
    'set grads off'
    'set ccolor 1'
*   'set clab forced'
    'set cstyle 1'
    'set cthick 5'
    'set clopts 1 4 0.08'
    'set clevs   'aa1' 'aa2' 'aa3' 'aa4' 'aa5' 'aa6' 'aa7' 'aa8' 'aa9' 'aa10' 'aa11 
    if( b>0); 'd fc'%i; endif 
*
    'set string 59 bl 7'
    'set strsiz 0.17 0.17'
    if(i=1);'draw string 'titlx' 'titly ' ${sname[1]} ';endif
    if(i=2);'draw string 'titlx' 'titly ' ${sname[2]} ';endif
    if(i=3);'draw string 'titlx' 'titly ' ${sname[3]} ';endif
    if(i=4);'draw string 'titlx' 'titly ' ${sname[4]} ';endif
    if(i=5);'draw string 'titlx' 'titly ' ${sname[5]} ';endif
    if(i=6);'draw string 'titlx' 'titly ' ${sname[6]} ';endif
    if(i=7);'draw string 'titlx' 'titly ' ${sname[7]} ';endif
    if(i=${nplt});'draw string 'titlx' 'titly ' Multi_model Mean';endif
i=i+1
endwhile

  'set string 4 bc 6'
  'set strsiz 0.14 0.14'
  'draw string 4.3 10.35 ${varname}, ${CDATE}${cyc} Cycle, Fcst Hour ${fhr}'
  'draw string 4.3 10.1 Verification Time: ${fdate}'
  'set string 1 bc 4'
  'set strsiz 0.13 0.13'
  'draw string 4.3 9.8  Contour: FCST; Color: FCST-ANL'
  'set string 1 bc 6'
  'set strsiz 0.16 0.16'
  if($nexp >1)
    'run ${help_srpt_dir}/cbarn.gs 0.95 0 4.1 0.28'
   else
    'run ${help_srpt_dir}/cbarn.gs 0.95 0 4.1 4.70'
   endif

  'printim ${var}_${area}${fhr}_t${cyc}z.gif gif x700 y700'
  'set vpage off'
'quit'
EOF1
grads -bcp "run ${var}_${area}${fhr}_t${cyc}z.gs"

## --  daily rotatigng display
if [ $doftp = "YES" ]; then 
cat << EOF >ftpin
  binary
  cd /home/people/emc/www/htdocs/gmb/$LOGNAME/directory
  mkdir ${edate}${cyc}Z_dropout$hem
  cd $ftpdir
    put ${var}_${area}${fhr}_t${cyc}z.gif     
  quit
EOF
sftp emcrzdm.ncep.noaa.gov <ftpin 
fi


#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
done   ;# end of sfc var
fi
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
done   ;# end of area
done   ;# end of fcst hour
#--------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
for var in  HGTprs ;do
for lev in  1000 850 500 200; do
#Create animated gif sequence
/usr/bin/convert -delay 100 -loop 15 ${var}${lev}_${area}f00_t${cyc}z.gif ${var}${lev}_${area}f12_t${cyc}z.gif ${var}${lev}_${area}f24_t${cyc}z.gif ${var}${lev}_${area}f36_t${cyc}z.gif ${var}${lev}_${area}f48_t${cyc}z.gif ${var}${lev}_${area}f60_t${cyc}z.gif ${var}${lev}_${area}f72_t${cyc}z.gif ${var}${lev}_${area}f84_t${cyc}z.gif ${var}${lev}_${area}f96_t${cyc}z.gif ${var}${lev}_${area}f108_t${cyc}z.gif ${var}${lev}_${area}f120_t${cyc}z.gif ${var}${lev}_${area}_anim_t${cyc}z.gif
if [ $doftp = "YES" ]; then

cat << EOF >ftpin
  binary
  cd /home/people/emc/www/htdocs/gmb/$LOGNAME/directory
  mkdir $ftpdir
  cd $ftpdir
  mput ${var}${lev}_${area}_anim_t${cyc}z.gif
  quit
EOF
sftp emcrzdm.ncep.noaa.gov <ftpin
fi
done
done

exit
