#!/bin/ksh 
set -x

#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#  Make plots that compare forecasts and analyses from all centers for the same forecast cycle (e.g. 00Z)
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
export vtype=${vtype:-sfc}
export vnamlist=${vnamlist:-"RH2m SPFH2m T2m TOZNE TG"}
export mdlist=${mdlist:-"gfs ecm"}
export levlist=${levlist:-"SL1L2"}
export reglist=${reglist:-"G2/NHX"}

export webhost=${webhost:-"emcrzdm.ncep.noaa.gov"}
export webhostid=${webhostid:-"$LOGNAME"}
export vsdb_data=${vsdb_data:-/climate/save/wx24fy/VRFY/vsdb_data}
export sorcdir=${sorcdir:-/global/save/wx24fy/VRFY/vsdb/map_util}
export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$webhostid/vsdb} 
export doftp=${doftp:-"YES"}        ;#ftp maps to web site
export mapdir=${mapdir:-/stmp/$LOGNAME/vsdb_exp/maps}
export copymap=${copymap:-"NO"}     ;#copy maps to a central directory
export archmon=${archmon:-"NO"}     ;# archive monthly means 
export rundir=${rundir:-/stmp/$LOGNAME/vsdb_stats0}
export xtick=`expr $ndays \/ 8 `

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


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
for vnam in $vnamlist; do
   vnam1=`echo $vnam | sed "s?/??g" |sed "s?_WV1?WV?g"`
   export exedir=${rundir}/${vtype}/${vnam1}
   if [ -s $exedir ]; then rm -r $exedir; fi
   mkdir -p $exedir; cd $exedir || exit

for lev  in $levlist ; do
for reg  in $reglist ; do
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------

# -- generate output names 
  ncyc=`echo $fcycle | wc -w`               ;#count number of cycles
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
  $sorcdir/gen_sfc.sh $vtype $vnam $reg $lev $edate $ndays "${fcycle}" $fdays $fhout $outname1 $maskmiss "$mdlist"


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# -- create grads scripts, allows up to 8 models 
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


# ----- PLOT TYPE 4:  time series of total fields ----
cat >sfc_${outname1}.gs <<EOF1 
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

*-- define line styles for forecasts
  'run $sorcdir/grads/rgbset.gs'
  cst.1=1;  cst.2=5;  cst.3=3; cst.4=5; cst.5=5; cst.6=3; cst.7=5; cst.8=5; cst.9=5; cst.10=5
  cth.1=10; cth.2=10; cth.3=9; cth.4=9; cth.5=9; cth.6=9; cth.7=9; cth.8=9; cth.9=9; cth.10=9
  cma.1=0;  cma.2=8;  cma.3=6; cma.4=1; cma.5=2; cma.6=7; cma.7=4; cma.8=3; cma.9=8; cma.10=5
  cco.1=1;  cco.2=2;  cco.3=3; cco.4=4; cco.5=8; cco.6=9; cco.7=15; cco.8=5; cco.9=6; cco.10=7


*------------------------
fhour=0 ;*start from fcst00
while ( fhour <= ${vlength} )
if (fhour=0|fhour=24|fhour=48|fhour=72|fhour=96|fhour=120|fhour=144|fhour=168|fhour=192|fhour=216|fhour=240)
*------------------------
  'c'
  day=fhour/24
  laty=fhour/${fhout}+1
  'set x 1'
  'set t 1 $ndays' 
  'set y '%laty 

  xwd=7.0; ywd=4.0; yy=ywd/17
  xmin=1.0; xmax=xmin+xwd; ymin=6.0; ymax=ymin+ywd
  xt=xmin+0.3; xtm=xt+0.25; xt1=xt+0.5; xt2=xt1+0.1; yt=ymin+yy ;*for legend  
  titlx=xmin+0.5*xwd;  titly=ymax+0.20; titly2=titly-0.4            ;*for tytle
  xlabx=xmin+0.5*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

*--find maximum and minmum values
   cmax=-10000000.0;  cmin=10000000.0
   cmax1=10000000.0; cmin1=-10000000.0
   i=1
   while (i <= $nmd)
    'set gxout stat'  
    'd fst(x='%i')'
    ln=sublin(result,7); wd=subwrd(ln,8); nn=substr(wd,1,3)
    if ( nn>0 )
      range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
      if(zmax > cmax & zmax < cmax1); cmax=zmax; endif
      if(zmin < cmin & zmin > cmin1); cmin=zmin; endif
    endif
    'd obs(x='%i')'
    ln=sublin(result,7); wd=subwrd(ln,8); nn=substr(wd,1,3)
    if ( nn>0 )
      range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
      if(zmax > cmax & zmax < cmax1); cmax=zmax; endif
      if(zmin < cmin & zmin > cmin1); cmin=zmin; endif
      cmax1=2*cmax; cmin1=0.5*cmin
    endif
   i=i+1
   endwhile
   dist=cmax-cmin; cmin=cmin-0.5*dist; cmax=cmax+0.1*dist
   cmin=substr(cmin,1,6); cmax=substr(cmax,1,6)
   dist=cmax-cmin; cint=0
   if ( dist > 0.01 ); cint=10*substr((cmax-cmin)/100,1,4); endif
   if (cint = 0 & dist > 0.001 ); cint=substr((cmax-cmin)/10,1,4); endif
   if (cint = 0 & dist > 0.0001 ); cint=0.1*substr((cmax-cmin),1,4); endif
   if (cint = 0 & dist > 0.00001 ); cint=0.01*substr((cmax-cmin)*10,1,4); endif
   if (cint = 0 & dist > 0.000001 ); cint=0.001*substr((cmax-cmin)*100,1,4); endif
   if (cint = 0 & dist > 0.0000001 ); cint=0.0001*substr((cmax-cmin)*1000,1,4); endif
   say 'cmin cmax cint 'cmin' 'cmax' 'cint

   i=1
   while (i <= $nmd)
    'set gxout stat'  ;* first compute means and count good data numbers
    'd fst(x='%i')'
    ln=sublin(result,11); wd=subwrd(ln,2); fc=substr(wd,1,5)
    ln=sublin(result,7); wd=subwrd(ln,8); nn=substr(wd,1,3)
    'd obs(x='%i')'
    ln=sublin(result,11); wd=subwrd(ln,2); ob=substr(wd,1,5)
    if ( nn>0 )
      'set strsiz 0.13 0.13'; yt=yt+yy
      'set cmark 'cma.i; 'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
      'draw mark 'cma.i' 'xtm' 'yt' 0.10'
*     'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i'  F 'fc'  A 'ob'  n='nn 
      'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i'  'fc'  n='nn 

      'set gxout line'
      'set display color white'; 'set missconn off';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 'cmin' 'cmax; 'set ylint 'cint
      'set xlint $xtick'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'd fst(x='%i')'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark  0'; 'set ccolor 'cco.i
*     'd obs(x='%i')'
     endif
   i=i+1
   endwhile

  'set string 1 bc 7'; 'set strsiz 0.16 0.16'
  if ( $ncyc >1 )
   'draw string 'titlx' 'titly' ${vnam} ${reg}, $sdate-$edate fh'%fhour
  else
   'draw string 'titlx' 'titly' ${vnam} ${reg} ${fcycle}Z, $sdate-$edate fh'%fhour
  endif
  'set string 1 bc 5'; 'set strsiz 0.12 0.12'
* 'draw string 'titlx' 'titly2' Forecasts: W/ marks;  Analyses: W/O marks '
  'set strsiz 0.15 0.15'
  'draw string 'xlabx' 'xlaby' Verification Date'

  'printim sfc_day'%day'_${namedaily}.png x800 y800'
  'set vpage off'
*--------
endif
fhour=fhour+${fhout}
endwhile
*-------
'quit'
EOF1
$GRADSBIN/grads -bcp "run sfc_${outname1}.gs"


# ----- PLOT TYPE 2:  sfc growth curve over $ndays days----
ndaysp1=`expr $ndays + 1`
fdaysp1=`expr $fdays + 1`
cat >sfcmean_${outname1}.gs <<EOF1 
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
  'run $sorcdir/grads/rgbset.gs'
* cst.1=1; cst.2=1; cst.3=1; cst.4=1; cst.5=1; cst.6=1; cst.7=1; cst.8=1
  cst.1=1; cst.2=5; cst.3=3; cst.4=5; cst.5=5; cst.6=3; cst.7=1; cst.8=5
  cth.1=9; cth.2=9; cth.3=6; cth.4=1; cth.5=4; cth.6=1; cth.7=9; cth.8=1
  cma.1=4; cma.2=8; cma.3=6; cma.4=1; cma.5=2; cma.6=5; cma.7=3; cma.8=7
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=9; cco.7=5; cco.8=6

  'set x 1'
  'set t $ndaysp1' 
  'set y 1 ${nfcst}'
  xwd=7.0; ywd=4.0; yy=ywd/17
  xmin=1.0; xmax=xmin+xwd; ymin=6.0; ymax=ymin+ywd
  xt=xmin+0.3; xt1=xt+0.5; xt2=xt1+0.1; yt=ymin+yy ;*for legend  
  titlx=xmin+0.5*xwd;  titly=ymax+0.20; titly2=titly-0.4            ;*for tytle
  xlabx=xmin+0.5*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax


*--find maximum and minmum values
   cmax=-10000000.0;  cmin=10000000.0
   cmax1=10000000.0; cmin1=-10000000.0
   i=1
   while (i <= $nmd)
    'set gxout stat'  
    'd fst(x='%i')'
    ln=sublin(result,7); wd=subwrd(ln,8); nn=substr(wd,1,3)
    if ( nn>0 )
      range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
      if(zmax > cmax & zmax < cmax1); cmax=zmax; endif
      if(zmin < cmin & zmin > cmin1); cmin=zmin; endif
    endif
    'd obs(x='%i')'
    ln=sublin(result,7); wd=subwrd(ln,8); nn=substr(wd,1,3)
    if ( nn>0 )
      range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
      if(zmax > cmax & zmax < cmax1); cmax=zmax; endif
      if(zmin < cmin & zmin > cmin1); cmin=zmin; endif
      cmax1=2*cmax; cmin1=0.5*cmin
    endif
   i=i+1
   endwhile
   dist=cmax-cmin; cmin=cmin-0.5*dist; cmax=cmax+0.1*dist
   cmin=substr(cmin,1,5); cmax=substr(cmax,1,5)
   dist=cmax-cmin; cint=0
   if ( dist > 0.01 ); cint=10*substr((cmax-cmin)/100,1,4); endif
   if (cint = 0 & dist > 0.001 ); cint=substr((cmax-cmin)/10,1,4); endif
   if (cint = 0 & dist > 0.0001 ); cint=0.1*substr((cmax-cmin),1,4); endif
   if (cint = 0 & dist > 0.00001 ); cint=0.01*substr((cmax-cmin)*10,1,4); endif
   if (cint = 0 & dist > 0.000001 ); cint=0.001*substr((cmax-cmin)*100,1,4); endif
   if (cint = 0 & dist > 0.0000001 ); cint=0.0001*substr((cmax-cmin)*1000,1,4); endif
   say 'cmin cmax cint 'cmin' 'cmax' 'cint


   i=1
   while (i <= $nmd)
    'set gxout stat'   ;* first compute means and count good data numbers
    'd rms(x='%i')'    ;* number of records
    ln=sublin(result,11); wd=subwrd(ln,2); nn=substr(wd,1,3)
    'd fst(x='%i')'
    ln=sublin(result,11); wd=subwrd(ln,2); fc=substr(wd,1,5)
    'd obs(x='%i')'
    ln=sublin(result,11); wd=subwrd(ln,2); ob=substr(wd,1,5)
    if ( nn>0 )
      'set strsiz 0.13 0.13'; yt=yt+yy
      'set cmark 'cma.i; 'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
*     'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i'  F 'fc'  A 'ob'  n='nn 
      'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i'  'fc'  n='nn 

      'set gxout line'
      'set mproj off'
      'set display color white'; 'set missconn off';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 'cmin ' 'cmax; 'set ylint 'cint; 'set xlint 24'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'd fst(x='%i')'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark  0'; 'set ccolor 'cco.i
*     'd obs(x='%i')'
     endif
   i=i+1
   endwhile

  'set string 1 bc 6'; 'set strsiz 0.16 0.16'
  if ( $ncyc >1 )
   'draw string 'titlx' 'titly' ${vnam} ${reg}, Mean for $sdate-$edate '
  else
   'draw string 'titlx' 'titly' ${vnam} ${reg} ${fcycle}Z, Mean for $sdate-$edate '
  endif
  'set string 1 bc 5'; 'set strsiz 0.14 0.14'
* 'draw string 'titlx' 'titly2' Forecasts: W/ marks;  Analyses: W/O marks '
  'set strsiz 0.14 0.14'
  'draw string 'xlabx' 'xlaby' Forecast Hour'

  'printim sfcmean_${namedaily}.png x800 y800'
  'set vpage off'
'quit'
EOF1
$GRADSBIN/grads -bcp "run sfcmean_${outname1}.gs"

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#--------------------
# remove whitesapce from.png files
# ImageMagic
export PATH=$PATH:/usrx/local/imajik/bin
export LIBPATH=/lib:/usr/lib
export LIBPATH=$LIBPATH:/usrx/local/imajik/lib
  nn=0
  while [ $nn -le $fdays ]; do
   $imgconvert -crop 0.2x0.2 sfc_day${nn}_${namedaily}.png sfc_day${nn}_${namedaily}.png
   nn=`expr $nn + 1 `
  done
   $imgconvert -crop 0.2x0.2 sfcmean_${namedaily}.png sfcmean_${namedaily}.png

#------------------------------------------------
  if [ $copymap = "YES" ]; then
    tmpdir=${mapdir}/allmodel/daily
    mkdir -p $tmpdir/sfc
    cp sfc*.png  $tmpdir/sfc/.
  fi
#-------------------------------------------------


chmod a+rw *.png
## -- for daily rotatigng display
cat << EOF >ftp_$vnam1$lev$reg1
  binary
  promt
  cd $ftpdir/allmodel/daily/sfc
    mput sfc_day*_${namedaily}.png
    mput sfcmean_${namedaily}.png
  quit
EOF
if [ $doftp = "YES" -a $CUE2RUN = $CUE2FTP ]; then 
 sftp ${webhostid}@${webhost} <ftp_$vnam1$lev$reg1  
 if [ $? -ne 0 ]; then 
   scp sfc_day*_${namedaily}.png ${webhostid}@${webhost}:$ftpdir/allmodel/daily/sfc/.            
   scp sfcmean_${namedaily}.png  ${webhostid}@${webhost}:$ftpdir/allmodel/daily/sfc/.            
 fi
fi



## -- make monthly archive
yyyy=`echo $edate |cut -c 1-4`
if [ $archmon = "YES" ]; then
  nn=0
  while [ $nn -le $fdays ]; do
   cp sfc_day${nn}_${namedaily}.png  sfc_day${nn}_${outmon}.png
   nn=`expr $nn + 1 `
  done
   cp sfcmean_${namedaily}.png  sfcmean_${outmon}.png
cat << EOF >ftpmon_$vnam1$lev$reg1
  binary
  promt
  cd $ftpdir/allmodel/arch_mon/sfc
    mkdir $yyyy
    cd $yyyy
    mput sfc_day*_${outmon}.png
    mput sfcmean_${outmon}.png
    mput meansfc*.txt
  quit
EOF
if [ $doftp = "YES"  -a $CUE2RUN = $CUE2FTP ]; then 
 sftp ${webhostid}@${webhost} <ftpmon_$vnam1$lev$reg1  
 if [ $? -ne 0 ]; then 
   scp sfc_day*_${outmon}.png ${webhostid}@${webhost}:$ftpdir/allmodel/arch_mon/sfc/$yyyy/.
   scp sfcmean_${outmon}.png ${webhostid}@${webhost}:$ftpdir/allmodel/arch_mon/sfc/$yyyy/.
   scp mput meansfc*.txt ${webhostid}@${webhost}:$ftpdir/allmodel/arch_mon/sfc/$yyyy/.
 fi
fi

fi
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
done  ;# end reg
done  ;# end lev
done  ;# end vnam
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------



#--------------------------------------------
##--send plots to web server using dedicated transfer node (required by NCEP WCOSS computers)
if [ $makemap = "YES" -a $doftp = "YES" -a $CUE2RUN != $CUE2FTP ]; then
#--------------------------------------------
cd ${rundir}
cat << EOF >ftpcard$$.sh
#!/bin/ksh
set -x
for vnam in $vnamlist; do
 exedir=${rundir}/${vtype}/\$vnam
 cd \$exedir
 for lev  in $levlist ; do
 for reg  in $reglist ; do
   reg1=\`echo \$reg | sed "s?/??g"\`
   if [ -s ftp_\$vnam\$lev\$reg1 ]; then sftp  ${webhostid}@${webhost} <ftp_\$vnam\$lev\$reg1 ; fi
   if [ -s ftpmon_\$vnam\$lev\$reg1 ]; then sftp  ${webhostid}@${webhost} <ftpmon_\$vnam\$lev\$reg1 ; fi
 done
 done
done
EOF
 chmod u+x $rundir/ftpcard$$.sh
 $SUBJOB -a $ACCOUNT -q $CUE2FTP -g $GROUP -p 1/1/S -t 1:00:00 -r 128/1 -j ftpcard -o ftpcard$$.out ${rundir}/ftpcard$$.sh
#--------------------------------------------
fi
#--------------------------------------------


exit

