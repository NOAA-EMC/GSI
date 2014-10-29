#!/bin/sh
set -x

#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#  make maps with and without the drop out cases of GFS forecasts
#  Include ECMWF forecast for comparison
#  Fanglin Yang, Jan 2009
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------

nca=1 
while [ $nca -le 33 ]; do

 case case$nca in
case1)   hemp=NH; monc=200710;  cycl=00; export dropdays="20071025 20071027";;
case2)   hemp=NH; monc=200710;  cycl=12; export dropdays="20071026 20071027 20071028";;
case3)   hemp=NH; monc=200711;  cycl=12; export dropdays="20071116";;
case4)   hemp=NH; monc=200712;  cycl=12; export dropdays="20071225 ";;
case5)   hemp=NH; monc=200806;  cycl=00; export dropdays="20080610 20080623 20080630";;
case6)   hemp=NH; monc=200807;  cycl=00; export dropdays="20080708 20080712 20080715 20080724";;
case7)   hemp=NH; monc=200807;  cycl=12; export dropdays="20080707 20080709 20080712 20080713";;
case8)   hemp=NH; monc=200810;  cycl=00; export dropdays="20081009 20081016 20081017 20081026";;
case9)   hemp=NH; monc=200810;  cycl=12; export dropdays="20081015 20081016 20081017";;
case10)  hemp=SH; monc=200710;  cycl=00; export dropdays="20071009 20071012 20071014 20071018 20071019 20071025 ";;
case11)  hemp=SH; monc=200710;  cycl=12; export dropdays="20071004 20071007 20071009 20071011 20071025";;
case12)  hemp=SH; monc=200711;  cycl=12; export dropdays="20071124 ";;
case13)  hemp=SH; monc=200712;  cycl=00; export dropdays="20071225";;
case14)  hemp=SH; monc=200712;  cycl=12; export dropdays="20071221 20071225";;
case15)  hemp=SH; monc=200801;  cycl=00; export dropdays="20080116";;
case16)  hemp=SH; monc=200801;  cycl=12; export dropdays="20080117";;
case17)  hemp=SH; monc=200802;  cycl=00; export dropdays="20080206 20080208";;
case18)  hemp=SH; monc=200803;  cycl=00; export dropdays="20080308 20080318 20080323 20080326";;
case19)  hemp=SH; monc=200803;  cycl=12; export dropdays="20080308 20080314 20080315 20080320 20080323 20080325 20080327";;
case20)  hemp=SH; monc=200804;  cycl=00; export dropdays="20080414 20080430 ";;
case21)  hemp=SH; monc=200804;  cycl=12; export dropdays="20080430";;
case22)  hemp=SH; monc=200805;  cycl=00; export dropdays="20080501 20080514 20080515 20080527 ";;
case23)  hemp=SH; monc=200805;  cycl=12; export dropdays="20080520 20080527";;
case24)  hemp=SH; monc=200806;  cycl=00; export dropdays="20080610 20080630";;
case25)  hemp=SH; monc=200806;  cycl=12; export dropdays="20080617 20080630";;
case26)  hemp=SH; monc=200807;  cycl=00; export dropdays="20080730";;
case27)  hemp=SH; monc=200808;  cycl=00; export dropdays="20080810 20080821";;
case28)  hemp=SH; monc=200808;  cycl=12; export dropdays="20080824";;
case29)  hemp=SH; monc=200810;  cycl=00; export dropdays="20081018 20081027 ";;
case30)  hemp=SH; monc=200810;  cycl=12; export dropdays="20081014 20081017 20081024 20081026";;
case31)  hemp=SH; monc=200811;  cycl=00; export dropdays="20081111 20081112 20081114 20081119 ";;
case32)  hemp=SH; monc=200811;  cycl=12; export dropdays="20081111 20081114 ";;
case33)  hemp=SH; monc=200812;  cycl=12; export dropdays="20081205 20081213 20081223";;
esac


export reglist="G2/${hemp}X"
export cyclist="$cycl"
export cyclist="$cycl"

mon=`echo ${monc} |cut -c 5-6`
set -A daymon 0 31 28 31 30 31 30 31 31 30 31 30 31
ndays=${daymon[$mon]}            ;#number of days back for verification
export edate=${monc}${ndays}


#===============================================================
#===============================================================
fdays=5             ;#forecast day for verification 
export vtype=${vtype:-anom}
export vnamlist=${vnamlist:-"HGT"}
export mdlist=${mdlist:-"gfs ecm"}
export levlist=${levlist:-"P500"}
export rundir=${rundir:-/stmp/$LOGNAME/vsdb_exp/drop}


#--------------------------------------------------------------------
#--------------------------------------------------------------------

export vsdb_data=${vsdb_data:-/climate/save/wx24fy/VRFY/vsdb_data}
export sorcdir=${sorcdir:-/climate/save/wx24fy/VRFY/vsdb_stats}
export mapdir=${mapdir:-/stmp/$LOGNAME/vsdb_exp/drop/maps}                          
export copymap=${copymap:-"YES"}   ;#copy maps to a central directory
export archmon=${archmon:-"NO"}      ;# archive monthly means 
export xtick=`expr $ndays \/ 8 `

export webhost=${webhost:-"rzdm.ncep.noaa.gov"}
export webhostid=${webhostid:-"myid"}
export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$webhostid/vsdb} 
export doftp=${doftp:-"NO"}        ;#ftp maps to web site


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
for vnam in $vnamlist; do
   vnam1=`echo $vnam | sed "s?/??g" |sed "s?_WV1?WV?g"`
   export exedir=${rundir}/${vtype}/${vnam1}
   if [ -s $exedir ]; then rm -r $exedir; fi
   mkdir -p $exedir; cd $exedir || exit

for cyc  in $cyclist ; do
for lev  in $levlist ; do
for reg  in $reglist ; do
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------

# -- generate output names 
  nhours=`expr $ndays \* 24 - 24`
  tmp=`${ndate_dir}/ndate  -$nhours ${edate}00 `
  sdate=`echo $tmp | cut -c 1-8`
  reg1=`echo $reg | sed "s?/??g"`
  outname1=${vnam1}_${lev}_${reg1}_${cyc}Z${sdate}${edate}
  outname2=${vnam1}_${lev}_${reg1}_${cyc}Z${sdate}${edate}d
  yyyymm=`echo $edate |cut -c 1-6`                                
  outmon=${vnam1}_${lev}_${reg1}_${cyc}Z${yyyymm}

# -- search data for all models; write out binary data, create grads control file
  for model in $mdlist ; do
     outname=${outname1}_${model}
     if [ $vnam = "WIND" ]; then
      /climate/save/wx24fy/VRFY/vsdb_exp/drop/gen_wind.sh $vtype $model $vnam $reg $lev $edate $ndays $cyc $fdays $outname
     else
      /climate/save/wx24fy/VRFY/vsdb_exp/drop/gen_scal.sh $vtype $model $vnam $reg $lev $edate $ndays $cyc $fdays $outname
     fi
  done


  for model in gfs ; do
     outname=${outname1}_${model}
     outnamedrop=${outname2}_${model}
     if [ $vnam = "WIND" ]; then
      /climate/save/wx24fy/VRFY/vsdb_exp/drop/gen_wind_drop.sh $vtype $model $vnam $reg $lev $edate $ndays $cyc $fdays ${outnamedrop}
     else
      /climate/save/wx24fy/VRFY/vsdb_exp/drop/gen_scal_drop.sh $vtype $model $vnam $reg $lev $edate $ndays $cyc $fdays ${outnamedrop}
     fi
  done

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# -- create grads scripts, allows up to 10 models 
nmd=`echo $mdlist | wc -w`  ;#count number of models
set -A mdname $mdlist
set -A mdnamec `echo $mdlist |tr "[a-z]" "[A-Z]" `
namedaily=${vnam1}_${lev}_${reg1}_${cyc}Z


# ----- PLOT TYPE 1:  time series of anomaly correlations ----
cat >acz_${outname1}.gs <<EOF1 
'reinit'; 'set font 1'
              'open ${outname2}_${mdname[0]}.ctl' 
              'open ${outname1}_${mdname[0]}.ctl' ; mdc.1=${mdnamec[0]}
 if($nmd >1); 'open ${outname1}_${mdname[1]}.ctl' ; mdc.2=${mdnamec[1]} ;endif
 if($nmd >2); 'open ${outname1}_${mdname[2]}.ctl' ; mdc.3=${mdnamec[2]} ;endif
 if($nmd >3); 'open ${outname1}_${mdname[3]}.ctl' ; mdc.4=${mdnamec[3]} ;endif
 if($nmd >4); 'open ${outname1}_${mdname[4]}.ctl' ; mdc.5=${mdnamec[4]} ;endif
 if($nmd >5); 'open ${outname1}_${mdname[5]}.ctl' ; mdc.6=${mdnamec[5]} ;endif
 if($nmd >6); 'open ${outname1}_${mdname[6]}.ctl' ; mdc.7=${mdnamec[6]} ;endif
 if($nmd >7); 'open ${outname1}_${mdname[7]}.ctl' ; mdc.8=${mdnamec[7]} ;endif
 if($nmd >8); 'open ${outname1}_${mdname[8]}.ctl' ; mdc.9=${mdnamec[8]} ;endif
 if($nmd >9); 'open ${outname1}_${mdname[9]}.ctl' ; mdc.10=${mdnamec[9]} ;endif

*-- define line styles and model names 
  cst.1=1; cst.2=1; cst.3=3; cst.4=5; cst.5=5; cst.6=3; cst.7=5; cst.8=5; cst.9=5; cst.10=5
  cth.1=12; cth.2=12; cth.3=4; cth.4=1; cth.5=4; cth.6=1; cth.7=1; cth.8=1; cth.9=1; cth.10=1
  cma.1=0; cma.2=8; cma.3=6; cma.4=1; cma.5=2; cma.6=0; cma.7=4; cma.8=3; cma.9=4; cma.10=5
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=9; cco.7=5; cco.8=6; cco.9=7; cco.10=15
*------------------------
day=${fdays} ;*start from fcst00
while ( day <= ${fdays} )
*------------------------
  'c'
  'set t 1 $ndays' 
   laty=day+1
  'set y '%laty 

  xwd=7.0; ywd=4.0; yy=ywd/16
  xmin=1.0; xmax=xmin+xwd; ymin=6.0; ymax=ymin+ywd
  xt=xmin+0.3; xt1=xt+0.5; xt2=xt1+0.1; yt=ymin+0.36*ywd ;*for legend  
  zxt=xt2+3.0; zxt1=zxt+0.5; zxt2=zxt1+0.1; zyt=ymin+0.36*ywd
  titlx=xmin+0.5*xwd;  titly=ymax+0.20                  ;*for tytle
  xlabx=xmin+0.45*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax

*--find maximum and minmum values
   cmax=-1.0; cmin=1.0
   j=2
   while (j <= 2)
    'set gxout stat'
    'd cor.'%j
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    if(zmax > cmax); cmax=zmax; endif
    if(zmin < cmin); cmin=zmin; endif
   j=j+1
   endwhile
   dist=cmax-cmin; cmin=cmin-0.9*dist; cmax=1.2*cmax        
   if (cmin < -1.0); cmin=-1.0; endif
   if (cmax > 1.0); cmax=1.0; endif
   cmin=substr(cmin,1,3); cmax=substr(cmax,1,3); cint=0.1
   say 'cmin cmax cint 'cmin' 'cmax' 'cint

   j=2
   i=1
   while (i <= $nmd )
**plot entire period
    'set gxout stat'  ;* first compute means and count good data numbers
    'd cor.'%j
    ln=sublin(result,11); wd=subwrd(ln,2); a=substr(wd,1,5)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if ( b>0 )
        'set strsiz 0.13 0.13'; yt=yt-yy
        'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
        'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i' ' a'  'b

      'set gxout line'
      'set display color white'; 'set missconn off';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'set vrange 'cmin' 'cmax; 'set ylint 'cint; 'set xlint $xtick'
      'd cor.'%j
    endif
   i=i+1
   j=j+1
   endwhile

    j=1
    i=1
** write out mean and counts with dropout days excluded
    'set gxout stat'  ;* first compute means and count good data numbers
    'd cor.'%j
    ln=sublin(result,11); wd=subwrd(ln,2); a=substr(wd,1,5)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if ( b>0 )
        'set strsiz 0.13 0.13'; yt=yt-yy-yy
        'set line 4 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
        'set string 4 bl 6';     'draw string 'xt2' 'yt' 'mdc.i' w/o dropout:   ' a'  'b
        'set strsiz 0.09 0.09'; yt=yt-yy
        'set string 'cco.i' bl 4';     'draw string 'xt' 'yt' drop days: $dropdays '

      'set gxout line'
      'set display color white'; 'set missconn off';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set cstyle 5 '; 'set cthick 7'; 'set cmark 2'; 'set ccolor 4'
      'set vrange 'cmin' 'cmax; 'set ylint 'cint; 'set xlint $xtick'
      'd cor.'%j
    endif

*  i=i+1
*  endwhile

  'set string 1 bc 7'
  'set strsiz 0.16 0.16'
  'draw string 'titlx' 'titly' Anomaly Correl: ${vnam} ${lev} ${reg} ${cyc}Z, Day '%day
  'set strsiz 0.15 0.15'
  'draw string 'xlabx' 'xlaby' Verification Date'

  'printim cor_day'%day'_${namedaily}_${monc}.gif gif x800 y800'
  'set vpage off'
*--------
day=day+1
endwhile
*-------
'quit'
EOF1
grads -bcp "run acz_${outname1}.gs"


done
done
done
done

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#--------------------
# remove whitesapce from gif files
# ImageMagic
export PATH=$PATH:/usrx/local/imajik/bin
export LIBPATH=/lib:/usr/lib
export LIBPATH=$LIBPATH:/usrx/local/imajik/lib

  nn=0
  while [ $nn -le $fdays ]; do
   convert -crop 0.2x0.2 cor_day${nn}_${namedaily}_${monc}.gif cor_day${nn}_${namedaily}_${monc}.gif
   nn=`expr $nn + 1 `
  done

chmod a+rw cor*.gif

#-------------------------------
  if [ $copymap = "YES" ]; then
    tmpdir=${mapdir}
    mkdir -p $tmpdir
    cp *gif  $tmpdir/.
  fi
#------------------------------


#===============================================================
#===============================================================
nca=`expr $nca + 1 `
done
#===============================================================
#===============================================================
exit

