#!/bin/ksh
set -x

## Fanglin Yang, Dec 2009 
## 1. Compute precip skill scores over CONUS and performance Monte Carlo signoficance tests.
## 2. Use GrADS to make plots including significance bars. Maps will be sent to a web server for display. 
## 3. Model forecast data can be located in different machines. The scipt
##    will grab the data by itself. The script assumes the rainfall stats  
##    ${expnlist}_rain_yyyymmdd are always saved under $expdlist/$expnlist
##

#  ------------------------------------------
myhost=`echo $(hostname) |cut -c 1-1 `
export scrdir=${scrdir:-/global/save/wx24fy/VRFY/vsdb/precip}    

export expnlist=${expnlist:-"gfs pre13a"}                                  ;#experiment names, up to 10
export expdlist=${expdlist:-"/global/shared/stat/wgne1 /global/hires/glopara/archive "}  ;#archive directory
export complist=${complist:-"cirrus cirrus"}                               ;#computer name     
export DATEST=${DATEST:-20080606}                                          ;#forecast starting date
export DATEND=${DATEND:-20081130}                                          ;#forecast ending date
export cyclist=${cyclist:-"00"}                                            ;#forecast cycles for verify, 00Z and/or 12Z      
export missdays=${missdays:-""}        ;#exclude dates when data are missing from any one of the experiments

export doftp=${doftp:-"NO"}                                               ;#whether or not sent maps to ftpdir
export webhost=${webhost:-"emcrzdm.ncep.noaa.gov"}
export webhostid=${webhostid:-$LOGNAME}
export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$webhostid/vsdb}   ;#where maps are displayed 
export scppgb=${scppgb:-"NO"}                                              ;#copy data from different machines
export gstat=${gstat:-/global/shared/stat}                                 ;#global operational GFS stats directory   

export NWPROD=${NWPROD:-/nwprod}
export ndate=${ndate:-$NWPROD/util/exec/ndate}
export GRADSBIN=${GRADSBIN:-/usrx/local/grads/bin}
export vsdbhome=${vsdbhome:-/global/save/$LOGNAME/VRFY/vsdb}
export SUBJOB=${SUBJOB:-$vsdbhome/bin/sub_wcoss}
export ACCOUNT=${ACCOUNT:-GFS-MTN}
export CUE2RUN=${CUE2RUN:-shared}
export CUE2FTP=${CUE2FTP:-${CUE2RUN:-transfer}}
export GROUP=${GROUP:-g01}


if [ $doftp = "YES" -a $CUE2RUN = $CUE2FTP ]; then ssh -q -l $webhostid ${webhost} "mkdir -p ${ftpdir}/rain ";  fi
#---------------------------------------------------------
#---------------------------------------------------------
## copy precip data 
#
nexp=0 ; for run in $expnlist ; do nexp=$((nexp+1)) ; done
n=-1 ; for runn in $expnlist ; do n=$((n+1)) ; expname[n]=$runn ; done
n=-1 ; for rund in $expdlist ; do n=$((n+1)) ; expdir[n]=$rund  ; done
n=-1 ; for comp in $complist ; do n=$((n+1)) ; compname[n]=$comp  ; done
echo ${expdir[n]}
echo ${compname[n]}

export tmpdir=${rundir:-/stmp/$LOGNAME/pvrfy/${expname[1]}}
export mapdir=${mapdir:-$tmpdir/web}
if [ -s $tmpdir ] ; then rm -r $tmpdir; fi
mkdir -p $tmpdir  $mapdir/rain || exit 8

#-------------------------
for cyc in $cyclist; do
#-------------------------

export cycle=$cyc
export rundir=$tmpdir/${cycle}Z
export datdir=$rundir/wgne
mkdir -p $rundir $datdir  
cd $datdir 

affix=" "
if [ $cycle = "12" ]; then affix=12;  fi

nn=-1
while [ $((nn+=1)) -lt $nexp ] ; do
 export exp=${expname[nn]}           ;#exp name
 export exp_dir=${expdir[nn]}        ;#exp directory
 export CLIENT=${compname[nn]}       ;#computer used
 myclient=`echo $CLIENT |cut -c 1-1 `

 cdate=$DATEST
 while [ $cdate -le ${DATEND} ]; do
   filein=${exp_dir}/${exp}/${exp}_rain_${cdate}${affix}
   if [ $exp = "gfs" ]; then 
     filein=$gstat/wgne1/${exp}_rain_${cdate}${affix}
   fi
   cp $filein $datdir/${exp}_rain_${cdate}${affix}
   if [ $? -ne 0 -a $scppgb = YES ]; then
     scp $LOGNAME@${CLIENT}:${filein} $datdir/${exp}_rain_${cdate}${affix}
   fi
   xdate=`$ndate +24 ${cdate}00`
   cdate=`echo $xdate | cut -c1-8`
 done
#------------------------
done
#------------------------

#------------------------
for day in ${missdays}; do
 rm ${datdir}/*_rain_${day}
done
#------------------------

# Run plotting script
if [ $cycle = "00" -o $cycle = "12" ]; then
 $scrdir/precip_score.sh 
else
 echo " ${cycle}Z cycle is not supported. Exit"
 exit
fi



#---wait for all maps to be made
nsleep=0
tsleep=30      #seconds to sleep before checking file again
msleep=40      #maximum number of times to sleep
while test ! -s $rundir/etsbis.p750*.png -a $nsleep -lt $msleep;do
  sleep $tsleep
  nsleep=`expr $nsleep + 1`
done


cd $rundir 
cp *png $mapdir/rain/.
cat << EOF >ftpin
  cd $ftpdir
  mkdir rain
  cd rain
  binary
  mput *.png
  quit
EOF
if [ $doftp = "YES" -a $CUE2RUN = $CUE2FTP ]; then   
 sftp ${webhostid}@${webhost} <ftpin 
 if [ $? -ne 0 ]; then scp -rp *png ${webhostid}@${webhost}:${ftpdir}/rain/. ;fi
fi



#--------------------------------------------
##--send plots to web server using dedicated
##--transfer node (required by NCEP WCOSS)
if [ $doftp = "YES" -a $CUE2RUN != $CUE2FTP ]; then
#--------------------------------------------
cd $rundir
cat << EOF >ftprain.sh
#!/bin/ksh
set -x
 if [ -s ftpin ]; then sftp  ${webhostid}@${webhost} < ftpin  ;fi
EOF
  chmod u+x $rundir/ftprain.sh 
  $SUBJOB -a $ACCOUNT -q $CUE2FTP -g $GROUP -p 1/1/S -t 0:30:00 -r 16/1 -j ftprain -o ftprain.out $rundir/ftprain.sh 
#--------------------------------------------
fi
#--------------------------------------------

#------------------------
done ;#end cyclist
#------------------------

exit

   
