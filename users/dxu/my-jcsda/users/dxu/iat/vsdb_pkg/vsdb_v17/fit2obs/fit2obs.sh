#!/bin/ksh
set -axu

## Fanglin Yang, Jan 2009
## 1. This program makes fit-to-obs comparison between two experiments.                
##    Maps will be sent to a web server for display. The source code
##    of plotall.sh and mkweb.sh was provided by Suranjana Saha. 
##    Fanglin wrote this interface program, generalized the source
##    code, and included this package in the VSDB verification system.
## 2. Model forecast data can be located in different machines. The scipt
##    will grab the data by itself. The script assumes the fit-to-obs output
##    is always saved under $expdlist/$expnlist/fits and $expdlist/$expnlist/horiz  
## 3. (Apr 2009) The fnl fit-to-obs is computed using all available obs.
##    The new fnc fit-to-obs is computed only using obs in cnvstat. 

#--------------------------------------------------------------
export scrdir=${scrdir:-/global/save/wx24fy/VRFY/vsdb/fits}

export expnlist=${expnlist:-${1:-"fnl prd09q1o"}}              ;#must be two experiments, fnl is operational gfs
export expdlist=${expdlist:-${2:-"/global/save/Suranjana.Saha /global/shared/glopara/archive"}} ;#exp directory
export complist=${complist:-${3:-"gyre gyre"}}                  ;#computers where experiments are run
export cyc=${cycle:-${4:-"00"}}                                ;#cycle, one only
export DATEST=${DATEST:-${5:-20131201}}                        ;#forecast starting date
export DATEND=${DATEND:-${6:-20131231}}                        ;#forecast ending date
export endianlist=${endianlist:-${7:-"big big"}}               ;#big_endian or little_endian of fits data

export machine=${machine:-IBM}
export webmch=${webhost:-"emcrzdm.ncep.noaa.gov"}
export webid=${webhostid:-$LOGNAME}
export cue=${CUE2RUN:-batch}
export CUE2FTP=${CUE2FTP:-${CUE2RUN:-transfer}}
export task=${ACCOUNT:-GFS-MTN}
export GROUP=${GROUP:-g01}
export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$webid/vsdb} ;# maps display site    
export doftp=${doftp:-"NO"}                                   ;#whether or not sent maps to ftpdir
export SUBJOB=${SUBJOB:-/u/wx20mi/bin/sub}                    ;#script for submitting batch jobs
export NWPROD=${NWPROD:-/nwprod}
export ndate=$NWPROD/util/exec/ndate
export GRADSBIN=${GRADSBIN:-/usrx/local/grads/bin}
export IMGCONVERT=${IMGCONVERT:-convert}
export oinc_f2o=${oinc_f2o:-24}      # increment (hours) between observation verify times for timeout plots
export finc_f2o=${finc_f2o:-12}      # increment (hours) between forecast lengths for timeout plots
export fmax_f2o=${fmax_f2o:-48}      # max forecast length to show for timeout plots
export PATH=$PATH:/nwprod/util/exec       

set -A expname none ${expnlist}
set -A expdir none ${expdlist}
set -A compname none ${complist}
set -A endianname none ${endianlist}

export rundir=${rundir:-/stmp/$LOGNAME/fit}
export mapdir=${mapdir:-$rundir/web}
if [ -s $rundir ];  then rm -r ${rundir}/${expname[2]}; fi
mkdir -p ${rundir}/${expname[2]}
myhost=`echo $(hostname) |cut -c 1-1 `

#######################################################################
#######################################################################
export sdate=${DATEST}${cyc}
export edate=${DATEND}${cyc}

n=1
while [ n -le 2 ]; do
  fnltype=0
  CLIENT=${compname[n]}
  myclient=`echo $CLIENT |cut -c 1-1 `
  exp=${expname[$n]}
  export exp$n=$exp                      
  export endian$n=${endianname[$n]}
  export dir$n=${expdir[$n]}/$exp         
  if [ ${exp} = "fnc" -o  ${exp} = "fnl" -o ${exp} = "ecm" -o ${exp} = "cdas" -o ${exp} = "avn" -o ${exp} = "gdas" -o ${exp} = "ukm" ]; then
   export dir$n=${gfsfitdir:-/global/save/Suranjana.Saha}/$exp
   fnltype=0
  fi

##get fit data from another machine
  if [ $machine = WCOSS -a $myhost != $myclient -a $fnltype = 0 ]; then
    export dirclient=${expdir[$n]}/$exp
    export dir$n=${rundir}/data/$exp
    rm -r ${rundir}/data/$exp
    mkdir -p ${rundir}/data/$exp/fits ${rundir}/data/$exp/horiz/anl ${rundir}/data/$exp/horiz/fcs 
    curday=$DATEST
    while [ $curday -le $DATEND ]; do
     scp $LOGNAME@${CLIENT}:$dirclient/fits/*${curday}*  ${rundir}/data/$exp/fits/.
     scp $LOGNAME@${CLIENT}:$dirclient/horiz/anl/*${curday}*  ${rundir}/data/$exp/horiz/anl/.
     scp $LOGNAME@${CLIENT}:$dirclient/horiz/fcs/*${curday}*  ${rundir}/data/$exp/horiz/fcs/.
     export curday=` $ndate +24 ${curday}00 `
     export curday=`echo $curday |cut -c 1-8 `
    done
  fi
n=`expr $n + 1 `
done


export mctl=1

dotp=1
dovp=1
dohp=1

export tplots=$dotp
export tcplots=$dotp

export vcomp=$dovp
export vplots=$dovp
export vcplots=$dovp

export hcomp=$dohp
export hplots=$dohp

export web=0
export rzdmdir=${ftpdir}
export tmpfit=${rundir}/fits

#-----------------------------------------------------------------------
#--local copy of the new html fit files, and place holder for web copy
cd $mapdir ||exit 8; mkdir -p $mapdir/fits
cp $scrdir/fitsweb/* $mapdir/fits
for html in `ls $scrdir/fitsweb/*box.html`  ##resolve exp names in box files
do
 sed -e "s/exp1/$exp1/g" $html|sed -e "s/exp2/$exp2/g" >$mapdir/fits/`basename $html`
done
cd $mapdir/fits ||exit 8
rm -rf horiz time vert; mkdir horiz time vert
cd $mapdir/fits/horiz; mkdir -p $exp1 $exp2
cd $mapdir/fits/vert;  mkdir -p $exp1 $exp2 ${exp1}-${exp2}
cd $mapdir/fits/time;  mkdir -p $exp1 $exp2 timeout f00af06 f12af36 f24af48


#--web server 
cd $mapdir
cat <<EOF >ftpfits.sh
#!/bin/ksh
set -x
 cd $mapdir
 tar cvf fits.tar ./fits
 ssh  -l $webid ${webmch} " rm -r ${ftpdir}/fits "
 ssh  -l $webid ${webmch} " mkdir  -p ${ftpdir}/fits "
 scp  ${mapdir}/fits.tar  ${webid}@${webmch}:${ftpdir}
 ssh  -l $webid ${webmch} "cd ${ftpdir} ; tar -xvf fits.tar "
 ssh  -l $webid ${webmch} "rm ${ftpdir}/fits.tar "
EOF
chmod u+x ftpfits.sh

#--if data transfer is allowed on computing nodes, plots are sent to server 
#--as soon as the they are made.  Otherwise, wait for 2hours for all jobs 
#--to finish, and then send plots to web server using dedicated transfer 
#--nodes (required by NCEP WCOSS)
waitfits=${waitfits:-"0100"}
if [ $doftp = "YES" ]; then
 export web=1
 if [ $CUE2RUN = $CUE2FTP ]; then
  sh +x $mapdir/ftpfits.sh
 else
  $SUBJOB -a $ACCOUNT -q $CUE2FTP -g $GROUP -p 1/1/S -r 256/1 -w +${waitfits} -t 1:00:00 -j ftpfits -o $mapdir/ftpfits.out ${mapdir}/ftpfits.sh
  sleep 120
 fi
fi


#-----------------------------------------------------------------------

#---get ready to plot fits
cd $rundir
export FITDIR=${scrdir}
export tmpdir=${rundir}                 
export MSCRIPTS=$FITDIR/scripts

export PROUT=$tmpdir/$exp2/prout
if [ ! -d $PROUT ] ; then
  mkdir -p $PROUT
fi
#export namstr="SURANJANA.SAHA/GMB/EMC/NCEP/NWS/NOAA"
export namstr="EMC/NCEP/NWS/NOAA"


export listvar1=exp1,exp2,sdate,edate,dir1,dir2,mctl,tplots,tcplots,vcomp,vplots,vcplots,hcomp,hplots,web,rzdmdir,webmch,webid,namstr,FITDIR,tmpdir,task,cue,NWPROD,GRADSBIN,IMGCONVERT,mapdir,endian1,endian2,,oinc_f2o,finc_f2o,fmax_f2o,expnlist,expdlist

${scrdir}/plotall.sh


exit
