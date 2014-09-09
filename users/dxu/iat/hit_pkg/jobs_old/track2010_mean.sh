#!/bin/ksh
set -x

## Hurricance track plots, Automated by Fanglin Yang (March 4, 2008)
## Please first have cardyyyy* ready in scrdir/sorc if they do not exist 
## Check /com/arch/prod/syndat/syndat_tcvitals.year and 
##       http://www.nhc.noaa.gov/tracks/yyyyatl.gif for named hurricances
## track source: /tpc/noscrub/data/atcf-noaa/archive (have mismatches with real-time track)
## new track source: /global/shared/stat/tracks (consistent with real-time track, made by Vjay Tallapragada)


export scrdir=/global/save/wx24fy/VRFY/hurtrack                    
export expdir=${expdir:-/global/hires/glopara/archive}       ;#experiment data archive directory
export mdlist=${mdlist:-"pre13d prd11q1y pre13j"}                      ;#experiment names
export DATEST=${DATEST:-20100602}                             ;#forecast starting date
export DATEND=${DATEND:-20101110}                             ;#forecast ending date
export OCEAN=${OCEAN:-"AL EP WP"}                             ;#basin, AL-Atlantic, EP-Eastern Pacific, WP-Western Pacific
export cyc=${cyc:-"00 06 12 18"}                              ;#forecast cycles to be included in verification        
export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$LOGNAME/vsdb_glopara/pre13j}   ;#where maps are displayed on emcrzdm.ncep.noaa.gov
export doftp=${doft:-"YES"}                                   ;#whether or not sent maps to ftpdir
export rundir=${rundir:-/stmp/$LOGNAME/track}
mkdir -p ${rundir}; cd $rundir


#---------------------------------------------------------
#---------------------------------------------------------
#---Most likely you do not need to change anything below--
#---------------------------------------------------------
#---------------------------------------------------------
set -A mdname $mdlist
execdir=${rundir}/${mdname[0]}                     ;# working directory
rm -r $execdir; mkdir -p $execdir
cd $execdir; chmod u+rw *

years=`echo $DATEST |cut -c 1-4 `
yeare=`echo $DATEND |cut -c 1-4 `
yearlabel=$years$yeare
if [ $years = $yeare ]; then yearlabel=$years ; fi


#----------------------------------
export year=$years
while  [ $year -le $yeare ]; do

## copy HPC/JTWC tracks to working directory (HPC's tracks sometime do not match with real-time tracks)
tpctrack=${execdir}/tpctrack           ;#place to hold HPC original track data 
mkdir -p $tpctrack

#TPC Atlantic and Eastern Pacific tracks
if [ -s $scrdir/tpctrack/${year}/aal01${year}.dat ]; then
 tpcdata=$scrdir/tpctrack
 cp ${tpcdata}/${year}/aal*   ${tpctrack}/.       
 cp ${tpcdata}/${year}/bal*   ${tpctrack}/.      
 cp ${tpcdata}/${year}/aep*   ${tpctrack}/.     
 cp ${tpcdata}/${year}/bep*   ${tpctrack}/.    
elif [ -s /tpc/noscrub/data/atcf-noaa/archive/${year}/aal01${year}.dat.gz ]; then
 tpcdata=/tpc/noscrub/data/atcf-noaa/archive
 cp ${tpcdata}/${year}/aal*   ${tpctrack}/.       
 cp ${tpcdata}/${year}/bal*   ${tpctrack}/.      
 cp ${tpcdata}/${year}/aep*   ${tpctrack}/.     
 cp ${tpcdata}/${year}/bep*   ${tpctrack}/.    
 gunzip ${tpctrack}/*${year}.dat.gz
elif [ -s /tpc/noscrub/data/atcf-noaa/aid/aal01${year}.dat ]; then
 tpcdata=/tpc/noscrub/data/atcf-noaa
 cp ${tpcdata}/aid/aal*${year}*  ${tpctrack}/.       
 cp ${tpcdata}/btk/bal*${year}*  ${tpctrack}/.      
 cp ${tpcdata}/aid/aep*${year}*  ${tpctrack}/.     
 cp ${tpcdata}/btk/bep*${year}*  ${tpctrack}/.    
else
 echo" HPC track not found, exit"
 exit 8
fi

#JTWC Western Pacific tracks
jtwcdata=/tpc/noscrub/data/atcf-navy
cp ${jtwcdata}/aid/awp*${year}.dat   ${tpctrack}/.       
cp ${jtwcdata}/btk/bwp*${year}.dat   ${tpctrack}/.       

year=` expr $year + 1 `
done       
#----------------------------------


#------------------------------------------------------------------------
#  insert experiment track to TPC track  for all runs and for all BASINs
#------------------------------------------------------------------------
newlist=""
fout=24      
nexp=`echo $mdlist |wc -w`           
ncyc=`echo $cyc |wc -w`           
if [ $ncyc -eq 3 ]; then ncyc=2; fi
fout=`expr $fout \/ $ncyc `

for exp in $mdlist; do

## cat experiment track data for each exp 
nameold=`echo $exp |cut -c 1-4 `                 ;#current fcst always uses first 4 letters of experiment name
nameold=`echo $nameold |tr "[a-z]" "[A-Z]" `
namenew=`echo $exp |cut -c 1-4 `                 ;#exclude "pr" to distinguish, say, prs50h and prs51h
 if [ $exp = "pru12h" ]; then namenew=u12h; fi
 if [ $exp = "pru12r" ]; then namenew=u12r; fi
 if [ $exp = "pre13" ]; then namenew=pe13 ; fi
 if [ $exp = "pre13a" ]; then namenew=e13a ; fi
 if [ $exp = "pre13d" ]; then namenew=e13d ; fi
 if [ $exp = "prd11q1y" ]; then namenew=gsiy ; fi
 if [ $exp = "pre13j" ]; then namenew=e13j ; fi
namenew=`echo $namenew |tr "[a-z]" "[A-Z]" `
export newlist=${newlist}"${namenew} "           ;#donot delete the space at the end

outfile=${execdir}/atcfunix.$exp.$yearlabel
if [ -s $outfile ]; then rm $outfile; fi
touch $outfile
indir=${expdir}/$exp
date=${DATEST}00    
until [ $date -gt ${DATEND}18 ] ; do
   infile=$indir/atcfunix.gfs.$date
   if [ -s $infile ]; 
     if [ -s infiletmp ]; then rm infiletmp; fi
     sed "s?$nameold?$namenew?g" $infile >infiletmp
     then cat infiletmp >> $outfile 
   fi
   date=`/nwprod/util/exec/ndate +$fout $date`
done

## insert experiment track into TPC tracks
for BASIN in $OCEAN; do
 export year=$years
 while  [ $year -le $yeare ]; do
  $scrdir/sorc/insert_new.sh $exp $BASIN $year $tpctrack $outfile $execdir
  ##$scrdir/sorc/hwrf_insert_track.pl -v  $outfile 
  year=` expr $year + 1 `
 done       
done   

done         ;#end of experiment loop
#------------------------------------------------------------------------


#------------------------------------------------------------------------
#  prepare data for GrADS graphics
#------------------------------------------------------------------------
for BASIN in $OCEAN; do
bas=`echo $BASIN |tr "[A-Z]" "[a-z]" `

## copy test cards, replace dummy exp name MOD# with real exp name
cp ${scrdir}/sorc/card${yearlabel}_${bas}.i .     
cp ${scrdir}/sorc/card${yearlabel}_${bas}.t .     

if [ $BASIN = "WP" ]; then
newlisti=${newlist}"AVNO JTWC"
newlistt=${newlist}"AVNO JTWC"
else
#newlistt=${newlist}"AVNO GFDL OFCL CLP5"
#newlistt=${newlist}"AVNO GFDL HWRF OFCL CLP5"
newlisti=${newlist}"AVNO OFCL SHF5"
newlistt=${newlist}"AVNO OFCL CLP5"
fi

nint=`echo $newlisti |wc -w`     ;#number of process for intensity plot, to replace NUMINT in card.i
ntrc=`echo $newlistt |wc -w`     ;#number of process for track plot, to replace NUMTRC in card.t
nint=`expr $nint + 0 `           ;#remove extra space
ntrc=`expr $ntrc + 0 `
sed -e "s/MODLIST/${newlisti}/g" -e "s/NUMINT/ ${nint}/g" card${yearlabel}_$bas.i >card_$bas.i   
sed -e "s/MODLIST/${newlistt}/g" -e "s/NUMTRC/ ${ntrc}/g" card${yearlabel}_$bas.t >card_$bas.t   


## produce tracks.t.out etc
cp $tpctrack/b*dat   .
${scrdir}/sorc/nhcver.x card_${bas}.t tracks_${bas}.t  $execdir
${scrdir}/sorc/nhcver.x card_${bas}.i tracks_${bas}.i  $execdir


## create grads files tracks_${bas}.t.dat etc for plotting
if [ $BASIN = "WP" ]; then
 ${scrdir}/sorc/top_tvercut.sh ${execdir}/tracks_${bas}.t.out $scrdir/sorc
 ${scrdir}/sorc/top_ivercut.sh ${execdir}/tracks_${bas}.i.out $scrdir/sorc
else
 ${scrdir}/sorc/tvercut_new.sh ${execdir}/tracks_${bas}.t.out $scrdir/sorc
 ${scrdir}/sorc/ivercut_new.sh ${execdir}/tracks_${bas}.i.out $scrdir/sorc
fi

## copy grads scripts and make plots                        
if [ $BASIN = "AL" ]; then place="Atlantic"; fi
if [ $BASIN = "EP" ]; then place="East-Pacific"; fi
if [ $BASIN = "WP" ]; then place="West-Pacific"; fi
period="${DATEST}__${DATEND}__4cyc"
#period="${DATEST}__${DATEND}__00Z"

cp ${scrdir}/sorc/*gs .

if [ $BASIN = "WP" ]; then
 grads -bcp "run top_iver.gs tracks_${bas}.i  $yearlabel $place $period"
 grads -bcp "run top_tver.gs tracks_${bas}.t  $yearlabel $place $period"
else
 grads -bcp "run iver.gs tracks_${bas}.i  $yearlabel $place $period"
 grads -bcp "run tver.gs tracks_${bas}.t  $yearlabel $place $period"
fi

#mv tracks_${bas}.i.gif  ${exp}${year}.tracks_${bas}.i.gif
#mv tracks_${bas}.t.gif  ${exp}${year}.tracks_${bas}.t.gif
#----------------------------
done     ;# end of BASIN loop
#----------------------------


if [ $doftp = "YES" ]; then
cat << EOF >ftpin
  cd $ftpdir
  mkdir track
  cd track
  binary
  promt
  mput tracks*.gif
  put tracks_al.t.out tracks_al.t.txt
  put tracks_ep.t.out tracks_ep.t.txt
  put tracks_wp.t.out tracks_wp.t.txt
  put tracks_al.i.out tracks_al.i.txt
  put tracks_ep.i.out tracks_ep.i.txt
  put tracks_wp.i.out tracks_wp.i.txt
  quit
EOF
 ftp -i -v emcrzdm.ncep.noaa.gov <ftpin 
fi

## save tracks 
savedir=${scrdir}/arch_trak/${mdname[1]}$yearlabel
mkdir -p $savedir
cp ${execdir}/*.gif ${savedir}/.
cp ${execdir}/a*dat ${savedir}/.
cp ${execdir}/b*dat ${savedir}/.
cp ${execdir}/atcfunix* ${savedir}/.
cp ${execdir}/tracks_al.t.out ${savedir}/tracks_al.t.txt
cp ${execdir}/tracks_ep.t.out ${savedir}/tracks_ep.t.txt
cp ${execdir}/tracks_wp.i.out ${savedir}/tracks_wp.t.txt
cp ${execdir}/tracks_al.i.out ${savedir}/tracks_al.i.txt
cp ${execdir}/tracks_ep.i.out ${savedir}/tracks_ep.i.txt
cp ${execdir}/tracks_wp.i.out ${savedir}/tracks_wp.i.txt

exit
