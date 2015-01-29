#!/bin/ksh
set -x

## plot mean tracks of individual storm
##---------------------------------------------------------------
## Hurricance track plots, Automated by Fanglin Yang (March 4, 2008)
## Please first have cardyyyy* ready in scrdir/sorc if they do not exist 
## Check /com/arch/prod/syndat/syndat_tcvitals.year and 
##       http://www.nhc.noaa.gov/tracks/yyyyatl.gif for named hurricances
## track source: /tpc/noscrub/data/atcf-noaa/archive (have mismatches with real-time track)
## new track source: /global/shared/stat/tracks (consistent with real-time track, made by Vjay Tallapragada)

#for storm in Andres Blanca Carlos Dolores Enrique Felicia Guillermo Hilda Ignacio Jimena Kevin Linda Marty Nora Olaf Patricia Rick; do
for storm in  Olaf Patricia Rick; do
 case $storm in
  Andres)      code1=ep022009.dat; DATEST=20090619; DATEND=20090624;;
  Blanca)      code1=ep032009.dat; DATEST=20090703; DATEND=20090709;;
  Carlos)      code1=ep042009.dat; DATEST=20090708; DATEND=20090716;;
  Dolores)     code1=ep052009.dat; DATEST=20090712; DATEND=20090717;;
  Enrique)     code1=ep072009.dat; DATEST=20090803; DATEND=20090807;;
  Felicia)     code1=ep082009.dat; DATEST=20090803; DATEND=20090812;;
  Guillermo)   code1=ep102009.dat; DATEST=20090809; DATEND=20090820;;
  Hilda)       code1=ep112009.dat; DATEST=20090820; DATEND=20090829;;
  Ignacio)     code1=ep122009.dat; DATEST=20090822; DATEND=20090827;;
  Jimena)      code1=ep132009.dat; DATEST=20090826; DATEND=20090904;;
  Kevin)       code1=ep142009.dat; DATEST=20090826; DATEND=20090901;;
  Linda)       code1=ep152009.dat; DATEST=20090903; DATEND=20090912;;
  Marty)       code1=ep162009.dat; DATEST=20090913; DATEND=20090919;;
  Nora)        code1=ep172009.dat; DATEST=20090920; DATEND=20090923;;
  Olaf)        code1=ep182009.dat; DATEST=20091002; DATEND=20091004;;
  Patricia)    code1=ep192009.dat; DATEST=20091012; DATEND=20091014;;
  Rick)        code1=ep202009.dat; DATEST=20091016; DATEND=20091021;;
 esac
OCEAN=EP
#-----------------------
export scrdir=/climate/save/wx24fy/VRFY/vrfy_fyang/hurtrack
export expdir=${expdir:-/global/hires/glopara/archive}       ;#experiment data archive directory
export mdlist=${mdlist:-"pru12r pre13d "}                             ;#experiment names
export DATEST=${DATEST:-20090619}                             ;#forecast starting date
export DATEND=${DATEND:-20091110}                             ;#forecast ending date
export OCEAN=${OCEAN:-"EP"}                                ;#basin you are verifying, AL-Atlantic, EP-Eastern Pacific
export cyc=${cyc:-"00 06 12 18"}                                    ;#forecast cycles to be included in verification        
export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$LOGNAME/vsdb_glopara/Q3FY10_2009}   ;#where maps are displayed on emcrzdm.ncep.noaa.gov
export doftp=${doft:-"YES"}                                   ;#whether or not sent maps to ftpdir
export rundir=${rundir:-/stmp/$LOGNAME/track1}
mkdir -p ${rundir}; cd $rundir


#---------------------------------------------------------
#---------------------------------------------------------
#---Most likely you do not need to change anything below--
#---------------------------------------------------------
#---------------------------------------------------------
set -A mdname $mdlist
execdir=${rundir}/${storm}                     ;# working directory
rm -r $execdir; mkdir -p $execdir
cd $execdir; chmod u+rw *

years=`echo $DATEST |cut -c 1-4 `
yeare=`echo $DATEND |cut -c 1-4 `
if [ $years -ne $yeare ]; then
 echo " years=$years, yeare=$yeare.  Must have years=yeare. exit"
 exit
fi 
export year=$years

## copy HPC/JTWC tracks to working directory (HPC's tracks sometime do not match with real-time tracks)
tpctrack=${execdir}/tpctrack           ;#place to hold HPC original track data
mkdir -p $tpctrack

#TPC Atlantic and Eastern Pacific tracks
#tpcdata=/tpc/noscrub/data/atcf-noaa/archive
#cp ${tpcdata}/${year}/aal*   ${tpctrack}/.
#cp ${tpcdata}/${year}/bal*   ${tpctrack}/.
#cp ${tpcdata}/${year}/aep*   ${tpctrack}/.
#cp ${tpcdata}/${year}/bep*   ${tpctrack}/.
#gunzip ${tpctrack}/*${year}.dat.gz

tpcdata=/global/shared/stat/tracks
#tpcdata=/climate/save/wx24fy/VRFY/vsdb_exp/hurtrack
cp ${tpcdata}/${year}/aal*   ${tpctrack}/.
cp ${tpcdata}/${year}/bal*   ${tpctrack}/.
cp ${tpcdata}/${year}/aep*   ${tpctrack}/.
cp ${tpcdata}/${year}/bep*   ${tpctrack}/.

#JTWC Western Pacific tracks
jtwcdata=/tpc/noscrub/data/atcf-navy
cp ${jtwcdata}/aid/awp*${year}.dat   ${tpctrack}/.
cp ${jtwcdata}/btk/bwp*${year}.dat   ${tpctrack}/.

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
namenew=`echo $exp |cut -c 1-4 `                 
 if [ $exp = "pru12h" ]; then namenew=u12h; fi
 if [ $exp = "pru12r" ]; then namenew=u12r; fi
 if [ $exp = "pre13" ]; then namenew=pe13 ; fi
 if [ $exp = "pre13a" ]; then namenew=e13a ; fi
 if [ $exp = "pre13d" ]; then namenew=e13d ; fi
namenew=`echo $namenew |tr "[a-z]" "[A-Z]" `
export newlist=${newlist}"${namenew} "           ;#donot delete the space at the end

outfile=${execdir}/atcfunix.$exp.$year
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
$scrdir/sorc/insert_new.sh $exp $BASIN $year $tpctrack $outfile $execdir
done   
done         ;#end of experiment loop

#------------------------------------------------------------------------


#------------------------------------------------------------------------
#  prepare data for GrADS graphics
#------------------------------------------------------------------------
for BASIN in $OCEAN; do
bas=`echo $BASIN |tr "[A-Z]" "[a-z]" `

## copy test cards, replace dummy exp name MOD# with real exp name
cp ${scrdir}/sorc/card.i .
cp ${scrdir}/sorc/card.t .
cat >stormlist <<EOF
$code1
EOF

cat card.i stormlist >card${year}_${bas}.i
cat card.t stormlist >card${year}_${bas}.t

#newlisti=${newlist}"AVNO GFDL OFCL SHF5 SHIP DSHP"
#newlistt=${newlist}"AVNO GFDL OFCL CLP5"
#newlistt=${newlist}"AVNO GFDL HWRF OFCL CLP5"
newlisti=${newlist}"AVNO OFCL SHF5"
newlistt=${newlist}"AVNO OFCL CLP5"

nint=`echo $newlisti |wc -w`     ;#number of process for intensity plot, to replace NUMINT in card.i
ntrc=`echo $newlistt |wc -w`     ;#number of process for track plot, to replace NUMTRC in card.t
nint=`expr $nint + 0 `           ;#remove extra space
ntrc=`expr $ntrc + 0 `
sed -e "s/MODLIST/${newlisti}/g" -e "s/NUMINT/${nint}/g" card${year}_$bas.i >card_$bas.i   
sed -e "s/MODLIST/${newlistt}/g" -e "s/NUMTRC/${ntrc}/g" card${year}_$bas.t >card_$bas.t   


## produce tracks.t.out etc
cp $tpctrack/b*${year}.dat .
${scrdir}/sorc/nhcver.x card_${bas}.t tracks_${bas}.t  $execdir
${scrdir}/sorc/nhcver.x card_${bas}.i tracks_${bas}.i  $execdir


## create grads files tracks_${bas}.t.dat etc for plotting
${scrdir}/sorc/tvercut_new.sh ${execdir}/tracks_${bas}.t.out $scrdir/sorc
${scrdir}/sorc/ivercut_new.sh ${execdir}/tracks_${bas}.i.out $scrdir/sorc

## copy grads scripts and make plots                        
if [ $BASIN = "AL" ]; then place="Atlantic"; fi
if [ $BASIN = "EP" ]; then place="East-Pacific"; fi
period="${storm}__${code1}__${DATEST}-${DATEND}_4cyc"
#period="${storm}__${code1}__${DATEST}-${DATEND}_00Z"
cp ${scrdir}/sorc/iver.gs .
cp ${scrdir}/sorc/tver.gs .
grads -bcp "run iver.gs tracks_${bas}.i  $year $place $period"
grads -bcp "run tver.gs tracks_${bas}.t  $year $place $period"

mv tracks_${bas}.i.gif  tracks_${storm}.i.gif
mv tracks_${bas}.t.gif  tracks_${storm}.t.gif
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
  mput tracks_${storm}*.gif
  put tracks_ep.t.out tracks_${storm}.t.txt 
  put tracks_ep.i.out tracks_${storm}.i.txt 
  quit
EOF
 ftp -i -v emcrzdm.ncep.noaa.gov <ftpin 
fi

## save tracks
savedir=${scrdir}/arch_trak/${mdname[1]}$years$yeare
mkdir -p $savedir
cp ${execdir}/tracks_${storm}*.gif  ${savedir}/.
cp ${execdir}/tracks_ep.t.out ${savedir}/tracks_${storm}.t.txt
cp ${execdir}/tracks_ep.i.out ${savedir}/tracks_${storm}.i.txt


#---end of individual storm 
done
#---end of individual storm 
exit
