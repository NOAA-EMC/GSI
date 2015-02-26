#!/bin/ksh
set -x

## plot mean tracks of individual storms in the Eastern Pacific
## Fanglin Yang, March 2008: original copy adopted from HWRF. Restructured and added driver scripts.
## Fanglin Yang, March 2013: Generalized for running on CCS, WCOSS and ZEUS
## Update: 
##  9/9/2014, Deyong Xu/RTi @ NOAA/NESDIS/STAR/JCSDA : added configuration file
##  

#-------------------------------------------------------------------------------------
export expdir=/scratch2/portfolios/NCEPDEV/global/noscrub/Fanglin.Yang/archive      ;#experiment data archive directory
export mdlist="prctl prexpgsi"                      ;#experiment names
export mdplot="ctl gsi"                             ;#names to be shown on plots, limitted to 4 letters
#export cyc="00 06 12 18"                           ;#forecast cycles to be included in verification
export cyc="00"                                     ;#forecast cycles to be included in verification
export doftp="YES"                                  ;#whether or not sent maps to ftpdir
export webhostid=wx24fy
export webhost=emcrzdm.ncep.noaa.gov
export ftpdir=/home/people/emc/www/htdocs/gmb/$webhostid/vsdb/prctl


#-------------------------------------------------------------------------------------
#---------------------------------------------------------
#---Most likely you do not need to change anything below--
#---------------------------------------------------------
#-------------------------------------------------------------------------------------
myhost=`echo $(hostname) |cut -c 1-1 `
if [ $myhost = "f" ]; then
 export scrdir=/scratch2/portfolios/NCEPDEV/global/save/Fanglin.Yang/VRFY/hurtrack
 export STMP="/scratch2/portfolios/NCEPDEV/stmp"
 export NDATE=/scratch2/portfolios/NCEPDEV/global/save/Fanglin.Yang/VRFY/vsdb/nwprod/util/exec/ndate
elif [ $myhost = "t" -o $myhost = "g" ]; then
 export scrdir=/global/save/Fanglin.Yang/VRFY/hurtrack
 export STMP="/stmpd2"
 export NDATE=/nwprod/util/exec/ndate
fi

##################################################################
# Use config file instead to streamline configuration
# without making big changes to original HIT package,
# which is easier for future merging the original HIT from svn.
##################################################################
source ./hit_template.config

export rundir=${rundir:-$STMP/$LOGNAME/track}
mkdir -p ${rundir}; cd $rundir || exit 8


#==================================================================
for storm in ${ENV_STORMS} ;do
 case $storm in
  Aletta)        code1=ep012012.dat; DATEST=20120514; DATEND=20120519;;
  Bud)           code1=ep022012.dat; DATEST=20120521; DATEND=20120526;;
  Carlotta)      code1=ep032012.dat; DATEST=20120614; DATEND=20120617;;
  Daniel)        code1=ep042012.dat; DATEST=20120704; DATEND=20120711;;
  Emilia)        code1=ep052012.dat; DATEST=20120707; DATEND=20120715;;
  Fabio)         code1=ep062012.dat; DATEST=20120712; DATEND=20120718;;
  Gilma)         code1=ep072012.dat; DATEST=20120807; DATEND=20120811;;
  Hector)        code1=ep082012.dat; DATEST=20120811; DATEND=20120817;;
  Ileana)        code1=ep092012.dat; DATEST=20120827; DATEND=20120902;;
  John)          code1=ep102012.dat; DATEST=20120902; DATEND=20120904;;
  Kristy)        code1=ep112012.dat; DATEST=20120912; DATEND=20120917;;
  Lane)          code1=ep122012.dat; DATEST=20120915; DATEND=20120919;;
  Miriam)        code1=ep132012.dat; DATEST=20120922; DATEND=20120928;;
  Norman)        code1=ep142012.dat; DATEST=20120928; DATEND=20120929;;
  Olivia)        code1=ep152012.dat; DATEST=20121006; DATEND=20121009;;
  Paul)          code1=ep162012.dat; DATEST=20121013; DATEND=20121017;;
  Rosa)          code1=ep172012.dat; DATEST=20121030; DATEND=20121103;;
 esac
OCEAN=EP

#---------------------------------------------------------
#---------------------------------------------------------
set -A mdname $mdlist; set -A mdpt $mdplot
execdir=${rundir}/${storm}                     ;# working directory
rm -r $execdir; mkdir -p $execdir
cd $execdir; chmod u+rw *

# Save output location where to extrac figures to generate PAR
echo ${execdir}  >> ${ENV_SCRDIR}/hit_data_loc

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
if [ -s $scrdir/tpctrack/${year}/aep01${year}.dat ]; then
 tpcdata=$scrdir/tpctrack
 cp ${tpcdata}/${year}/aep*.dat   ${tpctrack}/.
 cp ${tpcdata}/${year}/bep*.dat   ${tpctrack}/.
elif [ -s /nhc/noscrub/data/atcf-noaa/aid_nws/aep01${year}.dat ]; then
 tpcdata=/nhc/noscrub/data/atcf-noaa
 cp ${tpcdata}/aid_nws/aep*${year}*.dat  ${tpctrack}/.
 cp ${tpcdata}/btk/bep*${year}*.dat      ${tpctrack}/.
else
 echo" HPC track not found, exit"
 exit 8
fi

#JTWC Western Pacific tracks
#jtwcdata=/nhc/noscrub/data/atcf-navy
#cp ${jtwcdata}/aid/awp*${year}.dat   ${tpctrack}/.
#cp ${jtwcdata}/btk/bwp*${year}.dat   ${tpctrack}/.


#------------------------------------------------------------------------
#  insert experiment track to TPC track  for all runs and for all BASINs
#------------------------------------------------------------------------
newlist=""
fout=24      
nexp=`echo $mdlist |wc -w`           
ncyc=`echo $cyc |wc -w |sed 's/ //g'`           
if [ $ncyc -eq 3 ]; then ncyc=2; fi
fout=`expr $fout \/ $ncyc `


n=0
if [ $nexp -gt 0 ]; then
for exp in $mdlist; do

## cat experiment track data for each exp 
nameold=`echo $exp |cut -c 1-4 `                 ;#current fcst always uses first 4 letters of experiment name
nameold=`echo $nameold |tr "[a-z]" "[A-Z]" `
#namenew=`echo $exp |cut -c 1-4 `                 
namenew=${mdpt[$n]} ; n=$((n+1))
namenew=`echo $namenew |tr "[a-z]" "[A-Z]" `
export newlist=${newlist}"${namenew} "           ;#donot delete the space at the end

dump=.gfs.
if [ $exp = fim ]; then
 dump=.fim.
 nameold=" F8C"
fi

outfile=${execdir}/atcfunix.$exp.$year
if [ -s $outfile ]; then rm $outfile; fi
touch $outfile
indir=${expdir}/$exp
date=${DATEST}00    
until [ $date -gt ${DATEND}18 ] ; do
   infile=$indir/atcfunix${dump}$date
   if [ -s $infile ]; 
     if [ -s infiletmp ]; then rm infiletmp; fi
     sed "s?$nameold?$namenew?g" $infile >infiletmp
     then cat infiletmp >> $outfile 
   fi
   date=`$NDATE +$fout $date`
done


## insert experiment track into TPC tracks
for BASIN in $OCEAN; do
$scrdir/sorc/insert_new.sh $exp $BASIN $year $tpctrack $outfile $execdir
done   
done         ;#end of experiment loop

else
 ln -fs $tpctrack/* .
fi
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

if [ $BASIN = "WP" ]; then
newlisti=${newlist}"AVNO JTWC"
newlistt=${newlist}"AVNO JTWC"
else
#newlistt=${newlist}"AVNO GFDL OFCL CLP5"
#newlistt=${newlist}"AVNO GFDL HWRF OFCL CLP5"
#newlisti=${newlist}"AVNO HWRF GFDL  EMX  UKM NGPS OFCL "
#newlistt=${newlist}"AVNO HWRF GFDL  EMX  UKM NGPS OFCL"
newlisti=${newlist}"AVNO OFCL"
newlistt=${newlist}"AVNO OFCL"
fi

nint=`echo $newlisti |wc -w`     ;#number of process for intensity plot, to replace NUMINT in card.i
ntrc=`echo $newlistt |wc -w`     ;#number of process for track plot, to replace NUMTRC in card.t
nint=`expr $nint + 0 `           ;#remove extra space
ntrc=`expr $ntrc + 0 `
sed -e "s/MODLIST/${newlisti}/g" -e "s/NUMINT/${nint}/g" card${year}_$bas.i >card_$bas.i   
sed -e "s/MODLIST/${newlistt}/g" -e "s/NUMTRC/${ntrc}/g" card${year}_$bas.t >card_$bas.t   


## produce tracks.t.txt etc
cp $tpctrack/b*${year}.dat .
${scrdir}/sorc/nhcver.x card_${bas}.t tracks_${bas}.t  $execdir
${scrdir}/sorc/nhcver.x card_${bas}.i tracks_${bas}.i  $execdir


## create grads files tracks_${bas}.t.dat etc for plotting
 ${scrdir}/sorc/top_tvercut.sh ${execdir}/tracks_${bas}.t.txt $scrdir/sorc
 ${scrdir}/sorc/top_ivercut.sh ${execdir}/tracks_${bas}.i.txt $scrdir/sorc


## copy grads scripts and make plots                        
if [ $BASIN = "AL" ]; then place="Atlantic"; fi
if [ $BASIN = "EP" ]; then place="East-Pacific"; fi
period="${storm}__${DATEST}_${DATEND}_${ncyc}cyc"
cp ${scrdir}/sorc/*iver*.gs .
cp ${scrdir}/sorc/*tver*.gs .

 grads -bcp "run top_iver.gs tracks_${bas}.i  $year $place $period"
 grads -bcp "run top_tver_250.gs tracks_${bas}.t  $year $place $period"

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
  put tracks_al.t.txt tracks_${storm}.t.txt
  put tracks_al.i.txt tracks_${storm}.i.txt
  quit
EOF
 sftp  ${webhostid}@${webhost} <ftpin
fi


## save tracks
#savedir=${scrdir}/arch_trak/${mdname[0]}$years$yeare
#mkdir -p $savedir
#cp ${execdir}/tracks_${storm}*.gif  ${savedir}/.
#cp ${execdir}/tracks_al.t.txt ${savedir}/tracks_${storm}.t.txt
#cp ${execdir}/tracks_al.i.txt ${savedir}/tracks_${storm}.i.txt


#---end of individual storm 
done
#---end of individual storm 
exit
