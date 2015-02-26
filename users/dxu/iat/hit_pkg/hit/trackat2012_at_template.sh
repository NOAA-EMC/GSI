#!/bin/ksh
set -x

## plot mean tracks of individual storms in the Atlantic
## Fanglin Yang, March 2008: original copy adopted from HWRF. Restructured and added driver scripts.
## Fanglin Yang, March 2013: Generalized for running on WCOSS and ZEUS
## Update: 
##  9/9/2014, Deyong Xu/RTi @ NOAA/NESDIS/STAR/JCSDA : added configuration file
##  

#-------------------------------------------------------------------------------------
export expdir=/scratch2/portfolios/NCEPDEV/global/noscrub/Fanglin.Yang/archive      ;#experiment data archive directory
export mdlist="prctl prexpgsi"                      ;#experiment names
export mdplot="ctl gsi"                             ;#names to be shown on plots, limitted to 4 letters
#export cyc="00 06 12 18"                           ;#forecast cycles to be included in verification
export cyc="00"                                     ;#forecast cycles to be included in verification
export doftp="NO"                                   ;#whether or not sent maps to ftpdir
export webhostid=wx24fy
export webhost=emcrzdm.ncep.noaa.gov
export ftpdir=/home/people/emc/www/htdocs/gmb/$webhostid/vsdb/prt1534            


#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#---------------------------------------------------------
#---Most likely you do not need to change anything below--
#---------------------------------------------------------
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
for storm in Alberto Beryl Chris Debby Ernesto Florence Helene Gordon  Isaac Joyce Kirk Leslie Michael Nadine Oscar Patty Rafael Sandy Tony ;   do
 case $storm in
  Alberto)  code1=al012012.dat; DATEST=20120519; DATEND=20120522;;
  Beryl)    code1=al022012.dat; DATEST=20120526; DATEND=20120530;;
  Chris)    code1=al032012.dat; DATEST=20120619; DATEND=20120622;;
  Debby)    code1=al042012.dat; DATEST=20120623; DATEND=20120630;;
  Ernesto)  code1=al052012.dat; DATEST=20120801; DATEND=20120810;;
  Florence) code1=al062012.dat; DATEST=20120804; DATEND=20120808;;
  Helene)   code1=al072012.dat; DATEST=20120809; DATEND=20120819;;
  Gordon)   code1=al082012.dat; DATEST=20120815; DATEND=20120820;;
  Isaac)    code1=al092012.dat; DATEST=20120821; DATEND=20120826;;
  Joyce)    code1=al102012.dat; DATEST=20120822; DATEND=20120824;;
  Kirk)     code1=al112012.dat; DATEST=20120828; DATEND=20120902;;
  Leslie)   code1=al122012.dat; DATEST=20120830; DATEND=20120911;;
  Michael)  code1=al132012.dat; DATEST=20120903; DATEND=20120911;;
  Nadine)   code1=al142012.dat; DATEST=20120911; DATEND=20121004;;
  Oscar)    code1=al152012.dat; DATEST=20121003; DATEND=20121005;;
  Patty)    code1=al162012.dat; DATEST=20121011; DATEND=20121013;;
  Rafael)   code1=al172012.dat; DATEST=20121012; DATEND=20121017;;
  Sandy)    code1=al182012.dat; DATEST=20121022; DATEND=20121030;;
  Tony)     code1=al192012.dat; DATEST=20121022; DATEND=20121025;;
 esac
OCEAN=AL

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
if [ -s $scrdir/tpctrack/${year}/aal01${year}.dat ]; then
 tpcdata=$scrdir/tpctrack
 cp ${tpcdata}/${year}/aal*.dat  ${tpctrack}/.
 cp ${tpcdata}/${year}/bal*.dat  ${tpctrack}/.
elif [ -s /nhc/noscrub/data/atcf-noaa/aid_nws/aal01${year}.dat ]; then
 tpcdata=/nhc/noscrub/data/atcf-noaa
 cp ${tpcdata}/aid_nws/aal*${year}*.dat  ${tpctrack}/.
 cp ${tpcdata}/btk/bal*${year}*.dat      ${tpctrack}/.
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
