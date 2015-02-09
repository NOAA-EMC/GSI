#!/bin/ksh
set -x

## Hurricance track plots, Automated by Fanglin Yang 
## Fanglin Yang, March 2008: original copy adopted from HWRF. Restructured and added driver scripts.
## Fanglin Yang, March 2013: Generalized for running on WCOSS and ZEUS      
## Update: 
##  9/9/2014, Deyong Xu/RTi @ NOAA/NESDIS/STAR/JCSDA : added configuration file
##  

#-------------------------------------------------------------------------------------
#export expdir=/scratch2/portfolios/NCEPDEV/global/noscrub/Fanglin.Yang/archive      ;#experiment data archive directory
export expdir=/global/noscrub/Russ.Treadon/archive                                   ;#experiment data archive directory
export mdlist="prhw14"                             ;#experiment names
export mdplot="hw14"                                ;#names to be shown on plots, limitted to 4 letters
export DATEST=20140510                              ;#forecast starting date
export DATEND=20140706                              ;#forecast ending date
#export OCEAN="AL EP WP"                             ;#basin, AL-Atlantic, EP-Eastern Pacific, WP-Western Pacific
export OCEAN="AL"                             ;#basin, AL-Atlantic, EP-Eastern Pacific, WP-Western Pacific
export cyc="00 06 12 18"                           ;#forecast cycles to be included in verification        
export doftp="YES"                                  ;#whether or not sent maps to ftpdir
export webhostid=wx24fy
export webhost=emcrzdm.ncep.noaa.gov
export ftpdir=/home/people/emc/www/htdocs/gmb/$webhostid/vsdb/prhw14


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
mkdir -p ${rundir}; cd $rundir ||exit 8


#---------------------------------------------------------
#---------------------------------------------------------
set -A mdname $mdlist; set -A mdpt $mdplot
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
if [ -s /nhc/noscrub/data/atcf-noaa/aid_nws/aal01${year}.dat ]; then
 tpcdata=/nhc/noscrub/data/atcf-noaa
 cp ${tpcdata}/aid_nws/aal*${year}*.dat  ${tpctrack}/.       
 cp ${tpcdata}/btk/bal*${year}*.dat      ${tpctrack}/.      
 cp ${tpcdata}/aid_nws/aep*${year}*.dat  ${tpctrack}/.     
 cp ${tpcdata}/btk/bep*${year}*.dat      ${tpctrack}/.    
elif [ -s $scrdir/tpctrack/${year}/aal01${year}.dat ]; then
 tpcdata=$scrdir/tpctrack                   
 cp ${tpcdata}/${year}/aal*.dat   ${tpctrack}/.       
 cp ${tpcdata}/${year}/bal*.dat   ${tpctrack}/.      
 cp ${tpcdata}/${year}/aep*.dat   ${tpctrack}/.     
 cp ${tpcdata}/${year}/bep*.dat   ${tpctrack}/.    
else
 echo" HPC track not found, exit"
 exit 8
fi

#JTWC Western Pacific tracks
jtwcdata=/nhc/noscrub/data/atcf-navy
if [ -s $jtwcdata ]; then
 cp ${jtwcdata}/aid/awp*${year}.dat   ${tpctrack}/.       
 cp ${jtwcdata}/btk/bwp*${year}.dat   ${tpctrack}/.       
else
 cp ${tpcdata}/${year}/awp*.dat   ${tpctrack}/.
 cp ${tpcdata}/${year}/bwp*.dat   ${tpctrack}/.
fi


year=` expr $year + 1 `
done       
#----------------------------------


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
#namenew=`echo $exp |cut -c 1-4 `                 ;#exclude "pr" to distinguish, say, prs50h and prs51h
namenew=${mdpt[$n]} ; n=$((n+1))
namenew=`echo $namenew |tr "[a-z]" "[A-Z]" `
export newlist=${newlist}"${namenew} "           ;#donot delete the space at the end

dump=.gfs.
if [ $exp = fim ]; then 
 dump=.fim.
 nameold=" F8C"
fi

outfile=${execdir}/atcfunix.$exp.$yearlabel
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
 export year=$years
 while  [ $year -le $yeare ]; do
  $scrdir/sorc/insert_new.sh $exp $BASIN $year $tpctrack $outfile $execdir
  year=` expr $year + 1 `
 done       
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
cp ${scrdir}/sorc/card${yearlabel}_${bas}.i .     
cp ${scrdir}/sorc/card${yearlabel}_${bas}.t .     

if [ $BASIN = "WP" ]; then
newlisti=${newlist}"AVNO JTWC"
newlistt=${newlist}"AVNO JTWC"
elif [ $BASIN = "AL" ]; then
#newlisti=${newlist}"AVNO HWRF GFDL  EMX  UKM  CMC OFCL"
#newlistt=${newlist}"AVNO HWRF GFDL  EMX  UKM  CMC OFCL"
newlisti=${newlist}"AVNO HWRF GFDL  EMX  UKM OFCL"
newlistt=${newlist}"AVNO HWRF GFDL  EMX  UKM OFCL"
#newlisti=${newlist}"AVNO OFCL"
#newlistt=${newlist}"AVNO OFCL"
elif [ $BASIN = "EP" ]; then
newlisti=${newlist}"AVNO HWRF GFDL OFCL"
newlistt=${newlist}"AVNO HWRF GFDL OFCL"
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
 ${scrdir}/sorc/top_tvercut.sh ${execdir}/tracks_${bas}.t.txt $scrdir/sorc
 ${scrdir}/sorc/top_ivercut.sh ${execdir}/tracks_${bas}.i.txt $scrdir/sorc


## copy grads scripts and make plots                        
if [ $BASIN = "AL" ]; then place="Atlantic"; fi
if [ $BASIN = "EP" ]; then place="East-Pacific"; fi
if [ $BASIN = "WP" ]; then place="West-Pacific"; fi
period="${DATEST}__${DATEND}__${ncyc}cyc"

cp ${scrdir}/sorc/*gs .
 grads -bcp "run top_iver.gs tracks_${bas}.i  $yearlabel $place $period"
 grads -bcp "run top_tver_250.gs tracks_${bas}.t  $yearlabel $place $period"

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
  put tracks_al.t.txt 
  put tracks_ep.t.txt 
  put tracks_wp.t.txt 
  put tracks_al.i.txt 
  put tracks_ep.i.txt 
  put tracks_wp.i.txt 
  cd $ftpdir
  mkdir www
  cd www
  mkdir track
  cd track
  put ${scrdir}/html/track_body.html track_body.html
  put ${scrdir}/html/track.html      track.html
  put ${scrdir}/html/track_top${years}.html track_top.html
  quit
EOF
 sftp  ${webhostid}@${webhost} <ftpin
fi

exit





## save tracks 
savedir=${scrdir}/arch_trak/${mdname[1]}$yearlabel
mkdir -p $savedir
cp ${execdir}/*.gif ${savedir}/.
cp ${execdir}/a*dat ${savedir}/.
cp ${execdir}/b*dat ${savedir}/.
cp ${execdir}/atcfunix* ${savedir}/.
cp ${execdir}/tracks_al.t.txt ${savedir}/tracks_al.t.txt
cp ${execdir}/tracks_ep.t.txt ${savedir}/tracks_ep.t.txt
cp ${execdir}/tracks_wp.i.txt ${savedir}/tracks_wp.t.txt
cp ${execdir}/tracks_al.i.txt ${savedir}/tracks_al.i.txt
cp ${execdir}/tracks_ep.i.txt ${savedir}/tracks_ep.i.txt
cp ${execdir}/tracks_wp.i.txt ${savedir}/tracks_wp.i.txt

exit
