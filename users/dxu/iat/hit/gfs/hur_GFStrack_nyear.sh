#!/bin/ksh
set -x

# get operational GFS (AVNO) tracks for multiple years
 
## Hurricance track plots, Automated by Fanglin Yang (March 4, 2008)
## Please first have cardyyyy* ready in scrdir/sorc if they do not exist 
## Check /com/arch/prod/syndat/syndat_tcvitals.year and 
##       http://www.nhc.noaa.gov/tracks/yyyyatl.gif for named hurricances
## track source: /tpc/noscrub/data/atcf-noaa/archive (have mismatches with real-time track)
## new track source: /global/shared/stat/tracks (consistent with real-time track, made by Vjay Tallapragada)

#for yyyy in 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011; do 
for yyyy in 2013; do 

cdates=${yyyy}0501
#cdatee=${yyyy}1231
cdatee=${yyyy}1130

export scrdir=/global/save/$LOGNAME/VRFY/hurtrack               
export expdir=/global/hires/glopara/archive       ;#experiment data archive directory
export mdlist="gfs"            ;#experiment names
export DATEST=$cdates                              ;#forecast starting date
export DATEND=$cdatee                              ;#forecast ending date
export OCEAN="AL EP"                             ;#basin, AL-Atlantic, EP-Eastern Pacific, WP-Western Pacific
export cyc="00 06 12 18"                              ;#forecast cycles to be included in verification        
export ftpdir=/home/people/emc/www/htdocs/gmb/$LOGNAME/vsdb_glopara/pre13j   ;#where maps are displayed on rzdm.ncep.noaa.gov
export doftp="NO"                                   ;#whether or not sent maps to ftpdir
export rundir=/stmp/$LOGNAME/track
mkdir -p ${rundir}; cd $rundir


#---------------------------------------------------------
#---------------------------------------------------------
#---Most likely you do not need to change anything below--
#---------------------------------------------------------
#---------------------------------------------------------
set -A mdname $mdlist
execdir=${rundir}/$yyyy                     ;# working directory
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
#tpctrack=${execdir}/tpctrack           ;#place to hold HPC original track data 
tpctrack=${execdir}           ;#place to hold HPC original track data 
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

ncyc=`echo $cyc |wc -w |sed 's/ //g'`
if [ $ncyc -eq 3 ]; then ncyc=2; fi

#------------------------------------------------------------------------
#  prepare data for GrADS graphics
#------------------------------------------------------------------------
for BASIN in $OCEAN; do
bas=`echo $BASIN |tr "[A-Z]" "[a-z]" `

## copy test cards, replace dummy exp name MOD# with real exp name
cp ${scrdir}/sorc/card${yearlabel}_${bas}.i .     
cp ${scrdir}/sorc/card${yearlabel}_${bas}.t .     

if [ $BASIN = "WP" ]; then
#newlisti=${newlist}"AVNO JTWC"
#newlistt=${newlist}"AVNO JTWC"
newlisti="AVNO JTWC"
newlistt="AVNO JTWC"
else
#newlistt=${newlist}"AVNO GFDL OFCL CLP5"
#newlistt=${newlist}"AVNO GFDL HWRF OFCL CLP5"
newlisti="AVNO OFCL"
newlistt="AVNO OFCL"
fi

nint=`echo $newlisti |wc -w`     ;#number of process for intensity plot, to replace NUMINT in card.i
ntrc=`echo $newlistt |wc -w`     ;#number of process for track plot, to replace NUMTRC in card.t
nint=`expr $nint + 0 `           ;#remove extra space
ntrc=`expr $ntrc + 0 `
sed -e "s/MODLIST/${newlisti}/g" -e "s/NUMINT/ ${nint}/g" card${yearlabel}_$bas.i >card_$bas.i   
sed -e "s/MODLIST/${newlistt}/g" -e "s/NUMTRC/ ${ntrc}/g" card${yearlabel}_$bas.t >card_$bas.t   


## produce tracks.t.out etc
#cp $tpctrack/b*dat   .
${scrdir}/sorc/nhcver.x card_${bas}.t tracks_${bas}.t  $execdir
${scrdir}/sorc/nhcver.x card_${bas}.i tracks_${bas}.i  $execdir


## create grads files tracks_${bas}.t.dat etc for plotting
 ${scrdir}/sorc/top_tvercut.sh ${execdir}/tracks_${bas}.t.out $scrdir/sorc
 ${scrdir}/sorc/top_ivercut.sh ${execdir}/tracks_${bas}.i.out $scrdir/sorc


## copy grads scripts and make plots                        
if [ $BASIN = "AL" ]; then place="Atlantic"; fi
if [ $BASIN = "EP" ]; then place="East-Pacific"; fi
if [ $BASIN = "WP" ]; then place="West-Pacific"; fi
period="${DATEST}__${DATEND}__${ncyc}cyc"

cp ${scrdir}/sorc/*gs .
 grads -bcp "run top_iver.gs tracks_${bas}.i  $yearlabel $place $period"
 grads -bcp "run top_tver_250.gs tracks_${bas}.t  $yearlabel $place $period"


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
 ftp -i -v rzdm.ncep.noaa.gov <ftpin 
fi

## save tracks 
savedir=${scrdir}/arch_trak/gfs$yyyy
mkdir -p $savedir
cp ${execdir}/*.gif ${savedir}/.
#cp ${execdir}/a*dat ${savedir}/.
#cp ${execdir}/b*dat ${savedir}/.
#cp ${execdir}/atcfunix* ${savedir}/.
cp ${execdir}/tracks_al.t.out ${savedir}/tracks_al.t.txt
cp ${execdir}/tracks_ep.t.out ${savedir}/tracks_ep.t.txt
cp ${execdir}/tracks_wp.i.out ${savedir}/tracks_wp.t.txt
cp ${execdir}/tracks_al.i.out ${savedir}/tracks_al.i.txt
cp ${execdir}/tracks_ep.i.out ${savedir}/tracks_ep.i.txt
cp ${execdir}/tracks_wp.i.out ${savedir}/tracks_wp.i.txt

#-------finish yyyy---------
done
#-------finish yyyy---------
exit
