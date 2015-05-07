#!/bin/ksh
set -x

##---------------------------------------------------------------------
## Fanglin Yang, Oct 2010
##   Read in precip FHO data saved in VSDB format, compute precip threat 
##   skill scores, and make maps with Monte Carlo significane tests
##---------------------------------------------------------------------

export stymd=20091001           ;#verification starting date 
export edymd=20100930           ;#verification ending date
export expids="gfs nam cmcglb cmc "   ;#models to be included, up to 10 


export scrdir=/global/save/wx24fy/VRFY/vsdb/precip   ;#source code and executable etc, do not change
export webhost="emcrzdm.ncep.noaa.gov"                       ;#web display server                          
export webhostid="$LOGNAME"                               ;#user name on web server                                  
export doftp="YES"                                        ;#send maps to web server?     
export ftpdir=/home/people/emc/www/htdocs/gmb/$LOGNAME/misc/etsbis
export timeplot="NO"                                     ;#plot time series for each intensity?

export rundir=/ptmp/$LOGNAME/prvsdb                       ;#temporaray running directory
export datdir=$rundir/fhodata                             ;#temporary place to collect precip vsdb data
export mapdir=$rundir/prmap                               ;#place to save plots at local machine
mkdir -p $rundir $datdir

export NWPROD=${NWPROD:-/nwprod}
export ndate=${ndate:-$NWPROD/util/exec/ndate}
export GRADSBIN=${GRADSBIN:-/usrx/local/GrADS/2.0.2/bin}


archcomp=tempest                                          ;#computer where VSDB data are saved
archdir=/mmb/wd22yl/vsdb                                  ;#precip vsdb archive directory
host=`echo $(hostname) |cut -c 1-1 `
client=`echo $archcomp |cut -c 1-1 `
if [ $host = $client ]; then
 for mdl in  $expids ; do
  ln -fs ${archdir}/$mdl $datdir/.
 done
else
 for mdl in  $expids ; do
  mkdir -p ${datdir}/${mdl}  || exit 8
  ym=`echo $stymd |cut -c 1-6 `
  ym_end=`echo $edymd |cut -c 1-6 `
  while [ $ym -le $ym_end ]; do
   scp -pr ${LOGNAME}@${archcomp}:${archdir}/${mdl}/*${ym}*  ${datdir}/${mdl}/.
   ym=`$ndate 744 $ym\0100 | cut -c1-6`
  done
 done
fi


#=============================================
for region in G211/RFC; do
for fhour in  24 48 "24 48" ; do

 ${scrdir}/precip_score_vsdb.sh $stymd $edymd $region "$fhour" "$expids"

done
done
#=============================================


exit


