#
# Script history log:
# 1999-05-01  Mark Iredell
# 2000-02-14  S    Moorthi
# 2001-12-14  J    Alpert  (*j*)
# 2004-05-12  J    Alpert  (*j*) fix for E-W gaussian grid pt shift
# 2004-12-06  J    Alpert  (*j*)  script input settings for spect filter
# 2005-03-21  J    Alpert  (*j*)  Added GrumbineICE to orog/slm...
#
# W/Lott & Miller terrain principal coord. (*j*)
#
# Usage:  ml3b.sh slmgb orogb mtnvar14 nlon nlat jcap filter1 filter2 mtnres
#
# Normally: filter1~1/3 ((jcap/3)-1))
# Normally: filter2~jcap+2))
# Normally: mtnres=8 minute only (do not use =4, =2 except at own risk)
#            (see terr05/ml382 for example - run from ptmp) 
#   script changed like ml4b for spect filter input, otherwise same as ml2b
#
#   Input script fortran  positional parameters:
#     1             output sea-land mask GRIB file
#     2             output orography GRIB file
#     3             output 14-field mountain variance file
#     4             number of Gaussian longitudes
#     5             number of Gaussian latitudes
#     6             spectral triangular truncation
#     7             Envelope orography factor
#     8             Begining latitude (used only for nongaussian grid -
#                                      used only for switching north/south)
#     9             Mountain data resolution
#
#   Imported Shell Variables:
#     DATA          working directory
#                   defaults to a directory that is made, used and deleted
#     FIXDIR        fix directory
#                   defaults to /gloptmp/fix
#     TERRAINSORC   terrain source file
#                   defaults to /nfsuser/g01/wx23ja/terr04
#                   now this defaults to the local dir
#     LONSPERLAT    input lonsperlat text file (if it exists)
#                   defaults to $FIXDIR/global_lonsperlat.t$6.txt
#     VERBOSE       verbose flag (YES or NO)
#                   defaults to NO
#
#   Modules and files referenced:
#     scripts    : /u/wx20mi/bin/mkdata
#
#     source     : ${TERRAINSORC} or /global/save/wx23ja/terr06/ml382
#                       now defaults to the local dir
#
#     input data : /gloptmp/constant/TOP8M_avg.20I4.asc
#                  /gloptmp/constant/TOP8M_max.20I4.asc
#                  /gloptmp/constant/TOP8M_slm.80I1.asc
#                  /gloptmp/constant/TOP8M_var.20I4.asc
#                  /gloptmp/fix/global_lonsperlat.t$6.txt
#
#     output data: $1
#                  $2
#                  $3
#
#     scratch    : ${DATA}/terrain00.xd
#                  ${DATA}/fort.11
#                  ${DATA}/fort.12
#                  ${DATA}/fort.13
#                  ${DATA}/fort.14
#                  ${DATA}/fort.20
#                  ${DATA}/fort.51
#                  ${DATA}/fort.52
#                  ${DATA}/fort.53
#                  ${DATA}/fort.54
#                  ${DATA}/fort.55
#                  ${DATA}/fort.56
#                  ${DATA}/fort.57
#                  ${DATA}/fort.71
#
# Remarks:
#
#   Condition codes
#      0 - no problem encountered
#     >0 - some problem encountered
#
# Attributes:
#   Language: POSIX shell
#   Machine: IBM SP
#
####

################################################################################
# Check arguments
if [[ $# -ne 9 ]];then
 echo Usage: $0 slmgb orogb mtnvar14 IM JM NM filter1 filter2 MTNRES >&2
 exit 1
fi
#
# VERBOSE = YES means debug mode
#
# export VERBOSE=${VERBOSE:-"NO"}
export VERBOSE=${VERBOSE:-"YES"}
if [[ "$VERBOSE" = "YES" ]];then
 echo $(date) EXECUTING $0 $* >&2
 set -x
fi
pwd=$(pwd)
echo $pwd
export DISK_GLOB=${DISK_GLOB:-/global/save}
#typeset -L1 l1
#
slmgb=${1:-$pwd/slmgb}
orogb=${2:-$pwd/orogb}
mtnvar14=${3:-$pwd/mtnvar14}
echo $slmgb
echo $orogb
echo $mtnvar14
nlon=$4
nlat=$5
jcap=$6
#
#
if [[ jcap -le 0 ]] ; then
 export resl=LL${nlon}${nlat}
else
 export resl=T$jcap
fi
NR=0
#
#
#### efac=$7
#### blat=$8
efac=${efac:-0}
blat=${blat:-0}
#### mtnres=$9
export mtnres=${9:-"8"}
#### export NF1=${NF1:-$(($jcap+1))}
#### export NF2=${NF2:-$(($jcap+2))}
export NF1=${7:-$(($jcap+1))}
export NF2=${8:-$(($jcap+2))}
NR=0
 echo "Usage: $0 $slmgb $orogb $mtnvar14 $nlon $nlat $jcap $NF1 $NF2  $MTNRES "
echo "   efac=$efac  blat=$blat  NF1=$NF1   NF2=$NF2 "
echo " _______________________________________________  "

#
#  file names for Prin Coord dataset grib output
#
thetagb=${thetagb:-$pwd/thetagb$resl}
gammagb=${gammagb:-$pwd/gammagb$resl}
sigmagb=${sigmagb:-$pwd/sigmagb$resl}
vargb=${vargb:-$pwd/vargb$resl}
elvmaxgb=${elvmaxgb:-$pwd/elvmaxgb$resl}
#
NR=0
#
#
export DATA=${DATA}
export FIXDIR=${FIXDIR:-/nwprod/fix}
export LONSPERLAT=${LONSPERLAT:-${FIXDIR}/global_lonsperlat.t$jcap.txt}
## export TERRAINSORC=${TERRAINSORC:-/nfsuser/g01/wx23ja/terr04/fix/ml01rg2.f}
export ORODIR=${ORODIR:-$DISK_GLOB/wx23hh/topo/global}
export TERRAINSORC=${TERRAINSORC:-$DISK_GLOB/wx23sm/terrain_etc/terr06/t2000fix/ml01rg2.f}
# SORC location is general for all model truncations.
################################################################################
# Make working directory
. /u/wx20mi/bin/mkdata
# The name created is "/stmp/$LOGNAME/data$$"
cd $DATA

### MTNDATA=TOP${mtnres}M
### MTNDIR=/gloptmp/constant
### MTNDIR=/nfsuser/g01/wx23ja/terrain
MTNDATA=TOP${mtnres}M
MTNDIR=$DISK_GLOB/wx23ja/terrain
MTN_AVG=${MTNDATA}_avg.20I4.asc
MTN_VAR=${MTNDATA}_var.20I4.asc
MTN_MAX=${MTNDATA}_max.20I4.asc
MTN_SLM=${MTNDATA}_slm.80I1.asc
#
ln -fs        $MTNDIR/$MTN_AVG       fort.11
ln -fs        $MTNDIR/$MTN_VAR       fort.12
ln -fs        $MTNDIR/$MTN_MAX       fort.13
ln -fs        $MTNDIR/$MTN_SLM       fort.14
#ln -fs /nfsuser/g01/wx23ja/terr06/eight.minute.antarctic.new.bin    fort.15
ln -fs $DISK_GLOB/wx23ja/terr06/t2000fix/eight.minute.antarctic.new.bin    fort.15
ln -fs $DISK_GLOB/wx23sm/terrain_etc/terr06/t2000fix/eight.minute.antarctic.new.bin    fort.15
ln -fs $LONSPERLAT           fort.20
ln -fs SLM.$resl             fort.51
ln -fs ORO.$resl             fort.52
ln -sf $mtnvar14             fort.53
ln -fs ORS.$resl             fort.54
ln -fs ORU.$resl             fort.55
ln -sf $slmgb                fort.56
ln -sf $orogb                fort.57
ln -sf $thetagb              fort.58
ln -sf $gammagb              fort.59
ln -sf $sigmagb              fort.60
ln -sf $vargb                fort.61
ln -sf $elvmaxgb             fort.62
ln -sf THETA.$resl           fort.66
ln -sf GAMMA.$resl           fort.67
ln -sf SIGMA.$resl           fort.68
ln -sf ELVMAX.$resl          fort.69
ln -sf mtn.$resl.ieee        fort.71

CF=xlf_r
FFOPTS="-q64 -qnosave -qsmp=noauto -O -qmaxmem=-1 -qrealsize=4"
LDIR=/nwprod/lib
##LIBS="-L/$LDIR -lw3_4 -lbacio_4 -lsp_4  -lessl -lip_4"
  LIBS="-L/$LDIR -lw3_4 -lbacio_4         -lessl -lip_4"
f=$TERRAINSORC
x=/stmp/$LOGNAME/ml01rg2.x
echo "$CF $FFOPTS $f $LIBS $LDOPTS -o $x"
  $CF $FFOPTS $f $SHDIR/sp/*.o $LIBS $LDOPTS -o $x||exit 1
##$CF $FFOPTS $f $LIBS $LDOPTS -o $x||exit 1
echo " mtnres nlon nlat jcap NR NF1 NF2 efac blat "
echo $mtnres $nlon $nlat $jcap $NR $NF1 $NF2 $efac $blat
echo " exec located:  $x "
echo " EXECUTION BEGINS "
echo $mtnres $nlon $nlat $jcap $NR $NF1 $NF2 $efac $blat >inp
$x <inp >out
ret=$?
if [[ "$VERBOSE" = "YES" ]];then 
echo ret=$ret
fi
# copy files from working dir to present
echo  " cp $DATA/m* ${pwd}/. "
cp $DATA/m* ${pwd}/.
# this will get the mtnvar_14 and mtn.ieee file for grads use
# the ...gb files are present directly - from starting local dir.
# the other files working files are left to be copied by the user.
# Remove working directory
if [[ "$VERBOSE" = "YES" ]];then
     pwd
     ls -l
     echo " ml3b.sh: setting MKDATA = NO  " 
     echo " - not deleting working dir $DATA "
     MKDATA=NO
fi
# 
#rm -f $MTN_AVG
#rm -f $MTN_VAR
#rm -f $MTN_MAX
#rm -f $MTN_SLM

cd $pwd
[[ $MKDATA = YES ]] && rm -rf $DATA
set +x
if [[ "$VERBOSE" = "YES" ]];then
 echo " $(date) EXITING $0 with return code $ret >&2 "
fi
exit $ret
