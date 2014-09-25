#!/bin/sh

 set -x

  SETOPT=-e
# SETOPT=-eax
# SETOPT=-evx
# SETOPT=-vx


  set $SETOPT

if [ -d /global ]; then
  TOPDIR=/global   # This would be the WCOSS
  MACHINE=WCOSS
elif [ -d /scratch1/portfolios/NCEPDEV/da ]; then
  TOPDIR=/scratch1/portfolios/NCEPDEV/da     #This is zeus
  MACHINE=ZEUS
else
  echo CANNOT FIND A VALID TOP-LEVEL DIRECTORY
  exit 1
fi

# Other Executables and scripts
if [ $MACHINE = WCOSS ]; then
   export NDATE=/nwprod/util/exec/ndate
elif [ $MACHINE = ZEUS ]; then
   export NDATE=/scratch1/portfolios/NCEPDEV/da/save/Michael.Lueken/nwprod/util/exec/ndate
else
  echo "Unsupported machine $MACHINE (not sure how you got to here)"
  exit 1
fi


#--- Set directories
#     HOME     = directory containing diag2grads
#     DIAG_DIR = input directory containing diagnostic file(s)
#     GRADSDIR = output directory for GrADS station files

HOME=$TOPDIR/save/$USER/svn/EXP-port-r20613/util/Radiance_Utilities/diag2grads
DIAG_DIR=/scratch2/portfolios/NCEPDEV/ptmp/$USER/tmp574_sigmap/globalprod.2012012212.zeus
GRADSDIR=/scratch2/portfolios/NCEPDEV/ptmp/$USER/map


#=== parameters ======================================================

#--- time

  TIME_TOP=2012012212
  TIME_INT=06
  TIME_NUM=1

#--- area

  NORTH=90.
  SOUTH=-90.
  WEST=0.
  EAST=360.

#--- flags

  QC=-2		# all w/o any qc
# QC=-1		# not used in analysis
# QC=0		# all w/ Tb gross error check
# QC=1		# used in analysis

  SOLAR=0	# all
# SOLAR=1	# night
# SOLAR=2	# day
 
  ITERATION=1

#--- case

#CASE=pro	; COMMENT='PRODUCTION'	; DIAG_DIR=###          
CASE=new	; COMMENT='TEST RUN'



#--- innovation type

  OMF=ges   # read observed - guess files
## OMF=02    # read observed - guess files
## OMF=anl   # read observed - analysis files


#--- clean up working directory after the processing

  CLEAN=YES
# CLEAN=NO

#--- totalview

  TOTALVIEW=
# TOTALVIEW=totalview

#=====================================================================



#--- dirs and files
WORK=${GRADSDIR}/WORK

EXEFILE=$HOME/diag2grads

#--- creat working directory and clean up

mkdir -p $WORK
cd $WORK

rm -f fort.*
rm -f satlist.txt
rm -f namelist.txt

#--- satellite and sensor

cat << EOF > satlist.txt
### Available satellites and sensors ###
[SAT]  [SENSOR] [# CHAN] [prefix] [presat]
NOAA14  HIRS       19      hirs2     n14
NOAA14  MSU         4      msu       n14
NOAA14  SSU         3      ssu       n14
NOAA15  HIRS       19      hirs3     n15
NOAA15  AMSUA      15      amsua     n15
NOAA15  AMSUB       5      amsub     n15
NOAA15  AVHRR       3      avhrr     n15
NOAA16  HIRS       19      hirs3     n16
NOAA16  AMSUA      15      amsua     n16
NOAA16  AMSUB       5      amsub     n16
NOAA16  AVHRR       3      avhrr     n16
NOAA17  HIRS       19      hirs3     n17
NOAA17  AMSUA      15      amsua     n17
NOAA17  AMSUB       5      amsub     n17
NOAA17  AVHRR       3      avhrr     n17
NOAA18  HIRS       19      hirs4     n18
NOAA18  AMSUA      15      amsua     n18
NOAA18  MHS         5      mhs       n18
NOAA18  AVHRR       3      avhrr     n18
NOAA19  HIRS       19      hirs4     n19
NOAA19  AMSUA      15      amsua     n19
NOAA19  MHS         5      mhs       n19
NOAA19  AVHRR       3      avhrr     n19
METOPA  HIRS       19      hirs4     metop-a
METOPA  AMSUA      15      amsua     metop-a
METOPA  MHS         5      mhs       metop-a
METOPA  AVHRR       3      avhrr     metop-a
METOPA  IASI      616      iasi      metop-a
GOES08  SNDR       18      gsndr     g08
GOES08  IMGR        4      imgr      g08
GOES10  SNDR       18      gsndr     g10
GOES10  IMGR        4      imgr      g10
GOES11  SNDR       18      gsndr     g11
GOES11  IMGR        4      imgr      g11
GOES12  SNDR       18      gsndr     g12
GOES12  IMGR        4      imgr      g12
GOES13  SNDR       18      gsndr     g13
GOES13  IMGR        4      imgr      g13
GOES14  SNDR       18      gsndr     g14
GOES14  IMGR        4      imgr      g14
GOES15  SNDR       18      gsndr     g15
GOES15  IMGR        4      imgr      g15
GOES10  SNDRD1     18      sndrd1    g10
GOES10  SNDRD2     18      sndrd2    g10
GOES10  SNDRD3     18      sndrd3    g10
GOES10  SNDRD4     18      sndrd4    g10
GOES11  SNDRD1     18      sndrd1    g11
GOES11  SNDRD2     18      sndrd2    g11
GOES11  SNDRD3     18      sndrd3    g11
GOES11  SNDRD4     18      sndrd4    g11
GOES12  SNDRD1     18      sndrd1    g12
GOES12  SNDRD2     18      sndrd2    g12
GOES12  SNDRD3     18      sndrd3    g12
GOES12  SNDRD4     18      sndrd4    g12
GOES13  SNDRD1     18      sndrd1    g13
GOES13  SNDRD2     18      sndrd2    g13
GOES13  SNDRD3     18      sndrd3    g13
GOES13  SNDRD4     18      sndrd4    g13
GOES14  SNDRD1     18      sndrd1    g14
GOES14  SNDRD2     18      sndrd2    g14
GOES14  SNDRD3     18      sndrd3    g14
GOES14  SNDRD4     18      sndrd4    g14
GOES15  SNDRD1     18      sndrd1    g15
GOES15  SNDRD2     18      sndrd2    g15
GOES15  SNDRD3     18      sndrd3    g15
GOES15  SNDRD4     18      sndrd4    g15
AQUA    AIRS      281      airs      aqua
AQUA    AMSUA      15      amsua     aqua
AQUA    HSB         4      hsb       aqua
AQUA    AMSRE_LOW  12      amsre_low aqua
AQUA    AMSRE_MID  12      amsre_mid aqua
AQUA    AMSRE_HIG  12      amsre_hig aqua
DMSP13  SSMI        7      ssmi      f13
DMSP14  SSMI        7      ssmi      f14
DMSP15  SSMI        7      ssmi      f15
DMSP16  SSMIS_ENV  24      ssmis_env f16
DMSP16  SSMIS_IMG  24      ssmis_img f16
DMSP16  SSMIS_LAS  24      ssmis_las f16
DMSP16  SSMIS_UAS  24      ssmis_uas f16
DMSP17  SSMIS_ENV  24      ssmis_env f17
DMSP17  SSMIS_IMG  24      ssmis_img f17
DMSP17  SSMIS_LAS  24      ssmis_las f17
DMSP17  SSMIS_UAS  24      ssmis_uas f17
DMSP18  SSMIS_ENV  24      ssmis_env f18
DMSP18  SSMIS_IMG  24      ssmis_img f18
DMSP18  SSMIS_LAS  24      ssmis_las f18
DMSP18  SSMIS_UAS  24      ssmis_uas f18
DMSP19  SSMIS_ENV  24      ssmis_env f19
DMSP19  SSMIS_IMG  24      ssmis_img f19
DMSP19  SSMIS_LAS  24      ssmis_las f19
DMSP19  SSMIS_UAS  24      ssmis_uas f19
DMSP20  SSMIS_ENV  24      ssmis_env f20
DMSP20  SSMIS_IMG  24      ssmis_img f20
DMSP20  SSMIS_LAS  24      ssmis_las f20
DMSP20  SSMIS_UAS  24      ssmis_uas f20
EOF

if [ ! "$1" ]; then
  echo 'Usage > diag2grads.sh [ID]'
  cat satlist.txt
  exit
fi


# awk '$4 == '"$1"' { print $0 }' satlist.txt | tr -s ' ' ' ' > satsel1.txt
# awk '$5 == '"$2"' { print $0 }' satsel1.txt | tr -s ' ' ' ' > satsel.txt
grep " $1" satlist.txt | tr -s ' ' ' ' > satsel1.txt
grep " $2" satsel1.txt | tr -s ' ' ' ' > satsel.txt

SATNAME=`cut -d' ' -f1 satsel.txt`
SENNAME=`cut -d' ' -f2 satsel.txt`
senname=`cut -d' ' -f4 satsel.txt`
satname=`cut -d' ' -f5 satsel.txt`
SENID=$1
NCHAN=`cut -d' ' -f3 satsel.txt`

if [ ! "$SATNAME" ]; then
  echo "ERROR: Specified ID isn't found in the list"
  cat satlist.txt
  exit
fi

SATSEN=${SATNAME}${SENNAME}


#--- print

echo
echo '=== Create a GrADS station data file from SSI diagnostic files ==='
echo "--> $CASE" 1>&2
echo "--> SAT/SEN : ${SATNAME}/${SENNAME}"

#--- title, directories for diagnostic files

TITLE="${SATNAME}/${SENNAME} (${COMMENT})"
ZIPDIR=$DIAG_DIR                              # directory of diagnostic files
DATDIR=$WORK                           # directory of unzipped diagnostic files

#--- diagnostic files

#-- org or new or 'prx'

#--- set zip suffix
ZIP_SUFFIX=Z

if [ $MACHINE = WCOSS ]; then
  ZIP_SUFFIX=gz
elif [ $MACHINE = ZEUS ]; then
  ZIP_SUFFIX=gz
fi

if [ $CASE = 'org' -o $CASE = 'new' -o $CASE = 'prx' ]; then
  set_DATFIL() {
    set $SETOPT
    ZIPFIL=${ZIPDIR}/diag_${senname}_${satname}_${OMF}.${TIME}.${ZIP_SUFFIX}
    UNZIPFIL=${ZIPDIR}/diag_${senname}_${satname}_${OMF}.${TIME}
    DATFIL=${DATDIR}/diag_${senname}_${satname}_${OMF}.${TIME}
  }
fi

echo " "
echo "look at variables below"
echo $ZIPFIL
echo $UNZIPFIL
echo $DATFIL
echo " "

#-- pro ### under construction ###

if [ $CASE = 'pro' ]; then
  set_DATFIL() {
    set $SETOPT
    ZIPFIL=${ZIPDIR}/fnl.${DATE}/gdas1.t${ZTIME}z.radstat.${ZIP_SUFFIX}
    DATFIL=${DATDIR}/radstat.$TIME
  }
fi

#--- diagnostic files

FT_TOP=30
FT_END=`expr $FT_TOP + $TIME_NUM - 1`

FT=$FT_TOP
TIME=$TIME_TOP

while test $FT -le $FT_END
do
  DATE=`echo $TIME |cut -c1-8`
  ZTIME=`echo $TIME |cut -c9-10`
  set_DATFIL
  echo ${UNZIPFIL}
  ls -lt ${ZIPFIL}
  if [ ! -e $ZIPFIL ]; then
    echo ${ZIPFIL}
    ls -lt ${ZIPFIL}
    cp ${UNZIPFIL} ${DATFIL}
  fi
  echo ${UNZIPFIL}
  if [ ! -e $DATFIL ]; then
    echo "--> Unzip" ${ZIPFIL} 1>&2
    if [ ! -e ${DATFIL}.${ZIP_SUFFIX} ]; then
      cp ${ZIPFIL} ${DATFIL}.${ZIP_SUFFIX}
    fi
    gzip -d ${DATFIL}.${ZIP_SUFFIX}
  fi
  echo ${UNZIPFIL}
  ln -s ${DATFIL}  fort.$FT
  TIME_END=$TIME
  FT=`expr $FT + 1`
  TIME=`$NDATE $TIME_INT $TIME`
done

ls -l fort.* 1>&2

echo "--> TIME :" $TIME_TOP $TIME_INT $TIME_NUM $TIME_END  1>&2

#--- retrieve ymdhm

YEAR=`echo $TIME_TOP |cut -c1-4`
MON=`echo $TIME_TOP |cut -c5-6`
DAY=`echo $TIME_TOP |cut -c7-8`
HOUR=`echo $TIME_TOP |cut -c9-10`
MIN=00

#--- Create namelist input on the file.

cat << EOF > namelist.txt
 &PARM 
 comment = "${TITLE}",
 ft_diag_top = $FT_TOP, ft_diag_end = $FT_END,
 datetime = $YEAR, $MON, $DAY, $HOUR, $MIN,
 tint     = $TIME_INT,
 filename = "${GRADSDIR}/${SATSEN}",
 idsat    = $ITERATION,
 n_chan   = $NCHAN,
 North = $NORTH , South = $SOUTH , West = $WEST , East = $EAST ,
 qc      = $QC,
 solar   = $SOLAR
 /
EOF

ln -s namelist.txt  fort.1

#--- run

$TOTALVIEW  $EXEFILE

#--- create a station map file

stnmap -i ${GRADSDIR}/${SATSEN}.ctl > out.stnmap

NDATA=`head -n 9 out.stnmap | head -n 9 | tail -n 1 | tr -s " " " " | cut -f9 -d" "`

#--- results

echo
echo '# of DATA   :' $NDATA
echo 'GrADS FILES :'
ls -l ${GRADSDIR}/${SATSEN}.*

#--- clean up

if [ "$CLEAN" = "YES" ]; then
  cd
  rm -rf $WORK
fi
