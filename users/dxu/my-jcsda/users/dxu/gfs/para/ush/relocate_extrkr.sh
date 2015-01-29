
# This script is executed by the script relocate_relocate_ts.sh
# -------------------------------------------------------------

set -aeux

user=$LOGNAME
#	CDATE10 - 10 digit date
#	RUN     - network run (gfs, gdas, etc)
#	DATA    - working directory
#	SUPVX - tcvitals update program
#	GETTX - Tim's get track program

TIMEIT=""
[ -s $DATA/timex ] && TIMEIT=$DATA/timex

vdir=${vdir:-$DATA/trakout}
if [ ! -d ${vdir} ]; then
  mkdir -p ${vdir}
fi

cd $vdir

# This script kicks off the hurricane tracker system.  You have the option of 
# running the tracker on several operational models (the system will 
# automatically search in the appropriate operational directories for the
# data), or on your own model data.  In either case, the current system 
# will ONLY be able to read GRIB data.  To run the tracker, fill in the 
# required fields below on the "export" lines, then llsubmit this script.


#     -------------------
#     1. ENTER MODEL TYPE
#     -------------------
#
#     Enter the name of the model that you're running the tracker on.  The 
#     tracker is already hard-wired to read the operational GFS, MRF, UKMET, 
#     ECMWF, NGM, Early NAM and NOGAPS files in their operational directories 
#     on /com.  If your model is one of these, *AND* the data files are still 
#     on /com (data on the SP sits on /com for ~10 days), enter one of the 
#     following below:  GFS, MRF, UKMET, ECMWF, NGM, NAM, NGPS.  Otherwise, 
#     enter OTHER below:
#
#     Example:  export CMODEL=gfs

export CMODEL=${CMODEL:-$RUN}


#     -------------------
#     2. ENTER FILE NAME
#     -------------------
#
#     If you entered an operational model above, then skip this step, as the 
#     script already knows the filename and directory location .  Otherwise, 
#     you must enter the full name (include directory) of one of your GRIB 
#     forecast files.  Also, your 00h forecast file must have the characters 
#     "00" in the name, not just "anl".  
#
#     Finally, and this is important, in the export statement below, in the
#     character positions where the forecast hour characters "00" would appear
#     replace those characters with the characters "XX".  This allows the 
#     tracker script to manipulate the file names, and to avoid confusion of 
#     the forecast hour "00" with the initial hour "00" that may be in a name.
#
#     Example: If the actual name of your datafile is
#              /ptmp/wx20tm/trakdat/ngm.1997110700.pgrb.f00, then enter below:
#
#              export INPFILE=/ptmp/wx20tm/trakdat/ngm.1997110700.pgrb.fXX
 
export INPFILE=${INPFILE:-}
 

#     -------------------------------
#     3. ENTER FORECAST HOUR INTERVAL
#     -------------------------------
#
#     If above you entered an operational model from an operational directory,
#     then skip this step, as the script already knows the forecast hour 
#     interval.  Otherwise, enter the integer number of hours between 
#     successive forecasts.  By the way, there are a couple of caveats:
#      a) Your forecast hours must be evenly spaced (i.e., if you start out 
#         with 00, 06, 12,... etc, and then after 48 you skip to 60, the 
#         program will stop after 48 hours).
#      b) Currently, a maximum of 14 forecast times can be handled (this is
#         enough to get you 6h forecast intervals from 00 to 78h, or 12h
#         forecast intervals from 00 to 156h).

export FHINT=${FHINT:-03}


#     ------------------------------
#     4. ENTER FORECAST INITIAL TIME
#     ------------------------------
#
#     Enter the starting date, in yyyymmddhh format, of the forecast:

export YYYYMMDDHH=$GDATE10
#export YYYYMMDDHH=$CDATE10


#     -------------------------
#     5. ENTER STORM IDENTIFIER
#     -------------------------
#
#     Enter the 3-character identifier for the storm that you want to track
#     (e.g., 06L, 04E, etc...).  If you want all the storms from the input
#     forecast time, then just leave it blank, as "export INPSTORM=".

export INPSTORM=


#     -----------------------------------------
#     6. ENTER NAME OF AUXILIARY TC VITALS FILE
#     -----------------------------------------
#
#     If you are going to use this script to track a storm that has been 
#     tracked by an operational hurricane center (NHC, JTWC, etc.), then
#     skip this step, as the TC Vitals files for 1991-1999 are online
#     (except we're currently missing 1992) and will be used.
#     However, if you're trying to track, for example, a midlatitude 
#     synoptic storm or a subtropical low, then you need to create a TC 
#     Vitals record for that storm that includes the following parameters 
#     in the appropriate character positions: 3-character storm ID, Storm
#     name, starting YYMMDD, starting HHMM, initial Lat, initial Lon,
#     direction of storm movement, and speed of storm movement.  (See
#     Tim Marchok or Steve Lord to get the exact format).  NOTE: the
#     online TC Vitals files are the operational files, NOT Best Track. 
#     If you want to use Best Track data, include that file name here and
#     the tracker will use it instead of the operational file.  Enter the
#     name of your auxiliary TC Vitals file:
#
#     Example: export AUXTCVIT=/ptmp/wx20tm/ecoast.wint97
 
export AUXTCVIT=


#     -----------------------------------------------------
#     ENTER THRESHOLDS FOR USE IN VERIFYING STORM STRUCTURE
#     -----------------------------------------------------
#
#     This allows you to enter your own thresholds for mslp gradient and
#     850 mb tangential winds.  These are used in subroutine is_it_a_storm
#     at each forecast hour to verify that the center that you've found 
#     at least resembles a storm.  It helps to have this check so that, 
#     in the case of weak storms that have dissipated, you don't end up
#     latching onto some weak passing trough.  When this check was not in
#     there in the original version of the tracker, we wound up with some
#     bizarre, jumpy tracks for weak and/or dissipating systems.  In the
#     operational version, the values are hard-wired in as requiring a
#     mslp gradient of at least 1 mb / 200 km (0.005 mb/km), and requiring
#     the average cyclonic tangential winds at 850 mb within a specified
#     radius (the radius depends on each model's grid resolution) to be 
#     at least 5 m/s.  If you want different thresholds, then change the 
#     default values below....
#
#     Example:
#     export MSLPTHRESH=0.0025
#     export V850THRESH=2.0000

export MSLPTHRESH=0.003
export V850THRESH=3.000

#${scrdir}/gentrak_step2.sh


# This script is the second in a set of 2 that starts the tracker for either
# operational or user-defined model output.  This script makes sure that the
# data files exist, it also pulls all of the needed data records out of the
# various GRIB forecast files and puts them into one, consolidated GRIB file,
# it also runs scripts that read the TC Vitals records for the input day and
# updates the TC Vitals (if necessary).  It then executes the gettrk 
# executable, which actually does the tracking.


inpstorm=${INPSTORM}
inpfile=${INPFILE}
fhint=${FHINT}
cmodel=${CMODEL}
symdh=${YYYYMMDDHH}
auxtcvit=${AUXTCVIT}

user=$LOGNAME
#export TMPDIR=/nfstmp/${user}/trak/${RANDOM}
export TMPDIR=$DATA/trak/${RANDOM}
if [ ! -d $TMPDIR ]; then mkdir -p $TMPDIR; fi
if [ -s ${TMPDIR}/tmpsynvit ]; then rm ${TMPDIR}/tmpsynvit; fi
if [ -s ${TMPDIR}/tmprawvit ]; then rm ${TMPDIR}/tmprawvit; fi

export HOMEDIR=${HOMEDIR:-/global/save/wx23sm/GFS/f2010/trunk/para}
export NWPROD=${NWPROD:-$HOMEDIR}

if [ -s $NWPROD/util/exec/wgrib ]; then
  wgrib=$NWPROD/util/exec/wgrib
else
  set +x
  echo " "
  echo "!!! ERROR: wgrib is not available, script will crash.  Exiting...."
  echo " "
  set -x
  exit 8
fi

wgrib_parmlist=" HGT:850 HGT:700 UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 SurfaceU SurfaceV ABSV:850 ABSV:700 PRMSL:MSL "

export maxtime=22    # Max number of forecast time levels

if [ ! -d ${vdir} ];   then mkdir -p ${vdir};   fi
if [ ! -d ${TMPDIR} ]; then mkdir -p ${TMPDIR}; fi

CENT=`echo ${symdh} | cut -c1-2`
syy=`echo ${symdh} | cut -c3-4`
smm=`echo ${symdh} | cut -c5-6`
sdd=`echo ${symdh} | cut -c7-8`
shh=`echo ${symdh} | cut -c9-10`
dishh=${shh}
symd=${syy}${smm}${sdd}

case ${shh} in
 0|00) dishh="00";;
 3|03) dishh="03";;
 6|06) dishh="06";;
   12) dishh="12";;
   15) dishh="15";;
   18) dishh="18";;
esac

#---------------------------------------------------#
# Convert the input model to lowercase letters and 
# check to see if it's a valid model, and assign a
# model ID number to it.
#---------------------------------------------------#

cmodel=`echo ${cmodel} | tr "[A-Z]" "[a-z]"`

case ${cmodel} in 

  gdas) set +x; echo " "; echo " ++ operational GDAS chosen"; set -x;
       fcstlen=9                                        ;
       fcsthrs=' 00 03 06 09 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99';
       atcfnum=72                                       ;
       atcfname="gdas"                                  ;
       atcfout="gdas"                                   ;
       modtyp='global'
       model=8;;
  gfs) set +x; echo " "; echo " ++ operational GFS chosen"; set -x;
		fcsthrsgfs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78';
		gfsdir=$COMIN;
		gfsgfile=gfs.t${dishh}z.pgrbf;
        model=1;;
  mrf) set +x; echo " "; echo " ++ operational MRF chosen"; set -x;
		fcsthrsmrf=' 00 12 24 36 48 60 72';
		mrfdir=$COMIN;
		mrfgfile=drfmr.t${dishh}z.pgrbf;
        model=2;;
  ukmet) set +x; echo " "; echo " ++ operational UKMET chosen"; set -x;
		fcsthrsukmet=' 00 12 24 36 48 60 72';
		ukmetdir=$COMIN;
		ukmetgfile=ukmet.t${dishh}z.ukmet;
        model=3;;
  ecmwf) set +x; echo " "; echo " ++ operational ECMWF chosen"; set -x;
		fcsthrsecmwf=' 00 24 48 72';
		ecmwfdir=$COMIN;
		ecmwfgfile=ecmgrb25.t12z;
        model=4;;
  ngm) set +x; echo " "; echo " ++ operational NGM chosen"; set -x;
		fcsthrsngm=' 00 06 12 18 24 30 36 42 48';
		ngmdir=$COMIN;
		ngmgfile=ngm.t${dishh}z.pgrb.f;
        model=5;;
  nam) set +x; echo " "; echo " ++ operational Early NAM chosen"; set -x;
		fcsthrsnam=' 00 06 12 18 24 30 36 42 48';
		namdir=$COMIN;
		namgfile=nam.t${dishh}z.awip32;
        model=6;;
  ngps) set +x; echo " "; echo " ++ operational NOGAPS chosen"; set -x;
		fcsthrsngps=' 00 12 24 36 48 60 72';
		#ngpsdir=/com/hourly/prod/hourly.${CENT}${symd};
		ngpsdir=$OMIN;
		ngpsgfile=fnoc.t${dishh}z;
        model=7;;
  other) set +x; echo " "; echo " Model selected by user is ${cmodel}, which is a ";
         echo "user-defined model, NOT operational...."; echo " "; set -x;
         model=9;;
  *) set +x; echo " "; echo " !!! Model selected is not recognized.";
     echo " Model= ---> ${cmodel} <--- ..... Please submit the script again....";
     echo " ";  set -x; exit 8;;

esac

#-------------------------------------------------
# Initialize the fh array that's used in 
# telling the fortran program the number of
# forecast hours and what those forecast 
# hours are.  If the model selected is an 
# operational model, the tracker already knows
# what those hours are; it only needs this info
# if the model is user-defined.
#-------------------------------------------------

fct=1
while [ ${fct} -le 14 ]; 
do
  fh[${fct}]=99
  let fct=fct+1
done


#------------------------------------------------------#
# Set the directories for the operational files.  For
# a user-specified model, we need to process the 
# input file to get the necessary info about the 
# data directory, etc.....
#------------------------------------------------------#

if [ ${cmodel} = 'other' ]; then

# This next bit of code tears apart the input file name to get the
# data directory and data file names.  pos1, pos2, pos3 and pos4 
# refer to character string positions in the filename string.  The
# idea of this next bit of code is to pull the file name apart to
# get a shell for the file name, so that if a user has a file
# name such as pgbf00.97090100, the script knows where in the 
# string to substitute the forecast hours.  IMPORTANT NOTE: The 
# file name that the user enters must have the characters "XX" in
# in place of the forecast hour characters "00" in order for this
# to work.
#
# pos1= character position immediately before "00" starts
# pos2= character position at which "00" starts
# pos3= character position immediately after "00"
# pos4= character position of last character in name

  otherdir=`dirname ${inpfile}`
  fname=`basename ${inpfile}`
  
  pos2=`echo ${fname} | nawk '{ match($0,/XX/); print RSTART }'`
  pos4=`echo ${fname} | nawk '{ match($0,/$/); print RSTART }'`
  let pos4=pos4-1
  let pos1=pos2-1
  let pos3=pos2+2

  if [ ${pos2} -eq 0 ]; then
    set +x
    echo " "
    echo " !!! ERROR! Something wrong with name of input file name for the"
    echo " analysis file.  Either the input file is missing, or you did not"
    echo " replace the forecast hour characters 00 with XX.  Please check the"
    echo " name in the kickoff script and qsub it again.  Exiting....."
    echo " "
    set -x
    exit 8
  fi
  
  fnamebeg=`echo ${fname} | cut -c1-${pos1}`
  if [ ${pos4} -ge ${pos3} ]; then
    fnameend=`echo ${fname} | cut -c${pos3}-${pos4}`
  else
    fnameend=""
  fi
  
  fflag='y'
  fhour=0
  fcsthrsother=''
  fhrct=0
  while [ ${fflag} = 'y' ]; 
  do
  
    if [ ${fhrct} -eq 14 ]; then
      set +x
      echo " "
      echo " !!! Exiting loop, only processing 14 forecast files ...."
      echo " "
      set -x
      break
    fi 
  
    if [ ${fhour} -lt 10 ]; then
      fhour=0${fhour}
    fi
  
    if [ -s ${otherdir}/${fnamebeg}${fhour}${fnameend} ]; then
      maxhour=${fhour}
      fcsthrsother=${fcsthrsother}" ${fhour}"
      set +x
      echo " "
      echo " +++ Found file ${fnamebeg}${fhour}${fnameend}"
      echo " "
      set -x
      let fhrct=fhrct+1
    else
      fflag='n'
    fi

    let fhour=fhour+fhint
  
  done

  if [ ! -s ${otherdir}/${fnamebeg}00${fnameend} ]; then
    set +x
    echo " "
    echo " !!! ERROR in `basename $0`"
    echo " !!! Input analysis file cannot be found."
    echo " !!! The tracker is looking for this file in:  "
    echo " !!! ---->  ${otherdir}/${fnamebeg}00${fnameend}"
    echo " !!! Please check the directory to make sure the file"
    echo " !!! is there and then submit this job again."
    echo " "
    set -x
    exit 8
  fi
  
  set +x
  echo " "
  echo " Max forecast hour is $maxhour" 
  echo " List of forecast hours: $fcsthrsother"
  echo " "
  set -x

# --------------------------------------------------
# In order for the fortran program to know how many 
# forecast times there are and what those forecast
# hours are, we need to include this information in
# the namelist file.  So first, put this information
# into an array, then at the end of this script, 
# we'll put it into the namelist file.

  fhour=0
  fct=1
  while [ ${fct} -le 14 ]; 
  do

    if [ ${fhour} -le ${maxhour} ]; then
      fh[${fct}]=${fhour}
    else
      fh[${fct}]=99
    fi

    let fct=fct+1
    let fhour=fhour+fhint

  done

fi

cp $DATA/tcvitals ${vdir}/vitals.${symd}${dishh}

grep -v TEST ${vdir}/vitals.${symd}${dishh} | \
     nawk 'substr($0,6,1) !~ /[8-9]/ {print $0}' >${vdir}/tempvit.nonameless

mv ${vdir}/tempvit.nonameless ${vdir}/vitals.${symd}${dishh}

#--------------------------------------------------------------#
# Now run a fortran program that will read all the TC vitals
# records for the current dtg and the dtg from 6h ago, and
# sort out any duplicates.  If the program finds a storm that
# was included in the vitals file 6h ago but not for the current
# dtg, this program updates the 6h-old first guess position
# and puts these updated records as well as the records from
# the current dtg into a temporary vitals file.  It is this
# temporary vitals file that is then used as the input for the
# tracking program.
#--------------------------------------------------------------#

ymdh6ago=` $NWPROD/util/exec/ndate -6 ${CENT}${symd}${dishh}`
syy6=`echo ${ymdh6ago} | cut -c3-4`
smm6=`echo ${ymdh6ago} | cut -c5-6`
sdd6=`echo ${ymdh6ago} | cut -c7-8`
shh6=`echo ${ymdh6ago} | cut -c9-10`
symd6=${syy6}${smm6}${sdd6}

ymdh6ahead=` $NWPROD/util/exec/ndate 6 ${CENT}${symd}${dishh}`
syyp6=`echo ${ymdh6ahead} | cut -c3-4`
smmp6=`echo ${ymdh6ahead} | cut -c5-6`
sddp6=`echo ${ymdh6ahead} | cut -c7-8`
shhp6=`echo ${ymdh6ahead} | cut -c9-10`
symdp6=${syyp6}${smmp6}${sddp6}

echo "&datenowin   dnow%yy=${syy}, dnow%mm=${smm},"              >$TMPDIR/suv_input
echo "             dnow%dd=${sdd}, dnow%hh=${dishh}/"           >>$TMPDIR/suv_input
echo "&date6agoin  d6ago%yy=${syy6}, d6ago%mm=${smm6},"         >>$TMPDIR/suv_input
echo "             d6ago%dd=${sdd6}, d6ago%hh=${shh6}/"         >>$TMPDIR/suv_input
echo "&date6aheadin  d6ahead%yy=${syyp6}, d6ahead%mm=${smmp6}," >>$TMPDIR/suv_input
echo "               d6ahead%dd=${sddp6}, d6ahead%hh=${shhp6}/" >>$TMPDIR/suv_input

numvitrecs=`cat ${vdir}/vitals.${symd}${dishh} | wc -l`
if [ ${numvitrecs} -eq 0 ]; then
  set +x
  echo " "
  echo "!!! ERROR -- There are no vitals records for this time period."
  echo "!!! File ${vdir}/vitals.${symd}${dishh} is empty."
  echo "!!! It could just be that there are no storms for the current"
  echo "!!! time.  Please check the dates and submit this job again...."
  echo " "
  set -x
  exit 8
fi

# - - - - - - - - - - - - -
# Before running the program to read, sort and update the vitals,
# first run the vitals through some awk logic, the purpose of
# which is to convert all the 2-digit years into 4-digit years.
# Beginning 4/21/99, NHC and JTWC will begin sending the vitals
# with 4-digit years, however it is unknown when other global
# forecasting centers will begin using 4-digit years, thus we
# need the following logic to ensure that all the vitals going
# into supvitql.f have uniform, 4-digit years in their records.
#
# 1/8/2000: sed code added by Tim Marchok due to the fact that
#       some of the vitals were getting past the syndata/qctropcy
#       error-checking with a colon in them; the colon appeared
#       in the character immediately to the left of the date, which
#       was messing up the "(length($4) == 8)" statement logic.
# - - - - - - - - - - - - -

sed -e "s/\:/ /g"    ${vdir}/vitals.${symd}${dishh} > ${TMPDIR}/tempvit
mv ${TMPDIR}/tempvit ${vdir}/vitals.${symd}${dishh}

awk '
{
  yycheck = substr($0,20,2)
  if ((yycheck == 20 || yycheck == 19) && (length($4) == 8)) {
    printf ("%s\n",$0)
  }
  else {
    if (yycheck >= 0 && yycheck <= 50) {
      printf ("%s20%s\n",substr($0,1,19),substr($0,20))
    }
    else {
      printf ("%s19%s\n",substr($0,1,19),substr($0,20))
    }
  }
} ' ${vdir}/vitals.${symd}${dishh}  >${TMPDIR}/vitals.${symd}${dishh}.y4

mv ${TMPDIR}/vitals.${symd}${dishh}.y4 ${vdir}/vitals.${symd}${dishh}

#cp $auxtcvit ${vdir}/vitals.${symd}${dishh}

pgm=`basename  $SUPVX`
if [ -s $DATA/prep_step ]; then
   set +e
   . $DATA/prep_step
   set -e
else
   [ -f errfile ] && rm errfile
   export XLFUNITS=0
   unset `env | grep XLFUNIT | awk -F= '{print $1}'`

   set +u
   if [ -z "$XLFRTEOPTS" ]; then
     export XLFRTEOPTS="unit_vars=yes"
   else
     export XLFRTEOPTS="${XLFRTEOPTS}:unit_vars=yes"
   fi
   set -u

fi

export XLFUNIT_31=${vdir}/vitals.${symd}${dishh}
export XLFUNIT_51=${vdir}/vitals.upd.${cmodel}.${symd}${dishh}

$TIMEIT $SUPVX <$TMPDIR/suv_input > outout 2> errfile
err=$?
###cat errfile
cat errfile >> outout
cat outout >> supvit.out
set +u
[ -n "../$pgmout" ]  &&  cat outout >> ../$pgmout
set -u
rm outout
set +x
echo
echo 'The foreground exit status for SUPVIT is ' $err
echo
set -x

if [ $err -eq 0 ]; then
  set +x
  echo " "
  echo " Normal end for program supvitql (which updates TC vitals file)."
  echo " "
  set -x
else
  set +x
  echo " "
  echo "!!! ERROR -- An error occurred while running supvitql, "
  echo "!!! which is the program that updates the TC Vitals file."
  echo "!!! Return code from supvitql = ${err}"
  echo "!!! model= ${cmodel}, forecast initial time = ${symd}${dishh}"
  echo "!!! Exiting...."
  echo " "
  set -x
fi
if [ -s $DATA/err_chk ]; then
   $DATA/err_chk
else
   if test "$err" -gt '0'
   then
######kill -9 ${qid}
      exit 555
   fi
fi
[ "$err" -gt '0' ]  &&  exit 9


#------------------------------------------------------------------#
# Now select all storms to be processed, that is, process every
# storm that's listed in the updated vitals file for the current
# forecast hour.  If there are no storms for the current time,
# then exit.
#------------------------------------------------------------------#

numvitrecs=`cat ${vdir}/vitals.upd.${cmodel}.${symd}${dishh} | wc -l`
if [ ${numvitrecs} -eq 0 ]; then
  set +x
  echo " "
  echo "!!! ERROR -- There are no vitals records for this time period "
  echo "!!! in the UPDATED vitals file."
  echo "!!! File ${vdir}/vitals.upd.${cmodel}.${symd}${dishh} is empty."
  echo "!!! Please check the dates and submit this job again...."
  echo " "
  set -x
  exit 8
fi

set +x
echo " " | tee storm_list
echo " " | tee -a storm_list
echo " " | tee -a storm_list
echo " *--------------------------------*" | tee -a storm_list
echo " |        STORM SELECTION         |" | tee -a storm_list
echo " *--------------------------------*" | tee -a storm_list
echo " " | tee -a storm_list
echo " Below is a list of the storms to be processed: " | tee -a storm_list
echo " " | tee -a storm_list
cat ${vdir}/vitals.upd.${cmodel}.${symd}${dishh} | tee -a storm_list
echo " " | tee -a storm_list
set -x

set +u
[ -n "../$pgmout" ]  &&  cat storm_list >> ../$pgmout
set -u

rm storm_list

ict=1
while [ $ict -le 15 ]
do
  stormflag[${ict}]=3
  let ict=ict+1
done

dtg_current="${symd} ${dishh}00"
smax=` grep "${dtg_current}" ${vdir}/vitals.upd.${cmodel}.${symd}${dishh} | wc -l`

sct=1
while [ ${sct} -le ${smax} ]
do
  stormflag[${sct}]=1
  let sct=sct+1
done


#-----------------------------------------------------------------#
#
#         ------  CUT APART INPUT GRIB FILES  -------
#
# For the selected model, cut apart the GRIB input files in order
# to pull out only the variables that we need for the tracker.
# Put these selected variables from all forecast hours into 1 big
# GRIB file that we'll use as input for the tracker.
#
# The utility /nwprod/util/exec/wgrib is used to cut out the
# needed parms for the GFS, MRF, UKMET and NOGAPS files.
# The utility /nwprod/util/exec/copygb is used to interpolate the 
# NGM (polar stereographic) and NAM (Lambert Conformal) data from 
# their grids onto lat/lon grids.  Note that while the lat/lon 
# grid that I specify overlaps into areas that don't have any data 
# on the original grid, Mark Iredell wrote the copygb software so 
# that it will mask such "no-data" points with a bitmap (just be 
# sure to check the lbms in your fortran program after getgb).
#-----------------------------------------------------------------#

set +x
echo " "
echo " -----------------------------------------"
echo "   NOW CUTTING APART INPUT GRIB FILES TO "
echo "   CREATE 1 BIG GRIB INPUT FILE "
echo " -----------------------------------------"
echo " "
set -x

gix=$NWPROD/util/exec/grbindex
cgb=$NWPROD/util/exec/copygb
#grid='255 0 151 71 70000 190000 128 0000 340000 1000 1000 64'
#grid='255 0 360 181 90000 0000 128 -90000 -1000 1000 1000 64'
#grid='255 0 360 181 90000 0000 128 -90000 -1000 1000 1000 0'

regflag=`grep NHC ${vdir}/vitals.upd.${cmodel}.${symd}${dishh} | wc -l`

# ----------------------------
#   Process NGM, if selected
# ----------------------------
  
if [ ${model} -eq 5 ]; then

  grid='255 0 151 71 70000 190000 128 0000 340000 1000 1000 64'

  if [ ${regflag} = 'n' ]; then
    set +x
    echo " "
    echo " *******************************************************************"
    echo " !!! NGM model has been selected, but there are no storms in the"
    echo " !!! TC Vitals file that are from NHC.  Therefore, unless you have"
    echo " !!! entered your own auxiliary TC vitals file that has a storm "
    echo " !!! within the NGM domain, the tracker will exit after reading "
    echo " !!! in the analysis data."
    echo " *******************************************************************"
    echo " "
    set -x
  fi

  if [ -s ${vdir}/ngmlatlon.pgrb.${symd}${dishh} ]; then
    rm ${vdir}/ngmlatlon.pgrb.${symd}${dishh}
  fi

  for fhour in ${fcsthrsngm}
  do

    if [ ! -s ${ngmdir}/${ngmgfile}${fhour} ]; then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! NGM File missing: ${ngmdir}/${ngmgfile}${fhour}"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      set -x
      continue
    fi
    if [ -s $TMPDIR/tmpixfile ]; then rm $TMPDIR/tmpixfile; fi
    $gix ${ngmdir}/${ngmgfile}${fhour} $TMPDIR/tmpixfile
    x1=$TMPDIR/tmpixfile

    set +x
    echo " "
    echo " Extracting NGM GRIB data for forecast hour = $fhour"
    echo " "
    set -x

    g1=${ngmdir}/${ngmgfile}${fhour}       
   
    $cgb -g"$grid" -k'2*-1 104 -1 33 100 850' $g1 $x1 $TMPDIR/ngmllu850.grb.f${fhour}; rcc1=$?
    $cgb -g"$grid" -k'2*-1 104 -1 33 100 700' $g1 $x1 $TMPDIR/ngmllu700.grb.f${fhour}; rcc2=$?
    $cgb -g"$grid" -k'2*-1 104 -1 33 100 500' $g1 $x1 $TMPDIR/ngmllu500.grb.f${fhour}; rcc3=$?
    $cgb -g"$grid" -k'2*-1 104 -1 33 105 10'  $g1 $x1 $TMPDIR/ngmllu10m.grb.f${fhour};  rcc4=$?
    $cgb -g"$grid" -k'2*-1 104 -1 41 100 850' $g1 $x1 $TMPDIR/ngmllav850.grb.f${fhour}; rcc5=$?
    $cgb -g"$grid" -k'2*-1 104 -1 41 100 700' $g1 $x1 $TMPDIR/ngmllav700.grb.f${fhour}; rcc6=$?
    $cgb -g"$grid" -k'2*-1 104 -1  7 100 850' $g1 $x1 $TMPDIR/ngmllz850.grb.f${fhour}; rcc7=$?
    $cgb -g"$grid" -k'2*-1 104 -1  7 100 700' $g1 $x1 $TMPDIR/ngmllz700.grb.f${fhour}; rcc8=$?
    $cgb -g"$grid" -k'2*-1 104 -1  2 102 0'   $g1 $x1 $TMPDIR/ngmllmslp.grb.f${fhour}; rcc9=$?

    if [ $rcc1 -eq 134 -o $rcc2 -eq 134 -o $rcc3 -eq 134 -o $rcc4 -eq 134 -o $rcc5 -eq 134 -o \
         $rcc6 -eq 134 -o $rcc7 -eq 134 -o $rcc8 -eq 134 -o $rcc9 -eq 134 ]; then
      set +x
      echo " "
      echo "!!! ERROR using $cgb to interpolate ngm data.  We will stop execution because"
      echo "!!! some variables may have been copied okay, while some obviously have not, "
      echo "!!! and that could lead to unreliable results from the tracker.  Check to make"
      echo "!!! sure you've allocated enough memory for this job (error 134 using $cgb is "
      echo "!!! typically due to using more memory than you've allocated).  Exiting....."
      echo " "
      set -x
      exit 8
    fi

    cat $TMPDIR/ngmllu850.grb.f${fhour} $TMPDIR/ngmllu700.grb.f${fhour} \
        $TMPDIR/ngmllu500.grb.f${fhour} $TMPDIR/ngmllz850.grb.f${fhour} \
        $TMPDIR/ngmllz700.grb.f${fhour} $TMPDIR/ngmllmslp.grb.f${fhour} \
        $TMPDIR/ngmllav850.grb.f${fhour} $TMPDIR/ngmllav700.grb.f${fhour} \
        $TMPDIR/ngmllu10m.grb.f${fhour} \
        >>${vdir}/ngmlatlon.pgrb.${symd}${dishh}

  done

  $gix ${vdir}/ngmlatlon.pgrb.${symd}${dishh} ${vdir}/ngmlatlon.pgrb.ix.${symd}${dishh}
  gribfile=${vdir}/ngmlatlon.pgrb.${symd}${dishh}
  ixfile=${vdir}/ngmlatlon.pgrb.ix.${symd}${dishh}

fi


# ----------------------------------
#   Process Early NAM, if selected
# ----------------------------------
  
if [ ${model} -eq 6 ]; then

  grid='255 0 301 141 70000 190000 128 0000 340000  500  500 64'

  if [ ${regflag} = 'n' ]; then
    set +x
    echo " "
    echo " *******************************************************************"
    echo " !!! NAM model has been selected, but there are no storms in the"
    echo " !!! TC Vitals file that are from NHC.  Therefore, unless you have"
    echo " !!! entered your own auxiliary TC vitals file that has a storm "
    echo " !!! within the NAM domain, the tracker will exit after reading "
    echo " !!! in the analysis data."
    echo " *******************************************************************"
    echo " "
    set -x
  fi

  if [ -s ${vdir}/namlatlon.pgrb.${symd}${dishh} ]; then
    rm ${vdir}/namlatlon.pgrb.${symd}${dishh}
  fi

  for fhour in ${fcsthrsnam}
  do

    if [ ! -s ${namdir}/${namgfile}${fhour}.tm00 ]; then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! Early NAM File missing: ${namdir}/${namgfile}${fhour}.tm00"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      set -x
      continue
    fi
    if [ -s $TMPDIR/tmpixfile ]; then rm $TMPDIR/tmpixfile; fi
    $gix ${namdir}/${namgfile}${fhour}.tm00 $TMPDIR/tmpixfile
    x1=$TMPDIR/tmpixfile

    set +x
    echo " "
    echo " Extracting Early NAM GRIB data for forecast hour = $fhour"
    echo " "
    set -x
  
    g1=${namdir}/${namgfile}${fhour}.tm00
     
    if [ -s $TMPDIR/namlatlon.pgrb ]; then rm $TMPDIR/namlatlon.pgrb; fi
    $cgb -g"$grid" -k'4*-1 33 100 850' $g1 $x1 $TMPDIR/namllu850.grb.f${fhour}; rcc1=$?
    $cgb -g"$grid" -k'4*-1 33 100 700' $g1 $x1 $TMPDIR/namllu700.grb.f${fhour}; rcc2=$?
    $cgb -g"$grid" -k'4*-1 33 100 500' $g1 $x1 $TMPDIR/namllu500.grb.f${fhour}; rcc3=$?
    $cgb -g"$grid" -k'4*-1 33 105 10'  $g1 $x1 $TMPDIR/namllu10m.grb.f${fhour}; rcc4=$?
    $cgb -g"$grid" -k'4*-1 41 100 850' $g1 $x1 $TMPDIR/namllav850.grb.f${fhour}; rcc5=$?
    $cgb -g"$grid" -k'4*-1 41 100 700' $g1 $x1 $TMPDIR/namllav700.grb.f${fhour}; rcc6=$?
    $cgb -g"$grid" -k'4*-1  7 100 850' $g1 $x1 $TMPDIR/namllz850.grb.f${fhour}; rcc7=$?
    $cgb -g"$grid" -k'4*-1  7 100 700' $g1 $x1 $TMPDIR/namllz700.grb.f${fhour}; rcc8=$?
    $cgb -g"$grid" -k'4*-1  2 102   0' $g1 $x1 $TMPDIR/namllmslp.grb.f${fhour}; rcc9=$?

    if [ $rcc1 -eq 134 -o $rcc2 -eq 134 -o $rcc3 -eq 134 -o $rcc4 -eq 134 -o $rcc5 -eq 134 -o \
         $rcc6 -eq 134 -o $rcc7 -eq 134 -o $rcc8 -eq 134 -o $rcc9 -eq 134 ]; then
      set +x
      echo " "
      echo "!!! ERROR using $cgb to interpolate nam data.  We will stop execution because"
      echo "!!! some variables may have been copied okay, while some obviously have not, "
      echo "!!! and that could lead to unreliable results from the tracker.  Check to make"
      echo "!!! sure you've allocated enough memory for this job (error 134 using $cgb is "
      echo "!!! typically due to using more memory than you've allocated).  Exiting....."
      echo " "
      set -x
      exit 8
    fi

    cat $TMPDIR/namllu850.grb.f${fhour} $TMPDIR/namllu700.grb.f${fhour} \
        $TMPDIR/namllu500.grb.f${fhour} $TMPDIR/namllz850.grb.f${fhour} \
        $TMPDIR/namllz700.grb.f${fhour} $TMPDIR/namllmslp.grb.f${fhour} \
        $TMPDIR/namllav850.grb.f${fhour} $TMPDIR/namllav700.grb.f${fhour} \
        $TMPDIR/namllu10m.grb.f${fhour} \
        >>${vdir}/namlatlon.pgrb.${symd}${dishh}
  
  done

  $gix ${vdir}/namlatlon.pgrb.${symd}${dishh} ${vdir}/namlatlon.pgrb.ix.${symd}${dishh}
  gribfile=${vdir}/namlatlon.pgrb.${symd}${dishh}
  ixfile=${vdir}/namlatlon.pgrb.ix.${symd}${dishh}

fi


# ------------------------------
#   Process ECMWF, if selected
# ------------------------------

# ECMWF is not a regional grid, however they currently (6/98) only send us the
# global belt from 35S to 35N.  Thus, it will have grid boundaries that may 
# interfere with the tracking algorithm.  It is crucial to the proper 
# functioning of the tracking program to give any regional grid dataset a 
# buffer zone around the grid boundaries, with null values in that buffer 
# zone that are bitmapped out.  That's why we use Mark Iredell's grib 
# interpolater here, to add a 5 degree buffer zone to the north and south of
# the ECMWF grid boundary; his interpolater adds the null values in the
# bitmap surrounding the area with valid data.  If ECMWF begins sending us 
# the entire global data set, then this bit of code should
# be taken out, and the data should then be processed as the other normal
# full-coverage global models (ukmet, mrf, gfs, nogaps) currently are.
  
if [ ${model} -eq 4 ]; then

  if [ ! -s ${ecmwfdir}/${ecmwfgfile} ]; then
    set +x
    echo " "
    echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo " !!! ECMWF GRIB or Index File missing from directory: ${ecmwfdir}"
    echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo " "
    echo " !!! Due to missing ECMWF file, execution is ending...."
    echo " "
    set -x
    exit 8
  fi

  $gix ${ecmwfdir}/${ecmwfgfile} $TMPDIR/${ecmwfgfile}.ix
  x1=$TMPDIR/${ecmwfgfile}.ix

  if [ -s ${vdir}/ecmwf.bufzone.grb.${symd}${dishh} ]; then
    rm ${vdir}/ecmwf.bufzone.grb.${symd}${dishh}
  fi
  if [ -s ${vdir}/ecmwf.bufzone.ix.${symd}${dishh} ]; then
    rm ${vdir}/ecmwf.bufzone.ix.${symd}${dishh}
  fi

  g1=${ecmwfdir}/${ecmwfgfile}
  ecgrid='255 0 144 33 40000 0000 128 -40000 357500 2500 2500 64'
  $cgb -g"$ecgrid" $g1 $x1 ${vdir}/ecmwf.bufzone.grb.${symd}${dishh}
  $gix ${vdir}/ecmwf.bufzone.grb.${symd}${dishh} ${vdir}/ecmwf.bufzone.ix.${symd}${dishh}
  gribfile=${vdir}/ecmwf.bufzone.grb.${symd}${dishh}
  ixfile=${vdir}/ecmwf.bufzone.ix.${symd}${dishh}

fi


# ------------------------------
#   Process GFS, if selected
# ------------------------------
  
if [ ${model} -eq 1 ]; then

  if [ -s ${vdir}/gfsgribfile.${symd}${dishh} ]; then 
    rm ${vdir}/gfsgribfile.${symd}${dishh}
  fi

  for fhour in ${fcsthrsgfs}
  do

    if [ ! -s ${gfsdir}/${gfsgfile}${fhour} ]; then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! GFS File missing: ${gfsdir}/${gfsgfile}${fhour}"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      set -x
      continue
    fi

    gfile=${gfsdir}/${gfsgfile}${fhour}
    $wgrib -s $gfile >$TMPDIR/gfs.ix

    for parm in ${wgrib_parmlist}
    do
      case ${parm} in
        "SurfaceU")
          grep "UGRD:10 m " $TMPDIR/gfs.ix | $wgrib -s $gfile -i -grib -append \
                                          -o ${vdir}/gfsgribfile.${symd}${dishh} ;;
        "SurfaceV")
          grep "VGRD:10 m " $TMPDIR/gfs.ix | $wgrib -s $gfile -i -grib -append \
                                          -o ${vdir}/gfsgribfile.${symd}${dishh} ;;
                 *)
          grep "${parm}" $TMPDIR/gfs.ix | $wgrib -s $gfile -i -grib -append \
                                          -o ${vdir}/gfsgribfile.${symd}${dishh} ;;
      esac

    done

  done

  $gix ${vdir}/gfsgribfile.${symd}${dishh} ${vdir}/gfsixfile.${symd}${dishh}
  gribfile=${vdir}/gfsgribfile.${symd}${dishh}
  ixfile=${vdir}/gfsixfile.${symd}${dishh}

fi


# ------------------------------
#   Process GDAS, if selected
# ------------------------------
  
if [ ${model} -eq 8 ]; then

  if [ -s ${vdir}/gdasgribfile.${symd}${dishh} ]; then 
    rm ${vdir}/gdasgribfile.${symd}${dishh}
  fi

  for gfile in $DATA/pgm6prep $DATA/pgm3prep $DATA/pgesprep $DATA/pgp3prep
  do

    if [ ! -s $gfile ]; then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! gdas File missing: $gfile"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " "
      set -x
      continue
    fi

    $wgrib -s $gfile >$TMPDIR/gdas.ix

    for parm in ${wgrib_parmlist}
    do

      case ${parm} in
        "SurfaceU")
          grep "UGRD:10 m " $TMPDIR/gdas.ix | $wgrib -s $gfile -i -grib -append \
                                          -o ${vdir}/gdasgribfile.${symd}${dishh} ;;
        "SurfaceV")
          grep "VGRD:10 m " $TMPDIR/gdas.ix | $wgrib -s $gfile -i -grib -append \
                                          -o ${vdir}/gdasgribfile.${symd}${dishh} ;;
                 *)
          grep "${parm}" $TMPDIR/gdas.ix | $wgrib -s $gfile -i -grib -append \
                                          -o ${vdir}/gdasgribfile.${symd}${dishh} ;;
      esac

    done

  done

  $gix ${vdir}/gdasgribfile.${symd}${dishh} ${vdir}/gdasixfile.${symd}${dishh}
  gribfile=${vdir}/gdasgribfile.${symd}${dishh}
  ixfile=${vdir}/gdasixfile.${symd}${dishh}

fi
# ------------------------------
#   Process MRF, if selected
# ------------------------------
  
if [ ${model} -eq 2 ]; then

  if [ -s ${vdir}/mrfgribfile.${symd}${dishh} ]; then 
    rm ${vdir}/mrfgribfile.${symd}${dishh}
  fi

  for fhour in ${fcsthrsmrf}
  do

    if [ ! -s ${mrfdir}/${mrfgfile}${fhour} ]; then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! MRF File missing: ${mrfdir}/${mrfgfile}${fhour}"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " "
      set -x
      continue
    fi

    gfile=${mrfdir}/${mrfgfile}${fhour}
    $wgrib -s $gfile >$TMPDIR/mrf.ix

    for parm in ${wgrib_parmlist}
    do

      case ${parm} in
        "SurfaceU")
          grep "UGRD:10 m " $TMPDIR/mrf.ix | $wgrib -s $gfile -i -grib -append \
                                          -o ${vdir}/mrfgribfile.${symd}${dishh} ;;
        "SurfaceV")
          grep "VGRD:10 m " $TMPDIR/mrf.ix | $wgrib -s $gfile -i -grib -append \
                                          -o ${vdir}/mrfgribfile.${symd}${dishh} ;;
                 *)
          grep "${parm}" $TMPDIR/mrf.ix | $wgrib -s $gfile -i -grib -append \
                                          -o ${vdir}/mrfgribfile.${symd}${dishh} ;;
      esac

    done

  done

  $gix ${vdir}/mrfgribfile.${symd}${dishh} ${vdir}/mrfixfile.${symd}${dishh}
  gribfile=${vdir}/mrfgribfile.${symd}${dishh}
  ixfile=${vdir}/mrfixfile.${symd}${dishh}

fi


# ------------------------------
#   Process UKMET, if selected
# ------------------------------
  
if [ ${model} -eq 3 ]; then

  if [ -s ${vdir}/ukmetgribfile.${symd}${dishh} ]; then 
    rm ${vdir}/ukmetgribfile.${symd}${dishh}
  fi

  wgrib_parmlist=' HGT:850 HGT:700 UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 UGRD:sfc VGRD:sfc ABSV:850 ABSV:700 PRMSL:MSL '

  for fhour in ${fcsthrsukmet}
  do

    if [ ! -s ${ukmetdir}/${ukmetgfile}${fhour} ]; then
      set +x
      echo " "
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " !!! UKMET File missing: ${ukmetdir}/${ukmetgfile}${fhour}"
      echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      echo " "
      set -x
      continue
    fi

    gfile=${ukmetdir}/${ukmetgfile}${fhour}
    $wgrib -s $gfile >$TMPDIR/ukmet.ix

    for parm in ${wgrib_parmlist}
    do
      grep "${parm}" $TMPDIR/ukmet.ix | $wgrib -s $gfile -i -grib -append \
                                     -o ${vdir}/ukmetgribfile.${symd}${dishh}
    done

  done

  $gix ${vdir}/ukmetgribfile.${symd}${dishh} ${vdir}/ukmetixfile.${symd}${dishh}
  gribfile=${vdir}/ukmetgribfile.${symd}${dishh}
  ixfile=${vdir}/ukmetixfile.${symd}${dishh}

fi


# ------------------------------
#   Process NOGAPS, if selected
# ------------------------------

if [ ${model} -eq 7 ]; then

  if [ -s ${vdir}/ngpsgribfile.${symd}${dishh} ]; then
    rm ${vdir}/ngpsgribfile.${symd}${dishh}
  fi

  if [ ! -s ${ngpsdir}/${ngpsgfile} ]; then
    set +x
    echo " "
    echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo " !!! NOGAPS File missing: ${ngpsdir}/${ngpsgfile}"
    echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo " "
    echo " !!! Due to missing NOGAPS file, execution is ending...."
    echo " "
    set -x
    exit 8
  fi

  gfile=${ngpsdir}/${ngpsgfile}
  $wgrib -s $gfile >$TMPDIR/ngps.ix

  for fhour in ${fcsthrsngps}
  do

    if [ $fhour = '00' ]; then
      vtstring=":anl:"
    else
      vtstring="${fhour}hr"
    fi

    for parm in ${wgrib_parmlist}
    do
      case ${parm} in
        "SurfaceU")
          grep "UGRD:19 m " $TMPDIR/ngps.ix | grep ${vtstring} | \
            $wgrib -s $gfile -i -grib -append -o ${vdir}/ngpsgribfile.${symd}${dishh} ;;
        "SurfaceV")
          grep "VGRD:19 m " $TMPDIR/ngps.ix | grep ${vtstring} | \
            $wgrib -s $gfile -i -grib -append -o ${vdir}/ngpsgribfile.${symd}${dishh} ;;
                 *)
          grep "${parm}" $TMPDIR/ngps.ix | grep ${vtstring} | \
            $wgrib -s $gfile -i -grib -append -o ${vdir}/ngpsgribfile.${symd}${dishh} ;;
      esac
    done

  done

  $gix ${vdir}/ngpsgribfile.${symd}${dishh} ${vdir}/ngpsixfile.${symd}${dishh}
  gribfile=${vdir}/ngpsgribfile.${symd}${dishh}
  ixfile=${vdir}/ngpsixfile.${symd}${dishh}

fi


# ---------------------------------------------
#   Process User-specified Model, if selected
# ---------------------------------------------

if [ ${model} -eq 9 ]; then

# We need to first check whether or not the data in the file are stored
# on a lat/lon grid or not.  We do this by scanning the analysis file 
# with Wesley's grib utility, and checking the value of the "Data 
# Representation Type", which is stored in byte #6 in the GDS of each 
# grib file.  A value of 0 indicates an equidistant lat/lon grid.

  if [ -s ${vdir}/otherlatlon.pgrb.${symdh} ]; then
    rm ${vdir}/otherlatlon.pgrb.${symdh}
  fi

  gridtyp=`$wgrib -GDS10 ${otherdir}/${fnamebeg}00${fnameend} | \
           awk -FGDS10= '{print $2}' | awk '{print $6}' | sed -n 1p`

  if [ ${gridtyp} -eq 0 ]; then

#   The data are already on a lat/lon grid, we do not need to 
#   interpolate the data, just pull out the records that we need
#   using wgrib.

    for fhour in ${fcsthrsother}
    do
  
      if [ ! -s ${otherdir}/${fnamebeg}${fhour}${fnameend} ]; then
        set +x
        echo " "
        echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo "!!! Forecast File missing: ${otherdir}/${fnamebeg}00${fnameend}"
        echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo " "
        set -x
        continue
      fi
  
      gfile=${otherdir}/${fnamebeg}${fhour}${fnameend}
      $wgrib -s $gfile >$TMPDIR/other.ix

      for parm in ${wgrib_parmlist}
      do

        case ${parm} in
          "SurfaceU")
            grep "UGRD:10 m " $TMPDIR/other.ix | $wgrib -s $gfile -i -grib -append \
                                            -o ${vdir}/otherlatlon.pgrb.${symdh} ;;
          "SurfaceV")
            grep "VGRD:10 m " $TMPDIR/other.ix | $wgrib -s $gfile -i -grib -append \
                                            -o ${vdir}/otherlatlon.pgrb.${symdh} ;;
                   *)
            grep "${parm}" $TMPDIR/other.ix | $wgrib -s $gfile -i -grib -append \
                                            -o ${vdir}/otherlatlon.pgrb.${symdh} ;;
        esac

      done
  
    done
  
  else

#   The data are on a grid that is something other than a lat/lon grid.
#   Use Mark Iredell's interpolator to interpolate the data to a lat/lon
#   grid and pull out the records that we need.

    othergrid='255 0 360 181 90000 0000 128 -90000 -1000 1000 1000 64'

    for fhour in ${fcsthrsother}
    do

      if [ ! -s ${otherdir}/${fnamebeg}${fhour}${fnameend} ]; then
        set +x
        echo " "
        echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo "!!! Forecast File missing: ${otherdir}/${fnamebeg}00${fnameend}"
        echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo " "
        set +x
        continue
      fi

      if [ -s $TMPDIR/tmpixfile ]; then rm $TMPDIR/tmpixfile; fi
      $gix ${otherdir}/${fnamebeg}${fhour}${fnameend} $TMPDIR/tmpixfile
      x1=$TMPDIR/tmpixfile

      g1=${otherdir}/${fnamebeg}${fhour}${fnameend}
  
      $cgb -g"$othergrid" -k'4*-1 33 100 850' $g1 $x1 $TMPDIR/otherllu850.grb.f${fhour}; rcc1=$?
      $cgb -g"$othergrid" -k'4*-1 33 100 700' $g1 $x1 $TMPDIR/otherllu700.grb.f${fhour}; rcc2=$?
      $cgb -g"$othergrid" -k'4*-1 33 100 500' $g1 $x1 $TMPDIR/otherllu500.grb.f${fhour}; rcc3=$?
      $cgb -g"$othergrid" -k'4*-1 33 105 10'  $g1 $x1 $TMPDIR/otherllu10m.grb.f${fhour}; rcc4=$?
      $cgb -g"$othergrid" -k'4*-1 41 100 850' $g1 $x1 $TMPDIR/otherllav850.grb.f${fhour}; rcc5=$?
      $cgb -g"$othergrid" -k'4*-1 41 100 700' $g1 $x1 $TMPDIR/otherllav700.grb.f${fhour}; rcc6=$?
      $cgb -g"$othergrid" -k'4*-1  7 100 850' $g1 $x1 $TMPDIR/otherllz850.grb.f${fhour}; rcc7=$?
      $cgb -g"$othergrid" -k'4*-1  7 100 700' $g1 $x1 $TMPDIR/otherllz700.grb.f${fhour}; rcc8=$?
      $cgb -g"$othergrid" -k'4*-1  2 102   0' $g1 $x1 $TMPDIR/otherllmslp.grb.f${fhour}; rcc9=$?

      if [ $rcc1 -eq 134 -o $rcc2 -eq 134 -o $rcc3 -eq 134 -o $rcc4 -eq 134 -o $rcc5 -eq 134 -o \
           $rcc6 -eq 134 -o $rcc7 -eq 134 -o $rcc8 -eq 134 -o $rcc9 -eq 134 ]; then
        set +x
        echo " "
        echo "!!! ERROR using $cgb to interpolate data.  We will stop execution because"
        echo "!!! some variables may have been copied okay, while some obviously have not, "
        echo "!!! and that could lead to unreliable results from the tracker.  Check to make"
        echo "!!! sure you've allocated enough memory for this job (error 134 using $cgb is "
        echo "!!! typically due to using more memory than you've allocated).  Exiting....."
        echo " "
        set -x
        exit 8
      fi

      cat $TMPDIR/otherllu850.grb.f${fhour} $TMPDIR/otherllu700.grb.f${fhour} \
          $TMPDIR/otherllu500.grb.f${fhour} $TMPDIR/otherllz850.grb.f${fhour} \
          $TMPDIR/otherllz700.grb.f${fhour} $TMPDIR/otherllmslp.grb.f${fhour} \
         $TMPDIR/otherllav850.grb.f${fhour} $TMPDIR/otherllav700.grb.f${fhour} \
          $TMPDIR/otherllu10m.grb.f${fhour} \
          >>${vdir}/otherlatlon.pgrb.${symdh}

    done

  fi

  $gix ${vdir}/otherlatlon.pgrb.${symdh} ${vdir}/otherlatlon.pgrb.ix.${symdh}
  gribfile=${vdir}/otherlatlon.pgrb.${symdh}
  ixfile=${vdir}/otherlatlon.pgrb.ix.${symdh}

fi


#-------------------------------------------#
#   Now qsub the jobs to run the tracker    #
#-------------------------------------------#

ist=1
while [ $ist -le 15 ]
do
  if [ ${stormflag[${ist}]} -ne 1 ]
  then
    set +x; echo "Storm number $ist NOT selected for processing"; set -x
  else
    set +x; echo "Storm number $ist IS selected for processing...."; set -x
  fi
  let ist=ist+1
done

# Load the forecast hours for this particular model into an array
# that will be passed into the executable via a namelist....

ifh=1
while [ $ifh -le ${maxtime} ]
do
  fh[${ifh}]=` echo ${fcsthrs} | awk '{print $n}' n=$ifh`
  let ifh=ifh+1
done

namelist=${vdir}/gettrk.input.${cmodel}.${symdh}
ATCFNAME=` echo "${atcfname}" | tr '[a-z]' '[A-Z]'`
  
echo "&datein inp%byy=${syy},inp%bmm=${smm},inp%bdd=${sdd},"    >${namelist}
echo "        inp%bhh=${shh}, inp%model=${model}/"             >>${namelist}
echo "&stormlist stswitch = ${stormflag[1]},${stormflag[2]},"  >>${namelist}
echo "      ${stormflag[3]},${stormflag[4]},${stormflag[5]},"  >>${namelist}
echo "      ${stormflag[6]},${stormflag[7]},${stormflag[8]},"  >>${namelist}
echo "    ${stormflag[9]},${stormflag[10]},${stormflag[11]},"  >>${namelist}
echo "   ${stormflag[12]},${stormflag[13]},${stormflag[14]},"  >>${namelist}
echo "   ${stormflag[15]}/"                                    >>${namelist}
echo "&fhlist itmphrs = ${fh[1]},${fh[2]},${fh[3]},"           >>${namelist}
echo "      ${fh[4]},${fh[5]},${fh[6]},${fh[7]},"              >>${namelist}
echo "      ${fh[8]},${fh[9]},${fh[10]},${fh[11]},"            >>${namelist}
echo "      ${fh[12]},${fh[13]},${fh[14]},"                    >>${namelist}
echo "      ${fh[15]},${fh[16]},${fh[17]},"                    >>${namelist}
echo "      ${fh[18]},${fh[19]},${fh[20]},"                    >>${namelist}
echo "      ${fh[21]},${fh[22]}/"                              >>${namelist}
echo "&atcfinfo atcfnum=${atcfnum},atcfname='${ATCFNAME}'/"    >>${namelist}
  
pgm=`basename  $GETTX`
if [ -s $DATA/prep_step ]; then
   . $DATA/prep_step
else
   [ -f errfile ] && rm errfile
   export XLFUNITS=0
   unset `env | grep XLFUNIT | awk -F= '{print $1}'`

   set +u
   if [ -z "$XLFRTEOPTS" ]; then
     export XLFRTEOPTS="unit_vars=yes"
   else
     export XLFRTEOPTS="${XLFRTEOPTS}:unit_vars=yes"
   fi
   set -u

fi

# can't use XLFUNIT_?? with baopen!
#----------------------------------
#export XLFUNIT_11=${gribfile}
ln -s -f ${gribfile}                                   fort.11
export XLFUNIT_12=${vdir}/vitals.upd.${cmodel}.${symd}${dishh}
#export XLFUNIT_31=${ixfile}
ln -s -f ${ixfile}                                     fort.31
export XLFUNIT_61=${vdir}/trak.${cmodel}.all.${symdh}
export XLFUNIT_62=${vdir}/trak.${cmodel}.atcf.${symdh}
export XLFUNIT_63=${vdir}/trak.${cmodel}.radii.${symdh}
export XLFUNIT_64=${vdir}/trak.${cmodel}.atcfunix.${symdh}

$TIMEIT $GETTX <${namelist} > outout 2> errfile
err=$?
###cat errfile
cat errfile >> outout
cat outout > ${DATA}/model_track.out
set +u
[ -n "../$pgmout" ]  &&  cat outout >> ../$pgmout
set -u
rm outout
set +x
echo
echo 'The foreground exit status for GETTRK is ' $err
echo
set -x

if [ -s $DATA/err_chk ]; then
   $DATA/err_chk
else
   if test "$err" -gt '0'
   then
######kill -9 ${qid}
      exit 555
   fi
fi
[ "$err" -gt '0' ]  &&  exit 9

rm fort.11 fort.31

cp ${vdir}/trak.${cmodel}.all.${symdh}   ${DATA}/model_track.all

exit 0

