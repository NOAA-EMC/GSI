#!/bin/ksh
################################################################################
# This script runs the post processor.
# Usage: post.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
################################################################################
set -ux

################################################################################
# Go configure

export CKSH=$(echo $CSTEP|cut -c-4)
export CKND=$(echo $CSTEP|cut -c5-)
set -a;. $CONFIG;set +a
eval export DATA=$DATATMP
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
#chgrp ${group_name:-rstprod} $DATA
chmod ${permission:-755} $DATA
#
export machine=${machine:-ZEUS}
export machine=$(echo $machine|tr '[a-z]' '[A-Z]')

export XLF_LINKSSH=${XLF_LINKSSH:-$NWPROD/util/ush/xlf_links.sh}
#export XLF_LINKSSH=${XLF_LINKSSH:-""}
export APRNSM=${APRNSM:-""}
#if [ $machine = ZEUS ] ; then
#  module load intel
#  module load mpt
#  APRNSM=""
#fi
#
export HOMEDIR=${HOMEDIR:-/nwprod}
export FIXDIR=${FIXDIR:-$HOMEDIR/fix/fix_am}
export FIXGLOBAL=${FIXGLOBAL:-$FIXDIR}
export SHDIR=${SHDIR:-$HOMEDIR/bin}
export USHDIR=${USHDIR:-$HOMEDIR/ush}
export NWPROD=${NWPROD:-$HOMEDIR}
#
export PBEG=${PBEG:-$SHDIR/pbeg}
export PEND=${PEND:-$SHDIR/pend}
export PERR=${PERR:-$SHDIR/perr}
$PBEG

################################################################################
# Set other variables

export PCOP=${PCOP:-$SHDIR/pcop}
export NCP=${NCP:-cp}
export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
export COPYGB=${COPYGB:-$NWPROD/util/exec/copygb}
export COPYGB2=${COPYGB2:-$NWPROD/util/exec/copygb2}
export CNVGRIB=${CNVGRIB:-$NWPROD/util/exec/cnvgrib21_gfs}
export GRIBVERSION=${GRIBVERSION:-'grib2'}
export precpgb=${precpgb:-""}
export WGRIB=${WGRIB:-$NWPROD/util/exec/wgrib}
export WGRIB2=${WGRIB2:-$NWPROD/util/exec/wgrib2}
export windex=${windex:-$NWPROD/util/exec/grbindex}
export NCEPPOST=${NCEPPOST:-YES}
export POSTD3D=${POSTD3D:-NO}
export POSTOCN=${POSTOCN:-NO}
export JUST_POST=${JUST_POST:-NO}
export in_o=${in_o:-0}             # interpolation option, defaults to 0 (bilinear)
#export pgbf_gfs=${pgbf_gfs:-3}     #resolution of gfs pgbf files saved in HPSS archive, 3-1x1,4-0.5x0.5
#export pgbf_gdas=${pgbf_gdas:-4}   #resolution of gdas pgbf files saved in HPSS archive
##NOTE: pgbf_grid and flag_pgb if-statement can be found in reconcile.sh

if [ $NCEPPOST = YES ] ; then
 if [ $GRIBVERSION = grib1 ]; then
  export POSTGPSH=${POSTGPSH_NP:-${POSTGPSH:-$POSTDIR/ush/global_nceppost.sh}}
  export POSTGPEXEC=${POSTGPEXEC_NP:-${POSTGPEXEC:-$HOMEDIR/exec/ncep_post}}
  export PARM_CTL=${PARM_CTL:-$POSTDIR/parm}
  export CAT_FLX_TO_PGB=${CAT_FLX_TO_PGB:-YES}
  export OUTTYP=${OUTTYP:-1}
  export CHGRESEXEC=${CHGRESEXEC:-$HOMEDIR/exec/global_chgres}
  export SIGHDR=${SIGHDR:-$NWPROD/exec/global_sighdr}
  export USHGLOBAL=${USHGLOBAL:-$USHDIR}
 else
  export POSTGPSH=${POSTGPSH_NP:-${POSTGPSH:-$POSTDIR/ush/global_nceppost_grb2.sh}}
  export POSTGPEXEC=${POSTGPEXEC_NP:-${POSTGPEXEC:-$HOMEDIR/exec/ncep_post}}
  export CAT_FLX_TO_PGB=${CAT_FLX_TO_PGB:-YES}
  export OUTTYP=${OUTTYP:-3}
  export CHGRESEXEC=${CHGRESEXEC:-$HOMEDIR/exec/global_chgres}
  export SIGHDR=${SIGHDR:-$NWPROD/exec/global_sighdr}
  export PARM_CTL=${PARM_CTL:-$POSTDIR/parm}
  export POSTAVBLFLD=${PARM_CTL}/post_avblflds.xml
  export POSTGRB2TBL=${POSTGRB2TBL:-/nwprod/lib/sorc/g2tmpl/params_grib2_tbl_new}
  export CTLFILEGFSANL=${PARM_CTL}/postcntrl_gfs_anl.xml
  export CTLFILEGFS=${PARM_CTL}/postcntrl_gfs.xml
  export CTLFILEGFS_f00=${PARM_CTL}/postcntrl_gfs_f00.xml
  export CTLFILEGOES=${PARM_CTL}/postcntrl_gfs_goes.xml
  #export PARM_SIB=$USHDIR/../parms/parm_sib
  export PARM_SIB=${PARM_SIB:-$USHDIR/../parms/parm_sib}
 fi
else
 export POSTGPSH=${POSTGPSH_GP:-${POSTGPSH:-$USHDIR/global_postgp.sh}}
 export POSTGPEXEC=${POSTGPEXEC_GP:-${POSTGPEXEC:-$HOMEDIR/exec/global_postgp}}
 export OUTTYP=${OUTTYP:-2}
fi

export LINKPOSTFILESH=${LINKPOSTFILESH:-""}

export POSTMDLSH=${POSTMDLSH:-$USHDIR/post_mdl.sh}

#export MOM4POSTSH=${MOM4POSTSH:-$USHDIR/mom4_post.sh}
export COUP_FCST=${COUP_FCST:-NO}
#    For making time average of files
export EXTERNAL_AVG=${EXTERNAL_AVG:-NO}
export EXTERNALSH=${EXTERNALSH:-""}
export AVG_FCST=${AVG_FCST:-NO}
export AVG_SUB=${AVG_SUB:-YES}
export AVGSUBSH=${AVGSUBSH:-$SHDIR/pavg}
export TSER_FCST=${TSER_FCST:-NO}
# AVG_INT is used for both averaging and time-series extraction
if [ $AVG_FCST = YES -o $TSER_FCST = YES ] ; then export AVG_INT=${AVG_INT:-999} ; fi
export FCST_TIMEMEANSH=${FCST_TIMEMEANSH:-$USHDIR/fcst_timemean.sh}
export TMEANDIR=${TMEANDIR:-${MEANDIR:-$COMROT}}
export INDXDIR=${INDXDIR:-$DATA/index}
#    For extracting time series of selected variables
export TIMEDIR=${TIMEDIR:-$COMROT}
export TSER_SUB=${TSER_SUB:-YES}
export TSERSUBSH=${TSERSUBSH:-$SHDIR/ptsr}
export FCST_TSERSH=${FCST_TSERSH:-$USHDIR/fcst_timeser.sh}

export SMARTPRECIPSH=${SMARTPRECIPSH:-$HOMEDIR/ush/gfs_smart_precip.sh}
#export GFS_SMARTMAKEPRECIPEXEC=${GFS_SMARTMAKEPRECIPEXEC:-$HOMEDIR/exec/gfs_smartmakeprecip}

#
export CCPOST=${CCPOST:-NO}
export GGPOST=${GGPOST:-NO}
export FFPOST=${FFPOST:-YES}
export CDFNL=${CDFNL:-fnl}
export GDUMP=${GDUMP:-$CDFNL}
cycle=$(echo $CDATE|cut -c9-10)
cdump=$(echo $CDUMP|tr '[a-z]' '[A-Z]')
eval MANAL=\${MANAL$cycle$cdump:-1}
export MANAL
#
export CYINC=${CYINC:-06}
export GDATE=$($NDATE $CYINC $CDATE)
cyclen=$(echo $GDATE|cut -c9-10)
eval mlanl=\${MLANL$cyclen$cdump:-0}
export mlanl
#
eval mfcst=\${MFCST$cycle$cdump:-1}
export nknd=${CKND:-1}
export JCAP=$(eval echo \${JCAPFCST$cycle$cdump:-$JCAP}|cut -f$nknd -d,)
export FHINI=$(eval echo \${FHINIFCST$cycle$cdump:-0}|cut -f$nknd -d,)
export FHMAX=$(eval echo \${FHMAXFCST$cycle$cdump:-9}|cut -f$nknd -d,)
export FHOUT=$(eval echo \${FHOUTFCST$cycle$cdump:-3}|cut -f$nknd -d,)
export FHBAK=$(eval echo \${FHBAKFCST$cycle$cdump:-00}|cut -f$nknd -d,)
export LONB=$(eval echo \${LONBFCST$cycle$cdump:-3}|cut -f$nknd -d,)
export LATB=$(eval echo \${LATBFCST$cycle$cdump:-3}|cut -f$nknd -d,)
export GRID_ID11=$(eval echo \${GRID11FCST$cycle$cdump:-0}|cut -f$nknd -d,)
export GRID_ID25=$(eval echo \${GRID25FCST$cycle$cdump:-0}|cut -f$nknd -d,)
export GRID_ID62=$(eval echo \${GRID62FCST$cycle$cdump:-0}|cut -f$nknd -d,)
if [ $FHBAK -eq 0 ] ; then export FHINI=$FHBAK ; fi
export IO=$(eval echo \${IOPOST$cycle$cdump:-${IO:-360}}|cut -f$nknd -d,)
export JO=$(eval echo \${JOPOST$cycle$cdump:-${JO:-181}}|cut -f$nknd -d,)
export KO=$(eval echo \${KOPOST$cycle$cdump:-${KO:-26}}|cut -f$nknd -d,)
#if [ $NCEPPOST = YES ] ; then
export KTO=$(eval echo \${KTOPOST$cycle$cdump:-${KTO:-16}}|cut -f$nknd -d,)
export FH_STRT_POST=${FH_STRT_POST:-99999}
export FH_END_POST=${FH_END_POST:-99999}
export REDO_POST=${REDO_POST:-NO}
export JUST_AVG=${JUST_AVG:-NO}
export REDO_AVG=${REDO_AVG:-NO}
export JUST_TSER=${JUST_TSER:-NO}
export REDO_TSER=${REDO_TSER:-NO}
omres=${omres:-1x1}
outres=${outres:-1x1}
#else
#export KTO=0
#fi
if [ $GRIBVERSION = grib1 ]; then
 export IGEN_GFS=${IGEN_GFS:-81}
 export IGEN_ANL=${IGEN_ANL:-82}
 export IGEN_FCST=${IGEN_FCST:-96}
 export IGEN_GDAS_ANL=${IGEN_GDAS_ANL:-82}
elif [ $GRIBVERSION = grib2 ]; then
 export IGEN_GFS="gfs_avn"
 export IGEN_ANL="anal_gfs"
 export IGEN_FCST="gfs_avn"
 export IGEN_GDAS_ANL="anal_gdas"
fi
export VERBOSE=YES
export SIGIND=${SIGIND:-$COMROT}
export FLXIND=${FLXIND:-$COMROT}
export D3DIND=${D3DIND:-$COMROT}
export COMOUT=$COMROT

# Set dump prefix
PREFIX=$CDUMP
if [ $CDUMP = gdas ] ; then
   PREFIX=gdas1
fi

#
if [ $nknd -gt 1 -a $FHBAK -eq 0 ] ; then
 export SUFOUT=.${CDUMP}$nknd.$CDATE
else
 export SUFOUT=.$CDUMP.$CDATE
fi
#
#   Do the 2d fields at the analysis time
#
#if [ $nknd = 1 ] ; then
#export VDATE=$CDATE
#$POSTMDLSH anl $SUFOUT
#fi
#

#                    To start post from the middle of a run
#                    --------------------------------------
if [ $FH_STRT_POST -ne 99999 ] ; then
 FHINI=$FH_STRT_POST
elif [ -s $COMROT/FHREST.$CDUMP.$CDATE.$nknd ] ; then
 read FHINI < $COMROT/FHREST.$CDUMP.$CDATE.$nknd
fi
if [ $FH_END_POST -ne 99999 ] ; then
 FHEND=$FH_END_POST
else
 FHEND=$FHMAX
fi
if [ $FHINI -gt $FHEND ] ; then
 echo ' FHINI > FHEND Post processing stopped, FHINI= '$FHINI', FHEND= '$FHEND
fi
#
#
#
if [ $AVG_FCST = YES -o $TSER_FCST = YES ] ; then
  xdate=$($NDATE $FHINI $CDATE)
  if [ $AVG_INT -eq 999 ] ; then              # Monthly mean
    ydate=$(echo $xdate | cut -c1-6)0100
    if [ $ydate -gt $CDATE ] ; then
      PDATE=$ydate
    else
      PDATE=$CDATE
    fi
  else
    ydate=$CDATE
    until  [[ $ydate -gt $xdate ]] ; do
      PDATE=$ydate
      ydate=$($NDATE $AVG_INT $PDATE)
    done
  fi
fi
#
#
#   Some default output pressure levels
#
polist_47d="1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1."
#
polist_46d="1000.,975.,950.,925.,900.,875.,850.,825.,800.,750.,700., 650.,600.,550.,500.,450.,400.,350.,300.,250.,200.,150.,100.,70.,50.,40.,30.,20.,10.,7.,5.,4.,3.,2.,1.,0.7,0.5,0.4,0.3,0.2,0.1,0.07,0.05,0.04,0.03,0.02"
#
polist_31d="1000.,975.,950.,925.,900.,850.,800.,750.,700.,650.,600.,550.,500.,450.,400.,350.,300.,250.,200.,150.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1."
#
polist_37d="1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,700.,650.,600.,550.,500.,450.,400.,350.,300.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1."
#
polist_26d="1000.,975.,950.,925.,900.,850.,800.,750.,700.,650.,600.,550.,500.,450.,400.,350.,300.,250.,200.,150.,100.,70.,50.,30.,20.,10.,"
#
thlist_16d="270.,280.,290.,300.,310.,320.,330.,350.,400.,450.,550.,650.,850.,1000.,1250.,1500."
#

polist_47=${polist_47:-$polist_47d}
polist_46=${polist_46:-$polist_46d}
polist_31=${polist_31:-$polist_31d}
polist_37=${polist_37:-$polist_37d}
polist_26=${polist_26:-$polist_26d}
thlist_16=${thlist_16:-$thlist_16d}

#
if [[ $CCPOST = YES ]];then
  CSTEP0=$CSTEP
# eval CSTEP=fcst$CKND datafcst=$DATATMP
  eval CSTEP=fcst$CKND datafcst=$SIGIND
  CSTEP=$CSTEP0
# tsleep=$((JCAP/FHOUT/6*15+15))
  tsleep=10
  msleep=480
else
# datafcst=$DATA
  datafcst=$SIGIND
fi
#
# IDRT_NP is idrt for nceppost & GRID_ID is the corresponding NCEP grid
# GRID_ID25 and GRID_ID62 defaults to 0
#
export flag=${pgbres_flag:-m}

if [ $NCEPPOST = YES ] ; then
  export IDRT=${IDRT_NP:-0}         # defaults to lat/lon (for gfsio file)
  if [ $IDRT -eq 0 ] ; then
   export LONB=$IO
   export LATB=$JO
   if [ $IO -eq 2880 ] ; then         # 1/8th degree output
    export GRID_ID=${GRID_ID:-\"'255 0 2880 1441 90000 0 128 -90000 359875 125 125 0'\"}
    export flag=e
   elif [ $IO -eq 1440 ] ; then
    export GRID_ID=${GRID_ID:-193}    # defaults to lat/lon grid of 0.25 degree
    export flag=q
   elif [ $IO -eq 720 ] ; then
    export GRID_ID=${GRID_ID:-4}      # defaults to lat/lon grid of 0.5 degree
    export flag=h
   elif [ $IO -eq 360 ] ; then
    export GRID_ID=${GRID_ID:-3}      # defaults to lat/lon grid of 1.0 degree
    export flag=f
   elif [ $IO -eq 144 ] ; then
    export GRID_ID=${GRID_ID:-2}      # defaults to lat/lon grid of 2.5 degree
    export flag=l
   fi
  else
   LONB_NP=${LONB_NP:-0}
   if [ $LONB_NP -gt 0 ] ; then
    export LONB=$LONB_NP
    export LATB=$LATB_NP
    export flag=m
    export GRID_ID=${GRID_ID:-0}
    if [ $GRID_ID -le 0 ] ; then
     echo 'APPROPRIATE GRID_ID needs to be specified when LONB_NP > 0'
     exit
    fi 
   else
    export GRID_ID=0
   fi
  fi
# export GRID_ID25=${GRID_ID25:-2}  # defaults to lat/lon grid of 2.5 degree
# export GRID_ID62=${GRID_ID62:-98} # defaults to Gaussian grid of T62
#
  POSTGPVARSTMP=$(eval echo \${POSTGPVARSNP_$CDUMP:-""})
  if [ $KTO -eq 0 ] ; then
    export  POSTGPVARS=${POSTGPVARSTMP:-"KPO=$KO,PO=$(eval echo \${polist_$KO}),KTH=0,"}
#   export  POSTGPVARS=${POSTGPVARSTMP:-"KPO=$KO,PO=$(eval echo \${polist_$KO}),kpv=2,pv=-2.,2.,KTH=0,"}
  else
    export  POSTGPVARS=${POSTGPVARSTMP:-"KPO=$KO,PO=$(eval echo \${polist_$KO}),KTH=$KTO,TH=$(eval echo \${thlist_$KTO}),"}
#   export  POSTGPVARS=${POSTGPVARSTMP:-"KPO=$KO,PO=$(eval echo \${polist_$KO}),kpv=2,pv=-2.,2.,KTH=$KTO,TH=$(eval echo \${thlist_$KTO}),"}
  fi
else              # Old global_postgp case
  if [ $IO -eq 720 ] ; then
   export GRID_ID=${GRID_ID:-4}      # defaults to lat/lon grid of 0.5 degree
   export flag=h
  elif [ $IO -eq 360 ] ; then
   export GRID_ID=${GRID_ID:-3}      # defaults to lat/lon grid of 1.0 degree
   export flag=f
  else
   GRID_ID=${GRID_ID:-0}
  fi
# export POSTGPVARS="IDRTC=0,IOC=$IO,JOC=$JO,IDRT=0,IO=$IO,JO=$JO,MOO=48,MOW=12,MOOA=48,MOWA=12,pob(051)=1000,pob(154)=1000,pot(153)=0,pot(051)=0,"
  if [ $KO -ne 26 ] ; then
   if [ $KTO -eq 0 ] ; then
    export  POSTGPVARS=${POSTGPVARSGP:-"IOC=$IO,JOC=$JO,KPO=$KO,PO=$(eval echo \${polist_$KO}),POB(51)=1000,pob(154)=1000,pot(153)=0,pot(051)=0,KZZ=8,ZZ=305.,457.,610.,914.,1829.,2743.,3658.,4572.,KPTO=6,KTH=0,"}
   else
    export  POSTGPVARS=${POSTGPVARSGP:-"IOC=$IO,JOC=$JO,KPO=$KO,PO=$(eval echo \${polist_$KO}),POB(51)=1000,pob(154)=1000,pot(153)=0,pot(051)=0,KZZ=8,ZZ=305.,457.,610.,914.,1829.,2743.,3658.,4572.,KPTO=6,KTH=$KTO,TH=$(eval echo \${thlist_$KTO}),"}
   fi
  else
   export POSTGPVARS=${POSTGPVARSGP:-"IDRTC=0,IOC=$IO,JOC=$JO,IDRT=0,IO=$IO,JO=$JO,pob(051)=1000,pob(154)=1000,pot(153)=0,pot(051)=0,"}
  fi
fi
#

################################################################################
# Copy in restart and input files (disabled here)

#$PCOP $CDATE/$CDUMP/$CSTEP/ROTI $COMROT $DATA <$RLIST

################################################################################

# Post analysis guess
# -------------------

if [ $FHINI -eq 0 -a -s $SIGIND/siganl$SUFOUT ] ; then
 if [ $NCEPPOST = YES ] ; then
   if [ ${POSTSPL:-YES} = YES ] ; then
     export VDATE=$CDATE
     $POSTMDLSH anl $SUFOUT
   fi
   if [ $GRIBVERSION = grib1 ]; then
    export CTLFILE=${CTL_ANL:-$PARM_AM/am_cntrl.parm_anl}
   elif [ $GRIBVERSION = grib2 ]; then
    export CTLFILE1=${CTLFILEGFSANL:-$PARM_CTL/postcntrl_gfs_anl.xml}
    if [[ $CDUMP = gfs ]] ; then
       sed <$CTLFILE1 -e "s#${IGEN_ANL}#${IGEN_ANL}#" > ./ctlfile_anl
       export CTLFILE=./ctlfile_anl
    else
       sed <$CTLFILE1 -e "s#${IGEN_ANL}#${IGEN_GDAS_ANL}#" > ./ctlfile_anl
       export CTLFILE=./ctlfile_anl
    fi
   fi
    
 fi
 if [[ $nknd -eq 1 && $MANAL = 1 ]] ; then
   if [[ $GGPOST = YES ]];then
     if [[ $CDUMP = gfs ]] ; then
       export IGEN=$IGEN_GFS
     else
       export IGEN=$IGEN_GDAS_ANL
     fi
#    export POSTGPLIST=$FIXGLOBAL/global_kplist.1d.txt

     export VDATE=$($NDATE -3 $CDATE)
     export SIGINP=$SIGIND/siggm3.$CDUMP.$CDATE
     export FLXINP=NULL
     export PGBOUT1=$COMOUT/pgbgm3.$CDUMP.$CDATE
     export PGBOUT=$COMOUT/pgb${flag}m3.$CDUMP.$CDATE
     if [ $GRIBVERSION = grib2 ]; then
       export PGBOUT2=$COMOUT/pgb${flag}m3.$CDUMP.$CDATE.grib2
     fi
     rm $PGBOUT
     if [ $NCEPPOST = YES -a $KTO -gt 0 ] ; then
       export IPVOUT1=$COMOUT/ipvgm3.$CDUMP.$CDATE
       export IPVOUT=$COMOUT/ipv${flag}m3.$CDUMP.$CDATE
       rm $IPVOUT
     fi

     $POSTGPSH

     if [ $NCEPPOST = YES ] ; then
       if [ $GRID_ID -eq $pgbf_grid ] ; then
         mv $PGBOUT $PGBOUT1
         if [ $KTO -gt 0 ] ; then mv $IPVOUT $IPVOUT1 ; fi
       else 
         ${precpgb}$COPYGB -g$pgbf_grid -i$in_o -x $PGBOUT $PGBOUT1
         if [ $KTO -gt 0 ] ; then ${precpgb}$COPYGB -g$pgbf_grid -i$in_o -x $IPVOUT $IPVOUT1 ; fi
       fi
     elif [ $IO -gt 360 ] ; then
       ${precpgb}$COPYGB -g3 -i$in_o -x $PGBOUT $PGBOUT1
     else
       mv $PGBOUT $PGBOUT1
     fi

     export VDATE=$CDATE

     export SIGINP=$SIGIND/sigges.$CDUMP.$CDATE
     export FLXINP=NULL
     export PGBOUT1=$COMOUT/pgbges.$CDUMP.$CDATE
     export PGBOUT=$COMOUT/pgb${flag}es.$CDUMP.$CDATE
     if [ $GRIBVERSION = grib2 ]; then
       export PGBOUT2=$COMOUT/pgb${flag}es.$CDUMP.$CDATE.grib2
     fi
     rm $PGBOUT
     if [ $NCEPPOST = YES -a $KTO -gt 0 ] ; then
       export IPVOUT1=$COMOUT/ipvges.$CDUMP.$CDATE
       export IPVOUT=$COMOUT/ipv${flag}es.$CDUMP.$CDATE
       rm $IPVOUT
     fi
 
     $POSTGPSH

     if [ $NCEPPOST = YES ] ; then
       if [ $GRID_ID -eq $pgbf_grid ] ; then
         mv $PGBOUT $PGBOUT1
         if [ $KTO -gt 0 ] ; then mv $IPVOUT $IPVOUT1 ; fi
       else
         ${precpgb}$COPYGB -g$pgbf_grid -i$in_o -x $PGBOUT $PGBOUT1
         if [ $KTO -gt 0 ] ; then ${precpgb}$COPYGB -g$pgbf_grid -i$in_o -x $IPVOUT $IPVOUT1 ; fi
       fi
     elif [ $IO -gt 360 ] ; then
       ${precpgb}$COPYGB -g3 -i$in_o -x $PGBOUT $PGBOUT1
     else
       mv $PGBOUT $PGBOUT1
     fi

     export VDATE=$($NDATE +3 $CDATE)
     export SIGINP=$SIGIND/siggp3.$CDUMP.$CDATE
     export FLXINP=NULL
     export PGBOUT1=$COMOUT/pgbgp3.$CDUMP.$CDATE
     export PGBOUT=$COMOUT/pgb${flag}p3.$CDUMP.$CDATE
     if [ $GRIBVERSION = grib2 ]; then
       export PGBOUT2=$COMOUT/pgb${flag}p3.$CDUMP.$CDATE.grib2
     fi 
     rm $PGBOUT
     if [ $NCEPPOST = YES -a $KTO -gt 0 ] ; then
       export IPVOUT1=$COMOUT/ipvgp3.$CDUMP.$CDATE
       export IPVOUT=$COMOUT/ipv${flag}p3.$CDUMP.$CDATE
       rm $IPVOUT
     fi

     $POSTGPSH

     if [ $NCEPPOST = YES ] ; then
       if [ $GRID_ID -eq $pgbf_grid ] ; then
         mv $PGBOUT $PGBOUT1
         if [ $KTO -gt 0 ] ; then mv $IPVOUT $IPVOUT1 ; fi
       else
         ${precpgb}$COPYGB -g$pgbf_grid -i$in_o -x $PGBOUT $PGBOUT1
         if [ $KTO -gt 0 ] ; then ${precpgb}$COPYGB -g$pgbf_grid -i$in_o -x $IPVOUT $IPVOUT1 ; fi
       fi
     elif [ $IO -gt 360 ] ; then
       ${precpgb}$COPYGB -g3 -i$in_o -x $PGBOUT $PGBOUT1
     else
       mv $PGBOUT $PGBOUT1
     fi
   fi
  fi

# Post analysis file
# -------------------

  export VDATE=$CDATE
  export SIGINP=$SIGIND/siganl$SUFOUT
  export SFCINPUT=$SIGIND/sfcanl$SUFOUT
  export FLXINP=/dev/null
  export PGBOUT=$COMOUT/pgb${flag}nl$SUFOUT
  export PGBOUT1=$COMOUT/pgb${flag_pgb}nl$SUFOUT
  if [ $GRIBVERSION = grib2 ]; then
    export PGBOUT2=$COMOUT/pgb${flag}nl$SUFOUT.grib2
  fi
  if [ $pgbf_grid -eq 3 ] ; then export PGBOUT1=$COMOUT/pgbanl$SUFOUT ; fi
# export POSTGPLIST=$FIXGLOBAL/global_kplist.1d.txt
  if [[ $CDUMP = gfs ]] ; then
    export IGEN=$IGEN_GFS
  else
    export IGEN=$IGEN_GDAS_ANL
  fi
  rm $PGBOUT
  if [ $NCEPPOST = YES -a $KTO -gt 0 ] ; then
    export IPVOUT=$COMOUT/ipv${flag}nl$SUFOUT
    export IPVOUT1=$COMOUT/ipv${flag_pgb}nl$SUFOUT
    if [ $pgbf_grid -eq 3 ] ; then export IPVOUT1=$COMOUT/ipvanl$SUFOUT ; fi
    rm $IPVOUT
  fi

  $POSTGPSH
# post will now generate Grib2, however, we're still waiting for Sib
# to modifying GFS downstream to ingest Grib2, convert back to Grib1 
# for now  
  if [ $GRIBVERSION = grib2 ]; then
    mv $PGBOUT $PGBOUT2
  fi


  rc=$?
  if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

  if [ $GRIBVERSION = grib2 ]; then
    export FH=-1 #Set to negative for analysis
    if [ $GFS_DOWNSTREAM = 'YES' ] ; then
      $GFSDOWNSH
    fi
  else # for Grib1

   if [ $NCEPPOST = YES ] ; then
     if [ $GRID_ID -eq $pgbf_grid ] ; then
       mv $PGBOUT $PGBOUT1
       if [ $KTO -gt 0 ] ; then mv $IPVOUT $IPVOUT1 ; fi
     else
       ${precpgb}$COPYGB -g$pgbf_grid -i$in_o -x $PGBOUT $PGBOUT1
       if [ $KTO -gt 0 ] ; then
         ${precpgb}$COPYGB -g$pgbf_grid -i$in_o -x $IPVOUT $IPVOUT1
       fi
     fi
   elif [ $IO -gt 360 ] ; then
     ${precpgb}$COPYGB -g3 -i$in_o -x $PGBOUT $PGBOUT1
   else
     mv $PGBOUT $PGBOUT1
   fi
   if [ $GRID_ID11 -gt 0 ] ; then
     if [ $PGBOUT1 != $COMOUT/pgbanl$SUFOUT ] ; then
       ${precpgb}$COPYGB -g3 -i$in_o -x $PGBOUT1 $COMOUT/pgbanl$SUFOUT
       if [ $NCEPPOST = YES -a $KTO -gt 0 ] ; then
         ${precpgb}$COPYGB -g3 -i$in_o -x $IPVOUT1 $COMOUT/ipvanl$SUFOUT
       fi
     fi
   fi
   if [ $GRID_ID25 -gt 0 ] ; then
     if [ $GRID_ID -ne 3  -o $IO -gt 360 ] ; then
       pgbfile=$PGBOUT
       if [ $NCEPPOST = YES -a $KTO -gt 0 ] ; then ipvfile=$IPVOUT  ; fi
     else
       pgbfile=$PGBOUT1
       if [ $NCEPPOST = YES -a $KTO -gt 0 ] ; then ipvfile=$IPVOUT1 ; fi
     fi
#   ${precpgb}$COPYGB -g$GRID_ID25 -i$in_o -x $pgbfile $COMOUT/pgblnl${SUFOUT}
     rm pgb25a pgb25b

#   $WGRIB $pgbfile|grep -i :kpds6=100:|${precpgb}$COPYGB -xkw -i4,0,35 -g$GRID_ID25 $pgbfile pgb25a
#   $WGRIB $pgbfile|grep -vi :kpds6=100:|${precpgb}$COPYGB -xkw -i$in_o -g$GRID_ID25 $pgbfile pgb25b

     rm xxx yyy
     $WGRIB $pgbfile|grep -i :kpds6=100: > xxx
     ${precpgb}$COPYGB -xkw -i4,0,35 -g$GRID_ID25 $pgbfile pgb25a < xxx
     $WGRIB $pgbfile|grep -vi :kpds6=100: > yyy
     ${precpgb}$COPYGB -xkw -i$in_o -g$GRID_ID25 $pgbfile pgb25b < yyy

     cat pgb25a pgb25b > $COMOUT/pgblnl${SUFOUT}
     if [ $NCEPPOST = YES -a $KTO -gt 0 ] ; then ${precpgb}$COPYGB -g$GRID_ID25 -i$in_o -x $ipvfile $COMOUT/ipvlnl${SUFOUT} ; fi
   fi
  fi
# cp $PGBOUT1 $COMOUT/pgb${flag_pgb}nl$SUFOUT
fi

if [ $FHMAX -eq 0 ] ; then $PEND ; exit ; fi

################################################################################

# Post forecast files
# -------------------

export FHOUTPGB=$FHOUT
if [ $cdump = GFS ]; then 
 export FHOUTPGB=$(eval echo \${foutpgb$CKND:-$FHOUT})
fi

if [[ $nknd -le $mfcst ]];then
  if [[ 10#$FHINI -eq 0 ]] ; then
    export FH=-$FHOUTPGB
  else
    export FH=$(((10#$FHINI/10#$FHOUTPGB)*10#$FHOUTPGB))
#   FH=$FHINI
  fi
  if [ $NCEPPOST = YES ] ; then
    if [ $POSTD3D = YES ] ; then
     export CTLFILE=${CTL_FCS:-$PARM_AM/am_cntrl.parm}
    else
     if [ $GRIBVERSION = grib1 ]; then
      export CTLFILE=${CTL_FCS:-$PARM_AM/am_cntrl.parm_pgb}
     else
      export CTLFILE=${CTLFILEGFS:-$PARM_CTL/postcntrl_gfs.xml}
     fi
    fi
  fi
  until [[ $((FH=10#$FH+10#$FHOUTPGB)) -gt $FHEND ]];do [[ $FH -lt 10 ]]&&export FH=0$FH
    export VDATE=$($NDATE $FH $CDATE)
    if [ ! -s $COMOUT/pgb${flag_pgb}${FH}${SUFOUT} -o $REDO_POST = YES -a $JUST_AVG = NO ] ; then
      if [[ $CCPOST = YES ]];then
        export LOGINP=$datafcst/logf${FH}${SUFOUT}
        export SIGINP=$datafcst/sigf${FH}${SUFOUT}
        export SFCINPUT=$datafcst/sfcf${FH}${SUFOUT}

        nsleep=0
        until [[ -s $LOGINP || $((nsleep+=1)) -gt $msleep ]];do sleep $tsleep;done

        ls -l $LOGINP
        if [[ $nsleep -gt $msleep ]];then $PERR;exit 2;fi

        if [ $NCEPPOST = YES ] ; then
          if [ $IDRT -eq 0  -o $GRID_ID -gt 0 ] ; then
            rm flxfile

            if [ $CDUMP = gdas -a ${DO_FLX_INDX:-YES} = YES ] ; then
              $windex $datafcst/flxf${FH}${SUFOUT} $datafcst/flxf${FH}${SUFOUT}.index
            fi
            ${precpgb}$COPYGB -g$GRID_ID -i$in_o -x $datafcst/flxf${FH}${SUFOUT} flxfile
            export FLXINP=flxfile
            if [ $POSTD3D = YES ] ; then
             rm d3dfile
             ${precpgb}$COPYGB -g$GRID_ID -i$in_o -x $datafcst/d3df${FH}${SUFOUT} d3dfile
             export D3DINP=d3dfile
            fi
          elif [ $IDRT -eq 4 ] ; then
            export FLXINP=$datafcst/flxf${FH}${SUFOUT}
            if [ $POSTD3D = YES ] ; then
              export D3DINP=$datafcst/d3df${FH}${SUFOUT}
            fi
          else
            echo 'INVALID IDRT has value  '$IDRT
            exit
          fi
#
          if [ ${POSTSPL:-YES} = YES ] ; then
            if [ $nknd = 1 -a $FH -eq 06 ] ; then
             $POSTMDLSH f$FH $SUFOUT
            fi
          fi
#
        else
          export FLXINP=$datafcst/flxf${FH}${SUFOUT}
        fi
      else
        export SIGINP=$SIGIND/sigf${FH}${SUFOUT}
        export SFCINPUT=$SIGIND/sfcf${FH}${SUFOUT}
        nsleep=0
        until [[ -s $SIGINP || $((nsleep+=1)) -gt $msleep ]];do sleep $tsleep;done
        nsleep=0
        until [[ -s $FLXIND/flxf${FH}${SUFOUT} || $((nsleep+=1)) -gt $msleep ]];do sleep $tsleep;done

        if [ $NCEPPOST = YES ] ; then
          if [ $IDRT -eq 0  -o $GRID_ID -gt 0 ] ; then
            rm flxfile
            ${precpgb}$COPYGB -g$GRID_ID -i$in_o -x $FLXIND/flxf${FH}${SUFOUT} flxfile
            export FLXINP=flxfile
            if [ $POSTD3D = YES ] ; then
             rm d3dfile
             ${precpgb}$COPYGB -g$GRID_ID -i$in_o -x $D3DIND/d3df${FH}${SUFOUT} d3dfile
             export D3DINP=d3dfile
            fi
          elif [ $IDRT -eq 4 ] ; then
            export FLXINP=$FLXIND/flxf${FH}${SUFOUT}
            if [ $POSTD3D = YES ] ; then
             export D3DINP=$D3DIND/d3df${FH}${SUFOUT}
            fi
          else
            echo 'INVALID IDRT has value  '$IDRT
            exit
          fi
#
          if [ ${POSTSPL:-YES} = YES ] ; then
            if [ $nknd = 1 -a $FH -eq 06 ] ; then
             $POSTMDLSH f$FH $SUFOUT
            fi
          fi
#
        else
          export FLXINP=$FLXIND/flxf${FH}${SUFOUT}
        fi
      fi
      if [[ $FFPOST = YES || -s $SIGINP ]];then
        export PGBOUT1=$COMOUT/pgb${flag_pgb}${FH}${SUFOUT}
        export PGBOUT=$COMOUT/pgb${flag}${FH}${SUFOUT}
        if [ $GRIBVERSION = grib2 ]; then
          export PGBOUT2=$COMOUT/pgb${flag}${FH}${SUFOUT}.grib2
        fi
        rm $PGBOUT
#       export POSTGPLIST=$FIXGLOBAL/global_kplist.1d.txt
        export IGEN=$IGEN_ANL
        if [ $CDUMP = gfs ] ; then
          export IGEN=$IGEN_GFS
          if [ $FH -gt 0 ] ; then export IGEN=$IGEN_FCST ; fi
        else
          export IGEN=$IGEN_GDAS_ANL
          if [ $FH -gt 0 ] ; then export IGEN=$IGEN_FCST ; fi
        fi
        if [ $NCEPPOST = YES -a $KTO -gt 0 ] ; then
          export IPVOUT1=$COMOUT/ipv${flag_pgb}${FH}${SUFOUT}
          export IPVOUT=$COMOUT/ipv${flag}${FH}${SUFOUT}
          rm $IPVOUT
          if [ $POSTD3D = YES ] ; then
            export D3DOUT1=$COMOUT/diab${flag_pgb}${FH}${SUFOUT}
            export D3DOUT=$COMOUT/diab${flag}${FH}${SUFOUT}
            rm $D3DOUT
          fi
        fi
# need to use process ANL ID for F00
        if [ $GRIBVERSION = grib2 ]; then
         if [[ $CDUMP = gfs ]] ; then
          if test $FH -eq 0
          then
           export CTLFILE1=${CTLFILEGFS_f00:-$PARM_CTL/postcntrl_gfs.xml}
           sed <$CTLFILE1 -e "s#${IGEN_FCST}#${IGEN_ANL}#" > ./ctlfile_f00
           export CTLFILE=./ctlfile_f00
          else
           export CTLFILE=${CTLFILEGFS:-$PARM_CTL/postcntrl_gfs.xml}
          fi
         else
	  if test $FH -eq 0
	  then
           export CTLFILE1=${CTLFILEGFS_f00:-$PARM_CTL/postcntrl_gfs.xml}
           sed <$CTLFILE1 -e "s#${IGEN_FCST}#${IGEN_GDAS_ANL}#" > ./ctlfile_f00
           export CTLFILE=./ctlfile_f00
	  else
	   export CTLFILE=${CTLFILEGFS:-$PARM_CTL/postcntrl_gfs.xml}
	  fi
         fi
        fi
        $POSTGPSH
# post will now generate Grib2
        if [ $GRIBVERSION = grib2 ]; then
          mv $PGBOUT $PGBOUT2
        fi

        rc=$?
        if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

# At forecast hour 192 create a 12hr accum precip
        if [ $FH -eq 192 -a ${DO_SMARTP:-NO} = YES ] ; then
#         echo "PROCESSED HOUR $FH"
#         FH6=$(($FH-6))
#         $windex ${COMROT}/pgb${flag}186.gfs.$CDATE pgb${flag}186.gfs.$CDATE.idx
#         $windex ${COMROT}/pgb${flag}192.gfs.$CDATE pgb${flag}192.gfs.$CDATE.idx
#         export XLFUNIT_13="${COMROT}/pgb${flag}186.gfs.$CDATE"
#         export XLFUNIT_14="pgb${flag}186.gfs.$CDATE.idx"
#         export XLFUNIT_15="${COMROT}/pgb${flag}192.gfs.$CDATE"
#         export XLFUNIT_16="pgb${flag}192.gfs.$CDATE.idx"
#         export XLFUNIT_50="12precip"
#         export XLFUNIT_51="12cprecip"
#         export XLFUNIT_52="12snow"
#         $XLF_LINKSSH
#         $APRNSM $GFS_SMARTMAKEPRECIPEXEC <<EOF > makeprecip12.out
#$FH $FH6
#EOF

##          /gpfs/t3/global/save/wx23dc/nceppost/gfs_smartmakeprecip.fd/gfs_smartmakeprecip <<EOF > makeprecip12.out
##192 186   
##EOF
#         err=$?
#         if [ $err -eq 0 ] ; then
#           cat 12precip 12cprecip >> ${COMROT}/pgb${flag}192.gfs.$CDATE
#         else
#           echo " failed while generating 12 hr precipitation bucket for F192 "
#           exit 1
#         fi 
          if [ ! -z $SMARTPRECIPSH ] ; then
            if [ -s $SMARTPRECIPSH ] ; then
              $SMARTPRECIPSH $FH $flag
              err=$?
              if [ $err -ne 0 ] ; then
                echo " failed while generating 12 hr precipitation bucket for F192 "
                exit 1
              fi
            fi
          fi
        fi 
# Chuang: add part of Sib's scripts to generate .25/0.5/1/2.5 Grib2 files
        if [ $GRIBVERSION = grib2 ]; then
	  export FH=$FH
	  if [ $GFS_DOWNSTREAM = 'YES' ] ; then
            $GFSDOWNSH
          fi
        else # for Grib1
         if [ $NCEPPOST = YES ] ; then
          if [ $CAT_FLX_TO_PGB = YES ] ; then cat flxfile >> $PGBOUT ; fi
           if [ $GRID_ID -eq $pgbf_grid ] ; then
            mv $PGBOUT $PGBOUT1
            if [ $KTO -gt 0 ] ; then mv $IPVOUT $IPVOUT1 ; fi
            if [ $POSTD3D = YES ] ; then
              mv $D3DOUT $D3DOUT1
              if [ $IDRT -eq 0 ] ; then rm d3dfile ; fi
            fi
           else
            ${precpgb}$COPYGB -g$pgbf_grid -i$in_o -x $PGBOUT $PGBOUT1
            if [ $KTO -gt 0 ] ; then ${precpgb}$COPYGB -g$pgbf_grid -i$in_o -x $IPVOUT $IPVOUT1 ; fi
            if [ $POSTD3D = YES ] ; then
              ${precpgb}$COPYGB -g$pgbf_grid -i$in_o -x $D3DOUT $D3DOUT1
              if [ $IDRT -eq 0 ] ; then rm d3dfile ; fi
            fi
           fi
         elif [ $IO -gt 360 ] ; then
          ${precpgb}$COPYGB -g3 -i$in_o -x $PGBOUT $PGBOUT1
         else
          mv $PGBOUT $PGBOUT1
         fi
         if [ $GRID_ID11 -gt 0 ] ; then
          if [ $PGBOUT1 != $COMOUT/pgbf${FH}$SUFOUT ] ; then
            ${precpgb}$COPYGB -g3 -i$in_o -x $PGBOUT1 $COMOUT/pgbf${FH}$SUFOUT
            if [ $NCEPPOST = YES -a $KTO -gt 0 ] ; then
              ${precpgb}$COPYGB -g3 -i$in_o -x $IPVOUT1 $COMOUT/ipvf${FH}$SUFOUT
            fi
            if [ $POSTD3D = YES ] ; then
              ${precpgb}$COPYGB -g$pgbf_grid -i$in_o -x $D3DOUT1 $COMOUT/diabf${FH}$SUFOUT
            fi
          fi
         fi
         if [ $GRID_ID25 -gt 0 ] ; then
          if [ $GRID_ID -ne 3  -o $IO -gt 360 ] ; then
            pgbfile=$PGBOUT
            if [ $NCEPPOST = YES -a $KTO -gt 0 ]   ; then ipvfile=$IPVOUT  ; fi
            if [ $POSTD3D = YES ] ; then d3dfile=$D3DOUT  ; fi
          else
            pgbfile=$PGBOUT1
            if [ $NCEPPOST = YES -a $KTO -gt 0 ]   ; then ipvfile=$IPVOUT1 ; fi
            if [ $POSTD3D = YES ] ; then d3dfile=$D3DOUT1 ; fi
          fi
#         ${precpgb}$COPYGB -g$GRID_ID25 -i$in_o -x $pgbfile $COMOUT/pgblnl${SUFOUT}
          rm pgb25a pgb25b

#         $WGRIB $pgbfile|grep -i :kpds6=100:|${precpgb}$COPYGB -xkw -i4,0,35 -g$GRID_ID25 $pgbfile pgb25a
#         $WGRIB $pgbfile|grep -vi :kpds6=100:|${precpgb}$COPYGB -xkw -i$in_o -g$GRID_ID25 $pgbfile pgb25b

           rm xxx yyy
           $WGRIB $pgbfile|grep -i :kpds6=100: > xxx
           ${precpgb}$COPYGB -xkw -i4,0,35 -g$GRID_ID25 $pgbfile pgb25a < xxx
           $WGRIB $pgbfile|grep -vi :kpds6=100: > yyy
           ${precpgb}$COPYGB -xkw -i$in_o -g$GRID_ID25 $pgbfile pgb25b < yyy

          cat pgb25a pgb25b > $COMOUT/pgbl${FH}${SUFOUT}
          if [ $NCEPPOST = YES -a $KTO -gt 0 ] ; then ${precpgb}$COPYGB -g$GRID_ID25 -i$in_o -x $ipvfile $COMOUT/ipvl${FH}${SUFOUT} ; fi
          if [ $POSTD3D = YES ] ; then
            ${precpgb}$COPYGB -g$GRID_ID25 -i$in_o -x $d3dfile $COMOUT/diabl${FH}${SUFOUT}
          fi
         fi
         if [ $GRID_ID62 -gt 0 ] ; then
          ${precpgb}$COPYGB -g$GRID_ID62 -i$in_o -x $FLXIND/flxf${FH}${SUFOUT} $FLXIND/flxl${FH}${SUFOUT}
         fi

         if [ ! -z $LINKPOSTFILESH ]; then
          if [ -s $LINKPOSTFILESH ] ; then
            $LINKPOSTFILESH $FH
          fi
         fi
        fi # for Grib2 vs Grib1

      fi
    fi           ;  # End of "REDO_POST" if block
#                     For time averaging of forecasts
    if [ $AVG_FCST = YES -o $TSER_FCST = YES ] ; then
      FDATE=$($NDATE $FH $CDATE)
      if [ $AVG_INT -eq 999 ] ; then     # Monthly mean
        if [ $PDATE -gt $CDATE ] ; then
         xdate=$(echo $($NDATE 768 $PDATE) | cut -c1-6)0100
        else
         xdate=$(echo $($NDATE 768 $(echo $CDATE | cut -c1-6)0100) | cut -c1-6)0100
        fi
      else
        xdate=$(echo $($NDATE $AVG_INT $PDATE))
      fi
      if [ $FDATE -ge $xdate ] ; then
        export edate_av=$xdate
        export sdate_av=$($NDATE $FHOUT $PDATE)
        PDATE=$xdate
        if [ $EXTERNAL_AVG = YES ] ; then
          if [ ! -z $EXTERNALSH ]; then
            $EXTERNALSH $PSLOT $CDATE $sdate_av $edate_av $FHOUT $COMROT
          fi
        else
          if [ $AVG_FCST = YES ] ; then
            if [ $AVG_SUB = YES ] ; then
              $AVGSUBSH $PSLOT $CDATE $sdate_av $edate_av $FHOUT $COMROT $TMEANDIR $FHBAK
            else
              $FCST_TIMEMEANSH $PSLOT $CDATE $sdate_av $edate_av $FHOUT $COMROT $TMEANDIR $DATA/$sdate_av $INDXDIR $ENS_NUM
            fi
            rc=$?
            if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
          fi
          if [ $TSER_FCST = YES ] ; then
            if [ $TSER_SUB = YES ] ; then
              $TSERSUBSH $PSLOT $CDATE $sdate_av $edate_av $FHOUT $COMROT $TIMEDIR $FHBAK $DATA/tser_$sdate_av $ENS_NUM $CSTEP
            else
              $FCST_TSERSH $PSLOT $CDATE $sdate_av $edate_av $FHOUT $COMROT $TIMEDIR $FHBAK $DATA/tser_$sdate_av $ENS_NUM
            fi
            rc=$?
            if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
          fi
        fi
      fi
    fi
  done
fi
#
#    Post ocean files from netcdf to grib
#
#if [ $COUP_FCST = YES ] ; then
#  if [ POSTOCN = YES ] ; then
#    export FIX_OM=${FIX_OM:-$BASEDIR/fix_mom4}
#    ${NCP:-/bin/cp} -p $FIX_OM/OCNINTP${omres}TO${outres}.C  OCNINTPCOEF.C
#    ${NCP:-/bin/cp} -p $FIX_OM/OCNINTP${omres}TO${outres}.T  OCNINTPCOEF.T
#    sdate=$($NDATE $FHINI $CDATE)
#    edate=$($NDATE $FHMAX $CDATE)
#    $MOM4POSTSH $sdate $edate $CDATE $DATA $COMROT $COMOUT $CDUMP $FHOUT
#  fi
#fi
   
################################################################################
# Copy out restart and output files (disabled here)

#$PCOP $CDATE/$CDUMP/$CSTEP/ROTO $DATA $COMROT <$RLIST
rc=$?

#$PCOP $CDATE/$CDUMP/$CSTEP/OPTO $DATA $COMROT <$RLIST

################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
#
#if [ $mlanl -gt 0 -a $GDATE -gt $CDATE_SKIP -a $nknd -eq 1 ] ; then
# nsleep=0
# msleep=50
# until [[ -s $COMROT/sfcf06.$CDUMP.$CDATE.LIS || $((nsleep+=1)) -gt $msleep ]];do sleep 10 ; done
# if [[ $nsleep -gt $msleep ]] ; then $PERR ; exit 3 ; fi
#fi
#
if [ $JUST_POST = YES -o $JUST_AVG = YES -o JUST_TSER = YES ] ; then exit ; fi
#
# If Coupled forecast, make sure that the forecast is finished before pend
#
if [ $COUP_FCST = YES -a $CCPOST = YES ] ; then
  nsleep=0
  tsleep=10
  msleep=360
  FDATE=$($NDATE $FHMAX $CDATE)
  OCNR=$COMROT/RESTART/omrestart$SUFOUT.$FDATE.tar
  until [[ -s $OCNR || $((nsleep+=1)) -gt $msleep ]];do sleep $tsleep;done
  if [[ $nsleep -gt $msleep ]];then
    echo 'The last Ocean restart file does not exist - Stream Aborted'
    exit 777
  fi
fi
$PEND
