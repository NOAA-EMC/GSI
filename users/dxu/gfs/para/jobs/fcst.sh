#!/bin/ksh
################################################################################
# This script runs the forecast.
# Usage: fcst.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   INFOLEVELANAL
#   PMDLOGANAL
#   FNTSFATMP
#   SMIPCPTMP
#   TMIPCPTMP
#   DATATMP
#   COMIN
#   COMRS
#   COMROT
#   NCP
#   NDATE
#   FORECASTSH
#   PBEG
#   PERR
#   PEND
################################################################################
set -ux

################################################################################
# Go configure

export CKSH=$(echo $CSTEP|cut -c-4)
export CKND=$(echo $CSTEP|cut -c5-)
set -a;. $CONFIG;set +a
export machine=${machine:-WCOSS}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
eval export DATA=$DATATMP
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
#chgrp ${group_name:-rstprod} $DATA
chmod ${permission:-755} $DATA
#chmod 555 $DATA
#
export HOMEDIR=${HOMEDIR:-/nwprod}
export EXECDIR=${EXECDIR:-$HOMEDIR/exec}
export FIXDIR=${FIXDIR:-$HOMEDIR/fix/fix_am}
export FIXGLOBAL=${FIXGLOBAL:-$FIXDIR}
export SCRDIR=${SCRDIR:-$HOMEDIR/scripts}
export SHDIR=${SHDIR:-$HOMEDIR/bin}
export NWPROD=${NWPROD:-$HOMEDIR}
#
export PBEG=${PBEG:-$SHDIR/pbeg}
export PAVG=${PAVG:-$SHDIR/pavg}
export PEND=${PEND:-$SHDIR/pend}
export PERR=${PERR:-$SHDIR/perr}
#$PBEG

################################################################################
# Set other variables
export USE_RESTART=${USE_RESTART:-NO}
export COUP_FCST=${COUP_FCST:-YES}
export AVG_FCST=${AVG_FCST:-NO}
if [ $AVG_FCST = YES ] ; then export AVG_INT=${AVG_INT:-999} ; fi
export PCOP=${PCOP:-$SHDIR/pcop}
export NCP=${NCP:-/bin/cp}
export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
export NHOUR=${NHOUR:-$NWPROD/util/exec/nhour}
export ncdump=${ncdump:-$EXECDIR/ncdump}
export CHGRESSH=${CHGRESSH:-$NWPROD/ush/global_chgres.sh}
export CYCLESH=${CYCLESH:-$NWPROD/ush/global_cycle.sh}
export FORECASTSH=${FORECASTSH:-$SCRDIR/exglobal_fcst.sh.sms}
export REGRIDMOM4SH=${REGRIDMOM4SH:-$HOMEDIR/ush/regrid_mom4.sh}
export LINKFILESH=${LINKFILESH:-""}
export SIGHDR=${SIGHDR:-$NWPROD/exec/global_sighdr}
cycle=$(echo $CDATE|cut -c9-10)
cdump=$(echo $CDUMP|tr '[a-z]' '[A-Z]')
eval mfcst=\${MFCST$cycle$cdump:-1}
export nknd=${CKND:-1}
#nknd0=$((nknd-1))
#if [ $nknd0 -lt 1 ] ; then ; nknd0=$nknd
export ENS_NUM=${ENS_NUM:-1}
export SIGIND=${SIGIND:-$COMROT}
export RESDIR=${RESDIR:-$DATA}
export ENTHALPY=${ENTHALPY:-NO}
export RUN_ENTHALPY=${RUN_ENTHALPY:-.true.}
export CDFNL=${CDFNL:-gdas}
export GDUMP=${GDUMP:-$CDFNL}
export tasks=$(eval echo \${NUMPROCFCST$cycle$cdump:-$tasks}|cut -f$nknd -d,)
export JCAP=$(eval echo \${JCAPFCST$cycle$cdump:-$JCAP}|cut -f$nknd -d,)
export JCAPm=$(eval echo \${JCAPFCST$cycle$cdump:-$JCAP}|cut -f$mfcst -d,)
#export JCAP0=$(eval echo \${JCAPFCST$cycle$cdump:-$JCAP}|cut -f$nknd0 -d,)
export IDVC=$(eval echo \${IDVCFCST$cycle$cdump:-$idvc_a}|cut -f$nknd -d,)
export LEVS=$(eval echo \${LEVSFCST$cycle$cdump:-$LEVS}|cut -f$nknd -d,)
export LONB=$(eval echo \${LONBFCST$cycle$cdump:-$LONB}|cut -f$nknd -d,)
export LATB=$(eval echo \${LATBFCST$cycle$cdump:-$LATB}|cut -f$nknd -d,)
export NTRAN=$(eval echo \${NTRANFCST$cycle$cdump:-1}|cut -f$nknd -d,)
export LANDICE_OPT=$(eval echo \${LICEFCST$cycle$cdump:-2}|cut -f$nknd -d,)
export FHBAK=$(eval echo \${FHBAKFCST$cycle$cdump:-00}|cut -f$nknd -d,)
export FHDFI=$(eval echo \${FHDFIFCST$cycle$cdump:-03}|cut -f$nknd -d,)
export FHCYC=$(eval echo \${FHCYCFCST$cycle$cdump:-24}|cut -f$nknd -d,)
export FHINI=$(eval echo \${FHINIFCST$cycle$cdump:-00}|cut -f$nknd -d,)
export FHBEG=0
export FHBAK=${FHBAK:-0}
#export FHBEG=$FHINI
#if [ $FHBAK -eq 0 ] ; then export FHBEG=$FHBAK ; fi
export FHSTRT=${FHSTRT:-9999999}
export FHMAX=$(eval echo \${FHMAXFCST$cycle$cdump:-09}|cut -f$nknd -d,)
export FHOUT=$(eval echo \${FHOUTFCST$cycle$cdump:-03}|cut -f$nknd -d,)
export FHZER=$(eval echo \${FHZERFCST$cycle$cdump:-06}|cut -f$nknd -d,)
export FHRES=$(eval echo \${FHRESFCST$cycle$cdump:-24}|cut -f$nknd -d,)

##REV4GOCART
export LGOC3D=$(eval echo \${LG3DFCST$cycle$cdump:-$lg3d_1}|cut -f$nknd -d,)
if [ $LGOC3D = .true. ] ; then
 export FHGOC3D=${FHGOC3D:-72}
 export FCSTVARS="LGOC3D=$LGOC3D,FHGOC3D=$FHGOC3D,$FCSTVARS"
fi
##REV4GOCART

export DELTIM=$(eval echo \${DELTIMFCST$cycle$cdump:-${DELTIM:-""}}|cut -f$nknd -d,)
export DTPHYS=$(eval echo \${DTPHYSFCST$cycle$cdump:-${DTPHYS:-""}}|cut -f$nknd -d,)
export NTHREADS=$(eval echo \${NTHRFCST$cycle$cdump:-$nth_f1}|cut -f$nknd -d,)
export LDIAG3D=$(eval echo \${LD3DFCST$cycle$cdump:-$ld3d_1}|cut -f$nknd -d,)
export NTHSTACK=${NTHSTACK:-128000000}
export FCSTVARS="LDIAG3D=$LDIAG3D,$FCSTVARS"
if [ $DTPHYS -ne $DELTIM ] ; then
 export FCSTVARS="DTPHYS=$DTPHYS,$FCSTVARS"
fi
NST_FCST=${NST_FCST:-0}
export CHGRESVARS=${CHGRESVARS:-""}
export CHGRESTHREAD=${CHGRESTHREAD:-16}
export CHGRESSTACK=${CHGRESSTACK:-128000000}
########export MTNDIR=$(eval echo \${ORODIR/ml$JCAP:-$/nwprod/fix})
export FILESTYLE='L'
export VERBOSE=YES
#export COMOUT=$DATA
export COMOUT=$COMROT
if [ $nknd -gt 1 -a $FHBAK -eq 0 ] ; then
 export SUFIN=.${CDUMP}$nknd.$CDATE
 export SUFOUT=.${CDUMP}$nknd.$CDATE
 export SUFING=.${GDUMP}$nknd.$CDATE
 export SUFOUTG=.${GDUMP}$nknd.$CDATE
else
 export SUFIN=.${CDUMP}.$CDATE
 export SUFOUT=.${CDUMP}.$CDATE
 export SUFING=.${GDUMP}.$CDATE
 export SUFOUTG=.${GDUMP}.$CDATE
fi
[[ -n ${AMEXECTMP:-""} ]]&&eval export AM_EXEC=$AMEXECTMP
export FCSTEXEC=$AM_EXEC
[[ -n ${OROGRAPHYTMP:-""} ]]&&eval export OROGRAPHY=$OROGRAPHYTMP
[[ -n ${OROGRAPHY:-""} ]]&&eval export OROGRAPHY=$OROGRAPHY
[[ -n ${FNOROGTMP:-""} ]]&&eval export OROGRAPHY=$FNOROGTMP
[[ -n ${FNOROG:-""} ]]&&eval export OROGRAPHY=$FNOROG
[[ -n ${SLMASKTMP:-""} ]]&&eval export SLMASK=$SLMASKTMP
[[ -n ${SLMASK:-""} ]]&&eval export SLMASK=$SLMASK
[[ -n ${FNMASKTMP:-""} ]]&&eval export SLMASK=$FNMASKTMP
[[ -n ${FNMASK:-""} ]]&&eval export SLMASK=$FNMASK
[[ -n ${MTNVARTMP:-""} ]]&&eval export MTNVAR=$MTNVARTMP
[[ -n ${MTNVAR:-""} ]]&&eval export MTNVAR=$MTNVAR
[[ -n ${MTNVARTMP:-""} ]]&&eval export MTNVAR=$MTNVARTMP
[[ -n ${FCSTXLSMPOPTS:-""} ]]&&eval export XLSMPOPTS=$FCSTXLSMPOPTS
[[ -n ${OMEXECTMP:-""} ]]&&eval export OM_EXEC=$OMEXECTMP
[[ -n ${CEXECTMP:-""} ]]&&eval export C_EXEC=$CEXECTMP
#
#export MP_EAGER_LIMIT=32768

export LONR=$LONB
export LATR=$LATB
export LONF=$LONB
export LATG=$LATB
#
if [ $nknd -gt 1 -o $CDUMP = gfs ] ; then
 do_irestart=${do_irestart:-.false.}
fi
export do_irestart=${do_irestart:-.true.}
#
#
pfac=${pfac:-10}

if [ $COUP_FCST = YES ] ; then
 export omres=$(eval echo \${OMRESFCST$cycle$cdump:-05}|cut -f$nknd -d,)
 if [ $machine = IBMP6 ] ; then
   export NPROCS_o=$(eval echo \${NPROCOFCST$cycle$cdump:-60}|cut -f$nknd -d,)
#export INCH=$(eval echo \${INCHFCST$cycle$cdump:-$((FHMAX-FHINI))}|cut -f$nknd -d,)
#export INCH=24
#export NPROCS_o=${NPROCS_o:-$((tasks/pfac))}
   export NPROCS_c=1
   export NPROCS_a=$((tasks-NPROCS_o-NPROCS_c))
   if [ $NPROCS_a -lt 1 ] ; then
    export NPROCS_o=$((tasks/pfac))
    export NPROCS_a=$((tasks-NPROCS_o-NPROCS_c))
   fi
 else
   export NPROCS_o=$NPROCS_o
   export NPROCS_a=$NPROCS_a
   export NPROCS_c=$NPROCS_c
 fi
#grid_mom4ice=$FIX_OM/grid_spec_1x1.nc.T$JCAP
 omres=${omres:-1x1}
 export grid_mom4ice=$FIX_OM/grid_spec_$omres.nc.T$JCAP
 export chl=$FIX_OM/chl_$omres.nc
 export salt_sfc_restore=$FIX_OM/salt_sfc_restore_$omres.nc
 export SALTSFCRESTORE=$salt_sfc_restore
 export temp_sfc_restore=$FIX_OM/temp_sfc_restore_$omres.nc
 export runoff=$FIX_OM/runoff_$omres.nc
 export ohf_sice=$FIX_OM/ohf_sice.nc
# The following 3 lines may be for godas, so may not be needed here
 export SALTSFCRESTORE=$salt_sfc_restore
#export CHLNC=$FIX_OCN/chl_$omres.nc
#export RUNOFFNC=$FIX_OCN/RUNOFF_$omres.nc
#export SALTSFCRESTORE=$salt_sfc_restore
#export SALTSFCRESTORE=$FIX_OCN/salt_sfc_restore_$omres.nc
#
 if [ $omres = '1x1' ] ; then
   dt_ocean=${dt_1x1:-3600}                     # OM time step
   dt_cpld=${dt_cpld:-${dt_1x1:-3600}}          # OM coupling time interval
   export im_mom4=360
   export jm_mom4=231
   export jmtp_mom4=25
   export imos=80
 else
   dt_cpld=${dt_05:-1800}                       # OM coupling time interval
   dt_ocean=${dt_05:-1800}                      # OM time step
   dt_cpld=${dt_cpld:-${dt_05:-1800}}           # OM coupling time interval
   export im_mom4=720
   export jm_mom4=410
   export jmtp_mom4=50
   export imos=160
 fi
 dt_aocpl=$dt_ocean
#
else
 export NPROCS_a=$tasks
fi
#
#
task_mem=$((NPROCS_a/ENS_NUM))
c=1
while [ $c -le $ENS_NUM ] ; do
 export PE$c=$task_mem
 c=$((c+1))
done
#
############################################################################
# Set parameters for hybrid or sigma forecasts

export IDVM=1 ; export IDSL=1 ; export ivssig=198410
if [ $LEVS -gt 99 ] ; then export ivssig=200509 ; fi

if [ $IDVC = 1 ] ; then
 export IDVM=1 ; export IDSL=1 ; export nvcoord=1
 export SIGLEVEL1=${SIGLEVEL1:-$FIXGLOBAL/global_siglevel.l$LEVS.txt}
elif [ $IDVC = 2 ] ; then
 export IDVM=1 ; export IDSL=1 ; export nvcoord=2
 export SIGLEVEL2=${SIGLEVEL2:-$FIXGLOBAL/global_hyblev.l$LEVS.txt}
elif [ $IDVC = 3 ] ; then
#export IDVM=2 ; export IDSL=2 ; export nvcoord=3 
 export ivssig=200509
 Apercent=${Apercent:-100}
 if [ $Apercent -lt 100 ] ; then
  export SIGLEVEL3=${SIGLEVEL3:-$FIXGLOBAL/global_hyblev3.ipa$Apercent.l$LEVS.txt}
 else
  export SIGLEVEL3=${SIGLEVEL3:-$FIXGLOBAL/global_hyblev3.l$LEVS.txt}
 fi
#
#   SFCPRESS_ID=0 or 1 for ln(psfc), 2 for psfc
#   THERMODYN_ID=3 for enthalphy, 0 or 1 for virtual T, 2 for T
#
 if [ $ENTHALPY = YES ] ; then
  export SFCPRESS_ID=${SFCPRESS_ID:-2}
  export THERMODYN_ID=${THERMODYN_ID:-3}
#
#***************************************************************
#                                N2 ,     H2O,     O3,        CLW
# export RIlist=${RIlist:-"   296.8034, 461.50, 173.2247,    0.0"}
# export CPIlist=${CPIlist:-" 1039.645, 1846.0, 820.2391,    0.0"}
#                             Dry Air ,  H2O,     O3,       CLW
  export RIlist=${RIlist:-"   287.05, 461.50, 173.2247,    0.0"}
  export CPIlist=${CPIlist:-" 1004.6, 1846.0, 820.2391,    0.0"}
#
  export TRACERVARS="RI=$RIlist,CPI=$CPIlist,"
 else
  export SFCPRESS_ID=${SFCPRESS_ID:-1}
  export THERMODYN_ID=${THERMODYN_ID:-1}
  if [ $RUN_ENTHALPY = .true. ] ; then
   export RIlist=${RIlist:-"   287.05, 461.50, 173.2247,    0.0"}
   export CPIlist=${CPIlist:-" 1004.6, 1846.0, 820.2391,    0.0"}
   export TRACERVARS="RI=$RIlist,CPI=$CPIlist,"
   export FCSTVARS="RUN_ENTHALPY=$RUN_ENTHALPY,$FCSTVARS"
  fi
 fi
 export IDVM=${THERMODYN_ID}${SFCPRESS_ID}
 export IDSL=2 ; export nvcoord=3 ; export LATCH=${LATCH:-8}
else
 echo 'Invalid IDVC = ',IDVC ; exit 111
fi
#
export MTNDIR=${MTNDIR:-$FIXGLOBAL}
export SIGLEVEL=$(eval echo \${SIGLEVEL$IDVC})
export OROGRAPHY=${OROGRAPHY:-$MTNDIR/global_orography.t$JCAP.grb}
export OROGRAPHY_UF=${OROGRAPHY_UF:-$MTNDIR/global_orography_uf.t$JCAP.$LONB.$LATB.grb}
export FNOROG=${OROGRAPHY:-$MTNDIR/global_orography.t$JCAP.grb}
export FNMASK=${SLMASK:-$MTNDIR/global_slmask.t$JCAP.grb}
#
################################################################################
# Copy in restart and input files
if [ $COUP_FCST = YES ] ; then
  mkdir -p $DATA/RESTART
  mkdir -p $DATA/INPUT
  mkdir -p $DATA/OUTPUT
  export RESTART_CONTROL_FILE=${RESTART_CONTROL_FILE:-$RESDIR/pr$PSLOT.2restart$SUFIN}
  if [ $USE_RESTART = YES ] ; then
   if [ ! -s $RESTART_CONTROL_FILE ] ; then
     export USE_RESTART=NO
   fi
  fi
fi
FHREST=$FHINI

#$PCOP $CDATE/$CDUMP/$CSTEP/ROTI $COMROT $DATA <$RLIST

if [ $FHINI -eq 0 -o $FHBAK -eq 0 ] ; then
  export SIGI=$SIGIND/siganl$SUFIN
  export SIGI2=/dev/null
  export SFCI=$SIGIND/sfcanl$SUFIN
  if [ $NST_FCST -gt 0 ] ; then
    export NSTI=$SIGIND/nstanl$SUFIN
  fi
  if [ $NTRAN -lt 2 ] ; then export NTRAN=2 ; fi
  if [ -s $SIGI -a -s $SFCI ] ; then
   FHINI=`$SIGHDR $SIGI ifhr`
#  if [ $NTRAN -lt 2 ] ; then export NTRAN=2 ; fi
   if [ $COUP_FCST = YES ] ; then
    FDATE=$CDATE
    export OCNI=$COMROT/ocnanl$SUFING.tar
    if [ ! -s $OCNI -a $nknd -gt 1 ] ; then
     ${NCP:-/bin/cp} $COMROT/ocnanl.$GDUMP.$CDATE.tar $OCNI
    fi
   fi
  else
   export SIGI=$SIGIND/siganl.$CDUMP.$CDATE
   export SFCI=$SIGIND/sfcanl.$CDUMP.$CDATE
   if [ $NST_FCST -gt 0 ] ; then
     export NSTI=$SIGIND/nstanl.$CDUMP.$CDATE
   fi
   FHINI=`$SIGHDR $SIGI ifhr`
   if [ $COUP_FCST = YES ] ; then
    FDATE=$CDATE
    export OCNI=$COMROT/ocnanl$SUFING.tar
    if [ ! -s $OCNI -a $nknd -gt 1 ] ; then
     ${NCP:-/bin/cp} $COMROT/ocnanl.$GDUMP.$CDATE.tar $OCNI
    fi
   fi
  fi

  export sig_chg=$SIGIND/siganl
  export sfc_chg=$SIGIND/sfcanl
  if [ $NST_FCST -gt 0 ] ; then
    export nst_chg=$SIGIND/nstanl
  fi
fi

if [ $USE_RESTART = YES ] ; then

  if [ $FHSTRT = 9999999 ] ; then
    sigr1file=$(ls -1tr $RESDIR/sig1r*$SUFIN | tail -1)
    if [ -s $sigr1file ] ; then
      jcap=$($SIGHDR $sigr1file jcap)
      if [ $JCAP -ne $jcap ] ; then
        USE_RESTART=NO
      else
        FHREST=$($SIGHDR $sigr1file ifhr)
        if [ $COUP_FCST = YES ] ; then
          FDATE=$($NDATE $FHREST $CDATE)
          if [ ! -s $RESDIR/fluxes_for_OM$SUFIN.$FDATE ] ; then
            echo 'No fluxes for OM for FDATE= '$FDATE
            FHREST=$FHINI
          fi
        fi
        if [ $FHREST -lt $FHINI ] ; then FHREST=$FHINI ; fi
      fi
    fi
  elif [ $FHSTRT -gt $FHINI ] ; then
    if [ -s $RESDIR/sig1r${FHSTRT}${SUFIN} ] ; then
      jcap=$($SIGHDR $$RESDIR/sig1r${FHSTRT}${SUFIN} jcap)
      if [ $JCAP -ne $jcap ] ; then
        USE_RESTART=NO
      else
        export FHREST=$FHSTRT
        if [ $COUP_FCST = YES ] ; then
          FDATE=$($NDATE $FHREST $CDATE)
          if [ ! -s $RESDIR/fluxes_for_OM$SUFIN.$FDATE ] ; then
            echo 'No fluxes for OM for FDATE= '$FDATE
            exit
          fi
        fi
      fi
    fi
  else
    FHINI=0
    USE_RESTART=NO
  fi
fi
if [ $FHREST -eq 0 -o $FHREST -gt $FHMAX ] ; then USE_RESTART=NO ; fi
if [ $USE_RESTART = YES ] ; then
  if [[ $FHREST -lt 10 ]] ; then FHREST=0$FHREST ; fi
  if [ -s $RESDIR/sig1r${FHREST}${SUFIN} ] ; then
    export SIGI=$RESDIR/sig1r${FHREST}${SUFIN}
    export SIGI2=$RESDIR/sig2r${FHREST}${SUFIN}
    export SFCI=$RESDIR/sfcr${FHREST}${SUFIN}
    if [ $NST_FCST -gt 0 ] ; then
      export NSTI=$RESDIR/nstr${FHREST}${SUFIN}
    fi
    if [ $NTRAN -lt 2 ] ; then export NTRAN=2 ; fi
  elif [ $($SIGHDR $RESDIR/sig1r${FHMAX}${SUFIN} ifhr) -eq $FHREST ] ; then
    if [ $FHREST -lt $FHMAX ] ; then
      ${NCP:-/bin/cp} $RESDIR/sig1r${FHMAX}${SUFIN} $DATA/sig1r_loc
      ${NCP:-/bin/cp} $RESDIR/sig2r${FHMAX}${SUFIN} $DATA/sig2r_loc
      ${NCP:-/bin/cp} $RESDIR/sfcr${FHMAX}${SUFIN}  $DATA/sfcr_loc
      export SIGI=$DATA/sig1r_loc
      export SIGI2=$DATA/sig2r_loc
      export SFCI=$DATA/sfcr_loc
      if [ $NST_FCST -gt 0 ] ; then
        ${NCP:-/bin/cp} $RESDIR/nstr${FHMAX}${SUFIN}  $DATA/nstr_loc
        export NSTI=$DATA/nstr_loc
      fi
      if [ $NTRAN -lt 2 ] ; then export NTRAN=2 ; fi
    fi
  else
    echo ' No AM restart file sigr1 for starting FH= '$FHREST
    echo ' Execution Terminating'
    exit
  fi
  FHINI=$($SIGHDR $SIGI ifhr)
  if [ $COUP_FCST = YES ] ; then
   FDATE=$($NDATE $FHINI $CDATE)
   export OCNI=$RESDIR/omrestart$SUFIN.$FDATE.tar
  fi

elif [[ $NTRAN -le 1 && $FHBAK -gt 0 ]];then
  export SIGI=$SIGIND/sigf$FHBAK.$CDUMP.$CDATE
  export SIGI2=/dev/null
  export SFCI=$SIGIND/sfcf$FHBAK.$CDUMP.$CDATE
  export sig_chg=$DATA/sigchg
  export sfc_chg=$DATA/sfcchg
  if [ $COUP_FCST = YES ] ; then
   FDATE=$($NDATE $FHBAK $CDATE)
   export OCNI=$RESDIR/omrestart.$CDUMP.$CDATE.$FDATE.tar
  fi
  if [ $NST_FCST -gt 0 ] ; then
    export NSTI=$SIGIND/nstf$FHBAK.$CDUMP.$CDATE
    export nst_chg=$DATA/nstchg
  fi
fi


################################################################################
# Run change resolution and surface cycle if necessary

idvc=$($SIGHDR $SIGI idvc)
jcap=$($SIGHDR $SIGI jcap)
levs=$($SIGHDR $SIGI levs)
#
if [ $COUP_FCST = YES ] ; then
  export INCH=$(eval echo \${INCHFCST$cycle$cdump:-$((FHMAX-FHINI))}|cut -f$nknd -d,)
# ${NCP:-cp} $COMROT/restart.dta.$CDATE $DATA/restart.dta
  cd $DATA/INPUT
  tar -xvf $OCNI
  rc=$?
  if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
  for newname in $(ls -rt ${FDATE}* | cut -c11-) ; do
# while [ -s ${CDATE}* ] ; do
#  newname=$(ls -rt ${CDATE}* | cut 11-)
   mv ${FDATE}$newname $newname
  done

  if [ $FHINI -gt 0 ] ; then
   if [ $USE_RESTART = YES ] ; then
    if [ -s $RESDIR/fluxes_for_OM$SUFIN.$FDATE ] ; then
     ${NCP:-/bin/cp} $RESDIR/fluxes_for_OM$SUFIN.$FDATE  $DATA/fluxes_for_OM
    fi
   fi
  else
   im_om=$($ncdump -c ocean_temp_salt.res.nc| grep xaxis_1 | head -1 | awk -F= '{print $2}' | cut -c2-4)
   if [ $im_om -eq 720 -a $omres = 1x1 ] ; then
     COMDMPTMP=${COMDMPTMP:-$COMDMP}
     eval export COMDMP=$COMDMPTMP
     icefile=$COMDMP/icegrb.$GDUMP.$CDATE
     OCNINP=$OCNI
     OCNOUT=$COMROT/ocnanl$SUFOUTG.tar
     $REGRIDMOM4SH $CDATE $GDUMP $OCNINP $OCNOUT 05 1x1 $DATA $JCAP $icefile
     cd $DATA/INPUT
     tar -xvf $OCNOUT
   fi
    
  fi
  cd $DATA
fi
#

if [[ $NTRAN -eq 0 || $IDVC -ne $idvc || $JCAP -ne $jcap || $LEVS -ne $levs ]] ; then
  export SIGINP=$SIGI
  export SFCINP=$SFCI
  export SIGOUT=${sig_chg}$SUFOUT
  export SFCOUT=${sfc_chg}$SUFOUT
  if [ $NST_FCST -gt 0 ] ; then
    export NSTINP=$NSTI
    export NSTOUT=${nst_chg}$SUFOUT
  fi
  if [ $SIGINP = $SIGOUT -o $SFCINP = $SFCOUT ] ; then
    export SIGOUT=$DATA/$(basename $SIGOUT)
    export SFCOUT=$DATA/$(basename $SFCOUT)
  fi
  if [ $NST_FCST -gt 0 ] ; then
    if [ $NSTINP = $NSTOUT ] ; then
      export NSTOUT=$DATA/$(basename $NSTOUT)
    fi
  fi
  LATCH=${LATCH:-48}
  export CHGRESVARS="IDVC=$IDVC,IVSSIG=$ivssig,IVSSFC=$ivssfc,NVCOORD=$nvcoord,IDVM=$IDVM,IDSL=$IDSL,LATCH=$LATCH,$CHGRESVARS"
  if [ $IDVM -gt 3 ] ; then
   if [ $THERMODYN_ID -gt 2 ] ; then
     CPIlist=${CPIlist:-""}
     RIlist=${RIlist:-""}
     export CHGRESVARS=$CHGRESVARS"RI=$RIlist,CPI=$CPIlist"
   fi
  fi
# export XLSMPOPTS="parthds=16:spins=0:yields=0:stack=128000000"
  export XLSMPOPTS="parthds=$CHGRESTHREAD:stack=$CHGRESSTACK"
  export OUTTYP=2
  $CHGRESSH          # This chgres should include NST file version if NST_FCST>0
  rc=$?
  if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
  export SIGI=$SIGOUT
  export SFCI=$SFCOUT
  if [ $NST_FCST -gt 0 ] ; then
    export NSTI=$NSTOUT
  fi
fi

#
################################################################################
export NGPTC=${NGPTC:-$((JCAP/10))}
#export XLSMPOPTS="parthds=$NTHREADS:spins=0:yields=0:stack=128000000"
export XLSMPOPTS="parthds=$NTHREADS:stack=$NTHSTACK"
################################################################################

# Spin up forecast if necessary

if [[ $USE_RESTART = NO ]] ; then
 if [[ $NTRAN -le 1 ]];then
  if [[ $FHBAK -lt $FHINI ]];then
    export FHINI2=$FHINI
    export FHMAX2=$FHMAX
    export FHRES2=$FHRES
    export SUFOUT2=$SUFOUT
    export SIGR1=sig1lr
    export SIGR2=sig2lr
    export SFCR=sfclr
    if [ $NST_FCST -gt 0 ] ; then
      export NSTR=nstlr
    fi
    export FHINI=$FHBAK
    export FHMAX=$FHINI2
    export FHRES=$((10#$FHINI2-10#$FHBAK))
    export FHROT=$FHBAK
    export SUFOUT=.lr.$CDUMP.$CDATE
    if [ $FHINI -lt $FHMAX ] ; then
      if [ $COUP_FCST = YES ] ; then
        export VDATE=`$NDATE $FHINI $CDATE`
        export RESTART_CONTROL_FILE=${RESTART_CONTROL_FILE:-$RESDIR/pr$PSLOT.2restart$SUFOUT}
        if [ -s $RESTART_CONTROL_FILE ] ; then
          mv $RESTART_CONTROL_FILE ${RESTART_CONTROL_FILE}_$FHINI2
        fi
#       if [ $FHINI -eq 0 ] ; then rm -f $RESTART_CONTROL_FILE ; fi
#       export AM_SST=${AM_SST:-$RESDIR/AM_SST_${FHINI}_${FHMAX}}
        export AM_SST=$RESDIR/AM_SST_${FHINI}_${FHMAX}
        export INCHOUR=$((FHMAX-FHINI))

        date

        $FORECASTSH \
        - $DATA $VDATE $EXEC_OMD $COMROT \
        >$DATA/$(basename $FORECASTSH).out.$FHINI 2>$DATA/$(basename $FORECASTSH).err.$FHINI

        date

        rc=$?
        if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
#
        $LINKFILESH

#       cstr=$(((FHMAX-FHINI)*3600/DELTIM))
        cstr=$((FHMAX*3600/DELTIM))
        ${NCP:-/bin/cp} -p $DATA/fluxes_for_OM_$cstr $DATA/fluxes_for_OM
        ${NCP:-/bin/cp} -p $DATA/RESTART/* $DATA/INPUT/
      else
#       export FHINI=$nhourb
#       export FHMAX=$nhours
        $FORECASTSH
        rc=$?
        if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
      fi
    else
      echo 'FHINI >= FHMAX - run aborted'
    fi
#
    export FHINI=$FHINI2
    export FHMAX=$FHMAX2
    export FHRES=$FHRES2
    export FHROT=0
    export SUFOUT=$SUFOUT2
    export SIGI=sig1lr
    export SIGI2=sig2lr
    export SFCI=sfclr
    if [ $NST_FCST -gt 0 ] ; then
      export NSTI=nstlr
    fi
    USE_RESTART=YES
#
    if [ $($SIGHDR $SIGI idvc) -eq 3 -a $($SIGHDR $SIGI idvm) -eq 32 ] ; then
      export RIlist="   287.05, 461.50, 173.2247,    0.0"
      export CPIlist=" 1004.6, 1846.0, 820.2391,    0.0"
      export TRACERVARS="RI=$RIlist,CPI=$CPIlist,"
    fi
#
    if [ $COUP_FCST = YES ] ; then
      FHREST=$FHINI
    fi
  else
    export FHROT=$FHINI
  fi
 fi
fi
#
#     To synchronize post -- not tested yet
#
if [ $USE_RESTART = YES ] ; then
 if [ -s $COMROT/FHREST.$CDUMP.$CDATE.$nknd ] ; then
  rm $COMROT/FHREST.$CDUMP.$CDATE.$nknd
 fi
 echo $FHINI >$COMROT/FHREST.$CDUMP.$CDATE.$nknd
fi

$PBEG                 ; # This should start running concurrent post

#
################################################################################
# Run forecast

if [ $FHINI -ge $FHMAX ] ; then $PEND ; exit ; fi
#
export SIGR1=$RESDIR/sig1r${FHMAX}${SUFOUT}
export SIGR2=$RESDIR/sig2r${FHMAX}${SUFOUT}
export SFCR=$RESDIR/sfcr${FHMAX}${SUFOUT}
if [ $NST_FCST -gt 0 ] ; then
 export NSTR=$RESDIR/nstr${FHMAX}${SUFOUT}
fi
#
if [ $COUP_FCST = YES ] ; then
#
#   The time loop for the coupled run in $INCHOUR hr increment
#
  if [ $nknd -gt 1 -o $FHMAX -ge 24 ] ; then
    export diagtable=$diagtable_long
    export dt_rstrt=$(eval echo \$dt_rstrt_long_$nknd)
    if [ $do_irestart = .false. ] ; then
      dt_rstrt=$((INCH*3600))
    fi
  fi
#
#########################################################################
# if [ $AVG_FCST = YES ] ; then
#   xdate=$($NDATE $FHINI $CDATE)
#   if [ $AVG_INT -eq 999 ] ; then     # Monthly mean
#     ydate=$(echo $xdate | cut -c1-6)0100
#     if [ $ydate -gt $CDATE ] ; then
#       PDATE=$ydate
#     else
#       PDATE=$CDATE
#     fi
#   else
#     ydate=$CDATE
#     until  [[ $ydate -gt $xdate ]] ; do
#       PDATE=$ydate
#       ydate=$($NDATE $AVG_INT $PDATE)
#     done
#   fi
# fi
#
#########################################################################
#
  export nhourb=$FHINI
  export nhours=$FHMAX
  if [ $INCH -gt $((FHMAX-FHINI)) ] ; then INCH=$((FHMAX-FHINI)) ; fi
  until [[ $nhourb -ge $nhours ]]; do
    export FHINI=$((nhourb+0))
    if [[ $FHINI -lt 10 ]] ; then FHINI=0$FHINI ; fi
    export FHMAX=$((nhourb+$INCH))
    if [ $FHMAX -eq $nhours -a $nknd -lt $mfcst -a $JCAP -ne $JCAPm ] ; then
      mdate=$($NDATE $((FHMAX-12)) $CDATE)
#   mdate=$($NDATE -12 $mdate)
#   mdate=$(echo $mdate | cut -c1-8)00
      if [ $mdate -gt $CDATE -a $INCH -ge 24 ] ; then
        export FHMAX=$($NHOUR $mdate $CDATE)
        if [ $((FHMAX-(FHMAX/24)*24)) -ne 0 ] ; then export FHRES=$FHMAX ; fi
      fi
    fi
    if [ $FHMAX -gt $nhours ] ; then export FHMAX=$nhours ; fi
    export INCHOUR=${INCH:-$((FHMAX-FHINI))}
#   export INCHOUR=${INCHOUR:-$((FHMAX-FHINI))}
    export FHMAX=$((FHMAX+0))
    if [ $FHRES -gt 24 -a $(((FHMAX/FHRES)*FHRES)) -ne $FHMAX ] ; then
      export FHRES=$FHMAX
    fi
    if [ $(((FHMAX*3600/dt_rstrt)*(dt_rstrt/3600))) -ne $FHMAX ] ; then
      dt_rstrt=$((FHMAX*3600))
      export INCHOUR=$((FHMAX-FHINI))
    fi
#   export FHRES=$FHMAX
    if [[ $FHMAX -lt 10 ]] ; then export FHMAX=0$FHMAX ; fi
#
    export SIGR1=$DATA/sig1r${FHMAX}${SUFOUT}
    export SIGR2=$DATA/sig2r${FHMAX}${SUFOUT}
    export SFCR=$DATA/sfcr${FHMAX}${SUFOUT}
    if [ $NST_FCST -gt 0 ] ; then
     export NSTR=$DATA/nstr${FHMAX}${SUFOUT}
    fi
#   export SIGR1=$RESDIR/sig1r${FHMAX}${SUFOUT}
#   export SIGR2=$RESDIR/sig2r${FHMAX}${SUFOUT}
#   export SFCR=$RESDIR/sfcr${FHMAX}${SUFOUT}
#
    export VDATE=`$NDATE $FHINI $CDATE`
    export RESTART_CONTROL_FILE=${RESTART_CONTROL_FILE:-$RESDIR/pr$PSLOT.2restart$SUFOUT}
    if [ $FHINI -eq 0 ] ; then rm -f $RESTART_CONTROL_FILE ; fi
#   export AM_SST=${AM_SST:-$RESDIR/AM_SST_${FHINI}_${FHMAX}}
    export AM_SST=$RESDIR/AM_SST_${FHINI}_${FHMAX}

    echo ' Present Working Directory = ',$(pwd)
    echo $DATA

    date

    $FORECASTSH \
    - $DATA $VDATE $EXEC_OMD $COMROT \
    >$DATA/$(basename $FORECASTSH).out.$FHINI 2>$DATA/$(basename $FORECASTSH).err.$FHINI
#
    date

    rc=$?
    if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

    $LINKFILESH

#
#   ${NCP:-cp} $DATA/restart.dta      $RESDIR/restart.dta.$CDATE
#   ${NCP:-cp} $DATA/date.suffix.rest $RESDIR/date.suffix.rest.$CDATE
#   FDATE=$($NDATE $FHMAX $CDATE)
#   ${NCP:-cp} $DATA/fluxes_for_OM    $RESDIR/fluxes_for_OM.$CDUMP.$CDATE.$FDATE
#   ${NCP:-/bin/cp} -p $DATA/RESTART/* $DATA/INPUT/
#
    if [ $do_irestart = .true. ] ; then
      nres=$((FHMAX*3600/dt_rstrt-FHBEG*3600/dt_rstrt))
    else
      nres=1
    fi
#   if [ $dt_rstrt -lt $((FHMAX-FHINI)*3600) ] ; then
    if [ $nres -gt 1 ] ; then
#            This section archives the OM restarts at interval of dt_rstrt
      cd $DATA/IRESTART
      nres=$((FHMAX*3600/dt_rstrt-FHBEG*3600/dt_rstrt))
      n=$((FHINI*3600/dt_rstrt+1))
      until [ $n -gt $nres ] ; do
        FDATE=$($NDATE $((FHMAX-(nres-n)*dt_rstrt/3600)) $CDATE)
        cstr=$((n*dt_rstrt/DELTIM))
#       mv $FDATE.coupler.res coupler.res
#       mv $FDATE.grid_spec.nc grid_spec.nc
#       mv $FDATE.ocean_density.res.nc ocean_density.res.nc
#       mv $FDATE.ocean_frazil.res.nc ocean_frazil.res.nc
#       mv $FDATE.ocean_freesurf.res.nc ocean_freesurf.res.nc
#       mv $FDATE.ocean_neutral.res.nc ocean_neutral.res.nc
#       mv $FDATE.ocean_sbc.res.nc ocean_sbc.res.nc
#       mv $FDATE.ocean_temp_salt.res.nc ocean_temp_salt.res.nc
#       mv $FDATE.ocean_tracer.res ocean_tracer.res
#       mv $FDATE.ocean_velocity.res.nc ocean_velocity.res.nc
#       mv $FDATE.ocean_velocity_advection.res.nc ocean_velocity_advection.res.nc
#       mv $FDATE.oisst_clim.nc oisst_clim.nc
#       mv $FDATE.r2ts_clim.nc r2ts_clim.nc
#       mv $FDATE.salt_sfc_restore.nc salt_sfc_restore.nc
#       mv $FDATE.sst_ice_clim.nc sst_ice_clim.nc
#       mv $FDATE.temp_sfc_restore.nc temp_sfc_restore.nc

        tar -cvf $RESDIR/omrestart$SUFOUT.$FDATE.tar ${FDATE}*
        ${NCP:-/bin/cp} -p $DATA/fluxes_for_OM_$cstr $RESDIR/fluxes_for_OM$SUFOUT.$FDATE
        rc=$?
        if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
        n=$((n+1))
      done
    else
                                   # This archives the last OM restart
      cd $DATA/RESTART
      FDATE=$($NDATE $FHMAX $CDATE)
      tar -cvf $RESDIR/omrestart$SUFOUT.$FDATE.tar *
      cstr=$(((FHMAX-FHBEG)*3600/DELTIM))
      ${NCP:-/bin/cp} -p $DATA/fluxes_for_OM_$cstr $RESDIR/fluxes_for_OM$SUFOUT.$FDATE
      rc=$?
      if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
    fi
#
#########################################################################
#                     For time averaging of forecasts
#   if [ $AVG_FCST = YES ] ; then
#     if [ $AVG_INT -eq 999 ] ; then     # Monthly mean
#       if [ $PDATE -gt $CDATE ] ; then
#        xdate=$(echo $($NDATE 768 $PDATE) | cut -c1-6)0100
#       else
#        xdate=$(echo $($NDATE 768 $(echo $CDATE | cut -c1-6)0100) | cut -c1-6)0100
#       fi
#     else
#       xdate=$(echo $($NDATE $AVG_INT $PDATE))
#     fi
#     if [ $FDATE -ge $xdate ] ; then
#       export edate=$xdate
#       export sdate=$($NDATE $FHOUT $PDATE)
#       PDATE=$xdate
#       $PAVG
#     fi
#   fi
#########################################################################
#
    date

    nhourb=$FHMAX
    if [ $nhourb -lt $nhours ] ; then
      cd $DATA
      ls -l $SIGR1 ; ls -l $SIGR2 ; ls -l $SFCR
      ${NCP:-/bin/cp} $SIGR1 $DATA/sigr1a
      ${NCP:-/bin/cp} $SIGR2 $DATA/sigr2a
      ${NCP:-/bin/cp} $SFCR  $DATA/sfcra
      if [ $NST_FCST -gt 0 ] ; then
        ls -l $NSTR
        ${NCP:-/bin/cp} $NSTR  $DATA/nstra
      fi
      export SIGI=$DATA/sigr1a
      export SIGI2=$DATA/sigr2a
      export SFCI=$DATA/sfcra
      if [ $NST_FCST -gt 0 ] ; then
        export NSTI=$DATA/nstra
      fi
      ${NCP:-/bin/cp} -p $DATA/fluxes_for_OM_$cstr $DATA/fluxes_for_OM
      mv $DATA/time_stamp.out $DATA/time_stamp.out_$nhourb
      ${NCP:-/bin/cp} -p $DATA/RESTART/* $DATA/INPUT/
    fi
#             Move the AM restart files to RESTART directory
    if [ -s $SIGR1 ] ; then
      ${NCP:-/bin/cp} -p  $SIGR1 $RESDIR
      ${NCP:-/bin/cp} -p  $SIGR2 $RESDIR
      ${NCP:-/bin/cp} -p  $SFCR  $RESDIR
      if [ $NST_FCST -gt 0 ] ; then
        ${NCP:-/bin/cp} -p  $NSTR $RESDIR
      fi
    fi
  done
else
  if [ $FHINI -lt $FHMAX ] ; then
    $FORECASTSH
  else
   echo 'FHINI >= FHMAX - run aborted'
  fi
fi
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

################################################################################
# Copy out restart and output files

#$PCOP $CDATE/$CDUMP/$CSTEP/ROTO $DATA $COMROT <$RLIST

#/bin/mv $COMROT/date.suffix.rest $COMROT/date.suffix.rest.$CDMUP.$CDATE
#/bin/mv $COMROT/fluxes_for_OM    $COMROT/fluxes_for_OM.$CDUMP.$CDATE

#rc=$?

################################################################################
# Exit gracefully

#if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
#
# Remove all restart files from the RESTART directory when forecast is done
#                    disabled for now
#if [[ $mfcst -eq $nknd ]] ; then
# rm -f $RESDIR/sig1r* $RESDIR/sig2r* $RESDIR/sfcr* $RESDIR/nstr*
#fi
$PEND
