#/bin/sh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_atmo_conmon.sh
# Script description:  Runs data extract/validation for global conventional diag data
#
# Author:        Ed Safford       Org: NP23         Date: 2015-09
#
# Abstract: This script runs the data extract/validation portion of the 
#           ConMon package.  
#
#   Input script positional parameters:
#     1             Current analysis date in yyyymmddhh format
#                   defaults to PDY; required
#     2             cycle time in cc format
#                   defaults to CYC; required
#
#
#    Condition codes
#       0 - no problem encountered
#      >0 - some problem encountered
#
################################################################################

   export VERBOSE=${VERBOSE:-"NO"} 
   if [[ "$VERBOSE" = "YES" ]]
   then
      echo start exgdas_vrfyconv.sh.sms
      set -x
   fi

   export RUN_ENVIR=${RUN_ENVIR:-nco}
   export NET=${NET:-gfs}
   export RUN=${RUN:-gdas}
   export envir=${envir:-prod}
   export component=${component:-atmos}

   #  Command line arguments
   export PDY=${1:-${PDY:?}} 
   export CYC=${2:-${CYC:?}}

   #  Directories
   export C_DATA=${C_DATA:-$(pwd)/workdir}			# work directory
   export C_COM_IN=${C_COM_IN:-/${COMROOT}/${NET}/${envir}}
   export C_COMIN=${C_COMIN:-$C_COM_IN/${RUN}.${PDY}}

   export TANKDIR_conmon=${C_TANKDIR}/gdas.${PDY}/${CYC}/conmon

   export GDATE=`$NDATE -06 $PDY$CYC`
   export PDYm6h=`echo $GDATE|cut -c1-8`
   export GCYC=`echo $GDATE|cut -c9-10`
   export C_COMINm6h=${C_COMINm6h:-${C_COM_IN}/${RUN}.${PDYm6h}}
   export TANKDIR_prev_conmon=${C_TANKDIR}/gdas.${PDYm6h}/${GCYC}/conmon

   #  Filenames
   cnvstat=${cnvstat:-${C_COMIN}/${CYC}/${component}/gdas.t${CYC}z.cnvstat}
   if [[ ! -e ${cnvstat} ]]; then
      cnvstat=${C_COMIN}/gdas.t${CYC}z.cnvstat}
   fi 
   export cnvstat=${cnvstat}

   #----------------
   # analysis file:
   #
   pgrbf00=${pgrbf00:-${C_COMIN}/${CYC}/${component}/gdas.t${CYC}z.pgrb2.0p25.f000}
   if [[ ! -e ${pgrbf00} ]]; then
      pgrbf00=${C_COMIN}/gdas.t${CYC}z.pgrb2b.1p00.anl}
   fi
   export pgrbf00=${pgrbf00}

   #--------------
   # guess file:
   #
   pgrbf06=${pgrbf06:-${C_COMINm6h}/${GCYC}/${component}/gdas.t${GCYC}z.pgrb2.0p25.f006}  
   if [[ ! -e ${pgrbf06} ]]; then
      pgrbf06=${C_COMINm6h}/gdas.t${GCYC}z.pgrb2b.1p00.anl}
   fi
   export pgrbf06=${pgrbf06}

   export convinfo=${convinfo:-${FIXgdas}/global_convinfo.txt}	
   export conmon_base=${conmon_base:-${HOMEgdas_conmon}/fix/gdas_conmon_base.txt}

   echo "cnvstat = $cnvstat"
   echo "pgrbf00 = $pgrbf00"
   echo "pgrbf06 = $pgrbf06"


   #  Other variables
   export NCP=${NCP:-/bin/cp -f}
   export NDATE=${NDATE:-/nwprod/util/exec/ndate}
   export PDATE=${PDY}${CYC}

   #####################################################################
   # Preprocessing
   $INISCRIPT

   if [[ ! -d ${C_DATA} ]]; then
      mkdir $C_DATA
   fi
   cd $C_DATA
   export workdir=$C_DATA

   #--------------------------------------------------------
   #  Ensure necessary TANKDIR directories are in place
   #--------------------------------------------------------
   if [[ ! -d ${TANKDIR_conmon} ]]; then
      mkdir -p ${TANKDIR_conmon}
   fi
   if [[ ! -d ${TANKDIR_prev_conmon} ]]; then
      mkdir -p ${TANKDIR_prev_conmon}
   fi

   if [[ ! -d ${C_TANKDIR_conmon}/horz_hist ]]; then
      mkdir -p ${TANKDIR_conmon}/horz_hist
      mkdir -p ${TANKDIR_conmon}/horz_hist/anl
      mkdir -p ${TANKDIR_conmon}/horz_hist/ges
   fi

   if [[ ! -d ${TANKDIR_prev_conmon}/horz_hist ]]; then
      mkdir -p ${TANKDIR_prev_conmon}/horz_hist
      mkdir -p ${TANKDIR_prev_conmon}/horz_hist/anl
      mkdir -p ${TANKDIR_prev_conmon}/horz_hist/ges
   fi

   if [[ ! -d ${TANKDIR_conmon}/time_vert ]]; then
      mkdir -p ${TANKDIR_conmon}/time_vert
   fi
   if [[ ! -d ${TANKDIR_prev_conmon}/time_vert ]]; then
      mkdir -p ${TANKDIR_prev_conmon}/time_vert
   fi


   if [[ "$VERBOSE" = "YES" ]]; then
      if [[ -s ${cnvstat} ]]; then
         echo "$cnvstat is available"
      fi
      if [[ -s ${pgrbf00} ]]; then
         echo "$pgrbf00 is available"
      fi
      if [[ -s ${pgrbf06} ]]; then
         echo "$pgrbf06 is available"
      fi
   fi
   #####################################################################

   data_available=0
   if [[ -s ${cnvstat} && -s ${pgrbf00} && -s ${pgrbf06} ]]; then
      data_available=1                                         

      #------------------------------------------------------------------
      #  Copy data files file to local data directory.  
      #  Untar cnvstat file.  
      #------------------------------------------------------------------

      export grib2=${grib2:-1}   
      $NCP $cnvstat ./cnvstat.$PDATE
      $NCP $pgrbf00 ./pgbanl.$PDATE
      $NCP $pgrbf06 ./pgbf06.$GDATE

      tar -xvf ./cnvstat.$PDATE
      #rm cnvstat.$PDATE
   
      netcdf=0
      count=`ls diag* | grep ".nc4" | wc -l`
      if [ $count -gt 0 ] ; then
         netcdf=1
         for filenc4 in `ls diag*nc4.gz`; do
            file=`echo $filenc4 | cut -d'.' -f1-2`.gz
            mv $filenc4 $file
         done
      fi

      export CONMON_NETCDF=${netcdf}
      $UNCOMPRESS ./*.${Z}


      #------------------------------------------------------------------
      #  NOTE:  The anal (f00) and guess (f06) grib files
      #         contain more information than we need for the subsequent
      #         image generation.  Use wgrib and awk to reduce the file
      #         size.  The resulting files contain only RH, PRES, TMP,
      #            and UV data.
      #           
      #  NOTE:  The f06 file is taken from the GDATE cycle (the previous 
      #         cycle but is stored in TANKDIR in the 
      #         $NET.$PDATE/$cyc/conmon/horz_hist/ges subdirectory.
      #  
      #  NOTE:  In order to use the anal and guess files in GrADS we 
      #         need an index (.idx) and control (.ctl) file.  These can
      #         be created in the plot process and don't need to be 
      #         stored.
      #------------------------------------------------------------------
      echo "grib2 = $grib2"
      if [[ $grib2 -eq 0 ]]; then

         ${WGRIB} -s pgbanl.${PDATE} | awk '(/:RH:/ && /mb:/) || (/:RH:/ && /:2 m/) || (/:PRES:sfc/) || (/:UGRD:/ && /mb:/) || (/:VGRD:/ && /mb:/) || (/:TMP:/ && /mb:/)' | ${WGRIB} -i -grib pgbanl.${PDATE} -o ./pared_anal.${PDATE}

  
         ${WGRIB} -s pgbf06.${GDATE} | awk '(/:RH:/ && /mb:/) || (/:RH:/ && /:2 m/) || (/:PRES:sfc/) || (/:UGRD:/ && /mb:/) || (/:VGRD:/ && /mb:/) || (/:TMP:/ && /mb:/)' | ${WGRIB} -i -grib pgbf06.${GDATE} -o ./pared_guess.${PDATE}

      else
      
         ${WGRIB2} pgbanl.${PDATE} | awk '(/:RH:/ && /mb:/) || (/:RH:/ && /:2 m a/) || (/:PRES:surface:/) || (/:UGRD:/ && /mb:/) || (/:UGRD:/ && /:10 m a/) || (/:VGRD:/ && /mb:/) || (/:VGRD:/ && /:10 m a/) || (/:TMP:/ && /mb:/) || (/:TMP:/ && /:surface:/)' | ${WGRIB2} pgbanl.${PDATE} -i -grib  ./pared_anal.${PDATE}

         ${WGRIB2} pgbf06.${GDATE} | awk '(/:RH:/ && /mb:/) || (/:RH:/ && /:2 m a/) || (/:PRES:surface:/) || (/:UGRD:/ && /mb:/) || (/:UGRD:/ && /:10 m a/) || (/:VGRD:/ && /mb:/) || (/:VGRD:/ && /:10 m a/) || (/:TMP:/ && /mb:/) || (/:TMP:/ && /:surface:/)' | ${WGRIB2} pgbf06.${GDATE} -i -grib ./pared_guess.${PDATE}

      fi

      ${NCP} ./pared_anal.${PDATE} ${TANKDIR_conmon}/horz_hist/anl/anal.${PDATE}
      ${NCP} ./pared_guess.${PDATE} ${TANKDIR_conmon}/horz_hist/ges/guess.${PDATE}

 
      #------------------------------------------------------------------
      #   Run the child sccripts.
      #    -->  get unique rc values for each child, use in error reporting below
      #------------------------------------------------------------------

      #---------------------------------------
      #  run the horz-hist extraction script
      #
      ${USHconmon}/horz_hist.sh
      rc_horz_hist=$?
      echo "rc_horz_hist = $rc_horz_hist"

      #---------------------------------------
      #  run the time-vert extraction script
      #
      ${USHconmon}/time_vert.sh 
      rc_time_vert=$?
      echo "rc_time_vert = $rc_time_vert"

      #--------------------------------------
      #  optionally run clean_tankdir script
      #   
      if [[ ${CLEAN_TANKDIR} -eq 1 ]]; then
         ${USHconmon}/clean_tankdir.sh
         rc_clean_tankdir=$?
         echo "rc_clean_tankdir = $rc_clean_tankdir"
      fi
   fi

   #####################################################################
   # Postprocessing
   # 
   err=0
   if [[ ${data_available} -ne 1 ]]; then
      err=1
   elif [[ $rc_horz_hist -ne 0 ]]; then
      echo "ERROR repored from horz_hist.sh:  $rc_horz_hist"
      err=$rc_horz_hist
   elif [[ $rc_time_vert -ne 0 ]]; then
      echo "ERROR repored from time_vert.sh:  $rc_time_vert"
      err=$rc_time_vert
   fi

   if [[ "$VERBOSE" = "YES" ]]; then
      echo "end exgdas_conmon.sh.sms, exit value = ${err}"
   fi


   set +x

exit ${err}

