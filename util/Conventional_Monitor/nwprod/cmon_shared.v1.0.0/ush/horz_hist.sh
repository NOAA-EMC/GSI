#!/bin/sh

#------------------------------------------------------------------
#
#  horz_hist.sh
#
#------------------------------------------------------------------

   set -ax

   echo "--> horz_hist.sh"
   date
   rc=0
  
   echo "CMON_SUFFIX = $CMON_SUFFIX"
   export hint=10    ##(mb) the plot pressure interval press+-hint

   #----------------------------------------------------------
   # The list of data type, based on convinfo.txt file
   #----------------------------------------------------------
   ps_TYPE=" ps120_00 ps180_00 ps181_00 ps183_00 ps187_00 "
   q_TYPE=" q120_00 q130_00 q132_00 q133_00 q134_00 q135_00 q180_00 q181_00 q182_00 q183_00 q187_00 "
   t_TYPE=" t120_00 t130_00 t131_00 t132_00 t133_00 t134_00 t135_00 t180_00 t181_00 t182_00 t183_00 t187_00 "
   uv_TYPE=" uv220_00 uv221_00 uv223_00 uv224_00 uv228_00 uv229_00 uv230_00 uv231_00 uv232_00 uv233_00 uv234_00 uv235_00 uv242_00 uv243_00 uv243_55 uv243_56 uv245_00 uv245_15 uv246_00 uv246_15 uv247_00 uv248_00 uv249_00 uv250_00 uv251_00 uv252_00 uv253_00 uv253_55 uv253_56 uv254_00 uv254_55 uv254_56 uv255_00 uv256_00 uv257_00 uv258_00 uv280_00 uv281_00 uv282_00 uv284_00 uv287_00"


   echo TANKDIR_cmon = $TANKDIR_cmon

   mkdir -p ${TANKDIR_cmon}/horz_hist/ges
   mkdir -p ${TANKDIR_cmon}/horz_hist/anl

   export nreal_ps=${nreal_ps:-17}
   export nreal_q=${nreal_q:-18}
   export nreal_t=${nreal_t:-22}
   export nreal_uv=${nreal_uv:-21}


   for type in ps q t uv; do

      eval stype=\${${type}_TYPE}
      eval nreal=\${nreal_${type}}
      exec=read_${type}

      #---------------------------------
      #  decoding the dignostic file
      #---------------------------------

      for dtype in ${stype}; do

         mtype=`echo ${dtype} | cut -f1 -d_`
         subtype=`echo ${dtype} | cut -f2 -d_`

         if [[ "$VERBOSE" = "YES" ]]; then
            echo "DEBUG:  dtype = $dtype"
            echo "mtype, subtype = $mtype, $subtype"
         fi

         for cycle in ges anl; do
            cp ./diag_conv_${cycle}.${PDATE} ./conv_diag   # this appears to be unneeded?

            if [[ "$VERBOSE" = "YES" ]]; then
               echo "cycle = $cycle"
            fi 

            ${USHcmon}/diag2grad_${type}_case.sh

         done    #### done with cycle

      done   ### done with dtype

   done   ### done with type


echo "<-- horz_hist.sh"

exit ${rc}

