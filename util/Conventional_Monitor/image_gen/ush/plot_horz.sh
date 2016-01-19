#!/bin/sh

#----------------------------------------------------------------------------------------
#  plot_horz.sh
#
#    This produces the horizontal temperature images.
#----------------------------------------------------------------------------------------
##  the script to plot scater image for temperature

set -ax
date

echo "--> plot_horz.sh"


rc=0
pdy=`echo $PDATE|cut -c1-8`
cyc=`echo $PDATE|cut -c9-10`
hh_tankdir=${C_TANKDIR}/cmon.${pdy}/horz_hist

export xsize=x800
export ysize=y600

export tmpdir_plothorz=${C_PLOT_WORKDIR}/plothorz
rm -rf $tmpdir_plothorz
mkdir -p $tmpdir_plothorz
cd $tmpdir_plothorz


#----------------------------------------------------------------------
#  Link in the analysis and guess data files
#----------------------------------------------------------------------
ln -s ${hh_tankdir}/anl/anal.${PDATE}  anal.${PDATE}
ln -s ${hh_tankdir}/ges/guess.${PDATE} guess.${PDATE}


#----------------------------------------------------------------------
#  create the idx and ctl files for ges|anl grib|grib2 files
#----------------------------------------------------------------------
echo "grib2 = $grib2"

if [[ $grib2 -eq 0 ]]; then		# grib files
#   `module load wgrib`
   echo "handling grib files"
   ${C_IG_SCRIPTS}/grib2ctl.pl anal.${PDATE} > anal.ctl
   gribmap -i anal.ctl -0
   ${C_IG_SCRIPTS}/grib2ctl.pl -verf guess.${PDATE} > guess.ctl
   gribmap -i guess.ctl
else					# grib2
#   `module load wgrib2`
   echo "handling grib2 files"
   ${C_IG_SCRIPTS}/g2ctl.pl -0 anal.$PDATE > anal.ctl
   gribmap -0 -i anal.ctl
   ${C_IG_SCRIPTS}/g2ctl.pl guess.$PDATE > guess.ctl
   gribmap -i guess.ctl
fi

#----------------------------------------------------------------------
#  Link to required grads tools
#----------------------------------------------------------------------
ln -s ${C_IG_GSCRIPTS}/rgbset2.gs ./rgbset2.gs 
ln -s ${C_IG_GSCRIPTS}/page.gs ./page.gs 
ln -s ${C_IG_GSCRIPTS}/defint.gs ./defint.gs 
ln -s ${C_IG_GSCRIPTS}/setvpage.gs ./setvpage.gs


for type in ps q t; do

   eval stype=\${${type}_TYPE} 
   eval nreal=\${nreal_${type}} 


   for dtype in ${stype}; do
      mtype=`echo ${dtype} | cut -f1 -d_`
      subtype=`echo ${dtype} | cut -f2 -d_`

      for cycle in ges anl; do
         nt=1

         #---------------------------------------
         #  build the control file for the data
         #---------------------------------------
         if [ "$mtype" = 'ps180' -o "$mtype" = 'ps181' -o  "$mtype" = 'ps183' -o "$mtype" = 'ps187'  ]; then

            cp ${C_IG_FIX}/pstime.ctl ./${dtype}.ctl
            cp ${C_IG_GSCRIPTS}/plot_ps_horz.gs ./plot_${dtype}.gs

            if [ -s ${hh_tankdir}/${cycle}/nt_${dtype}.${PDATE} ]; then
               echo "LOCATED nt file"
               nt=`cat ${hh_tankdir}/${cycle}/nt_${dtype}.${PDATE}`
               echo "nt set to $nt"
            fi

         elif [ "$mtype" = 'ps120' ]; then

            cp ${C_IG_FIX}/pssfc.ctl ./${dtype}.ctl
            cp ${C_IG_GSCRIPTS}/plot_ps_horz.gs ./plot_${dtype}.gs
            nt=1

         elif [ "$mtype" = 't120' ]; then

            cp ${C_IG_FIX}/tmandlev.ctl ./${dtype}.ctl
            cp ${C_IG_GSCRIPTS}/plot_tallev_horz.gs ./plot_${dtype}.gs
            nt=1

         elif [ "$mtype" = 't180' -o "$mtype" = 't181' -o "$mtype" = 't182' -o "$mtype" = 't183' -o "$mtype" = 't187'  ]; then

            cp ${C_IG_FIX}/tsfc.ctl ./${dtype}.ctl
            cp ${C_IG_GSCRIPTS}/plot_tsfc_horz.gs ./plot_${dtype}.gs
            nt=1

         elif [ "$mtype" = 't130' -o "$mtype" = 't131' -o "$mtype" = 't132' -o "$mtype" = 't133' -o "$mtype" = 't134' -o "$mtype" = 't135' ]; then

            cp ${C_IG_FIX}/tallev.ctl ./${dtype}.ctl
            cp ${C_IG_GSCRIPTS}/plot_tallev_horz.gs ./plot_${dtype}.gs
            nt=1

         elif [ "$mtype" = 'q120' ]; then

            cp ${C_IG_FIX}/qmandlev.ctl ./${dtype}.ctl
            cp ${C_IG_GSCRIPTS}/plot_qallev_horz.gs ./plot_${dtype}.gs
            nt=1

         elif [ "$mtype" = 'q180' -o "$mtype" = 'q181' -o  "$mtype" = 'q183' -o "$mtype" = 'q187'  ];then
            cp ${C_IG_FIX}/qsfc.ctl ./${dtype}.ctl
            cp ${C_IG_GSCRIPTS}/plot_qsfc_horz.gs ./plot_${dtype}.gs
            if [ -s ${hh_tankdir}/${cycle}/nt_${dtype}.${PDATE} ]; then
               echo "LOCATED nt file"
               nt=`cat ${hh_tankdir}/${cycle}/nt_${dtype}.${PDATE}`
               echo "nt set to $nt"
            fi

         elif [ "$mtype" = 'q130' -o "$mtype" = 'q131' -o "$mtype" = 'q132' -o "$mtype" = 'q133' -o "$mtype" = 't134' ]; then
            cp ${C_IG_FIX}/qallev.ctl ./${dtype}.ctl
            cp ${C_IG_GSCRIPTS}/plot_qallev_horz.gs ./plot_${dtype}.gs
            nt=1

#         elif [ "$mtype" = 'uv220' ]; then
#
#            cp $CTLDIR/uvmandlev.ctl ./${dtype}.ctl
#            cp $GSCRIPTS/plot_uvallev_horz.gs ./plot_${dtype}.gs
#            nt=1
#
#         elif  [ "$mtype" = 'uv223' -o "$mtype" = 'uv224' -o "$mtype" = 'uv228' ]; then
#
#            cp $CTLDIR/uvsig.ctl ./${dtype}.ctl
#            cp $GSCRIPTS/plot_uvallev_horz.gs ./plot_${dtype}.gs
#            nt=1
#
#         elif  [ "$mtype" = 'uv221' -o "$mtype" = 'uv230' -o "$mtype" = 'uv231' -o "$mtype" = 'uv232' -o "$mtype" = 'uv233' -o "$mtype" = 'uv234' -o "$mtype" = 'uv235' ]; then
#
#            cp $CTLDIR/uvallev.ctl  ./${dtype}.ctl
#            cp $GSCRIPTS/plot_uvallev_horz.gs ./plot_${dtype}.gs
#            nt=1

         fi


         sdir=" dset ${dtype}_grads_${cycle}.${PDATE}"
         title="title  ${dtype}  ${cycle}"
         sed -e "s/^title.*/${title}/" ${dtype}.ctl >tmp.ctl
         echo $sdir >${dtype}_grads_${cycle}.ctl
         cat tmp.ctl >>${dtype}_grads_${cycle}.ctl
         rm -f tmp.ctl
         rm -f ${dtype}.ctl


         #--------------------------------------------------------------
         #  link in the ${dtype}_grads.${PDATE} data file from TANKDIR
         #--------------------------------------------------------------
         grads_file=${hh_tankdir}/${cycle}/${dtype}_grads.${PDATE}

         if [ -s ${grads_file} ]; then
            ln -s ${grads_file} ${dtype}_grads_${cycle}.${PDATE} 
         else
            echo "WARNING:  unable to locate ${grads_file}"
            continue
         fi

         stnmap -i ${dtype}_grads_${cycle}.ctl 

      done         ## done with cycle


      ### set up plot variables
#      hint=0
      sed -e "s/XSIZE/$xsize/" \
          -e "s/YSIZE/$ysize/" \
          -e "s/PLOTFILE/$mtype/" \
          -e "s/PLOT2/$dtype/" \
          -e "s/RDATE/$PDATE/" \
          -e "s/HINT/${hint}/" \
          -e "s/NT/$nt/" \
         plot_${dtype}.gs >plothorz_${dtype}.gs

      $GRADS -blc "run plothorz_${dtype}.gs" 

      mkdir -p ${C_IMGNDIR}/pngs/horz/${CYC}
      cp -f *.png ${C_IMGNDIR}/pngs/horz/${CYC}/.

#      rm -f *png

   done      ### dtype loop 

done      ### type loop


#cd $tmpdir_plothorz
#rm -rf  $PDATE
#cd ..
#rm -rf $tmpdir_plothorz


echo "<-- plot_horz.sh"

exit $rc

