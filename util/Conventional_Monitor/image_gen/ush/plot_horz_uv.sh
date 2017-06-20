#!/bin/sh
set -ax

#---------------------------------------------------------------------
#
#  plot_horz_uv.sh
#
#  plot scater image for temperature
#---------------------------------------------------------------------
echo "--> plot_horz_uv.sh"

rc=0
pdy=`echo $PDATE|cut -c1-8`
cyc=`echo $PDATE|cut -c9-10`
hh_tankdir=${C_TANKDIR}/cmon.${pdy}/horz_hist
export savedir=${hh_tankdir}

export tmpdir_plothorz=${C_PLOT_WORKDIR}/plothorz_uv
rm -rf $tmpdir_plothorz
mkdir -p $tmpdir_plothorz
cd $tmpdir_plothorz

export xsize=x800
export ysize=y600
export NCP="cp -f"
#export hint=0

#----------------------------------------------------------------------
#  Link to required data and control files
#----------------------------------------------------------------------
ln -s ${hh_tankdir}/anl/anal.${PDATE}  anal.${PDATE}
ln -s ${hh_tankdir}/ges/guess.${PDATE} guess.${PDATE}


#----------------------------------------------------------------------
#  create the idx and ctl files for ges|anl grib|grib2 files
#----------------------------------------------------------------------
echo "grib2 = $grib2"

if [[ $grib2 -eq 0 ]]; then             # grib files
#   `module load wgrib`
   echo "handling grib files"
   ${C_IG_SCRIPTS}/grib2ctl.pl anal.${PDATE} > anal.ctl
   gribmap -i anal.ctl -0
   ${C_IG_SCRIPTS}/grib2ctl.pl -verf guess.${PDATE} > guess.ctl
   gribmap -i guess.ctl
else                                    # grib2
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


for type in uv; do

   eval stype=\${${type}_TYPE} 
   eval nreal=\${nreal_${type}} 
#   exec=read_${type}

   ## decoding the dignostic file

   for dtype in ${stype}; do

      mtype=`echo ${dtype} | cut -f1 -d_`
      subtype=`echo ${dtype} | cut -f2 -d_`

      for cycle in ges anl; do

         nt=1

         ### determine what kind data to plotted: 1: all data, 0: assimilated, -1: rejected
         ### or not assimilated

         if [ "$mtype" = 'uv220' ]; then

            ${NCP} ${C_IG_FIX}/uvmandlev.ctl ./${dtype}.ctl
            ${NCP} ${C_IG_GSCRIPTS}/plot_uvallev_horz.gs ./plot_${dtype}.gs

         elif  [ "$mtype" = 'uv223' -o "$mtype" = 'uv224' -o "$mtype" = 'uv228' ]; then

            ${NCP} ${C_IG_FIX}/uvsig.ctl ./${dtype}.ctl
            ${NCP} ${C_IG_GSCRIPTS}/plot_uvallev_horz.gs ./plot_${dtype}.gs

         elif  [ "$mtype" = 'uv221' -o "$mtype" = 'uv230' -o "$mtype" = 'uv231' -o "$mtype" = 'uv232' -o "$mtype" = 'uv233' -o "$mtype" = 'uv234' -o "$mtype" = 'uv235' ]; then

            ${NCP} ${C_IG_FIX}/uvallev.ctl  ./${dtype}.ctl
            ${NCP} ${C_IG_GSCRIPTS}/plot_uvallev_horz.gs ./plot_${dtype}.gs

         elif [ "$mtype" = 'uv242' -o "$mtype" = 'uv243'  -o "$mtype" = 'uv245' -o "$mtype" = 'uv246' -o "$mtype" = 'uv247' -o "$mtype" = 'uv248' -o "$mtype" = 'uv249' -o "$mtype" = 'uv250' -o "$mtype" = 'uv251' -o "$mtype" = 'uv252' -o "$mtype" = 'uv253' -o "$mtype" = 'uv254' -o "$mtype" = 'uv255' -o "$mtype" = 'uv256' -o "$mtype" = 'uv257' -o "$mtype" = 'uv258' ]; then

            ${NCP} ${C_IG_FIX}/uvallev.ctl ./${dtype}.ctl
            ${NCP} ${C_IG_GSCRIPTS}/plot_uvsatwind_horz.gs ./plot_${dtype}.gs

         elif [ "$mtype" = 'uv280' -o "$mtype" = 'uv281' -o "$mtype" = 'uv282' -o "$mtype" = 'uv284'  -o "$mtype" = 'uv287' ]; then

            ${NCP} ${C_IG_FIX}/uvsfc11.ctl ./${dtype}.ctl
            ${NCP} ${C_IG_GSCRIPTS}/plot_uvsfc_horz.gs ./plot_${dtype}.gs
            if [ -s ${hh_tankdir}/${cycle}/nt_${dtype}.${PDATE} ]; then
               echo "LOCATED nt file"
               nt=`cat ${hh_tankdir}/${cycle}/nt_${dtype}.${PDATE}`
               echo "nt set to $nt"
            fi

         elif [ "$mtype" = 'uv229' ]; then

            ${NCP} ${C_IG_FIX}/uvsfc7.ctl ./${dtype}.ctl
            ${NCP} ${C_IG_GSCRIPTS}/plot_uvsfc_horz.gs ./plot_${dtype}.gs
            if [ -s ${hh_tankdir}/${cycle}/nt_${dtype}.${PDATE} ]; then
               echo "LOCATED nt file"
               nt=`cat ${hh_tankdir}/${cycle}/nt_${dtype}.${PDATE}`
               echo "nt set to $nt"
            fi
         fi

         if [  ! -s $savedir/${cycle}/${dtype}_grads.${PDATE} ]; then
            echo "BREAKing for want of ${dtype}_grads.${PDATE}"
            break
         fi 

         sdir=" dset $savedir/${cycle}/${dtype}_grads.${PDATE}"
         title="title  ${dtype}  ${cycle}"
         sed -e "s/^title.*/${title}/" ${dtype}.ctl >tmp.ctl
         echo $sdir >${dtype}_grads_${cycle}.ctl
         cat tmp.ctl >>${dtype}_grads_${cycle}.ctl
         rm -f tmp.ctl
         rm -f ${dtype}.ctl

      done         ## done with cycle

      if [  ! -s $savedir/$cycle/${dtype}_grads.${PDATE} ]; then
         continue
      fi

      stnmap -1 -i ${dtype}_grads_ges.ctl 

      ### set up plot variables

      sed -e "s/XSIZE/$xsize/" \
          -e "s/YSIZE/$ysize/" \
          -e "s/PLOTFILE/$mtype/" \
          -e "s/PLOT2/$dtype/" \
          -e "s/RDATE/$PDATE/" \
          -e "s/HINT/${hint}/" \
          -e "s/NT/$nt/" \
          -e "s/DINDEX/$dindex/" \
         plot_${dtype}.gs >plothorz_${dtype}.gs


      ${GRADS} -blc "run plothorz_${dtype}.gs"


      mkdir -p ${C_IMGNDIR}/pngs/horz/${CYC}
      ${NCP} -f *.png ${C_IMGNDIR}/pngs/horz/${CYC}/.
#      rm *.png

   done      ### dtype loop 
done      ### type loop

#cd $tmpdir_plothorz
#rm -rf $PDATE
#cd ..
#rm -rf $tmpdir_plothorz 

echo "<-- plot_horz_uv.sh"
exit $rc

