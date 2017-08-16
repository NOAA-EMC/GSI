#!/bin/sh
set -ax

#----------------------------------------------------------------------
#  plot_time_ps.sh
#----------------------------------------------------------------------

echo "---> plot_time_ps.sh"

echo "START_DATE  = $START_DATE"
echo "NUM_CYCLES  = $NUM_CYCLES"
echo "PDATE       = $PDATE"
echo "NDATE       = $NDATE"

plotdir=${C_PLOT_WORKDIR}/plottime_ps
rm -rf $plotdir
mkdir -p $plotdir
cd $plotdir

rc=0
pdy=`echo $PDATE|cut -c1-8`
cyc=`echo $PDATE|cut -c9-10`
tv_tankdir=${C_TANKDIR}/cmon.${pdy}/time_vert

export xsize=x800
export ysize=y600

#---------------------------------------------------
#  plot surface pressure time series counts
#---------------------------------------------------

   cp -f ${C_IG_GSCRIPTS}/plotstas_time_count_ps.gs . 
   cp -f ${C_IG_GSCRIPTS}/plotstas_time_bias_ps.gs  . 
   cp -f ${C_IG_GSCRIPTS}/plotstas_time_bias2_ps.gs . 

   #---------------------------------------------------
   #  Link in the data files.
   #    going to need ndays worth here
   #---------------------------------------------------
   cdate=$START_DATE
   edate=$PDATE

   while [[ $cdate -le $edate ]] ; do
      day=`echo $cdate | cut -c1-8 `

      if [[ -d ${C_TANKDIR}/cmon.${day} ]]; then
         for cycle in ges anl; do
            if [[ -s ${C_TANKDIR}/cmon.${day}/time_vert/${cycle}_ps_stas.${cdate} ]]; then 
               ln -s ${C_TANKDIR}/cmon.${day}/time_vert/${cycle}_ps_stas.${cdate} .
            fi
         done
         echo " ${C_TANKDIR}/cmon.${day} exists"
      fi

      adate=`${NDATE} +6 ${cdate}`
      cdate=${adate}
   done

   #---------------------------------------------------
   #  Copy over the ctl files, modify dset and tset
   #---------------------------------------------------
   for cycle in ges anl; do

      cp -f ${tv_tankdir}/${cycle}_ps_stas.ctl      tmp.ctl 
      new_dset=" dset ${cycle}_ps_stas.%y4%m2%d2%h2"

      tdef=`${C_IG_SCRIPTS}/make_tdef.sh ${START_DATE} ${NUM_CYCLES} 06`
      echo "tdef = $tdef"

      sed -e "s/^dset*/${new_dset}/" tmp.ctl >tmp2.ctl
      sed -e "s/^tdef.*/${tdef}/" tmp2.ctl >${cycle}_ps_stas.ctl
      rm -f tmp.ctl 
      rm -f tmp2.ctl
   done

   #------------------------------------------
   #  ensure the imgn destination dir exists
   #------------------------------------------
   outdir=${C_IMGNDIR}/pngs/time/${cyc}
   if [[ ! -d ${outdir} ]]; then
      mkdir -p ${outdir}
   fi

   #-------------------------
   #  run the plot scripts
   #-------------------------
   grads -bpc "run ./plotstas_time_count_ps.gs"
   cp -f *.png ${outdir}/.
   rm -f ./*.png
 
   grads -bpc "run ./plotstas_time_bias_ps.gs"
   cp -f *.png ${outdir}/.
   rm -f ./*.png

   grads -bpc "run ./plotstas_time_bias2_ps.gs"
   cp -f *.png ${outdir}/.
   rm -f ./*.png

   echo "<--- plot_time_ps.sh"
exit

