#!/bin/sh

#----------------------------------------------------------
#
#  plot_time.sh
#
#----------------------------------------------------------
set -ax

type=${TYPE}

echo "--> plot_time.sh, type=${type}"

plotdir=${C_PLOT_WORKDIR}/plottime_${type}
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
   #  Link in the data files.
   #    going to need ndays worth here
   #---------------------------------------------------
   cdate=$START_DATE
   edate=$PDATE

   while [[ $cdate -le $edate ]] ; do
      day=`echo $cdate | cut -c1-8 `

      if [[ -d ${C_TANKDIR}/cmon.${day} ]]; then
         for cycle in ges anl; do
            if [[ -s ${C_TANKDIR}/cmon.${day}/time_vert/${cycle}_${type}_stas.${cdate} ]]
            then
               ln -s ${C_TANKDIR}/cmon.${day}/time_vert/${cycle}_${type}_stas.${cdate} .
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

      cp -f ${tv_tankdir}/${cycle}_${type}_stas.ctl      tmp.ctl
      new_dset=" dset ${cycle}_${type}_stas.%y4%m2%d2%h2"

      tdef=`${C_IG_SCRIPTS}/make_tdef.sh ${START_DATE} ${NUM_CYCLES} 06`
      echo "tdef = $tdef"

      sed -e "s/^dset*/${new_dset}/" tmp.ctl >tmp2.ctl
      sed -e "s/^tdef.*/${tdef}/" tmp2.ctl >${cycle}_${type}_stas.ctl
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

  
   #---------------------------------------------------
   #  copy plots scripts locally, modify, and run
   #---------------------------------------------------

   for script in plotstas_time_count.gs plotstas_time_bias.gs plotstas_time_bias2.gs ;do
      plot_script=${C_IG_GSCRIPTS}/${script}

      if [[ -s  ${plot_script} ]]; then
         cp -f ${plot_script} .
      else
         rc=9
         echo "unable to find ${plot_script}, exiting"
         exit ${rc} 
      fi

      #--------------------------------
      #  modify plot script for type
      #--------------------------------
      base_name=`echo "${script}" | awk -F. '{print $1}'`
      local_plot_script=${base_name}_${type}.gs
      sed -e "s/DTYPE/$type/" \
         ${script} > ${local_plot_script}

      #-------------------------
      #  run the plot scripts
      #-------------------------
      grads -bpc "run ./${local_plot_script}"
      cp -f *.png ${outdir}/.
      rm -f ./*.png

   done

   #cd ..
   #rm -rf $plotdir

   echo "<-- plot_time.sh, type=${type}"
exit

