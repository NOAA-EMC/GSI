#!/bin/sh
#-------------------------------------------------------
#
#  plot_vert.sh
#
#-------------------------------------------------------
set -ax

echo "--> plot_vert.sh "

   type=${TYPE}

   plotdir=${C_PLOT_WORKDIR}/plotvert_${type}
   rm -rf $plotdir
   mkdir -p $plotdir
   cd $plotdir

   rc=0
   pdy=`echo ${PDATE}|cut -c1-8`
   dday=`echo $PDATE|cut -c7-8`
   cyc=`echo ${PDATE}|cut -c9-10`

   tv_tankdir=${C_TANKDIR}/cmon.${pdy}/time_vert


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
      num_cycles=`expr ${NUM_CYCLES} + 1`

      tdef=`${C_IG_SCRIPTS}/make_tdef.sh ${START_DATE} ${num_cycles} 06`
      echo "tdef = $tdef"

      sed -e "s/^dset*/${new_dset}/" tmp.ctl >tmp2.ctl
      sed -e "s/^tdef.*/${tdef}/" tmp2.ctl >${cycle}_${type}_stas.ctl
      rm -f tmp.ctl
      rm -f tmp2.ctl
   done

   #------------------------------------------
   #  ensure the imgn destination dir exists
   #------------------------------------------
   outdir=${C_IMGNDIR}/pngs/vert/${cyc}
   if [[ ! -d ${outdir} ]]; then
      mkdir -p ${outdir}
   fi

   #---------------------------------------------------
   #  copy plots scripts locally, modify, and run
   #---------------------------------------------------

   for script in page.gs rgbset2.gs setvpage.gs ;do
      plot_script=${C_IG_GSCRIPTS}/${script}

      if [[ -s  ${plot_script} ]]; then
         cp -f ${plot_script} .
      else
         rc=10
         echo "unable to find ${plot_script}, exiting"
         exit ${rc}
      fi
   done

   for script in plotstas_vert_count.gs plotstas_vert_bias.gs plotstas_vert_bias2.gs ;do
      plot_script=${C_IG_GSCRIPTS}/${script}

      if [[ -s  ${plot_script} ]]; then
         cp -f ${plot_script} .
      else
         rc=11
         echo "unable to find ${plot_script}, exiting"
         exit ${rc}
      fi

      #--------------------------------
      #  modify plot script for type
      #--------------------------------
      base_name=`echo "${script}" | awk -F. '{print $1}'`
      local_plot_script=${base_name}_${type}.gs
      sed -e "s/DTYPE/${type}/" \
          -e "s/HOUR/${cyc}/" \
          -e "s/DDAY/${dday}/" \
         ${script} > ${local_plot_script}

      grads -blc "run ./${local_plot_script}"

   done

   cp -f *.png ${outdir}/.
   rm -f *.png


echo "<-- plot_vert.sh "
exit

