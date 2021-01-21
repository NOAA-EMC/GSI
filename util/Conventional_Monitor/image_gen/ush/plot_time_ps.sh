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

   workdir=${C_PLOT_WORKDIR}/plottime_ps
   rm -rf $workdir
   mkdir -p $workdir
   cd $workdir

   rc=0
   pdy=`echo $PDATE|cut -c1-8`
   cyc=`echo $PDATE|cut -c9-10`
   tv_tankdir=${C_TANKDIR}/${RUN}.${pdy}/${cyc}/conmon/time_vert

   export xsize=x800
   export ysize=y600

   #---------------------------------------------------
   #  plot surface pressure time series counts
   #---------------------------------------------------

   cp -f ${C_IG_GSCRIPTS}/plotstas_time_count_ps.gs . 
   cp -f ${C_IG_GSCRIPTS}/plotstas_time_bias_ps.gs  . 
#   cp -f ${C_IG_GSCRIPTS}/plotstas_time_bias2_ps.gs . 

   #---------------------------------------------------
   #  Link in the data files.
   #    going to need ndays worth here
   #---------------------------------------------------
   cdate=$START_DATE
   edate=$PDATE

   while [[ $cdate -le $edate ]] ; do
      day=`echo $cdate | cut -c1-8 `
      dcyc=`echo $cdate | cut -c9-10 `
      
      if [[ -d ${C_TANKDIR}/${RUN}.${day}/${dcyc}/conmon ]]; then

         for cycle in ges anl; do
            stas_file=${C_TANKDIR}/${RUN}.${day}/${dcyc}/conmon/time_vert/${cycle}_ps_stas.${cdate}
            if [[ -e ${stas_file}.${Z} ]]; then
               ${UNCOMPRESS} ${stas_file}.${Z}
            fi
            if [[ -s ${stas_file} ]]; then
               ln -s ${stas_file} .
            fi
         done

      fi

      adate=`${NDATE} +6 ${cdate}`
      cdate=${adate}
   done

   #---------------------------------------------------
   #  Copy over the ctl files, modify dset and tset
   #---------------------------------------------------
   for cycle in ges anl; do

      ctl_file=${tv_tankdir}/${cycle}_ps_stas.ctl
      if [[ -e ${ctl_file}.${Z} ]]; then
         cp -f ${ctl_file}.${Z} tmp.ctl.${Z}
         ${UNCOMPRESS} tmp.ctl.${Z}
      else
         cp -f ${ctl_file} tmp.ctl 
      fi

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
#   mv -f *.png ${outdir}/.
 
   grads -bpc "run ./plotstas_time_bias_ps.gs"
#   mv -f *.png ${outdir}/.

#   grads -bpc "run ./plotstas_time_bias2_ps.gs"

   img_files=`ls *.png`
   for imgf in $img_files; do
      newf=`echo $imgf | sed -e "s/\./.${PDATE}./g"`
      cp $imgf $newf
      mv $newf ${C_IMGNDIR}/pngs/time/.
   done

   if [[ $CONMON_SUFFIX != "v16rt2" ]]; then
      mv -f *.png ${outdir}/.
   fi




   if [[ ${C_IG_SAVE_WORK} -eq 0 ]]; then
      cd $workdir
      cd ..
      rm -rf $workdir
   fi

   echo "<--- plot_time_ps.sh"
exit

