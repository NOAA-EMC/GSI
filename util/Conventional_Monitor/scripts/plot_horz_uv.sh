#!/bin/sh
set -ax
date
export list=$listvar

#---------------------------------------------------------------------
#
#  plot_horz_uv.sh
#
#  plot scater image for temperature
#---------------------------------------------------------------------

export tmpdir_plothorz=${workdir}/plothorz_uv/$SUFFIX
export savedir=${TANKDIR}/horz_hist
CYA=`echo $PDATE|cut -c9-10`

rm -rf $tmpdir_plothorz
mkdir -p $tmpdir_plothorz
cd $tmpdir_plothorz
mkdir -p $PDATE
cd $PDATE

export GDATE=$(/nwprod/util/exec/ndate -06 $PDATE)

cp $DATDIR/pgbanl.$PDATE pgbanl.$PDATE
cp $DATDIR/pgbf06.$GDATE pgbf06.$GDATE

if [[ ${MY_MACHINE} = "wcoss" ]]; then

   if [[ $SUFFIX = "copr" ]]; then
      ${SCRIPTS}/grib2ctl.pl pgbanl.${PDATE} > anal.ctl
      gribmap -i anal.ctl -0
      ${SCRIPTS}/grib2ctl.pl -verf pgbf06.${GDATE} > guess.ctl
      gribmap -i guess.ctl
   else
      ${SCRIPTS}/g2ctl.pl -0 pgbanl.$PDATE > anal.ctl
      gribmap -0 -i anal.ctl
      ${SCRIPTS}/g2ctl.pl pgbf06.$GDATE > guess.ctl
      gribmap -i guess.ctl
   fi

fi

#cp $DATDIR/pgbanl.${PDATE}.idx pgbanl.${PDATE}.idx
#cp $DATDIR/pgbf06.${GDATE}.idx pgbf06.${GDATE}.idx
#cp $DATDIR/anal.ctl anal.ctl
#cp $DATDIR/guess.ctl guess.ctl

### some grads tools
cp ${GSCRIPTS}/rgbset2.gs ./rgbset2.gs
cp ${GSCRIPTS}/page.gs ./page.gs
cp ${GSCRIPTS}/defint.gs ./defint.gs
cp ${GSCRIPTS}/setvpage.gs ./setvpage.gs


for type in uv; do

   eval stype=\${${type}_TYPE} 
   eval nreal=\${nreal_${type}} 
   exec=read_${type}

   ## decoding the dignostic file

   for dtype in ${stype}; do

      mtype=`echo ${dtype} | cut -f1 -d_`
      subtype=`echo ${dtype} | cut -f2 -d_`

      for cycle in ges anl; do

         ### determine what kind data to plotted: 1: all data, 0: assimilated, -1: rejected
         ### or not assimilated

         if [ "$mtype" = 'uv220' ]; then

            cp $CTLDIR/uvmandlev.ctl ./${dtype}.ctl
            cp $GSCRIPTS/plot_uvallev_horz.gs ./plot_${dtype}.gs
            nt=1

         elif  [ "$mtype" = 'uv223' -o "$mtype" = 'uv224' -o "$mtype" = 'uv228' ]; then

            cp $CTLDIR/uvsig.ctl ./${dtype}.ctl
            cp $GSCRIPTS/plot_uvallev_horz.gs ./plot_${dtype}.gs
            nt=1

         elif  [ "$mtype" = 'uv221' -o "$mtype" = 'uv230' -o "$mtype" = 'uv231' -o "$mtype" = 'uv232' -o "$mtype" = 'uv233' -o "$mtype" = 'uv234' -o "$mtype" = 'uv235' ]; then

            cp $CTLDIR/uvallev.ctl  ./${dtype}.ctl
            cp $GSCRIPTS/plot_uvallev_horz.gs ./plot_${dtype}.gs
            nt=1

         elif [ "$mtype" = 'uv242' -o "$mtype" = 'uv243'  -o "$mtype" = 'uv245' -o "$mtype" = 'uv246' -o "$mtype" = 'uv247' -o "$mtype" = 'uv248' -o "$mtype" = 'uv249' -o "$mtype" = 'uv250' -o "$mtype" = 'uv251' -o "$mtype" = 'uv252' -o "$mtype" = 'uv253' -o "$mtype" = 'uv254' -o "$mtype" = 'uv255' -o "$mtype" = 'uv256' -o "$mtype" = 'uv257' -o "$mtype" = 'uv258' ]; then

            cp $CTLDIR/uvallev.ctl ./${dtype}.ctl
            cp $GSCRIPTS/plot_uvsatwind_horz.gs ./plot_${dtype}.gs
            nt=1

         elif [ "$mtype" = 'uv280' -o "$mtype" = 'uv281' -o "$mtype" = 'uv282' -o "$mtype" = 'uv284'  -o "$mtype" = 'uv287' ]; then

            cp $CTLDIR/uvsfc11.ctl ./${dtype}.ctl
            cp $GSCRIPTS/plot_uvsfc_horz.gs ./plot_${dtype}.gs
            if [ $cycle = ges ]; then
               nt=`tail -1 $workdir/$PDATE/stdout_diag2grads_${dtype}_ges`
            fi

         elif [ "$mtype" = 'uv229' ]; then

            cp $CTLDIR/uvsfc7.ctl ./${dtype}.ctl
            cp $GSCRIPTS/plot_uvsfc_horz.gs ./plot_${dtype}.gs
            if [ $cycle = ges ]; then
               nt=`tail -1 $workdir/$PDATE/stdout_diag2grads_${dtype}_ges`
            fi
         fi

         if [  ! -s $savedir/$cycle/${dtype}_grads.${PDATE} ]; then
            break
         fi 

         sdir=" dset $savedir/$cycle/${dtype}_grads.${PDATE}"
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

      stnmap -i ${dtype}_grads_ges.ctl 

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


      echo 'quit' |grads -blc " run plothorz_${dtype}.gs" 


##scp *png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/horz/${CYA}/.
##scp *png ${WSUSER}@${WS}:${WEBDIR}/horz/${CYA}/.

      mkdir -p ${IMGNDIR}/pngs/horz/${CYA}
      cp -f *.png ${IMGNDIR}/pngs/horz/${CYA}/.
      rm *.png

   done      ### dtype loop 
done      ### type loop

#cd $tmpdir_plothorz
#rm -rf $PDATE
#cd ..
#rm -rf $tmpdir_plothorz 

exit

