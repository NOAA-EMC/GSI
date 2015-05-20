#!/bin/sh
set -ax
date
export list=$listvar


##  the script to plot scater image for temperature

export tmpdir_plothorz=${workdir}/plothorz/$SUFFIX
CYA=`echo $PDATE|cut -c9-10`
export savedir=${TANKDIR}/horz_hist


### makdirectory web site machine to show the results
##ssh -l wd20xs emcrzdm.ncep.noaa.gov "mkdir -p ${WEBDIR}/horz/${CYA}"
##ssh -l wd20xs lnx42.ncep.noaa.gov "mkdir -p ${WEBDIR}/horz/${CYA}"
#ssh -l $WSUSER $WS "mkdir -p ${WEBDIR}/horz/${CYA}"
export xsize=x800
export ysize=y600

rm -rf $tmpdir_plothorz
mkdir -p $tmpdir_plothorz
cd $tmpdir_plothorz
rm -rf *
mkdir -p $PDATE
cd $PDATE

export GDATE=$(/nwprod/util/exec/ndate -06 $PDATE)
cp $DATDIR/pgbanl.$PDATE pgbanl.$PDATE
cp $DATDIR/pgbf06.$GDATE pgbf06.$GDATE
#cp $DATDIR/pgbanl.${PDATE}.idx pgbanl.${PDATE}.idx
#cp $DATDIR/pgbf06.${GDATE}.idx pgbf06.${GDATE}.idx
#cp $DATDIR/anal.ctl anal.ctl
#cp $DATDIR/guess.ctl guess.ctl

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


### some grads tools
cp ${GSCRIPTS}/rgbset2.gs ./rgbset2.gs 
cp ${GSCRIPTS}/page.gs ./page.gs 
cp ${GSCRIPTS}/defint.gs ./defint.gs 
cp ${GSCRIPTS}/setvpage.gs ./setvpage.gs


for type in ps q t; do

   eval stype=\${${type}_TYPE} 
   eval nreal=\${nreal_${type}} 

   ## decoding the dignostic file

   for dtype in ${stype}; do
      mtype=`echo ${dtype} | cut -f1 -d_`
      subtype=`echo ${dtype} | cut -f2 -d_`

      for cycle in ges anl; do
         ### build the control file for the data

         if [ "$mtype" = 'ps180' -o "$mtype" = 'ps181' -o  "$mtype" = 'ps183' -o "$mtype" = 'ps187'  ]; then

            cp $CTLDIR/pstime.ctl ./${dtype}.ctl
            cp $GSCRIPTS/plot_ps_horz.gs ./plot_${dtype}.gs

            if [ $cycle = ges ]; then
               nt=`tail -1 $workdir/$PDATE/stdout_diag2grads_${dtype}.ges`
            fi

         elif [ "$mtype" = 'ps120' ]; then

            cp $CTLDIR/pssfc.ctl ./${dtype}.ctl
            cp $GSCRIPTS/plot_ps_horz.gs ./plot_${dtype}.gs
            nt=1

         elif [ "$mtype" = 't120' ]; then

            cp $CTLDIR/tmandlev.ctl ./${dtype}.ctl
            cp $GSCRIPTS/plot_tallev_horz.gs ./plot_${dtype}.gs
            nt=1

         elif [ "$mtype" = 't180' -o "$mtype" = 't181' -o "$mtype" = 't182' -o "$mtype" = 't183' -o "$mtype" = 't187'  ]; then

            cp $CTLDIR/tsfc.ctl ./${dtype}.ctl
            cp $GSCRIPTS/plot_tsfc_horz.gs ./plot_${dtype}.gs
            nt=1

         elif [ "$mtype" = 't130' -o "$mtype" = 't131' -o "$mtype" = 't132' -o "$mtype" = 't133' -o "$mtype" = 't134' -o "$mtype" = 't135' ]; then

            cp $CTLDIR/tallev.ctl ./${dtype}.ctl
            cp $GSCRIPTS/plot_tallev_horz.gs ./plot_${dtype}.gs
            nt=1

         elif [ "$mtype" = 'q120' ]; then

            cp $CTLDIR/qmandlev.ctl ./${dtype}.ctl
            cp $GSCRIPTS/plot_qallev_horz.gs ./plot_${dtype}.gs
            nt=1
         elif [ "$mtype" = 'q180' -o "$mtype" = 'q181' -o  "$mtype" = 'q183' -o "$mtype" = 'q187'  ];then
            cp $CTLDIR/qsfc.ctl ./${dtype}.ctl
            cp $GSCRIPTS/plot_qsfc_horz.gs ./plot_${dtype}.gs
            if [ $cycle = ges ]; then
               nt=`tail -1 $workdir/$PDATE/stdout_diag2grads_${dtype}.ges`
            fi

         elif [ "$mtype" = 'q130' -o "$mtype" = 'q131' -o "$mtype" = 'q132' -o "$mtype" = 'q133' -o "$mtype" = 't134' ]; then
            cp $CTLDIR/qallev.ctl ./${dtype}.ctl
            cp $GSCRIPTS/plot_qallev_horz.gs ./plot_${dtype}.gs
            nt=1

         elif [ "$mtype" = 'uv220' ]; then

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
         plot_${dtype}.gs >plothorz_${dtype}.gs



      echo 'quit' |grads -blc " run plothorz_${dtype}.gs" 

##scp *png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/horz/${CYA}/.
##scp *png ${WSUSER}@${WS}:${WEBDIR}/horz/${CYA}/.

      mkdir -p ${IMGNDIR}/pngs/horz/${CYA}
      cp -f *.png ${IMGNDIR}/pngs/horz/${CYA}/.

      rm -f *png

   done      ### dtype loop 

done      ### type loop

#cd $tmpdir_plothorz
 
#rm -rf  $PDATE

#cd ..
#rm -rf $tmpdir_plothorz


exit

