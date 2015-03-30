#!/bin/sh
set -ax
date
export list=$listvar

#---------------------------------------------------
#
#  plot_hist.sh
#
#  plot scater images for temperature
#
#---------------------------------------------------


export tmpdir_plothist=${STMP}/plothist/$SUFFIX

export xsize=800	# add standard plot sizes to parms file?
export ysize=600

export CYA=`echo $PDATE|cut -c9-10`

### put into local machine to show the results

rm -rf $tmpdir_plothist
mkdir -p $tmpdir_plothist
cd $tmpdir_plothist
mkdir -p $PDATE
cd $PDATE


for type in ps q t uv; do  
   eval stype=\${${type}_TYPE} 
   eval nreal=\${nreal_${type}} 
   exec=read_${type}.x

   ## decoding the dignostic file

   for dtype in ${stype}; do

      mtype=`echo ${dtype} | cut -f1 -d_`
      subtype=`echo ${dtype} | cut -f2 -d_`
      rm -f fileout

      for cycle in ges anl; do

         ### read scatter data for histgram file

         /bin/sh  $SCRIPTS/read_scatter.sh $SUFFIX $dtype $mtype $subtype $PDATE $FIXDIR $nreal $exec $type $cycle $TANKDIR/horz_hist/$cycle $EXEDIR 

         ####

         ### build the control file for the data

         if [  -s $TANKDIR/horz_hist/$cycle/${dtype}.scater.${PDATE} ];then
            cp $CTLDIR/hist_${type}.ctl ./hist_${dtype}.ctl

            nlev_str=`cat stdout_${dtype}_${cycle}.${PDATE} | grep nlev`
            nlev=`echo $nlev_str | gawk '{print $2}'`
            echo "DEBUG:  nlev = $nlev"

            sdir=" dset ^out_${dtype}_${cycle}.${PDATE}"
            title="title  ${dtype}  ${cycle}"
            xdef="xdef $nlev   linear 1 1 "
            sed -e "s/^title.*/${title}/" hist_${dtype}.ctl >tmp.ctl
            sed -e "s/^xdef.*/${xdef}/" tmp.ctl >tmp1.ctl

            yr=`echo ${PDATE} | cut -c1-4`
            mo=`echo ${PDATE} | cut -c5-6`
            da=`echo ${PDATE} | cut -c7-8`
            hr=`echo ${PDATE} | cut -c9-10`

            case $mo in
               01) month=jan;;
               02) month=feb;;
               03) month=mar;;
               04) month=apr;;
               05) month=may;;
               06) month=jun;;
               07) month=jul;;
               08) month=aug;;
               09) month=sep;;
               10) month=oct;;
               11) month=nov;;
               12) month=dec;;
                *) echo "month error $mo"
                   exit 1;;
            esac

            tdef="tdef 1 linear ${hr}z${da}${month}${yr} 1hr"
            sed -e "s/^tdef.*/${tdef}/" tmp1.ctl >tmp2.ctl
 
            echo $sdir >${cycle}_${dtype}.ctl
            cat tmp2.ctl >>${cycle}_${dtype}.ctl
            rm -f tmp.ctl
            rm -f tmp1.ctl
            rm -f tmp2.ctl

            tail -3 stdout_${dtype}_${cycle}.${PDATE} >>fileout

         fi

      done         ## done with cycle

      ### set up plot variables

      if [ -s $TANKDIR/horz_hist/$cycle/${dtype}.scater.${PDATE} ];then

         cp ${GSCRIPTS}/plot_hist.gs ./plot_hist.gs 
         cp ${GSCRIPTS}/setvpage.gs ./setvpage.gs

         sed -e "s/XSIZE/$xsize/" \
             -e "s/YSIZE/$ysize/" \
             -e "s/PLOTFILE/$dtype/" \
             -e "s/SDATE/$PDATE/" \
            plot_hist.gs >plothist_${dtype}.gs

         echo 'quit' |grads -blc " run plothist_${dtype}.gs" 

         if [ "${type}" = 'uv' ]; then

            for uvtype in u v; do
               rm -f fileout

               for cycle in ges anl ; do

                 nlev_str=`cat stdout_${dtype}_${uvtype}_${cycle}.${PDATE} | grep nlev`
                 if [[ $MY_MACHINE == "ccs" ]]; then
                    nlev=`echo $nlev_str | sed 's/:/ /g' | gawk '{print $1}'`        
                 elif [[ $MY_MACHINE == "wcoss" ]]; then
                    nlev=`echo $nlev_str | gawk '{print $2}'`
                 fi

                  sdir=" dset ^out_${dtype}_${uvtype}_${cycle}.${PDATE}"
                  title="title  ${dtype}_${uvtype}  ${cycle}"
                  xdef="xdef $nlev   linear 1 1 "
                  sed -e "s/^title.*/${title}/" hist_${dtype}.ctl >tmp.ctl
                  sed -e "s/^xdef.*/${xdef}/" tmp.ctl >tmp1.ctl

                  yr=`echo ${PDATE} | cut -c1-4`
                  mo=`echo ${PDATE} | cut -c5-6`
                  da=`echo ${PDATE} | cut -c7-8`
                  hr=`echo ${PDATE} | cut -c9-10`

                  case $mo in
                     01) month=jan;;
                     02) month=feb;;
                     03) month=mar;;
                     04) month=apr;;
                     05) month=may;;
                     06) month=jun;;
                     07) month=jul;;
                     08) month=aug;;
                     09) month=sep;;
                     10) month=oct;;
                     11) month=nov;;
                     12) month=dec;;
                      *) echo "month error $mo"
                         exit 1;;
                  esac

                  tdef="tdef 1 linear ${hr}z${da}${month}${yr} 1hr"
                  sed -e "s/^tdef.*/${tdef}/" tmp1.ctl >tmp2.ctl

                  echo $sdir >${cycle}_${dtype}_${uvtype}.ctl
                  cat tmp2.ctl >>${cycle}_${dtype}_${uvtype}.ctl

                  rm -f tmp.ctl
                  rm -f tmp1.ctl
                  rm -f tmp2.ctl

                  tail -3 stdout_${dtype}_${uvtype}_${cycle}.${PDATE} >>fileout

               done

               ### set up plot variables
               cp $GSCRIPTS/plot_hist.gs ./plot_hist.gs

               sed -e "s/XSIZE/$xsize/" \
                   -e "s/YSIZE/$ysize/" \
                   -e "s/PLOTFILE/${dtype}_${uvtype}/" \
                   -e "s/SDATE/$PDATE/" \
                  plot_hist.gs >plothist_${dtype}_${uvtype}.gs


               echo 'quit' |grads -blc " run plothist_${dtype}_${uvtype}.gs"

            done      ### uvtype loop
         fi
      fi
   done      ### dtype loop 

##scp *hist*.png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/hist/${CYA}/.
##scp *hist*.png wd20xs@lnx42.ncep.noaa.gov:${WEBDIR}/hist/${CYA}/.
##scp *hist*.png $WSUSER@$WS:${WEBDIR}/hist/${CYA}/.

   mkdir -p ${IMGNDIR}/pngs/hist/${CYA}
   cp -f *hist*.png ${IMGNDIR}/pngs/hist/${CYA}/.

   #rm -f *hist*.png

done      ### type loop

##cd $tmpdir_plothist
##rm -rf *
##cd ..
##rm -rf $tmpdir_plothist

exit

