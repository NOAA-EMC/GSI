#!/bin/sh
set -ax

#------------------------------------------------------------
#  Function getField returns the corresponding value for
#  a specified field ($2) in the input ($1) grads_info_file.
#
getField () {
   field='default'
   echo "getField $1 $2"
   file=$1
   field=$2
  
   search_string=`cat ./${file} | grep $field`
   out=`echo $search_string | gawk '{print $3}'`
   return 
}



date
export list=$listvar
echo "list = $list"
#---------------------------------------------------
#
#  plot_hist.sh
#
#  plot scater image histographs
#
#---------------------------------------------------

echo "---> plot_hist.sh"

   export workdir=${C_PLOT_WORKDIR}/plothist


   export xsize=800	# add standard plot sizes to parms file?
   export ysize=600
 
   export PDY=`echo $PDATE|cut -c1-8`
   export CYC=`echo $PDATE|cut -c9-10`


   rm -rf $workdir
   mkdir -p $workdir
   cd $workdir

   echo "C_TANKDIR = $C_TANKDIR"
   hh_tankdir=${C_TANKDIR}/${RUN}.${PDY}/${CYC}/conmon/horz_hist

   $UNCOMPRESS ${hh_tankdir}/anl/*.scater.*${Z}
   $UNCOMPRESS ${hh_tankdir}/ges/*.scater.*${Z}


#for type in ps q t uv; do  
   for type in ps q t; do  
  
      eval stype=\${${type}_TYPE} 
      eval nreal=\${nreal_${type}} 
      exec=conmon_read_${type}_IG.x

      echo "stype, nreal, exec = $stype, $nreal, $exec"


      for dtype in ${stype}; do

         mtype=`echo ${dtype} | cut -f1 -d_`
         subtype=`echo ${dtype} | cut -f2 -d_`
         rm -f ./fileout
 
         tankdir=${C_TANKDIR}/${RUN}.${PDY}/${CYC}/conmon


         #------------------------------------------
         #  The GrADS plot scripts for horiz plots
         #  run in two loops, ges & anl, so the
         #  out and ctl files for each must be set
         #  up before plotting.
         # 
         for cycle in ges anl; do		
 
            scater_file=${hh_tankdir}/$cycle/${dtype}.scater.${cycle}.${PDATE}

            if [[ -s ${scater_file} ]]; then

               #------------------------------------------
               # Read scatter data and create a GrADS 
               # data file for histogram generation. 
               #
               /bin/sh  ${C_IG_SCRIPTS}/read_scatter.sh $CONMON_SUFFIX \
   		  $dtype $mtype $subtype $PDATE ${HOMEgdas_conmon}/fix \
  		  ${nreal} ${exec} ${type} ${cycle} \
  		  ${hh_tankdir}/${cycle} ${C_IG_EXEC} 



               #------------------------------------------
               # build the control file for the data
               #
               cp ${C_IG_FIX}/hist_${type}.ctl ./hist_${dtype}.ctl
  
               nlev_str=`cat grads_info_file_${dtype}_${cycle}.${PDATE} | grep nlev`
               nlev=`echo $nlev_str | gawk '{print $3}'`
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
 
               #--------------------------------------------
               #  Build the fileout.${cycle}.${dtype} file.
               #
               info_file="grads_info_file_${dtype}_${cycle}.${PDATE}"

               out='';  getField ${info_file} 'all_ncount'; all_ncount=${out}
               out='';  getField ${info_file} 'all_rejqc'; all_rejqc=${out}
               out='';  getField ${info_file} 'all_gros'; all_gros=${out}
               out='';  getField ${info_file} 'all_std'; all_std=${out}
               out='';  getField ${info_file} 'all_mean'; all_mean=${out}

               out='';  getField ${info_file} 'ioqc_ncount'; ioqc_ncount=${out}
               out='';  getField ${info_file} 'ioqc_rejqc'; ioqc_rejqc=${out}
               out='';  getField ${info_file} 'ioqc_gros'; ioqc_gros=${out}
               out='';  getField ${info_file} 'ioqc_std'; ioqc_std=${out}
               out='';  getField ${info_file} 'ioqc_mean'; ioqc_mean=${out}

               out='';  getField ${info_file} 'mon_ncount'; mon_ncount=${out}
               out='';  getField ${info_file} 'mon_rejqc'; mon_rejqc=${out}
               out='';  getField ${info_file} 'mon_gros'; mon_gros=${out}
               out='';  getField ${info_file} 'mon_std'; mon_std=${out}
               out='';  getField ${info_file} 'mon_mean'; mon_mean=${out}
 
               outfile=fileout.${cycle}.${dtype} 
               echo ${all_ncount} ${all_rejqc} ${all_gros} ${all_std} ${all_mean}       > ${outfile}
               echo ${ioqc_ncount} ${ioqc_rejqc} ${ioqc_gros} ${ioqc_std} ${ioqc_mean} >> ${outfile}
               echo ${mon_ncount} ${mon_rejqc} ${mon_gros} ${mon_std} ${mon_mean}      >> ${outfile}
  

               #-----------------------------
               # set up plot variables
               #
               if [[ ! -e ./plot_hist.gs ]]; then
                  cp ${C_IG_GSCRIPTS}/plot_hist.gs ./plot_hist.gs 
               fi
               if [[ ! -e ./setvpage.gs ]]; then 
                  cp ${C_IG_GSCRIPTS}/setvpage.gs ./setvpage.gs
               fi

               sed -e "s/XSIZE/$xsize/" \
                   -e "s/YSIZE/$ysize/" \
                   -e "s/PLOTFILE/$dtype/" \
                   -e "s/SDATE/$PDATE/" \
               plot_hist.gs >plothist_${dtype}.gs

               if [[ ! -d ${C_IMGNDIR}/pngs/hist/${CYC} ]]; then
                  mkdir -p ${C_IMGNDIR}/pngs/hist/${CYC}
               fi



   #           if [ "${type}" = 'uv' ]; then
#
#                 for uvtype in u v; do
#                    rm -f fileout
#
#                    for cycle in ges anl ; do
#
#                       nlev_str=`cat stdout_${dtype}_${uvtype}_${cycle}.${PDATE} | grep nlev`
#                       nlev=`echo $nlev_str | gawk '{print $2}'`
# 
#                       sdir=" dset ^out_${dtype}_${uvtype}_${cycle}.${PDATE}"
#                       title="title  ${dtype}_${uvtype}  ${cycle}"
#                       xdef="xdef $nlev   linear 1 1 "
#                       sed -e "s/^title.*/${title}/" hist_${dtype}.ctl >tmp.ctl
#                       sed -e "s/^xdef.*/${xdef}/" tmp.ctl >tmp1.ctl
#
#                       yr=`echo ${PDATE} | cut -c1-4`
#                       mo=`echo ${PDATE} | cut -c5-6`
#                       da=`echo ${PDATE} | cut -c7-8`
#                       hr=`echo ${PDATE} | cut -c9-10`
#
#                       case $mo in
#                          01) month=jan;;
#                          02) month=feb;;
#                          03) month=mar;;
#                          04) month=apr;;
#                          05) month=may;;
#                          06) month=jun;;
#                          07) month=jul;;
#                          08) month=aug;;
#                          09) month=sep;;
#                          10) month=oct;;
#                          11) month=nov;;
#                          12) month=dec;;
#                           *) echo "month error $mo"
#                            exit 1;;
#                       esac
#
#
#                       tdef="tdef 1 linear ${hr}z${da}${month}${yr} 1hr"
#                       sed -e "s/^tdef.*/${tdef}/" tmp1.ctl >tmp2.ctl
#
#                       echo $sdir >${cycle}_${dtype}_${uvtype}.ctl
#                       cat tmp2.ctl >>${cycle}_${dtype}_${uvtype}.ctl
#
#                       rm -f tmp.ctl
#                       rm -f tmp1.ctl
#                       rm -f tmp2.ctl
#
#                       tail -3 stdout_${dtype}_${uvtype}_${cycle}.${PDATE} >>fileout
#
#                    done

                     #--------------------------
                     ##  set up plot variables
     #               cp $C_IM_GSCRIPTS/plot_hist.gs ./plot_hist.gs
#
#                    sed -e "s/XSIZE/$xsize/" \
#                        -e "s/YSIZE/$ysize/" \
#                        -e "s/PLOTFILE/${dtype}_${uvtype}/" \
#                        -e "s/SDATE/$PDATE/" \
#                    plot_hist.gs >plothist_${dtype}_${uvtype}.gs
#
#
#                    echo 'quit' |grads -blc " run plothist_${dtype}_${uvtype}.gs"
#
#
#                 done      ### uvtype loop
#              fi

#           fi

   #        mkdir -p ${C_IMGNDIR}/pngs/hist/${CYC}
#           mv -f *hist*.png ${C_IMGNDIR}/pngs/hist/${CYC}/.
#            cp -f *hist*.png ${C_IMGNDIR}/pngs/hist/${CYC}/.

            fi 		## -s $scater_file
         done         ## done with cycle


         #-------------------------------------
         #  run the GrADS plot script
         #
         cat fileout.ges.${dtype} >  fileout
         cat fileout.anl.${dtype} >> fileout
         cp fileout fileout_all.${dtype}
 
         echo 'quit' | grads -blc " run plothist_${dtype}.gs" 
         rm fileout

#         mv -f *hist*.png ${C_IMGNDIR}/pngs/hist/${CYC}/.
         cp -f *hist*.png ${C_IMGNDIR}/pngs/hist/${CYC}/.

      done      ### dtype loop 


#     #rm -f *hist*.png

   done      ### type loop


   $COMPRESS ${hh_tankdir}/anl/*.scater.*
   $COMPRESS ${hh_tankdir}/ges/*.scater.*


   ##cd $workdir
   ##rm -rf *
   ##cd ..
   ##rm -rf $workdir

echo "<--- plot_hist.sh"
exit

