#!/bin/sh
set -xa

#----------------------------------------------------
#  This script performs the data extraction for the     
#  the time and vertical profile plots.
#
#  time_vert.sh
#
#----------------------------------------------------

echo "--> time_vert.sh"

   echo " $CWD"

   rc=0
   export nregion=10

   echo "CONMON_NETCDF = ${CONMON_NETCDF}"
   netcdf=".false."

   if [ $CONMON_NETCDF -eq 1 ]; then
      netcdf=".true."
   fi
   echo "netcdf = $netcdf"


   #------------------------------------------
   # set up TANKDIR (output) sub-directories
   #
   echo TANKDIR_conmon = $TANKDIR_conmon
   export savedir=$TANKDIR_conmon/time_vert
   mkdir -p ${savedir}


   echo "convinfo = $convinfo"			# defined in calling script
   cp ${convinfo} ./convinfo

   export execfile=${EXECconmon}/conmon_time.x
   cp ${execfile} ./execfile

   export CYA=`echo $PDATE | cut -c9-10`


   hour=`echo $PDATE | cut -c9-10`
   dday=`echo $PDATE | cut -c7-8`


   if [ $CONMON_NETCDF -eq 0 ]; then

      for run in ges anl; do

         echo " cycle = $cycle "
   
         ${UNCOMPRESS} ./diag_conv_${run}.${PDATE}.${Z}
 
         cat << EOF > input
&input
         input_file=diag_conv_${run}.${PDATE},
         nregion=${nregion},
         netcdf=${netcdf},
         run=${run},
         region(1)='GL',rlonmin(1)=-180.0,rlonmax(1)=180.0,rlatmin(1)=-90.0,rlatmax(1)= 90.0,
         region(2)='NH',rlonmin(2)=-180.0,rlonmax(2)=180.0,rlatmin(2)= 20.0,rlatmax(2)= 90.0,
         region(3)='SH',rlonmin(3)=-180.0,rlonmax(3)=180.0,rlatmin(3)=-90.0,rlatmax(3)=-20.0,
         region(4)='TR',rlonmin(4)=-180.0,rlonmax(4)=180.0,rlatmin(4)=-20.0,rlatmax(4)= 20.0,
         region(5)='USA',rlonmin(5)=-125.0,rlonmax(5)=-65.0,rlatmin(5)=25.0,rlatmax(5)=50.0,
         region(6)='CAN',rlonmin(6)=-125.0,rlonmax(6)=-65.0,rlatmin(6)= 50.0,rlatmax(6)= 90.0,
         region(7)='N&CA',rlonmin(7)=-165.0,rlonmax(7)=-60.0, rlatmin(7)= 0.0,rlatmax(7)=90.0,
         region(8)='S&CA',rlonmin(8)=-165.0,rlonmax(8)=-30.0,rlatmin(8)=-90.0,rlatmax(8)=0.0,
         region(9)='EU',rlonmin(9)=-10.0,rlonmax(9)=25.0,rlatmin(9)=35.0,rlatmax(9)=70.0,
         region(10)='AS',rlonmin(10)=65.0,rlonmax(10)=145.0,rlatmin(10)=5.0,rlatmax(10)=45.0,
/
EOF

         stdout=stdout_${run}.${PDATE}
         ./execfile <input  >${stdout}  2>&1
       
         echo " execfile completed "


         cp uv_stas.ctl u_stas.ctl
         cp uv_stas.ctl v_stas.ctl

         for type in ps t q uv u v; do

            for file in ${type}*stas; do
               ${COMPRESS} ${file}
               mv -f ${file}.${Z} ${savedir}/${run}_${file}.${PDATE}.${Z}
            done

            for file in ${type}*stas.ctl; do
               ${COMPRESS} ${file}
               mv -f ${file}.${Z} ${savedir}/${run}_${file}.${Z}
            done


         done

         ${COMPRESS} ${stdout}
         cp ${stdout}.${Z} ${savedir}/. 
      done

   else
      echo "Handle NetCDF files here"

      for run in ges anl; do

         for type in gps ps t q uv; do

         
            cat << EOF > input
&input
            input_file=diag_conv_${type}_${run}.${PDATE},
            ctype=${type},
            nregion=${nregion},
            netcdf=${netcdf},
            run=${run},
            region(1)='GL',rlonmin(1)=-180.0,rlonmax(1)=180.0,rlatmin(1)=-90.0,rlatmax(1)= 90.0,
            region(2)='NH',rlonmin(2)=-180.0,rlonmax(2)=180.0,rlatmin(2)= 20.0,rlatmax(2)= 90.0,
            region(3)='SH',rlonmin(3)=-180.0,rlonmax(3)=180.0,rlatmin(3)=-90.0,rlatmax(3)=-20.0,
            region(4)='TR',rlonmin(4)=-180.0,rlonmax(4)=180.0,rlatmin(4)=-20.0,rlatmax(4)= 20.0,
            region(5)='USA',rlonmin(5)=-125.0,rlonmax(5)=-65.0,rlatmin(5)=25.0,rlatmax(5)=50.0,
            region(6)='CAN',rlonmin(6)=-125.0,rlonmax(6)=-65.0,rlatmin(6)= 50.0,rlatmax(6)= 90.0,
            region(7)='N&CA',rlonmin(7)=-165.0,rlonmax(7)=-60.0, rlatmin(7)= 0.0,rlatmax(7)=90.0,
            region(8)='S&CA',rlonmin(8)=-165.0,rlonmax(8)=-30.0,rlatmin(8)=-90.0,rlatmax(8)=0.0,
            region(9)='EU',rlonmin(9)=-10.0,rlonmax(9)=25.0,rlatmin(9)=35.0,rlatmax(9)=70.0,
            region(10)='AS',rlonmin(10)=65.0,rlonmax(10)=145.0,rlatmin(10)=5.0,rlatmax(10)=45.0,
/
EOF
            stdout=stdout_${run}_${type}.${PDATE}
            ./execfile <input  >${stdout}  2>&1
       
            echo " execfile completed "

            for file in ${type}*stas; do
               ${COMPRESS} ${file}
               mv -f ${file}.${Z} ${savedir}/${run}_${file}.${PDATE}.${Z}
            done


            if [ $type == 'uv' ]; then
               file=u_stas
               ${COMPRESS} ${file}
               mv -f ${file}.${Z} ${savedir}/${run}_${file}.${PDATE}.${Z}
               cp uv_stas.ctl u_stas.ctl

               file=v_stas
               ${COMPRESS} ${file}
               mv -f ${file}.${Z} ${savedir}/${run}_${file}.${PDATE}.${Z}
               cp uv_stas.ctl v_stas.ctl
            fi

            for file in *stas.ctl; do
               ${COMPRESS} ${file}
               mv -f ${file}.${Z} ${savedir}/${run}_${file}.${Z}
            done

         done

      done

      ${COMPRESS} stdout*
      tar -cf stdout.tar stdout*
      ${COMPRESS} stdout.tar
      mv -f stdout.tar.${Z} ${savedir}/.

   fi         



   echo "<-- time_vert.sh"

exit ${rc}
