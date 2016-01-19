#!/bin/sh
set -xa

#--------------------------------------------------
#
#  time_vert.sh
#
#--------------------------------------------------

echo "--> time_vert.sh"

`pwd`

rc=0
export nregion=10

# set up TANKDIR directories
echo TANKDIR_cmon = $TANKDIR_cmon
export savedir=$TANKDIR_cmon/time_vert
mkdir -p ${savedir}

echo "convinfo = $convinfo"			# defined in calling script

export execfile=${EXECcmon}/conv_time.x
export CYA=`echo $PDATE | cut -c9-10`


#gdate=`$NDATE -720 $PDATE`
hour=`echo $PDATE | cut -c9-10`
dday=`echo $PDATE | cut -c7-8`


for cycle in ges anl;do

   rm -f ./conv_diag
   ln -s ./diag_conv_${cycle}.${PDATE} conv_diag
#   cp $DATDIR/diag_conv_${cycle}.${PDATE} conv_diag

   cat << EOF > input
&input
   filein='conv_diag', nregion=${nregion},
   region(1)='GL', rlonmin(1)=-180.0,rlonmax(1)=180.0,rlatmin(1)=-90.0,rlatmax(1)= 90.0,
   region(2)='NH',rlonmin(2)=-180.0,rlonmax(2)=180.0,rlatmin(2)= 20.0,rlatmax(2)= 90.0,
   region(3)='SH',rlonmin(3)=-180.0,rlonmax(3)=180.0,rlatmin(3)=-90.0,rlatmax(3)=-20.0,
   region(4)='TR' rlonmin(4)=-180.0,rlonmax(4)=180.0,rlatmin(4)=-20.0,rlatmax(4)= 20.0,
   region(5)='USA' , rlonmin(5)=-125.0,rlonmax(5)=-65.0,rlatmin(5)=25.0,rlatmax(5)=50.0,
   region(6)='CAN', rlonmin(6)=-125.0,rlonmax(6)=-65.0,rlatmin(6)= 50.0,rlatmax(6)= 90.0,
   region(7)='N&CA',rlonmin(7)=-165.0,rlonmax(7)=-60.0,rlatmin(7)= 0.0,rlatmax(7)=90.0,
   region(8)='S&CA',rlonmin(8)=-165.0,rlonmax(8)=-30.0,rlatmin(8)=-90.0, rlatmax(8)=0.0,
   region(9)='EU',  rlonmin(9)=-10.0, rlonmax(9)=25.0, rlatmin(9)=35.0,rlatmax(9)=70.0,
   region(10)='AS',  rlonmin(10)=65.0,  rlonmax(10)=145.0,rlatmin(10)=5.0,rlatmax(10)=45.0,
/
EOF

   cp ${execfile} ./execfile
   cp ${convinfo} ./convinfo
   ./execfile <input  > stdout  2>&1

   mv stdout ${cycle}_stdout

#   for type in u v; do
#     cp ${cycle}_uv_stas.ctl ${cycle}_${type}_stas.ctl
#   done

   cp uv_stas.ctl u_stas.ctl
   cp uv_stas.ctl v_stas.ctl

   for type in ps t q uv u v; do

      for file2 in ${type}*stas; do
         mv -f $file2 ${savedir}/${cycle}_${file2}.${PDATE}
      done

      for file3 in ${type}*stas.ctl; do
         mv -f ${file3} ${savedir}/${cycle}_${file3}
      done

#      /bin/sh  make_timesers_ctl.sh ${gdate} ${PDATE} $savedir $cycle $type

   done

done



echo "<-- time_vert.sh"

exit ${rc}
