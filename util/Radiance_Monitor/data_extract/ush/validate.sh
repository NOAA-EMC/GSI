#! /bin/bash

   function usage {
     echo "Usage:  validate.sh pdate"
     echo "            pdate is the cycle to be processed in YYYYMMDDHH format"
   }


echo "--> start validate.sh"
echo "       TEST:  COMPRESS = $COMPRESS"

   nargs=$#
   if [[ $nargs -ne 1 ]]; then
      usage
      exit 1
   fi

   PDATE=$1


   sat="sndrd4_g15"
   iyy=`echo $PDATE | cut -c1-4`
   imm=`echo $PDATE | cut -c5-6`
   idd=`echo $PDATE | cut -c7-8`
   ihh=`echo $PDATE | cut -c9-10`

#
#  Get the gdas_radmon_base.tar file and open it
#
   cp ~/nbns/stats/wopr/info/gdas_radmon_base.tar* .
   if [[ -s gdas_radmon_base.tar.gz ]]; then
      gunzip gdas_radmon_base.tar.gz
   fi
   tar -xvf gdas_radmon_base.tar
   rm -f gdas_radmon_base.tar

#
#  Get satype list, loop over satype
#
   PDATE=${iyy}${imm}${idd}${ihh}
   echo PDATE = $PDATE

   test_list=`ls time.*.${PDATE}.ieee_d*`
   for test in ${test_list}; do
      this_file=`basename $test`
      tmp=`echo "$this_file" | cut -d. -f2`
      SATYPE_LIST="$SATYPE_LIST $tmp"
   done
   echo $SATYPE_LIST

# testing
#  SATYPE_LIST="sndrd4_g15"



#
# loop over SATYPE_LIST  
#
   for sat in ${SATYPE_LIST}; do
      echo sat = $sat

      gunzip time.${sat}.${PDATE}.ieee_d.gz
      gunzip time.${sat}.ctl.gz


      nchan=`cat time.${sat}.ctl | gawk '/title/{print $NF}'`
      chan_list=`cat time.${sat}.ctl | gawk '/ x=/{print $5}'`

      chanl=`echo $chan_list | sed 's/ /,/g'`
      echo chanl = ${chanl}

      iuse=`cat time.${sat}.ctl | gawk '/ x=/{print $8}'`
      iuse=`echo $iuse | sed 's/ /,/g'`
      echo iuse = $iuse

      rm ./input

#
# contstruct namelists
#
cat << EOF > input
 &INPUT
  satname='${sat}',
  iyy=${iyy},
  imm=${imm},
  idd=${idd},
  ihh=${ihh},
  nchanl=${nchan},
 /
  &IUSEFLG
  test_iuse=${iuse},
 /
  &ICHANNUM
  test_chan=${chanl}
 /
EOF

      ./validate_time.x < input >   stdout.validate.$sat.$ihh


      gzip time.${sat}.${PDATE}.ieee_d

   done              #  end loop over SATYPE_LIST

   rm -f *.base

echo "<-- end validate.sh"
