#! /bin/bash

   function usage {
     echo "Usage:  validate.sh pdate"
     echo "            pdate is the cycle to be processed in YYYYMMDDHH format"
   }

##############################################################
#
#  validate.sh 
#
#     Purpose:  re-run validation from copied data files using
#     different (usually updated) base files than the ones
#     in oper (which can't be updated as easily/quickly)
#
#     This is only needed with data that has been copied 
#     (using Copy_glb.sh). 
##############################################################

echo "--> start validate.sh"

set -ax

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

#---------------------------------------------------
#  Get the gdas_radmon_base.tar file and open it
# 
   base_file=${RUN}_radmon_base.tar

   if [[ -s ${TANKverf}/info/${base_file} || \
	 -s ${TANKverf}/info/${base_file}.${Z} ]]; then
      cp ${TANKverf}/info/${base_file}* .
   fi

   if [[ ! -s ${base_file} && ! -s ${base_file}.${Z} ]]; then
      cp ${FIXgdas}/${base_file}* .
   fi
   if [[ ! -s ${base_file} && ! -s ${base_file}.${Z} ]]; then
      echo "WARNING:  Unable to locate ${base_file}"
   fi

   if [[ -s ${base_file}.${Z} ]]; then
      $UNCOMPRESS ${base_file}.${Z}
   fi

   tar -xvf ${base_file}
   rm -f ${base_file}

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


#
# loop over SATYPE_LIST  
#
   for sat in ${SATYPE_LIST}; do
      echo sat = $sat

      gunzip time.${sat}.${PDATE}.ieee_d.${Z}
      gunzip time.${sat}.ctl.${Z}


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

      ./radmon_validate_tm.x < input >   stdout.validate.$sat.$ihh


      ${COMPRESS} time.${sat}.${PDATE}.ieee_d

   done              #  end loop over SATYPE_LIST

   rm -f *.base

echo "<-- end validate.sh"
