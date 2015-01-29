#! /bin/ksh

#------------------------------------------------------------------
#  makeall.sh
#
#  This scripts makes each of the ConvMon executables in the sorc 
#  directory. 
#
#  An optional argument to this script is "clean".  Use this if 
#  you wish to remove *.o, *.mod, and *.x files in the sorc
#  directory.  If "clean" is not used, "all" is assumed.
#------------------------------------------------------------------

mode=${1:-all}

top_level=`pwd`

machine=`/usr/bin/perl ./scripts/get_hostname.pl`
echo machine = $machine

executables="conv_time grads_sfc make_t read_q grads_lev grads_sfctime read_ps read_uv grads_mandlev grads_sig read_pw"

if [[ ${machine} = "wcoss" || ${machine} = "ccs" || ${machine} = "zeus" ]]; then
   cd ./sorc
   rm -f Makefile.conf
   cp -f ../parm/Makefile.conf.${machine} Makefile.conf

   for var in ${executables}; do
      echo make ${var} ${mode}
      echo ""
      echo ""
      make -f makefile.${var} ${mode}
      echo ""
   done

else
   echo ${machine} is not supported 
fi


exit

