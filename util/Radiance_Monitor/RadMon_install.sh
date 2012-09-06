#! /bin/ksh

#------------------------------------------------------------------
#  RadMon_install.sh
#
#  This script makes sets all necessary configuration definitions
#  and calls the makeall.sh script to build all the necessary 
#  executables.  This script works for both the CCS and Zeus 
#  machines.
#
#------------------------------------------------------------------
echo
echo
echo "Beginning Radiance Monitor Package RadMon_install.sh script"
echo
sleep 2

my_os=`uname -s`
my_os=`echo ${my_os} | tr '[:upper:]' '[:lower:]'`
my_osys="export MY_OS=$my_os"

machine_line=`uname -m`
my_machine=`echo $machine_line | awk '{print tolower($1)}'`

if [[ $my_machine = "x86_64" ]]; then
   lendian=1
else
   lendian=0
fi
little_endian="export LITTLE_ENDIAN=$lendian"


if [[ ${my_os} = "linux" || ${my_os} = "aix" ]]; then


   #
   #  Identify basedir location of package
   #
   echo "locating and saving RadMon package location"
   sleep 1
   radmon=`dirname $0`
   if [[ $radmon = "." ]]; then
      radmon=`pwd`
   fi
   my_radmon="export MY_RADMON=$radmon"
   echo $my_radmon
   echo
   echo
   sleep 1
 
   #
   #  TANKDIR location
   #
   if [[ ${my_os} = "linux" ]]; then
      tankdir=/scratch2/portfolios/NCEPDEV/global/save/${USER}/nbns
   else
      tankdir=/global/save/${USER}/nbns
   fi
   
   echo "Please specify TANKDIR location for storage of data and image files."
   echo "  Return to accept default location or enter new location now."
   echo
   echo "  Default TANKDIR:  $tankdir"
   echo "?"

   read new < /dev/tty
   len_new=`echo $new | awk '{print length($0)}'`

   if [[ $len_new -gt 0 ]]; then
      tankdir=$new
   fi
   my_tankdir="export MY_TANKDIR=$tankdir"
   echo
   echo $my_tankdir
   echo
   echo
   sleep 1

   #
   #  Web server name
   #
   new=
   server="emcrzdm"
   echo "Please specify web server name."
   echo "  Return to accept default server or enter server name now."
   echo
   echo "  Default web server:  $server"
   echo "?"
   read new < /dev/tty
   len_new=`echo $new | awk '{print length($0)}'`

   if [[ $len_new -gt 0 ]]; then
      server=$new
   fi
   web_svr="export WEB_SVR=$server"
   echo
   echo $web_svr
   echo
   echo
   sleep 1


   #
   #  Web server name
   #
   new=
   webuser="$USER"
   echo "Please specify your user name on the web server."
   echo "  Return to accept default user name (this login ID) or enter different user name now."
   echo
   echo "  Default user name on web server:  $webuser"
   echo "?"
   read new < /dev/tty
   len_new=`echo $new | awk '{print length($0)}'`

   if [[ $len_new -gt 0 ]]; then
      webuser=$new
   fi
   web_user="export WEB_USER=$webuser"
   echo
   echo $web_user
   echo
   echo
   sleep 1


   #
   #  Web directory 
   #
   new=
   webdir=/home/people/emc/www/htdocs/gmb/gdas/radiance/${webuser}

   echo "Please specify the top level web site directory location on server"
   echo "  Return to accept default directory location or enter different location now."
   echo
   echo "  Default directory on web server:  $webdir"
   echo "?"
   read new < /dev/tty
   len_new=`echo $new | awk '{print length($0)}'`

   if [[ $len_new -gt 0 ]]; then
      webdir=$new
   fi
   web_dir="export WEBDIR=$webdir"
   echo
   echo $web_dir
   echo
   echo
   sleep 1

   if [[ $MY_OS = "aix" ]]; then
      my_ptmp="export PTMP=/ptmp"
      my_stmp="export STMP=/stmp"
   else
      my_ptmp="export PTMP=/scratch2/portfolios/NCEPDEV/ptmp"
      my_stmp="export STMP=/scratch2/portfolios/NCEPDEV/stmp"
   fi

   radmon_config=parm/RadMon_config

   sed "/export MY_RADMON/c $my_radmon" $radmon_config >tmp1
   sed "/export MY_TANKDIR/c $my_tankdir" tmp1 > tmp2
   mv -f tmp2 tmp1
   sed "/export WEB_SVR/c $web_svr" tmp1 > tmp2
   mv -f tmp2 tmp1
   sed "/export WEB_USER/c $web_user" tmp1 > tmp2
   mv -f tmp2 tmp1
   sed "/export WEBDIR/c $web_dir" tmp1 > tmp2
   mv -f tmp2 tmp1
   sed "/export LITTLE_ENDIAN/c $little_endian" tmp1 > tmp2
   mv -f tmp2 tmp1
   sed "/export MY_OS/c $my_osys" tmp1 > tmp2
   mv -f tmp2 tmp1
   sed "/export PTMP/c $my_ptmp" tmp1 > tmp2
   mv -f tmp2 tmp1
   sed "/export STMP/c $my_stmp" tmp1 > tmp2
   mv -f tmp2 tmp1

   mv -f tmp1 $radmon_config
   rm -f tmp1

   #
   #  build all executables in package
   #
   makeall.sh 

else
   echo ${my_os} is not supported 
fi

exit

