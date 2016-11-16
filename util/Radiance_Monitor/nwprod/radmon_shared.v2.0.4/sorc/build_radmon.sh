
this_sys=`hostname`
two=`echo $this_sys | cut -c1-2`
one=`echo $this_sys | cut -c1`

#  System is either wcoss, cray, or theia
#         (# = a digit)
#   hostnames on theia are tfe##
#                cray  are s|tlogin#
#                wcoss are t|g##a#

system="wcoss"
if [[ $two = "tf" ]]; then
   system="theia"
elif [[ $two = "tl" || $two = "sl" ]]; then
   system="cray":
fi

echo "system = $system"

module use -a ../modulefiles/${system}
module load RadMonBuild


for dir in verf_radbcoef.fd verf_radtime.fd verf_radang.fd verf_radbcor.fd
do
  cd $dir
  make check_prereqs
  make
  make install
  cd ../
done

module unload RadMonBuild
