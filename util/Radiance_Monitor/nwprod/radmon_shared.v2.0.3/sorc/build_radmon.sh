module use -a ../modulefiles/wcoss
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
