workdir=/global/noscrub/wx23dc/preg2
suff="siganl.ecm2 siganl.gdas sfcanl.gdas prepqc.gdas"  
yyyymm=200806
cd $workdir
for file in $suff; do
/u/wx20mi/bin/hpsstar put ecmarchive/${file}${yyyymm}.tar ${file}*
done

/u/wx20mi/bin/hpsstar put ecmarchive/sigsfcecm${yyyymm}.tar sig*gfs*${yyyymm}* sfc*gfs*${yyyymm}*
/u/wx20mi/bin/hpsstar put ecmarchive/pgbecm${yyyymm}.tar pgb*gfs*${yyyymm}*
