#!/bin/sh
#SBATCH -o getfiles.out
#SBATCH -e getfiles.err
#SBATCH -p u1-service
#SBATCH --ntasks=1
#SBATCH --time=6:00:00
#SBATCH -A da-cpu
#SBATCH -J get_hpss_stuff

bdate=2025041500   # For stand alone GSI just need to change this date
edate=$bdate
cdate=${bdate}

expid=GDAS-ops
prod=v16.3

hpss_base_dir=/NCEPPROD/hpssprod/runhistory

NDATE=/home/Andrew.Collard/bin/ndate
HPSSTAR=/home/Andrew.Collard/bin/hpsstar

while [[ ${cdate} -le ${edate} ]]; do

   # for current analysis cycle
   data_dir=/scratch3/NCEPDEV/stmp/$USER/$expid
   mkdir -p ${data_dir}
   cd ${data_dir}

   y4a=`echo $cdate | cut -c1-4`
   m2a=`echo $cdate | cut -c5-6`
   d2a=`echo $cdate | cut -c7-8`
   h2a=`echo $cdate | cut -c9-10`

   yyyymmdda=${y4a}${m2a}${d2a}
   hha=${h2a}
   yyyymmddhha=${yyyymmdda}${hha}

   # get required initial files from previous cycle 
   gdate=`$NDATE -6 ${cdate}`
   y4g=`echo $gdate | cut -c1-4`
   m2g=`echo $gdate | cut -c5-6`
   d2g=`echo $gdate | cut -c7-8`
   h2g=`echo $gdate | cut -c9-10`

   yyyymmddg=${y4g}${m2g}${d2g}
   hhg=${h2g}
   yyyymmddhhg=${yyyymmddg}${hhg}

   hpssa_dir=${hpss_base_dir}/rh${y4a}/${y4a}${m2a}/${yyyymmdda}
   hpssg_dir=${hpss_base_dir}/rh${y4g}/${y4g}${m2g}/${yyyymmddg}

   # =====================
   #  Get deterministics
   # =====================
   # Get initial conditions related to bias correction from previous (guess) cycle
   $HPSSTAR get ${hpssg_dir}/com_gfs_${prod}_gdas.${yyyymmddg}_${h2g}.gdas_restart.tar ./gdas.${yyyymmddg}/${hhg}/atmos/gdas.t${hhg}z.abias
   $HPSSTAR get ${hpssg_dir}/com_gfs_${prod}_gdas.${yyyymmddg}_${h2g}.gdas_restart.tar ./gdas.${yyyymmddg}/${hhg}/atmos/gdas.t${hhg}z.abias_pc
   $HPSSTAR get ${hpssg_dir}/com_gfs_${prod}_gdas.${yyyymmddg}_${h2g}.gdas_restart.tar ./gdas.${yyyymmddg}/${hhg}/atmos/gdas.t${hhg}z.abias_air
   $HPSSTAR get ${hpssg_dir}/com_gfs_${prod}_gdas.${yyyymmddg}_${h2g}.gdas_restart.tar ./gdas.${yyyymmddg}/${hhg}/atmos/gdas.t${hhg}z.radstat

   # Get initial conditions related to model states from previous(guess) cycle
   $HPSSTAR get ${hpssg_dir}/com_gfs_${prod}_gdas.${yyyymmddg}_${h2g}.gdas_nc.tar ./gdas.${yyyymmddg}/${hhg}/atmos/gdas.t${hhg}z.atmf003.nc
   $HPSSTAR get ${hpssg_dir}/com_gfs_${prod}_gdas.${yyyymmddg}_${h2g}.gdas_nc.tar ./gdas.${yyyymmddg}/${hhg}/atmos/gdas.t${hhg}z.atmf006.nc
   $HPSSTAR get ${hpssg_dir}/com_gfs_${prod}_gdas.${yyyymmddg}_${h2g}.gdas_nc.tar ./gdas.${yyyymmddg}/${hhg}/atmos/gdas.t${hhg}z.atmf009.nc
   $HPSSTAR get ${hpssg_dir}/com_gfs_${prod}_gdas.${yyyymmddg}_${h2g}.gdas_nc.tar ./gdas.${yyyymmddg}/${hhg}/atmos/gdas.t${hhg}z.sfcf003.nc
   $HPSSTAR get ${hpssg_dir}/com_gfs_${prod}_gdas.${yyyymmddg}_${h2g}.gdas_nc.tar ./gdas.${yyyymmddg}/${hhg}/atmos/gdas.t${hhg}z.sfcf006.nc
   $HPSSTAR get ${hpssg_dir}/com_gfs_${prod}_gdas.${yyyymmddg}_${h2g}.gdas_nc.tar ./gdas.${yyyymmddg}/${hhg}/atmos/gdas.t${hhg}z.sfcf009.nc

   $HPSSTAR get ${hpssg_dir}/com_gfs_${prod}_enkfgdas.${yyyymmddg}_${h2g}.enkfgdas.tar ./enkfgdas.${yyyymmddg}/${hhg}/atmos/gdas.t${hhg}z.sfcf006.ensmean.nc

   # =============================================================
   #  Get ensembles
   #  ./enkfgdas.20200914/18/atmos/mem002/gdas.t18z.atmf003.nc
   # =============================================================
#   $HPSSTAR get ${hpssg_dir}/com_gfs_${prod}_enkfgdas.${yyyymmddg}_${h2g}.enkfgdas_restart_grp1.tar . &
#   $HPSSTAR get ${hpssg_dir}/com_gfs_${prod}_enkfgdas.${yyyymmddg}_${h2g}.enkfgdas_restart_grp1.tar . &
#   $HPSSTAR get ${hpssg_dir}/com_gfs_${prod}_enkfgdas.${yyyymmddg}_${h2g}.enkfgdas_restart_grp1.tar . &
#   $HPSSTAR get ${hpssg_dir}/com_gfs_${prod}_enkfgdas.${yyyymmddg}_${h2g}.enkfgdas_restart_grp1.tar . &

#   $HPSSTAR get ${hpssg_dir}/com_gfs_${prod}_enkfgdas.${yyyymmddg}_${h2g}.enkfgdas_restart_grp1.tar . &

   cdate=`$NDATE +6 ${cdate}`
done

