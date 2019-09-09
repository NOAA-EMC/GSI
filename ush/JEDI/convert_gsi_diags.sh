#!/bin/bash
#SBATCH -J convert_diags_jedi 
#SBATCH -A da-cpu
#SBATCH -q batch 
#SBATCH --nodes=1
#SBATCH -t 2:00:00
#SBATCH --mail-user=$LOGNAME@noaa.gov
OutDir=$1
DATE=2018041500

# load modules here used to compile GSI
module purge
module use -a /scratch1/NCEPDEV/da/Cory.R.Martin/Modulefiles
module load modulefile.ProdGSI.hera
module list

# load python module from Stelios
module use -a /home/Stylianos.Flampouris/modulefiles
module load anaconda/2019.08.07
module load nccmp # for ctests

IODACDir=/scratch1/NCEPDEV/da/Cory.R.Martin/JEDI/ioda-converters/build/bin

cd $IODACDir

rm -rf $OutDir/obs
rm -rf $OutDir/geoval
rm -rf $OutDir/log.proc_gsi_ncdiag
mkdir -p $OutDir/obs
mkdir -p $OutDir/geoval

python ./proc_gsi_ncdiag.py -n 24 -o $OutDir/obs -g $OutDir/geoval $OutDir/GSI_diags

# subset obs
python ./subset_files.py -n 24 -m $OutDir/obs -g $OutDir/geoval
python ./subset_files.py -n 24 -s $OutDir/obs -g $OutDir/geoval

# combine conventional obs
python ./combine_conv.py -i $OutDir/obs/sfc_*m.nc4 -o $OutDir/obs/sfc_obs_"$DATE"_m.nc4 -g $OutDir/geoval/
python ./combine_conv.py -i $OutDir/obs/sfcship_*m.nc4 -o $OutDir/obs/sfcship_obs_"$DATE"_m.nc4 -g $OutDir/geoval/
python ./combine_conv.py -i $OutDir/obs/aircraft_*m.nc4 -o $OutDir/obs/aircraft_obs_"$DATE"_m.nc4 -g $OutDir/geoval/
python ./combine_conv.py -i $OutDir/obs/sondes_ps*m.nc4 $OutDir/obs/sondes_q*m.nc4 $OutDir/obs/sondes_tsen*m.nc4 $OutDir/obs/sondes_uv*m.nc4 -o $OutDir/obs/sondes_obs_"$DATE"_m.nc4 -g $OutDir/geoval/
python ./combine_conv.py -i $OutDir/obs/sondes_ps*m.nc4 $OutDir/obs/sondes_q*m.nc4 $OutDir/obs/sondes_tv*m.nc4 $OutDir/obs/sondes_uv*m.nc4 -o $OutDir/obs/sondes_tvirt_obs_"$DATE"_m.nc4 -g $OutDir/geoval/
python ./combine_conv.py -i $OutDir/obs/sfc_*s.nc4 -o $OutDir/obs/sfc_obs_"$DATE"_s.nc4 -g $OutDir/geoval/
python ./combine_conv.py -i $OutDir/obs/sfcship_*s.nc4 -o $OutDir/obs/sfcship_obs_"$DATE"_s.nc4 -g $OutDir/geoval/
python ./combine_conv.py -i $OutDir/obs/aircraft_*s.nc4 -o $OutDir/obs/aircraft_obs_"$DATE"_s.nc4 -g $OutDir/geoval/
python ./combine_conv.py -i $OutDir/obs/sondes_ps*s.nc4 $OutDir/obs/sondes_q*s.nc4 $OutDir/obs/sondes_tsen*s.nc4 $OutDir/obs/sondes_uv*s.nc4 -o $OutDir/obs/sondes_obs_"$DATE"_s.nc4 -g $OutDir/geoval/
python ./combine_conv.py -i $OutDir/obs/sondes_ps*s.nc4 $OutDir/obs/sondes_q*s.nc4 $OutDir/obs/sondes_tv*s.nc4 $OutDir/obs/sondes_uv*s.nc4 -o $OutDir/obs/sondes_tvirt_obs_"$DATE"_s.nc4 -g $OutDir/geoval/
