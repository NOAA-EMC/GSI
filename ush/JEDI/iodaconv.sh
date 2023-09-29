#!/bin/bash
#SBATCH -J iodaconv
#SBATCH -o iodaconv.o%j
#SBATCH -A da-cpu
#SBATCH -q batch
#SBATCH -p orion
#SBATCH --nodes=1
#SBATCH --exclusive
#SBATCH -t 1:30:00
# run python ioda-iodaconverters
# on GSI netCDF diag files to generate
# IODA formatted observations for UFO H(x)
# cory.r.martin@noaa.gov
set -x

GDASApp=$1
workdir=$2
adate=$3

# source modulefile to get proper python on environment
module purge
module use $GDASApp/modulefiles
module load GDAS/orion
module list 

# executable paths
IODA_iodaconv_iodaconvbin=$GDASApp/build/bin/proc_gsi_ncdiag.py
IODA_iodaconv_iodacombinebin=$GDASApp/build/bin/combine_obsspace.py
#IODA_iodaconv_iodaconvgnssrobin=$GDASApp/build/bin/gnssro_gsidiag2ioda

# make working directory
IODA_data_iodaworkdir=$workdir/iodawork
rm -rf $IODA_data_iodaworkdir
mkdir -p $IODA_data_iodaworkdir
cd $IODA_data_iodaworkdir

# make output directory
IODA_data_iodaoutdir=$workdir/ioda
rm -rf $IODA_data_iodaoutdir/obs
mkdir -p $IODA_data_iodaoutdir/obs
rm -rf $IODA_data_iodaoutdir/geovals
mkdir -p $IODA_data_iodaoutdir/geovals

#export PYTHONPATH=$GDASApp/build/lib/python3.7/pyioda:$PYTHONPATH
#export PYTHONPATH=$GDASApp/build/lib/pyiodaconv:$PYTHONPATH
export PYTHONPATH=$GDASApp/build/lib/python3.7:$PYTHONPATH
export PYTHONPATH=$GDASApp/build/lib/python3.7/pyiodaconv:$PYTHONPATH
#export PYTHONPATH=$GDASApp/iodaconv/src:$PYTHONPATH
#export PYTHONPATH=$PYTHONPATH:$GDASApp/iodaconv/src

#
# run script to generate IODA obs files
$IODA_iodaconv_iodaconvbin -o $IODA_data_iodaoutdir/obs -g $IODA_data_iodaoutdir/geovals $workdir/diags
#$IODA_iodaconv_iodaconvbin -o $IODA_data_iodaoutdir/obs -g $IODA_data_iodaoutdir/geovals $workdir/diags -q True -r True
#
# concatenate these files together
python $IODA_iodaconv_iodacombinebin -i $IODA_data_iodaoutdir/obs/sfc_*.nc4 -o $IODA_data_iodaoutdir/obs/sfc_obs_"$adate".nc4 -g $IODA_data_iodaoutdir/geovals
python $IODA_iodaconv_iodacombinebin -i $IODA_data_iodaoutdir/obs/sfcship_*.nc4 -o $IODA_data_iodaoutdir/obs/sfcship_obs_"$adate".nc4 -g $IODA_data_iodaoutdir/geovals
python $IODA_iodaconv_iodacombinebin -i $IODA_data_iodaoutdir/obs/aircraft_*.nc4 -o $IODA_data_iodaoutdir/obs/aircraft_obs_"$adate".nc4 -g $IODA_data_iodaoutdir/geovals
python $IODA_iodaconv_iodacombinebin -i $IODA_data_iodaoutdir/obs/sondes_ps*.nc4 $IODA_data_iodaoutdir/obs/sondes_q*.nc4 $IODA_data_iodaoutdir/obs/sondes_tsen*.nc4 $IODA_data_iodaoutdir/obs/sondes_tv*.nc4 $IODA_data_iodaoutdir/obs/sondes_uv*.nc4 -o $IODA_data_iodaoutdir/obs/sondes_obs_"$adate".nc4 -g $IODA_data_iodaoutdir/geovals
python $IODA_iodaconv_iodacombinebin -i $IODA_data_iodaoutdir/obs/sondes_ps*.nc4 $IODA_data_iodaoutdir/obs/sondes_q*.nc4 $IODA_data_iodaoutdir/obs/sondes_tv*.nc4 $IODA_data_iodaoutdir/obs/sondes_uv*.nc4 -o $IODA_data_iodaoutdir/obs/sondes_tvirt_obs_"$adate".nc4 -g $IODA_data_iodaoutdir/geovals

# gnssro converter
#ln -sf $IODA_data_iodaoutdir/obs/gnssro_obs_${adate}.nc4 ./gnssro_obs_${adate}.nc4
#$IODA_iodaconv_iodaconvgnssrobin $adate $IODA_data_gsiindir/diag_conv_gps_* 1

date
echo "GSI ncdiag ioda converter script completed"
