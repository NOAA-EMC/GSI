#!/bin/bash
# run_gsincdiag_iodaconv.sh
# run python ioda-iodaconverters
# on GSI netCDF diag files to generate
# IODA formatted observations for UFO H(x)
# cory.r.martin@noaa.gov
set -x
## variable definitions
MYDIR=`dirname "$0"`
YAMLFILE=$1
## source helper functions
source $MYDIR/helpers/parse_yaml.sh

## read YAML config file
if [[ -e $YAMLFILE ]]; then
   eval $(parse_yaml $YAMLFILE "IODA_")
else
  echo "ERROR: YAML FILE $YAMLFILE DOES NOT EXIST, ABORT!"
  exit 1
fi

# source modulefile to get proper python on environment
source $IODA_env_modulefile

# make working directory
rm -rf $IODA_data_iodaworkdir
mkdir -p $IODA_data_iodaworkdir
cd $IODA_data_iodaworkdir

# make output directory
rm -rf $IODA_data_iodaoutdir
mkdir -p $IODA_data_iodaoutdir

#
# run script
python $IODA_iodaconv_iodaconvbin -n 20 -o $IODA_data_iodaoutdir -g $IODA_data_geovaloutdir $IODA_data_gsiindir

# subset obs
python $IODA_iodaconv_subsetbin -n 24 -m $IODA_data_iodaoutdir -g $IODA_data_geovaloutdir
python $IODA_iodaconv_subsetbin -n 24 -s $IODA_data_iodaoutdir -g $IODA_data_geovaloutdir

# combine conventional obs
python $IODA_iodaconv_combineconvbin -i $IODA_data_iodaoutdir/sfc_*m.nc4 -o $IODA_data_iodaoutdir/sfc_obs_"$adate"_m.nc4 -g $IODA_data_geovaloutdir/
python $IODA_iodaconv_combineconvbin -i $IODA_data_iodaoutdir/sfcship_*m.nc4 -o $IODA_data_iodaoutdir/sfcship_obs_"$adate"_m.nc4 -g $IODA_data_geovaloutdir/
python $IODA_iodaconv_combineconvbin -i $IODA_data_iodaoutdir/aircraft_*m.nc4 -o $IODA_data_iodaoutdir/aircraft_obs_"$adate"_m.nc4 -g $IODA_data_geovaloutdir/
python $IODA_iodaconv_combineconvbin -i $IODA_data_iodaoutdir/sondes_ps*m.nc4 $IODA_data_iodaoutdir/sondes_q*m.nc4 $IODA_data_iodaoutdir/sondes_tsen*m.nc4 $IODA_data_iodaoutdir/sondes_uv*m.nc4 -o $IODA_data_iodaoutdir/sondes_obs_"$adate"_m.nc4 -g $IODA_data_geovaloutdir/
python $IODA_iodaconv_combineconvbin -i $IODA_data_iodaoutdir/sondes_ps*m.nc4 $IODA_data_iodaoutdir/sondes_q*m.nc4 $IODA_data_iodaoutdir/sondes_tv*m.nc4 $IODA_data_iodaoutdir/sondes_uv*m.nc4 -o $IODA_data_iodaoutdir/sondes_tvirt_obs_"$adate"_m.nc4 -g $IODA_data_geovaloutdir/
python $IODA_iodaconv_combineconvbin -i $IODA_data_iodaoutdir/sfc_*s.nc4 -o $IODA_data_iodaoutdir/sfc_obs_"$adate"_s.nc4 -g $IODA_data_geovaloutdir/
python $IODA_iodaconv_combineconvbin -i $IODA_data_iodaoutdir/sfcship_*s.nc4 -o $IODA_data_iodaoutdir/sfcship_obs_"$adate"_s.nc4 -g $IODA_data_geovaloutdir/
python $IODA_iodaconv_combineconvbin -i $IODA_data_iodaoutdir/aircraft_*s.nc4 -o $IODA_data_iodaoutdir/aircraft_obs_"$adate"_s.nc4 -g $IODA_data_geovaloutdir/
python $IODA_iodaconv_combineconvbin -i $IODA_data_iodaoutdir/sondes_ps*s.nc4 $IODA_data_iodaoutdir/sondes_q*s.nc4 $IODA_data_iodaoutdir/sondes_tsen*s.nc4 $IODA_data_iodaoutdir/sondes_uv*s.nc4 -o $IODA_data_iodaoutdir/sondes_obs_"$adate"_s.nc4 -g $IODA_data_geovaloutdir/
python $IODA_iodaconv_combineconvbin -i $IODA_data_iodaoutdir/sondes_ps*s.nc4 $IODA_data_iodaoutdir/sondes_q*s.nc4 $IODA_data_iodaoutdir/sondes_tv*s.nc4 $IODA_data_iodaoutdir/sondes_uv*s.nc4 -o $IODA_data_iodaoutdir/sondes_tvirt_obs_"$adate"_s.nc4 -g $IODA_data_geovaloutdir/


if [[ "$IODA_iodaconv_cleanup" = "true" ]]; then
  cd $IODA_data_iodaoutdir
  rm -rf $IODA_data_iodaworkdir
fi
date
echo "GSI ncdiag ioda converter script completed"
