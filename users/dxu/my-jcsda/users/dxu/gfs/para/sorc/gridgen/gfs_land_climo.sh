#!/bin/sh
#--------------------------------------------------------------------------------
# Script history log:
# 2007-05-11 G Gayno
#
# Usage: gfs_land_climo.sh (no arguments)
#
# Imported Shell Variables
#
#   Some variables must be set in the config file.  Some have
#   defaults set in this script that are overridden by defining it
#   in the config file.
#
#   GRIDGEN_SFC_EXE     path/name of the gridgen_sfc program which maps
#                       climatological data to the target gfs grid.
#                       defaults to /nwprod/exec/gridgen_sfc.exe
#   im                  number of Gaussian longitudes - must be set
#                       in config file
#   jm                  number of Gaussian latitudes - must be set
#                       in config file
#   jcap                spectral triangular truncation - must be set
#                       in config file
#   MASK_GFS_FILENAME   name of landmask file for target gfs grid -
#                       must be set in config file.
#   MXSALB_GFS_FILENAME name of the maximum snow albedo file on the
#                       target GFS grid.  defaults to "global_mxsalbedo.${jcap}.grb"
#   MXSALB_SRC_FILE     path/name of the input maximum snow albedo data
#                       defaults to legacy data in /nwprod/fix
#   OROG_GFS_FILENAME   name of orography file for target gfs grid - 
#                       must be set in config file.
#   SAVEDIR             directory where the data output from the
#                       gridgen_sfc program is saved - must be set in
#                       config file
#   SFALB_GFS_FILENAME  name of the snow-free albedo file on the
#                       target GFS grid.  defaults to "global_albedo.${jcap}.grb"
#   SFALB_SRC_FILE      path/name of the input snow-free albedo data - 
#                       defaults to legacy data in /nwprod/fix
#   VERBOSE             verbose flag (YES or NO)
#                       defaults to NO
#   WRKDIR              working directory.  no default, must set in config file
#
# Modules and files referenced:
#
#  scripts      : config  (contains imported shell variables)
#
#  input data   : fort.81 - configuration namelist for gridgen_sfc program
#                 MXSALB_SRC_FILE
#                 SFALB_SRC_FILE
#                 MASK_GFS_FILENAME
#                 OROG_GFS_FILENAME
#
#  output data  : SFALB_GFS_FILENAME
#                 MXSALB_GFS_FILENAME
#--------------------------------------------------------------------------------

# import shell variables
. ./config

VERBOSE=${VERBOSE:-"NO"}
if [[ "$VERBOSE" = "YES" ]];then
 echo $(date) EXECUTING $0 $* >&2
 set -x
fi

mkdir -p $WRKDIR
rm -fr ${WRKDIR}/*

cd $WRKDIR

SFALB_GFS_FILENAME=${SFALB_GFS_FILENAME:-"global_albedo.t${jcap}.grb"}
MXSALB_GFS_FILENAME=${MXSALB_GFS_FILENAME:-"global_mxsalbedo.t${jcap}.grb"}

GRIDGEN_SFC_EXE=${GRIDGEN_SFC_EXE:-"/nwprod/exec/gridgen_sfc.exe"}
cp $GRIDGEN_SFC_EXE $WRKDIR

MXSALB_SRC_FILE=${MXSALB_SRC_FILE:-"/nwprod/fix/global_snoalb.1x1.grb"}
SFALB_SRC_FILE=${SFALB_SRC_FILE:-"/nwprod/fix/global_albedo4.1x1.grb"}

cat > ${WRKDIR}/fort.81 << !

&grid
 domain_name="t${jcap}"
 imdl=$im
 jmdl=$jm
/

&tiling
 max_total_land_tiles=1
/

&veg_tiling
 max_veg_tiles=1
 veg_tile_threshold=0.0
 default_veg_category=7
 num_veg_groups = 6
 veg_groups =3,4,4,4,4,4,4,4,4,4,1,1,1,1,1,9,5,5,2,6,6,6,2,2
/

&soil_tiling
 max_soil_tiles=1
 soil_tile_threshold=0.0
 default_soil_category=7
 num_soil_groups = 7
 soil_groups = 3,6,6,4,4,2,5,1,1,5,1,1,2,9,7,6
/

&lsmask_orog_tiling
 max_orog_tiles=1
 orog_bin_width=500.0
 orog_tile_threshold=0.05
 lsmask_tiles=.false.
 lsmask_tile_threshold=0.3
 smooth=2
 num_smooth_passes=0
/

&input_data
 leaf_area_idx_file=""
 glacier_file=""
 gfrac_file=""
 mxsnow_alb_file="${MXSALB_SRC_FILE}"
 roughness_file=""
 seaice_file=""
 slopetype_file=""
 snow_climo_file=""
 snowfree_albedo_file="${SFALB_SRC_FILE}"
 soilm_file=""
 soiltype_tile_file=""
 soiltype_notile_file=""
 sst_climo_file=""
 substrate_temp_file=""
 vegtype_tile_file=""
 vegtype_notile_file=""
 lsmask_file="${SAVEDIR}/${MASK_GFS_FILENAME}"
 orog_file="${SAVEDIR}/${OROG_GFS_FILENAME}"
/

!

ln -fs ${WRKDIR}/${SFALB_GFS_FILENAME}      ${WRKDIR}/fort.46
ln -fs ${WRKDIR}/${MXSALB_GFS_FILENAME}     ${WRKDIR}/fort.45

export MP_STDOUTMODE="0"

timex poe $GRIDGEN_SFC_EXE 

if [[ $? != 0 ]]
then
   echo "ERROR IN GRIDGEN_SFC PROGRAM"
   exit 1
fi

cp ${WRKDIR}/${SFALB_GFS_FILENAME} ${SAVEDIR}/${SFALB_GFS_FILENAME}
cp ${WRKDIR}/${MXSALB_GFS_FILENAME} ${SAVEDIR}/${MXSALB_GFS_FILENAME}

if [[ "$VERBOSE" = "NO" ]];then
  echo REMOVE ${WRKDIR}
  cd $SAVEDIR
  rm -f ${WRKDIR}/* 
fi

exit 0

