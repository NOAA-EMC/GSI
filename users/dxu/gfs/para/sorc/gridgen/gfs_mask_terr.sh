#!/bin/sh
#--------------------------------------------------------------------------------
# Script history log:
# 2007-05-11 G Gayno
#
# Usage: gfs_mask_terr.sh (no arguments)
#
# Imported Shell Variables
#
#   Most variables must be set in the config file.  Some have
#   defaults set in this script that may be overridden by defining it
#   in the config file.
#
#   CHECKDIR            looks for the orog and mask files for the
#                       target gfs grid in this directory.  if they
#                       exist, they are copied to $SAVEDIR to be
#                       used as inputs to the gridgen_sfc program.
#                       otherwise, script ml3b.sh in invoked to
#                       create the mask and orog data - must be
#                       set in config file
#   f1/2                orography filter coefficients - must be set
#                       in config file
#   im                  number of Gaussian longitudes - must be set
#                       in config file
#   jm                  number of Gaussian latitudes - must be set
#                       in config file
#   jcap                spectral triangular truncation - must be set
#                       in config file
#   mt                  mountain data resolution - must be set in
#                       config file.
#   MASK_GFS_FILENAME   name of landmask file for the target gfs grid -
#                       must be set in config file.
#   OROG_GFS_FILENAME   name of orography file for the target gfs grid -
#                       must be set in config file.
#   SAVEDIR             directory where the mask and orog data for
#                       the target grid are saved.  these files may
#                       have been copied from CHECKDIR or created by
#                       the ml3b.sh script.  
#   VERBOSE             verbose flag (YES or NO)
#                       defaults to NO
#
# Modules and files referenced:
#
#  scripts      : config  (contains imported shell variables)
#               : ml3b.sh (runs gfs terrain/landmask program)
#--------------------------------------------------------------------------------

# import shell variables
. ./config

export VERBOSE=${VERBOSE:-"NO"}
if [[ "$VERBOSE" = "YES" ]];then
 echo $(date) EXECUTING $0 $* >&2
 set -x
fi

mkdir -p $SAVEDIR

# if the terrain and land mask already exist for the target gfs grid, copy
# the data to the SAVEDIR directory.  Otherwise, invoke the ml3b.sh script to
# create the terrain and land mask (ml3b.sh places the output in SAVEDIR).  
# the gridgen_sfc program, called by a subsequent script, looks in SAVEDIR
# for the mask and terrain, which it needs to map the land surface fields.

if [[ ! -a ${CHECKDIR}/${OROG_GFS_FILENAME} || ! -a ${CHECKDIR}/${MASK_GFS_FILENAME} ]] 
then

  cp ./ml3b.sh ${SAVEDIR}/ml3b.sh

  cd $SAVEDIR

  ./ml3b.sh  $MASK_GFS_FILENAME $OROG_GFS_FILENAME  mtnvar14_${jcap} ${im} ${jm} ${jcap} ${f1} ${f2} ${mt} > ${SAVEDIR}/ml${jcap}.out

  if [[ $? != 0 ]]
  then
    echo "ERROR CREATING LANDMASK AND TERRAIN"
    exit 1
  fi

  rm -f ${SAVEDIR}/ml3b.sh

else

  echo $OROG_GFS_FILENAME AND $MASK_GFS_FILENAME ALREADY EXISTS

  cp ${CHECKDIR}/${OROG_GFS_FILENAME} $SAVEDIR
  cp ${CHECKDIR}/${MASK_GFS_FILENAME} $SAVEDIR

fi

exit 0