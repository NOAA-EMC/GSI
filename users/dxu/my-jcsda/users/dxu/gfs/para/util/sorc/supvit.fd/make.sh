#!/bin/sh
###############################################################
#
#   AUTHOR:  Michaud
#
#   DATE:    Mar 29 2000
#
#   PURPOSE: To make gfdl_fcst module
#
#   REMARKS:  
#
###############################################################

set -x 

make_dir=/nfstmp/$(whoami)/fcst.fd
sorc_dir=$(pwd)
exec_dir=$(pwd)

mkdir -p $make_dir

cp $sorc_dir/* $make_dir/.

cd $make_dir

###############################################################
# Make fcst
###############################################################
make

###############################################################
# Change Permissions on Executable
###############################################################
cp $make_dir/supvit $exec_dir/.

###############################################################
# Remove Temporary Working Directory $make_dir
###############################################################
cd /nfstmp
rm -rf $make_dir

