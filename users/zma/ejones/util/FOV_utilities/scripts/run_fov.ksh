#!/bin/ksh
#---------------------------------------------------------------------------
# 
# Purpose:
# --------
# Run the gsi field of view code for conical and crosstrack instruments
# in a 'stand-alone' mode.  GRADS is required to visualize output.
#
# Program:
# --------
# This script runs a simple Fortran program that computes the FOV
# size and relative antenna power for numerous instruments.  The
# program is located in the ../sorc directory.  To compile, 
# type 'make'.  Then ensure the path ($EXE_PATH) and name of 
# the executable ($EXE) are set below.
#
# Setup:
# ------
# Before running, you must set the following namelist options below:
# 
# instr - instrument number.  
#   Crosstrack scanners:
#         1 = AVHRR-2 LAC/HRPT
#         2 = AVHRR-3 LAC/HRPT
#         3 = AVHRR-3 LAC/HRPT on NOAA-16
#         4 = HIRS-2
#         5 = HIRS-2I
#         6 = HIRS-3 NOAA-K
#         7 = HIRS-3 NOAA-L,M
#         8 = HIRS-4
#         9 = SSU
#        10 = MSU
#        11 = AMSU-A
#        12 = AMSU-B, HSB
#        13 = MHS
#        14 = ATMS 5.2 DEG
#        15 = ATMS 2.2 DEG
#        16 = ATMS 1.1 DEG
#        17 = AIRS
#        18 = IASI
#   Conical scanners
#        25 = DEFAULT CIRCULAR FOV
#        26 = F16 SSMIS (CAN ALSO BE USED FOR AMSRE)
#        27 = F17 SSMIS
#        28 = F18 SSMIS
#        29 = F19 SSMIS
#        30 = F20 SSMIS
#
# satid - satellite id.  valid choices are:
#         - 'tirosn'
#         - 'dmsp' 
#         - 'f13', 'f14', 'f15', 'f16', 'f17'  
#         - 'trmm'
#         - 'aura'
#         - 'aqua'
#         - 'metop-a' 'metop-b', 'metop-c'
#         - 'n05', 'n06', 'n07', 'n08', 'n09',
#           'n10', 'n11', 'n12', 'n14', 'n15',
#           'n16', 'n17', 'n18', 'n19'
#
# fov_num - field of view number. valid ranges are:
#           All AVHRR: 1 thru 2048
#           All HIRS:  1 thru 56
#           SSU:       1 thru 8
#           MSU:       1 thru 11
#           MHS:       1 thru 13
#           AMSU-A:    1 thru 30
#           AMSU-B:    1 thru 90
#           AIRS:      1 thru 90
#           All ATMS:  1 thru 96
#           IASI:      1 thru 120
#           OTHERS:    NOT USED FOR CONICAL SENSORS  SET TO -999
#
# sat_az  - satellite azimuth angle (degrees)
#
# lat_cent_fov - latitude of center of field of view (degrees)
#
# lon_cent_fov - longitude of center of field of view (degrees)
#
# Output GRADS station files:
# --------------------------
# 
# The edge of the FOV may be visualized with:
#
# ellipse.dat  (data file)
# ellipse.map  (station map file)
# ellipse.ctl  (control file)
#
# The returned power from within the FOV may be visualized with:
#
# power.dat  (data file)
# power.map  (station map file)
# power.ctl  (control file)
#
# For some sensors, the FOV size and/or the antenna power is channel specific:
#   AMSUA - antenna power channel specific (15 channels)
#   MHS   - antenna power channel specific (5 channels)
#   SSMIS - FOV size and antenna power channel specific (24 channels)
#
# Multiple channels are stored as mulitple 'time' levels.  For example,
# to view channel 3, do a "set t 3" during your GRADS session.
#
# Working directory:
# ------------------
# Set in variable $WORK
#
# Standard output from Fortran program:
# ------------------------------------
# Placed in $WORK/log
#
#---------------------------------------------------------------------------

#set -x

#---------------------------------------------------------------------------
# Location of program executable.
#---------------------------------------------------------------------------

EXE_PATH="../sorc"
EXE="fov_util.exe"

if [ ! -f $EXE_PATH/$EXE ]; then
  echo ERROR: PROGRAM EXECUTABLE NOT FOUND
  exit 88
fi

#---------------------------------------------------------------------------
# Working directory.
#---------------------------------------------------------------------------

WORK="/ptmpp1/$LOGNAME/fov_util"
mkdir -p $WORK
cp $EXE_PATH/$EXE $WORK
cd $WORK

#---------------------------------------------------------------------------
# Input namelist
#---------------------------------------------------------------------------

rm -f config.nml
cat > config.nml << !
  &SETUP
  instr=26
  satid='f16'
  fov_num=-999
  sat_az=90.0
  lat_cent_fov = 0.
  lon_cent_fov = -0.2
  /
!

rm -f ellipse.dat power.dat log
rm -f ellipse.ctl ellipse.map
rm -f power.map power.ctl

#---------------------------------------------------------------------------
# Run program.
#---------------------------------------------------------------------------

$EXE > log

status=$?
if (( status != 0 ));then
  echo ERROR IN PROGRAM
  echo EXIT WITH STATUS CODE $status
  exit $status
fi

#---------------------------------------------------------------------------
# Create GRADS map and control file for visualizing edge of FOV.
#---------------------------------------------------------------------------

INSTR=$(grep instr config.nml)
n=$(echo ${INSTR##*=})
if (( n >= 25 && n <= 30 )); then
  num_ch=24
else
  num_ch=1
fi

cat > ellipse.ctl << !
dset ^ellipse.dat
dtype station
stnmap ^ellipse.map
options sequential
undef -999.0
title junk
tdef ${num_ch} linear jan1980 1mo
vars  1
 p  0 99 fov
endvars
!

stnmap -i ellipse.ctl

#---------------------------------------------------------------------------
# Create GRADS map and control file for visualizing returned power.
#---------------------------------------------------------------------------

if (( n == 11 )); then
  num_ch=15
elif (( n == 13 )); then
  num_ch=5
elif (( n >= 25 && n <= 30 )); then
  num_ch=24
else
  num_ch=1
fi

cat > power.ctl << !
dset ^power.dat
dtype station
stnmap ^power.map
options sequential
undef -999.0
title junk
tdef ${num_ch} linear jan1980 1mo
vars  1
 p  0 99 fov
endvars
!

stnmap -i power.ctl

echo DONE

exit 0
