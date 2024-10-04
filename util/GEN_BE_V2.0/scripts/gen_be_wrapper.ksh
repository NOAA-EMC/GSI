#! /bin/ksh
#-----------------------------------------------------------------------
# Script gen_be_wrapper.ksh
#
# Purpose: Calculates background error statistics for WRF-Var.
#-----------------------------------------------------------------------

#[1] Define job by overriding default environment variables:
# this steps need to be run successively by setting up each variable to true

export RUN_GEN_BE_STAGE0=true  # Run stage 0 (create perturbation files).
export RUN_GEN_BE_STAGE1=false   # Run stage 1 (Remove mean, split variables).
export RUN_GEN_BE_STAGE2=false  # Run stage 2 (Regression coefficients).
export RUN_GEN_BE_STAGE3=false  # Run stage 3 (Vertical Covariances).
export RUN_GEN_BE_STAGE4=false  # Run stage 4 (Horizontal Covariances).
export RUN_GEN_BE_DIAGS=false   # Generate the be.nc file


export GEN_BE_DIR="/glade/gael/code"  # code directory
export START_DATE=2012060312                           # the first perturbation valid date
export END_DATE=2012060312                             # the last perturbation valid date
export NUM_LEVELS=39                                   # number levels - 1
export FC_DIR="/glade/gael/forecast"  # forecast directory"
export RUN_DIR="/glade/gael/code/scripts"            # scripts directory
export WORK_DIR="/glade/gael/code/working" # working directory
export DOMAIN=01
export INTERVAL=12
export STRIDE=1
export FCST_RANGE=0
export FCST_RANGE1=0
export FCST_RANGE2=0

export BE_METHOD="ENS"
export NE="50"

#[2] Run gen_be:
cd "$GEN_BE_DIR/scripts"
./gen_be.ksh
