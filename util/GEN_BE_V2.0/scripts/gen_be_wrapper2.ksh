#! /bin/ksh
#-----------------------------------------------------------------------
# Script gen_be_wrapper.ksh
#
# Purpose: Calculates background error statistics
#-----------------------------------------------------------------------

#[1] Define job by overriding default environment variables:
# this steps need to be run successively by setting up each variable to true

export RUN_GEN_BE_STAGE0=false  # Run stage 0 (create perturbation files).
export RUN_GEN_BE_STAGE1=false   # Run stage 1 (Remove mean, split variables).
export RUN_GEN_BE_STAGE2=false  # Run stage 2 (Regression coefficients).
export RUN_GEN_BE_STAGE3=false  # Run stage 3 (Vertical Covariances).
export RUN_GEN_BE_STAGE4=true  # Run stage 4 (Horizontal Covariances).
export RUN_GEN_BE_DIAGS=true   # Generate the be.nc file


export GEN_BE_DIR="/glade/p/work/gael/archive/mpas_gen_be/gen_be_gmd_021415/gmdd-7-4291-2014-supplement-4/code"  # code directory
export START_DATE=2012060312                           # the first perturbation valid date
export END_DATE=2012060312                             # the last perturbation valid date
export NUM_LEVELS=39                                   # number levels - 1
export FC_DIR="/glade/p/work/gael/archive/mpas_gen_be/gen_be_gmd_021415/forecast/2012060312"  # forecast directory"
export RUN_DIR="${GEN_BE_DIR}/scripts"            # scripts directory
export WORK_DIR="/glade/scratch/gael/test2/working_05m_gmd" # working directory
export DOMAIN=01
export INTERVAL=12
export STRIDE=1
export FCST_RANGE=0
export FCST_RANGE1=0
export FCST_RANGE2=0

export BE_METHOD="ENS"
export NE="05"

#[2] Run gen_be:
cd "$GEN_BE_DIR/scripts"
./gen_be.ksh
