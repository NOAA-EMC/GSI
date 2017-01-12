#!/bin/ksh
#-----------------------------------------------------------------------
# Script: gen_be_set_defaults.ksh
#
# Purpose: This scripts sets the environment variables used within the 
# entire scripts system to values corresponding to a "standard case".
# The standard case currently used is the con200 application.
# The namelist parameters specified here is that sub-set of the entire
# range of parameters for all namelists that we have found necessary
# to change through experience of the applications tested so far. As
# new applications are tests, additional environment valiables may
# be added. 

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

# External libraries:
export NCARG_ROOT=${NCARG_ROOT:-/usr/local/ncarg}    # NCAR graphics (required only for plots).

# Decide which stages to run (run if true):
export RUN_GEN_BE_STAGE0=${RUN_GEN_BE_STAGE0:-false} # Run stage 0 (create perturbation files).
export RUN_GEN_BE_STAGE1=${RUN_GEN_BE_STAGE1:-false} # Run stage 1 (Remove mean, split variables).
export RUN_GEN_BE_STAGE2=${RUN_GEN_BE_STAGE2:-false} # Run stage 2 (Regression coefficients).
export RUN_GEN_BE_STAGE3=${RUN_GEN_BE_STAGE3:-false} # Run stage 3 (Vertical Covariances).
export RUN_GEN_BE_STAGE4=${RUN_GEN_BE_STAGE4:-true} # Run stage 4 (Horizontal Covariances).
export RUN_GEN_BE_DIAGS=${RUN_GEN_BE_DIAGS:-false}   # Run gen_be diagnostics.
export RUN_GEN_BE_DIAGS_READ=${RUN_GEN_BE_DIAGS_READ:-false}   # Run gen_be diagnostics_read.
export RUN_GEN_BE_HOLM_VARIANCE=${RUN_GEN_BE_HOLM_VARIANCE:-false}   # Run gen_be holm Variance     
export RUN_GEN_BE_MULTICOV=${RUN_GEN_BE_MULTICOV:-false} # Set to calculate multivariate correlations.
export RUN_GEN_BE_GRAPHICS=${RUN_GEN_BE_GRAPHICS:-false} # Set to produce graphics.
export RUN_GEN_BE_HISTOG=${RUN_GEN_BE_HISTOG:-false} # Set to calculate Hitograms for MBE          
export RUN_GEN_BE_VARIANCE_CONTRIB=${RUN_GEN_BE_VARIANCE_CONTRIB:-false} # Set to calculate Hitograms for MBE          
#export RUN_GEN_BE_MULTICOV_CONTRIB=${RUN_GEN_BE_MULTICOV_CONTRIB:-false} # Set to calculate contrib for MBE          
#export RUN_GEN_BE_GSI_STAGE0=${RUN_GEN_BE_GSI_STAGE0:-false} # Run stage 0 (create perturbation files).
#export RUN_GEN_BE_GSI_STAGE1=${RUN_GEN_BE_GSI_STAGE1:-false} # Run stage 1 (Remove mean, split variables).
#export RUN_GEN_BE_GSI_STAGE2=${RUN_GEN_BE_GSI_STAGE2:-false} # Run stage 2 (Regression coefficients).
#export BY_LEVELS=${BY_LEVELS:-True} 

export DOMAIN=${DOMAIN:-01}                          # domain id.
export START_DATE=${START_DATE:-2003010200}          # Time of first perturbation.
export END_DATE=${END_DATE:-2003012812}              # Time of last perturbation.
export FCST_RANGE=${FCST_RANGE:-24}                  # Forecast range of forecast (hours).
export FCST_RANGE1=${FCST_RANGE1:-24}                # Forecast range of forecast 1 (hours).
export FCST_RANGE2=${FCST_RANGE2:-12}                # Forecast range of forecast 2 (hours).
export INTERVAL=${INTERVAL:-12}                      # Period between files (hours).
export BE_METHOD=${BE_METHOD:-NMC}                   # NMC (NMC-method), ENS (Ensemble-Method).
export NE=${NE:-1}                                   # Number of ensemble members (for ENS).
export BIN_TYPE=${BIN_TYPE:-5}                       # 0=None, 1=1:ni, 2=latitude, ....
export LAT_MIN=${LAT_MIN:--90.0}                     # Used if BIN_TYPE = 2.
export LAT_MAX=${LAT_MAX:-90.0}                      # Used if BIN_TYPE = 2.
export BINWIDTH_LAT=${BINWIDTH_LAT:-10.0}            # Used if BIN_TYPE = 2.
export BINWIDTH_HGT=${BINWIDTH_HGT:-1000.0}          # Used if BIN_TYPE = 2.
export HGT_MIN=${HGT_MIN:-0.0}                       # Used if BIN_TYPE = 2.
export HGT_MAX=${HGT_MAX:-20000.0}                   # Used if BIN_TYPE = 2.
export REMOVE_MEAN=${REMOVE_MEAN:-.true.}            # Remove time/ensemble/area mean.
export GAUSSIAN_LATS=${GAUSSIAN_LATS:-.false.}       # Set if Gaussian latitudes used (global only). 
export TESTING_EOFS=${TESTING_EOFS:-.true.}          # True if performing EOF tests.
export NUM_PASSES=${NUM_PASSES:-0}                   # Number of passes of recursive filter.
export RF_SCALE=${RF_SCALE:-1.0}                     # Recursive filter scale.
export USE_GLOBAL_EOFS=${USE_GLOBAL_EOFS:-.true.}    # Use domain-averaged EOFS for stage3.
export DATA_ON_LEVELS=${DATA_ON_LEVELS:-.false.}     # False if fields projected onto modes.
export GLOBAL=${GLOBAL:-false}                       # Global or regional models
export NUM_WE=${NUM_WE:-100}                         # Hard-wired for now....
export NUM_SN=${NUM_SN:-100}                         # Hard-wired for now....
export NUM_LEVELS=${NUM_LEVELS:-27}                  # Hard-wired for now....
export N_SMTH_SL=${N_SMTH_SL:-2}                     # Amount of lengthscale smoothing (0=none).
export STRIDE=${STRIDE:-1}                           # Calculate correlation evert STRIDE point (stage4 regional).
export NBINS=${NBINS:-1}                             # Number of latitude bins for length scale computation
export IBIN=${IBIN:-1}                               # Index of latitude bin to compute length scale for
export SL_METHOD=${SL_METHOD:-2}                     # Sl method =1 (Gaussian curve fitting) =2 (Laplacian) 
export TESTING_SPECTRAL=${TESTING_SPECTRAL:-.false.} # True if performing spectral tests.
export LOCAL=${LOCAL:-false}                          # True if local machine.
export NUM_JOBS=${NUM_JOBS:-1}                       # Number of jobs to run (stage4 regional)).
export MACHINES=${MACHINES:-" node1 node1 node2 node2 node3 node3 node4 node4 "\
                            " node5 node5 node6 node6 node7 node7 node8 node8"}
export REGION=${REGION:-afwa}
export EXPT=${EXPT:-noobs}
export ID=${ID:-gen_be}
export ID1=${ID1:-${BE_METHOD}.bin_type${BIN_TYPE}}
export VARIABLE1=${VARIABLE1:-chi_u}              # For cov3d
export VARIABLE2=${VARIABLE2:-chi}                # For cov3d
export CLEAN=false
export FILE_TYPE=${FILE_TYPE:-wrfout}
export RESOLUTION_KM=${RESOLUTION_KM:-3.}
export CV_OPTIONS=${CV_OPTIONS:-5}
export GRAPHIC_WORKS=${GRAPHIC_WORKS:-ps}

export MODEL=${MODEL:-WRF}
export MASSCV=${MASSCV:-temp}
export HUMCV=${HUMCV:-rh}
export CUT=${CUT:-0}
export BALPRES=${BALPRES:-purestats}
export NOBALDIV=${NOBALDIV:-.false.}
export VERTICAL_IP=${VERTICAL_IP:-0}              # Use inner product in EOF: 0=No, 1=Yes
export HORIZVAR=${HORIZVAR:-covar}            # Variable to compute horizontal shape = 'covar' or 'correl'
export HORIZFUNCT=${HORIZFUNCT:-gaus}             # Correlation function = 'gaus' or 'soar'
export FILEPLEV=${FILEPLEV:-"/ptmp/rizvi/SUK_plevel.txt"}
export FILETLEV=${FILETLEV:-"/ptmp/rizvi/SUK_tlevel.txt"}
export UMRUN=${UMRUN:-Q1}
export UMTAG=${UMTAG:-qwainga}
export UM_NPOLE_LAT=${UM_NPOLE_LAT:-37.5}
export UM_NPOLE_LON=${UM_NPOLE_LON:-177.5}
export UM_LAT_SW_CORNER=${UM_LAT_SW_CORNER:--2.63}
export UM_LON_SW_CORNER=${UM_LON_SW_CORNER:-357.8}
export UM_DX_DEG=${UM_DX_DEG:-0.0135}

# Directories:
export REL_DIR=${REL_DIR:-$HOME/trunk}                      # Directory containing codes.
export GEN_BE_DIR=${GEN_BE_DIR:-$REL_DIR/gen_be}            # gen_be code directory.
export BUILD_DIR=${BUILD_DIR:-$GEN_BE_DIR/src}              # gen_be code build directory.
export SCRIPTS_DIR=${SCRIPTS_DIR:-$GEN_BE_DIR/scripts}      # gen_be scripts directory.
export GRAPHICS_DIR=${GRAPHICS_DIR:-$GEN_BE_DIR/graphics}   # gen_be NCL graphics directory.
export DAT_DIR=${DAT_DIR:-${HOME}/data}                     # Top-level data directory.
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}       # Region-specific data dir.
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}         # Experiment-specific data dir.
export FC_DIR=${FC_DIR:-$EXP_DIR/fc}              # Forecast directory
export RUN_DIR=${RUN_DIR:-$EXP_DIR/gen_be$BIN_TYPE} # Run dir.
export WORK_DIR=${WORK_DIR:-$RUN_DIR/working_ncep}     # Working directory
export STAGE0_DIR=${STAGE0_DIR:-$WORK_DIR/stage0} # Output for stage0.
export STAGE0_GSI_DIR=${STAGE0_GSI_DIR:-$EXP_DIR/stage0_gsi} # Output for GSI stage0.
export STAGE1_GSI_DIR=${STAGE1_GSI_DIR:-$EXP_DIR/stage1_gsi} # Output for GSI stage0.
export LESS_LEVELS_FROM_TOP=${LESS_LEVELS_FROM_TOP:-0}
export LAT_BINS_IN_DEG=${LAT_BINS_IN_DEG:-1.0}

if $GLOBAL; then
   export UH_METHOD=power
else
   export UH_METHOD=scale
fi

if [[ $CV_OPTIONS == 6 ]]; then
export CONTROL_VARIABLES=${CONTROL_VARIABLES:-" psi chi_u t_u rh_u ps_u "}
export RUN_GEN_BE_STAGE2A=false
else
export CONTROL_VARIABLES=${CONTROL_VARIABLES:-" psi chi_u t_u rh ps_u "}
fi
export DELETE_DIRS=${DELETE_DIRS:-" "}

export DEBUG=${DEBUG:-0}                          
