TrunkDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )

export CRTM_SRC=${TrunkDir}/libsrc/crtm
export BACIO_SRC=${TrunkDir}/libsrc/bacio
export BUFR_SRC=${TrunkDir}/libsrc/bufr
export NEMSIO_SRC=${TrunkDir}/libsrc/nemsio
export SIGIO_SRC=${TrunkDir}/libsrc/sigio
export SFCIO_SRC=${TrunkDir}/libsrc/sfcio
export SP_SRC=${TrunkDir}/libsrc/sp
export W3NCO_SRC=${TrunkDir}/libsrc/w3nco
export W3EMC_SRC=${TrunkDir}/libsrc/w3emc

source $MODULESHOME/init/bash
module load other/cmake
