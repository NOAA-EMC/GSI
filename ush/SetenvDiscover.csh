#!/bin/csh

set TrunkDir = `echo $cwd`

setenv CRTM_SRC ${TrunkDir}/libsrc/crtm
setenv BACIO_SRC ${TrunkDir}/libsrc/bacio
setenv BUFR_SRC ${TrunkDir}/libsrc/bufr
setenv NEMSIO_SRC ${TrunkDir}/libsrc/nemsio
setenv SIGIO_SRC ${TrunkDir}/libsrc/sigio
setenv SFCIO_SRC ${TrunkDir}/libsrc/sfcio
setenv SP_SRC ${TrunkDir}/libsrc/sp
setenv W3NCO_SRC ${TrunkDir}/libsrc/w3nco
setenv W3EMC_SRC ${TrunkDir}/libsrc/w3emc

#source $MODULESHOME/init/bash
module load other/cmake
