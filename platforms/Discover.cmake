macro (setDiscover)
  message("Setting paths for Discover")
# option(FIND_HDF5 "Try to Find HDF5 libraries" OFF)
# option(FIND_HDF5_HL "Try to Find HDF5 libraries" OFF)
  set(HDF5_USE_STATIC_LIBRARIES "OFF")

  set(GSI_LDFLAGS "${OpenMP_Fortran_FLAGS}" CACHE INTERNAL "")
  set(HOST_FLAG "${XHOST}" CACHE INTERNAL "Host Flag")
  set(GSI_Fortran_FLAGS "-DPOUND_FOR_STRINGIFY ${FOPT3} ${FP_MODEL_SOURCE} ${BYTERECLEN} ${BIG_ENDIAN} ${DEBINFO} ${TRACEBACK} -D_REAL8_ ${OpenMP_Fortran_FLAGS} ${MPI_Fortran_COMPILE_FLAGS}" CACHE INTERNAL "GSI Fortran Flags")
  set(ENKF_Fortran_FLAGS "${FOPT3} ${HOST_FLAG} ${WARNALL} ${IMPLICITNONE} ${TRACEBACK} ${FP_MODEL_STRICT} ${BIG_ENDIAN} -DGFS -D_REAL8_ ${MPI3FLAG} ${OpenMP_Fortran_FLAGS}" CACHE INTERNAL "ENKF Fortran Flags")
  set(host "Discover" CACHE INTERNAL "")
  
  if( ENV{BASEDIR} )
    set(BASEDIR $ENV{BASEDIR}/Linux CACHE INTERNAL "")
  endif()
  set(USE_WRF "OFF" CACHE INTERNAL "")
  set(BUILD_GLOBAL "ON" CACHE INTERNAL "")

  set(ENV{MPI_HOME} $ENV{MPI_ROOT} )

endmacro()

