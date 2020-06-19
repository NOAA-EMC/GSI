macro (setWCOSS_D)
  message("Setting paths for Dell")
  option(FIND_HDF5 "Try to Find HDF5 libraries" OFF)
  option(FIND_HDF5_HL "Try to Find HDF5 libraries" OFF)

  set(HOST_FLAG "${XHOST}" CACHE INTERNAL "Host Flag")
  set(GSI_Fortran_FLAGS "-DPOUND_FOR_STRINGIFY ${FP_MODEL_STRICT} ${BYTERECLEN} ${BIG_ENDIAN} ${IMPLICITNONE} -D_REAL8_ ${OpenMP_Fortran_FLAGS} ${MPI_Fortran_COMPILE_FLAGS} ${FOPT3}" CACHE INTERNAL "GSI Fortran Flags")
  set(GSI_LDFLAGS "/usr/lib64/libm.so;/usr/lib64/libz.so;${OpenMP_Fortran_FLAGS};${MKL_FLAG}" CACHE INTERNAL "")
  set(ENKF_Fortran_FLAGS "${FOPT3} ${FP_MODEL_STRICT} ${BIG_ENDIAN} ${BYTERECLEN} ${IMPLICITNONE}  -DGFS -D_REAL8_ ${MPI3FLAG} ${OpenMP_Fortran_FLAGS} " CACHE INTERNAL "ENKF Fortran Flags")

  set(HDF5_USE_STATIC_LIBRARIES "ON" CACHE INTERNAL "" )
endmacro()
