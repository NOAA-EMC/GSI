macro (setGaea)

  message("Setting flags and paths for Cray")
  option(FIND_HDF5 "Try to Find HDF5 libraries" OFF)
  option(FIND_HDF5_HL "Try to Find HDF5 libraries" ON)
  set(HDF5_USE_STATIC_LIBRARIES "ON" CACHE INTERNAL "HDF5_Static" )

  set(HOST_FLAG "-xCORE-AVX2" CACHE INTERNAL "Host Flag") # for Haswell (C4)
  set(GSI_Fortran_FLAGS "-DPOUND_FOR_STRINGIFY ${FP_MODEL_STRICT} ${BYTERECLEN} ${BIG_ENDIAN} ${IMPLICITNONE} -D_REAL8_ ${TRACEBACK} ${HOST_FLAG} ${MKL_FLAG} ${OpenMP_Fortran_FLAGS} ${MPI_Fortran_COMPILE_FLAGS} ${FOPT3}" CACHE INTERNAL "")
  set(ENKF_Fortran_FLAGS "${FOPT3} ${FP_MODEL_STRICT} ${BIG_ENDIAN} ${BYTERECLEN} ${IMPLICITNONE}  -DGFS -D_REAL8_ ${TRACEBACK} ${HOST_FLAG} ${MKL_FLAG} ${MPI3FLAG} ${OpenMP_Fortran_FLAGS} " CACHE INTERNAL "")
  set(GSI_LDFLAGS "${MKL_FLAG} ${OpenMP_Fortran_FLAGS}" CACHE INTERNAL "")
endmacro()
