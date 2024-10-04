if(NOT TARGET ncdiag)
  if(NOT ncdiag_FOUND)
    message(STATUS "Utils: ncdiag library was not found. Disable Correlated Obs utility")
    return()
  endif()
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

list(APPEND SRCS
  cconstants.f90
  ckinds.f90
  cov_calc.f90
  matrix_tools.f90
  obs_tools.f90
  pairs.f90
  readsatobs.f90
  read_diag.f90)

# Create cov_calc.x executable
add_executable(cov_calc.x ${SRCS})
target_link_libraries(cov_calc.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(cov_calc.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(cov_calc.x PRIVATE ${LAPACK_LIBRARIES})
if(TARGET ncdiag)
  add_dependencies(cov_calc.x ncdiag)
endif()
target_link_libraries(cov_calc.x PRIVATE ncdiag::ncdiag)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(cov_calc.x PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Install executable targets
install(TARGETS cov_calc.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
