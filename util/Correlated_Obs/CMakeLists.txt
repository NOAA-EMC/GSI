if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

if(NOT TARGET ncdiag)
  find_package(ncdiag REQUIRED)
endif()

list(APPEND SRCS
  cconstants.f90
  ckinds.f90
  matrix_tools.f90
  obs_tools.f90
  pairs.f90
  readsatobs.f90
  read_diag.f90)

# Create cov_calc library
set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/cov_calc")
add_library(cov_calc OBJECT ${SRCS})
set_target_properties(cov_calc PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
target_include_directories(cov_calc INTERFACE $<BUILD_INTERFACE:${module_dir}>)

target_link_libraries(cov_calc PUBLIC NetCDF::NetCDF_Fortran)
target_link_libraries(cov_calc PUBLIC MPI::MPI_Fortran)
target_link_libraries(cov_calc PUBLIC ${LAPACK_LIBRARIES})
if(TARGET ncdiag)
  add_dependencies(cov_calc ncdiag)
  add_library(ncdiag::ncdiag ALIAS ncdiag)
endif()
target_link_libraries(cov_calc PUBLIC ncdiag::ncdiag)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(cov_calc PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Create cov_calc.x executable
add_executable(cov_calc.x cov_calc.f90)
add_dependencies(cov_calc.x cov_calc)
target_link_libraries(cov_calc.x PRIVATE cov_calc)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(cov_calc.x PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Install executable targets
install(TARGETS cov_calc.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
