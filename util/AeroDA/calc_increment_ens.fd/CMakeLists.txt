if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

list(APPEND SRCS
  calc_increment_interface.f90
  configure
  constants.f90
  fv3_interface.f90
  gfs_nems_interface.f90
  kinds.f90
  namelist_def.f90
  pmain.f90
  variable_interface.f90)

add_executable(calc_increment_ens_aero.x ${SRCS})
target_compile_definitions(calc_increment_ens_aero.x PRIVATE "_REAL8_")
target_link_libraries(calc_increment_ens_aero.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(calc_increment_ens_aero.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(calc_increment_ens_aero.x PRIVATE bacio::bacio_4)
target_link_libraries(calc_increment_ens_aero.x PRIVATE w3emc::w3emc_d)
target_link_libraries(calc_increment_ens_aero.x PRIVATE nemsio::nemsio)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(calc_increment_ens_aero.x PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Install executable targets
install(TARGETS calc_increment_ens_aero.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

