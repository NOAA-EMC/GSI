if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

list(APPEND SRCS
  constants.f90
  getsfcnstensupdp.f90
  grdcrd.f90
  intrp_msk.f90
  kinds.F90
  nstio_module.f90)

add_executable(getsfcnstensupdp.x ${SRCS})
target_compile_definitions(getsfcnstensupdp.x PRIVATE "_REAL8_")
target_link_libraries(getsfcnstensupdp.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(getsfcnstensupdp.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(getsfcnstensupdp.x PRIVATE bacio::bacio_4)
target_link_libraries(getsfcnstensupdp.x PRIVATE w3emc::w3emc_d)
target_link_libraries(getsfcnstensupdp.x PRIVATE nemsio::nemsio)
target_link_libraries(getsfcnstensupdp.x PRIVATE sfcio::sfcio)
target_link_libraries(getsfcnstensupdp.x PRIVATE sp::sp_4)

# Install executable targets
install(TARGETS getsfcnstensupdp.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

