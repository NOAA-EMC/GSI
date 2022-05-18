if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian -assume byterecl")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0")
endif()

add_executable(recenterens_ncio.x recenterens_ncio.f90)
target_link_libraries(recenterens_ncio.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(recenterens_ncio.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(recenterens_ncio.x PRIVATE bacio::bacio_4)
target_link_libraries(recenterens_ncio.x PRIVATE w3emc::w3emc_d)
target_link_libraries(recenterens_ncio.x PRIVATE ncio::ncio)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(recenterens_ncio.x PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Install executable targets
install(TARGETS recenterens_ncio.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

