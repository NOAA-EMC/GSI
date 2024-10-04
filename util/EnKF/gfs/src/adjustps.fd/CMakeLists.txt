if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

add_executable(adjustps.x adjustps.f90)
target_link_libraries(adjustps.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(adjustps.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(adjustps.x PRIVATE bacio::bacio_4)
target_link_libraries(adjustps.x PRIVATE nemsio::nemsio)
target_link_libraries(adjustps.x PRIVATE w3emc::w3emc_d)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(adjustps.x PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Install executable targets
install(TARGETS adjustps.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
