if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian -assume byterecl")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0")
endif()

add_executable(getsigensmeanp_smooth.x getsigensmeanp_smooth_ncep.f90)
target_link_libraries(getsigensmeanp_smooth.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(getsigensmeanp_smooth.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(getsigensmeanp_smooth.x PRIVATE bacio::bacio_4)
target_link_libraries(getsigensmeanp_smooth.x PRIVATE w3emc::w3emc_d)
target_link_libraries(getsigensmeanp_smooth.x PRIVATE nemsio::nemsio)
target_link_libraries(getsigensmeanp_smooth.x PRIVATE sp::sp_4)
target_link_libraries(getsigensmeanp_smooth.x PRIVATE sigio::sigio)
target_link_libraries(getsigensmeanp_smooth.x PRIVATE ncio::ncio)

# Install executable targets
install(TARGETS getsigensmeanp_smooth.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

