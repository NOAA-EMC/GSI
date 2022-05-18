if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian -assume byterecl")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0")
endif()

add_executable(recenternemsiop_hybgain.x recenternemsiop_hybgain.f90)
target_link_libraries(recenternemsiop_hybgain.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(recenternemsiop_hybgain.x PRIVATE bacio::bacio_4)
target_link_libraries(recenternemsiop_hybgain.x PRIVATE w3emc::w3emc_d)
target_link_libraries(recenternemsiop_hybgain.x PRIVATE nemsio::nemsio)

# Install executable targets
install(TARGETS recenternemsiop_hybgain.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

