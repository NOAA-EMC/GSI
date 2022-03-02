if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

add_executable(getnstensmeanp.x getnstensmeanp.f90 nstio_module.f90)
target_link_libraries(getnstensmeanp.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(getnstensmeanp.x PRIVATE bacio::bacio_4)
target_link_libraries(getnstensmeanp.x PRIVATE w3emc::w3emc_d)
target_link_libraries(getnstensmeanp.x PRIVATE nemsio::nemsio)

# Install executable targets
install(TARGETS getnstensmeanp.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

