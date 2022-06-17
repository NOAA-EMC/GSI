if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

list(APPEND SRCS
  inc2anl.f90
  init_calc_analysis.f90
  init_io.f90
  main.f90
  vars_calc_analysis.f90)

add_executable(calc_analysis.x ${SRCS})
target_link_libraries(calc_analysis.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(calc_analysis.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(calc_analysis.x PRIVATE bacio::bacio_4)
target_link_libraries(calc_analysis.x PRIVATE nemsio::nemsio)
target_link_libraries(calc_analysis.x PRIVATE ncio::ncio)
target_link_libraries(calc_analysis.x PRIVATE w3emc::w3emc_d)

# Install executable targets
install(TARGETS calc_analysis.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
