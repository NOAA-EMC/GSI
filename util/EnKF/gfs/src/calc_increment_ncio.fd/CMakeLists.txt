if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

add_executable(calc_increment_ncio.x calc_increment_ncio.f90)
target_link_libraries(calc_increment_ncio.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(calc_increment_ncio.x PRIVATE ncio::ncio)

# Install executable targets
install(TARGETS calc_increment_ncio.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
