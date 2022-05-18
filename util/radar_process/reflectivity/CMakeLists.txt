if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

list(APPEND SRCS
  kinds.F90
  nc_subs.f90
  process_NSSL_mosaic.f90
  read_nssl_binary.f90
  write_bufr_ref.f90)

# Create process_NSSL_mosaic.x executable
add_executable(process_NSSL_mosaic.x ${SRCS})
target_link_libraries(process_NSSL_mosaic.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(process_NSSL_mosaic.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(process_NSSL_mosaic.x PRIVATE bufr::bufr_d)

# Install executable targets
install(TARGETS process_NSSL_mosaic.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
