if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

list(APPEND SRCS
  kinds.F90
  nc_subs.f90
  read_nssl_binary.f90
  write_bufr_ref.f90)

# Create library
set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/process_NSSL_mosaic")
add_library(process_NSSL_mosaic OBJECT ${SRCS})
set_target_properties(process_NSSL_mosaic PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
target_include_directories(process_NSSL_mosaic INTERFACE $<BUILD_INTERFACE:${module_dir}>)

target_link_libraries(process_NSSL_mosaic PUBLIC NetCDF::NetCDF_Fortran)
target_link_libraries(process_NSSL_mosaic PUBLIC MPI::MPI_Fortran)
target_link_libraries(process_NSSL_mosaic PUBLIC bufr::bufr_d)

# Create process_NSSL_mosaic.x executable
add_executable(process_NSSL_mosaic.x process_NSSL_mosaic.f90)
add_dependencies(process_NSSL_mosaic.x process_NSSL_mosaic)
target_link_libraries(process_NSSL_mosaic.x PRIVATE process_NSSL_mosaic)

# Install executable targets
install(TARGETS process_NSSL_mosaic.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
