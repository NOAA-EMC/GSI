if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -assume byterecl -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

list(APPEND SRCS
  conmon_read_diag.F90
  convinfo2.f90
  data_mod.f90
  generic_list.f90
  kinds.F90
  mainconv_time.f90
  process_time_data.f90
  stas2ctl.f90
  stas_time.f90
  stas_time_gps.f90
)

add_executable(conmon_time.x ${SRCS})
if(TARGET ncdiag)
  add_dependencies(conmon_time.x ncdiag)
endif()

target_compile_definitions(conmon_time.x PRIVATE "_REAL8_")
target_link_libraries(conmon_time.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(conmon_time.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(conmon_time.x PRIVATE ncdiag::ncdiag)
target_link_libraries(conmon_time.x PRIVATE w3emc::w3emc_4)

# Install executable targets
install(TARGETS conmon_time.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

