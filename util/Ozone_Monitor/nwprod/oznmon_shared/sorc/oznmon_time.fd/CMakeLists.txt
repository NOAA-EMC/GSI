if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -assume byterecl -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

list(APPEND SRCS
  avgsdv.f90
  create_ctl_time.f90
  kinds.f90
  oznmon_read_diag.f90
  time.f90
  valid.f90
)

add_executable(oznmon_time.x ${SRCS})
if(TARGET ncdiag)
  add_dependencies(oznmon_time.x ncdiag)
endif()

target_compile_definitions(oznmon_time.x PRIVATE "_REAL8_")
target_link_libraries(oznmon_time.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(oznmon_time.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(oznmon_time.x PRIVATE ncdiag::ncdiag)
target_link_libraries(oznmon_time.x PRIVATE w3emc::w3emc_4)

# Install executable targets
install(TARGETS oznmon_time.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

