if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -assume byterecl -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

list(APPEND SRCS
  create_ctl_horiz.f90
  horiz.f90
  kinds.f90
  oznmon_read_diag.f90
)

add_executable(oznmon_horiz.x ${SRCS})
if(TARGET ncdiag)
  add_dependencies(oznmon_horiz.x ncdiag)
endif()

target_compile_definitions(oznmon_horiz.x PRIVATE "_REAL8_")
target_link_libraries(oznmon_horiz.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(oznmon_horiz.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(oznmon_horiz.x PRIVATE ncdiag::ncdiag)
target_link_libraries(oznmon_horiz.x PRIVATE w3emc::w3emc_4)

# Install executable targets
install(TARGETS oznmon_horiz.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

