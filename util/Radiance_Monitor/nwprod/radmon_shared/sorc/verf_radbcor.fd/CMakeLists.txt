if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -assume byterecl -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

list(APPEND SRCS
  avgsdv.f90
  bcor.f90
  create_ctl_bcor.f90
  kinds.F90
  read_diag.f90
  sparsearr.f90
)

add_executable(radmon_bcor.x ${SRCS})
if(TARGET ncdiag)
  add_dependencies(radmon_bcor.x ncdiag)
endif()

target_compile_definitions(radmon_bcor.x PRIVATE "_REAL8_")
target_link_libraries(radmon_bcor.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(radmon_bcor.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(radmon_bcor.x PRIVATE ncdiag::ncdiag)
target_link_libraries(radmon_bcor.x PRIVATE w3emc::w3emc_4)

# Install executable targets
install(TARGETS radmon_bcor.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

