if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -assume byterecl -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

list(APPEND SRCS
  conmon_read_diag.F90
  data_mod.f90
  generic_list.f90
  grads_sfc.f90
  kinds.F90
  maingrads_sfc.f90
  rm_dups.f90
)

add_executable(conmon_grads_sfc.x ${SRCS})
if(TARGET ncdiag)
  add_dependencies(conmon_grads_sfc.x ncdiag)
endif()

target_compile_definitions(conmon_grads_sfc.x PRIVATE "_REAL8_")
target_link_libraries(conmon_grads_sfc.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(conmon_grads_sfc.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(conmon_grads_sfc.x PRIVATE ncdiag::ncdiag)
target_link_libraries(conmon_grads_sfc.x PRIVATE w3emc::w3emc_4)

# Install executable targets
install(TARGETS conmon_grads_sfc.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

