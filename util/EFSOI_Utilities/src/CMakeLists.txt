if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

list(APPEND SRCS
  efsoi.f90
  efsoi_main.f90
  gridio_efsoi.f90
  loadbal_efsoi.f90
  loc_advection.f90
  scatter_chunks_efsoi.f90
  statevec_efsoi.f90)

add_executable(efsoi.x ${SRCS})
target_compile_definitions(efsoi.x PRIVATE "_REAL8_")
if(TARGET enkf)
  add_dependencies(efsoi.x enkf)
endif()
target_link_libraries(efsoi.x PRIVATE enkf::enkf)
target_link_libraries(efsoi.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(efsoi.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(efsoi.x PRIVATE ${LAPACK_LIBRARIES})
target_link_libraries(efsoi.x PRIVATE bacio::bacio_4)
target_link_libraries(efsoi.x PRIVATE sigio::sigio)
target_link_libraries(efsoi.x PRIVATE sfcio::sfcio)
target_link_libraries(efsoi.x PRIVATE nemsio::nemsio)
target_link_libraries(efsoi.x PRIVATE ncio::ncio)
target_link_libraries(efsoi.x PRIVATE w3emc::w3emc_d)
target_link_libraries(efsoi.x PRIVATE sp::sp_d)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(efsoi.x PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Install executable targets
install(TARGETS efsoi.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
