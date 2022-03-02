if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

if(NOT TARGET enkf)
  find_package(enkf QUIET)
endif()

if(NOT enkf_FOUND)
  message(STATUS "EnKF library was not found. Disable EFSOI utilities")
  return()
endif()

if(NOT ENKF_APP MATCHES "GFS")
  message(STATUS "Incompatible EnKF = ${ENKF_APP}. Disable EFSOI utilities")
  return()
endif()

if(NOT TARGET gsi)
  find_package(gsi REQUIRED)
endif()

if(NOT GFS_APP MATCHES "GFS")
  message(STATUS "Incompatible GFS = ${GFS_APP}. Disable EFSOI utilities")
  return()
endif()

if(NOT TARGET ncdiag)
  find_package(ncdiag REQUIRED)
endif()

list(APPEND SRCS
  efsoi.f90
  efsoi_main.f90
  gridio_efsoi.f90
  loadbal_efsoi.f90
  loc_advection.f90
  scatter_chunks_efsoi.f90
  statevec_efsoi.f90)

add_executable(global_efsoi.x ${SRCS})

if(TARGET ncdiag)
  add_dependencies(global_efsoi.x ncdiag)
endif()

if(TARGET gsi)
  add_dependencies(global_efsoi.x gsi)
endif()

if(TARGET gsi)
  add_dependencies(global_efsoi.x enkf)
endif()

target_compile_definitions(global_efsoi.x PRIVATE "_REAL8_")

# Link the application targets with dependencies
target_link_libraries(global_efsoi.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(global_efsoi.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(global_efsoi.x PRIVATE ${LAPACK_LIBRARIES})
target_link_libraries(global_efsoi.x PRIVATE bacio::bacio_4)
target_link_libraries(global_efsoi.x PRIVATE sigio::sigio)
target_link_libraries(global_efsoi.x PRIVATE sfcio::sfcio)
target_link_libraries(global_efsoi.x PRIVATE nemsio::nemsio)
target_link_libraries(global_efsoi.x PRIVATE ncio::ncio)
target_link_libraries(global_efsoi.x PRIVATE w3emc::w3emc_d)
target_link_libraries(global_efsoi.x PRIVATE sp::sp_d)
target_link_libraries(global_efsoi.x PRIVATE bufr::bufr_d)
target_link_libraries(global_efsoi.x PRIVATE crtm::crtm)
target_link_libraries(global_efsoi.x PRIVATE ncdiag::ncdiag)
target_link_libraries(global_efsoi.x PRIVATE gsi::gsi)
target_link_libraries(global_efsoi.x PRIVATE enkf::enkf)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(global_efsoi.x PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Install executable targets
install(TARGETS global_efsoi.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
