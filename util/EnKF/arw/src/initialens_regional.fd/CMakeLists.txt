if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

list(APPEND SRCS
  initial_arw_ens.f90
  read_netcdf_mass.f90
  update_netcdf_mass.f90)

add_executable(initialens.x ${SRCS})

if(TARGET ncdiag)
  add_dependencies(initialens.x ncdiag)
endif()

if(TARGET gsi)
  add_dependencies(initialens.x gsi)
endif()

target_compile_definitions(initialens.x PRIVATE "WRF")

target_link_libraries(initialens.x PRIVATE gsi::gsi)

# Link the application targets with dependencies
target_link_libraries(initialens.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(initialens.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(initialens.x PRIVATE ${LAPACK_LIBRARIES})
target_link_libraries(initialens.x PRIVATE bacio::bacio_4)
target_link_libraries(initialens.x PRIVATE sigio::sigio)
target_link_libraries(initialens.x PRIVATE sfcio::sfcio)
target_link_libraries(initialens.x PRIVATE nemsio::nemsio)
target_link_libraries(initialens.x PRIVATE ncio::ncio)
target_link_libraries(initialens.x PRIVATE w3emc::w3emc_d)
target_link_libraries(initialens.x PRIVATE sp::sp_d)
target_link_libraries(initialens.x PRIVATE bufr::bufr_d)
target_link_libraries(initialens.x PRIVATE crtm::crtm)
target_link_libraries(initialens.x PRIVATE wrf_io::wrf_io)
target_link_libraries(initialens.x PRIVATE ncdiag::ncdiag)
target_link_libraries(initialens.x PRIVATE gsi::gsi)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(initialens.x PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Install executable targets
install(TARGETS initialens.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

