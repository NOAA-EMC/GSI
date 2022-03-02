if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

list(APPEND SRCS
  get_gefs_for_regional_enspro.f90
  gfsp2wrfg.f90
  guess_grids_enspro.f90
  read_wrf_mass_guess4ens.F90
  wrf_netcdf_interface4ens.F90)

add_executable(enspreproc.x ${SRCS})

if(TARGET ncdiag)
  add_dependencies(enspreproc.x ncdiag)
endif()

if(TARGET gsi)
  add_dependencies(enspreproc.x gsi)
endif()

target_compile_definitions(enspreproc.x PRIVATE "WRF")

target_link_libraries(enspreproc.x PRIVATE gsi::gsi)

# Link the application targets with dependencies
target_link_libraries(enspreproc.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(enspreproc.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(enspreproc.x PRIVATE ${LAPACK_LIBRARIES})
target_link_libraries(enspreproc.x PRIVATE bacio::bacio_4)
target_link_libraries(enspreproc.x PRIVATE sigio::sigio)
target_link_libraries(enspreproc.x PRIVATE sfcio::sfcio)
target_link_libraries(enspreproc.x PRIVATE nemsio::nemsio)
target_link_libraries(enspreproc.x PRIVATE ncio::ncio)
target_link_libraries(enspreproc.x PRIVATE w3emc::w3emc_d)
target_link_libraries(enspreproc.x PRIVATE sp::sp_d)
target_link_libraries(enspreproc.x PRIVATE bufr::bufr_d)
target_link_libraries(enspreproc.x PRIVATE crtm::crtm)
target_link_libraries(enspreproc.x PRIVATE wrf_io::wrf_io)
target_link_libraries(enspreproc.x PRIVATE ncdiag::ncdiag)
target_link_libraries(enspreproc.x PRIVATE gsi::gsi)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(enspreproc.x PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Install executable targets
install(TARGETS enspreproc.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

