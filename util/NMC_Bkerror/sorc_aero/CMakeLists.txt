if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0")
endif()

list(APPEND SRCS
comm_mod.f90
delvars_aerosol.f90
deter_subdomain.f90
getcases_nems.f90
grdsphdp.f90
horizsc_aerosol.f90
init_commvars.f90
initvars.f90
kinds.F90
mat.f90
postmod.f90
readpairs_aerosol.f90
smoothz.f90
specgrid.f90
statsmain_aerosol.f90
variables.f90
variances_aerosol.f90
vertsc_aerosol.f90)

add_executable(calcstats_aero.x ${SRCS})
target_link_libraries(calcstats_aero.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(calcstats_aero.x PRIVATE ${LAPACK_LIBRARIES})
target_link_libraries(calcstats_aero.x PRIVATE bacio::bacio_4)
target_link_libraries(calcstats_aero.x PRIVATE w3emc::w3emc_d)
target_link_libraries(calcstats_aero.x PRIVATE sp::sp_4)
target_link_libraries(calcstats_aero.x PRIVATE nemsio::nemsio)

install(TARGETS calcstats_aero.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
