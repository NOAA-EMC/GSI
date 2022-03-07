if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0")
endif()

list(APPEND SRCS
balprojs.F90
biascor.f90
comm_mod.f90
delvars.f90
deter_subdomain.f90
genqsat.f90
getcases.f90
grdsphdp.f90
horizsc.f90
init_commvars.f90
initvars.f90
isrchfge.f90
kinds.f90
mat.f90
postmod.f90
readpairs.f90
smooth2d.f90
smoothz.f90
specgrid.f90
sstmod.f90
statsmain.f90
variables.f90
variances.f90
variances3d.f90
vertsc.f90)

add_executable(calcstats.x ${SRCS})
target_compile_definitions(calcstats.x PRIVATE "_LAPACK_")
target_link_libraries(calcstats.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(calcstats.x PRIVATE ${LAPACK_LIBRARIES})
target_link_libraries(calcstats.x PRIVATE bacio::bacio_4)
target_link_libraries(calcstats.x PRIVATE w3emc::w3emc_d)
target_link_libraries(calcstats.x PRIVATE sp::sp_4)
target_link_libraries(calcstats.x PRIVATE sigio::sigio)
target_link_libraries(calcstats.x PRIVATE nemsio::nemsio)

install(TARGETS calcstats.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
