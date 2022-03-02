if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

if(NOT TARGET enkf)
  find_package(enkf QUIET)
  if(NOT enkf_FOUND)
    message(STATUS "EFSOI: EnKF library was not found. Disable EFSOI utilities")
    return()
  endif()
endif()

if(NOT ENKF_APP MATCHES "GFS")
  message(STATUS "EFSOI: Incompatible EnKF = ${ENKF_APP}. Disable EFSOI utilities")
  return()
endif()

if(NOT TARGET gsi)
  find_package(gsi REQUIRED)
endif()

if(NOT GFS_APP MATCHES "GFS")
  message(STATUS "EFSOI: Incompatible GFS = ${GFS_APP}. Disable EFSOI utilities")
  return()
endif()

if(NOT TARGET ncdiag)
  find_package(ncdiag REQUIRED)
endif()

list(APPEND SRCS
  efsoi.f90
  gridio_efsoi.f90
  loadbal_efsoi.f90
  loc_advection.f90
  scatter_chunks_efsoi.f90
  statevec_efsoi.f90)

set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/efsoi")
add_library(efsoi_objs OBJECT ${SRCS})
set_target_properties(efsoi_objs PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
target_include_directories(efsoi_objs INTERFACE $<BUILD_INTERFACE:${module_dir}>)

if(TARGET ncdiag)
  add_dependencies(efsoi_objs ncdiag)
endif()
if(TARGET gsi)
  add_dependencies(efsoi_objs gsi)
endif()
if(TARGET enkf)
  add_dependencies(efsoi_objs enkf)
endif()

target_compile_definitions(efsoi_objs PRIVATE "_REAL8_")

target_link_libraries(efsoi_objs PUBLIC enkf::enkf)
target_link_libraries(efsoi_objs PUBLIC gsi::gsi)
target_link_libraries(efsoi_objs PUBLIC ncdiag::ncdiag)
target_link_libraries(efsoi_objs PUBLIC NetCDF::NetCDF_Fortran)
target_link_libraries(efsoi_objs PUBLIC MPI::MPI_Fortran)
target_link_libraries(efsoi_objs PUBLIC ${LAPACK_LIBRARIES})
target_link_libraries(efsoi_objs PUBLIC bacio::bacio_4)
target_link_libraries(efsoi_objs PUBLIC sigio::sigio)
target_link_libraries(efsoi_objs PUBLIC sfcio::sfcio)
target_link_libraries(efsoi_objs PUBLIC nemsio::nemsio)
target_link_libraries(efsoi_objs PUBLIC ncio::ncio)
target_link_libraries(efsoi_objs PUBLIC w3emc::w3emc_d)
target_link_libraries(efsoi_objs PUBLIC sp::sp_d)
target_link_libraries(efsoi_objs PUBLIC bufr::bufr_d)
target_link_libraries(efsoi_objs PUBLIC crtm::crtm)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(efsoi_objs PRIVATE OpenMP::OpenMP_Fortran)
endif()

add_library(efsoi STATIC $<TARGET_OBJECTS:efsoi_objs>)
target_include_directories(efsoi PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include/efsoi>
                                        $<INSTALL_INTERFACE:include/efsoi>)

set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/efsoi")
add_executable(efsoi.x efsoi_main.f90)
add_dependencies(efsoi.x efsoi)
set_target_properties(efsoi.x PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
target_link_libraries(efsoi.x PRIVATE efsoi)

target_compile_definitions(efsoi.x PRIVATE "_REAL8_")

# Link the application targets with dependencies
target_link_libraries(efsoi.x PRIVATE enkf::enkf)
target_link_libraries(efsoi.x PRIVATE gsi::gsi)
target_link_libraries(efsoi.x PRIVATE ncdiag::ncdiag)
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
target_link_libraries(efsoi.x PRIVATE bufr::bufr_d)
target_link_libraries(efsoi.x PRIVATE crtm::crtm)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(efsoi.x PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Install executable targets
install(TARGETS efsoi.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
