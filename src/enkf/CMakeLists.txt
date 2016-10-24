cmake_minimum_required(VERSION 2.6)
message("in enkf")
if(BUILD_ENKF)
  enable_language (Fortran)
  set(Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
  set(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
  set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

  if(BUILD_GFS)
    set(ENKF_SUFFIX "gfs")
  elseif(BUILD_NMMB)
    set(ENKF_SUFFIX "nmmb")
  else()
    set(ENKF_SUFFIX "wrf")
  endif()
  if(USE_WRF)
    set(GSILIB gsilib_wrf${debug_suffix})
  else()
    set(GSILIB  gsilib_global${debug_suffix})
  endif()
  set(ENKF_EXTRA_SRCS gridio_${ENKF_SUFFIX}.f90 )
  add_library( MODS1 OBJECT gridinfo_${ENKF_SUFFIX}.f90 )  
  add_library( MODS2 OBJECT ${ENKF_EXTRA_SRCS} )  

  set(ENKFMOD_SRCS 
  netcdf_io_wrf.f90 params.f90 covlocal.f90 fftpack.f90 genqsat1.f90 mpisetup.F90 rnorm.f90
  sorting.f90 specmod.f90 reducedgrid.f90 readozobs.f90 readsatobs.f90 readconvobs.f90
  write_logfile.f90 kdtree2.f90 mpi_readobs.f90 enkf_obsmod.f90 
  )
  set(ENKF_SRCS 
  innovstats.f90
  enkf.f90
  read_locinfo.f90
  enkf_main.f90
  inflation.f90
  letkf.F90
  quicksort.f90
  radbias.f90
  statevec.f90
  loadbal.f90
  smooth_${ENKF_SUFFIX}.f90 
  gridio_${ENKF_SUFFIX}.f90 
  gridinfo_${ENKF_SUFFIX}.f90 
  )
  if( NOT crayComp )
    find_package( LAPACK REQUIRED )
  endif()
  
  add_definitions(${MPI_Fortran_FLAGS})
  
  include_directories(${CMAKE_CURRENT_BINARY_DIR} "${PROJECT_BINARY_DIR}/include/wrf" "${PROJECT_BINARY_DIR}/include/global"  ${CMAKE_CURRENT_BINARY_DIR}/..
  ${MPI_Fortran_INCLUDE_DIRS} ${MPI_INCLUDE_PATH} ${CORE_INCS}
  ${NETCDF_INCLUDES} )
  link_directories(${MPI_Fortran_LIBRARIES})
 
  set_source_files_properties( ${ENKF_SRCS} PROPERTIES COMPILE_FLAGS ${ENKF_Fortran_FLAGS} )
  set_source_files_properties( ${ENKFMOD_SRCS} PROPERTIES COMPILE_FLAGS ${ENKF_Fortran_FLAGS} )
  
  add_library(enkfdeplib STATIC ${ENKFMOD_SRCS} )
  add_library(enkflib STATIC ${ENKF_SRCS} $<TARGET_OBJECTS:MODS1> $<TARGET_OBJECTS:MODS2> )
  add_dependencies(enkfdeplib ${GSILIB} )
  add_dependencies(MODS1 enkfdeplib gsilib_shrd${debug_suffix} ${GSILIB} )
  add_dependencies(MODS2 MODS1 enkfdeplib gsilib_shrd${debug_suffix} ${GSILIB} )
  add_dependencies(enkflib enkfdeplib gsilib_shrd${debug_suffix} ${GSILIB} )
  add_executable(enkf_${ENKF_SUFFIX}.x${debug_suffix} enkf_main.f90)
  add_dependencies(enkf_${ENKF_SUFFIX}.x${debug_suffix} enkflib enkfdeplib ${GSI_LIB} )
  target_link_libraries(enkf_${ENKF_SUFFIX}.x${debug_suffix} enkflib enkfdeplib gsilib_shrd${debug_suffix} ${GSILIB} ${CORE_LIBRARIES}
  ${MPI_Fortran_LIBRARIES} ${LAPACK_LIBRARIES} ${NETCDF_LIBRARIES_F90} ${NETCDF_LIBRARIES}  ${HDF5_Fortran_HL_LIBRARIES} ${EXTRA_LINKER_FLAGS} ${HDF5_LIBRARIES} ${CURL_LIBRARIES} )
endif()
