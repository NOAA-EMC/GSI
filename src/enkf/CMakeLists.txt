cmake_minimum_required(VERSION 2.6)
if(BUILD_ENKF)
  enable_language (Fortran)
  set(Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
  set(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
  set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

  if(BUILD_PRODUCTION)
    set(ENKF_SUFFIX "gfs")
    set(ENKF_BASE "global_enkf.x")
  elseif(BUILD_WRF)
    set(ENKF_SUFFIX "wrf")
    set(ENKF_BASE "enkf_wrf")
  elseif(BUILD_GFS)
    set(ENKF_SUFFIX "gfs")
    set(ENKF_BASE "enkf_gfs")
  elseif(BUILD_NMMB)
    set(ENKF_SUFFIX "nmmb")
    set(ENKF_BASE "enkf_nmmb")
  elseif(BUILD_FV3reg)
    set(ENKF_SUFFIX "fv3reg")
    set(ENKF_BASE "enkf_fv3reg")
    set(ENKF_extra_src1 read_fv3reg_restarts.f90 write_fv3reg_restarts.f90)
  else()
    set(ENKF_SUFFIX "wrf")
    set(ENKF_BASE "enkf_wrf")
  endif()
# if(USE_WRF)
#   set(GSILIB "gsilib_${debug_suffix}")
# else()
#   set(GSILIB  "gsilib_global${debug_suffix}")
# endif()
  set(ENKF_EXTRA_SRCS ${ENKF_extra_src1} gridio_${ENKF_SUFFIX}.f90 )
  add_library( MODS1 OBJECT gridinfo_${ENKF_SUFFIX}.f90 )  
  add_library( MODS2 OBJECT ${ENKF_EXTRA_SRCS} )  

  set(ENKFMOD_SRCS 
  netcdf_io_wrf.f90  params.f90 covlocal.f90 fftpack.f90 genqsat1.f90 mpisetup.f90 rnorm.f90
  sorting.f90 specmod.f90 reducedgrid.f90 readozobs.f90 readsatobs.f90 readconvobs.f90
  write_logfile.f90 kdtree2.f90 mpi_readobs.f90 enkf_obsmod.f90 statevec.f90 controlvec.f90
  observer_${ENKF_SUFFIX}.f90 gridio_${ENKF_SUFFIX}.f90 gridinfo_${ENKF_SUFFIX}.f90 expand_ens.f90
  ${ENKF_extra_src1} 
  )
  set(ENKF_SRCS 
  innovstats.f90
  enkf.f90
  read_locinfo.f90
  enkf_main.f90
  inflation.f90
  letkf.f90
  quicksort.f90
  radbias.f90
  loadbal.f90
  smooth_${ENKF_SUFFIX}.f90 
  gridio_${ENKF_SUFFIX}.f90 
  gridinfo_${ENKF_SUFFIX}.f90
  enkf_obs_sensitivity.f90 
  )
# if( (NOT HOST-WCOSS_C) AND (NOT HOST-WCOSS_D) )
#   find_package( LAPACK REQUIRED )
# endif()
  
  add_definitions(${MPI_Fortran_FLAGS})
  
  include_directories(${CMAKE_CURRENT_BINARY_DIR} "${PROJECT_BINARY_DIR}/include/wrf" "${PROJECT_BINARY_DIR}/include/global"  ${CMAKE_CURRENT_BINARY_DIR}/.. ${MPI_Fortran_INCLUDE_DIRS} ${MPI_Fortran_INCLUDE_PATH} ${CORE_INCS} ${NETCDF_INCLUDES} ${NCDIAG_INCS} ${FV3GFS_NCIO_INCS})
  link_directories(${MPI_Fortran_LIBRARIES})

  set_source_files_properties( ${ENKF_SRCS} PROPERTIES COMPILE_FLAGS ${ENKF_Fortran_FLAGS} )
  set_source_files_properties( ${ENKFMOD_SRCS} PROPERTIES COMPILE_FLAGS ${ENKF_Fortran_FLAGS} )
  set_source_files_properties( ${ENKF_EXTRA_SRCS} PROPERTIES COMPILE_FLAGS ${ENKF_Fortran_FLAGS} )
  
  add_library(enkfdeplib STATIC ${ENKFMOD_SRCS} )
  add_library(enkflib STATIC ${ENKF_SRCS} $<TARGET_OBJECTS:MODS1> $<TARGET_OBJECTS:MODS2> )
  add_dependencies(enkfdeplib ${GSILIB} ${GSISHAREDLIB} )
  add_dependencies(MODS1 enkfdeplib ${GSILIB} ${GSISHAREDLIB} )
  add_dependencies(MODS2 MODS1 enkfdeplib ${GSILIB} ${GSISHAREDLIB} )
  add_dependencies(enkflib enkfdeplib ${GSILIB} ${GSISHAREDLIB} )
  set(ENKFEXEC "${ENKF_BASE}${debug_suffix}${GSISUFFIX}" CACHE INTERNAL "ENKF Executable name")
  add_executable(${ENKFEXEC} enkf_main.f90)
  target_link_libraries(${ENKFEXEC} enkflib enkfdeplib ${GSILIB} ${GSISHAREDLIB} ${CORE_LIBRARIES}
  ${MPI_Fortran_LIBRARIES} ${LAPACK_LIBRARIES} ${NETCDF_LIBRARIES_F90} ${NETCDF_LIBRARIES}  ${HDF5_HL_LIBRARIES}
  ${EXTRA_LINKER_FLAGS} ${HDF5_LIBRARIES} ${CURL_LIBRARIES} ${GSI_LDFLAGS} ${CORE_BUILT} ${CORE_LIBRARIES} ${CORE_BUILT} ${NCDIAG_LIBRARIES} ${FV3GFS_NCIO_LIBRARIES})
  install(TARGETS ${ENKFEXEC} enkfdeplib enkflib 
        RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}/bin
        LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX}/lib
        ARCHIVE DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/static)
endif()
