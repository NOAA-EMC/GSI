cmake_minimum_required(VERSION 2.8)
# need to set CMP0046 when using add_dependencies with cmake version 3.6.2
  if(crayComp)
    cmake_policy(SET CMP0046 NEW)
  endif()
  set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
# get a list of all the fortran source files
  file(GLOB GSI_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*90 )
# get a list of all the c source files
  file(GLOB GSI_C_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*c )
# get a list of all cplr files
  file(GLOB CLASS_SRC ${CMAKE_CURRENT_SOURCE_DIR}/class*90 )
# create a list of all corresponding stub files
  string(REGEX REPLACE "class" "stub" STUB_SRC "${CLASS_SRC}") 
# create a list of all corresponding class files
  string(REGEX REPLACE "class" "cplr" CPLR_SRC "${CLASS_SRC}") 

# these files use the cplr or stub modules, so they need to be compiled for all versions of GSI (WRF, no-WRF, etc)
  set(LINKED_SRC "${CMAKE_CURRENT_SOURCE_DIR}/gesinfo.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/get_gefs_ensperts_dualres.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/get_gefs_for_regional.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/read_guess.F90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/bkgcov.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/bicglanczos.F90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/bicg.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/set_crtm_cloudmod.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/crtm_interface.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/gsi_nemsio_mod.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/ensctl2model_ad.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/ensctl2model.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/ensctl2state.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/ensctl2state_ad.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/lanczos.F90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/getsiga.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/test_obsens.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/sqrtmin.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/evaljgrad.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/pcgsoi.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/en_perts_io.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/bkerror.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/hybrid_ensemble_isotropic.F90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/write_all.F90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/glbsoi.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/setupaod.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/setuprad.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/general_read_nmmb.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/get_nmmb_ensperts.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/gsimod.F90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/gsimain.f90")

# remove all stub, cplr and linked files from the rest of the source files
  list( REMOVE_ITEM GSI_SRC ${LINKED_SRC} )
  list( REMOVE_ITEM GSI_SRC ${STUB_SRC} )
  list( REMOVE_ITEM GSI_SRC ${CPLR_SRC} )

# remove stub_nstmod from the rest of the source files
  list( REMOVE_ITEM GSI_SRC "${CMAKE_CURRENT_SOURCE_DIR}/stub_nstmod.f90" )
  if( BUILD_NCDIAG )
    list( REMOVE_ITEM GSI_SRC "${CMAKE_CURRENT_SOURCE_DIR}/nc_diag_read_mod.f90" )
    list( REMOVE_ITEM GSI_SRC "${CMAKE_CURRENT_SOURCE_DIR}/nc_diag_write_mod.f90" )
  endif( BUILD_NCDIAG )
# The specific regional/WRF source files
  if(USE_WRF)
    set( REGIONAL_SRC ${CPLR_SRC} )
    list( APPEND REGIONAL_SRC ${LINKED_SRC} )
    set_source_files_properties( ${REGIONAL_SRC} PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS} )
  endif()

# The non-WRF specific src files
  set( GLOBAL_SRC ${CPLR_SRC} )
  set( GLOBAL_SRC ${STUB_SRC} )
  list( APPEND GLOBAL_SRC ${LINKED_SRC} )
  
  list( APPEND GSI_SRC  ${GSI_C_SRC} )

# set up the compiler flags
  set_source_files_properties( ${GSI_SRC} PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS} )
  set_source_files_properties( ${GLOBAL_SRC} PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS} )
  set_source_files_properties( ${GSI_C_SRC} PROPERTIES COMPILE_FLAGS ${GSI_CFLAGS} )
# set_source_files_properties( ${CMAKE_CURRENT_SOURCE_DIR}/blockIO.c  PROPERTIES COMPILE_FLAGS ${GSI_CFLAGS} )

# Add the include paths
  message("MPI include PATH  ${MPI_Fortran_INCLUDE_PATH}")
  include_directories( ${CORE_INCS} ${NETCDF_INCLUDES} ${MPI_Fortran_INCLUDE_PATH} "./"  )

#define the name used for GSI Shared lib and add it with properties and include dirs
  set(GSISHAREDLIB "gsilib_shrd${debug_suffix}" CACHE INTERNAL "")
  add_library(${GSISHAREDLIB} STATIC ${GSI_SRC} )
  set_target_properties(${GSISHAREDLIB} PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
  target_include_directories(${GSISHAREDLIB} PUBLIC "${PROJECT_BINARY_DIR}/include")
  if( BUILD_NCDIAG )
      add_dependencies(${GSISHAREDLIB} ${NCDIAG_LIBRARIES} )
  endif( BUILD_NCDIAG )

# Definte the base name of the executable 
  if(BUILD_PRODUCTION)
    set(GSIBASE "global_gsi.x")
  else()
    if(BUILD_GLOBAL)
      set(GSIBASE "gsi_global")
    else()
      set(GSIBASE "gsi")
    endif()
    set(GSISUFFIX ".x" CACHE INTERNAL "Executable suffix")
  endif()

# Set names, libs, and properties depending on if we are building with WRF or not
  if(USE_WRF) 
    set(GSIEXEC "${GSIBASE}${debug_suffix}${GSISUFFIX}" CACHE INTERNAL "GSI Executable Name")
    add_executable(${GSIEXEC} ${CMAKE_CURRENT_SOURCE_DIR}/gsimain.f90 ${CMAKE_CURRENT_SOURCE_DIR}/gsimod.F90 )
    set(GSILIB "gsilib_wrf${debug_suffix}" CACHE INTERNAL "")
    add_library(${GSILIB} STATIC ${REGIONAL_SRC} )
    set_target_properties(${GSILIB} PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include/wrf")
    target_include_directories(${GSILIB} PUBLIC "${PROJECT_BINARY_DIR}/include/wrf")
    set_target_properties(${GSIEXEC} PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
    set_target_properties(${GSIEXEC} PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include/wrf")
  endif()

  if(BUILD_GLOBAL)
    set(GSIEXEC "${GSIBASE}${debug_suffix}${GSISUFFIX}" CACHE INTERNAL "GSI Executable Name")
    add_executable(${GSIEXEC} ${CMAKE_CURRENT_SOURCE_DIR}/gsimain.f90 ${CMAKE_CURRENT_SOURCE_DIR}/gsimod.F90 )
    set(GSILIB "gsilib_global${debug_suffix}" CACHE INTERNAL "")
    set(WRF_LIBRARIES "")
    add_library(${GSILIB} STATIC ${GLOBAL_SRC} )
    target_link_libraries( ${GSISHAREDLIB} ${GSILIB} ${CORE_LIBRARIES})
    target_link_libraries( ${GSILIB} ${CORE_LIBRARIES})
    set_target_properties(gsilib_global PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
    target_include_directories(gsilib_global PUBLIC "${PROJECT_BINARY_DIR}/include")
    add_dependencies(${GSILIB} ${GSISHAREDLIB} )
    set_target_properties(${GSIEXEC} PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
  endif()

  add_dependencies(${GSILIB} ${GSISHAREDLIB} )

# Add dependencies if we are building the core libs or are just using pre-built libs 
  if(CORE_BUILT)
    add_dependencies(${GSILIB} ${GSISHAREDLIB} ${CORE_BUILT} )
    add_dependencies(${GSISHAREDLIB} ${bacio} ${CORE_BUILT} )
    add_dependencies(${GSIEXEC} ${GSISHAREDLIB} ${GSILIB} ${CORE_BUILT} )
  else()
    add_dependencies(${GSIEXEC} ${GSISHAREDLIB} ${GSILIB} )
  endif()
# Specify all the libraries to be linked into the executable
  target_link_libraries(${GSIEXEC} ${GSISHAREDLIB} ${GSILIB} ${GSISHAREDLIB} ${WRF_LIBRARIES} 
         ${NETCDF_LIBRARIES_F90} ${NETCDF_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES} ${MPI_Fortran_LIBRARIES} 
         ${LAPACK_LIBRARIES} -L./ ${EXTRA_LINKER_FLAGS} ${HDF5_LIBRARIES} ${CURL_LIBRARIES} ${CORE_LIBRARIES} ${CORE_BUILT}
         ${GSI_LDFLAGS} ${NCDIAG_LIBRARIES} ${GSDCLOUD_LIBRARY} ${ZLIB_LIBRARIES} ${wrflib} ${EXTRA_LINKER_FLAGS} )
