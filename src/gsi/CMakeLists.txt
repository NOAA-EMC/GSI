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

# stub_nstmod.f90 is not used by global GSI.

  set( SRC_MAINS
    "${CMAKE_CURRENT_SOURCE_DIR}/gsimod.F90" 
    "${CMAKE_CURRENT_SOURCE_DIR}/gsimain.f90"
  )

  set( SRC_NCDIAG )
  if( BUILD_NCDIAG )
    set( SRC_NCDIAG
      "${CMAKE_CURRENT_SOURCE_DIR}/nc_diag_read_mod.f90"
      "${CMAKE_CURRENT_SOURCE_DIR}/nc_diag_write_mod.f90"
    )
  endif(BUILD_NCDIAG )

# cplr_nstmod.f90 is used by global GSI.  And stub_nstmod.f90 is used by regional GSI.
  set( SRC_FOR_GLOBAL   ${STUB_SRC} "${CMAKE_CURRENT_SOURCE_DIR}/cplr_gfs_nstmod.f90" )
  set( SRC_FOR_REGIONAL ${CPLR_SRC} "${CMAKE_CURRENT_SOURCE_DIR}/stub_nstmod.f90" )

# Remove all configurable entries
  list(REMOVE_ITEM GSI_SRC
    ${SRC_MAINS}
    ${SRC_FOR_REGIONAL}
    ${SRC_FOR_GLOBAL}
    ${SRC_NCDIAG}
  )

# Include configuration specific entries
  set( LIBSRC_GLOBAL   ${GSI_SRC} ${SRC_FOR_GLOBAL}   ${GSI_C_SRC} )
  set( LIBSRC_REGIONAL ${GSI_SRC} ${SRC_FOR_REGIONAL} ${GSI_C_SRC} )

  set( SRC_GSILIB )
  if(USE_WRF)
    set( SRC_GSILIB ${LIBSRC_REGIONAL} )
  endif(USF_WRF)
  if(BUILD_GLOBAL)
    set( SRC_GSILIB ${LIBSRC_GLOBAL} )
  endif(BUILD_GLOBAL)

  set_source_files_properties( ${SRC_MAINS}  PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS} )
  set_source_files_properties( ${SRC_GSILIB} PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS} )
  set_source_files_properties( ${GSI_C_SRC}  PROPERTIES COMPILE_FLAGS ${GSI_CFLAGS} )

  list( APPEND SRC_GSILIB ${GSI_C_SRC} )

# The specific regional/WRF source files
#  if(USE_WRF)
#    set( REGIONAL_SRC ${CPLR_SRC} )
#    list( APPEND REGIONAL_SRC ${LINKED_SRC} )
#    set_source_files_properties( ${REGIONAL_SRC} PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS} )
#  endif()

# The non-WRF specific src files
#  set( GLOBAL_SRC ${CPLR_SRC} )
#  set( GLOBAL_SRC ${STUB_SRC} )
#  list( APPEND GLOBAL_SRC ${LINKED_SRC} )
  
#  list( APPEND GSI_SRC  ${GSI_C_SRC} )

# set up the compiler flags
#  set_source_files_properties( ${GSI_SRC} PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS} )
#  set_source_files_properties( ${GLOBAL_SRC} PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS} )
#  set_source_files_properties( ${GSI_C_SRC} PROPERTIES COMPILE_FLAGS ${GSI_CFLAGS} )
# set_source_files_properties( ${CMAKE_CURRENT_SOURCE_DIR}/blockIO.c  PROPERTIES COMPILE_FLAGS ${GSI_CFLAGS} )

# Add the include paths
  message("MPI include PATH  ${MPI_Fortran_INCLUDE_PATH}")
  include_directories( ${CORE_INCS} ${NETCDF_INCLUDES} ${MPI_Fortran_INCLUDE_PATH} "./"  )

#define the name used for GSI Shared lib and add it with properties and include dirs
  set(GSISHAREDLIB "gsilib_shrd${debug_suffix}" CACHE INTERNAL "")
  add_library(${GSISHAREDLIB} STATIC ${SRC_GSILIB})

#  if(BUILD_GLOBAL)
#    add_library(${GSISHAREDLIB} STATIC ${SRC_GLOBAL})
#  else()
#    add_library(${GSISHAREDLIB} STATIC ${SRC_REGIONAL})
#  endif()

  set_target_properties(${GSISHAREDLIB} PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
  target_include_directories(${GSISHAREDLIB} PUBLIC "${PROJECT_BINARY_DIR}/include")
  target_include_directories(${GSISHAREDLIB} PUBLIC "${PROJECT_BINARY_DIR}/include")
  if( BUILD_NCDIAG )
      add_dependencies(${GSISHAREDLIB} ${NCDIAG_LIBRARIES} )
  endif( BUILD_NCDIAG )

#---- target: ${GSIEXEC} ----
#${GSIEXEC}: ${SRC_MAINS} -I ${PROJECT_BINARY_DIR}/include ${GSISHAREDLIB}

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
  set(GSIEXEC "${GSIBASE}${debug_suffix}${GSISUFFIX}" CACHE INTERNAL "GSI Executable Name")
  add_executable( ${GSIEXEC} ${SRC_MAINS} )
  set_target_properties(${GSIEXEC} PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")

  set( REGIONAL_LIBRARIES "" )
  if(USE_WRF)
    set( REGIONAL_LIBRARIES ${WRF_LIBRARIES} )
    set_target_properties(${GSIEXEC} PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include/wrf")
    set_target_properties(${GSISHAREDLIB} PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include/wrf")
  endif()

# Set names, libs, and properties depending on if we are building with WRF or not
#  if(USE_WRF) 
#    set(GSIEXEC "${GSIBASE}${debug_suffix}${GSISUFFIX}" CACHE INTERNAL "GSI Executable Name")
#    add_executable(${GSIEXEC} ${CMAKE_CURRENT_SOURCE_DIR}/gsimain.f90 ${CMAKE_CURRENT_SOURCE_DIR}/gsimod.F90 )
#    set(GSILIB "gsilib_wrf${debug_suffix}" CACHE INTERNAL "")
#    add_library(${GSILIB} STATIC 
#      "${CMAKE_CURRENT_SOURCE_DIR}/gsimod.F90" 
#      "${CMAKE_CURRENT_SOURCE_DIR}/gsimain.f90"
#    )
#    set_target_properties(${GSILIB} PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include/wrf")
#    target_include_directories(${GSILIB} PUBLIC "${PROJECT_BINARY_DIR}/include/wrf")
#    set_target_properties(${GSIEXEC} PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
#    set_target_properties(${GSIEXEC} PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include/wrf")
#  endif()

#  if(BUILD_GLOBAL)
#    set(GSIEXEC "${GSIBASE}${debug_suffix}${GSISUFFIX}" CACHE INTERNAL "GSI Executable Name")
#    add_executable(${GSIEXEC} ${CMAKE_CURRENT_SOURCE_DIR}/gsimain.f90 ${CMAKE_CURRENT_SOURCE_DIR}/gsimod.F90 )
#    set(GSILIB "gsilib_global${debug_suffix}" CACHE INTERNAL "")
#    set(WRF_LIBRARIES "")
#    add_library(${GSILIB} STATIC 
#      "${CMAKE_CURRENT_SOURCE_DIR}/gsimod.F90" 
#      "${CMAKE_CURRENT_SOURCE_DIR}/gsimain.f90"
#    )

#    add_dependencies(${GSILIB} ${GSISHAREDLIB} )
    if(crtm)
      add_dependencies(${GSISHAREDLIB} ${crtm} )
      message(">>> GSISHAREDLIB = ${GSISHAREDLIB}")
      message(">>>         crtm = ${crtm}")
      message(">>>   CORE_BUILD = ${CORE_BUILD}")
    else()
      message(">>> GSISHAREDLIB = ${GSISHAREDLIB}")
      message(">>>         crtm = <null>")
      message(">>>   CORE_BUILD = ${CORE_BUILD}")
    endif()

#    target_link_libraries( ${GSILIB} ${CORE_LIBRARIES})

#    set_target_properties(gsilib_global PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
#    target_include_directories(gsilib_global PUBLIC "${PROJECT_BINARY_DIR}/include")
#    set_target_properties(${GSIEXEC} PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
#  endif()

#  add_dependencies(${GSILIB} ${GSISHAREDLIB} )

# Add dependencies if we are building the core libs or are just using pre-built libs 
  message(">>>     CORE_BUILT = ${CORE_BUILT}")
  message(">>>   CRTM_LIBRARY = ${CRTM_LIBRARY}")
  message(">>> CORE_LIBRARIES = ${CORE_LIBRARIES}")
  message(">>>           crtm = ${crtm}")

  if(CORE_BUILT)
    #add_dependencies(${GSISHAREDLIB} ${bacio} ${crtm} ${CORE_BUILT} )
    add_dependencies(${GSISHAREDLIB} ${CORE_BUILT} )
    add_dependencies(${GSIEXEC} ${GSISHAREDLIB} ${CORE_BUILT} )
  else()
    add_dependencies(${GSISHAREDLIB} ${CORE_LIBRARIES})
    add_dependencies(${GSIEXEC} ${GSISHAREDLIB} )
  endif()
# Specify all the libraries to be linked into the executable

  target_link_libraries(${GSIEXEC} ${GSISHAREDLIB} ${REGIONAL_LIBRARIES} 
         ${NETCDF_LIBRARIES_F90} ${NETCDF_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES} ${MPI_Fortran_LIBRARIES} 
         ${LAPACK_LIBRARIES} -L./ ${EXTRA_LINKER_FLAGS} ${HDF5_LIBRARIES} ${CURL_LIBRARIES} ${CORE_LIBRARIES} ${CORE_BUILT}
         ${GSI_LDFLAGS} ${NCDIAG_LIBRARIES} ${ZLIB_LIBRARIES} ${wrflib} )
  install(TARGETS ${GSIEXEC}
        RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}/bin
        LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX}/lib
        ARCHIVE DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/static)
