cmake_minimum_required(VERSION 2.8)
# need to set CMP0046 when using add_dependencies with cmake version 3.6.2
  include(${CMAKE_CURRENT_SOURCE_DIR}/GSISRC.cmake) #this is a macro that contains all the source files needed for GSI builds
  if(crayComp)
    cmake_policy(SET CMP0046 NEW)
  endif()
  set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
  setSRCFiles() # All source files are set in this macro to help with readability of this CMakeLists.txt file
                # Variables set are as follows--
                # GSIFORT_SRC contains the main fortran source files for GSI
                # GSI_C_SRC contains the C source files for GSI
                # CLASS_SRC contains the class files for WRF and nonWRF based builds
                # CPLR_SRC contains the coupler files for WRF-based builds
                # STUB_SRC contains the stub files for non-WRF builds 
                # GSIMAIN_SRC contains the Fortran main(s) for gsi executable
                # GSIUTIL_SRC list of all Fortran modules as *base level* utilities.
                

# remove NCDIAG, if it is a component built somewhere else.
  if( BUILD_NCDIAG )
    list( REMOVE_ITEM GSIFORT_SRC nc_diag_read_mod.f90 )
    list( REMOVE_ITEM GSIFORT_SRC nc_diag_write_mod.f90 )
  endif( BUILD_NCDIAG )

# The specific regional/WRF source files
  if(USE_WRF)
    list( APPEND GSIFORT_SRC
	${CPLR_SRC}
	${CLASS_SRC}
	gsi_fixture_REGIONAL.F90
    )
  endif()

# The specific global source files
  if(BUILD_GLOBAL)
    list( APPEND GSIFORT_SRC
	${STUB_SRC}
	${CLASS_SRC}
	${CMAKE_CURRENT_SOURCE_DIR}/gsi_fixture_GFS.F90
    )
  endif(BUILD_GLOBAL)
  set(GSI_Fortran_FLAGS ${GSI_Platform_FLAGS})
  message("GSI_Fortran flags are ${GSI_Fortran_FLAGS}")
# set up the compiler flags
  set_source_files_properties( ${GSIMAIN_SRC}	PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS} )
  set_source_files_properties( ${GSIFORT_SRC}	PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS} )
  set_source_files_properties( ${GSIUTIL_SRC}	PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS} )
  set_source_files_properties( ${GSI_C_SRC}	PROPERTIES COMPILE_FLAGS ${GSI_CFLAGS} )

  set( GSICORE_SRC ${GSIFORT_SRC} ${GSI_C_SRC} )

#define the name used for GSI Shared lib and add it with properties and include dirs
  set(GSISHAREDLIB "gsilib_shrd${debug_suffix}" CACHE INTERNAL "")
  message("GSIUTIL src is ${GSIUTIL_SRC}")
  add_library(${GSISHAREDLIB} STATIC ${GSIUTIL_SRC} )
  set_target_properties(${GSISHAREDLIB} PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
  target_include_directories(${GSISHAREDLIB} PUBLIC "${PROJECT_BINARY_DIR}/include"
     ${sigio_inc4} ${nemsio_inc} ${crtm_inc} ${sfcio_inc4} ${w3emc_incd} ${NetCDF_INCLUDE_DIRS} 
     ${MPI_Fortran_INCLUDE_PATH} ${FV3GFS_NCIO_INCS} "./"  )

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
    add_library(${GSILIB} STATIC ${GSICORE_SRC} )
    set_target_properties(${GSILIB} PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
    target_include_directories(${GSILIB} PUBLIC "${PROJECT_BINARY_DIR}/include" ${crtm_inc} ${w3emc_incd} ${sigio_inc4} ${sfcio_inc4} ${nemsio_inc} ${NETCDF_INCLUDE_DIRS})
    set_target_properties(${GSIEXEC} PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
#   target_link_libraries( ${GSILIB} ${GSISHAREDLIB} ${wrflib} bufr_d crtm nemsio sp_d sfcio_4 sigio_4 bacio_4 w3emc_d w3nco_d bufr_d bacio_4 )
  endif()

  if(BUILD_GLOBAL)
    set(GSIEXEC "${GSIBASE}${debug_suffix}${GSISUFFIX}" CACHE INTERNAL "GSI Executable Name")
    add_executable( ${GSIEXEC} ${GSIMAIN_SRC} )
    set(GSILIB "gsilib_global${debug_suffix}" CACHE INTERNAL "")
    set(WRF_LIBRARIES "")
    add_library(${GSILIB} STATIC ${GSICORE_SRC} )
    target_link_libraries( ${GSILIB} ${GSISHAREDLIB} )
    add_dependencies(${GSILIB} ${GSISHAREDLIB} )
    target_link_libraries( ${GSIEXEC} ${GSISHAREDLIB} bufr_d crtm nemsio sp_d sigio_4 sfcio_4 bacio_4 w3emc_d w3nco_d bufr_d bacio_4 )
    set_target_properties( ${GSILIB} PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
    target_include_directories( ${GSILIB} PUBLIC "${PROJECT_BINARY_DIR}/include" ${crtm_inc} ${w3emc_inc} ${NETCDF_INCLUDE_DIRS})
    set_target_properties(${GSIEXEC} PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
  endif()
  if( BUILD_NCDIAG )
      add_dependencies(${GSILIB} ${NCDIAG_LIBRARIES} )
  endif( BUILD_NCDIAG )

  add_dependencies(${GSILIB} ${GSISHAREDLIB} )
  add_dependencies(${GSIEXEC} ${GSISHAREDLIB} ${GSILIB} )
  # Removing the -lm flag is required to get bitwise reproducibility with old ProdGSI. May not be necessary after full conversion to GSI
  string(REGEX REPLACE " -lm" "" NETCDF_C_LIBRARIES "${NETCDF_C_LIBRARIES}") 
  string(REGEX REPLACE " -lm" "" NETCDF_Fortran_LIBRARIES "${NETCDF_Fortran_LIBRARIES}") 
  message("netcdf and mpi ${NETCDF_Fortran_LIBRARIES} ${NETCDF_C_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES} ${MPI_Fortran_LIBRARIES} ")
  message("lapack ${LAPACK_LIBRARIES}")
  message(" linker flags and curl ${EXTRA_LINKER_FLAGS} ${HDF5_LIBRARIES}||||${CURL_LIBRARIES} ")
  message("ldflags  ${GSI_LDFLAGS}")

  target_link_libraries(${GSIEXEC} ${GSISHAREDLIB} ${GSILIB} ${GSISHAREDLIB} ${WRF_LIBRARIES} 
         ${NETCDF_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES} ${MPI_Fortran_LIBRARIES} 
         ${LAPACK_LIBRARIES} -L./ ${EXTRA_LINKER_FLAGS} ${HDF5_LIBRARIES} ${CURL_LIBRARIES} 
         bacio_4 bufr_d sigio_4 bacio_4 nemsio crtm sp_d sfcio_4 w3emc_d w3nco_d ip_d 
         ${GSI_LDFLAGS} ${NCDIAG_LIBRARIES} ${GSDCLOUD_LIBRARY} ${ZLIB_LIBRARIES} ${wrflib} ${EXTRA_LINKER_FLAGS} ${FV3GFS_NCIO_LIBRARIES})

  install(TARGETS ${GSIEXEC}
        RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}/bin
        LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX}/lib
        ARCHIVE DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/static)
