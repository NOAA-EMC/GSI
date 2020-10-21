cmake_minimum_required(VERSION 2.8)
if(BUILD_NCDIAG)

  if( NOT USE_BASELIBS )
  if( BUILD_NCDIAG_SERIAL )
    set(NCDIAG_SERIAL_MODULE_DIR ${PROJECT_BINARY_DIR}/include/ncdiag_serial)
    # NetCDF-4 library
    set(NCDIAG_SERIAL_INCS "${PROJECT_BINARY_DIR}/include/ncdiag_serial")

    include_directories( ${NETCDF_INCLUDES} ${NCDIAG_SERIAL_INCS} )

    # 32-bit reals, for now
    add_definitions(-D_REAL4_)

    message("HEY!!! ncdiag flags are ${NCDIAG_Fortran_FLAGS}")
    FILE(GLOB NCDIAG_SRC ${CMAKE_CURRENT_SOURCE_DIR}/../*90 )
    set_source_files_properties( ${NCDIAG_SRC} PROPERTIES COMPILE_FLAGS ${NCDIAG_Fortran_FLAGS} )
    LIST(REMOVE_ITEM NCDIAG_SRC ${CMAKE_CURRENT_SOURCE_DIR}/test_nc_unlimdims.F90 ) 
    LIST(REMOVE_ITEM NCDIAG_SRC ${CMAKE_CURRENT_SOURCE_DIR}/nc_diag_cat.F90 ) 
    add_library(ncdiag_serial STATIC ${NCDIAG_SRC})
    set_target_properties( ncdiag_serial PROPERTIES Fortran_MODULE_DIRECTORY ${NCDIAG_SERIAL_MODULE_DIR} )
    add_executable(nc_diag_cat_serial.x ${CMAKE_CURRENT_SOURCE_DIR}/../nc_diag_cat.F90 )
    set_target_properties( nc_diag_cat_serial.x PROPERTIES COMPILE_FLAGS ${NCDIAG_Fortran_FLAGS} )
    set_target_properties( nc_diag_cat_serial.x PROPERTIES Fortran_MODULE_DIRECTORY ${NCDIAG_SERIAL_MODULE_DIR} )
    target_link_libraries(nc_diag_cat_serial.x ncdiag_serial ${NETCDF_LIBRARIES} ${HDF5_HL_LIBRARIES} ${HDF5_LIBRARIES} ${ZLIB_LIBRARIES} ${MPI_Fortran_LIBRARIES} )  
  endif(BUILD_NCDIAG_SERIAL)
  endif( NOT USE_BASELIBS )
endif(BUILD_NCDIAG)



