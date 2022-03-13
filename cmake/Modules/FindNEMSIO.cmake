# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
if(DEFINED ENV{NEMSIO_VER})
  set(NEMSIO_VER $ENV{NEMSIO_VER})
  STRING(REGEX REPLACE "v" "" NEMSIO_VER ${NEMSIO_VER})
endif()

set( NO_DEFAULT_PATH )
if(NOT BUILD_NEMSIO )
  if(DEFINED ENV{NEMSIO_LIB} )
    set(NEMSIO_LIBRARY $ENV{NEMSIO_LIB} )
    set(NEMSIOINC $ENV{NEMSIO_INC} )
    message("NEMSIO library ${NEMSIO_LIBRARY} set via Environment variable")
  endif()
endif()
if( CORE_LIBRARIES )
  list( APPEND CORE_LIBRARIES ${NEMSIO_LIBRARY} )
else()
  set( CORE_LIBRARIES ${NEMSIO_LIBRARY} )
endif()

if( CORE_INCS )
  list( APPEND CORE_INCS ${NEMSIOINC} )
else()
  set( CORE_INCS ${INCLUDE_OUTPUT_PATH} ${NEMSIOINC} )
endif()

set( NEMSIO_LIBRARY_PATH ${NEMSIO_LIBRARY} CACHE STRING "NEMSIO Library Location" )
set( NEMSIO_INCLUDE_PATH ${NEMSIOINC} CACHE STRING "NEMSIO Include Location" )

