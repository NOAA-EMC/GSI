# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
if(DEFINED ENV{SFCIO_VER})
  set(SFCIO_VER $ENV{SFCIO_VER})
  STRING(REGEX REPLACE "v" "" SFCIO_VER ${SFCIO_VER})
endif()

set( NO_DEFAULT_PATH )
if(NOT BUILD_SFCIO )
  if(DEFINED ENV{SFCIO_LIB4} )
    set(SFCIO_LIBRARY $ENV{SFCIO_LIB4} )
    set(SFCIOINC $ENV{SFCIO_INC4} )
    message("SFCIO library ${SFCIO_LIBRARY} set via Environment variable")
  elseif(DEFINED ENV{SFCIO_LIB} )
    set(SFCIO_LIBRARY $ENV{SFCIO_LIB} )
    set(SFCIOINC $ENV{SFCIO_INC} )
    message("SFCIO library ${SFCIO_LIBRARY} set via Environment variable")
  endif()
endif()
if( CORE_LIBRARIES )
  list( APPEND CORE_LIBRARIES ${SFCIO_LIBRARY} )
else()
  set( CORE_LIBRARIES ${SFCIO_LIBRARY} )
endif()

if( CORE_INCS )
  list( APPEND CORE_INCS ${SFCIOINC} )
else()
  set( CORE_INCS ${INCLUDE_OUTPUT_PATH} ${SFCIOINC} )
endif()

set( SFCIO_LIBRARY_PATH ${SFCIO_LIBRARY} CACHE STRING "SFCIO Library Location" )
set( SFCIO_INCLUDE_PATH ${SFCIOINC} CACHE STRING "SFCIO Include Location" )

