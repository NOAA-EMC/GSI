# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
if(DEFINED ENV{BACIO_VER})
  set(BACIO_VER $ENV{BACIO_VER})
  STRING(REGEX REPLACE "v" "" BACIO_VER ${BACIO_VER})
endif()
if(NOT BUILD_BACIO )
  if(DEFINED ENV{BACIO_LIB4})
    set(BACIO_LIBRARY $ENV{BACIO_LIB4} )
  endif()
endif()
if( CORE_LIBRARIES )
  list( APPEND CORE_LIBRARIES ${BACIO_LIBRARY} )
else()
  set( CORE_LIBRARIES ${BACIO_LIBRARY} )
endif()

set( BACIO_LIBRARY_PATH ${BACIO_LIBRARY} CACHE STRING "BACIO Library Location" )

