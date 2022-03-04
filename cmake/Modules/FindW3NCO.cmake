# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
if(DEFINED ENV{W3NCO_VER})
  set(W3NCO_VER $ENV{W3NCO_VER})
  STRING(REGEX REPLACE "v" "" W3NCO_VER ${W3NCO_VER})
endif()

set( NO_DEFAULT_PATH )
if(NOT BUILD_W3NCO )
  if(DEFINED ENV{W3NCO_LIBd} )
    set(W3NCO_LIBRARY $ENV{W3NCO_LIBd} )
    set(W3NCO_4_LIBRARY $ENV{W3NCO_LIB4} )
    message("W3NCO library ${W3NCO_LIBRARY} set via Environment variable")
    message("W3NCO_4 library ${W3NCO_4_LIBRARY} set via Environment variable")
  endif()
endif()
if( CORE_LIBRARIES )
  list( APPEND CORE_LIBRARIES ${W3NCO_LIBRARY} )
else()
  set( CORE_LIBRARIES ${W3NCO_LIBRARY} )
endif()

set( W3NCO_LIBRARY_PATH ${W3NCO_LIBRARY} CACHE STRING "W3NCO Library Location" )

