# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
if(DEFINED ENV{IP_VER})
  set(IP_VER $ENV{IP_VER})
  STRING(REGEX REPLACE "v" "" IP_VER ${IP_VER})
endif()

set( NO_DEFAULT_PATH )
if(NOT BUILD_IP )
  if(DEFINED ENV{IP_LIBd} )
    set(IP_LIBRARY $ENV{IP_LIBd} )
    message("IP library ${IP_LIBRARY} set via Environment variable")
  endif()
  if(DEFINED ENV{IP_LIB4} )
    set(IP_4_LIBRARY $ENV{IP_LIB4} )
    message("IP 4 library ${IP_4_LIBRARY} set via Environment variable")
  endif()
endif()
if( CORE_LIBRARIES )
  list( APPEND CORE_LIBRARIES ${IP_LIBRARY} )
else()
  set( CORE_LIBRARIES ${IP_LIBRARY} )
endif()


set( IP_LIBRARY_PATH ${IP_LIBRARY} CACHE STRING "IP Library Location" )
set( IP_4_LIBRARY_PATH ${IP_4_LIBRARY} CACHE STRING "IP_4 Library Location" )

