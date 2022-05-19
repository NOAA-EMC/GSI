# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
if(DEFINED ENV{BUFR_VER})
  set(BUFR_VER $ENV{BUFR_VER})
  STRING(REGEX REPLACE "v" "" BUFR_VER ${BUFR_VER})
endif()

set( NO_DEFAULT_PATH )
if(NOT BUILD_BUFR )
  if(DEFINED ENV{BUFR_LIBd_DA} )
    set(BUFR_LIBRARY $ENV{BUFR_LIBd_DA} )
    message("BUFR library ${BUFR_LIBRARY} set via Environment variable")
  endif()
endif()
if( CORE_LIBRARIES )
  list( APPEND CORE_LIBRARIES ${BUFR_LIBRARY} )
else()
  set( CORE_LIBRARIES ${BUFR_LIBRARY} )
endif()
set( BUFR_LIBRARY_PATH ${BUFR_LIBRARY} CACHE STRING "BUFR Library Location" )

