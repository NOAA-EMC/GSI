# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
if(DEFINED ENV{SP_VER})
  set(SP_VER $ENV{SP_VER})
  STRING(REGEX REPLACE "v" "" SP_VER ${SP_VER})
endif()

set( NO_DEFAULT_PATH )
if(NOT BUILD_SP )
  if(DEFINED ENV{SP_LIBd} )
    set(SP_LIBRARY $ENV{SP_LIBd} )
    message("SP library ${SP_LIBRARY} set via Environment variable")
  endif()
  if(DEFINED ENV{SP_LIB4} )
    set(SP_4_LIBRARY $ENV{SP_LIB4} )
    message("SP library ${SP_4_LIBRARY} set via Environment variable")
  endif()
endif()
if( CORE_LIBRARIES )
  list( APPEND CORE_LIBRARIES ${SP_LIBRARY} )
else()
  set( CORE_LIBRARIES ${SP_LIBRARY} )
endif()


set( SP_LIBRARY_PATH ${SP_LIBRARY} CACHE STRING "SP Library Location" )
set( SP_4_LIBRARY_PATH ${SP_4_LIBRARY} CACHE STRING "SP_4 Library Location" )

