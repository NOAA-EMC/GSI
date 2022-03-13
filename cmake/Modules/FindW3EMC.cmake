# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
if(DEFINED ENV{W3EMC_VER})
  set(W3EMC_VER $ENV{W3EMC_VER})
  set(W3EMCINC $ENV{W3EMC_INCd} )
  set(W3EMC4INC $ENV{W3EMC_INC4} )
  STRING(REGEX REPLACE "v" "" W3EMC_VER ${W3EMC_VER})
endif()
if(DEFINED ENV{W3EMC_LIBd})
    set(W3EMC_LIBRARY $ENV{W3EMC_LIBd} )
    set(W3EMCINC $ENV{W3EMC_INCd} )
    set(W3EMC_4_LIBRARY $ENV{W3EMC_LIB4} )
    set(W3EMC4INC $ENV{W3EMC_INC4} )
    message("Setting W3EMC library via environment variable ${W3EMC_LIBRARY}")
endif()

set( NO_DEFAULT_PATH )
if((NOT BUILD_W3EMC ) AND ( NOT DEFINED W3EMC_LIBRARY ))
  if(DEFINED ENV{W3EMC_LIB} )
    set(W3EMC_LIBRARY $ENV{W3EMC_LIB} )
    set(W3EMCINC $ENV{W3EMC_INC} )
    set(W3EMC_4_LIBRARY $ENV{W3EMC_LIB4} )
    set(W3EMC4INC $ENV{W3EMC_INC4} )
    message("W3EMC library ${W3EMC_LIBRARY} set via Environment variable")
    message("W3EMC_4 library ${W3EMC_4_LIBRARY} set via Environment variable")
  endif()
endif()
if( CORE_LIBRARIES )
  list( APPEND CORE_LIBRARIES ${W3EMC_LIBRARY} )
else()
  set( CORE_LIBRARIES ${W3EMC_LIBRARY} )
endif()

if( CORE_INCS )
  list( APPEND CORE_INCS ${W3EMCINC} )
else()
  set( CORE_INCS ${INCLUDE_OUTPUT_PATH} ${W3EMCINC} )
endif()

set( W3EMC_LIBRARY_PATH ${W3EMC_LIBRARY} CACHE STRING "W3EMC Library Location" )
set( W3EMC_INCLUDE_PATH ${W3EMCINC} CACHE STRING "W3EMC Include Location" )
set( W3EMC_4_LIBRARY_PATH ${W3EMC_4_LIBRARY} CACHE STRING "W3EMC_4 Library Location" )
set( W3EMC_INCLUDE_4_PATH ${W3EMC4INC} CACHE STRING "W3EMC_4 Include Location" )

