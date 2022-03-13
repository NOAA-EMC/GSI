# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
if(DEFINED ENV{SIGIO_VER})
  set(SIGIO_VER $ENV{SIGIO_VER})
  STRING(REGEX REPLACE "v" "" SIGIO_VER ${SIGIO_VER})
endif()

set( NO_DEFAULT_PATH )
if(NOT BUILD_SIGIO )
  if(DEFINED ENV{SIGIO_LIB4} )
    set(SIGIO_LIBRARY $ENV{SIGIO_LIB4} )
    set(SIGIOINC $ENV{SIGIO_INC4} )
    message("SIGIO library ${SIGIO_LIBRARY} set via Environment variable")
  elseif(DEFINED ENV{SIGIO_LIB} )
    set(SIGIO_LIBRARY $ENV{SIGIO_LIB} )
    set(SIGIOINC $ENV{SIGIO_INC} )
    message("SIGIO library ${SIGIO_LIBRARY} set via Environment variable")
  endif()
endif()
if( CORE_LIBRARIES )
  list( APPEND CORE_LIBRARIES ${SIGIO_LIBRARY} )
else()
  set( CORE_LIBRARIES ${SIGIO_LIBRARY} )
endif()

if( CORE_INCS )
  list( APPEND CORE_INCS ${SIGIOINC} )
else()
  set( CORE_INCS ${INCLUDE_OUTPUT_PATH} ${SIGIOINC} )
endif()


set( SIGIO_LIBRARY_PATH ${SIGIO_LIBRARY} CACHE STRING "SIGIO Library Location" )
set( SIGIO_INCLUDE_PATH ${SIGIOINC} CACHE STRING "SIGIO Include Location" )

