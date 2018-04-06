# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
include(findHelpers)
if(DEFINED ENV{W3NCO_VER})
  set(W3NCO_VER $ENV{W3NCO_VER})
  STRING(REGEX REPLACE "v" "" W3NCO_VER ${W3NCO_VER})
endif()

set( NO_DEFAULT_PATH )
if(NOT BUILD_W3NCO )
  if(DEFINED ENV{W3NCO_LIBd} )
    set(W3NCO_LIBRARY $ENV{W3NCO_LIBd} )
    message("W3NCO library ${W3NCO_LIBRARY} set via Environment variable")
  else()
    find_library( W3NCO_LIBRARY 
    NAMES libw3nco_v${W3NCO_VER}_d.a libw3nco_d.a  libw3nco_i4r8.a 
    HINTS 
       $ENV{COREPATH}/lib 
       /usr/local/jcsda/nwprod_gdas_2014	
       ${COREPATH}/w3nco/v${W3NCO_VER}
       ${COREPATH}/w3nco/v${W3NCO_VER}/intel
    PATH_SUFFIXES
        lib
     ${NO_DEFAULT_PATH})
#   set( w3nco "w3nco_v${W3NCO_VER}")
    message("Found W3NCO library ${W3NCO_LIBRARY}")
    set( w3nco ${W3NCO_LIBRARY})
  endif()
endif()
if( NOT W3NCO_LIBRARY ) # didn't find the library, so build it from source
    message("Could not find W3NCO library, so building from libsrc")
    if( NOT DEFINED ENV{W3NCO_SRC} )
        findSrc( "w3nco" W3NCO_VER W3NCO_DIR )
    else()
      set( W3NCO_DIR "$ENV{W3NCO_SRC}/libsrc" CACHE STRING "W3NCO Source Location")
    endif()
    set( libsuffix "_v${W3NCO_VER}${debug_suffix}" )
    set( W3NCO_LIBRARY "${LIBRARY_OUTPUT_PATH}/libw3nco${libsuffix}.a" CACHE STRING "W3NCO Library" )
    set( w3nco "w3nco${libsuffix}")
    set( BUILD_W3NCO "ON" CACHE INTERNAL "Build the W3NCO library")
    add_subdirectory(${CMAKE_SOURCE_DIR}/libsrc/w3nco)
    set( W3NCO_LIBRARY ${w3nco} )
    if( CORE_BUILT )
      list( APPEND CORE_BUILT ${W3NCO_LIBRARY} )
    else()
      set( CORE_BUILT ${W3NCO_LIBRARY} )
    endif()
else( NOT W3NCO_LIBRARY )
  if( CORE_LIBRARIES )
    list( APPEND CORE_LIBRARIES ${W3NCO_LIBRARY} )
  else()
    set( CORE_LIBRARIES ${W3NCO_LIBRARY} )
  endif()
endif( NOT W3NCO_LIBRARY )

set( W3NCO_LIBRARY_PATH ${W3NCO_LIBRARY} CACHE STRING "W3NCO Library Location" )

