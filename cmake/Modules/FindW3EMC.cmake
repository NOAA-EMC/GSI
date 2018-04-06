# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
include(findHelpers)
if(DEFINED ENV{W3EMC_VERd})
  set(W3EMC_VER $ENV{W3EMC_VERd})
  set(W3EMCINC $ENV{W3EMC_INCd} )
  STRING(REGEX REPLACE "v" "" W3EMC_VER ${W3EMC_VER})
endif()

set( NO_DEFAULT_PATH )
if(NOT BUILD_W3EMC )
  if(DEFINED ENV{W3EMC_LIB} )
    set(W3EMC_LIBRARY $ENV{W3EMC_LIB} )
    set(W3EMCINC $ENV{W3EMC_INC} )
    message("W3EMC library ${W3EMC_LIBRARY} set via Environment variable")
  else()
    find_path( W3EMCINC 
      NAMES mersenne_twister.mod 
      HINTS 
        $ENV{COREPATH}/lib/incmod/w3emc_4 
        $ENV{COREPATH}/include 
        /usr/local/jcsda/nwprod_gdas_2014/lib/incmod/w3emc_4 
        ${COREPATH}/w3emc/v${W3EMC_VER}/incmod/w3emc_v${W3EMC_VER}_d
        ${COREPATH}/w3emc/v${W3EMC_VER}/intel/w3emc_v${W3EMC_VER}_d
    )
    find_library( W3EMC_LIBRARY 
    NAMES libw3emc_4.a libw3emc_i4r8.a libw3emc_v${W3EMC_VER}_d.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014	
      ${COREPATH}/w3emc/v${W3EMC_VER}
      ${COREPATH}/w3emc/v${W3EMC_VER}/intel
    PATH_SUFFIXES
        lib
     ${NO_DEFAULT_PATH})
    set( w3emc "w3emc_v${W3EMC_VER}")
    message("Found W3EMC library ${W3EMC_LIBRARY}")
  endif()
endif()
if( NOT W3EMC_LIBRARY ) # didn't find the library, so build it from source
    message("Could not find W3EMC library, so building from libsrc")
    if( NOT DEFINED ENV{W3EMC_SRC} )
        findSrc( "w3emc" W3EMC_VER W3EMC_DIR )
        set(W3EMCINC  "${CMAKE_BINARY_DIR}/include")
    else()
      set( W3EMC_DIR "$ENV{W3EMC_SRC}/libsrc" CACHE STRING "W3EMC Source Location")
    endif()
    set( libsuffix "_v${W3EMC_VER}${debug_suffix}" )
    set( W3EMC_LIBRARY "${LIBRARY_OUTPUT_PATH}/libw3emc${libsuffix}.a" CACHE STRING "W3EMC Library" )
    set( w3emc "w3emc${libsuffix}")
    set( BUILD_W3EMC "ON" CACHE INTERNAL "Build the W3EMC library")
    add_subdirectory(${CMAKE_SOURCE_DIR}/libsrc/w3emc)
    set( W3EMC_LIBRARY ${w3emc} )
  if( CORE_BUILT )
    list( APPEND CORE_BUILT ${W3EMC_LIBRARY} )
  else()
    set( CORE_BUILT ${W3EMC_LIBRARY} )
  endif()
else( NOT W3EMC_LIBRARY )
  if( CORE_LIBRARIES )
    list( APPEND CORE_LIBRARIES ${W3EMC_LIBRARY} )
  else()
    set( CORE_LIBRARIES ${W3EMC_LIBRARY} )
  endif()
endif( NOT W3EMC_LIBRARY )

if( CORE_INCS )
  list( APPEND CORE_INCS ${W3EMCINC} )
else()
  set( CORE_INCS ${INCLUDE_OUTPUT_PATH} ${W3EMCINC} )
endif()

set( W3EMC_LIBRARY_PATH ${W3EMC_LIBRARY} CACHE STRING "W3EMC Library Location" )
set( W3EMC_INCLUDE_PATH ${W3EMCINC} CACHE STRING "W3EMC Include Location" )

