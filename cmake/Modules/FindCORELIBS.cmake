function (findSrc varName version varDir )
        set(searchName ${varName}_v${${version}})
        message("searching for source for ${searchName} in ${CRTM_BASE}")
        string( TOLOWER ${varName} varNameLower )
        find_path( TMP_DIR
        NAMES ${searchName}
        HINTS
          ${CMAKE_SOURCE_DIR}/../libs
          ${CRTM_BASE}/${version}
          ${CRTM_BASE}/${varName}
          ${CRTM_BASE}/${varName}/${version}
          ${CRTM_BASE}/${varNameLower}
          ${CRTM_BASE}/${varNameLower}/${version}
          ${COREPATH}/sorc
          $ENV{${varDir}}/libsrc
          $ENV{${varDir}}/lib/sorc
          $ENV{CORPATH}/lib/sorc
          ${CMAKE_SOURCE_DIR}/core-libs/${varName}
        ) 
        if( NOT TMP_DIR ) 
           message("didn't find directory")
           set(secondSearchName v${${version}})
           find_path( TMP2_DIR
           NAMES ${secondSearchName}
           HINTS
             ${CRTM_BASE}/${varName}
           )
        endif()
        set( varCacheName "${varDir}_SRC" )
        file(GLOB f_FILES "${TMP_DIR}/${varName}_v${${version}}/*.f*" "${TMP_DIR}/${varName}_v${${version}}/*.F*")
        if( f_FILES )
          set( ${varDir} "${TMP_DIR}/${varName}_v${${version}}" PARENT_SCOPE)
          set( ${varCacheName} "${TMP_DIR}/${varName}_v${${version}}" CACHE STRING "" FORCE  )
        else()
          file(GLOB f_FILES "${TMP_DIR}/${varName}_v${${version}}/src/*.f*" "${TMP_DIR}/${varName}_v${${version}}/src/*.F*")
          if( f_FILES )
            set( ${varDir} "${TMP_DIR}/${varName}_v${${version}}/src" PARENT_SCOPE)
            set( ${varCacheName} "${TMP_DIR}/${varName}_v${${version}}/src" CACHE STRING "" FORCE  )
          else()
            file(GLOB f_FILES "${TMP_DIR}/${varName}_v${${version}}/libsrc/*.f*" "${TMP_DIR}/${varName}_v${${version}}/src/*.F*")
            if( f_FILES )
              set( ${varDir} "${TMP_DIR}/${varName}_v${${version}}/libsrc" PARENT_SCOPE)
              set( ${varCacheName} "${TMP_DIR}/${varName}_v${${version}}/libsrc" CACHE STRING "" FORCE  )
            else()
              file(GLOB f_FILES "${TMP_DIR}/${varName}_v${${version}}/sorc/*.f*" "${TMP_DIR}/${varName}_v${${version}}/sorc/*.F*")
              if( f_FILES )
                set( ${varDir} "${TMP_DIR}/${varName}_v${${version}}/sorc" PARENT_SCOPE)
                set( ${varCacheName} "${TMP_DIR}/${varName}_v${${version}}/sorc" CACHE STRING "" FORCE  )
              else()
                file(GLOB f_FILES "${TMP_DIR}/${varName}_v${${version}}/sorc/libsrc/*.f*" 
                        "${TMP_DIR}/${varName}_v${${version}}/sorc/libsrc/*.F*")
                 if( f_FILES )
                  set( ${varDir} "${TMP_DIR}/${varName}_v${${version}}/sorc/libsrc" PARENT_SCOPE)
                  set( ${varCacheName} "${TMP_DIR}/${varName}_v${${version}}/sorc/libsrc" CACHE STRING "" FORCE  )
                 else() 
                   file(GLOB f_FILES "${TMP2_DIR}/v${${version}}/src/*.f*" 
                        "${TMP_DIR}/v${${version}}/src/*.F*")
                   if( f_FILES )
                    set( ${varDir} "${TMP2_DIR}/v${${version}}/src" PARENT_SCOPE)
                    set( ${varCacheName} "${TMP2_DIR}/v${${version}}/src" CACHE STRING "" FORCE  )
                   endif()
                 endif()
              endif()
            endif()
          endif()
        endif()
        if( NOT f_FILES ) # look for source that is of a different version
          message("WARNING: Did not find ${${version}} of ${varName}, looking for alternates")
          findOtherVersion( TMP_DIR ${varName} srcPath ${version} )
          file(GLOB f_FILES "${srcPath}/*.f*" "${srcPath}/*.F*")
          if( f_FILES )
            set( ${varDir} "${srcPath}" PARENT_SCOPE)
            set( ${varCacheName} "${srcPath}" CACHE STRING "" FORCE  )
          else()
            file(GLOB f_FILES "${srcPath}/src/*.f*" "${srcPath}/src/*.F*")
            if( f_FILES )
              set( ${varDir} "${srcPath}/src" PARENT_SCOPE)
              set( ${varCacheName} "${srcPath}/src" CACHE STRING "" FORCE  )
            else()
              file(GLOB f_FILES "${srcPath}/libsrc/*.f*" "${srcPath}/src/*.F*")
              if( f_FILES )
                set( ${varDir} "${srcPath}/libsrc" PARENT_SCOPE)
                set( ${varCacheName} "${srcPath}/libsrc" CACHE STRING "" FORCE  )
              else()
                file(GLOB f_FILES "${srcPath}/sorc/*.f*" "${srcPath}/sorc/*.F*")
                if( f_FILES )
                  set( ${varDir} "${srcPath}/sorc" PARENT_SCOPE)
                  set( ${varCacheName} "${srcPath}/sorc" CACHE STRING "" FORCE  )
                else()
                  file(GLOB f_FILES "${srcPath}/sorc/libsrc/*.f*" 
                          "${srcPath}/sorc/libsrc/*.F*")
                   if( f_FILES )
                    set( ${varDir} "${srcPath}/sorc/libsrc" PARENT_SCOPE)
                    set( ${varCacheName} "${srcPath}/sorc/libsrc" CACHE STRING "" FORCE  )
                  endif()
                endif()
              endif()
            endif()
          endif()
        endif()
endfunction()

function (findInc incName version incFile )
  cmake_policy(SET CMP0011 NEW)
  cmake_policy(SET CMP0009 NEW)
  STRING(COMPARE EQUAL ${incFile} "CRTMINC" USECRTMBASE )
  if(( USECRTMBASE ) AND ( CRTM_BASE ))
    execute_process(COMMAND find ${CRTM_BASE} -iname ${incName}_module.mod RESULT_VARIABLE res OUTPUT_VARIABLE INCFILES)
#   file(GLOB_RECURSE INCFILES ${CRTM_BASE}/*${CRTM_VER}*/*mod )
#   file(GLOB_RECURSE INCFILES2 ${CRTM_BASE}/crtm/*${CRTM_VER}*/*/*mod )
#   list(APPEND INCFILES ${INCFILES2} )
  else()
    execute_process(COMMAND find ${COREPATH}/${incName} -iname ${incName}_module.mod RESULT_VARIABLE res OUTPUT_VARIABLE INCFILES)
    if( NOT (INCFILES) )
      execute_process(COMMAND find ${COREPATH}/sorc -iname ${incName}_module.mod RESULT_VARIABLE res OUTPUT_VARIABLE INCFILES)
    endif()
  endif()
  message("incfiles are ${INCFILES}")
  if( INCFILES )
     string(REGEX REPLACE "\n" ";" INCFILES ${INCFILES} )
  endif()
  foreach( INC_FILE in ${INCFILES} )
    string(REGEX MATCH ${${version}} MATCHFOUND ${INC_FILE} )
#   message("matchfound is ${MATCHFOUND}, version is ${${version}} for ${INC_FILE}")
    if( MATCHFOUND ) 
      message("found ${INC_FILE}")
      string(REGEX REPLACE "${incName}_module.mod" "" INCPATH ${INC_FILE} )
      set( ${incFile} ${INCPATH} PARENT_SCOPE )
      return()
    endif()
  endforeach()
  file(GLOB_RECURSE INCFILES ${COREPATH}/${incName}_module.mod )
  list(LENGTH INCFILES numFiles)
  if(numFiles EQUAL 1) 
      get_filename_component( INCPATH ${INCFILES} DIRECTORY )
  else()  
    foreach( INC_FILE in ${INCFILES} )
      get_filename_component( INCPATH ${INC_FILE} DIRECTORY )
#     message("WARNING: Did not find explicit version ${${version}} of ${incName} module, using un-versioned path")
#     set( ${incFile} ${INCPATH} PARENT_SCOPE )
#     return()
    endforeach()
  endif()
  set( ${incFile} ${INCPATH} PARENT_SCOPE )
endfunction()

function (findOtherVersion rootPath srcName srcPath newVer )
  file(GLOB SRCDIRS ${${rootPath}}/${srcName}* )
  foreach( SRC_DIR in ${SRCDIRS} )
    string(REGEX MATCH ${srcName} MATCHFOUND ${SRC_DIR} )
    if( MATCHFOUND ) 
      set( ${srcPath} ${SRC_DIR} PARENT_SCOPE )
      string(REGEX MATCH "[0-9].[0-9].[0-9]" ALTVER ${SRC_DIR}  )
      message("Found ${ALTVER} of ${srcName}. Proceeding with Alternative")
      set( ${newVer} ${ALTVER} PARENT_SCOPE )
      return()
    endif()
  endforeach()
endfunction()

# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
if(DEFINED ENV{BACIO_VER})
  set(BACIO_VER $ENV{BACIO_VER})
  STRING(REGEX REPLACE "v" "" BACIO_VER ${BACIO_VER})
endif()
if(DEFINED ENV{BUFR_VER})
  set(BUFR_VER $ENV{BUFR_VER})
  STRING(REGEX REPLACE "v" "" BUFR_VER ${BUFR_VER})
endif()
if(DEFINED ENV{CRTM_VER})
  set(CRTM_VER $ENV{CRTM_VER})
  STRING(REGEX REPLACE "v" "" CRTM_VER ${CRTM_VER})
endif()
if(DEFINED ENV{NEMSIO_VER})
  set(NEMSIO_VER $ENV{NEMSIO_VER})
  STRING(REGEX REPLACE "v" "" NEMSIO_VER ${NEMSIO_VER})
endif()
if(DEFINED ENV{SFCIO_VER})
  set(SFCIO_VER $ENV{SFCIO_VER})
  STRING(REGEX REPLACE "v" "" SFCIO_VER ${SFCIO_VER})
endif()
if(DEFINED ENV{SIGIO_VER})
  set(SIGIO_VER $ENV{SIGIO_VER})
  STRING(REGEX REPLACE "v" "" SIGIO_VER ${SIGIO_VER})
endif()
if(DEFINED ENV{SP_VER})
  set(SP_VER $ENV{SP_VER})
  STRING(REGEX REPLACE "v" "" SP_VER ${SP_VER})
endif()
if(DEFINED ENV{W3EMC_VER})
  set(W3EMC_VER $ENV{W3EMC_VER})
  STRING(REGEX REPLACE "v" "" W3EMC_VER ${W3EMC_VER})
endif()
if(DEFINED ENV{W3NCO_VER})
  set(W3NCO_VER $ENV{W3NCO_VER})
  STRING(REGEX REPLACE "v" "" W3NCO_VER ${W3NCO_VER})
endif()

set (CORE_DEPS " ")
set( NO_DEFAULT_PATH )
if(NOT  BUILD_CRTM )
  if(DEFINED ENV{CRTM_LIB} )
    set(CRTM_LIBRARY $ENV{CRTM_LIB} )
    set(CRTMINC $ENV{CRTM_INC} )
    message("CRTM library ${CRTM_LIBRARY} set via Environment variable")
  else()
  findInc( crtm CRTM_VER CRTMINC )
  find_library( CRTM_LIBRARY 
    NAMES libcrtm_v${CRTM_VER}.a libCRTM.a libcrtm.a 
    HINTS 
      ${CRTM_BASE}
      ${CRTM_BASE}/${CRTM_VER}
      ${COREPATH}
      ${COREPATH}/lib
      /nwprod2/lib/crtm/v${CRTM_VER}
      $ENV{COREPATH} 
      $ENV{COREPATH}/lib 
      $ENV{COREPATH}/include 
      /usr/local/jcsda/nwprod_gdas_2014	
      ${CORECRTM}/crtm/${CRTM_VER}
    PATH_SUFFIXES
        lib
     ${NO_DEFAULT_PATH})
    set( crtm "crtm_v${CRTM_VER}")
    message("Found CRTM library ${CRTM_LIBRARY}")
  endif()
else()
    if( NOT DEFINED ENV{CRTM_SRC} )
      if( FIND_SRC ) 
        findSrc( "crtm" CRTM_VER CRTM_DIR )
      endif()
    else()
      set( CRTM_DIR "$ENV{CRTM_SRC}/libsrc" CACHE STRING "CRTM Source Location")
    endif()
    set( libsuffix "_v${CRTM_VER}${debug_suffix}" )
    set( CRTM_LIBRARY "${LIBRARY_OUTPUT_PATH}/libcrtm${libsuffix}.a" CACHE STRING "CRTM Library" )
    set(CRTMINC  "${CORECRTM}/crtm/${CRTM_VER}/incmod/crtm_v${CRTM_VER}")
    set( crtm "crtm${libsuffix}")
endif()
if(NOT  BUILD_EMC  )
  if(DEFINED ENV{W3EMC_LIBd} )
    set(W3EMC_LIBRARY $ENV{W3EMC_LIBd} )
    set(W3EMCINC $ENV{W3EMC_INCd} )
  else()
  find_path( W3EMCINC 
    NAMES mersenne_twister.mod 
    HINTS 
      $ENV{COREPATH}/lib/incmod/w3emc_4 
      $ENV{COREPATH}/include 
      /usr/local/jcsda/nwprod_gdas_2014/lib/incmod/w3emc_4 
      ${COREPATH}/w3emc/v${W3EMC_VER}/incmod/w3emc_v${W3EMC_VER}_d
  )
  find_library( W3EMC_LIBRARY 
    NAMES libw3emc_4.a libw3emc_i4r8.a libw3emc_v${W3EMC_VER}_d.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014	
      ${COREPATH}/w3emc/v${W3EMC_VER}
    PATH_SUFFIXES
        lib
    )
    message("Found W3EMC library ${W3EMC_LIBRARY}")
  endif()
else()
    set( libsuffix "_v${W3EMC_VER}${debug_suffix}" )
    set( W3EMC_LIBRARY "${LIBRARY_OUTPUT_PATH}/libw3emc${libsuffix}.a" CACHE STRING "W3EMC Library" )
    set( w3emc "w3emc${libsuffix}")
    if( DEFINED ENV{W3EMC_SRC} )
      set( W3EMC_DIR $ENV{W3EMC_SRC} CACHE STRING "W3EMC Source Directory" )
    else()
      if( FIND_SRC ) 
        findSrc( "w3emc" W3EMC_VER W3EMC_DIR )
      endif()
    endif()
endif()
if(NOT  BUILD_NCO )
  if(DEFINED ENV{W3NCO_LIBd} )
    set(W3NCO_LIBRARY $ENV{W3NCO_LIBd} )
  else()
  find_library( W3NCO_LIBRARY 
    NAMES libw3nco_v${W3NCO_VER}_d.a libw3nco_d.a  libw3nco_i4r8.a 
    HINTS 
       $ENV{COREPATH}/lib 
       /usr/local/jcsda/nwprod_gdas_2014	
       ${COREPATH}/w3nco/v${W3NCO_VER}
    PATH_SUFFIXES
        lib
     ${NO_DEFAULT_PATH})
    message("Found W3NCO library ${W3NCO_LIBRARY}")
  endif()
else()
    if( DEFINED ENV{W3NCO_SRC} )
      set( W3NCO_DIR $ENV{W3NCO_SRC} CACHE STRING "W3NCO Source Directory" )
    else()
      if( FIND_SRC ) 
        findSrc( "w3nco" W3NCO_VER W3NCO_DIR )
      endif()
    endif()
    set( libsuffix "_v${W3NCO_VER}${debug_suffix}" )
    set( W3NCO_LIBRARY "${LIBRARY_OUTPUT_PATH}/libw3nco${libsuffix}.a" CACHE STRING "W3NCO Library" )
    set( w3nco "w3nco${libsuffix}")
endif()
if(NOT  BUILD_BACIO  )
  if(DEFINED ENV{BACIO_LIB4})
    set(BACIO_LIBRARY $ENV{BACIO_LIB4} )
  else()
    find_library( BACIO_LIBRARY 
      NAMES libbacio.a libbacio_4.a libbacio_v${BACIO_VER}_4.a 
      HINTS $ENV{COREPATH}/lib /usr/local/jcsda/nwprod_gdas_2014	
          ${COREPATH}/bacio/v${BACIO_VER}
      PATH_SUFFIXES
        lib
       ${NO_DEFAULT_PATH}
      )
    message("Found BACIO library ${BACIO_LIBRARY}")
  endif()
else()
    if( DEFINED ENV{BACIO_SRC} )
      set( BACIO_DIR $ENV{BACIO_SRC} CACHE STRING "BACIO Source Directory" )
    else()
      if( FIND_SRC ) 
        findSrc( "bacio" BACIO_VER BACIO_DIR )
      endif()
    endif()
    set( libsuffix "_v${BACIO_VER}${debug_suffix}" )
    set( BACIO_LIBRARY "${LIBRARY_OUTPUT_PATH}/libbacio${libsuffix}.a" CACHE STRING "BACIO Library" )
    set( bacio "bacio${libsuffix}")
endif()
if(NOT  BUILD_BUFR  )
  if(DEFINED ENV{BUFR_LIBd} )
    set(BUFR_LIBRARY $ENV{BUFR_LIBd} )
  else()
  find_library( BUFR_LIBRARY 
    NAMES libbufr.a libbufr_d_64.a libbufr_i4r8.a libbufr_v${BUFR_VER}_d_64.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014	
      ${COREPATH}/bufr/v${BUFR_VER}
    PATH_SUFFIXES
        lib
     ${NO_DEFAULT_PATH})
    set( bufr "bufr_v${BUFR_VER}")
    message("Found BUFR library ${BUFR_LIBRARY}")
  endif()
else()
    if( DEFINED ENV{BUFR_SRC} )
      set( BUFR_DIR $ENV{BUFR_SRC} CACHE STRING "BUFR Source Directory" )
    else()
      if( FIND_SRC ) 
        findSrc( "bufr" BUFR_VER BUFR_DIR )
      endif()
    endif()
    set( libsuffix "_v${BUFR_VER}${debug_suffix}" )
    set( BUFR_LIBRARY "${LIBRARY_OUTPUT_PATH}/libbufr${libsuffix}.a" CACHE STRING "BUFR Library" )
    set( bufr "bufr${libsuffix}")
endif()
if(NOT  BUILD_SFCIO )
  if(DEFINED ENV{SFCIO_LIB4} )
    set(SFCIO_LIBRARY $ENV{SFCIO_LIB4} )
    set(SFCIOINC $ENV{SFCIO_INC4} )
  else()
  findInc( sfcio SFCIO_VER SFCIOINC )
  find_library( SFCIO_LIBRARY 
    NAMES libsfcio.a libsfcio_4.a libsfcio_i4r4.a libsfcio_v${SFCIO_VER}_4.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014	
      ${COREPATH}/sfcio/v${SFCIO_VER}
    PATH_SUFFIXES
        lib
       ${NO_DEFAULT_PATH})
    set( sfcio "sfcio_v${SFCIO_VER}_4")
    message("Found SFCIO library ${SFCIO_LIBRARY}")
  endif()
else()
    if( DEFINED ENV{SFCIO_SRC} )
      set( SFCIO_DIR $ENV{SFCIO_SRC} CACHE STRING "SFCIO Source Directory" )
    else()
      if( FIND_SRC ) 
        findSrc( "sfcio" SFCIO_VER SFCIO_DIR )
      endif()
    endif()
    set( libsuffix "_v${SFCIO_VER}${debug_suffix}" )
    set( SFCIO_LIBRARY "${LIBRARY_OUTPUT_PATH}/libsfcio${libsuffix}.a" CACHE STRING "SFCIO Library" )
    set( sfcio "sfcio${libsuffix}")
endif()
if(NOT  BUILD_SIGIO )
  if(DEFINED ENV{SIGIO_LIB4} )
    set(SIGIO_LIBRARY $ENV{SIGIO_LIB4} )
    set(SIGIOINC $ENV{SIGIO_INC4} )
  else()
  findInc( sigio SIGIO_VER SIGIOINC )
  message("SIGIOINC is ${SIGIOINC}")
  find_library( SIGIO_LIBRARY 
    NAMES libsigio.a libsigio_4.a libsigio_i4r4.a libsigio_v${SIGIO_VER}_4.a
    HINTS 
     $ENV{COREPATH}/lib 
     /usr/local/jcsda/nwprod_gdas_2014	
     ${COREPATH}/sigio/v${SIGIO_VER}
    PATH_SUFFIXES
        lib
       ${NO_DEFAULT_PATH})
    set( sigio "sigio_v${SIGIO_VER}_4")
    message("Found SIGIO library ${SIGIO_LIBRARY}")
  endif()
else()
    if( DEFINED ENV{SIGIO_SRC} )
      set( SIGIO_DIR $ENV{SIGIO_SRC} CACHE STRING "SIGIO Source Directory" )
    else()
      if( FIND_SRC ) 
        findSrc( "sigio" SIGIO_VER SIGIO_DIR )
      endif()
    endif()
    set( libsuffix "_v${SIGIO_VER}${debug_suffix}" )
    set( SIGIO_LIBRARY "${LIBRARY_OUTPUT_PATH}/libsigio${libsuffix}.a" CACHE STRING "SIGIO Library" )
    set( sigio "sigio${libsuffix}")
    set( CORE_DEPS "${CORE_DEPS} ${baseName}" )
endif()
if(NOT  BUILD_NEMSIO )
  if(DEFINED ENV{NEMSIO_LIB} )
    set(NEMSIO_LIBRARY $ENV{NEMSIO_LIB} )
    set(NEMSIOINC $ENV{NEMSIO_INC} )
  else()
  findInc( nemsio NEMSIO_VER NEMSIOINC )
  find_library( NEMSIO_LIBRARY 
    NAMES libnemsio.a libnemsio_v${NEMSIO_VER}.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014	
      ${COREPATH}/nemsio/v${NEMSIO_VER}
    PATH_SUFFIXES
        lib
       ${NO_DEFAULT_PATH})
    set( nemsio "nemsio_v${NEMSIO_VER}")
    message("Found NEMSIO library ${NEMSIO_LIBRARY}")
  endif()
else()
    if( DEFINED ENV{NEMSIO_SRC} )
      set( NEMSIO_DIR $ENV{NEMSIO_SRC} CACHE STRING "NEMSIO Source Directory" )
    else()
      if( FIND_SRC ) 
        findSrc( "nemsio" NEMSIO_VER NEMSIO_DIR )
      endif()
    endif()
    set( libsuffix "_v${NEMSIO_VER}${debug_suffix}" )
    set( NEMSIO_LIBRARY "${LIBRARY_OUTPUT_PATH}/libnemsio${libsuffix}.a" CACHE STRING "NEMSIO Library" )
    set( nemsio "nemsio${libsuffix}")
endif()
if(NOT  BUILD_SP )
  if(DEFINED ENV{SP_LIBd} )
    set(SP_LIBRARY $ENV{SP_LIBd} )
  else()
  find_library( SP_LIBRARY 
    NAMES libsp_d.a libsp_i4r8.a libsp_v${SP_VER}_d.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014	
      ${COREPATH}/sp/v${SP_VER}
    PATH_SUFFIXES
        lib
       ${NO_DEFAULT_PATH})
    set( sp "sp_v${SP_VER}_d")
    message("Found SP library ${SP_LIBRARY}")
  endif()
else()
    if( DEFINED ENV{SP_SRC} )
      set( SP_DIR $ENV{SP_SRC} CACHE STRING "SP Source Directory" )
    else() 
      if( FIND_SRC ) 
        findSrc( "sp" SP_VER SP_DIR )
      endif()
    endif()
    set( libsuffix "_v${SP_VER}${debug_suffix}" )
    set( SP_LIBRARY "${LIBRARY_OUTPUT_PATH}/libsp${libsuffix}.a" CACHE STRING "SP Library" )
    set( sp "sp${libsuffix}")
endif()

set( CORE_LIBRARIES ${CRTM_LIBRARY} ${SFCIO_LIBRARY} ${SIGIO_LIBRARY} 
                  ${NEMSIO_LIBRARY} ${SP_LIBRARY} ${W3NCO_LIBRARY} ${BUFR_LIBRARY}  
                  ${BACIO_LIBRARY} ${W3EMC_LIBRARY} )
set( CORE_INCS ${INCLUDE_OUTPUT_PATH} ${CRTMINC} ${SFCIOINC} ${SIGIOINC} ${NEMSIOINC} ${W3EMCINC}  )

set( CRTM_LIBRARY_PATH ${CRTM_LIBRARY} CACHE STRING "CRTM Library Location" )
set( CRTM_INCLUDE_PATH ${CRTMINC} CACHE STRING "CRTM Include Location" )

set( BUFR_LIBRARY_PATH ${BUFR_LIBRARY} CACHE STRING "BUFR Library Location" )

set( SFCIO_LIBRARY_PATH ${SFCIO_LIBRARY} CACHE STRING "SFCIO Library Location" )
set( SFCIO_INCLUDE_PATH ${SFCIOINC} CACHE STRING "SFCIO Include Location" )

set( SIGIO_LIBRARY_PATH ${SIGIO_LIBRARY} CACHE STRING "SIGIO Library Location" )
set( SIGIO_INCLUDE_PATH ${SIGIOINC} CACHE STRING "SIGIO Include Location" )

set( W3NCO_LIBRARY_PATH ${W3NCO_LIBRARY} CACHE STRING "W3NCO Library Location" )

set( W3EMC_LIBRARY_PATH ${W3EMC_LIBRARY} CACHE STRING "W3EMC Library Location" )
set( W3EMC_INCLUDE_PATH ${W3EMCINC} CACHE STRING "W3EMC Include Location" )

set( BACIO_LIBRARY_PATH ${BACIO_LIBRARY} CACHE STRING "BACIO Library Location" )

set( NEMSIO_LIBRARY_PATH ${NEMSIO_LIBRARY} CACHE STRING "NEMSIO Library Location" )
set( NEMSIO_INCLUDE_PATH ${NEMSIOINC} CACHE STRING "NEMSIO Include Location" )

set( SP_LIBRARY_PATH ${SP_LIBRARY} CACHE STRING "SP Library Location" )

