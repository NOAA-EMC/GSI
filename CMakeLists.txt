cmake_minimum_required(VERSION 3.15)

project(GSIApp
        VERSION 1.0.0
        LANGUAGES C Fortran)

set(CMAKE_DIRECTORY_LABELS ${PROJECT_NAME})

include(GNUInstallDirs)

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(GNU|Intel|Clang|AppleClang)$")
  message(WARNING "${CMAKE_C_COMPILER_ID} is not supported.")
endif()

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Intel)$")
  message(WARNING "${CMAKE_Fortran_COMPILER_ID} is not supported.")
endif()

# User options
option(OPENMP "Enable OpenMP Threading" OFF)
option(ENABLE_MKL "Use MKL for LAPACK implementation (if available)" ON)
option(BUILD_NCDIAG "Build GSI NetCDF Diagnostic Library" ON)
option(BUILD_GSDCLOUD "Build GSD Cloud Analysis Library" OFF)
option(BUILD_GSI "Build GSI" ON)
option(BUILD_ENKF "Build EnKF" ON)
option(BUILD_UTIL "Build Utilities" ON)

# Echo user options
message(STATUS "OPENMP ................. ${OPENMP}")
message(STATUS "ENABLE_MKL ............. ${ENABLE_MKL}")
message(STATUS "BUILD_NCDIAG ........... ${BUILD_NCDIAG}")
message(STATUS "BUILD_GSDCLOUD ......... ${BUILD_GSDCLOUD}")
message(STATUS "BUILD_GSI .............. ${BUILD_GSI}")
message(STATUS "BUILD_ENKF ............. ${BUILD_ENKF}")
message(STATUS "BUILD_UTIL ............. ${BUILD_UTIL}")

# Build components
add_subdirectory(src)

if(BUILD_UTIL)
  add_subdirectory(util)
endif()

#add_subdirectory(unit-tests)
#add_subdirectory(regression)
