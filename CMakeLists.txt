cmake_minimum_required(VERSION 3.15)

project(GSI
        VERSION 1.0.0
        LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
set(CMAKE_DIRECTORY_LABELS ${PROJECT_NAME})

include(compiler_flags)
include(GNUInstallDirs)

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel|Production)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo" "Production")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(GNU|Intel|Clang|AppleClang)$")
  message(WARNING "${CMAKE_C_COMPILER_ID} is not supported.")
endif()

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Intel)$")
  message(WARNING "${CMAKE_Fortran_COMPILER_ID} is not supported.")
endif()

# User options
option(OPENMP "Enable OpenMP Threading" OFF)
option(BUILD_NCDIAG "Build and use inline NetCDF diagnostics library" ON)
option(BUILD_REGIONAL "Build Regional GSI" OFF)

# Echo user options
message(STATUS "OPENMP ................. ${OPENMP}")
message(STATUS "BUILD_NCDIAG ........... ${BUILD_NCDIAG}")
message(STATUS "BUILD_REGIONAL ......... ${BUILD_REGIONAL}")

# Dependencies
find_package(MPI REQUIRED)
find_package(NetCDF REQUIRED Fortran)
find_package(LAPACK REQUIRED)
if(OPENMP)
  find_package(OpenMP REQUIRED)
endif()

# NetCDF Diagnostic IO library dependency
if(NOT BUILD_NCDIAG)
  find_package(ncdiag REQUIRED)
endif()

# NCEPLibs dependencies
find_package(bacio REQUIRED)
find_package(sigio REQUIRED)
find_package(sfcio REQUIRED)
find_package(nemsio REQUIRED)
find_package(ncio REQUIRED)
find_package(sp REQUIRED)
find_package(ip REQUIRED)
find_package(w3emc REQUIRED)
find_package(w3nco REQUIRED)
find_package(bufr REQUIRED)
find_package(crtm REQUIRED)

if(BUILD_REGIONAL)
  find_package(wrf_io REQUIRED)
endif()

# See https://github.com/NOAA-EMC/NCEPLIBS-nemsio/pull/22
target_link_libraries(nemsio::nemsio INTERFACE w3emc::w3emc_d bacio::bacio_4)

# Build components
add_subdirectory(src)
#add_subdirectory(util)
#add_subdirectory(unit-tests)
#add_subdirectory(regression)
