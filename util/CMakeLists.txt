cmake_minimum_required(VERSION 3.15)

project(gsiutils
        VERSION 1.0.0
        LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
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

# Build options
option(BUILD_AERODA_UTIL "Build Aerosol DA utilities" OFF)
option(BUILD_COV_CALC "Build Correlated Observation Error Utility" OFF)
option(BUILD_EFSOI_UTIL "Build EFSOI Utilities" OFF)
option(BUILD_ENKF_GFS "Build EnKF GFS Utilities" OFF)
option(BUILD_ENKF_ARW "Build EnKF ARW Utilities" OFF)
option(BUILD_MON_UTIL "Build Monitoring Utilities" OFF)
option(BUILD_NCIO_UTIL "Build NetCDF IO Utilities" OFF)
option(BUILD_UTIL_COM "Build community utilities" OFF)

# Echo user options
message(STATUS "Utils: OPENMP ................. ${OPENMP}")
message(STATUS "Utils: ENABLE_MKL ............. ${ENABLE_MKL}")
message(STATUS "Utils: BUILD_AERODA_UTIL ...... ${BUILD_AERODA_UTIL}")
message(STATUS "Utils: BUILD_COV_CALC ......... ${BUILD_COV_CALC}")
message(STATUS "Utils: BUILD_EFSOI_UTIL ....... ${BUILD_EFSOI_UTIL}")
message(STATUS "Utils: BUILD_ENKF_GFS ......... ${BUILD_ENKF_GFS}")
message(STATUS "Utils: BUILD_ENKF_ARW ......... ${BUILD_ENKF_ARW}")
message(STATUS "Utils: BUILD_MON_UTIL ......... ${BUILD_MON_UTIL}")
message(STATUS "Utils: BUILD_NCIO_UTIL ........ ${BUILD_NCIO_UTIL}")
message(STATUS "Utils: BUILD_UTIL_COM ......... ${BUILD_UTIL_COM}")

# Dependencies
if(ENABLE_MKL)
  find_package(MKL QUIET)
endif()
if(MKL_FOUND)
  set(LAPACK_LIBRARIES ${MKL_LIBRARIES})
else()
  set(ENABLE_MKL OFF CACHE INTERNAL "GSI Utils: Disable MKL since it was NOT FOUND")
  find_package(LAPACK REQUIRED)
endif()
find_package(MPI REQUIRED)
find_package(NetCDF REQUIRED Fortran)
if(OPENMP)
  find_package(OpenMP REQUIRED)
endif()

if(NOT TARGET ncdiag)
  find_package(ncdiag QUIET)
endif()

if(TARGET gsi)
  get_target_property(GSI_MODE gsi "MODE")
else()
  find_package(gsi QUIET)
endif()

if(TARGET enkf)
  get_target_property(ENKF_MODE enkf "MODE")
else()
  find_package(enkf QUIET)
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
find_package(bufr REQUIRED)
find_package(wrf_io QUIET)

# See https://github.com/NOAA-EMC/NCEPLIBS-nemsio/pull/22
target_link_libraries(nemsio::nemsio INTERFACE w3emc::w3emc_d bacio::bacio_4)

# Get compiler flags for the utilities
include(gsiutils_compiler_flags)

if(BUILD_AERODA_UTIL)
  add_subdirectory(AeroDA)
endif()

if(BUILD_COV_CALC)
  add_subdirectory(Correlated_Obs)
endif()

if(BUILD_EFSOI_UTIL)
  add_subdirectory(EFSOI_Utilities)
endif()

if(BUILD_ENKF_GFS OR BUILD_ENKF_ARW)
  add_subdirectory(EnKF)
endif()

if(BUILD_NCIO_UTIL)
  add_subdirectory(netcdf_io)
endif()

if(BUILD_UTIL_COM)
  add_subdirectory(Analysis_Utilities)
  add_subdirectory(bufr_tools)
  add_subdirectory(radar_process)
endif()

add_subdirectory(ndate)
add_subdirectory(zero_biascoeff)

if(BUILD_MON_UTIL)
  message(FATAL_ERROR "CMake in the Monitoring utilities have not been updated")
#  add_subdirectory(Conventional_Monitor)
#  add_subdirectory(Ozone_Monitor)
#  add_subdirectory(Radiance_Monitor)
endif()

# These utilities do not have CMake builds
# Some are used occasionally e.g. NMC_Bkerror
# Some are really really old and likely unusable today e.g. Config
#add_subdirectory(Config)
  #add_subdirectory(FOV_utilities)
#add_subdirectory(GEN_BE_V2.0)
#add_subdirectory(global_angupdate)
#add_subdirectory(GMI_BUFR_gen)
#add_subdirectory(Misc)
#add_subdirectory(MODIS_AOD)
#add_subdirectory(NCEP_bkerror)
#add_subdirectory(NCEPgsi_Coupler)
  #add_subdirectory(NMC_Bkerror)
#add_subdirectory(Radiance_bias_correction_Utilities)
#add_subdirectory(Radiance_Utilities)
  #add_subdirectory(Single_Observation)
