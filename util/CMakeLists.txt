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
option(USE_NCDIAG "Use NetCDF diagnostics library" ON)

set(GSI_APP "GFS" CACHE STRING "Choose the GSI Application.")
set_property(CACHE GSI_APP PROPERTY STRINGS "GFS" "Regional")

set(ENKF_APP "GFS" CACHE STRING "Choose the GSI Application.")
set_property(CACHE ENKF_APP PROPERTY STRINGS "GFS" "Regional")

# Echo user options
message(STATUS "GSI Utils: OPENMP ................. ${OPENMP}")
message(STATUS "GSI Utils: ENABLE_MKL ............. ${ENABLE_MKL}")
message(STATUS "GSI Utils: USE_NCDIAG ............. ${USE_NCDIAG}")
message(STATUS "GSI Utils: GSI_APP ................ ${GSI_APP}")
message(STATUS "GSI Utils: ENKF_APP ............... ${ENKF_APP}")

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
find_package(crtm REQUIRED)
if(GSI_APP MATCHES "Regional")
  find_package(wrf_io REQUIRED)
endif()

# See https://github.com/NOAA-EMC/NCEPLIBS-nemsio/pull/22
target_link_libraries(nemsio::nemsio INTERFACE w3emc::w3emc_d bacio::bacio_4)

# Get compiler flags for the utilities
include(gsiutils_compiler_flags)

add_subdirectory(AeroDA)
add_subdirectory(Analysis_Utilities)
add_subdirectory(bufr_tools)
#add_subdirectory(Conventional_Monitor)
add_subdirectory(Correlated_Obs)
add_subdirectory(EFSOI_Utilities)
add_subdirectory(EnKF)
add_subdirectory(ndate)
add_subdirectory(netcdf_io)
#add_subdirectory(Ozone_Monitor)
add_subdirectory(radar_process)
#add_subdirectory(Radiance_Monitor)
add_subdirectory(zero_biascoeff)

# These utilities do not have CMake builds
# Some are used occasionally e.g. NMC_Bkerror
# Some are really really old and likely unusable today e.g. Config
#add_subdirectory(Config)
#add_subdirectory(FOV_utilities)
#add_subdirectory(GEN_BE_V2.0)
#add_subdirectory(global_angupdate)
#add_subdirectory(GMI_BUFR_gen)
#add_subdirectory(Minimization_Monitor)
#add_subdirectory(Misc)
#add_subdirectory(MODIS_AOD)
#add_subdirectory(NCEP_bkerror)
#add_subdirectory(NCEPgsi_Coupler)
#add_subdirectory(NMC_Bkerror)
#add_subdirectory(Radiance_bias_correction_Utilities)
#add_subdirectory(Radiance_Utilities)
#add_subdirectory(Single_Observation)
