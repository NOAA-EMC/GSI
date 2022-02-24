cmake_minimum_required(VERSION 3.15)

project(gsi
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
option(ENABLE_MKL "Use MKL for LAPACK implementation (if available)" ON)
option(USE_NCDIAG "Use NetCDF diagnostics library" ON)
option(BUILD_GSI_REGIONAL "Build Regional GSI" OFF)

# Echo user options
message(STATUS "OPENMP ................. ${OPENMP}")
message(STATUS "ENABLE_MKL ............. ${ENABLE_MKL}")
message(STATUS "USE_NCDIAG ............. ${USE_NCDIAG}")
message(STATUS "BUILD_GSI_REGIONAL ..... ${BUILD_GSI_REGIONAL}")

# Dependencies
if(ENABLE_MKL)
  find_package(MKL QUIET)
endif()
if(MKL_FOUND)
  set(LAPACK_LIBRARIES ${MKL_LIBRARIES})
else()
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

if(BUILD_GSI_REGIONAL)
  find_package(wrf_io REQUIRED)
endif()

# NetCDF Diagnostic IO library dependency
if(USE_NCDIAG)
  if(NOT TARGET ncdiag::ncdiag)
    find_package(ncdiag REQUIRED)
  endif()
endif()

# See https://github.com/NOAA-EMC/NCEPLIBS-nemsio/pull/22
target_link_libraries(nemsio::nemsio INTERFACE w3emc::w3emc_d bacio::bacio_4)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# Get compiler flags for the GSI application
include(gsiapp_compiler_flags)

# Get the list of all source files
include(gsi_files.cmake)

# Include GSI ncdiag files if NetCDF Diagnostics is disabled
if(NOT USE_NCDIAG)
  list(APPEND GSI_SRC_Fortran
    ${GSI_SRC_ncdiag}
  )
endif()

# Collect files specific for each application
# Global GSI application
list(APPEND GSI_SRC_Fortran_global
  ${GSI_SRC_Fortran}
  ${GSI_SRC_class}
  ${GSI_SRC_stub}
  ${GSI_SRC_fixture_gfs}
)

# Regional GSI application
list(APPEND GSI_SRC_Fortran_regional
  ${GSI_SRC_Fortran}
  ${GSI_SRC_class}
  ${GSI_SRC_cplr}
  ${GSI_SRC_fixture_regional}
)

# Compiler options and definitions
list(APPEND GSI_C_DEFS
  FortranByte=char
  FortranInt=int
  funder
  "FortranLlong=long long"
)

# Create object libraries for various components
# GSI_SRC_C files
add_library(gsi_c_obj OBJECT ${GSI_SRC_C})
target_compile_definitions(gsi_c_obj PUBLIC ${GSI_C_DEFS})

# Create a library of GLOBAL GSI Fortran Objects
set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/global_gsi")
add_library(global_gsi_fortran_obj OBJECT ${GSI_SRC_Fortran_global})
set_target_properties(global_gsi_fortran_obj PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
target_include_directories(global_gsi_fortran_obj INTERFACE
  $<BUILD_INTERFACE:${module_dir}>
  $<INSTALL_INTERFACE:include/global_gsi>)
list(APPEND GSI_LIB_Fortran_Targets
  global_gsi_fortran_obj
)

# Create a library of REGIONAL GSI Fortran objects
if(BUILD_GSI_REGIONAL)
  set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/regional_gsi")
  add_library(regional_gsi_fortran_obj OBJECT ${GSI_SRC_Fortran_regional})
  set_target_properties(regional_gsi_fortran_obj PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
  target_include_directories(regional_gsi_fortran_obj INTERFACE
    $<BUILD_INTERFACE:${module_dir}>
    $<INSTALL_INTERFACE:include/regional_gsi>)
  list(APPEND GSI_LIB_Fortran_Targets
    regional_gsi_fortran_obj
  )
endif()

# Link the Fortran targets with dependencies
foreach(lib ${GSI_LIB_Fortran_Targets})
  target_compile_definitions(${lib} PUBLIC "_REAL8_")
  target_link_libraries(${lib} PUBLIC bacio::bacio_4)
  target_link_libraries(${lib} PUBLIC sigio::sigio)
  target_link_libraries(${lib} PUBLIC sfcio::sfcio)
  target_link_libraries(${lib} PUBLIC nemsio::nemsio)
  target_link_libraries(${lib} PUBLIC ncio::ncio)
  target_link_libraries(${lib} PUBLIC w3emc::w3emc_d)
  target_link_libraries(${lib} PUBLIC crtm::crtm)
  target_link_libraries(${lib} PUBLIC NetCDF::NetCDF_Fortran)
  target_link_libraries(${lib} PUBLIC MPI::MPI_Fortran)
  if(OpenMP_Fortran_FOUND)
    target_link_libraries(${lib} PRIVATE OpenMP::OpenMP_Fortran)
  endif()
  if(USE_NCDIAG)
    if(TARGET ncdiag::ncdiag)
      add_dependencies(${lib} ncdiag::ncdiag)
      add_library(ncdiag::ncdiag ALIAS ncdiag)
    endif()
    target_link_libraries(${lib} PUBLIC ncdiag::ncdiag)
  endif()
endforeach()

# Create a GLOBAL GSI library
add_library(global_gsi STATIC $<TARGET_OBJECTS:gsi_c_obj>
                              $<TARGET_OBJECTS:global_gsi_fortran_obj>)
add_library(${PROJECT_NAME}::global_gsi ALIAS global_gsi)
target_include_directories(global_gsi PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include/global_gsi>
                                             $<INSTALL_INTERFACE:include/global_gsi>)
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/global_gsi DESTINATION ${CMAKE_INSTALL_PREFIX}/include)
list(APPEND GSI_LIB_Targets global_gsi)

# Create a REGIONAL GSI library
if(BUILD_GSI_REGIONAL)
  add_library(regional_gsi STATIC $<TARGET_OBJECTS:gsi_c_obj>
                                  $<TARGET_OBJECTS:regional_gsi_fortran_obj>)
  add_library(${PROJECT_NAME}::regional_gsi ALIAS regional_gsi)
  target_include_directories(regional_gsi PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include/regional_gsi>
                                                 $<INSTALL_INTERFACE:include/regional_gsi>)
  install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/regional_gsi DESTINATION ${CMAKE_INSTALL_PREFIX}/include)
  list(APPEND GSI_LIB_Targets regional_gsi)
endif()

# Create a GLOBAL GSI executable
set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/global_gsi")
add_executable(global_gsi.x ${GSI_SRC_main})
add_dependencies(global_gsi.x global_gsi)
set_target_properties(global_gsi.x PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
target_link_libraries(global_gsi.x PRIVATE global_gsi)
list(APPEND GSI_APP_Targets
  global_gsi.x
)

# Create a REGIONAL GSI executable
if(BUILD_GSI_REGIONAL)
  set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/regional_gsi")
  add_executable(regional_gsi.x ${GSI_SRC_main})
  add_dependencies(regional_gsi.x regional_gsi)
  set_target_properties(regional_gsi.x PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
  target_link_libraries(regional_gsi.x PRIVATE regional_gsi)
  list(APPEND GSI_APP_Targets
    regional_gsi.x
  )
endif()

# Link the application targets with dependencies
foreach(app ${GSI_APP_Targets})
  target_link_libraries(${app} PUBLIC bacio::bacio_4)
  target_link_libraries(${app} PUBLIC sigio::sigio)
  target_link_libraries(${app} PUBLIC sfcio::sfcio)
  target_link_libraries(${app} PUBLIC nemsio::nemsio)
  target_link_libraries(${app} PUBLIC ncio::ncio)
  target_link_libraries(${app} PUBLIC w3emc::w3emc_d)
  target_link_libraries(${app} PUBLIC sp::sp_d)
  target_link_libraries(${app} PUBLIC bufr::bufr_d)
  target_link_libraries(${app} PUBLIC crtm::crtm)
  if(USE_NCDIAG)
    target_link_libraries(${app} PUBLIC ncdiag::ncdiag)
  endif()
  target_link_libraries(${app} PUBLIC NetCDF::NetCDF_Fortran)
  target_link_libraries(${app} PUBLIC MPI::MPI_Fortran)
  target_link_libraries(${app} PUBLIC ${LAPACK_LIBRARIES})
  if(OpenMP_Fortran_FOUND)
    target_link_libraries(${app} PRIVATE OpenMP::OpenMP_Fortran)
  endif()
endforeach()

# REGIONAL GSI Application specific library dependencies
if(BUILD_GSI_REGIONAL)
  target_link_libraries(regional_gsi.x PUBLIC wrf_io::wrf_io)
endif()

# Install executable targets
install(TARGETS ${GSI_APP_Targets} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# Install library archive targets
install(
  TARGETS ${GSI_LIB_Targets}
  EXPORT ${PROJECT_NAME}Exports
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

### Package config
include(CMakePackageConfigHelpers)
set(CONFIG_INSTALL_DESTINATION lib/cmake/${PROJECT_NAME})

export(EXPORT ${PROJECT_NAME}Exports
  NAMESPACE ${PROJECT_NAME}::
  FILE ${PROJECT_NAME}-targets.cmake)

configure_package_config_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/PackageConfig.cmake.in ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
  INSTALL_DESTINATION ${CONFIG_INSTALL_DESTINATION})
install(FILES ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})

write_basic_package_version_file(
  ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion)
install(FILES ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})

install(EXPORT ${PROJECT_NAME}Exports
  NAMESPACE ${PROJECT_NAME}::
  FILE ${PROJECT_NAME}-targets.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})
