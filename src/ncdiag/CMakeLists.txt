cmake_minimum_required(VERSION 3.19)

project(ncdiag
        VERSION 1.0.0
        LANGUAGES Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(${PROJECT_NAME}_compiler_flags)
include(GNUInstallDirs)

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Intel)$")
  message(WARNING "${CMAKE_Fortran_COMPILER_ID} is not supported.")
endif()

option(ENABLE_NCDIAG_SERIAL "Enable Serial NetCDF diagnostics" ON)

# Dependencies
find_package(MPI REQUIRED)
find_package(NetCDF REQUIRED COMPONENTS Fortran)

# Echo user options
message(STATUS "ENABLE_NCDIAG_SERIAL ... ${ENABLE_NCDIAG_SERIAL}")

# Get list of files
include(ncdiag_files.cmake)

set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/ncdiag")
add_library(ncdiag STATIC ${NCDIAG_SRC_lib})
add_library(${PROJECT_NAME}::ncdiag ALIAS ncdiag)
set_target_properties(ncdiag PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
target_compile_definitions(ncdiag PRIVATE "_REAL4_")
target_compile_definitions(ncdiag PRIVATE "USE_MPI")
target_link_libraries(ncdiag PUBLIC NetCDF::NetCDF_Fortran)
target_link_libraries(ncdiag PUBLIC MPI::MPI_Fortran)
target_include_directories(ncdiag PUBLIC $<BUILD_INTERFACE:${module_dir}>
                                         $<INSTALL_INTERFACE:include/ncdiag>)
list(APPEND NCDIAG_Targets ncdiag)
install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX}/include)

# Build applications
foreach(app ${NCDIAG_SRC_app})
  get_filename_component(app_name ${app} NAME_WLE)
  add_executable(${app_name}.x ${app})
  add_dependencies(${app_name}.x ncdiag)
  target_compile_definitions(${app_name}.x PRIVATE "USE_MPI")
  target_link_libraries(${app_name}.x PUBLIC ncdiag)
  target_link_libraries(${app_name}.x PUBLIC NetCDF::NetCDF_Fortran)
  target_link_libraries(${app_name}.x PUBLIC MPI::MPI_Fortran)
  list(APPEND NCDIAG_Targets ${app_name}.x)
endforeach()

# Build serial library and applications
if(ENABLE_NCDIAG_SERIAL)
  set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/ncdiag_serial")
  add_library(ncdiag_serial STATIC ${NCDIAG_SRC_lib})
  add_library(${PROJECT_NAME}::ncdiag_serial ALIAS ncdiag_serial)
  set_target_properties(ncdiag_serial PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
  target_compile_definitions(ncdiag_serial PRIVATE "_REAL4_")
  target_link_libraries(ncdiag_serial PUBLIC NetCDF::NetCDF_Fortran)
  target_include_directories(ncdiag_serial PUBLIC $<BUILD_INTERFACE:${module_dir}>
                                                  $<INSTALL_INTERFACE:include/ncdiag_serial>)
  list(APPEND NCDIAG_Targets ncdiag_serial)

  set(app_name "ncdiag_cat_serial")
  add_executable(${app_name}.x nc_diag_cat.F90)
  add_dependencies(${app_name}.x ncdiag_serial)
  target_link_libraries(${app_name}.x PUBLIC ncdiag_serial)
  target_link_libraries(${app_name}.x PUBLIC NetCDF::NetCDF_Fortran)
  list(APPEND NCDIAG_Targets ${app_name}.x)
  install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX}/include)
endif()

install(
  TARGETS ${NCDIAG_Targets}
  EXPORT ${PROJECT_NAME}Exports
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

# Package config
include(CMakePackageConfigHelpers)
set(CONFIG_INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})

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
