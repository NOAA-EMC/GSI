list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# Get compiler flags for the GSI application
include(gsiapp_compiler_flags)

# Get the list of all source files
include(gsi_files.cmake)

# Include GSI ncdiag files if NetCDF Diagnostics is disabled
if(NOT BUILD_NCDIAG)
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
if(BUILD_REGIONAL)
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
  if(BUILD_NCDIAG)
    add_dependencies(${lib} ncdiag)
  endif()
  target_link_libraries(${lib} PUBLIC ncdiag::ncdiag)
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
if(BUILD_REGIONAL)
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
if(BUILD_REGIONAL)
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
#  target_link_libraries(${app} PUBLIC w3nco::w3nco_d)
  target_link_libraries(${app} PUBLIC sp::sp_d)
  target_link_libraries(${app} PUBLIC bufr::bufr_d)
  target_link_libraries(${app} PUBLIC crtm::crtm)
  target_link_libraries(${app} PUBLIC ncdiag::ncdiag)
  target_link_libraries(${app} PUBLIC NetCDF::NetCDF_Fortran)
  target_link_libraries(${app} PUBLIC MPI::MPI_Fortran)
  target_link_libraries(${app} PUBLIC ${LAPACK_LIBRARIES})
  if(OpenMP_Fortran_FOUND)
    target_link_libraries(${app} PRIVATE OpenMP::OpenMP_Fortran)
  endif()
endforeach()

# REGIONAL GSI Application specific library dependencies
if(BUILD_REGIONAL)
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
