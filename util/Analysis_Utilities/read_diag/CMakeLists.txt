if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/read_diag")
add_library(kinds_obj OBJECT kinds.F90)
set_target_properties(kinds_obj PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
target_include_directories(kinds_obj INTERFACE
  $<BUILD_INTERFACE:${module_dir}>)

add_executable(read_diag_conv.x read_diag_conv.f90)
add_executable(read_diag_rad.x  read_diag_rad.f90)

list(APPEND _targets
  read_diag_conv.x
  read_diag_rad.x)

foreach(_tgt ${_targets})
  add_dependencies(${_tgt} kinds_obj)
  target_compile_definitions(${_tgt} PRIVATE "_REAL8_")
  target_include_directories(${_tgt} PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include/read_diag>)
endforeach()

# Install executable targets
install(TARGETS ${_targets} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
