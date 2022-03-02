if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

add_executable(interp_inc.x driver.f90)
set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/interp_inc")
set_target_properties(interp_inc.x PROPERTIES Fortran_MODULE_DIRECTORY ${module_dir})
target_include_directories(interp_inc.x INTERFACE $<BUILD_INTERFACE:${module_dir}>)
target_link_libraries(interp_inc.x PRIVATE NetCDF::NetCDF_Fortran)
target_link_libraries(interp_inc.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(interp_inc.x PRIVATE w3emc::w3emc_d)
target_link_libraries(interp_inc.x PRIVATE sp::sp_d)
target_link_libraries(interp_inc.x PRIVATE ip::ip_d)

# Install executable targets
install(TARGETS interp_inc.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

