list(APPEND SRCS
calc_fov_conical.f90
calc_fov_crosstrk.f90
constants.f90
driver.f90
kinds.F90)

add_executable(fov_util.x ${SRCS})
target_compile_definitions(fov_util.x PRIVATE "_REAL8_")

# Install executable targets
install(TARGETS fov_util.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

