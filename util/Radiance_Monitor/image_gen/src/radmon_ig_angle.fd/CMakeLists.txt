if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -assume byterecl -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

list(APPEND SRCS
  angle.f90
  avgsdv.f90
)

add_executable(radmon_ig_angle.x ${SRCS})

target_compile_definitions(radmon_ig_angle.x PRIVATE "_REAL8_")
target_link_libraries(radmon_ig_angle.x PRIVATE w3emc::w3emc_4)

# Install executable targets
install(TARGETS radmon_ig_angle.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

