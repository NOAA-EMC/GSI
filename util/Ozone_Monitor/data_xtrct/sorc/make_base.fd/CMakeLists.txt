if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -assume byterecl -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

list(APPEND SRCS
  make_base.f90
)

add_executable(oznmon_make_base.x ${SRCS})
target_link_libraries(oznmon_make_base.x PRIVATE w3emc::w3emc_4)

# Install executable targets
install(TARGETS oznmon_make_base.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

