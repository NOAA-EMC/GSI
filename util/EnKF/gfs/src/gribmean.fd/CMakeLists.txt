if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

add_executable(gribmean.x gribmean.f90)
target_link_libraries(gribmean.x PRIVATE bacio::bacio_4)
target_link_libraries(gribmean.x PRIVATE w3emc::w3emc_d)

# Install executable targets
install(TARGETS gribmean.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

