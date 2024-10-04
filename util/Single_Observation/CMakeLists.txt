if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -i4 -r8")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-real-8")
endif()

add_executable(makeoneobbufr.x makeoneobbufr.f90)
target_link_libraries(makeoneobbufr.x PRIVATE bufr::bufr_d)

# Install executable targets
install(TARGETS makeoneobbufr.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

