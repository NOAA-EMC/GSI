if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

add_executable(bufr_decode_l2rwbufr.x bufr_decode_l2rwbufr.f90)
add_executable(bufr_encode_l2rwbufr.x bufr_encode_l2rwbufr.f90)
add_executable(bufr_encode_radarbufr.x bufr_encode_radarbufr.f90)

list(APPEND _targets
  bufr_decode_l2rwbufr.x
  bufr_encode_l2rwbufr.x
  bufr_encode_radarbufr.x)

foreach(_tgt ${_targets})
  target_link_libraries(${_tgt} PRIVATE bufr::bufr_d)
endforeach()

install(TARGETS ${_targets} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
