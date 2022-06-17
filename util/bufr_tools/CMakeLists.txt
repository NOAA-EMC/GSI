if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

add_executable(bufr_append_sample.x bufr_append_sample.f90)
add_executable(bufr_decode_sample.x bufr_decode_sample.f90)
add_executable(bufr_encode_sample.x bufr_encode_sample.f90)
add_executable(bufr_decode_radiance.x bufr_decode_radiance.f90)
add_executable(prepbufr_decode_all.x prepbufr_decode_all.f90)
add_executable(prepbufr_encode_surface.x prepbufr_encode_surface.f90)
add_executable(prepbufr_encode_upperair.x prepbufr_encode_upperair.f90)
add_executable(prepbufr_inventory.x prepbufr_inventory.f90)
add_executable(prepbufr_append_retrieve.x prepbufr_append_retrieve.f90)
add_executable(prepbufr_append_surface.x prepbufr_append_surface.f90)
add_executable(prepbufr_append_upperair.x prepbufr_append_upperair.f90)

list(APPEND _targets
  bufr_append_sample.x
  bufr_decode_sample.x
  bufr_encode_sample.x
  bufr_decode_radiance.x
  prepbufr_decode_all.x
  prepbufr_encode_surface.x
  prepbufr_encode_upperair.x
  prepbufr_inventory.x
  prepbufr_append_retrieve.x
  prepbufr_append_surface.x
  prepbufr_append_upperair.x)

foreach(_tgt ${_targets})
  target_link_libraries(${_tgt} PRIVATE bufr::bufr_d)
endforeach()

install(TARGETS ${_targets} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
