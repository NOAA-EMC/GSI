if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -convert big_endian -assume byterecl")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0")
endif()

add_executable(controlpreproc.x controlpreproc.f90)
add_executable(ensincpreproc.x ensincpreproc.f90)
add_executable(enspreproc.x enspreproc.f90)

list(APPEND _targets
  controlpreproc.x
  ensincpreproc.x
  enspreproc.x)

foreach(_tgt ${_targets})
  target_link_libraries(${_tgt} PRIVATE sp::sp_4)
  target_link_libraries(${_tgt} PRIVATE sigio::sigio)
  if(OpenMP_Fortran_FOUND)
     target_link_libraries(${_tgt} PRIVATE OpenMP::OpenMP_Fortran)
   endif()
endforeach()

target_link_libraries(ensincpreproc.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(enspreproc.x PRIVATE MPI::MPI_Fortran)

# Install executable targets
install(TARGETS ${_targets} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

