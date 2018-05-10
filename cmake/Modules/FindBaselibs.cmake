if (NOT BASEDIR)
  message (FATAL_ERROR "ERROR: Must specify a value for BASEDIR with cmake ... -DBASEDIR=<path>.")
endif ()
if (ESMA_SDF)
  message (FATAL_ERROR "ERROR: -hdf option was thought to be obsolete when CMake was crafted.")
endif ()

link_directories (${BASEDIR}/lib)

#------------------------------------------------------------------
# netcdf
# The following command provides the list of libraries that netcdf
# uses.  Unfortunately it also includes the library path and "-l"
# prefixes, which CMake handles in a different manner. So we need so
# strip off that item from the list
execute_process (
  COMMAND ${BASEDIR}/bin/nf-config --flibs
  OUTPUT_VARIABLE LIB_NETCDF
  )

string(REGEX MATCHALL " -l[^ ]*" _full_libs "${LIB_NETCDF}")
set (NETCDF_LIBRARIES)
foreach (lib ${_full_libs})
  string (REPLACE "-l" "" _tmp ${lib})
  string (STRIP ${_tmp} _tmp)
  list (APPEND NETCDF_LIBRARIES ${_tmp})
endforeach()
#------------------------------------------------------------------

list(APPEND NETCDF_INCLUDES ${BASEDIR}/include/netcdf)
list(APPEND NETCDF_INCLUDES ${BASEDIR}/include/hdf5)

message(STATUS "NETCDF_INCLUDES: ${NETCDF_INCLUDES}")
message(STATUS "NETCDF_LIBRARIES: ${NETCDF_LIBRARIES}")

