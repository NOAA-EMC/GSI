# Find the GSI and EnKF control executables
find_program(GSICONTROLEXEC
  NAMES gsi.x
  HINTS ${CONTROLPATH}
        ${CONTROLPATH}/bin
        ${CONTROLPATH}/exec
        ${GSICONTROLPATH}
        ${GSICONTROLPATH}/bin
        ${GSICONTROLPATH}/exec
        ENV ${CONTROLPATH}
        ENV ${CONTROLPATH}/bin
        ENV ${CONTROLPATH}/exec
        ENV ${GSICONTROLPATH}
        ENV ${GSICONTROLPATH}/bin
        ENV ${GSICONTROLPATH}/exec
  NO_DEFAULT_PATH
)

if (GSICONTROLEXEC_FOUND)
  message(STATUS "RT: Control GSIexec Found: ${GSICONTROLEXEC}")
else()
  message(WARNING "RT: Control GSIexec Not Found, GSI Regression Tests Disabled!")
endif()

find_program(ENKFCONTROLEXEC
  NAMES enkf.x
  HINTS ${CONTROLPATH}
        ${CONTROLPATH}/bin
        ${CONTROLPATH}/exec
        ${ENKFCONTROLPATH}
        ${ENKFCONTROLPATH}/bin
        ${ENKFCONTROLPATH}/exec
        ENV ${CONTROLPATH}
        ENV ${CONTROLPATH}/bin
        ENV ${CONTROLPATH}/exec
        ENV ${ENKFCONTROLPATH}
        ENV ${ENKFCONTROLPATH}/bin
        ENV ${ENKFCONTROLPATH}/exec
  NO_DEFAULT_PATH
)

if (ENKFCONTROLEXEC_FOUND)
  message(STATUS "RT: Control EnKFexec Found: ${ENKFCONTROLEXEC}")
else()
  message(WARNING "RT: Control EnKFexec Not Found, EnKF Regression Tests Disabled!")
endif()

# If neither are found, nothing to do; simply return
if(NOT (GSICONTROLEXEC_FOUND AND ENKFCONTROLEXEC_FOUND))
  message(WARNING "RT: Regression Tests Disabled!")
  return()
endif()

# Create regression_var.out file
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/regression_var.out" "${CMAKE_CURRENT_SOURCE_DIR}/regression_var.sh ${PROJECT_SOURCE_DIR}/.. ${PROJECT_BINARY_DIR} ${PROJECT_SOURCE_DIR} ${PROJECT_BINARY_DIR}/src/gsi/gsi.x ${PROJECT_BINARY_DIR}/src/enkf/enkf.x ${GSICONTROLEXEC} ${ENKFCONTROLEXEC}")

list(APPEND REG_TEST_NAMES
  global_T62 global_T62_ozonly global_4dvar_T62 global_4denvar_T126
  global_fv3_4denvar_T126 global_fv3_4denvar_C192 global_lanczos_T62
  arw_netcdf arw_binary nmm_binary nmm_netcdf
  nmmb_nems_4denvar
  hwrf_nmm_d2 hwrf_nmm_d3
  rtma
  global_enkf_T62
  netcdf_fv3_regional
  global_C96_fv3aero global_C96_fv3aerorad
)

# Run each regression test; one at a time
foreach(REG_TEST ${REG_TEST_NAMES})
  add_test(NAME ${REG_TEST}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMAND regression_driver.sh ${REG_TEST} ${CMAKE_CURRENT_BINARY_DIR})
endforeach(REG_TEST)
set_tests_properties(${REG_TEST_NAMES} PROPERTIES TIMEOUT 86400)
