# - Find the Control version of GSI to use for regression testing

set( NO_DEFAULT_PATH )
find_file( CONTROL_EXE 
    NAMES global_gsi gsi.x gsi_wrf.x
    HINTS
        ${CMAKE_SOURCE_DIR}/../trunk/src
        ${PROJECT_BINARY_DIR}/../build-trunk/bin
        $ENV{CONTROLPATH}
        $ENV{CONTROLPATH}/src
        /da/save/Michael.Lueken/trunk/src
        /scratch4/NCEPDEV/da/save/Michael.Lueken/svn1/src
        /scratch4/NCEPDEV/da/save/Michael.Lueken/trunk/src
   
    ${NO_DEFAULT_PATH})

set( GSICONTROL ${CONTROL_EXE} CACHE STRING "GSI control executable for regression testing" FORCE )

find_file( ENKF_CONTROL_EXE 
    NAMES global_enkf enkf.x enkf_wrf.x
    HINTS
        ${CMAKE_SOURCE_DIR}/../trunk/src/enkf
        ${PROJECT_BINARY_DIR}/../build-trunk/bin
        $ENV{CONTROLPATH}/enkf
        $ENV{CONTROLPATH}/src/enkf
        /da/save/Michael.Lueken/trunk/src/enkf
        /scratch4/NCEPDEV/da/save/Michael.Lueken/svn1/src/enkf
        /scratch4/NCEPDEV/da/save/Michael.Lueken/trunk/src/enkf
   
    ${NO_DEFAULT_PATH})

set( ENKFCONTROL ${ENKF_CONTROL_EXE} CACHE STRING "ENKF control executable for regression testing" FORCE )

