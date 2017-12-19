# Setup the minimum version required of CMake to generate the Makefile
# Raises a FATAL_ERROR if version < 2.8
cmake_minimum_required (VERSION 2.8 FATAL_ERROR)

# Project name and LANGUAGE
project (zero_biascoeff Fortran)

# Where are the additional libraries installed? Note: provide includes
# path here, subsequent checks will resolve everything else
set( CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/CMake/modules/" )

# Fortran compiler flags
if (CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
    set(CMAKE_Fortran_FLAGS "-g -C -traceback -warn all")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
    set(CMAKE_Fortran_FLAGS "-g -W -fbounds-check -fbacktrace")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL Cray)
    set(CMAKE_Fortran_FLAGS "-g -check all -traceback -warn")
else (CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
    message(FATAL_ERROR "Unsupported compiler ${CMAKE_Fortran_COMPILER_ID}, Abort!")
endif (CMAKE_Fortran_COMPILER_ID STREQUAL Intel)

# Lists
# Setup the list of source files
set( ZERO_BIASCOEFF_SOURCES
    zero_biascoeff.f90
)

# ${PROJECT_NAME} refers to Learn_CMake
add_executable( ${PROJECT_NAME}.x ${ZERO_BIASCOEFF_SOURCES} )
