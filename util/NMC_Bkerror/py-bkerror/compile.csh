#!/bin/csh

set todo = "clean"
set compiler = "gnu95"

if ( $#argv >= 1 ) set todo = $1
if ( $#argv >= 2 ) set compiler = $2

if ( $compiler == "gfortran" ) then
    set FC = "gfortran"
    set FFLAGS = "-g"
else if ( $compiler == "gnu95" ) then
    set FC = "gfortran"
    set FFLAGS = "-g"
else if ( $compiler == "intel" ) then
    set FC = "ifort"
    set FFLAGS = "-g -traceback"
else if ( $compiler == "intelem" ) then
    set FC = "ifort"
    set FFLAGS = "-g -traceback"
else
    echo "UNKNOWN compiler = $compiler"
    echo "USING GNU95 compiler instead"
    set compiler = "gnu95"
    set FC = "gfortran"
    set FFLAGS = "-g"
endif

set echo

if ( $todo == "clean" ) then

    rm -f *.x
    rm -f *.so
    rm -f *.o
    rm -f *.mod
    rm -f *.pyf

else if ( $todo == "build" ) then

    $FC -c $FFLAGS str2arr2str.f90
    f2py -c splat.F -m splat --fcompiler=$compiler
    f2py -c str2arr2str.f90 -m str2arr2str --fcompiler=$compiler
    f2py -m bkerror -h bkerror.pyf bkerror.f90
    f2py -c --fcompiler=$FC bkerror.pyf bkerror.f90 str2arr2str.o

else if ( $todo == "test" ) then

    $FC -c $FFLAGS str2arr2str.f90
    $FC -c $FFLAGS bkerror.f90
    $FC -c $FFLAGS read_bkerror.f90
    $FC $FFLAGS -o read_bkerror.x *.o

else

    echo "undefined task: $todo"

endif
