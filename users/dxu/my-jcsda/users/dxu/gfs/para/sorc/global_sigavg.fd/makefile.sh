#!/bin/sh
set -x
currdir=$(pwd)
cd ../../
export LIBDIR=$(pwd)/lib
export FCMP=xlf90_r
cd $currdir
make -f makefile
