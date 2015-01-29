#!/bin/sh
set -x
export LIBDIR=../../lib
export FCMP=xlf90_r
make -f makefile
