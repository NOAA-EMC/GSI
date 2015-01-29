#!/bin/ksh
#-----------------------------------------------------------------
#
# script: makelib.ksh
#    prgmmr: george gayno     ORG: NP2      DATE: 2005-09-15
#
# abstract: 
#    builds sfcutil libraries with either 4 or 8 byte floats.
#
# usage:
#    takes one command line argument. 
#    type "makelib.ksh 4" for 4 byte version
#    type "makelib.ksh 8" for 8 byte version
#    type "makelib.ksh clean to remove both libraries
#
#-----------------------------------------------------------------
if [[ $1 = 4 ]]
then
  echo Will build library with 4 byte floats.
  ln -fs makefile.conf_4  makefile.conf
  make
  rm -f makefile.conf
elif [[ $1 = 8 ]]
then
  echo Will build library with 8 byte floats.
  ln -fs makefile.conf_d makefile.conf
  make
  rm -f makefile.conf
elif [[ $1 = "clean" ]]
then
  ln -fs makefile.conf_4 makefile.conf
  make clean 
  rm -f makefile.conf
  ln -fs makefile.conf_d makefile.conf
  make clean 
  rm -f makefile.conf
else
  echo Must choose either 4 or 8 byte option.
  exit 1
fi

exit 0
