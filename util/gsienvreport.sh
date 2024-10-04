#!/bin/bash
# by Guoqing Ge, 2018/09/10 
#
echo "
-------------------!!! Disclaimer !!!------------------------------------------
 This shell script will report some enviromental information 
      about the host on which you plan to compile/run GSI/EnKF.
 The information will be displayed to the screen for you to review.
 You will decide what information is appropriate to be sent to helpdesk.
 The helpdesk does not collect any personal information.
-------------------------------------------------------------------------------
 "

echo "comgsi version  : v3.7"
cat /proc/cpuinfo | grep 'vendor' | uniq    
cat /proc/cpuinfo | grep 'model name' | uniq
echo "Processors      : `cat /proc/cpuinfo | grep processor | wc -l`"
echo "Current host    : `hostname`"
echo "Current OS      : `uname`"
echo "Current shell   : `ps -o comm= $PPID`"
echo "NETCDF          : $NETCDF"

# try to find a working Fortran compiler in current enviroment
echo
compiler="ifort"
echo "[INTEL]:
`${compiler} --version 2>/dev/null`"
echo
compiler="pgfortran"
echo "[PGI]:
`${compiler} --version 2>/dev/null`"
echo
compiler="gfortran"
echo "[GNU]:
`${compiler} --version 2>/dev/null`"

## check whether mpirun, mpirun.lsf or mpiexec exist
echo
mpi=`which mpirun 2>/dev/null`
echo "mpirun --  ${mpi}"
mpi=`which mpirun.lsf 2>/dev/null`
echo "mpirun.lsf --  ${mpi}"
mpi=`which mpiexec 2>/dev/null`
echo "mpiexec --  ${mpi}"
RMS=`which qsub 2>/dev/null`
echo "qsub --  ${RMS}"
RMS=`which bsub 2>/dev/null`
echo "bsub --  ${RMS}"
RMS=`which sbatch 2>/dev/null`
echo "sbatch --  ${RMS}"

## check the host operating system
echo "
The operation system and version information: "
cat /proc/version 2>/dev/null; cat /etc/*release 2>/dev/null; cat /etc/*version 2>/dev/null
