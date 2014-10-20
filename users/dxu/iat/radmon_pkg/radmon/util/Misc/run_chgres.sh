#!/bin/sh
set -x
#
#@ account_no = GFS-MTN
#@ class = dev
#@ group = dev
#@ output = test.out
#@ error = test.out
#@ job_type = serial
#@ parallel_threads=32
#@ task_affinity = core(32)
#@ wall_clock_limit = 00:02:00
#@ node_resources = ConsumableMemory(106 GB)
#@ queue

# working directory
tmpdir=/ptmp/wx23adc/test
mkdir -p $tmpdir; cd $tmpdir                     

export VERBOSE=YES

# input surface file
export SFCINP=/global/noscrub/wx23adc/prAMSRE/sfcanl.gdas.2010051012
# input sigma file
export SIGINP=/global/noscrub/wx23adc/prAMSRE/siganl.gdas.2010051012
# output surface file
export SFCOUT=./sfcanl.gdas.t254.2010051012
# output sigma file
export SIGOUT=./siganl.gdas.t254.2010051012

# output jcap and i/j dimension
export JCAP=254
export LONB=768
export LATB=384
# number of vertical levels.  same as input.
export LEVS=64

# options for surface data interpolation.  don't touch.
export CLIMO_FIELDS_OPT=3
export LANDICE_OPT=2

export CHGRESEXEC=/nwprod/exec/global_chgres

export XLSMPOPTS=parthds=32:stack=4000000000
/nwprod/ush/global_chgres.sh 

exit
