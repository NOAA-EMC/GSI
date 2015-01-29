#!/bin/ksh

#@ job_name = gfs_grid_setup
#@ job_type = serial
#@ step_name = serial_1
#@ output = gfs_mask_terr.log
#@ error = gfs_mask_terr.log
#@ resources = consumablecpus(1) consumablememory(2000 MB)
#@ wall_clock_limit = 00:03:00
#@ node_usage=shared
#@ account_no=GFS-MTN
#@ class = dev
#@ executable = gfs_mask_terr.sh
#@ queue

#@ job_type = parallel
#@ step_name = parallel_1
#@ dependency = serial_1 == 0
#@ output = gfs_land_climo.log
#@ error = gfs_land_climo.log
#@ network.MPI=csss,shared,us
#@ total_tasks=1
#@ resources = consumablecpus(1) consumablememory(1800 MB)
#@ wall_clock_limit = 00:03:00
#@ node_usage=shared
#@ account_no=GFS-MTN
#@ class = dev
#@ executable = gfs_land_climo.sh
#@ queue


exit 0

