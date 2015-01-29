#!/bin/sh
#set -x
dir1=/global/save/glopara/RFC/GSI_Q2FY2012/parms/parm_prep
dir2=/global/save/wx24fy/gfs_trunk/para/parms/parm_prep

file1=`ls $dir1`
file2=`ls $dir2`
for f in $file1
do
  if [ ! -s $dir2/$f ]; then
    echo $f does not exist in dir2
  else
    cmp $dir1/$f  $dir2/$f
  fi
done


