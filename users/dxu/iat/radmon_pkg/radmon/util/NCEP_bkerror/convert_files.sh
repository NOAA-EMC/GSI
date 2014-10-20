#!/bin/sh

cd /global/noscrub/wx20kd/bkgerror_stats/EXP-rhberr/


list="130 192 258 290 386 578 96 882"
for lat in $list; do

  ln -s fixold/global_berror.l64y${lat}.f77 ./berror_stats_in
  ./reduce_rhvars.x
  mv berror_stats_out fixnew/global_berror.l64y${lat}.f77
  rm berror_stats_in

done
