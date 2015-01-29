#!/bin/bash

cd /scratch5/snebuda/pgbfiles/lndvctl

for f in `ls  pgbf00.gfs.*`
do
   fnew=pgbanl.gfs.${f##pgbf00.gfs.}
   echo ln $fnew $f
done 
