#!/bin/bash

files=/data/daves/archive_files/avhrr/gdas2012090800.tar.gz
outdir=/scratch5/$USER/pgbfiles/avhrr
mkdir -p $outdir

cd $outdir

for f in $files
do
   echo $f
   /bin/tar xvzf $f "pgb*"
done

