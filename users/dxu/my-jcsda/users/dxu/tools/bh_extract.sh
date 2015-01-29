#!/bin/bash

files=/data/dxu/archive_files/lndvctl/gfs2012091[46]00.tar
outdir=/scratch5/$USER/pgbfiles/lnvdctl

cd $outdir

for f in $files
do
   echo $f
   /bin/tar xvf $f "pgb*"
done

files=/data/dxu/archive_files/lndvctl/gfs2012100800.tar
outdir=/scratch5/$USER/pgbfiles/lnvdctl

cd $outdir

for f in $files
do
   echo $f
   /bin/tar xvf $f "pgb*"
done

files=/data/dxu/archive_files/expfill/gfs??????????.tar
outdir=/scratch5/$USER/pgbfiles/lnvdexp

cd $outdir

for f in $files
do
   echo $f
   /bin/tar xvf $f "pgb*"
done

