#!/bin/ksh
set -xa
export VERBOSE=YES
export FIXGLOBAL=/nwprod/fix
export OUTTYP=2
export PGMOUT=stdout.chgres
export PGMERR=stderr.chgres
export CHGRESVARS="IALB=0,ntrac=3,idvc=2,idvt=21,idsl=1,IDVM=0,"
export GFSOUT=gfsout
export NSTOUT=nstout
export IDRT=4
export NSTINP=NULL
export NSTOUT=nstout
./global_chgres.sh sanl_2012102312_highres sfcanl_2012102312_highres sanl_2012102312_hybrid_lores sfcanl_2012102312_hybrid_lores 254 64 768 384

#./global_chgres.sh sanl_2012102300_highres sfcanl_2012102300_highres sanl_2012102300_hybrid_lores sfcanl_2012102300_hybrid_lores 254 64 768 384
