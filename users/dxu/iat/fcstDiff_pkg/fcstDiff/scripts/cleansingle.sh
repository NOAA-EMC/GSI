#!/bin/sh
export dir=${dir:-${1:-preg}}
cd /global/noscrub/wx23dc/$dir
rm -f flx* gsistat* log* diag_* *.bin sub* *.txt c* obs* mpi* aer* SIG* SFC* FLX* D3D*
rm -rf dir*
rm -f fort* PET* LOG* no
rm -f sfc* gdas* post* bias* gfs* satang* global* pgi*
