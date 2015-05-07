#!/bin/sh
set -x

# IBM: xlf_r; Gaea: ftn; Zeus: ifort
FCMP=${1:-ifort}

export LIBDIR=../../nwprod/lib
export W3LIB=w3lib-2.0_4
#export LIBDIR=/nwprod/lib        
#export W3LIB=w3nco_4                 

if [ $FCMP = xlf_r ] ; then
 export LIBDIR=/nwprod/lib
 export W3LIB=w3_4          
 export FFLAGS="-qsmp=noauto -qmaxmem=-1 "
else
 export FFLAGS="-O3 -traceback -mkl"
fi

rm -f *.x

for file in havgfit sfcfit suruplot dieraob diesurf dieship dieacft dieacar; do
#-------------------
cat >makefile <<EOF
FOPTS=${FFLAGS}
FC=${FCMP}
LIBS=-L${LIBDIR} -l${W3LIB}    
SRCS=   opendian.f miscng.f ${file}.f        
CMD=${file}.x
\$(CMD): \$(SRCS)
	 \$(FC) \$(FOPTS) \$(SRCS) \$(LIBS) -o \$(CMD)
EOF
#-------------------

make

rm makefile 
done

exit

