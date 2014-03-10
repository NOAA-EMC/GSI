#!/bin/bash 
#!/bin/bash
#---------------------------------------------------------------------------------
# Name: listGen.sh
#
# Description:
#   To create list file dynamically for testing.
#
# Author: Deyong Xu (RTI) @ JCSDA,
#         Deyong.Xu@noaa.gov
# Version: Mar 10, 2014, DXu, Initial coding
#
#---------------------------------------------------------------------------------

totalOrbit=`wc -l  meas.list_orig |cut -d" " -f1 `

echo "Total number of orbits is ${totalOrbit}"
echo "Enter the number of orbits you want to test: " 
echo "  1. Number entered should be within range[1, ${totalOrbit}]"
echo "  2. Enter 'all' to use all orbits."

read nOrbits 

if [ ${nOrbits} == 'all' ] 
then 
   cp  meas.list_orig  meas.list
   cp  fwd_fix_em.list_orig  fwd_fix_em.list
else 
   head -${nOrbits} meas.list_orig  >  meas.list
   head -${nOrbits} fwd_fix_em.list_orig  >  fwd_fix_em.list
fi 

echo "List files containg ${nOrbits} orbits are created!!! "

