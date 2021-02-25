#!/bin/sh -x 


# Author: Andrew Eichmann
# Purpose: to obtain from the HPSS archive the minimal set of files needed to 
# run the gdaseupd task of GFS workflow, intended for EFSOI processing
# Script assumes that these files have been archived during a run; this will
# probably require modifications to the archiving script in workflow.

# This will have to be changed to handle nc4 files 

EXP=nov2019c
#EDATE=2019111800
#EDATE=2019111818
EDATE=2019111712
NMEM_ENKF=20 # total number of ensemble members
#NMEM_ENKF=0
NMEM_EARCGRP=10  # number of ensemble members in archiving group
NDATE=/scratch1/NCEPDEV/global/Fanglin.Yang/save/VRFY/vsdb/nwprod/util/exec/ndate

PDATE=`${NDATE} -6 ${EDATE}`
VDATE=`${NDATE} +24 ${EDATE}`

EDATECYC="${EDATE:8:2}"
PDATECYC="${PDATE:8:2}"
VDATECYC="${VDATE:8:2}"

EDATEDIR=enkfgdas."${EDATE:0:8}"
PDATEDIR=enkfgdas."${PDATE:0:8}"
VDATEDIR=enkfgdas."${VDATE:0:8}"


echo $PDATE
echo $VDATE
echo $EDATECYC
echo $EDATEDIR

tarfile=enkfgdas.tar


hpssdir=/1year/NCEPDEV/emc-global/Andrew.Eichmann/HERA/scratch/${EXP}/${EDATE}

hpsstar get ${hpssdir}/${tarfile} ./${EDATEDIR}/${EDATECYC}/gdas.t${EDATECYC}z.cnvstat.ensmean
hpsstar get ${hpssdir}/${tarfile} ./${EDATEDIR}/${EDATECYC}/gdas.t${EDATECYC}z.oznstat.ensmean
hpsstar get ${hpssdir}/${tarfile} ./${EDATEDIR}/${EDATECYC}/gdas.t${EDATECYC}z.radstat.ensmean
hpsstar get ${hpssdir}/${tarfile} ./${EDATEDIR}/${EDATECYC}/gdas.t${EDATECYC}z.abias_int.ensmean

hpssdir=/1year/NCEPDEV/emc-global/Andrew.Eichmann/HERA/scratch/${EXP}/${PDATE}

hpsstar get ${hpssdir}/${tarfile} ./${PDATEDIR}/${PDATECYC}/gdas.t${PDATECYC}z.atmf006.ensmean.nemsio 



imem=1
while [[ $imem -le $NMEM_ENKF ]]; do
   grpnum=`echo "( ($imem - 1) / ${NMEM_EARCGRP} ) + 1" | bc` 
   group="grp"`printf %02i $grpnum`
   member="mem"`printf %03i $imem`

   echo $member
   echo $group

   tarfile=enkfgdas_${group}.tar
   hpssdir=/1year/NCEPDEV/emc-global/Andrew.Eichmann/HERA/scratch/${EXP}/${EDATE}
   hpsstar get ${hpssdir}/${tarfile} ./${EDATEDIR}/${EDATECYC}/${member}/gdas.t${EDATECYC}z.cnvstat
   hpsstar get ${hpssdir}/${tarfile} ./${EDATEDIR}/${EDATECYC}/${member}/gdas.t${EDATECYC}z.oznstat
   hpsstar get ${hpssdir}/${tarfile} ./${EDATEDIR}/${EDATECYC}/${member}/gdas.t${EDATECYC}z.radstat
   hpsstar get ${hpssdir}/${tarfile} ./${EDATEDIR}/${EDATECYC}/${member}/gdas.t${EDATECYC}z.atmanl.nemsio

   hpssdir=/1year/NCEPDEV/emc-global/Andrew.Eichmann/HERA/scratch/${EXP}/${PDATE}
   hpsstar get ${hpssdir}/${tarfile} ./${PDATEDIR}/${PDATECYC}/${member}/gdas.t${PDATECYC}z.atmf006s.nemsio

   (( imem = $imem + 1 ))
done



