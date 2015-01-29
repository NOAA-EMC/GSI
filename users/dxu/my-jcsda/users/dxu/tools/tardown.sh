#!/bin/bash 


DDIR=/data/dxu/archive_files/${1}
DATE=${2}

tar -xvf ${DDIR}/gdas${DATE}00.tar cnvstat.gdas.${DATE}00
tar -xvf ${DDIR}/gdas${DATE}06.tar cnvstat.gdas.${DATE}06
tar -xvf ${DDIR}/gdas${DATE}12.tar cnvstat.gdas.${DATE}12
tar -xvf ${DDIR}/gdas${DATE}18.tar cnvstat.gdas.${DATE}18
tar -xvf ${DDIR}/gfs${DATE}00.tar  cnvstat.gfs.${DATE}00
tar -xvf ${DDIR}/gfs${DATE}00.tar  pgbanl.gfs.${DATE}00
tar -xvf ${DDIR}/gfs${DATE}00.tar  pgbf*.gfs.${DATE}00

tar -cvf ${DATE}00-${DATE}18.tar cnvstat.* pgb*

rm cnvstat.* pgb*



