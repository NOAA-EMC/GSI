#!/bin/bash

case $(hostname -f) in

  llogin[1-3]) MACHINE_ID=wcoss_cray ;; ### luna
  slogin[1-3]) MACHINE_ID=wcoss_cray ;; ### surge

  m7[12]a[1-3].ncep.noaa.gov) MACHINE_ID=wcoss_dell_p3 ;; ### mars
  v7[12]a[1-3].ncep.noaa.gov) MACHINE_ID=wcoss_dell_p3 ;; ### venus
  m109a[1-4].ncep.noaa.gov)   MACHINE_ID=wcoss_dell_p3 ;; ### mars3.5
  m110a[1-4].ncep.noaa.gov)   MACHINE_ID=wcoss_dell_p3 ;; ### mars3.5
  v109a[1-4].ncep.noaa.gov)   MACHINE_ID=wcoss_dell_p3 ;; ### venus3.5
  v110a[1-4].ncep.noaa.gov)   MACHINE_ID=wcoss_dell_p3 ;; ### venus3.5

  adecflow0[12].acorn.wcoss2.ncep.noaa.gov)  MACHINE_ID=wcoss2 ;; ### acorn
  alogin0[12].acorn.wcoss2.ncep.noaa.gov)    MACHINE_ID=wcoss2 ;; ### acorn
  clogin0[1-9].cactus.wcoss2.ncep.noaa.gov)  MACHINE_ID=wcoss2 ;; ### cactus01-9
  clogin10.cactus.wcoss2.ncep.noaa.gov)      MACHINE_ID=wcoss2 ;; ### cactus10
  dlogin0[1-9].dogwood.wcoss2.ncep.noaa.gov) MACHINE_ID=wcoss2 ;; ### dogwood01-9
  dlogin10.dogwood.wcoss2.ncep.noaa.gov)     MACHINE_ID=wcoss2 ;; ### dogwood10

  gaea9)               MACHINE_ID=gaea ;; ### gaea9
  gaea1[0-6])          MACHINE_ID=gaea ;; ### gaea10-16
  gaea9.ncrc.gov)      MACHINE_ID=gaea ;; ### gaea9
  gaea1[0-6].ncrc.gov) MACHINE_ID=gaea ;; ### gaea10-16

  hfe0[1-9]) MACHINE_ID=hera ;; ### hera01-9
  hfe1[0-2]) MACHINE_ID=hera ;; ### hera10-12
  hecflow01) MACHINE_ID=hera ;; ### heraecflow01

  s4-submit.ssec.wisc.edu) MACHINE_ID=s4 ;; ### s4

  fe[1-8]) MACHINE_ID=jet ;; ### jet01-8
  tfe[12]) MACHINE_ID=jet ;; ### tjet1-2

  Orion-login-[1-4].HPC.MsState.Edu) MACHINE_ID=orion ;; ### orion1-4

  cheyenne[1-6].cheyenne.ucar.edu)     MACHINE_ID=cheyenne ;; ### cheyenne1-6
  cheyenne[1-6].ib0.cheyenne.ucar.edu) MACHINE_ID=cheyenne ;; ### cheyenne1-6
  chadmin[1-6].ib0.cheyenne.ucar.edu)  MACHINE_ID=cheyenne ;; ### cheyenne1-6

  login[1-4].stampede2.tacc.utexas.edu) MACHINE_ID=stampede ;; ### stampede1-4

  login0[1-2].expanse.sdsc.edu) MACHINE_ID=expanse ;; ### expanse1-2

  discover3[1-5].prv.cube) MACHINE_ID=discover ;; ### discover31-35
esac

# Overwrite auto-detect with MACHINE if set
MACHINE_ID=${MACHINE:-${MACHINE_ID}}

# Append compiler (only on machines that have multiple compilers)
if [ $MACHINE_ID = hera ] || [ $MACHINE_ID = cheyenne ]; then
    MACHINE_ID=${MACHINE_ID}.${COMPILER}
fi
