#!/bin/bash

# The authoritative copy of this script lives in the ufs-weather-model at:
# https://github.com/ufs-community/ufs-weather-model/blob/develop/tests/detect_machine.sh
# If any local modifications are made or new platform support added,
# please consider opening an issue and a PR to the ufs-weather-model
# so that this copy remains in sync with its authoritative source
#
# Thank you for your contribution

# Overwrite auto-detect in in container
if [[ -v SINGULARITY_CONTAINER ]]; then
  MACHINE_ID=container
fi

# If the MACHINE_ID variable is set, skip this script.
[[ -n ${MACHINE_ID:-} ]] && return

# First detect w/ hostname
case $(hostname -f) in

  adecflow0[1-3].acorn.wcoss2.ncep.noaa.gov)  MACHINE_ID=acorn ;; ### acorn
  alogin0[1-3].acorn.wcoss2.ncep.noaa.gov)    MACHINE_ID=acorn ;; ### acorn
  clogin0[1-9].cactus.wcoss2.ncep.noaa.gov)  MACHINE_ID=wcoss2 ;; ### cactus01-9
  clogin10.cactus.wcoss2.ncep.noaa.gov)      MACHINE_ID=wcoss2 ;; ### cactus10
  dlogin0[1-9].dogwood.wcoss2.ncep.noaa.gov) MACHINE_ID=wcoss2 ;; ### dogwood01-9
  dlogin10.dogwood.wcoss2.ncep.noaa.gov)     MACHINE_ID=wcoss2 ;; ### dogwood10

  gaea6[1-8])          MACHINE_ID=gaeac6 ;; ### gaea61-68
  gaea6[1-8].ncrc.gov) MACHINE_ID=gaeac6 ;; ### gaea61-68

  hfe0[1-9]) MACHINE_ID=hera ;; ### hera01-09
  hfe1[0-2]) MACHINE_ID=hera ;; ### hera10-12
  hecflow01) MACHINE_ID=hera ;; ### heraecflow01

  ufe0[1-9]) MACHINE_ID=ursa ;; ### ursa01-09
  ufe1[0-2]) MACHINE_ID=ursa ;; ### ursa10-12
  uecflow01) MACHINE_ID=ursa ;; ### ursaecflow01

  der*) MACHINE_ID=derecho ;; ### derecho[1-8]
  dec*) MACHINE_ID=derecho ;; ### decxxx computing node

  ip-*|compute-dy-*|processing-dy-*)
    case ${PW_CSP:-} in
      "aws" | "google" | "azure") MACHINE_ID=noaacloud ;;
      *) MACHINE_ID=aws-ec2 ;;
    esac
    ;;

  Orion-login-[1-4].HPC.MsState.Edu) MACHINE_ID=orion ;; ### orion1-4

  [Hh]ercules-login-[1-4].[Hh][Pp][Cc].[Mm]s[Ss]tate.[Ee]du) MACHINE_ID=hercules ;; ### hercules1-4

  *) MACHINE_ID=UNKNOWN ;;  # Unknown platform
esac

if [[ ${MACHINE_ID} == "UNKNOWN" ]]; then
   case ${PW_CSP:-} in
      "aws" | "google" | "azure") MACHINE_ID=noaacloud ;;
      *) PW_CSP="UNKNOWN"
   esac
fi

# Overwrite auto-detect with MACHINE if set
MACHINE_ID=${MACHINE:-${MACHINE_ID}}

# If MACHINE_ID is no longer UNKNNOWN, return it
if [[ "${MACHINE_ID}" != "UNKNOWN" ]]; then
  return
fi

# Try searching based on paths since hostname may not match on compute nodes
if [[ -d /lfs/h3 ]]; then
  # We are on NOAA Cactus or Dogwood
  MACHINE_ID=wcoss2
elif [[ -d /lfs/h1 && ! -d /lfs/h3 ]]; then
  # We are on NOAA TDS Acorn
  MACHINE_ID=acorn
elif [[ -d /scratch3 ]]; then
  # We are on NOAA Hera or Ursa
  mount=$(findmnt -n -o SOURCE /home)
  if [[ ${mount} =~ "ursa" ]]; then
    MACHINE_ID=ursa
  else
    MACHINE_ID=hera
  fi
elif [[ -d /work ]]; then
  # We are on MSU Orion or Hercules
  mount=$(findmnt -n -o SOURCE /home)
  if [[ ${mount} =~ "hercules" ]]; then
    MACHINE_ID=hercules
  else
    MACHINE_ID=orion
  fi
elif [[ -d /gpfs/f6 ]]; then
  # We are on GAEAC6.
  MACHINE_ID=gaeac6
elif [[ -d /gpfs/csfs1 ]]; then
  # We are on NCAR DERECHO.
  MACHINE_ID=derecho
elif [[ -d /opt/spack-stack && -d /lustre ]]; then
  # We are on AWS ec2
  MACHINE_ID=aws-ec2
else
  echo WARNING: UNKNOWN PLATFORM 1>&2
fi
