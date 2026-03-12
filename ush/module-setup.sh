#!/bin/bash
set -eu

if [[ $MACHINE_ID = hera* ]] ; then
    # We are on NOAA Hera
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /apps/lmod/lmod/init/bash
    fi
    module purge

elif [[ $MACHINE_ID = ursa* ]] ; then
    # We are on NOAA Ursa
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /apps/lmod/lmod/init/bash
    fi
    module purge

elif [[ $MACHINE_ID = hercules* ]] ; then
    # We are on Hercules
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /apps/other/lmod/lmod/init/bash
    fi
    module purge

elif [[ $MACHINE_ID = orion* ]] ; then
    # We are on Orion
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /apps/lmod/lmod/init/bash
    fi
    module purge

elif [[ $MACHINE_ID = container ]] ; then
    # We are in a container
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /usr/lmod/lmod/init/bash
    fi
    module purge

elif [[ $MACHINE_ID = wcoss2 || $MACHINE_ID = acorn ]]; then
    # We are on WCOSS2 (cactus, dogwood, or acorn)
    module reset

elif [[ $MACHINE_ID = gaeac6 ]] ; then
    # We are on GAEA C6.
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /opt/cray/pe/lmod/lmod/init/bash
    fi
    module reset

elif [[ $MACHINE_ID = derecho ]] ; then
    # We are on NCAR derecho
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /glade/u/apps/derecho/24.12/spack/opt/spack/lmod/8.7.37/gcc/12.4.0/nr3e/lmod/lmod/init/bash
    fi
    module --force purge

elif [[ $MACHINE_ID = noaacloud* ]]; then
    # We are on NOAA Cloud
    module purge

else
    echo WARNING: UNKNOWN PLATFORM 1>&2
fi
