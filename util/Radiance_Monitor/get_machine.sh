#! /bin/bash

#------------------------------------------------------------------
#  get_machine.sh
#
#  Determine which platform we're running on.  System name will
#  be journaled to stdout so calling scripts can get it there.
#  If system is not recognized an empty string will be returned
#  so it's up to the calling scripts to correctly interpret that.
#------------------------------------------------------------------

target=""

if [[ -d /cm ]] ; then
    . $MODULESHOME/init/sh
    target=wcoss_c
elif [[ -d /ioddev_dell ]]; then
    . $MODULESHOME/init/sh
    target=wcoss_d
elif [[ -d /scratch1 ]] ; then
    . /apps/lmod/lmod/init/sh
    target=hera
elif [[ -d /data ]] ; then
    . /usr/share/lmod/lmod/init/sh
    target=s4
elif [[ -d /work ]]; then
    . $MODULESHOME/init/sh
    target=orion
elif [[ -d /jetmon ]] ; then
    . /apps/lmod/lmod/init/sh
    target=jet
elif [[ -d /lfs ]] ; then
    . $MODULESHOME/init/sh
    target=wcoss2
fi

echo $target
exit
