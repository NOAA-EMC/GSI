#!/bin/bash

#qdel hold jobs - not very careful script so beware!

# create an array
declare -a jobsid=( $( qstat | grep snebuda | grep time | awk '{ print $1 }' | tr ":" "\n" ) )

for j in "${jobsid[@]}"
do
   jobid=${j%%.pbsa1}
   qdel ${jobid}
done

