#!/bin/sh
set -xa
while read line        
do        
    grep $line bufr_stalist.meteo.gfs3.dana20120214.sorted >> addcollect.txt        
done <newcollect.txt
