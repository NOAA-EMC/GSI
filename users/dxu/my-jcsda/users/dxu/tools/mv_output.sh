#!/bin/bash

for DAY in 21 22 
do
   for CYC in 00 
   do
      cd /scratch4/snebuda/par/goesr_noa
      tar -cvf /data/snebuda/archive_files/goesr_noa/gdas_rs_201112$DAY$CYC.tar \
	       sfcanl.gdas.201112$DAY$CYC siganl.gdas.201112$DAY$CYC \
	       biascr.gdas.201112$DAY$CYC satang.gdas.201112$DAY$CYC \
	       siganl_201112$DAY${CYC}_mem??? sfcanl_201112$DAY${CYC}_mem???
   done
done

