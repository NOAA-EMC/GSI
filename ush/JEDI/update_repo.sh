#!/bin/bash
# update JEDI IODA/UFO repository with files
# after generating them
# 
ingeo="/scratch3/NCEPDEV/stmp1/Cory.R.Martin/JEDI/output/2018041500/geoval/"
outufo="/scratch4/NCEPDEV/da/save/Cory.R.Martin/JEDI/src/ufo-bundle/ufo/test/testinput/atmosphere/"
inobs="/scratch3/NCEPDEV/stmp1/Cory.R.Martin/JEDI/output/2018041500/obs/"
outioda="/scratch4/NCEPDEV/da/save/Cory.R.Martin/JEDI/src/ufo-bundle/ioda/test/testinput/atmosphere/"
adate="2018041500"
suffix="s.nc4"
files="
airs_aqua
amsua_aqua
amsua_metop-a
amsua_metop-b
amsua_n15
amsua_n18
amsua_n19
atms_npp
cris-fsr_npp
gmi_gpm
hirs4_metop-a
hirs4_metop-b
hirs4_n19
iasi_metop-a
iasi_metop-b
mhs_metop-a
mhs_metop-b
mhs_n18
mhs_n19
satwind_uv
scatwind_uv
seviri_m08
seviri_m11
sfc
sfcship
sndrd1_g15
sndrd2_g15
sndrd3_g15
sndrd4_g15
sondes
sondes_tv
vadwind_uv
windprof_uv
"

for f in $files; do
  echo "copying $f obs and geoval"
  cp $inobs/"$f"_obs_"$adate"_$suffix $outioda/"$f"_obs_"$adate"_$suffix
  cp $ingeo/"$f"_geoval_"$adate"_$suffix $outufo/"$f"_geoval_"$adate"_$suffix
done

uvfiles="
vadwind
windprof
satwind
scatwind
"

for f in $uvfiles; do
  mv -f $outioda/"$f"_uv_obs_"$adate"_$suffix $outioda/"$f"_obs_"$adate"_$suffix
  mv -f $outufo/"$f"_uv_geoval_"$adate"_$suffix $outufo/"$f"_geoval_"$adate"_$suffix
done  
