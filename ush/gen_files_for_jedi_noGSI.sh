#!/bin/bash
#SBATCH -J gen_files_for_jedi 
#SBATCH -A da-cpu
#SBATCH -q batch 
#SBATCH --nodes=1
#SBATCH -t 20:00
#SBATCH -o SLURM_%x.o%j
#SBATCH -e SLURM_%x.e%j
#SBATCH –mail-user=$LOGNAME@noaa.gov

set -x 

### user defined arguments
adate=2018041500
ObsDir=/scratch4/NCEPDEV/global/noscrub/dump/
GuessDir=/scratch4/NCEPDEV/da/noscrub/Andrew.Collard/ICs_for_JEDI
WorkDir=/scratch3/NCEPDEV/stmp1/$LOGNAME/JEDI/GSI_work/$adate
OutDir=/scratch3/NCEPDEV/stmp1/$LOGNAME/JEDI/output/$adate

GSIDir=/scratch4/NCEPDEV/da/save/Cory.R.Martin/GSI/
nccat=/scratch4/NCEPDEV/da/save/Cory.R.Martin/GSI/build_jedi/bin/nc_diag_cat_serial.x

IODACDir=/scratch4/NCEPDEV/da/save/Cory.R.Martin/JEDI/src/ioda-converters
nccat=$IODACDir/src/gsi-ncdiag/cat_nc_files.py
pyrad=$IODACDir/src/gsi-ncdiag/rename_rad.py
pyconvsplit=$IODACDir/src/gsi-ncdiag/split_conv.py
pyconvrename=$IODACDir/src/gsi-ncdiag/rename_conv.py
pyconvmerge=$IODACDir/src/gsi-ncdiag/merge_conv.py
pyconvsubset=$IODACDir/src/gsi-ncdiag/subset_conv.py
pyradsubset=$IODACDir/src/gsi-ncdiag/subset_rad.py

dumpobs=gdas

# load modules here used to compile GSI


#####----- normal users need not change anything below this line -----##### 
export crtm_coeffs=./crtm_coeffs/
NDATE=${NDATE:-/scratch4/NCEPDEV/global/save/glopara/nwpara/util/exec/ndate}
ncpc=/bin/cp
ncpl="ln -fs"

# get analysis/guess date
PDYa=`echo $adate | cut -c1-8`
cyca=`echo $adate | cut -c9-10`
gdate=`$NDATE -06 $adate`
PDYg=`echo $gdate | cut -c1-8`
cycg=`echo $gdate | cut -c9-10`

cd $WorkDir

# cat diag files
ntype=3
numfile[0]=0
numfile[1]=0
numfile[2]=0
numfile[3]=0
diagtype[0]="conv conv_gps conv_ps conv_q conv_sst conv_t conv_uv"
diagtype[1]="pcp_ssmi_dmsp pcp_tmi_trmm"
diagtype[2]="sbuv2_n16 sbuv2_n17 sbuv2_n18 sbuv2_n19 gome_metop-a gome_metop-b omi_aura mls30_aura ompsnp_npp
ompstc8_npp"
diagtype[3]="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep
sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13
sndrd2_g13 sndrd3_g13 sndrd4_g13 sndrd1_g14 sndrd2_g14 sndrd3_g14 sndrd4_g14 sndrd1_g15 sndrd2_g15 sndrd3_g15 sndrd4_g15
hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua
imgr_g08 imgr_g11 imgr_g12 imgr_g14 imgr_g15 ssmi_f13 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18
mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_f16 ssmis_f17 ssmis_f18 ssmis_f19 ssmis_f20 iasi_metop-a
hirs4_n19 amsua_n19 mhs_n19 seviri_m08 seviri_m09 seviri_m10 seviri_m11 cris_npp cris-fsr_npp cris-fsr_n20 atms_npp
atms_n20 hirs4_metop-b amsua_metop-b mhs_metop-b iasi_metop-b avhrr_n18 avhrr_metop-a amsr2_gcom-w1 gmi_gpm
saphir_meghat ahi_himawari8"

prefix=" dir.*/"
loops="01"
for loop in $loops; do
   case $loop in
      01) string=ges;;
      03) string=anl;;
       *) string=$loop;;
   esac
   echo $(date) START loop $string >&2
   n=-1
   while [ $((n+=1)) -le $ntype ] ;do
      for type in $(echo ${diagtype[n]}); do
         count=$(ls ${prefix}${type}_${loop}* | wc -l)
         if [ $count -gt 0 ]; then
	    file=diag_${type}_${string}.${adate}_ensmean.nc4 # _ensmean is to work with python script
	    # note if the GSI utility is not working correctly, use the python version
	    # same syntax is used to call it, just change what $nccat is 
              # below is a "poor man's parallelization"
              $nccat -o $file ${prefix}${type}_${loop}.nc4 &
	      sleep 5
            echo "diag_${type}_${string}.${adate}*" >> ${diaglist[n]}
            numfile[n]=$(expr ${numfile[n]} + 1)
         fi
      done
   done
   echo $(date) END loop $string >&2
done

# move GSI diags
mkdir -p $OutDir/GSI_diags
mv diag_* $OutDir/GSI_diags/. 

# now use ioda-converters to convert to JEDI compatible files

cd $OutDir/GSI_diags

# for radiance obs
python $pyrad &

# for conventional obs
python $pyconvsplit
python $pyconvrename
#python $pyconvmerge this doesn't work unless each variable has the same num of obs

# might also want to subset the files here too (_m and _s)
python $pyradsubset &
python $pyconvsubset

# move the output of the python files to final directories
mkdir -p $OutDir/obs
mkdir -p $OutDir/geovals

mv *obs* $OutDir/obs/.
mv *geoval* $OutDir/geovals/.

