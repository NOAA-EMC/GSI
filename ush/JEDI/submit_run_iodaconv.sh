#!/bin/bash
#SBATCH --ntasks-per-node 40
#SBATCH -A da-cpu
#SBATCH -J run_gsi_iodaconv
#SBATCH -t 00:30:00
#SBATCH -q debug
#SBATCH --nodes 1

##### things you should change/confirm
adate=$1
iodaconvbuild=/scratch1/NCEPDEV/da/Cory.R.Martin/JEDI/ioda-converters/build
JEDImodule=/scratch1/NCEPDEV/da/Cory.R.Martin/JEDI/env_jedi_rh
##### things it is not necessary to change but you can
RootWork=/scratch1/NCEPDEV/stmp2/$LOGNAME/GSI_forJEDI/
cleanup='true'

##### do not modify below here ####
MyDir=$GSIDir/ush/JEDI
Ya=`echo $adate | cut -c1-4`
Ma=`echo $adate | cut -c5-6`
Da=`echo $adate | cut -c7-8`
Ha=`echo $adate | cut -c9-10`

mkdir -p $RootWork

# create YAML for IODA converters
rm -rf $RootWork/GSI_iodaconv_$adate.yaml
cat > $RootWork/GSI_iodaconv_$adate.yaml << EOF
data:
  gsiindir: $RootWork/${adate}/GSI_out
  iodaoutdir: $RootWork/${adate}/ioda
  geovaloutdir: $RootWork/${adate}/geovals
  iodaworkdir: $RootWork/${adate}/iodaconv_work
env:
  launcher: srun --export=ALL
  modulefile: $JEDImodule
  nthreads: 1
iodaconv:
  iodaconvbin: $iodaconvbuild/bin/proc_gsi_ncdiag.py
  subsetbin: $iodaconvbuild/bin/subset_files.py
  combineconvbin: $iodaconvbuild/bin/combine_conv.py
time:
  cycle: '$Ha'
  day: '$Da'
  month: '$Ma'
  year: '$Ya'
EOF

# run IODA converters
$MyDir/run_gsi_iodaconv.sh $RootWork/GSI_iodaconv_$adate.yaml || exit 1
