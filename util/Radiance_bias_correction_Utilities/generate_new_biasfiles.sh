!/bin/sh
set -ux
my_userid=Yanqiu.Zhu
date=2012070200
hpsstar=/scratch1/portfolios/NCEPDEV/da/save/Russ.Treadon/svn/gfs/trunk/para/ush/hpsstar
gdate=`echo $date | cut -c1-8`
ghour=`echo $date | cut -c9-10`
comdir=/scratch2/portfolios/NCEPDEV/ptmp/Yanqiu.Zhu/premis_46nnf0
mkdir -p $comdir
cd $comdir

#=================================================================================
# With the bias files generated from the original radiance bias correction scheme, 
# how to prepare the bias files for a GDAS run using the enhanced bias scheme ---
# 
# Get abias and satang files, and diagrad file
#=================================================================================
${hpsstar} get /NCEPDEV/hpssuser/g01/wx20kd/ZEUS/pr3dvclo/${date}gdas.tar biascr.gdas.$date satang.gdas.$date radstat.gdas.$date
#hpsstar get /NCEPDEV/hpssuser/g01/wx20rt/Zeus/prda141b/${date}gdas.enkf.anl.tar "sfcanl_${date}_*"  "siganl_${date}_*"
${hpsstar} get /NCEPDEV/hpssuser/g01/wx20kd/ZEUS/pr3dvclo/${date}gdas.tar sfcanl.gdas.$date siganl.gdas.$date

#=======================
# Make new bias files
#=======================
mv biascr.gdas.$date biascr.gdas.$date.orig
mv satang.gdas.$date satang.gdas.$date.orig
ln -s biascr.gdas.$date.orig satbias_in
ln -s satang.gdas.$date.orig satbias_angle
/scratch1/portfolios/NCEPDEV/da/save/Yanqiu.Zhu/EXP-testCRTM_zhu_submit/util/Radiance_bias_correction_Utilities/write_biascr_option.x -newpc4pred -adp_anglebc 4
mv satbias_angle.new satang.gdas.$date
mv satbias_in.new biascr.gdas.$date
mv satbias_pc biascr_pc.gdas.$date

#=================================================================================
# Once the bias files generated from the enhanced scheme are available, one can  
# always use biascr.gdas.$date, biascr_pc.gdas.$date, and radstat.gdas.$date from 
# a previous run for a GDAS experiment.  
#=================================================================================

