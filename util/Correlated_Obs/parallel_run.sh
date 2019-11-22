#!/bin/sh
#date of first radstat file
bdate=2014040700
#date of last radstat file
edate=2014041718
#instrument name, as it would appear in the title of a diag file
#instr=airs_aqua
instr=iasi_metop-a
#location of radstat file
exp=prCtl
diagdir=/scratch4/NCEPDEV/da/noscrub/${USER}/archive/${exp}
#working directory
wrkdir=/scratch4/NCEPDEV/stmp4/${USER}/corr_obs
#location the covariance matrix is saved to
savdir=$diagdir
#FOV type- 0 for all, 1 for sea, 2 for land, 3 for snow, 
#4 for mixed (recommended to use 0 for mixed)
#5 for ice and 6 for snow and ice combined (recommended when using ice)
type=1
#cloud 1 for clear FOVs, 2 for clear channels
cloud=2
#absolute value of the maximum allowable sensor zenith angle (degrees)
angle=30
#option to output the channel wavenumbers
wave_out=.false.
#option to output the assigned observation errors
err_out=.false.
#option to output the correlation matrix
corr_out=.false.
#condition number to recondition Rcov.  Set <0 to not recondition
kreq=-150
#method to recondition:  1 for trace method, 2 for Weston's second method
method=1
#method to compute covariances: 1 for Hollingsworth-Lonnberg, 2 for Desroziers
cov_method=2
#maximum time between observations in a pair, in minutes
time_sep=1.0
#bin size for obs pairs in km
bsize=1
#bin center, in km, needed for Hollingsworth-Lonnberg
bcen=80
#channel set choice:  0 to only use active channels, 1 to use all channels
chan_set=0
#number of processors to use to unpack radstat files-most efficient if # of radstats/$num_proc has a small remainder
num_proc=11
#number of processors to run cov_calc on
NP=16
#wall time to unpack radstat files format hh:mm:ss for theia, hh:mm for wcoss
unpack_walltime=02:30:00
#wall time to run cov_calc hh:mm:ss for theia, hh:mm for wcoss
wall_time=01:00:00
#requested memory in MB to unpack radstats, on WCOSS/Cray.  Increases with decreasing $num_proc
#should be at least 15
Umem=50
#requested memory in MB for cov_calc, on WCOSS/Cray
Mem=50
#job account name (needed on theia only)
account=da-cpu
#job project code (needed on wcoss only)
project_code=GFS-T2O
#machine-theia or wcoss, all lower case
machine=theia
#netcdf or binary diag files-0 for binary, 1 for netcdf
netcdf=0
ndate=/scratch4/NCEPDEV/da/save/Michael.Lueken/nwprod/util/exec/ndate
#ndate=/gpfs/dell2/emc/modeling/noscrub/Kristen.Bathmann/ndate

####################################################################

cdate=$bdate
[ ! -d ${wrkdir} ] && mkdir ${wrkdir}
nt=0
one=1
while [[ $cdate -le $edate ]] ; do
   while [[ ! -f $diagdir/radstat.gdas.$cdate ]] ; do 
      cdate=`$ndate +06 $cdate`
      if [ $cdate -ge $edate ] ; then
         break
      fi
   done
   cdate=`$ndate +06 $cdate`
   nt=$((nt+one))
done
dattot=$nt
cp unpack_rads.sh $wrkdir
cp par_run.sh $wrkdir
cp sort_diags.sh $wrkdir
cp ../../exec/cov_calc $wrkdir

cd $wrkdir
num_jobs=$num_proc
if [ $num_proc -ge $nt ] ; then
   num_jobs=$nt
fi
jobs_per_proc=$((nt/num_jobs))
last_proc_jobs=$((nt-jobs_per_proc*num_jobs+jobs_per_proc))
nt=1
cdate=$bdate
while [[ $nt -le $num_jobs ]] ; do
   nd=1
   date1=$cdate
   jobsn=$jobs_per_proc
   if [ $nt -eq $num_jobs ] ; then
      jobsn=$last_proc_jobs
   fi
   while [[ $nd -lt $jobsn ]] ; do
      while [[ ! -f $diagdir/radstat.gdas.$cdate ]] ; do
         cdate=`$ndate +06 $cdate`
         if [ $cdate -gt $edate ] ; then
            break
         fi
      done
      nd=$((nd + one))
      cdate=`$ndate +06 $cdate`
   done
   date2=$cdate
   numin=$((nt - one))
   coun=1
   if [[ $numin -gt 0 ]] ; then
      numin=$((numin*jobs_per_proc))
   fi
   numin=$((numin + one))
    
   cat << EOF > params.sh
#!/bin/sh
bdate=$date1
edate=$date2
start_nt=$numin
ndate=$ndate
wrkdir=$wrkdir
diagdir=$diagdir
instr=$instr
netcdf=$netcdf
EOF
   chmod +rwx params.sh
   cat unpack_rads.sh >> params.sh
   mv params.sh unpack_rads_${nt}.sh
   nt=$((nt + one))
   cdate=`$ndate +06 $cdate`
done

cat << EOF > jobchoice.sh
#!/bin/sh
nt=\$1
one=1
njobs=$jobs_per_proc
./unpack_rads_\${nt}.sh
EOF
chmod +rwx jobchoice.sh

if [ $machine = theia ] ; then
cat << EOF > jobarray.sh
#!/bin/sh
#SBATCH -A $account
#SBATCH -o unpack_out
#SBATCH -e unpack_err
#SBATCH -q batch
#SBATCH --time=${unpack_walltime}
#SBATCH --ntasks=1
#SBATCH -J unpack
#SBATCH --array 1-${num_jobs}
cd $wrkdir
./jobchoice.sh \${SLURM_ARRAY_TASK_ID}
EOF
jobid=$(sbatch jobarray.sh)
elif [ $machine = wcoss ] ; then
cat << EOF > jobarray.sh
#!/bin/sh
#BSUB -o unpack_out
#BSUB -e unpack_err
#BSUB -q dev
#BSUB -M ${Umem}
#BSUB -n 1
#BSUB -W ${unpack_walltime}
#BSUB -R span[ptile=1]
#BSUB -P ${project_code}
#BSUB -J unpack[1-${num_jobs}]
cd $wrkdir
echo ${LSB_JOBINDEX}
./jobchoice.sh \${LSB_JOBINDEX} 
EOF
bsub < jobarray.sh
else
   echo cannot submit job, not on theia or wcoss
   exit 1
fi
#check if shifts are needed
if [ $machine = theia ] ; then
cat << EOF > params.sh
#!/bin/sh
#SBATCH -A $account
#SBATCH -o sort_out
#SBATCH -e sort_err
#SBATCH -q batch
#SBATCH --time=00:02:00
#SBATCH --ntasks=1
#SBATCH -J sort_diag
#SBATCH --dependency=afterany:${jobid##* }
wrkdir=$wrkdir
ntot=$dattot
EOF
chmod +rwx params.sh
cat sort_diags.sh >> params.sh
mv params.sh sort_diags.sh

jobid=$(sbatch sort_diags.sh )
elif [ $machine = wcoss ] ; then
cat << EOF > params.sh
#!/bin/sh
#BSUB -o sort_out
#BSUB -e sort_err
#BSUB -q dev
#BSUB -M 30
#BSUB -n 1
#BSUB -W 00:02
#BSUB -R span[ptile=1]
#BSUB -P ${project_code}
#BSUB -J sort_diag
wrkdir=$wrkdir
ntot=$dattot
EOF
chmod +rwx params.sh
cat sort_diags.sh >> params.sh
mv params.sh sort_diags.sh
bsub -w "done(unpack)" < sort_diags.sh
#bsub < sort_diags.sh
else
   exit 1
fi
#run cov_calc
if [ $machine = theia ] ; then
cat << EOF > params.sh
#!/bin/sh
#SBATCH -A $account
#SBATCH -o comp_out
#SBATCH -e comp_err
#SBATCH -q batch
#SBATCH --time=$wall_time
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=$NP
#SBATCH -J cov_calc
#SBATCH --dependency=after:${jobid##* }
bdate=$bdate
edate=$edate
instr=$instr
diagdir=$diagdir
wrkdir=$wrkdir
savdir=$savdir
type=$type
cloud=$cloud
angle=$angle
wave_out=$wave_out
err_out=$err_out
corr_out=$corr_out
kreq=$kreq
method=$method
cov_method=$cov_method
time_sep=$time_sep
bsize=$bsize
bcen=$bcen
chan_set=$chan_set
ntot=$dattot
NP=$NP
netcdf=$netcdf
EOF
chmod +rwx params.sh
cat par_run.sh >> params.sh
mv params.sh par_run.sh
sbatch par_run.sh
elif [ $machine = wcoss ] ; then
cat << EOF > params.sh
#!/bin/sh
#BSUB -o comp_out
#BSUB -e comp_err
#BSUB -openmp
#BSUB -q dev
#BSUB -M ${Mem}
#BSUB -n $NP
#BSUB -W $wall_time
#BSUB -R span[ptile=$NP]
#BSUB -P ${project_code}
#BSUB -J cov_calc
bdate=$bdate
edate=$edate
instr=$instr
diagdir=$diagdir
wrkdir=$wrkdir
savdir=$savdir
type=$type
cloud=$cloud
angle=$angle
wave_out=$wave_out
err_out=$err_out
corr_out=$corr_out
kreq=$kreq
method=$method
cov_method=$cov_method
time_sep=$time_sep
bsize=$bsize
netcdf=$netcdf
bcen=$bcen
chan_set=$chan_set
ntot=$dattot
NP=$NP
EOF
chmod +rwx params.sh
cat par_run.sh >> params.sh
mv params.sh par_run.sh
bsub -w "done(sort_diag)" < par_run.sh
else 
   exit 1
fi
exit 0
