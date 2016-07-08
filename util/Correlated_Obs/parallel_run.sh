#!/bin/sh
#date of first radstat file
bdate=2014040100
#date of last radstat file
edate=2014041018
#instrument name, as it would appear in the title of a diag file
#instr=airs_aqua
instr=iasi_metop-b
#location of radstat file
#exp=para_2014
exp=prfullo
diagdir=/da/noscrub/${USER}/archive/${exp}
#working directory
wrkdir=/stmpp1/${USER}/iasib_global
#location the covariance matrix is saved to
savdir=$diagdir
#type- 0 for all, 1 for sea, 2 for land, 3 for ice, 4 for snow, 5 for ice, snow, land, and mixed FOVs
type=5
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
kreq=180
#method to recondition:  1 for trace method, 2 for Weston's second method
method=1
#number of processors to use to unpack radstat files-most efficient if # of radstats/$num_proc has a small remainder
num_proc=10
#wall time to unpack radstat files format hh:mm:ss for theia, hh:mm for wcoss
unpack_walltime=02:30
#wall time to run cov_calc hh:mm:ss for theia, hh:mm for wcoss
wall_time=03:00
#job account name (needed on theia only)
account=cloud
#job project code (needed on wcoss only)
project_code=GFS-T2O
#machine-theia or wcoss, all lower case
machine=wcoss

ndate=/da/save/Kristen.Bathmann/anl_tools/ndate

####################################################################

cdate=$bdate
[ ! -d ${wrkdir} ] && mkdir ${wrkdir}
nt=0
one=1
while [[ $cdate -le $edate ]] ; do
   while [[ ! -f $diagdir/radstat.gdas.$cdate ]] ; do 
      cdate=`$ndate +06 $cdate`
      if [ $cdate -gt $edate ] ; then
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
cp cov_calc $wrkdir

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
#PBS -A $account
#PBS -o unpack_out
#PBS -e unpack_err
#PBS -q batch
#PBS -l walltime=${unpack_walltime}
#PBS -l procs=1
#PBS -N unpack
#PBS -t 1-${num_jobs}
cd $wrkdir
./jobchoice.sh \${PBS_ARRAYID}
EOF
jobid=$(qsub jobarray.sh)
elif [ $machine = wcoss ] ; then
cat << EOF > jobarray.sh
#!/bin/sh
#BSUB -o unpack_out
#BSUB -e unpack_err
#BSUB -q dev
#BSUB -n 1
#BSUB -W ${unpack_walltime}
#BSUB -R affinity[core]
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
echo $jobid
#check if shifts are needed
if [ $machine = theia ] ; then
cat << EOF > params.sh
#!/bin/sh
#PBS -A $account
#PBS -o sort_out
#PBS -e sort_err
#PBS -q batch
#PBS -l walltime=00:02:00
#PBS -l procs=1
#PBS -N sort_diag
#PBS -W depend=afteranyarray:${jobid}
wrkdir=$wrkdir
ntot=$dattot
EOF
chmod +rwx params.sh
cat sort_diags.sh >> params.sh
mv params.sh sort_diags.sh

jobid=$(qsub sort_diags.sh )
echo $jobid
elif [ $machine = wcoss ] ; then
cat << EOF > params.sh
#!/bin/sh
#BSUB -o sort_out
#BSUB -e sort_err
#BSUB -q dev
#BSUB -n 1
#BSUB -W 02:00
#BSUB -R affinity[core]
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
#PBS -A $account
#PBS -o comp_out
#PBS -e comp_err
#PBS -q batch
#PBS -l walltime=$wall_time
#PBS -l procs=1
#PBS -N cov_calc
#PBS -W depend=afterany:${jobid}
bdate=$bate
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
ntot=$dattot
EOF
chmod +rwx params.sh
cat par_run.sh >> params.sh
mv params.sh par_run.sh
qsub par_run.sh
elif [ $machine = wcoss ] ; then
cat << EOF > params.sh
#!/bin/sh
#BSUB -o comp_out
#BSUB -e comp_err
#BSUB -q dev
#BSUB -n 1
#BSUB -W $wall_time
#BSUB -R affinity[core]
#BSUB -R span[ptile=1]
#BSUB -P ${project_code}
#BSUB -J cov_calc
bdate=$bate
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
ntot=$dattot
EOF
chmod +rwx params.sh
cat par_run.sh >> params.sh
mv params.sh par_run.sh
bsub -w "done(sort_diag)" < par_run.sh
else 
   exit 1
fi
