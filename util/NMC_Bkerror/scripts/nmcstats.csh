#!/bin/csh -x

#PBS -N nmcstats
#PBS -l walltime=03:00:00
#PBS -l nodes=150:ppn=3
#PBS -q urgent
#PBS -A da-cpu
#PBS -j oe

# Set NMC statistics utility directory and executable
setenv GSIDIR /scratch4/NCEPDEV/global/save/$USER/svn/gsi/branches/EXP-fv3gsi
setenv CALCSTATS_EXEC $GSIDIR/util/NMC_Bkerror/sorc/calcstats.exe

# Set Input Resolution and path to lagged pairs database
setenv JCAPIN 574
setenv PERTURBDIR /scratch4/NCEPDEV/stmp4/Daryl.Kleist/bkerrdat

# Set Output Resolution
setenv JCAP 766
setenv NLAT 770
setenv NLON 1536
setenv LEVS 64

# Number of cases to calculate statistics from
setenv MAXCASES 450

# Create a temporary working directory
setenv DATA /scratch3/NCEPDEV/stmp1/$USER/tmp/nmcstats_T${JCAP}_n${MAXCASES}_new
if ( -d $DATA ) rm -rf $DATA
mkdir -p $DATA
cd $DATA

module unload intel impi
module load intel impi

# Namelist
rm -f stats.parm
cat << EOF > stats.parm
&namstat
    jcapin=$JCAPIN,
    jcap=$JCAP,
    jcapsmooth=$JCAP,
    nsig=$LEVS,
    nlat=$NLAT,
    nlon=$NLON,
    maxcases=$MAXCASES,
    hybrid=.true.,
    smoothdeg=0.5,
    biasrm=.true.,
    vertavg=.true.
/
EOF

# Link perturbation database
rm -f infiles
touch infiles
set vdate = 2010041900
set edate = 2011043018
while ( $vdate <= $edate )
    set adate = `advance_time $vdate -24h`
    set f24 = $PERTURBDIR/sigf24.gfs.$adate
    set adate = `advance_time $vdate -48h`
    set f48 = $PERTURBDIR/sigf48.gfs.$adate
    if ( ( -f $f24 ) & ( -f $f48 ) ) then
        ls $f24 >> infiles
        ls $f48 >> infiles
    endif
    set vdate = `advance_time $vdate +18h`
end
ln -sf infiles fort.10

# Copy the executable and link necessary files
if ( ! -f $CALCSTATS_EXEC ) then
    echo "$CALCSTATS_EXEC not found, ABORT!"
    exit 1
endif
cp -f $CALCSTATS_EXEC                                   ./calcstats.exe
cp -f $GSIDIR/util/NMC_Bkerror/fix/sst2dvar_stat0.5.ufs ./berror_sst

setenv MPI_BUFS_PER_PROC 1024
setenv MPI_BUFS_PER_HOST 1024
setenv MPI_GROUP_MAX     1024
setenv OMP_NUM_THREADS   1

setenv APRUN "mpirun -np $PBS_NP"

$APRUN calcstats.exe |& tee calcstats.out
if ( $status ) exit $status

rm -f fort.[0-9]*

exit 0
