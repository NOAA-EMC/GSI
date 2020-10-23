## GSI/feature/jedi_gfsv16: a version of GSI to generate GeoVaLs for JEDI UFO development

This is a modified version of the Gridpoint Statistical Interpolation (GSI) software that includes the writing out of interpolated model background fields and other variables needed to run the JEDI Unified Forward Operator (UFO) without needing the full 3D model state.

### To build
Just like standard GSI, on supported platforms:
- git clone --recursive -b feature/jedi_gfsv16 https://github.com/CoryMartin-NOAA/GSI.git GSI_forJEDI_GFSv16
- cd GSI_forJEDI_GFSv16/ush
- ./build_all_cmake.sh

Verify you have global_gsi.x in the GSI_forJEDI_GFSv16/exec directory.

### To generate GSI netCDF diag files with extra model fields included
A slurm submission script has been provided that should run for most people on Hera.

ush/JEDI/submit_run_gsi.sh

This script will generate a YAML configuration file and then runs ush/JEDI/run_gsi_observer.sh.

Things to change in submit_run_gsi.sh include:
- GSIDir: path to where you cloned this repository to
- adate: YYYYMMDDHH analysis date
- format: nemsio or netcdf depending on model backgrounds
- gfsv16: true or false; depending on if atmos/ subdirectory
- guessroot: path to $ROTDIR
- jcap, jcap_b, levs: hopefully self explanatory, set to match your background files
- dump: gdas or gfs

Other things one can change
- RootWork: defaults to stmp2 but can be anywhere
- obsdir: default is glopara global dump
- rstprod: false, needs to be false if uploading anything off of Hera/WCOSS
- cleanup: true, will delete working directories when finished

After making your changes, submit the script:
sbatch submit_run_gsi.sh

### To convert these GSI netCDF diag files to JEDI UFO GeoVaLs files and IODA observation files

At the end of submit_run_gsi.sh, by default, the script will then submit submit_run_iodaconv.sh.
This is to save resources, as the IODA-converters script does not require more than one node.

Things specific to the IODA-converters portion
- iodaconvbuild: path to the build directory of the ioda-converters
- JEDImodule: path to a modulefile to source to get the JEDI environment
Other things you will still need to change
- adate: YYYYMMDDHH analysis date (or it gets this from $1)
- rootwork: if you changed this before, need to be consistent!

To run this again, without running GSI, just submit the file to slurm.
sbatch submit_run_iodaconv.sh YYYYMMDDHH

### Summary of changes made between NOAA-EMC/GSI/release/gfsda.v16.0.0 and this branch feature/jedi_gfsv16

### Example of where/how to add additional fields to GSI/ioda-converters
