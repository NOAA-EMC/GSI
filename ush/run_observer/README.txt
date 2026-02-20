Script to run the GSI observer in standalone mode.
--------------------------------------------------

To get inital files from operations:

> sbatch rungsi_v17_dev.sh

You will need to set bdate in this script to the correct date.



To run the standalone code:

> sbatch rungsi_v17_dev.sh


This is basically a modified exglobal_atmos_analysis.sh.   
To get started you will need to modify some of the following lines:
-----
# Path to previously built GSI is here
GSIDIR=/scratch3/NCEPDEV/da/${USER}/git/GSI_develop    <<<<<<<<<<
machine=ursa                                           <<<<<<<<<<

# Set experiment name and analysis date
adate=2025041500     
exp=v17-dev.$adate

