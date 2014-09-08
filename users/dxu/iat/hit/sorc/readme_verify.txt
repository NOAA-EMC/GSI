This directory contains the hurricane statistical verification
programs.  These were copied from Timothy Marchok's directory
/nfsuser/g01/wx20tm/hur/verif2/  
and are based on the latest version of NHC's verification software.  

There are two types of track data files
./yyyy/aalnnyyyy.dat 
./yyyy/balnnyyyy.dat 
where al is for Atlantic, nn is storm number of year yyyy.
aalnnyyyy.dat has model vortex tracker and guidence data
balnnyyyy.dat has Best (observational) track.  
Note that the aalnnyyyy.dat files must have the HWRF model 
tracks already inserted into it.  This is done with the 
program insert_atx_tracks.sh, which puts the tracks in order
(your model HWRF tracks cannot just be appended to the aalnnyyyy.dat file).  

There are four source code files and a makefile 
dataio.f 
dtgutils.f
nhcver.f 
upcase.f 
Makefile
The source code does not contain hardwired pathway names and 
does not need to be changed.  
To compile the program 
make -f Makefile
produces the object codes *.o and the executable 
nhcver.x 
--------------------------------------------------------------------

To produce storm track statistics:   

1.  Put the tracks in a new subdirectory 
/emc2/wx20wc/trak/H001/  
and concatenate and clean
them as necessary with the scripts copied from 
/emc2/wx20wc/trak/TRACKS_ALL/
and see readme_tracks_all.txt in that directory. 
Also, give the tracks a specific number (H001, H002, etc.),
with the script change_name.sh.  
The resulting files will be named  
/emc2/wx20wc/trak/H001/STORM_nodup 
Note that now the directories are named 
/emc2/wx20wc/trak/TRACKS_ALL_GFDL  (Phil Hayes runs H002) 
/emc2/wx20wc/trak/TRACKS_ALL_JW    (John Waldrop's runs H001) 

2. THIS HAS BEEN DONE (ONLY NEED TO DO ONCE AT START).
Get the most recently updated and sorted (or final) adeck and 
bdeck files for all the storms, which are on BLUE.
The adeck files aalnnyyyy.dat are in  
/tpcprd/databases/atcf/noaa/aid_nws/  
The bdeck (best track - observed) files balnnyyyy.dat are in 
/tpcprd/databases/atcf/noaa/btk/

Copy both these adeck and bdeck files to the directory
/emc2/wx20wc/trak/verif2/2005/ 
Copy the bdeck files to the directory  
/emc2/wx20wc/trak/interp 

3. Before the track statistics can be computed for the HWRF model, 
the HWRF tracks must be inserted into the aalnnyyyy.dat files. 

First, as a precaution, copy all the cumulative track files  
/emc2/wx20wc/trak/interp/aalnn2005.dat  
to directory 
/emc2/wx20wc/trak/interp/SAVE/  
This is because the old track file is overwritten by the new trak file. 

To insert the aalnnyyyy,dat files use the script 
/emc2/wx20wc/trak/verif2/insert_atx_tracks.sh 
In this file you must set the pathways and variable names 
BASIN=AL
yyyy=2005  
adeckdir = directory containing the NHC atcf files 
#######          = /emc2/wx20wc/trak/verify2/2005
         = /emc2/wx20wc/trak/interp 
mytracks = full path name of file with your HWRF tracks
         = /emc2/wx20wc/trak/H001/STORM_nodup 
rundir   = directory from which you are running the script 
         = /emc2/wx20wc/trak/interp
newdir   = directory which will contain new output atcf files 
           with HWRF tracks inserted into them. 
         = /emc2/wx20wc/trak/interp 
Usually, you just need to change the STORM_nodup file of mytracks. 

run the script with the command  
llsubmit  /emc2/wx20wc/trak/verif2/insert_atx_tracks.sh 
This will insert the HWRF tracks into the atcf file and put the 
resulting file 
/emc2/wx20wc/trak/interp/aalnnyyyy.dat   
You can compare this file with the corresponding one 
##########(only first time) #####/emc2/wx20wc/trak/verify2/2005/aalnnyyyy.dat 
/emc2/wx20wc/trak/interp/SAVE/aalnnyyyy.dat   
to see that the HWRF tracks were put in properly.  

If you want to make hurricane trak plots of these runs, copy these files 
/emc2/wx20wc/trak/interp/aalnnyyyy.dat   
to 
/emc2/wx20wc/trak/plot/aalnnyyyy.dat 
and then use the GRADS plotting script as before 
sh trakplot.sh  al 
Note that to plot tracks such as H001, H002, the GRADS script 
/emc2/wx20wc/trak/plot/trakplot.gs  
needs to be modified for these names.  

4. Run the track verification program.  The verification program 
can be used for three types of data
  (1) tracks,    with input file testcard.t
  (2) intensity, with input file testcard.i
  (3) radii,     with input file testcard.r   

Only one of these files is run at a time. In the chosen file 
testcard.t, testcard.i, testcard.r, specify the following:  
  (1) Specify the list and number of technique (model) names to process, with 
  a maximum of 12.  Each name must be 4 characters, including a 
  blank space on the left postion if necessary.  
  For example, for the number 7 
HWRF GFDL AVN0  UKM NGPS CLP5 OFCL 
  where
  HWRF = Hurricane WRF model  
  AVN0 = Global Forecasting System (formerly AVIATION)
  GFDL = Geophysical Fluid Dynamics Model
  CLP5 = Climatology and Persistence 
  OFCL = official forecast from NHC 
  UKM  = UK Met office 
  NGPS = NOGAPS Navy model 

  (2) Specify the list of storm IDs to be processed, for example 
for the HWRF 2005 season 
  al012005 
  al042005 
  al052005  
  al062005  
  al082005  
  al092005  
  al122005  
  al162005  
  al182005  
  al212005  
  al242005  

  (3) Specify other information, such as which data is to be written
  out, with the logical .true. 

The executable has three arguments that are specified:   
  (1) the desired testcard file to be read in for track (testcard.t), 
  intensity (testcard.i), or radius (testcard.r).
  (2) the desired name of the output file. The program will
  automatically created an output file that is this specified name
  with the suffix .out
  (3) the directory that contains the adeck and bdeck files 

For example, to execute the program for the track verification, 
use the command 
./nhcver.x testcard.t testcard.t  /emc2/wx20wc/trak/interp 

and the output file will be the second variable name with the 
suffix .out, in this directory   
/emc2/wx20wc/trak/verif2/testcard.t.out   

To execute the program for intensity verification, use the command 
./nhcver.x testcard.i testcard.i  /emc2/wx20wc/trak/interp

and the outut file will be the second variable name with the 
suffix .out, in this directory 
/emc2/wx20wc/trak/verif2/testcard.i.out   

To save these files, copy them to the appropriate subdirectory
./HWRF/ 
./H001/  
and rename them with a name describing the storms analysed. 
