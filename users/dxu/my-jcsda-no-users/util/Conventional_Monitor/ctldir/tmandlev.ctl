DTYPE  station 
options big_endian sequential
STNMAP t120.map 
UNDEF  -999.0
TITLE  Station Data Sample
TDEF   1 linear 00z27may2006 12hr
* ZDEF mandatary level 1000,925,850,700,500,400,300,250,200,150,100,70,50
VARS 15 
shgt     1  0   the station elsvation(meters)  
press    1  0   surface pressure   
ohgt     1  0   the observation height(meters)  
dtime    1  0   relative time to analysis hours
iqc      1  0   input prepbufr qc or event mark
setqc    1  0   setup qc or event mark
iuse     1  0   read_prepbufr data usage flag
muse     1  0   setup data usage flag
rwgt     1  0   nonlear qc relative weight (weight/0.25)
err      1  0   the original data (bufr table) 1/error
rerr     1  0   the readbufr subroutine data 1/error
ferr     1  0   the final data 1/error
obs      1  0   oberved values
obg      1  0   obs-ges used in analysis 
obg_ori  1  0   obs-ges w/o adjustment 
ENDVARS
