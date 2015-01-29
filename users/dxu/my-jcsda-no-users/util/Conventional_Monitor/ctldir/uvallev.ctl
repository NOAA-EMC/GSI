DTYPE  station 
options big_endian sequential
STNMAP uvsig.map 
UNDEF  -999.0
TITLE  Station Data Sample
TDEF   1 linear 00z22jun2006 12hr
* ZDEF  915.,840.,690.,590.,490.,390.,290.,240.,190.,90    
VARS 19 
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
obsu     1  0   oberved u values
obgu     1  0   obs-ges u used in analysis 
obgu_ori 1  0   obs-ges u w/o adjustment 
obsv     1  0   oberved v values
obgv     1  0   obs-ges v used in analysis 
obgv_ori 1  0   obs-ges v w/o adjustment 
factw    1  0   10m wind reduction factor 
ENDVARS
