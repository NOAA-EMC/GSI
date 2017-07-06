DTYPE  station 
options big_endian sequential
STNMAP qsfc.map
UNDEF  -999.0
TITLE  Station Data Sample
TDEF   11 linear 00z27may2006 30mn 
VARS  16
shgt     0  0   the station elsvation(meters)
press    0  0   surface pressure
ohgt     0  0   the observation height(meters)
dtime    0  0   relative time to analysis hours
iqc      0  0   input prepbufr qc or event mark
setqc    0  0   setup qc or event mark
iuse     0  0   read_prepbufr data usage flag
muse     0  0   setup data usage flag
rwgt     0  0   nonlear qc relative weight (weight/0.25)
err      0  0   the original data (bufr table) 1/error
rerr     0  0   the readbufr subroutine data 1/error
ferr     0  0   the final data 1/error
obs      0  0   oberved values
obg      0  0   obs-ges used in analysis
obg_ori  0  0   obs-ges w/o adjustment 
qsges    0  0   guess saturation specific humidity
ENDVARS
