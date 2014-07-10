dset fitname
options sequential 
undef 0.00           
title EXP                                                                      
xdef       1 linear    1.000  1.000
ydef  ntimes linear    1.000  1.000
zdef       3 levels    1000  700  300
tdef      20 linear  00z01jan2001 fitinc                 
vars   12   
tcnt       3 0 Temp Counts
trmse      3 0 Temp RMS Values
tbias      3 0 Temp Bias Values
zcnt       3 0 Height Counts
zrmse      3 0 Height RMS Values
zbias      3 0 Height Bias Values
wcnt       3 0 Counts
wrmse      3 0 Mean [ (uf-uo)**2 + (vf-vo)**2 ] Speed Bias Term
wbias      3 0 RMS Vector Wind Error
qcnt       3 0 Moisture Counts
qrmse      3 0 Moisture RMS Values
qbias      3 0 Moisture Bias Values
endvars
