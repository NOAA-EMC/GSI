dset fitname
options sequential 
undef 0.00           
title EXP                                                                      
xdef       7 linear    1.000  1.000
ydef  ntimes linear    1.000  1.000
zdef       1 linear    1      1
tdef   20  linear  00z01jan2001 fitinc                 
vars   15   
tcnt       1 0 Temp Counts
trmse      1 0 Temp RMS Values
tbias      1 0 Temp Bias Values
zcnt       1 0 Height Counts
zrmse      1 0 Height RMS Values
zbias      1 0 Height Bias Values
wcnt       1 0 Counts
wrmse      1 0 Mean [ (uf-uo)**2 + (vf-vo)**2 ] Speed Bias Term
wbias      1 0 RMS Vector Wind Error
qcnt       1 0 Moisture Counts
qrmse      1 0 Moisture RMS Values
qbias      1 0 Moisture Bias Values
pcnt       1 0 Moisture Counts
prmse      1 0 Moisture RMS Values
pbias      1 0 Moisture Bias Values
endvars
