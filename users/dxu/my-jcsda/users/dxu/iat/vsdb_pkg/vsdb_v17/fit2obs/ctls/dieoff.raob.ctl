dset fitname
options sequential 
undef 0.00           
title EXP                                                                      
xdef       7 linear    1.000  1.000
ydef  ntimes linear    1.000  1.000
zdef      21 levels 1000 925 850 700 500 400 300 250 200 150 100 70 50 30 20 10 7 5 3 2 1
tdef      20 linear  00z01jan2001 fitinc                 
vars   12   
tcnt       21 0 Temp Counts
trmse      21 0 Temp RMS Values
tbias      21 0 Temp Bias Values
zcnt       21 0 Height Counts
zrmse      21 0 Height RMS Values
zbias      21 0 Height Bias Values
wcnt       21 0 Counts
wrmse      21 0 Mean [ (uf-uo)**2 + (vf-vo)**2 ] Speed Bias Term
wbias      21 0 RMS Vector Wind Error
qcnt       21 0 Moisture Counts
qrmse      21 0 Moisture RMS Values
qbias      21 0 Moisture Bias Values
endvars
