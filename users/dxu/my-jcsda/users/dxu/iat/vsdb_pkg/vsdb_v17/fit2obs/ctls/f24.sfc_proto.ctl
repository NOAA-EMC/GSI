dset dir/f24.sfc.%y4%m2%d2%h2
options ENDIAN sequential template
undef 000000
title EXP1                                                                      
xdef     7 linear    1.000  1.000
ydef     2 linear    1.000  1.000
zdef     1 linear    1.000  1.000
tdef   1000 linear            00zdate     24hr
vars    6
pcnt       1 99 Surface Pressure Counts
pf         1 99 Surface Pressure Mean [f] Forecast Values
po         1 99 Surface Pressure Mean [o] Observed Values
pfo        1 99 Surface Pressure Mean [f*o] Values
pfs        1 99 Surface Pressure Mean [f**2] Values
pos        1 99 Surface Pressure Mean [o**2] Values
endvars
