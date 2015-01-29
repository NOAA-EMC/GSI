dset ^plottrak.grib
options yrev
dtype grib
index ^plottrak.ix
undef -9.99E+33
title AVN FORECAST
xdef  360 linear    0.0 1.0
ydef  181 linear  -90.0 1.0
zdef  16 levels 1000 925 850 700 500 400 300 250 200 150 100 70 50 30 20 10
tdef  15 linear  06Z02AUG2001 06hr 
vars 1
u     16  33,100,0  u wind (m/s)
endvars

