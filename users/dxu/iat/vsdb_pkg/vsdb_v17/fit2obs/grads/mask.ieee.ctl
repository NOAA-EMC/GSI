dset ^mask.ll.ieee
options yrev big_endian
undef -9.99E+33
title NCEP/NCAR REANALYSIS PROJECT
xdef   144 linear    0.000  2.500
ydef    73 linear  -90.000  2.500
zdef 1 linear  1 1
tdef 1 linear   00z01jan1900 1mo          
vars     1
mask     1 99 Land-Sea Mask  (land=1; sea=0) Fraction
endvars
