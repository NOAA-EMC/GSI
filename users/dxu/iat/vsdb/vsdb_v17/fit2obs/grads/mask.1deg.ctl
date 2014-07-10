dset ^mask.1deg.ieee
undef 9.999E+20
title mask
options big_endian yrev
xdef 360 linear   -0. 1
ydef 181 linear  -90. 1
tdef 1 linear 00Z22jul1998 1mo
zdef 1 linear 1 1
vars 1
mask     0 81,1,0  ** Land-sea mask (land=1;sea=0) [fraction]
ENDVARS
