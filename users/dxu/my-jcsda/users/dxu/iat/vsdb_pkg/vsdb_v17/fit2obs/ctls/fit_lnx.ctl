dset ^fitname
options sequential 
undef -99999.9 
title EXP1                                                                      
xdef     7 linear    1.000  1.000
ydef     1 linear    1.000  1.000
zdef    21 levels 
    1000     925     850     700     500     400     300     250     200
     150     100      70      50      30      20      10   7  5  3  2  1
tdef   365 linear            hhzddcmonyyyy     incrhr
vars    34
tcnt       21 0 Temp Counts
tf         21 0 Temp Mean [f] Forecast Values
to         21 0 Temp Mean [o] Observed Values
tfo        21 0 Temp Mean [f*o] Values
tfs        21 0 Temp Mean [f**2] Values
tos        21 0 Temp Mean [o**2] Values
trmse      21 0 Temp RMS Values
tbias      21 0 Temp Bias Values
zcnt       21 0 Height Counts
zf         21 0 Height Mean [f] Forecast Values
zo         21 0 Height Mean [o] Observed Values
zfo        21 0 Height Mean [f*o] Values
zfs        21 0 Height Mean [f**2] Values
zos        21 0 Height Mean [o**2] Values
zrmse      21 0 Height RMS Values
zbias      21 0 Height Bias Values
wcnt       21 0 Counts
uf         21 0 Mean [uf] u-wind Forecast Values
vf         21 0 Mean [vf] v-wind Forecast Values
uo         21 0 Mean [uo] u-wind Observed Values
vo         21 0 Mean [vo] v-wind Observed Values
uv         21 0 Mean [ uf*uo + vf*vo ] Cross Product
uvf        21 0 Mean [ uf**2 + vf**2 ] Squared Forecast Values
uvo        21 0 Mean [ uo**2 + vo**2 ] Squared Observed Values
wbias      21 0 Mean [ (uf-uo)**2 + (vf-vo)**2 ] Speed Bias Term
wrmse      21 0 RMS Vector Wind Error
qcnt       21 0 Moisture Counts
qf         21 0 Moisture Mean [f] Forecast Values
qo         21 0 Moisture Mean [o] Observed Values
qfo        21 0 Moisture Mean [f*o] Values
qfs        21 0 Moisture Mean [f**2] Values
qos        21 0 Moisture Mean [o**2] Values
qrmse      21 0 Moisture RMS Values
qbias      21 0 Moisture Bias Values
endvars
