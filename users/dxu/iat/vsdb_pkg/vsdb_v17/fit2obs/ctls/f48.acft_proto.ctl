dset dir/f48.acft.%y4%m2%d2%h2
options ENDIAN sequential template
undef 000000
title EXP1                                                                      
xdef     7 linear    1.000  1.000
ydef     1 linear    1.000  1.000
zdef     3 levels     1000   700   300
tdef   1000 linear            00zdate     24hr
vars    27
tcnt       3 0 Temp Counts
tf         3 0 Temp Mean [f] Forecast Values
to         3 0 Temp Mean [o] Observed Values
tfo        3 0 Temp Mean [f*o] Values
tfs        3 0 Temp Mean [f**2] Values
tos        3 0 Temp Mean [o**2] Values
zcnt       3 0 Height Counts
zf         3 0 Height Mean [f] Forecast Values
zo         3 0 Height Mean [o] Observed Values
zfo        3 0 Height Mean [f*o] Values
zfs        3 0 Height Mean [f**2] Values
zos        3 0 Height Mean [o**2] Values
wcnt       3 0 Counts
uf         3 0 Mean [uf] u-wind Forecast Values
vf         3 0 Mean [vf] v-wind Forecast Values
uo         3 0 Mean [uo] u-wind Observed Values
vo         3 0 Mean [vo] v-wind Observed Values
uv         3 0 Mean [ uf*uo + vf*vo ] Cross Product
uvf        3 0 Mean [ uf**2 + vf**2 ] Squared Forecast Values
uvo        3 0 Mean [ uo**2 + vo**2 ] Squared Observed Values
spd        3 0 Mean [ (uf-uo)**2 + (vf-vo)**2 ] Speed Bias Term
qcnt       3 0 Moisture Counts
qf         3 0 Moisture Mean [f] Forecast Values
qo         3 0 Moisture Mean [o] Observed Values
qfo        3 0 Moisture Mean [f*o] Values
qfs        3 0 Moisture Mean [f**2] Values
qos        3 0 Moisture Mean [o**2] Values
endvars
