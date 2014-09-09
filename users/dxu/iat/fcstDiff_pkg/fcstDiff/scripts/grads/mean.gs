'open /ptmpc/rl23ss/runkan/prateclm.ctl'
'open /reanl2/msurain/prateclm.ctl'
*
'run /wd5/wd51/wd51js/rgbset.gs'
'set display color white'
'clear'
*
'enable print out.gr'
*
'define exp=prate.1(t=1)*86400'
'define obs=prate.2(t=11)*86400'
'define anm=exp-obs'
*
'set clevs -16 -8 -4 -2  2 4  8 16 32'
'set ccols  29 27 25 23 0 33 35 37 39 44'
'set grads off'
'set gxout shaded'
'd anm'
'run /wd2/wd23/wd23ss/cbar.gs'
'draw title Nov 1997 Precip Anom (mm/day)'
*
'print'
