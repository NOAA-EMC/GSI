'open pgbfcs25_prx.ctl'
'open pgbanl25_prx.ctl'

xsize=x640
ysize=y540

'set t 1'
'set lat -90 90'
'set lon -270 90'
'set dfile 1'
'define prxfcs0=ave(tozneclm.1,t=11,t=13)'
'set dfile 2'
'define prxanl0=(tozneclm.2(t=1)+tozneclm.2(t=3)+tozneclm.2(t=5))/3'
'set dfile 1'

'run /nfsuser/g01/wx20rt/grads/scripts/tomscols.v2.gs'

'set t 11 13'
'query time'
date1=subwrd(result,3)
date2=subwrd(result,5)
'set t 1'

'set lat 20 90'
'set lon -270 90'
'set mproj nps'
'set frame circle'

'clear'
'set grads off'
'set gxout shaded'
'set clevs 100 125 150 175 200 225 250 275 300 325 350 375 400 425 450 475 500'
'set ccols  0  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  0'
'd prxfcs0'
'cbarnew.gs'
'set gxout contour'
'set cint 25'
'set clab off'
'set ccolor 1'
'd prxfcs0'
'set string 1 l 6'
'set strsiz 0.18 0.18'
'draw string 0.5 8.3 PRX Total Ozone 120h forecast (DU)'
'draw string 0.5 8.0 Valid:  'date1' to 'date2
'printim prx5o3_nps.png 'xsize' 'ysize' white'

*pull var


'run /nfsuser/g01/wx20rt/grads/scripts/rgbset.gs'
'clear'
'set grads off'
'set gxout shaded'
'set ccols 49 48 47 46 45 44 43 42 41 0 21 22 23 24 25 26 27 28 29'
'set clevs -40 -35 -30 -25 -20 -15 -10 -5 2 2 5 10 15 20 25 30 35 40'
'd prxfcs0-prxanl0'
'cbarnew.gs'
'set gxout contour'
'set clevs -30 -20 -10 10 20 30'
'set clab off'
'set ccolor 1'
'd prxfcs0-prxanl0'
'set string 1 l 6'
'set strsiz 0.18 0.18'
'draw string 0.5 8.3 F120-analysis PRX Total Ozone (DU)'
'draw string 0.5 8.0 Valid:  'date1' to 'date2
'printim prx5_prx0o3_nps.png 'xsize' 'ysize' white'

*pull var


'run /nfsuser/g01/wx20rt/grads/scripts/tomscols.v2.gs'
'set mproj sps'
'set t 1'
'set lon -270 90'
'set lat -90 -20'


'clear'
'set grads off'
'set gxout shaded'
'set clevs 100 125 150 175 200 225 250 275 300 325 350 375 400 425 450 475 500'
'set ccols  0  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  0'
'd prxfcs0'
'cbarnew.gs'
'set gxout contour'
'set cint 25'
'set clab off'
'set ccolor 1'
'd prxfcs0'
'set string 1 l 6'
'set strsiz 0.18 0.18'
'draw string 0.5 8.3 PRX Total Ozone 120h forecast (DU)'
'draw string 0.5 8.0 Valid:  'date1' to 'date2
'printim prx5o3_sps.png 'xsize' 'ysize' white'

*pull var
  
'run /nfsuser/g01/wx20rt/grads/scripts/rgbset.gs'
'clear'
'set grads off'
'set gxout shaded'
'set ccols 49 48 47 46 45 44 43 42 41 0 21 22 23 24 25 26 27 28 29'
'set clevs -40 -35 -30 -25 -20 -15 -10 -5 -2 2 5 10 15 20 25 30 35 40'
'd prxfcs0-prxanl0'
'cbarnew.gs'
'set gxout contour'
'set clevs -30 -20 -10 10 20 30'
'set clab off'
'set ccolor 1'
'd prxfcs0-prxanl0'
'set string 1 l 6'
'set strsiz 0.18 0.18'
'draw string 0.5 8.3 F120-analysis PRX Total Ozone (DU)'
'draw string 0.5 8.0 Valid:  'date1' to 'date2
'printim prx5_prx0o3_sps.png 'xsize' 'ysize' white'

*pull var

*'reinit'
*'open pgbopl_fcs.ctl'
*'open pgbopl_anl.ctl'
'set t 1'
'set lat -90 90'
'set lon -180 180'
'set dfile 1'
'define prxfcs0=ave(tozneclm.1,t=11,t=13)'
'set dfile 2'
'define prxanl0=(tozneclm.2(t=1)+tozneclm.2(t=3)+tozneclm.2(t=5))/3'
'set dfile 1'


'run /nfsuser/g01/wx20rt/grads/scripts/tomscols.v2.gs'

'set t 11 13'
'query time'
date1=subwrd(result,3)
date2=subwrd(result,5)
'set t 1'


'set mproj robinson'
'set lon -180 180'
'set lat -90 90'

'clear'
'set grads off'
'set gxout shaded'
'set clevs 100 125 150 175 200 225 250 275 300 325 350 375 400 425 450 475 500'
'set ccols  0  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  0'
'd prxfcs0'
'cbarnew.gs'
'set gxout contour'
'set cint 25'
'set clab off'
'set ccolor 1'
'd prxfcs0'
'set string 1 l 6'
'set strsiz 0.18 0.18'
'draw string 0.5 8.3 PRX Total Ozone 120h forecast (DU)'
'draw string 0.5 8.0 Valid:  'date1' to 'date2
'printim prx5o3_glb.png 'xsize' 'ysize' white'

*pull var
  
'run /nfsuser/g01/wx20rt/grads/scripts/rgbset.gs'
'clear'
'set grads off'
'set gxout shaded'
'set ccols 49 48 47 46 45 44 43 42 41 0 21 22 23 24 25 26 27 28 29'
'set clevs -40 -35 -30 -25 -20 -15 -10 -5 -2 2 5 10 15 20 25 30 35 40'
'd prxfcs0-prxanl0'
'cbarnew.gs'
'set gxout contour'
'set clevs -30 -20 -10 10 20 30'
'set clab off'
'set ccolor 1'
'd prxfcs0-prxanl0'
'set string 1 l 6'
'set strsiz 0.18 0.18'
'draw string 0.5 8.3 F120-analysis PRX Total Ozone (DU)'
'draw string 0.5 8.0 Valid:  'date1' to 'date2
'printim prx5_prx0o3_glb.png 'xsize' 'ysize' white'

'quit'
