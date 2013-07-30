'open pgbanl_gfs.ctl'
'open pgbanl_prx.ctl'

'run /nfsuser/g01/wx20rt/grads/scripts/tomscols.v2.gs'

xsize=x640
ysize=y480

'set t 1 5'
'query time'
date1=subwrd(result,3)
date2=subwrd(result,5)

'set lat -90 90'
'set lon -271 91'
'define gfsanl=tozneclm.1'
'define prxanl=tozneclm.2'


'set t 1'
'define gfsanl0=ave(gfsanl,t=1,t=5)'
'define prxanl0=ave(prxanl,t=1,t=5)'

'set lat 20 90'
'set lon -270 90'
'set mproj nps'
'set frame circle'

'clear'
'set grads off'
'set gxout shaded'
'set clevs 100 125 150 175 200 225 250 275 300 325 350 375 400 425 450 475 500'
'set ccols  0  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65 0'
'd gfsanl0'
'cbarnew.gs'
'set gxout contour'
'set cint 25'
'set clab off'
'set ccolor 1'
'd gfsanl0'
'set string 1 l 6'
'set strsiz 0.19 0.18'
'draw string 0.5 8.3 GFS Total Ozone analysis (DU)'
'draw string 0.5 8.0 Valid:  'date1' to 'date2
'printim gfso3_nps.png 'xsize' 'ysize' white'


*pull var

'clear'
'set grads off'
'set gxout shaded'
'set clevs 100 125 150 175 200 225 250 275 300 325 350 375 400 425 450 475 500'
'set ccols   0  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  0'
'd prxanl0'
'cbarnew.gs'
'set gxout contour'
'set cint 25'
'set clab off'
'set ccolor 1'
'd prxanl0'
'set string 1 l 6'
'set strsiz 0.18 0.18'
'draw string 0.5 8.3 PRX Total Ozone analysis (DU)'
'draw string 0.5 8.0 Valid:  'date1' to 'date2
'printim prxo3_nps.png 'xsize' 'ysize' white'



*pull var


'run /nfsuser/g01/wx20rt/grads/scripts/rgbset.gs'
'clear'
'set grads off'
'set gxout shaded'
'set ccols 49 48 47 46 45 44 43 42 41 0 21 22 23 24 25 26 27 28 29'
'set clevs -40 -35 -30 -25 -20 -15 -10 -5 -2 2 5 10 15 20 25 30 35 40'
'd prxanl0-gfsanl0'
'cbarnew.gs'
'set gxout contour'
'set clevs -30 -25 -20 -15 -10 -5 5 10 15 20 25 30'
'set clab off'
'set ccolor 1'
'd prxanl0-gfsanl0'
'set string 1 l 6'
'set strsiz 0.18 0.18'
'draw string 0.5 8.3 PRX-GFS Total Ozone analysis (DU)'
'draw string 0.5 8.0 Valid:  'date1' to 'date2
'printim prx_gfso3_nps.png 'xsize' 'ysize' white'

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
'd gfsanl0'
'cbarnew.gs'
'set gxout contour'
'set cint 25'
'set clab off'
'set ccolor 1'
'd gfsanl0'
'set string 1 l 6'
'set strsiz 0.19 0.18'
'draw string 0.5 8.3 GFS Total Ozone analysis (DU)'
'draw string 0.5 8.0 Valid:  'date1' to 'date2
'printim gfso3_sps.png 'xsize' 'ysize' white'

*pull var


'clear'
'set grads off'
'set gxout shaded'
'set clevs 100 125 150 175 200 225 250 275 300 325 350 375 400 425 450 475 500'
'set ccols  0  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  0'
'd prxanl0'
'cbarnew.gs'
'set gxout contour'
'set cint 25'
'set clab off'
'set ccolor 1'
'd prxanl0'
'set string 1 l 6'
'set strsiz 0.18 0.18'
'draw string 0.5 8.3 PRX Total Ozone analysis (DU)'
'draw string 0.5 8.0 Valid:  'date1' to 'date2
'printim prxo3_sps.png 'xsize' 'ysize' white'

*pull var
  
'run /nfsuser/g01/wx20rt/grads/scripts/rgbset.gs'
'clear'
'set grads off'
'set gxout shaded'
'set ccols 49 48 47 46 45 44 43 42 41 0 21 22 23 24 25 26 27 28 29'
'set clevs -40 -35 -30 -25 -20 -15 -10 -5 -2 2 5 10 15 20 25 30 35 40'
'd prxanl0-gfsanl0'
'cbarnew.gs'
'set gxout contour'
'set clevs -30 -25 -20 -15 -10 -5 5 10 15 20 25 30'
'set clab off'
'set ccolor 1'
'd prxanl0-gfsanl0'
'set string 1 l 6'
'set strsiz 0.18 0.18'
'draw string 0.5 8.3 PRX-GFS Total Ozone analysis (DU)'
'draw string 0.5 8.0 Valid:  'date1' to 'date2
'printim prx_gfso3_sps.png 'xsize' 'ysize' white'
'print'
'disable print'


'quit'
