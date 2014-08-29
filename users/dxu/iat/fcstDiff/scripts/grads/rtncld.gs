'open /tmp/wd23sm/rt62hr0012.ctl'
*
'run /emcsrc3/wd23sm/scripts/rgbset.gs'
'set display color white'
'clear'
*
'enable print out.gr'
*
' set lon 0 360'
' set lat -90 90'
*
*  Plotting total cloudiness
*
'set parea 1.0 5.5  4.5 7.5'
'set t 1'
'set grads off'
'set gxout shaded'
'set clevs -0.001 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9'
'set ccols 59 49 48 47 46 45 44 43 42 41 0'
'd cltotday'
'draw ylab Latitude'
'set string 1 tl 4 0'
'set strsiz 0.1'
'draw string 1.0 7.60 (a)'
'set string 1 tr 4 0'
'set strsiz 0.1'
'draw string 5.5 7.60 98101812'
*
'set parea 6.0 10.5  4.5 7.5'
*
'set grads off'
'set gxout shaded'
'set t 2'
'set clevs -0.001 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9'
'set ccols 59 49 48 47 46 45 44 43 42 41 0'
'd cltotday'
'set strsiz 0.1'
'set string 1 tl 4 0'
'draw string 6.0 7.60 (b)'
'set string 1 tr 4 0'
'draw string 10.5 7.60 98101900'
*
'set parea 1.0 5.5  0.9 3.9'
*
'set grads off'
'set gxout shaded'
'set t 3'
'set clevs -0.001 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9'
'set ccols 59 49 48 47 46 45 44 43 42 41 0'
'd cltotday'
*'run /emcsrc3/wd23sm/scripts/cbarnew.gs 0.75 0 3.25 4.2'
'draw ylab Latitude'
'draw xlab Longitude'
*
'set strsiz 0.1'
'set string 1 tl 4 0'
'draw string 1.0 4.00 (c)'
'set string 1 tr 4 0'
'draw string 5.5 4.00 98101912'
*
'set parea 6.0 10.5  0.9 3.9'
*
*
'set grads off'
'set gxout shaded'
'set t 4'
'set clevs -0.001 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9'
'set ccols 59 49 48 47 46 45 44 43 42 41 0'
'd cltotday'
*'run /emcsrc3/wd23sm/scripts/cbarnew.gs 0.75 0 8.25 4.2'
*
'draw xlab Longitude'
*
'set strsiz 0.1'
'set string 1 tl 4 0'
'draw string 6.0 4.00 (d)'
'set string 1 tr 4 0'
'draw string 10.5 4.00 98102000'
'set strsiz 0.2'
'set string 1 tc 6 0'
'draw string 5.5 8.0 Total Cloud cover from RTNEPH'
*
'run /emcsrc3/wd23sm/scripts/cbarnew.gs 0.75 0 5.5 4.2'
*
'print'
*'c'
