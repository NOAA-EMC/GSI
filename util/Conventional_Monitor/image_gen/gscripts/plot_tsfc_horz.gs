******
*--- initialize

'reinit'

'rgbset2'
'set background 99'
'page'
'c'

plotfile2=PLOT2
plotfile=PLOTFILE
xsize=XSIZE
ysize=YSIZE
rdate=RDATE
nt=NT


** dindex=1, plot all station data
** dindex=-1 plot assimilated data

'open anal.ctl'
'open guess.ctl'
'open 'plotfile2'.grads.anl.ctl'
'open 'plotfile2'.grads.ges.ctl'

if(plotfile = t181 | plotfile = t182 | plotfile = t183 | plotfile = t187);nhe=5;endif
if(plotfile = t180);nhe=10;endif

he=1
while(he<=nhe)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,nt)
he=he+1
endwhile

function plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,nt)
dbug=0

'rgbset2'
'set background 99'
'page'
'c'

'run setvpage 1 2 2 2 0.9'
setmap(plotfile,he)
'set gxout shaded'
'defint.gs  0.5 0'
'd TMPprs.1-TMPprs.2'
'colorbar.gs'
'set ccolor 6'
'set digsiz 0.12'
'd maskout(obg.4(t='nt'),muse.4(t='nt'))'
'set ccolor 4'
'd maskout(obg.4(t='nt'),-muse.4(t='nt'))'
'set ccolor 98'
'draw title 'plotfile2' INCT and OBS-GUESS(red-used,blue-rej,C)'

'run setvpage 2 2 2 2 0.9'
setmap(plotfile,he)
'set gxout shaded'
'defint.gs  0.5 0'
'd TMPprs.1-TMPprs.2'
'colorbar.gs'
*'set ccolor 98'
'set ccolor 6'
'set digsiz 0.12'
'd maskout(obg.3(t='nt'),muse.3(t='nt'))'
'set ccolor 4'
'd maskout(obg.3(t='nt'),-muse.3(t='nt'))'
'set ccolor 98'
'draw title 'plotfile2' INCT and OBS-ANAL(red-used,blue-rej,C)'

'run setvpage 1 1 2 2 0.9'
setmap(plotfile,he)
'set gxout shaded'
'defint.gs  0.5 0'
'd (TMPprs.1-TMPprs.2)'
'colorbar.gs'
'set ccolor 98'
'draw title Temperature INCT(C) at 'rdate'z'

'run setvpage 2 1 2 2 0.9'
setmap(plotfile,he)
'set csmooth on'
'set lev 1'
'set gxout shaded'
'set cterp off'
'set  rbcols 49 48 47 46 45 44 43 42 41 81 21 22 23 24 25 26 27 28 29'
'd TMPprs.1-273.15'
'set digsiz 0.12'
'set gxout contour'
'set clevs'
'set ccolor 98'
*'set cint 0'
'd TMPprs.1-273.15'
'colorbar.gs'
'set ccolor 98'
'draw title Surface Temperature analysis(C) at 'rdate'z'

if(dbug=1)
 say 'hit enter to continue'
    pull var
endif


'printim 'plotfile2'_region'he'.png 'xsize' 'ysize


return

function setmap(plotfile,he)
'set annot 98'
'set line 98'
'set map 98'
'set xlopts 98'
'set ylopts 98'
if(he = 1)
'set lat -90 90';'set lon 0 360'
endif
if(he=2)
if(plotfile = t180);'set lat 30 70'; 'set lon 0 75';endif
if(plotfile = t181 | plotfile = t182 | plotfile = t183 | plotfile = t187);'set lat 0 90'; 'set lon 0 180';endif
endif
if(he=3)
if(plotfile = t180);'set lat -35 30'; 'set lon 0 120';endif
if(plotfile = t181 | plotfile = t182 | plotfile = t183 | plotfile = t187);'set lat 0 90'; 'set lon 180 360';endif
endif
if(he=4)
if(plotfile = t180);'set lat 0 60'; 'set lon 120 240';endif
if(plotfile =t181 | plotfile = t182 | plotfile = t183 | plotfile = t187);'set lat -90 0'; 'set lon 0 180';endif
endif
if(he=5)
if(plotfile = t180);'set lat 0 30'; 'set lon 240 300';endif
if(plotfile = t181 | plotfile = t182 | plotfile = t183 | plotfile = t187);'set lat -90 0'; 'set lon 180 360';endif
endif
if(he=6)
if(plotfile = t180);'set lat 30 60'; 'set lon 240 300';endif
endif
if(he=7)
if(plotfile = t180);'set lat 0 32'; 'set lon 300 360';endif
endif
if(he=8)
if(plotfile = t180);'set lat 32 65'; 'set lon 300 360';endif
endif
if(he=9)
if(plotfile = t180);'set lat -60 0'; 'set lon 120 240';endif
endif
if(he=10)
if(plotfile = t180);'set lat -60 0'; 'set lon 240 360';endif
endif

return

