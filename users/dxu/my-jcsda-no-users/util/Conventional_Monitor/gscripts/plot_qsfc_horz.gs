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
hint=HINT
nt=NT


** dindex=1, plot all station data
** dindex=0 plot assimilated data
** dindex=-1 plot not assimilated data 
** this script plots air craft data (915.,840.,690.,590.,490.,390.,290.,240.,190.,90.)
** the data plotted is 2*hint interval  915-935
** and the rawinsonde whose height same as defined, hint=0 
'open anal.ctl'
'open guess.ctl'
'open 'plotfile2'_grads_anl.ctl'
'open 'plotfile2'_grads_ges.ctl'
say  'open the file'

if (plotfile = q180 | plotfile = q183); nhe=5;endif
if (plotfile = q181 | plotfile = q187); nhe=23;endif
he=1
while (he <=nhe)
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
'd 10*(RH2m.1-RH2m.2)/RHprs.1'
'cbarb'
*'set ccolor 98'
'set ccolor 6'
'set digsiz 0.12'
'd maskout(10*obg.4(t='nt')/obs.4(t='nt'),muse.4(t='nt'))'
'set ccolor 4'
'd maskout(10*obg.4(t='nt')/obs.4(t='nt'),-muse.4(t='nt'))'
'set ccolor 98'
'draw title 'plotfile2' INCT*10/Anal and 10*(OBS-GUESS)/OBS(red-used,blue-rej) '

'run setvpage 2 2 2 2 0.9'
setmap(plotfile,he)
'set gxout shaded'
'defint.gs  0.5 0'
'd 10*(RH2m.1-RH2m.2)/RHprs.1'
'cbarb'
'set ccolor 6'
'set digsiz 0.12'
'd maskout(10*obg.3(t='nt')/obs.3(t='nt'),muse.3(t='nt'))'
'set ccolor 4'
'd maskout(10*obg.3(t='nt')/obs.3(t='nt'),0-muse.3(t='nt'))'
'set ccolor 98'
'draw title 'plotfile2' INCT*10/Anal and 10*(OBS-ANAL)/OBS(red-used,blue-rej) '

'run setvpage 1 1 2 2 0.9'
setmap(plotfile,he)
'set gxout shaded'
'defint.gs  0.5 0'
'd 10*(RH2m.1-RH2m.2)/RHprs.1'
'cbarb'
'set ccolor 98'
'draw title Humidity INCT*10/Anal at 'rdate'z' 

'run setvpage 2 1 2 2 0.9'
setmap(plotfile,he)
'set gxout shaded'
'defint 10 50'
'd RH2m.1'
'cbarb'
'draw title Surface Humidity Analysis(RH,%) at 'rdate'z'
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
if(he=1)
'set lat -90 90';'set lon 0 360'
endif
if(he = 2)
if(plotfile = q180 | plotfile = q183); 'set lat 0 90'; 'set lon 0 180';endif
if(plotfile = q181 | plotfile = q187); 'set lat 0 35'; 'set lon 0 60' ;endif 
endif
if(he = 3)
if(plotfile = q180 | plotfile = q183); 'set lat 0 90'; 'set lon 180 360';endif
if(plotfile = q181 | plotfile = q187); 'set lat 35 50'; 'set lon 0 30' ;endif 
endif
if(he = 4)
if( plotfile = q180 | plotfile = q183); 'set lat -90 0'; 'set lon 0 180';endif
if(plotfile = q181 | plotfile = q187); 'set lat 50 80'; 'set lon 0 60' ;endif 
endif
if(he = 5)
if(plotfile = q180 | plotfile = q183);'set lat -90 0'; 'set lon 180 360';endif
if(plotfile = q181 | plotfile = q187); 'set lat 35 50 '; 'set lon 30 60' ;endif 
endif
if(he = 6)
if(plotfile = q181 | plotfile = q187); 'set lat 0 35 '; 'set lon 60 120' ;endif 
endif
if(he = 7)
if(plotfile = q181 | plotfile = q187); 'set lat 35 50'; 'set lon 60 90' ;endif 
endif
if(he = 8)
if(plotfile = q181 | plotfile = q187); 'set lat 50 80'; 'set lon 60 120' ;endif 
endif
if(he = 9)
if(plotfile = q181 | plotfile = q187); 'set lat 35 50'; 'set lon 90 120' ;endif 
endif
if(he = 10)
if(plotfile = q181 | plotfile = q187); 'set lat 0 70'; 'set lon 120 240' ;endif 
endif
if(he = 11)
if(plotfile = q181 | plotfile = q187); 'set lat 0 35'; 'set lon 240 300' ;endif 
endif
if(he = 12)
if(plotfile = q181 | plotfile = q187); 'set lat 35 50'; 'set lon 240 270';endif
endif
if(he = 13)
if(plotfile = q181 | plotfile = q187); 'set lat 50 80'; 'set lon 240 300';endif
endif
if(he = 14)
if(plotfile = q181 | plotfile = q187); 'set lat 35 50'; 'set lon 270 300';endif
endif
if(he = 15)
if(plotfile = q181 | plotfile = q187); 'set lat 0 30'; 'set lon 300 360';endif
endif
if(he = 16)
if(plotfile = q181 | plotfile = q187); 'set lat 30 60'; 'set lon 300 360';endif
endif
if(he = 17)
if(plotfile = q181 | plotfile = q187); 'set lat 60 90'; 'set lon 300 360';endif
endif
if(he = 18)
if(plotfile = q181 | plotfile = q187); 'set lat -60 0'; 'set lon 0 120';endif
endif
if(he = 19)
if(plotfile = q181 | plotfile = q187); 'set lat -30 0'; 'set lon 120 180';endif
endif
if(he = 20)
if(plotfile = q181 | plotfile = q187); 'set lat -90 -30'; 'set lon 120 240';endif
endif
if(he = 21)
if(plotfile = q181 | plotfile = q187); 'set lat -30 0'; 'set lon 240 300';endif
endif
if(he = 22)
if(plotfile = q181 | plotfile = q187); 'set lat -75 -30'; 'set lon 260 335';endif
endif
if(he = 23)
if(plotfile = q181 | plotfile = q187); 'set lat -30 0'; 'set lon 300 360';endif
endif

return
