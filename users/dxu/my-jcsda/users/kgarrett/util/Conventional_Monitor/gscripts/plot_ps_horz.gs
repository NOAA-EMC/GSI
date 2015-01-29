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
** dindex=-1 plot rejected or not assimilated data

'open anal.ctl'
'open guess.ctl'
'open 'plotfile2'_grads_anl.ctl'
'open 'plotfile2'_grads_ges.ctl'

if (plotfile = ps180 ); nhe=11;endif
if (plotfile = ps183 ); nhe=9;endif
if (plotfile = ps120); nhe=9;endif
if (plotfile = ps181 | plotfile = ps187); nhe=23;endif
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
'd (PRESsfc.1-PRESsfc.2)/100.0'
'cbarb'
*'set ccolor 98'
'set ccolor 6'
'set digsiz 0.12'
'set ccolor 6'
'set digsiz 0.12'
'd maskout(obg.4(t='nt'),muse.4(t='nt'))'
'set ccolor 4'
'd maskout(obg.4(t='nt'),-muse.4(t='nt'))'
'set ccolor 98'
'draw title 'plotfile2' INCT and OBS-GUESS(red-used,blue-rej.,mb) '

'run setvpage 2 2 2 2 0.9'
setmap(plotfile,he)
'set gxout shaded'
'defint.gs  0.5 0'
'd (PRESsfc.1-PRESsfc.2)/100.0'
'cbarb'
'set ccolor 6'
'set digsiz 0.12'
'd maskout(obg.3(t='nt'),muse.3(t='nt'))'
'set ccolor 4'
'd maskout(obg.3(t='nt'),-muse.3(t='nt'))'
'set ccolor 98'
'draw title 'plotfile2' INCT and OBS-ANAL(red-used,blue-rej,mb) '

'run setvpage 1 1 2 2 0.9'
setmap(plotfile,he)
'set gxout shaded'
'defint.gs  0.5 0'
'd (PRESsfc.1-PRESsfc.2)/100.0'
'cbarb'
'set ccolor 98'
'draw title Surface pressure  INCT(mb) at 'rdate'z'

'run setvpage 2 1 2 2 0.9'
setmap(plotfile,he)
'set csmooth on'
'set lev 1'
'set gxout shaded'
'set cterp off'
'set  rbcols 49 48 47 46 45 44 43 42 41 81 21 22 23 24 25 26 27 28 29'
'd PRESsfc.1/1000.0'
'cbarb'
'set digsiz 0.12'
'set gxout contour'
'set clevs'
'set ccolor 98'
'set ccols 98'
'set cint 0'
'd PRESsfc.1/1000.0'
'cbarb'
'set ccolor 98'
'draw title Surface Pressure analysis(mb*10) at 'rdate'z'

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
'set lat -90 90'; 'set lon 0 360'
endif
if(he = 2)
if(plotfile = ps120); 'set lat 30 75';'set lon 0 80';endif
if(plotfile = ps180 ); 'set lat 30 70'; 'set lon 0 75';endif 
if(plotfile = ps181 | plotfile = ps187); 'set lat 0 35'; 'set lon 0 60' ;endif
if(plotfile = ps183); 'set lat 0 35'; 'set lon 0 70';endif
endif
if(he = 3)
if(plotfile = ps120); 'set lat 30 75';'set lon 80 160';endif 
if(plotfile = ps180); 'set lat -35 30'; 'set lon 0 120';endif 
if(plotfile = ps181 | plotfile = ps187); 'set lat 35 50'; 'set lon 0 30' ;endif 
if(plotfile = ps183); 'set lat 35 70'; 'set lon 0 70';endif
endif
if(he = 4)
if(plotfile = ps120);'set lat 0 75';'set lon 120 260 ';endif
if(plotfile = ps180 ); 'set lat 0 60'; 'set lon 120 240';endif 
if(plotfile = ps181 | plotfile = ps187); 'set lat 50 80'; 'set lon 0 60' ;endif 
if(plotfile = ps183); 'set lat 0 35'; 'set lon 70 130';endif
endif
if(he = 5)
if(plotfile = ps120); 'set lat 20 65';'set lon 240 320';endif
if(plotfile = ps180 ); 'set lat 0 30'; 'set lon 240 300';endif
if(plotfile = ps181 | plotfile = ps187); 'set lat 35 50 '; 'set lon 30 60' ;endif 
if(plotfile = ps183);'set lat 35 70'; 'set lon 75 135';endif
endif
if(he = 6)
if(plotfile = ps120); 'set lat 20 65';'set lon 280 360';endif
if(plotfile = ps180 ); 'set lat 30 60'; 'set lon 240 300';endif
if(plotfile = ps181 | plotfile = ps187); 'set lat 0 35 '; 'set lon 60 120' ;endif 
if(plotfile = ps183);'set lat 35 75'; 'set lon 220 300';endif
*if(plotfile = ps183);'set lat 0 40'; 'set lon 200 280';endif
endif
if(he = 7)
if(plotfile = ps120); 'set lat -35 30';'set lon 0 120';endif
if(plotfile = ps180 ); 'set lat 0 32'; 'set lon 300 360';endif 
if(plotfile = ps181 | plotfile = ps187); 'set lat 35 50'; 'set lon 60 90' ;endif 
if(plotfile = ps183);'set lat 30 65'; 'set lon 290 360';endif
endif
if(he = 8)
if(plotfile = ps120); 'set lat -65 0';'set lon 120 240';endif
if(plotfile = ps180 ); 'set lat 32 65'; 'set lon 300 360';endif 
if(plotfile = ps181 | plotfile = ps187); 'set lat 50 80'; 'set lon 60 120' ;endif 
if(plotfile = ps183);'set lat -90 0'; 'set lon 0 180';endif
endif
if(he = 9)
if(plotfile = ps120); 'set lat -65 20';'set lon 200 360';endif
if(plotfile = ps180 ); 'set lat -60 0'; 'set lon 120 240';endif 
if(plotfile = ps181 | plotfile = ps187); 'set lat 35 50'; 'set lon 90 120' ;endif 
if(plotfile = ps183);'set lat -90 0'; 'set lon 180 360';endif
endif
if(he = 10)
if(plotfile = ps180 ); 'set lat -60 0'; 'set lon 240 360';endif 
if(plotfile = ps181 | plotfile = ps187); 'set lat 0 70'; 'set lon 120 240' ;endif 
endif
if(he = 11)
if(plotfile = ps181 | plotfile = ps187);'set lat 0 35'; 'set lon 240 300' ;endif 
if(plotfile = ps180 ); 'set lat -65 5'; 'set lon 0 120';endif 
endif
if(he = 12)
if(plotfile = ps181 | plotfile = ps187); 'set lat 35 50'; 'set lon 240 270';endif 
endif
if(he = 13)
if(plotfile = ps181 | plotfile = ps187); 'set lat 50 80'; 'set lon 240 300';endif 
endif
if(he = 14)
if(plotfile = ps181 | plotfile = ps187); 'set lat 35 50'; 'set lon 270 300';endif
endif
if(he = 15)
if(plotfile = ps181 | plotfile = ps187); 'set lat 0 30'; 'set lon 300 360';endif
endif
if(he = 16)
if(plotfile = ps181 | plotfile = ps187); 'set lat 30 60'; 'set lon 300 360';endif
endif
if(he = 17)
if(plotfile = ps181 | plotfile = ps187); 'set lat 60 90'; 'set lon 300 360';endif
endif
if(he = 18)
if(plotfile = ps181 | plotfile = ps187); 'set lat -60 0'; 'set lon 0 120';endif
endif
if(he = 19)
if(plotfile = ps181 | plotfile = ps187); 'set lat -30 0'; 'set lon 120 180';endif
endif
if(he = 20)
if(plotfile = ps181 | plotfile = ps187); 'set lat -90 -30'; 'set lon 120 240';endif
endif
if(he = 21)
if(plotfile = ps181 | plotfile = ps187); 'set lat -30 0'; 'set lon 240 300';endif
endif
if(he = 22)
if(plotfile = ps181 | plotfile = ps187); 'set lat -75 -30'; 'set lon 260 335';endif
endif
if(he = 23)
if(plotfile = ps181 | plotfile = ps187); 'set lat -30 0'; 'set lon 300 360';endif
endif
return

