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

if(nt != 1); quit;endif

** dindex=1, plot all station data
** dindex=0 plot assimilated data
** dindex=-1 plot not assimilated data 
** this script plots air craft data (940.,840.,690.,590.,490.,390.,290.,240.,190.,90.)
** the data plotted is 2*hint interval  915-935
** and the rawinsonde whose height same as defined, hint=0 
'open anal.ctl'
'open guess.ctl'
'open 'plotfile2'_grads_anl.ctl'
'open 'plotfile2'_grads_ges.ctl'
say  'open the file'

if(plotfile = t120); hint=0; nhe=9;endif
if(plotfile = t130); nhe=6;endif
if(plotfile = t131); nhe=12;endif
if(plotfile = t132); nhe=1;endif
if(plotfile = t133); nhe=11;endif
if(plotfile = t134 | plotfile = t135);  nhe=2;endif

he=1
while(he <=nhe)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,950,1,hint)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,850,2,hint)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,700,3,hint)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,500,4,hint)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,300,5,hint)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,250,6,hint)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,200,7,hint)
if(plotfile=t120)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,100,8,hint)
endif
he=he+1
endwhile

function plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,levx,levx1,hint)
dbug=0

'rgbset2'
'set background 99'
'page'
'c'

'run setvpage 1 2 2 2 0.9'
setmap(plotfile,he)
'set gxout shaded'
'set lev 'levx
'defint.gs  0.5 0'
'd TMPprs.1-TMPprs.2'
'cbarb'
*'set ccolor 98'
'set ccolor 6'
'set digsiz 0.12'
levy=levx-hint
ley1=levx+hint+1
'set lev 'levy
'set dfile 4'
'set lev 'levy
'd maskout(maskout(obg.4(t=1),'ley1'-press.4(t=1)),muse.4(t=1))'
'set ccolor 4'
'd maskout(maskout(obg.4(t=1),'ley1'-press.4(t=1)),-muse.4(t=1))'
'set ccolor 98'
'draw title 'plotfile2' INCT and OBS-GUESS(red-used,blue-rej,C) '

'run setvpage 2 2 2 2 0.9'
setmap(plotfile,he)
'set lev 'levx
'set gxout shaded'
'defint.gs  0.5 0'
'd TMPprs.1-TMPprs.2'
'cbarb'
*'set ccolor 98'
'set ccolor 6'
'set digsiz 0.12'
levy=levx-hint
ley1=levx+hint+1
'set dfile 3'
'set lev 'levy
'd maskout(maskout(obg.3(t=1),'ley1'-press.3(t=1)),muse.3(t=1))'
'set ccolor 4'
'd maskout(maskout(obg.3(t=1),'ley1'-press.3(t=1)),-muse.3(t=1))'
'set ccolor 98'
'draw title 'plotfile2' INCT and OBS-ANAL(used,C) at 'rdate'z'

'run setvpage 1 1 2 2 0.9'
setmap(plotfile,he)
'set lev 'levx
'set gxout shaded'
'defint.gs  0.5 0'
'd (TMPprs.1-TMPprs.2)'
'cbarb'
'set ccolor 98'
'draw title Temperature INCT(C) 'levx'mb at 'rdate'z' 

'run setvpage 2 1 2 2 0.9'
setmap(plotfile,he)
'set lev 'levx
'set csmooth on'
'set gxout shaded'
'set cterp off'
'set  rbcols 49 48 47 46 45 44 43 42 41 81 21 22 23 24 25 26 27 28 29'
'd TMPprs.1-273.15'
'cbarb'
'set digsiz 0.12'
'set gxout contour'
'set clevs'
'set ccolor 98'
'set cint 0'
'd TMPprs.1-273.15'
'cbarb'
'set ccolor 98'
'draw title Temperature analysis(C) at 'rdate'z'

if(dbug=1)
 say 'hit enter to continue'
    pull var
endif


'printim 'plotfile2'_region'he'_lev'levx1'.png 'xsize' 'ysize


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
if(he=2)
if(plotfile = t120); 'set lat 30 75';'set lon 0 80';endif 
if(plotfile = t130); 'set lat 0 60';'set lon 120 240';endif
if(plotfile = t131); 'set lat 0 65';'set lon 0 130';endif
if(plotfile = t133); 'set lat 35 60';'set lon 140 180';endif
if(plotfile = t134); 'set lat 30 50';'set lon 250 290';endif
if(plotfile = t135); 'set lat 30 70';'set lon 220 305';endif
endif
if(he=3)
if(plotfile = t120); 'set lat 30 75';'set lon 80 160';endif
if(plotfile = t130); 'set lat 0 60';'set lon 240 360';endif
if(plotfile = t131); 'set lat 0 35';'set lon 0 60';endif
if(plotfile = t133); 'set lat 60 90';'set lon 120 180';endif
endif
if(he=4)
if(plotfile = t120); 'set lat 0 75';'set lon 120 260';endif
if(plotfile = t130); 'set lat 30 60';'set lon 240 300';endif
if(plotfile = t131); 'set lat 30 65';'set lon 0 60';endif
if(plotfile = t133); 'set lat 20 45';'set lon 200 245';endif
endif
if(he=5)
if(plotfile = t120); 'set lat 20 65';'set lon 240 320';endif
if(plotfile = t130); 'set lat 40 60';'set lon 300 335';endif
if(plotfile = t131);'set lat 30 60';'set lon 60 115';endif
if(plotfile = t133); 'set lat 45 75';'set lon 180 240';endif
endif
if(he=6)
if(plotfile = t120); 'set lat 20 65';'set lon 280 360';endif
if(plotfile = t130); 'set lat -50 0';'set lon 140 240';endif
if(plotfile = t131);'set lat 20 45';'set lon 100 150';endif
if(plotfile = t133); 'set lat 0 30';'set lon 240 300';endif
endif
if(he=7)
if(plotfile = t120); 'set lat -35 30';'set lon 0 120';endif
if(plotfile = t131);'set lat 25 70';'set lon 240 300';endif
if(plotfile = t133); 'set lat 30 40';'set lon 270 290';endif
endif
if(he=8)
if(plotfile = t120); 'set lat -65 0';'set lon 120 240';endif
if(plotfile = t131); 'set lat 0 30';'set lon 300 360';endif
if(plotfile = t133); 'set lat 40 50';'set lon 270 290';endif
endif
if(he=9)
if(plotfile = t120);'set lat -65 20';'set lon 200 360';endif
if(plotfile = t131); 'set lat 30 65';'set lon 300 360';endif
if(plotfile = t133);'set lat 50 65';'set lon 270 295';endif
endif
if(he = 10 )
if(plotfile = t131); 'set lat -35 0';'set lon 0 60';endif
if(plotfile = t133);'set lat 30 60';'set lon 290 350';endif
endif
if(he = 11 )
if(plotfile = t131); 'set lat -30 0';'set lon 60 120';endif
if(plotfile = t133);'set lat -90 0';'set lon 180 360';endif
endif
if(he = 12 )
if(plotfile = t131); 'set lat -45 -20';'set lon 130 180';endif
endif
return



