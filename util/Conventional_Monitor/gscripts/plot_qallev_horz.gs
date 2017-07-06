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
** this script plots air craft data (950.,840.,690.,590.,490.,390.,290.,240.,190.,90.)
** the data plotted is 2*hint interval  940-960
** and the rawinsonde whose height same as defined, hint=0 
'open anal.ctl'
'open guess.ctl'
'open 'plotfile2'_grads_anl.ctl'
'open 'plotfile2'_grads_ges.ctl'
say  'open the file'

if(plotfile = q120); hint=0; nhe=9;endif
if(plotfile = q130 | plotfile = q132); nhe=1;endif
if(plotfile = q133 | plotfile = q134 | plotfile = q135);  nhe=2;endif

he=1
while(he <=nhe)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,950,1,hint)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,850,2,hint)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,700,3,hint)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,500,4,hint)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,300,5,hint)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,250,6,hint)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,200,7,hint)
if(plotfile=q133 | plotfile=q120)
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
if(levx1 >=4)
'defint.gs  1 0'
else
'defint.gs  0.5 0'
endif
'd 10*(RHprs.1-RHprs.2)/RHprs.1'
'cbarb'
'set ccolor 6'
'set digsiz 0.12'
levy=levx-hint
ley1=levx+hint+1
'set lev 'levy
'set dfile 4'
'set lev 'levy
'd maskout(maskout(10*obg.4(t=1)/obs.4(t=1),'ley1'-press.4(t=1)),muse.4(t=1))'
'set ccolor 4'
'd maskout(maskout(10*obg.4(t=1)/obs.4(t=1),'ley1'-press.4(t=1)),-muse.4(t=1))'
'set ccolor 98'
'draw title 'plotfile2' INCT*10/Anal and 10*(OBS-GUESS)/OBS(red-used,blue-rej) '

'run setvpage 2 2 2 2 0.9'
setmap(plotfile,he)
'set lev 'levx
'set gxout shaded'
if(levx1 >=4)
'defint.gs  1 0'
else
'defint.gs  0.5 0'
endif
'd 10*(RHprs.1-RHprs.2)/RHprs.1'
'cbarb'
*'set ccolor 98'
'set ccolor 6'
'set digsiz 0.12'
levy=levx-hint
ley1=levx+hint+1
'set dfile 3'
'set lev 'levy
'd maskout(maskout(10*obg.3(t=1)/obs.3(t=1),'ley1'-press.3(t=1)),muse.3(t=1))'
'set ccolor 4'
'd maskout(maskout(10*obg.3(t=1)/obs.3(t=1),'ley1'-press.3(t=1)),0-muse.3(t=1))'
'set ccolor 98'
'draw title 'plotfile2' INCT*10/Anal and 10*(OBS-ANAL)/OBS(red-used,blue-rej) '

'run setvpage 1 1 2 2 0.9'
setmap(plotfile,he)
'set lev 'levx
'set gxout shaded'
if(levx1 >=4)
'defint.gs  1 0'
else
'defint.gs  0.5 0'
endif
'd 10*(RHprs.1-RHprs.2)/RHprs.1'
'cbarb'
'set ccolor 98'
'draw title Humidity INCT*10/Anal 'levx'mb at 'rdate'z' 

'run setvpage 2 1 2 2 0.9'
setmap(plotfile,he)
'set lev 'levx
'set lev 'levx
'set gxout shaded'
'defint 10 50'
'd RHprs.1'
'cbarb'
'draw title Humidity Analysis(RH,%) 'levx'mb at 'rdate'z'
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
if(plotfile = q120); 'set lat 30 75';'set lon 0 80';endif 
if(plotfile = q133); 'set lat 15 55';'set lon 220 300';endif
if(plotfile = q134); 'set lat 30 50';'set lon 250 290';endif
if(plotfile = q135); 'set lat 30 70';'set lon 220 305';endif
endif
if(he=3)
if(plotfile = q120); 'set lat 30 75';'set lon 80 160';endif 
endif
if(he=4)
if(plotfile = q120); 'set lat 0 75';'set lon 120 260';endif 
endif
if(he=5)
if(plotfile = q120); 'set lat 20 65';'set lon 240 320';endif
endif
if(he=6)
if(plotfile = q120); 'set lat 20 65';'set lon 280 360';endif 
endif
if(he=7)
if(plotfile = q120); 'set lat -35 30';'set lon 0 120';endif 
endif
if(he=8)
if(plotfile = q120); 'set lat -65 0';'set lon 120 240';endif 
endif
if(he=9)
if(plotfile = q120); 'set lat -65 20';'set lon 200 360';endif 
endif

return



