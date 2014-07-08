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
** dindex=-1 plot assimilated data

'open anal.ctl'
'open guess.ctl'
'open 'plotfile2'_grads_anl.ctl'
'open 'plotfile2'_grads_ges.ctl'

if( plotfile = uv282 | plotfile = uv229); nhe=1; endif
if( plotfile = uv280) ; nhe=10; endif
if( plotfile = uv284); nhe=11; endif
if( plotfile = uv281 | plotfile = uv287); nhe=23; endif

he=1
while(he <=nhe)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,nt,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,nt,v)
he=he+1
endwhile

function plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,nt,var)
dbug=0

'rgbset2'
'set background 99'
'page'
'c'

'run setvpage 1 2 2 2 0.9'
setmap(plotfile,he)
'set gxout shaded'
*'defint.gs  0.5 0'
'defint.gs  1.0 0'
if(var=u); 'd UGRDprs.1-UGRDprs.2';endif
if(var=v); 'd VGRDprs.1-VGRDprs.2';endif
'cbarb'
'set digsiz 0.12'
if(var=u)
'set ccolor 6'
'd maskout(obgu.4(t='nt'),muse.4(t='nt'))'
'set ccolor 4'
'd maskout(obgu.4(t='nt'),-muse.4(t='nt'))'
'set ccolor 98'
'draw title 'plotfile2'-u INCT and OBS-GUESS(red-used,blue-rej,m/s)'
endif
if(var=v)
'set ccolor 6'
'd maskout(obgv.4(t='nt'),muse.4(t='nt'))'
'set ccolor 4'
'd maskout(obgv.4(t='nt'),-muse.4(t='nt'))'
'set ccolor 98'
'draw title 'plotfile2'-v INCT and OBS-GUESS(red-used,blue-rej,m/s) '
endif

'run setvpage 2 2 2 2 0.9'
setmap(plotfile,he)
'set gxout shaded'
'defint.gs  1.0 0'
if(var=u); 'd UGRDprs.1-UGRDprs.2';endif
if(var=v); 'd VGRDprs.1-VGRDprs.2';endif
'cbarb'
*'set ccolor 98'
'set digsiz 0.12'
if(var=u)
'set ccolor 6'
'd maskout(obgu.3(t='nt'),muse.3(t='nt'))'
'set ccolor 4'
'd maskout(obgu.3(t='nt'),-muse.3(t='nt'))'
'set ccolor 98'
'draw title 'plotfile2'-u INCT and OBS-ANAL(red-used,blue-rej,m/s) at 'rdate'z'
endif
if(var=v)
'set ccolor 6'
'd maskout(obgv.3(t='nt'),muse.3(t='nt'))'
'set ccolor 4'
'd maskout(obgv.3(t='nt'),-muse.3(t='nt'))'
'set ccolor 98'
'draw title 'plotfile2'-u INCT and OBS-ANAL(red-used,blue-rej,m/s) at 'rdate'z'
endif

'run setvpage 1 1 2 2 0.9'
setmap(plotfile,he)
'set gxout shaded'
'defint.gs  1.0 0'
if(var=u); 'd UGRDprs.1-UGRDprs.2';endif
if(var=v); 'd VGRDprs.1-VGRDprs.2';endif
'cbarb'
'set ccolor 98'
if(var=u)
'draw title  Surface wind(u) INCT(m/s) at 'rdate'z' 
endif
if(var=v)
'draw title  Surface wind(v) INCT(m/s)  at 'rdate'z' 
endif

'run setvpage 2 1 2 2 0.9'
setmap(plotfile,he)
'set csmooth on'
'set gxout shaded'
'set cterp off'
'set  rbcols 49 48 47 46 45 44 43 42 41 81 21 22 23 24 25 26 27 28 29'
if(var=u)
'd UGRDprs.1'
endif
if(var=v)
'd VGRDprs.1'
endif
'cbarb'
'set digsiz 0.12'
'set gxout contour'
'set clevs'
'set ccolor 98'
'set cint 0'
if(var=u)
'd UGRDprs.1'
endif
if(var=v)
'd VGRDprs.1'
endif

'cbarb'
'set ccolor 98'
if(var=u)
'draw title  Surface wind(u) analysis at 'rdate'z'
endif
if(var=v)
'draw title  Surface wind(v) analysis at 'rdate'z'
endif

if(dbug=1)
 say 'hit enter to continue'
    pull var
endif
if(var=u)
'printim 'plotfile2'_u_region'he'.png 'xsize' 'ysize
endif
if(var=v)
'printim 'plotfile2'_v_region'he'.png 'xsize' 'ysize
endif


return

function setmap(plotfile,he)
'set annot 98'
'set line 98'
'set map 98'
'set xlopts 98'
'set ylopts 98'
if(he = 1)
'set lat  -90 90'; 'set lon 0 360'
endif
if(he = 2)
if(plotfile = uv280 ); 'set lat 30 70'; 'set lon 0 75';endif
if(plotfile = uv281 | plotfile = uv287); 'set lat 0 35'; 'set lon 0 60' ;endif
if(plotfile = uv284 ); 'set lat 10 40'; 'set lon 0 60' ;endif
endif
if(he = 3)
if(plotfile = uv280 ); 'set lat -35 30'; 'set lon 0 120';endif
if(plotfile = uv281 | plotfile = uv287); 'set lat 35 50'; 'set lon 0 30' ;endif
if(plotfile = uv284 ); 'set lat 40 75'; 'set lon 0 60';endif
endif
if(he = 4)
if(plotfile = uv280 ); 'set lat 0 60'; 'set lon 120 240';endif
if(plotfile = uv281 | plotfile = uv287); 'set lat 50 80'; 'set lon 0 60' ;endif 
if(plotfile = uv284 ); 'set lat 0 30'; 'set lon 60 120 ';endif
endif
if(he = 5)
if(plotfile = uv280 ); 'set lat 0 30'; 'set lon 240 300';endif
if(plotfile = uv281 | plotfile = uv287); 'set lat 35 50 '; 'set lon 30 60' ;endif 
if(plotfile = uv284 ); 'set lat 30 60'; 'set lon 60 120';endif
endif
if(he = 6)
if(plotfile = uv280 ); 'set lat 30 60'; 'set lon 240 300';endif
if(plotfile = uv281 | plotfile = uv287); 'set lat 0 35 '; 'set lon 60 120' ;endif 
if(plotfile = uv284 ); 'set lat 0 60'; 'set lon 120 240';endif
endif
if(he = 7)
if(plotfile = uv280 ); 'set lat 0 32'; 'set lon 300 360';endif
if(plotfile = uv281 | plotfile = uv287); 'set lat 35 50'; 'set lon 60 90' ;endif 
if(plotfile = uv284 ); 'set lat 40 70'; 'set lon 240 300';endif
endif
if(he = 8)
if(plotfile = uv280 ); 'set lat 32 65'; 'set lon 300 360';endif
if(plotfile = uv281 | plotfile = uv287); 'set lat 50 80'; 'set lon 60 120' ;endif 
if(plotfile = uv284 ); 'set lat 0 65'; 'set lon 250 360';endif
endif
if(he = 9)
if(plotfile = uv280 ); 'set lat -60 0'; 'set lon 120 240';endif
if(plotfile = uv281 | plotfile = uv287); 'set lat 35 50'; 'set lon 90 120' ;endif 
if(plotfile = uv284 ); 'set lat -60 0'; 'set lon 0 120';endif
endif
if(he = 10)
if(plotfile = uv280 ); 'set lat -60 0'; 'set lon 240 360';endif
if(plotfile = uv281 | plotfile = uv287); 'set lat 0 70'; 'set lon 120 240' ;endif 
if(plotfile = uv284 ); 'set lat -60 0'; 'set lon 120 240';endif
endif
if(he = 11)
if(plotfile = uv281 | plotfile = uv287); 'set lat 0 35'; 'set lon 240 300' ;endif 
if(plotfile = uv284 ); 'set lat -60 0'; 'set lon 240 360';endif
endif
if(he = 12)
if(plotfile = uv281 | plotfile = uv287); 'set lat 35 50'; 'set lon 240 270';endif 
endif
if(he = 13)
if(plotfile = uv281 | plotfile = uv287); 'set lat 50 80'; 'set lon 240 300';endif 
endif
if(he = 14)
if(plotfile = uv281 | plotfile = uv287); 'set lat 35 50'; 'set lon 270 300';endif 
endif
if(he = 15)
if(plotfile = uv281 | plotfile = uv287); 'set lat 0 30'; 'set lon 300 360';endif 
endif
if(he = 16)
if(plotfile = uv281 | plotfile = uv287); 'set lat 30 60'; 'set lon 300 360';endif 
endif
if(he = 17)
if(plotfile = uv281 | plotfile = uv287); 'set lat 60 90'; 'set lon 300 360';endif 
endif
if(he = 18)
if(plotfile = uv281 | plotfile = uv287); 'set lat -60 0'; 'set lon 0 120';endif 
endif
if(he = 19)
if(plotfile = uv281 | plotfile = uv287); 'set lat -30 0'; 'set lon 120 180';endif 
endif
if(he = 20)
if(plotfile = uv281 | plotfile = uv287); 'set lat -90 -30'; 'set lon 120 240';endif 
endif
if(he = 21)
if(plotfile = uv281 | plotfile = uv287); 'set lat -30 0'; 'set lon 240 300';endif 
endif
if(he = 22)
if(plotfile = uv281 | plotfile = uv287); 'set lat -75 -30'; 'set lon 260 335';endif 
endif
if(he = 23)
if(plotfile = uv281 | plotfile = uv287); 'set lat -30 0'; 'set lon 300 360';endif 
endif
return

