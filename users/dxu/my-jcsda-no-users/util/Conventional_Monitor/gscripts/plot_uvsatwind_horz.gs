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

if( nt !=1); quit;endif


** dindex=1, plot all station data
** dindex=-1 plot assimilated data
** this script plots air craft data (915.,840.,690.,590.,490.,390.,290.,240.,190.,90.)
** and rawinsond data,the air craft data plotted is 2*hint interval  915-935, for the
** rawinsond data, the height is as define

'open anal.ctl'
'open guess.ctl'
'open 'plotfile2'_grads_anl.ctl'
'open 'plotfile2'_grads_ges.ctl'
say  'open the file:' plotfile2'_grads_anl.ctl'
if(plotfile = uv242 | plotfile = uv252);nhe=3;endif
if(plotfile = uv253);nhe=5;endif

if(plotfile = uv242 | plotfile = uv252 | plotfile = uv253)
he=1
while(he <=nhe)
if(plotfile = uv242)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,850,2,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,850,2,hint,v)
endif
if(plotfile = uv252 | plotfile = uv253)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,700,3,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,700,3,hint,v)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,500,4,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,500,4,hint,v)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,300,5,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,300,5,hint,v)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,250,6,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,250,6,hint,v)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,200,7,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,200,7,hint,v)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,100,8,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,100,8,hint,v)
endif
he=he+1
endwhile
endif
if(plotfile = uv243)
he=1
while(he <=5)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,850,2,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,700,3,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,850,2,hint,v)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,700,3,hint,v)
he=he+1
endwhile
endif
if(plotfile = uv245 | plotfile = uv246 )
he=1
while(he <=13)
if(plotfile = uv245)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,925,1,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,850,2,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,700,3,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,925,1,hint,v)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,850,2,hint,v)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,700,3,hint,v)
endif
if(plotfile = uv245 | plotfile = uv246)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,500,4,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,300,5,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,250,6,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,200,7,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,300,5,hint,v)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,250,6,hint,v)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,200,7,hint,v)
endif
he=he+1
endwhile
endif


if(plotfile = uv257 | plotfile = uv258)
he=1
while(he <=13)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,700,3,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,700,3,hint,v)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,500,4,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,500,4,hint,v)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,300,5,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,300,5,hint,v)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,250,6,hint,u)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,250,6,hint,v)
he=he+1
endwhile
endif

function plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,levx,levx1,hint,var)
dbug=0

'rgbset2'
'set background 99'
'page'
'c'

'run setvpage 1 2 2 2 0.9'
setmap(plotfile,he)
'set gxout shaded'
'set lev 'levx
'defint.gs  1.0 0'
if(var=u); 'd UGRDprs.1-UGRDprs.2';endif
if(var=v); 'd VGRDprs.1-VGRDprs.2';endif
'cbarb'
*'set ccolor 98'
'set digsiz 0.12'
levy=levx-hint
ley1=levx+hint+1
say 'levy='levy
say 'ley1='ley1
'set lev 'levy
'set dfile 4'
'set lev 'levy
if(var=u)
'set ccolor 6'
'd maskout(maskout(obgu.4(t=1),'ley1'-press.4(t=1)),muse.4(t=1))'
'set ccolor 4'
'd maskout(maskout(obgu.4(t=1),'ley1'-press.4(t=1)),-muse.4(t=1))'
'set ccolor 98'
'draw title 'plotfile2'-u INCT and OBS-GUESS(red-used,blue-rej,m/s)'
endif
if(var=v)
'set ccolor 6'
'd maskout(maskout(obgv.4(t=1),'ley1'-press.4(t=1)),muse.4(t=1))'
'set ccolor 4'
'd maskout(maskout(obgv.4(t=1),'ley1'-press.4(t=1)),-muse.4(t=1))'
'set ccolor 98'
'draw title 'plotfile2'-v INCT and OBS-GUESS(red-used,blue-rej,m/s) '
endif

'run setvpage 2 2 2 2 0.9'
setmap(plotfile,he)
'set lev 'levx
'set gxout shaded'
'defint.gs  1.0 0'
if(var=u); 'd UGRDprs.1-UGRDprs.2';endif
if(var=v); 'd VGRDprs.1-VGRDprs.2';endif
'cbarb'
*'set ccolor 98'
'set ccolor 6'
'set digsiz 0.12'
levy=levx-hint
ley1=levx+hint+1
'set dfile 3'
'set lev 'levy
if(var=u)
'set ccolor 6'
'd maskout(maskout(obgu.3(t=1),'ley1'-press.3(t=1)),muse.3(t=1))'
'set ccolor 4'
'd maskout(maskout(obgu.3(t=1),'ley1'-press.3(t=1)),-muse.3(t=1))'
'set ccolor 98'
'draw title 'plotfile2'-u INCT and OBS-ANAL(red-used,blue-rej,m/s)'
endif
if(var=v)
'set ccolor 6'
'd maskout(maskout(obgv.3(t=1),'ley1'-press.3(t=1)),muse.3(t=1))'
'set ccolor 4'
'd maskout(maskout(obgv.3(t=1),'ley1'-press.3(t=1)),-muse.3(t=1))'
'set ccolor 98'
'draw title 'plotfile2'-v INCT and OBS-ANAL(red-used,blue-rej,m/s)'
endif

'run setvpage 1 1 2 2 0.9'
setmap(plotfile,he)
'set lev 'levx
'set gxout shaded'
'defint.gs  1.0 0'
if(var=u); 'd UGRDprs.1-UGRDprs.2';endif
if(var=v); 'd VGRDprs.1-VGRDprs.2';endif
'cbarb'
'set ccolor 98'
if(var=u)
'draw title  Wind(u) INCT(m/s) 'levx'mb at 'rdate'z'
endif
if(var=v)
'draw title  Wind(v) INCT(m/s) 'levx'mb at 'rdate'z'
endif

'run setvpage 2 1 2 2 0.9'
setmap(plotfile,he)
'set lev 'levx
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
'draw title  Wind(u) analysis at 'rdate'z'
endif
if(var=v)
'draw title  Wind(v) analysis at 'rdate'z'
endif

if(dbug=1)
 say 'hit enter to continue'
    pull var
endif

if(var=u)
'printim 'plotfile2'_u_region'he'_lev'levx1'.png 'xsize' 'ysize
endif
if(var=v)
'printim 'plotfile2'_v_region'he'_lev'levx1'.png 'xsize' 'ysize
endif



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
if(plotfile = uv242 | plotfile = uv252 ); 'set lat 0 60';'set lon 100 210';endif
if(plotfile = uv243); 'set lat 0 60';'set lon 0 120';endif
if(plotfile = uv245 | plotfile = uv246); 'set lat 0 30';'set lon 180 240';endif
if(plotfile = uv253); 'set lat 0 60';'set lon 0 120';endif
if(plotfile = uv257 | plotfile = uv258); 'set lat 60 90';'set lon 0 60';endif
endif
if(he=3)
if(plotfile = uv242 | plotfile = uv252 ); 'set lat -60 0';'set lon 90 200';endif
if(plotfile = uv243); 'set lat -60 0';'set lon 0 120';endif
if(plotfile = uv245 | plotfile = uv246); 'set lat 30  60';'set lon 180 240 ';endif
if(plotfile = uv253); 'set lat -60  0';'set lon 0 120';endif
if(plotfile = uv257 | plotfile = uv258); 'set lat 60 90';'set lon 60 120';endif
endif
if(he=4)
if(plotfile = uv243); 'set lat 0 35';'set lon 300 360';endif
if(plotfile = uv245 | plotfile = uv246); 'set lat 0 30';'set lon 240 300';endif
if(plotfile = uv253); 'set lat 0 60';'set lon 260 360';endif
if(plotfile = uv257 | plotfile = uv258); 'set lat 60 90';'set lon 120 180';endif
endif
if(he=5)
if(plotfile = uv243); 'set lat -35 0';'set lon 300 360';endif
if(plotfile = uv245 | plotfile = uv246); 'set lat 30 60';'set lon 240 300';endif
if(plotfile = uv253); 'set lat -60 0';'set lon 260 360';endif
if(plotfile = uv257 | plotfile = uv258); 'set lat 60 90';'set lon 180 240';endif
endif
if(he=6)
if(plotfile = uv245 | plotfile = uv246); 'set lat 0 30';'set lon 300 360';endif
if(plotfile = uv257 | plotfile = uv258); 'set lat 60 90';'set lon 240 300';endif
endif
if(he=7)
if(plotfile = uv245 | plotfile = uv246); 'set lat 30 60';'set lon 300 360';endif
if(plotfile = uv257 | plotfile = uv258); 'set lat 60 90';'set lon 300 360';endif
endif
if(he=8)
if(plotfile = uv245 | plotfile = uv246); 'set lat -30 0';'set lon 180 240';endif
if(plotfile = uv257 | plotfile = uv258); 'set lat -90 -60';'set lon  0 60';endif
endif
if(he=9)
if(plotfile = uv245 | plotfile = uv246); 'set lat -60 -30';'set lon 180 240';endif
if(plotfile = uv257 | plotfile = uv258); 'set lat -90 -60';'set lon 60 120';endif
endif
if(he=10)
if(plotfile = uv245 | plotfile = uv246); 'set lat -30 0';'set lon 240 300';endif
if(plotfile = uv257 | plotfile = uv258); 'set lat -90 -60';'set lon 120 180';endif
endif
if(he=11)
if(plotfile = uv245 | plotfile = uv246); 'set lat -60 -30';'set lon 240 300';endif
if(plotfile = uv257 | plotfile = uv258); 'set lat -90 -60';'set lon 180 240';endif
endif
if(he=12)
if(plotfile = uv245 | plotfile = uv246); 'set lat -30 0';'set lon 300 360';endif
if(plotfile = uv257 | plotfile = uv258); 'set lat -90 -60';'set lon 240 300';endif
endif
if(he=13)
if(plotfile = uv245 | plotfile = uv246); 'set lat -60 -30';'set lon 300 360';endif
if(plotfile = uv257 | plotfile = uv258); 'set lat -90 -60';'set lon 300 360';endif
endif

return


