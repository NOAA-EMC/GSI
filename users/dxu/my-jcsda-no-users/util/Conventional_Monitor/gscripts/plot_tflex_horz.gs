******
*--- initialize

'reinit'

'rgbset2'
'set background 99'
'page'
'c'

plotfile=PLOTFILE
plotfile2=PLOTFILE2
xsize=XSIZE
ysize=YSIZE
rdate=RDATE
dindex=DINDEX
latmin=LATMIN
latmax=LATMAX
lonmin=LONMIN
lonmax=LONMAX
hint=HINT


** dindex=1, plot all station data
** dindex=-1 plot assimilated data
** this script plots air craft data (915.,840.,690.,590.,490.,390.,290.,240.,190.,90.)
** the data plotted is 2*hint interval  915-935
'open anal.ctl'
'open guess.ctl'
'open 'plotfile2'_grads_anl.ctl'
'open 'plotfile2'_grads_ges.ctl'
say  'open the file'

he=1
while(he <=4)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,dindex,925,1,latmin,latmax,lonmin,lonmax,hint)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,dindex,850,2,latmin,latmax,lonmin,lonmax,hint)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,dindex,700,3,latmin,latmax,lonmin,lonmax,hint)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,dindex,500,4,latmin,latmax,lonmin,lonmax,hint)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,dindex,300,5,latmin,latmax,lonmin,lonmax,hint)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,dindex,250,6,latmin,latmax,lonmin,lonmax,hint)
plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,dindex,200,7,latmin,latmax,lonmin,lonmax,hint)
he=he+1
endwhile

function plot_horz(plotfile,plotfile2,xsize,ysize,he,rdate,dindex,levx,levx1,latmin,latmax,lonmin,lonmax,hint)
dbug=1

'rgbset2'
'set background 99'
'page'
'c'

'run setvpage 1 2 2 2 0.9'
if(he=1);_he='ne';endif;
if(he=2);_he='nw';endif;
if(he=3);_he='se';endif;
if(he=4);_he='sw';endif;
setmap(latmin,latmax,lonmin,lonmax)
'set gxout shaded'
'set lev 'levx
'defint.gs  0.5 0'
'd TMPprs.1-TMPprs.2'
'cbarb'
*'set ccolor 98'
'set ccolor 6'
'set digsiz 0.12'
levy=levx-hint
ley1=levx+hint
'set lev 'levy
'set dfile 4'
'set lev 'levy
if(dindex=1)
'd maskout(obg.4(t=1),'ley1'-press.4(t=1))'
'set ccolor 98'
'draw title 'plotfile2' INCT and OBS-GUESS(all,C) at 'rdate'z'
endif
if(dindex=-1)
'd maskout(maskout(obg.4(t=1),'ley1'-press.4(t=1)),muse.4(t=1))'
'set ccolor 98'
'draw title 'plotfile2' INCT and OBS-GUESS(used,C) at 'rdate'z'
endif

'run setvpage 2 2 2 2 0.9'
if(he=1);_he='ne';endif;
if(he=2);_he='nw';endif;
if(he=3);_he='se';endif;
if(he=4);_he='sw';endif;
setmap(latmin,latmax,lonmin,lonmax)
'set lev 'levx
'set gxout shaded'
'defint.gs  0.5 0'
'd TMPprs.1-TMPprs.2'
'cbarb'
*'set ccolor 98'
'set ccolor 6'
'set digsiz 0.12'
levy=levx-hint
ley1=levx+hint
'set dfile 3'
'set lev 'levy
if(dindex=1)
'd maskout(obg.3(t=1),'ley1'-press.3(t=1))'
'set ccolor 98'
'draw title 'plotfile2' INCT and OBS-ANAL(all,C) at 'rdate'z'
endif
if(dindex=-1)
'd maskout(maskout(obg.3(t=1),'ley1'-press.3(t=1)),muse.3(t=1))'
'set ccolor 98'
'draw title 'plotfile2' INCT and OBS-ANAL(used,C) at 'rdate'z'
endif

'run setvpage 1 1 2 2 0.9'
if(he=1);_he='ne';endif;
if(he=2);_he='nw';endif;
if(he=3);_he='se';endif;
if(he=4);_he='sw';endif;
setmap(latmin,latmax,lonmin,lonmax)
'set lev 'levx
'set gxout shaded'
'defint.gs  0.5 0'
'd (TMPprs.1-TMPprs.2)'
'cbarb'
'set ccolor 98'
'draw title Temperature INCT(C) 'levx'mb at 'rdate'z' 

'run setvpage 2 1 2 2 0.9'
if(he=1);_he='ne';endif;
if(he=2);_he='nw';endif;
if(he=3);_he='se';endif;
if(he=4);_he='sw';endif;
setmap(latmin,latmax,lonmin,lonmax)
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

function setmap(latmin,latmax,lonmin,lonmax)
'set annot 98'
'set line 98'
'set map 98'
'set xlopts 98'
'set ylopts 98'
lat1=(latmax+latmin)/2
lon1=(lonmax+lonmin)/2
if(_he='ne')
'set lat 'lat1' 'latmax
'set lon 'lonmin' 'lon1
endif
if(_he='nw')
'set lat 'lat1' 'latmax
'set lon 'lon1' 'lonmax
endif
if(_he='se')
'set lat 'latmin' 'lat1
'set lon 'lonmin' 'lon1
endif
if(_he='sw')
'set lat 'latmin' 'lat1
'set lon 'lon1' 'lonmax
endif
return

