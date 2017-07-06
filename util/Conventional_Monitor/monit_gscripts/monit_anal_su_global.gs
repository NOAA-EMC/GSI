******
*--- initialize

'reinit'

'rgbset2'
'set background 99'

'open anal.ctl'
'open guess.ctl'

'q time'
dmy=sublin(result,1)
ti=subwrd(dmy,3)
say ti

hh=substr(ti,1,2)
dd=substr(ti,4,2)


drlvs(1,WH,500,dd,hh,1)
drlvs(1,WH,400,dd,hh,3)
drlvs(1,WH,300,dd,hh,5)
drlvs(1,WH,850,dd,hh,9)
drlvs(1,WH,700,dd,hh,11)
drlvs(1,WH,200,dd,hh,13)
drlvs(1,WH,100,dd,hh,15)
drclmmst(1,WH,dd,hh,7)

drlvs(2,EH,500,dd,hh,1)
drlvs(2,EH,400,dd,hh,3)
drlvs(2,EH,300,dd,hh,5)
drlvs(2,EH,850,dd,hh,9)
drlvs(2,EH,700,dd,hh,11)
drlvs(2,EH,200,dd,hh,13)
drlvs(2,EH,100,dd,hh,15)

drclmmst(2,EH,dd,hh,7)

function prps(grfile,gsfile)
'print'
'disable print'
say 'from 'grfile ' to ' gsfile
*'!/u/wx20mf/grads/bin/g.png -c -i 'grfile' -o 'gsfile
* '!/u/wx20yt/bin/gxpng -c -i 'grfile' -o 'gsfile
' !gxpng -x 900 -y 700  -i 'grfile ' -o 'gsfile 
return


function drlvs(he,che,levx,dd,hh,pn) 

****************page 1******************
*'rgbset2'
*'set background 99'
if(he=1);_he='n';endif;
if(he=2);_he='s';endif;
t0=273.15
grfile='anal_d'dd%hh'_'_he''pn'.gr'
gsfile='anal_d'dd%hh'_'_he''pn'.png'
'enable print 'grfile
'page'
'c'
***** Geopotential height*********
'run setvpage 3 2 3 2 0.9'
setmap()
'set csmooth on'
'set lev 'levx
'set gxout shaded'
'set cterp off'
'set cint 0'
'set  rbcols 49 48 47 46 45 44 43 42 41 81 21 22 23 24 25 26 27 28 29'
'd HGTprs/100'
'cbarb '
'set gxout contour'
'set clevs'
'set ccolor 98'
'set ccols 98'
'set cint 0'
'd HGTprs/100'
'set ccols 98'
'draw title Anal:height/100 'che' 'levx'mb 'dd''hh'z'


'run setvpage 2 2 3 2 0.9'
setmap()
'set csmooth on'
'set lev 'levx
'set gxout shaded'
'set cterp off'
'set cint 0'
'set  rbcols 49 48 47 46 45 44 43 42 41 81 21 22 23 24 25 26 27 28 29'
'd HGTprs.2/100'
'cbarb'
'set gxout contour'
'set clevs'
'set ccolor 98'
'set ccols 98'
'set cint 0'
'd HGTprs.2/100'
'set ccolor 98'
'draw title Backg:height/100 'che' 'levx'mb 'dd''hh'z'

'run setvpage 1 2 3 2 0.9'
setmap()
'set lev 'levx
'set gxout shaded'
'defint 5 0'
'd  HGTprs-HGTprs.2'
'cbarb'
'draw title Increment: height 'che' 'levx'mb 'dd''hh'z'

********* wind vector*******

'run setvpage 3 1 3 2 0.9'
setmap()
'set lev 'levx
'set ccolor 2'
'd skip(UGRDprs,5,5);VGRDprs'
'set gxout contour'
setcnt(10,4,1,0.08); 'd mag(UGRDprs,VGRDprs)'
'draw title Anal: Wind vector 'che' 'levx'mb 'dd''hh'z'

'run setvpage 2 1 3 2 0.9'
setmap()
'set lev 'levx
'set ccolor 2'
'd skip(UGRDprs.2,5,5);VGRDprs.2'
'set gxout contour'
setcnt(10,4,1,0.08); 'd mag(UGRDprs.2,VGRDprs.2)'
'draw title Backg: Wind vector 'che' 'levx'mb 'dd''hh'z'

'run setvpage 1 1 3 2 0.9'
setmap()
'set lev 'levx
'set ccolor 2'
'd skip(UGRDprs-UGRDprs.2,5,5);VGRDprs-VGRDprs.2'
setcnt(3,4,1,0.08); 'd mag(UGRDprs-UGRDprs.2,VGRDprs-VGRDprs.2)'
'draw title Increment: wind 'che' 'levx'mb 'dd''hh'z'
prps(grfile,gsfile)
*     say 'type in c to continue or quit to exit'
*     pull corquit
*     corquit
*'clear'

***********page 2**************

***********Temperature*************
pn=pn+1
grfile='anal_d'dd%hh'_'_he''pn'.gr'
gsfile='anal_d'dd%hh'_'_he''pn'.png'
'enable print 'grfile
'page'
'clear'
'run setvpage 3 2 3 2 0.9'
setmap()
'set csmooth on'
'set lev 'levx
'set gxout shaded'
'set cterp off'
'set cint 0'
'set  rbcols 49 48 47 46 45 44 43 42 41 81 21 22 23 24 25 26 27 28 29'
'd TMPprs-'t0
'cbarb'
'set gxout contour'
'set clevs'
'set ccolor 98'
'set ccols 98'
'set cint 0'
'd TMPprs-'t0
'set ccols 98'
'draw title Anal:temperature(C) 'che' 'levx'mb 'dd''hh'z'

'run setvpage 2 2 3 2 0.9'
setmap()
'set csmooth on'
'set lev 'levx
'set gxout shaded'
'set cterp off'
'set cint 0'
'set  rbcols 49 48 47 46 45 44 43 42 41 81 21 22 23 24 25 26 27 28 29'
'd TMPprs-'t0
'cbarb'
'set gxout contour'
'set clevs'
'set ccolor 98'
'set ccols 98'
'set cint 0'
'd TMPprs.2-'t0
'set ccols 98'

'draw title Backg:temperature(C) 'che' 'levx'mb 'dd''hh'z'

'run setvpage 1 2 3 2 0.9'
setmap()
'set lev 'levx
'set gxout shaded'
'defint 0.5 0'
 'd TMPprs-TMPprs.2'
'cbarb'
'draw title Increment:temp 'che' 'levx'mb 'dd''hh'z'

'run setvpage 3 1 3 2 0.9'
setmap()
'set lev 'levx
'set gxout shaded'
'defint 10 50'
'd RHprs'
'cbarb'
'draw title Anal: RH(%) 'che' 'levx'mb 'dd''hh'z'

'run setvpage 2 1 3 2 0.9'
setmap()
'set lev 'levx
'set gxout shaded'
'defint 10 50'
'd RHprs.2'
'cbarb'
'draw title Backg: RH(%) 'che' 'levx'mb 'dd''hh'z'

'run setvpage 1 1 3 2 0.9'
setmap()
'set lev 'levx
'set gxout shaded'
'defint 10 0'
'd RHprs-RHprs.2'
'cbarb'
'draw title Increment: RH(%) 'che' 'levx'mb 'dd''hh'z'

prps(grfile,gsfile)
*     say 'type in c to continue or quit to exit'
*     pull corquit
*     corquit
*'c'
return

***************
function drclmmst(he,che,dd,hh,pn)
if(he=1);_he='n';endif;
if(he=2);_he='s';endif;
grfile='anal_d'dd%hh'_'_he''pn'.gr'
gsfile='anal_d'dd%hh'_'_he''pn'.png'
'enable print 'grfile
'page'
'c'


*********** Column water vapor*************
*'rgbset2'
*'set background 99'

'run setvpage 3 2 3 2 0.9'
setmap()
'set gxout shaded'
'defint 5 30'
'd PWATclm-CWATclm'
'cbarb'
'draw title Anal: column WV(kg/m*m) 'che' 'dd''hh'z'

'run setvpage 2 2 3 2 0.9'
setmap()
'set gxout shaded'
'defint 5 30'
'd PWATclm.2-CWATclm.2'
'cbarb'
'draw title Backg: column WV(kg/m*m) 'che' 'dd''hh'z'

'run setvpage 1 2 3 2 0.9'
setmap()
'set gxout shaded'
'defint 1 0'
'd (PWATclm-CWATclm)-(PWATclm.2-CWATclm.2)'
'cbarb'
'draw title Increment: column WV(kg/m*m) 'che' 'dd''hh'z'

************ Column cloud water**************
'run setvpage 3 1 3 2 0.9'
setmap()
'set gxout shaded'
'defint 0.5 4'
'd CWATclm*10'
'cbarb'
'draw title Anal: column CW kg/m*m)'che' 'dd''hh'z'

'run setvpage 2 1 3 2 0.9'
setmap()
'set gxout shaded'
'defint 0.5 4'
'd CWATclm.2*10'
'cbarb'
'draw title Backg: column CW kg/m*m*10) 'che' 'dd''hh'z'

'run setvpage 1 1 3 2 0.9'
setmap()
'set gxout shaded'
'defint  0.1 0'
'd (CWATclm-CWATclm.2)*100'
'cbarb'
'draw title Increment: column CW kg/m*m*100) 'che' 'dd''hh'z'
prps(grfile,gsfile)
*     say 'type in c to continue or quit to exit'
*     pull corquit
*     corquit
*'c'
return



function setmap()
'set annot 98'
'set line 98'
'set map 98'
'set xlopts 98'
'set ylopts 98'
*if(_he='n')
* 'set mproj nps'
*'set mpvals -270 90 0 90'
** 'set mpvals -40 320 10 90'
*endif
*if(_he='s')
* 'set mproj sps'
* 'set mpvals -270 90 -90 -10'
* 'set mpvals -40 320 -90 0'
*endif
*'set map 95 1 9'
*'set grid on 1 15'
*'set grads off'
*'set xlint 30'
*'set ylint 30'
*'set xlopts 1 5 0.15'
*'set ylopts 1 5 0.15'
if(_he='n')
'set lat -90 90'
'set lon 160 360'
endif
if(_he='s')
'set lat -90 90'
'set lon -20 180'
endif
return

*****

function setcnt(it,cl,th,sz)
 'set ccolor 'cl
 'set cthick 'th
 'set clopts 'cl' 'th' 'sz
 'set cint 'it
return


