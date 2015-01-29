*
* Draw title
*
* Usage: run  drvtitle  title  (pos  fxw fyw  fnt fthck fcol  xoff yoff)
*
*   title     : Characters to draw. They cannot be include space ' '.
*               If you want to use space, replace it by '_'.
*               (ex. 'Psea Anal' -> 'Psea_Anal')
*   pos       : Position (l:left, c:center(default), r:right)
*   fxw fyw   : Font size (default: 0.15 0.18)
*   fnt       : Font number (default: 4)
*   fthck     : Font thickness (default: 6)
*   fcol      : Font color number (default: 1(white))
*   xoff yoff : Offset of position
*
* Parameters pos,...,yoff could be eliminated.
* If you set pos,...,yoff as piriod '.', default values are used.
*
* Created by Y.Tahara    July,1999
*

function  drvtitle(args)

chr = subwrd(args,1)
pos = subwrd(args,2)
fxw = subwrd(args,3)
fyw = subwrd(args,4)
fnt = subwrd(args,5)
fthck= subwrd(args,6)
fcol = subwrd(args,7)
xoff = subwrd(args,8)
yoff = subwrd(args,9)

if(pos=''|pos='.'); pos=c; endif
if(fxw=''|fxw='.'); fxw=0.15; endif
if(fyw=''|fyw='.'); fyw=0.18; endif
if(fnt=''|fnt='.'); fnt=4; endif
if(fthck=''|fthck='.'); fthck=6; endif
if(fcol=''|fcol='.'); fcol=1; endif
if(xoff=''|xoff='.'); xoff=0; endif
if(yoff=''|yoff='.'); yoff=0; endif

*** convert '_' -> ' ' ***
i=1
ch1=''
while(i>0)
 cc=substr(chr,i,1)
 if(cc=''); break; endif
 if(cc='_'); ch1=ch1%' '; endif
 if(cc!='_'); ch1=ch1%cc; endif
 i=i+1
endwhile

*** set font ***

'set font 'fnt
'set string 'fcol' 'pos' 'fthck
'set strsiz 'fxw' 'fyw

*** set position ***

'q gxinfo'
lin=sublin(result,3)
xl=subwrd(lin,4)
xr=subwrd(lin,6)
xm=(xl+xr)/2
if(pos='l'); xm=xl; endif
if(pos='r'); xm=xr; endif
xm=xm+xoff
lin=sublin(result,4)
yt=subwrd(lin,6) + fyw*1.5 + yoff

*** draw ***

'draw string 'xm' 'yt' 'ch1

return
