*
* Set vpage & parea parameters to display multi figures.
*
* Usage: run  setvpage  x y  xn yn (sc  xpoff ypoff  xwdth ywdth  xwoff ywoff)
*
*   x y         : figure position from left,bottom  (1<=x<=xn, 1<=y<=yn)
*   xn yn       : numbers of figures for x,y
*   sc          : figure scale (0<sc<=1, default: sc=0.85)
*   xpoff ypoff : offset of figure in page (default: 0)
*   xwdth ywdth : sheet size (default: page size)
*   xwoff ywoff : offset of sheet (default: 0)
*
* Parameters sc,xpoff,...,ywoff could be eliminated.
* If sc,xpoff,...,ywoff are set as piriod '.', default values are used.
*
* Created by Y.Tahara    July,1999
*

function setvpage(options)

x    =subwrd(options,1)
y    =subwrd(options,2)
xn   =subwrd(options,3)
yn   =subwrd(options,4)
sc   =subwrd(options,5)
xpoff=subwrd(options,6)
ypoff=subwrd(options,7)
xwdth=subwrd(options,8)
ywdth=subwrd(options,9)
xwoff=subwrd(options,10)
ywoff=subwrd(options,11)

'set vpage off'
'set parea off'

'q gxinfo'
page=sublin(result,2)
xpage=subwrd(page,4)
ypage=subwrd(page,6)

if(sc=''|sc='.'); sc=0.85; endif
if(xpoff=''|xpoff='.'); xpoff=0; endif
if(ypoff=''|ypoff='.'); ypoff=0; endif
if(xwdth=''|xwdth='.'); xwdth=xpage; endif
if(ywdth=''|ywdth='.'); ywdth=ypage; endif
if(xwoff=''|xwoff='.'); xwoff=0; endif
if(ywoff=''|ywoff='.'); ywoff=0; endif

*** set vpage ***

dx=xwdth/xn
dy=ywdth/yn

x1=xwoff+dx*(x-1)
y1=ywoff+dy*(y-1)
x2=xwoff+dx*x
y2=ywoff+dy*y

'set vpage 'x1' 'x2' 'y1' 'y2

*** set parea ***

'q gxinfo'
page=sublin(result,2)
xpage=subwrd(page,4)
ypage=subwrd(page,6)

ex=xpage*(1-sc)/2
ey=ypage*(1-sc)/2

x3=ex+xpoff
y3=ey+ypoff
x4=xpage-ex+xpoff
y4=ypage-ey+ypoff

'set parea 'x3' 'x4' 'y3' 'y4

*** initialize ***

'set grads off'

return
