*
*	shell for a 3 panel plot in portrait mode for color plots
*
function main(args)

if(args='') 
 defunc='y'
else
 defunc=subwrd(args,1)
endif

script='/lib/3p_p.gs'

amipdir='../../../data/amip/monthly/'
d1=ofile(amipdir'amav79018812sf.ctl')
prntfile='3p_p.gm'

if (d1=0) 
  say 'Error opening files'
  return
endif
* 	grey shade call done or use color?
  grydone='y'
*
*	loop and printing options
*
printer='on'

if(printer='on');'enable print 'prntfile;endif

if(grydone!='y')
  rc=gryshade()
  'set display grey'
endif
*
*	dimensions of virtual and parea
*

xl1v='0.0'
xr1v='8.5'
yb1v='7.50'
yt1v='10.50'

xl1='1.0'
xr1='8.0'
yb1='0.25'
yt1='2.75'

xl2v=xl1v
xr2v=xr1v
yb2v='4.25'
yt2v='7.25'

xl2=xl1
xr2=xr1
yb2=yb1
yt2=yt1

xl3v=xl1v
xr3v=xr1v
yb3v='1.00'
yt3v='4.00'

xl3=xl1
xr3=xr1
yb3=yb1
yt3=yt1

*
*	the sets
*

'set grads off'

rc=setpage(xl1v,xr1v,yb1v,yt1v,xl1,xr1,yb1,yt1)
rc=plotitle(xl1,xr1,yb1,yt1,ptvar1)

rc=setpage(xl2v,xr2v,yb2v,yt2v,xl2,xr2,yb2,yt2)
rc=plotitle(xl2,xr2,yb2,yt2,ptvar2)

rc=setpage(xl3v,xr3v,yb3v,yt3v,xl3,xr3,yb3,yt3)
rc=plotitle(xl2,xr2,yb2,yt2,ptvar2)


*
*	top title
*

'set vpage off'
rc=toptitle(xl1,xr1,t1,t2)

say 'all done folk!!!'

*
*ffffffffffffffffff	function definitions      ffffffffffffffffff
*
function setpage(xlv,xrv,ybv,ytv,xl,xr,yb,yt)
  setv='set vpage 'xlv' 'xrv' 'ybv' 'ytv
  setv
  setp='set parea 'xl' 'xr' 'yb' 'yt
  setp
  'set grads off'
return

function toptitle(xl,xr,t1,t2)
  xs=xl+(xr-xl)*0.5
  'set strsiz 0.125'
  'set string 1 c 5'
  'draw string 'xs' 10.85 't1
  'set string 1 c 5'
  'set strsiz 0.10'
  'draw string 'xs' 10.65 't2
  say 'ending toptitle'
return

function plotitle(xl,xr,yb,yt,title)
*
*  plot title
*
  xs=xl+(xr-xl)*0.5
  ys=yt+0.15
  'set strsiz 0.10'
  'set string 1 c 6'
  'draw string 'xs' 'ys' 'title
return

function ofile (fname)
  'query files'
  i = 0
  while (1)  
    if (subwrd(result,1)='No')       
      ret = 0
      break;
    endif
    rec = sublin(result,i*3+2)
    if (rec='') 
      ret = 0;
      break; 
    endif
    if (subwrd(rec,2)=fname)
      rec = sublin(result,i*3+1)
      ret = subwrd(rec,2)
      break;
    endif
    i = i + 1
  endwhile
  if (ret=0) 
    'open 'fname
    if (rc>0) 
      say "Error opening "fname
      return (0)
    endif
    rec = sublin(result,2)
    ret = subwrd(rec,8)
  endif
  return (ret)

function gryshade
  'set rgb 21 15 15 15'
  'set rgb 22 31 31 31'
  'set rgb 23 47 47 47'
  'set rgb 24 63 63 63'
  'set rgb 25 79 79 79'
  'set rgb 26 95 95 95'
  'set rgb 27 111 111 111'
  'set rgb 28 127 127 127'
  'set rgb 29 143 143 143'
  'set rgb 30 159 159 159'
  'set rgb 31 173 173 173'
  'set rgb 32 191 191 191'
  'set rgb 33 207 207 207'
  'set rgb 34 223 223 223'
  'set rgb 35 239 239 239'
  'set rgb 36 255 255 255'
return
