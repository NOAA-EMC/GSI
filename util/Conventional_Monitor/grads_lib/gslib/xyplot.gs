*  Script to draw an XY plot.  
*  Does no Error checking on the input file at all. 
*  Assumes the input file is set up as follows:
*
*    Line 1:  Title
*    Line 2:  X Axis Label
*    Line 3:  Y Axis Label (not drawn; needs a GrADS mod)  
*    Line 4:  Axes Limits:  xmin xmax ymin ymax
*    Line 5:  Axes Labels:  xlow xint ylow yint
*    Rest of lines:  X Y points
*
*  Also assumes that a file has been opened (any file, doesn't
*  matter -- the set command doesn't work until a file has been
*  opened).
*
function main(args)

'clear'

if (args='') 
  say 'Enter File Name:'
  pull fname
else
  fname = args
endif

*  Read the 1st record: Title

ret = read(fname)
rc = sublin(ret,1)
if (rc>0) 
  say 'File Error'
  return
endif
title = sublin(ret,2)

*  Read the 2nd record: X Label

ret = read(fname)
rc = sublin(ret,1)
if (rc>0) 
  say 'File Error'
  return
endif
xlab = sublin(ret,2)

*  Read the 3rd record: Y Label

ret = read(fname)
rc = sublin(ret,1)
if (rc>0) 
  say 'File Error'
  return
endif
ylab = sublin(ret,2)

*  Read the 4th record: xmin xmax ymin ymax

ret = read(fname)
rc = sublin(ret,1)
if (rc>0) 
  say 'File Error'
  return
endif
rec = sublin(ret,2)
xmin = subwrd(rec,1)
xmax = subwrd(rec,2)
ymin = subwrd(rec,3)
ymax = subwrd(rec,4)

xdif = xmax - xmin
xdif = 9.0/xdif
ydif = ymax - ymin
ydif = 6.0/ydif

*  Read the 5th record: xlow xint ylow yint

ret = read(fname)
rc = sublin(ret,1)
if (rc>0) 
  say 'File Error'
  return
endif
rec = sublin(ret,2)
xlow = subwrd(rec,1)
xint = subwrd(rec,2)
ylow = subwrd(rec,3)
yint = subwrd(rec,4)

* Loop on the rest of the records: x y

first = 0
'set ccolor 1'
'set parea 1 10 1.5 7.5'
'set line 1 1 6'
'draw line 1 1.5 10 1.5'
'draw line 10 1.5 10 7.5'
'draw line 10 7.5 1 7.5'
'draw line 1 7.5 1 1.5'
'set line 1 1 3'

while (1)
  ret = read(fname)
  rc = sublin(ret,1)
  if (rc>0) 
    if (rc!=2) 
      say 'File I/O Error'
      return
    endif
    break
  endif
  rec = sublin(ret,2)
  x = subwrd(rec,1)
  y = subwrd(rec,2)
  x = 1.0+(x-xmin)*xdif
  y = 1.5+(y-ymin)*ydif
  say x' 'y
  if (first) 
    'draw line 'xold' 'yold' 'x' 'y
    'set line 0'  
    'draw mark 3 'xold' 'yold' 0.1'
    'set line 1'
    'draw mark 2 'xold' 'yold' 0.1'
  endif
  first = 1
  xold = x
  yold = y
endwhile
'set line 0'  
'draw mark 3 'xold' 'yold' 0.1'
'set line 1'
'draw mark 2 'xold' 'yold' 0.1'

'set line 1 1 5'
'set string 1 tc 5'
'set strsiz 0.15 0.17'
say xlow' 'xint' 'ylow' 'yint
xx = xlow*'1.0'
while (xx<=xmax & xx>=xmin)
  x = 1.0+(xx-xmin)*xdif
  'draw line 'x' 1.5 'x' 1.47'
  'draw string 'x' 1.40 'xx
  xx = xx + xint
endwhile
yy = ylow*'1.0'
'set string 1 r 5'
while (yy<=ymax & yy>=ymin)
  y = 1.5+(yy-ymin)*ydif
  'draw line 0.97 'y' 1.0 'y
  'draw string 0.90 'y' 'yy
  yy = yy + yint
endwhile

'set string 1 c 6'
'set strsiz 0.23 0.26'
'draw string 5.5 7.9 'title
'set strsiz 0.18 0.20'
'draw string 5.5 0.8 'xlab
