function cmap (args)

if (args='') 

  say 'Enter Number of Colors: '
  pull args
  num = args

  i = 1
  while (i<=num) 
    red.i = 0
    blue.i = 0
    green.i = 0
    cnum = i+39
    'set rgb 'cnum' 127 127 127' 
    i = i + 1
  endwhile

  fname='grads.gct'

else

  num=0
  fname=subwrd(args,1)'.gct'
  while (1)
    rc=read(fname)
    icode=sublin(rc,1)
    card=sublin(rc,2)

    if(icode = 1)
      'c' 
      say 'GrADS color table "'subwrd(args,1)'" file not available'
      say ' '
      say 'hit any key to continue'
      pull tmp
      quit
    endif

    if(icode != 0);break;endif 

    num=num+1
    red.num = subwrd(card,2)
    green.num = subwrd(card,3)
    blue.num = subwrd(card,4)
    cnum = num+39
    'set rgb 'cnum' 'red.num' 'green.num' 'blue.num
  endwhile
  say 'num = 'num
endif

xb = 1
xt = 10
xi = 9/num
x = xb
i = 1
'set string 1 bc'
while (i<=num)
  x1 = x
  x2 = x+xi
  xm = (x1+x2)/2
  cnum = i+39
  'set line 'cnum
  'draw recf 'x1' 7 'x2' 7.3'
  'draw string 'xm' 7.5 'i
  i = i + 1
  x = x + xi
endwhile

'set line 1 1 1'
'draw line 3 1 3 5'
'draw line 5.5 1 5.5 5'
'draw line 8 1 8 5'
'draw line 2.9 1 3.1 1'
'draw line 2.9 5 3.1 5'
'draw line 5.4 1 5.6 1'
'draw line 5.4 5 5.6 5'
'draw line 7.9 1 8.1 1'
'draw line 7.9 5 8.1 5'
r = red.1
g = green.1
b = blue.1
ry = 1 + 4*r/255
gy = 1 + 4*g/255
by = 1 + 4*b/255
'draw line 3.1 'ry' 3.3 'ry
'draw line 5.6 'gy' 5.8 'gy
'draw line 8.1 'by' 8.3 'by
'set string 1 l 1'
'set strsiz 0.14 0.16'
'draw string 3.5 'ry' 'r
'draw string 6.0 'gy' 'g
'draw string 8.5 'by' 'b
'set string 1 c 6'
'set strsiz 0.16 0.18'
'draw string 5.5 6.5 1'
'draw rec 0.2 0.2 1.6 0.95'
'draw string 0.9 0.7 Save &'
'draw string 0.9 0.4 Quit'

'set string 1 c 8'
'set strsiz 0.25'
'draw string 5.5 8.0 GrADS Color Table for :' fname

c = 1
while (1) 

  'q pos'
  x = subwrd(result,3)
  y = subwrd(result,4)
  if (x<1.6 & y<1.1); break; endif;
  if (y>6.9 & y<7.4) 
    i = 1 
    tmp = 0;
    xp = xb
    while (i<=num) 
      if (x>=xp & x<=xp+xi); tmp=i; endif
      i = i + 1
      xp = xp + xi
    endwhile
    if (tmp>0)
      c = tmp
      'set line 0'
      'draw recf 5.2 6.3 5.7 6.8'
      'set string 1 c 6'
      'set strsiz 0.16 0.18'
      'draw string 5.5 6.5 'c
      tmp = 1 + 4*red.c/255
      'set line 0'
      'draw recf 3.1 0.8 4.5 5.1'
      'set line 1 1 1'
      'draw line 3.1 'tmp' 3.3 'tmp
      'set string 1 l 1'
      'draw string 3.5 'tmp' 'red.c
      tmp = 1 + 4*green.c/255
      'set line 0'
      'draw recf 5.6 0.8 7.0 5.1'
      'set line 1 1 1'
      'draw line 5.6 'tmp' 5.8 'tmp
      'set string 1 l 1'
      'draw string 6.0 'tmp' 'green.c
      tmp = 1 + 4*blue.c/255
      'set line 0'
      'draw recf 8.1 0.8 9.5 5.1'
      'set line 1 1 1'
      'draw line 8.1 'tmp' 8.3 'tmp
      'set string 1 l 1'
      'draw string 8.5 'tmp' 'blue.c
    endif
  endif
  flag = 0
  if (y>1 & y<5 & x>2.5 & x<3.5) 
    tmp = 255*(y-1)/4
    red.c = int(tmp) 
    tmp = 1 + 4*red.c/255
    'set line 0'
    'draw recf 3.1 0.8 4.5 5.1'
    'set line 1 1 1'
    'draw line 3.1 'tmp' 3.3 'tmp
    'set string 1 l 1'
    'draw string 3.5 'tmp' 'red.c
    flag = 1
  endif
  if (y>1 & y<5 & x>5.0 & x<6.0) 
    tmp = 255*(y-1)/4
    green.c = int(tmp)
    tmp = 1 + 4*green.c/255
    'set line 0'
    'draw recf 5.6 0.8 7.0 5.1'
    'set line 1 1 1'
    'draw line 5.6 'tmp' 5.8 'tmp
    'set string 1 l 1'
    'draw string 6.0 'tmp' 'green.c
    flag = 1
  endif
  if (y>1 & y<5 & x>7.5 & x<8.5) 
    tmp = 255*(y-1)/4
    blue.c = int(tmp)
    tmp = 1 + 4*blue.c/255
    'set line 0'
    'draw recf 8.1 0.8 9.5 5.1'
    'set line 1 1 1'
    'draw line 8.1 'tmp' 8.3 'tmp
    'set string 1 l 1'
    'draw string 8.5 'tmp' 'blue.c
    flag = 1
  endif
  if (flag) 
    cnum = c+39
    'set rgb 'cnum' 'red.c' 'green.c' 'blue.c
    'set line 'cnum
    x1 = xb+xi*(c-1)
    x2 = x1+xi
    'draw recf 'x1' 7 'x2' 7.3'
  endif
endwhile

i = 1

close (fname)

while (i<=num) 
  say i'   red = 'red.i'  green = 'green.i'   blue = 'blue.i
  ctab=i' 'red.i' 'green.i' 'blue.i
  rc = write ( fname ,ctab)
  i = i + 1
endwhile

'quit'

return

function int(stuff)

  res = ''
  i = 1
  c = substr(stuff,i,1)
  while (c!='' & ('x'%c)!='x.') 
    res = res%c
    i = i + 1
    c = substr(stuff,i,1)
  endwhile
  return res
