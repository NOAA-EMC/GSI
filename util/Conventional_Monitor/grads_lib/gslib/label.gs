'set strsiz 0.11 0.13'
w = 0.11*3/2 + 0.02
h = 0.11/2 + 0.02
while (1) 
  pull lab
  if (lab='quit'); break; endif;
  if (lab='200') 
    'set string 1 c 7'
  else 
    'set string 1 c 4'
  endif
  'q pos'
  x = subwrd(result,3)
  y = subwrd(result,4)
  'set line 0'
  'draw recf '%(x-w)%' '%(y-h)%' '%(x+w)%' '%(y+h)
  'draw string 'x' 'y' 'lab
endwhile
