*  "connects the dots" -- while user points and clicks, 
*  this script connects the points with a line using the
*  current line attributes.  User must point and click
*  near the bottom of the screen to quit.
*
say 'Click near bottom of the screen to quit'
'query bpos'
xold = subwrd(result,3)
yold = subwrd(result,4)
while (1)
  'query bpos'
  x = subwrd(result,3)
  y = subwrd(result,4)
  if (y<'0.5'); break; endif;
  'draw line 'xold' 'yold' 'x' 'y
  xold = x
  yold = y
endwhile
