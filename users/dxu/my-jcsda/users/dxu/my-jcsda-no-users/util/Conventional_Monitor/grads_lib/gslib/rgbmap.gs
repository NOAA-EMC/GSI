r = 0
b = 80
c = 16
y = 0.5
while (r<256)
  x = 0.5
  g = 0
  while (g<256)
    'set rgb 'c' 'r' 'g' 'b
    'set line 'c
    'draw recf 'x' 'y' '%(x+0.8)%' '%(y+0.6)
    g = g + 30
    c = c + 1
    x = x + 1
  endwhile
  y = y + 0.8
  r = r + 30
endwhile

