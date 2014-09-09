'open sach.ctl'
'set parea 0.7 10.3 1.5 8'
lon = '-71.0 -71.9 -72.6 -73.4 -74.1 -75.0 -75.8 -76.5 -77.3 -78.1'
lon = lon%' -79.3 -80.3 -81.2 -82.1 -83.1 -83.9 -84.9 -85.7 -86.8'
lon = lon%' -87.4 -88.4 -89.2'
lon = lon%' -91.0 -92.4 -93.0 -93.4 -94.0'
lat = '25.8 25.6 25.5 25.5 25.5 25.4 25.4 25.4 25.4 25.4'
lat = lat%' 25.4 25.4 25.5 25.7 25.7 25.8 26.1 26.3 26.5 26.8 27.2 27.5'
lat = lat%' 28.7 29.7 30.6 31.1 32.0'
cnt = 28
lll = 22
i = 1
'clear'
'set dfile 1'
'set lon -100 -71'
'set lat 20 35'
'set mproj latlon'
'set clevs 100'
'set mpdset nam'
'set grid off'
'set map 15 1 1'
'd ts'
col = -1
'set line 1 5'
while (i<cnt)
  'q w2xy '%subwrd(lon,i)%' '%subwrd(lat,i)
  x = subwrd(result,3)
  y = subwrd(result,6)
  if (i>1)
    'draw line 'xold' 'yold' 'x' 'y
  endif
  xold = x
  yold = y
  i = i + 1
endwhile
i = 1
while (i<cnt)
  if (i>lll); col=7; endif;
  'q w2xy '%subwrd(lon,i)%' '%subwrd(lat,i)
  x = subwrd(result,3)
  y = subwrd(result,6)
  if (i<cnt-2)
    'draw wxsym 41 'x' 'y' 0.30 'col
  else
    'draw wxsym 40 'x' 'y' 0.30 'col
  endif
  i = i + 1
endwhile
'draw title Hurricane Andrew'
'draw wxsym 41 1.5 0.9 0.30 -1'
'set string 1 l 6'
'set strsiz 0.16 0.18'
'draw string 1.7 0.9 3 Hr Positions'
'draw wxsym 41 4.5 0.9 0.30 7'
'draw string 4.7 0.9 Forecast'
llon = '-88.0965 -88.6698 -89.007 -89.1418 -89.1756 -89.3442 -89.0744'
llon = llon%' -89.007 -89.2767 -89.6814 -89.8837 -90.0861 -90.457'
llon = llon%' -90.9628 -91.3674 -91.5698 -91.9407 -92.143 -92.7837'
llon = llon%' -93.3907 -93.9639 -94.3011 -94.6046 -94.7395'
llon = llon%' -94.807 -94.9081 -95.0093 -93.8628'
llat = '30.3239 30.2489 30.2739 30.1739 29.899 29.6741 29.3742 29.0744'
llat = llat%' 28.8744 29.0244 29.2493 29.0744 29.0744 29.0994 29.1743'
llat = llat%' 29.4242 29.6491 29.3742 29.4742 29.6241 29.5991 29.4742'
llat = llat%' 29.3742 29.2243 28.9744 28.7745 28.7245 17.9789'
'set line 8 1 6'
i = 1
while (i<28)
  if (i<3 | i>24)
    'set line 12 1 12'
  else
    'set line 6 1 12'
  endif
  'q w2xy '%subwrd(llon,i)%' '%subwrd(llat,i)
  x = subwrd(result,3)
  y = subwrd(result,6)
  if (i>1)
    'draw line 'xold' 'yold' 'x' 'y
  endif
  xold = x
  yold = y
  i = i + 1
endwhile
'set line 6 1 12'
'draw line 6.5 0.9 7.0 0.9'
'draw string 7.2 0.9 Hurricane Warning'
'draw string 0.8 2.1 11AM EDT Teusday'
'set string 1 c 6'
'draw string 5.5 0.4 27.5N, 89.2W    140 MPH    944 MB'
pull dummy
