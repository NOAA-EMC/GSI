*
* zoom.gs  v1.0                                w. ebisuzaki 1/2002
* a simple way to zoom into a plot
*   usage:
*          d (data)   (must be a map)
*          zoom       (or run zoom.gs for older systems)
*          click on right button to clear old mouse clicks
*          click on bottom left corner of zoom area
*          click on top right corner of zoom area
*          d (data)
*
say 'click right botton .. to clear any previous mouse clicks'
button=0
while (button != 3)
  'q pos'
  button=subwrd(result,5)
endwhile

say 'click on bottom left corner of zoom area'

'q pos'
x=subwrd(result,3)
y=subwrd(result,4)
'q xy2w ' x ' ' y
lat1=subwrd(result,6)
lon1=subwrd(result,3)

say 'click on top right corner of zoom area'
'q pos'
x=subwrd(result,3)
y=subwrd(result,4)
'q xy2w ' x ' ' y
lat2=subwrd(result,6)
lon2=subwrd(result,3)

'set lat ' lat1 ' ' lat2
'set lon ' lon1 ' ' lon2
'c'
