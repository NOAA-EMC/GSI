'set vpage 0.0 11.0 0.0 8.5'
'set grads off'
'set gxout line'
'enable print fig.meta'
'open nms.ctl'

'set vpage 0.0 5.5 4.25 8.5'
'set grads off'
'set xaxis  1 18'
'set vrange 0.0 11.0'
i=1
while (i <=4)
'set cmark 0'
'set ccolor 'i
'set cthick 5'
'set cstyle 'i
'set x 1'
'set y 1 18'
'set z 'i
'd nms'
i=i+1
endwhile
'draw title Sample number at 00z'
'draw xlab Vertical level'
'draw ylab Number(in 1000)'

'set strsiz 0.15'
'set string 1 bl 5'
'set line 1 1 5'
'draw line 4.0  7.0  5.0 7.0'
'draw string 5.1 7.0 20000818 global mean'
'set line 2 2 5'
'draw line 4.0 6.5 5.0 6.5'
'draw string 5.1 6.5 200008 global mean '
'set line 3 3 5'
'draw line 4.0 6.0 5.0 6.0'
'draw string 5.1 6.0 20000818 tropic mean '
'set line 4 4 5'
'draw line 4.0 5.5 5.0 5.5'
'draw string 5.1 5.5 200008 tropical mean'

'set vpage 0.0 5.5 0.0 4.25'
'set xaxis  1 18'
'set vrange 0.0 4.0'
i=1
while (i <=4)
'set cmark 0'
'set ccolor 'i
'set cthick 5'
'set cstyle 'i
'set x 2'
'set y 1 18'
'set z 'i
'd nms'
i=i+1
endwhile
'draw title Sample number at 06z'
'draw xlab Vertical level'
'draw ylab Number(in 1000)'


'set vpage 5.5 11.0 4.25 8.5'
'set xaxis  1 18'
'set vrange 0.0 6.0'
i=1
while (i <=4)
'set cmark 0'
'set ccolor 'i
'set cthick 5'
'set cstyle 'i
'set x 3'
'set y 1 18'
'set z 'i
'd nms'
i=i+1
endwhile
'draw title Sample number at 12z'
'draw xlab Vertical level'
'draw ylab Number(in 1000)'

'set vpage 5.5 11.0  0.0 4.25'
'set xaxis  1 18'
'set vrange 0.0 13.0'
i=1
while (i <=4)
'set cmark 0'
'set ccolor 'i
'set cthick 5'
'set cstyle 'i
'set x 4'
'set y 1 18'
'set z 'i
'd nms'
i=i+1
endwhile
'draw title Sample number at 18z'
'draw xlab Vertical level'
'draw ylab Number(in 1000)'

'print'

