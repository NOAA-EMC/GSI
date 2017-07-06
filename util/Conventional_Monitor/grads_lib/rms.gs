'set vpage 0.0 11.0 0.0 8.5'
'set grads off'
'set gxout line'
'enable print fig.meta'
'open rms.ctl'
*'set xyrev on'

'set vpage 0.0 5.5 4.25 8.5'
'set grads off'
*'set xyrev on'
'set xaxis  1 18'
'set vrange 0.0 12.0'
i=3
while (i <=4)
'set cmark 0'
'set ccolor 1'
*'set ccolor 'i
'set cthick 5'
'set cstyle 'i-2
'set x 1'
'set y 1 18'
'set z 'i
'd rms'
i=i+1
endwhile
*'draw title Wind vector RMS bet satellite and analysis at 00z'
'draw xlab vertical level (hPa)'
'draw ylab rms(m/s)'


'set vpage 0.0 5.5 0.0 4.25'
'set xaxis  1 18'
'set vrange 0.0 12.0'
i=3
while (i <=4)
'set cmark 0'
*'set ccolor 'i
'set ccolor 1'
'set cthick 5'
'set cstyle 'i-2
'set x 2'
'set y 1 18'
'set z 'i
'd rms'
i=i+1
endwhile
*'draw title Wind vector RMS bet satellite and analysis at 06z'
'draw xlab vertical level (hPa)'
'draw ylab rms(m/s)'
'set grads off'


'set vpage 5.5 11.0 4.25 8.5'
'set xaxis  1 18'
'set vrange 0.0 12.0'
i=3
while (i <=4)
'set cmark 0'
*'set ccolor 'i
'set ccolor 1'
'set cthick 5'
'set cstyle 'i-2
'set x 3'
'set y 1 18'
'set z 'i
'd rms'
i=i+1
endwhile
*'draw title Wind vector RMS bet satellite and analysis at 12z'
'draw xlab vertical level (hPa)'
'draw ylab rms(m/s)'
'set grads off'

'set vpage 5.5 11.0  0.0 4.25'
'set xaxis  1 18'
'set vrange 0.0 12.0'
i=3
while (i <=4)
'set cmark 0'
*'set ccolor 'i
'set ccolor 1'
'set cthick 5'
'set cstyle 'i-2
'set x 4'
'set y 1 18'
'set z 'i
'd rms'
i=i+1
endwhile
*'draw title Wind vector RMS bet satellite and analysis at 18z'
'draw xlab vertical level (hPa)'
'draw ylab rms(m/s)'
'set grads off'
'set strsiz 0.15'
'set string 1 bl 5'
'set line 1 1 5'
'draw line 2.0 7.0  3.5 7.0'
'draw string   3.6 7.0 20000818 tropical mean'
'set line 2 2 5'
'draw line 2.0 6.5 3.5 6.5'
'draw string 3.6 6.5 200008 tropical mean '
'set line 3 3 5'
*'draw line 2.0 6.0 3.5 6.0'
*'draw string 3.6 6.0 20000818 tropic mean '
*'set line 4 4 5'
*'draw line 2.0 5.5 3.5 5.5'
*'draw string 3.6 5.5 200008 tropical mean'

'print'

