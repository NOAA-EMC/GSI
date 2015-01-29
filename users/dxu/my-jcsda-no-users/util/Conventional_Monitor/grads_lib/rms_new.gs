'set grads off'
'set gxout line'
'enable print fig.meta'
'open rms.ctl'
'page'
'c'

j=1
ts=00
while (j <=4)
'page.gs q' j
i=3
while (i <=4)
'set axlim 0.0 12.0'
'set yaxis 0.0 12.0 1.0'
'set xaxis 950.0 100.0 50.0'
'set cmark 0'
'set ccolor 1'
*'set ccolor 'i
'set cthick 5'
'set cstyle 'i-2
'set x 'j 
'set y 1 18'
'set z 'i
'd rms'
i=i+1
endwhile
'set line  1 1 5'
'draw line 4.0 7.5 4.5 7.5'
'set line 1 2 5'
'draw line 4.0 7.2 4.5 7.2'
*'set line  3 1 5'
*'draw line 4.0 6.9 4.3 6.9'
*'set line 4 1 5'
*'draw line 4.0 6.6 4.3 6.6'
'set strsiz 0.15'
'set string 1 bl 5'
'draw string 4.6 7.45 20000818'ts' averaged over tropics '
'draw string 4.6 7.15 200008'ts' averaged over tropics'
*'draw string 4.4 6.85 20000818 averaged over tropics'
*'draw string 4.4 6.55 200008 averaged over tropics'

*'draw title Wind vector RMS bet satellite and analysis at 'ts'z'
'draw xlab vertical level (hPa)'
'draw ylab rms(m/s)'
j=j+1
ts=ts+06
endwhile

'print'

