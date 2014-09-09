function dline(args)
x1=0
y1=0
x2=360
y2=0
*
'query ll2xy 'x1' 'y1
line=sublin(result,1)
x1pos=subwrd(line,1)
y1pos=subwrd(line,2)
*
'query ll2xy 'x2' 'y2
line=sublin(result,1)
x2pos=subwrd(line,1)
y2pos=subwrd(line,2)
*
*'set line 1 1 6'
'set line 1 1 1'
'draw line 'x1pos' 'y1pos' 'x2pos' 'y2pos
