function dline(args)
x1=subwrd(args,1)
y1=subwrd(args,2)
x2=subwrd(args,3)
y2=subwrd(args,4)
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
