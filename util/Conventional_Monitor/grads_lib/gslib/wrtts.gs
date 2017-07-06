function wrtts(arg)
*
* writes out a time series as a text file
* usage run wrtts.gs variablename file
*
* v1.0 w. ebisuzaki

var=subwrd(arg,1)
file=subwrd(arg,2)

'query dim'
diminfo = result
line5 = sublin(diminfo,5)
time1 = subwrd(line5,11)
time2 = subwrd(line5,13)
date1 = subwrd(line5,6)
date2 = subwrd(line5,8)

i=time1

r='data from ' date1 ' to ' date2
err=write(file, r )
err=write(file, '"----------"', append)

while (i <= time2)
   'set t ' i
   'd ' var
   r=subwrd(result,4)
   err=write(file, r, append)
   i = i + 1
endwhile
'set t ' time1 ' ' time2
