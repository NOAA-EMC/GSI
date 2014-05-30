function dlymsk(arg)

* v0.9 wesley ebisuzaki
*
*
* instead of "d tmp" use "d tmp*mX"
*

* get time info
'query dim'
diminfo = result
line5 = sublin(diminfo,5)
time1 = subwrd(line5,6)
time2 = subwrd(line5,8)

'set t 1 4'

'define mask=0'
'define m1=const(mask(t-1),1,-u)'
'define m1=m1/m1'
'define m2=m1(t-1)'
'define m3=m2(t-1)'
'define m4=m3(t-1)'

'modify m1 diurnal'
'modify m2 diurnal'
'modify m3 diurnal'
'modify m4 diurnal'


if (time2 = '=') 
  'set time ' time1
else
  'set time ' time1 ' ' time2
endif

'set missconn on'
exit
