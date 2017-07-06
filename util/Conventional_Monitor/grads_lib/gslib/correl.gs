function correl(arg)
*
*
a=subwrd(arg,1)
b=subwrd(arg,2)

'query dim'
diminfo = result
line5 = sublin(diminfo,5)
time1 = subwrd(line5,6)
time2 = subwrd(line5,8)
say time1
say time2
say a
say b
'set t 1'
say 'define ma=ave(' a ',time=' time1 ',time=' time2 ')'
'define ma=ave(' a ',time=' time1 ',time=' time2 ')'
'define mb=ave(' b ',time=' time1 ',time=' time2 ')'
say 'define aa=ave((' a '-ma)*(' a '-ma),time=' time1 ',time=' time2 ')'
'define aa=ave((' a '-ma)*(' a '-ma),time=' time1 ',time=' time2 ')'
'define bb=ave((' b '-mb)*(' b '-mb),time=' time1 ',time=' time2 ')'
say 'define ab=ave((' a '-ma)*(' b '-mb),time=' time1 ',time=' time2 ')'
'define ab=ave((' a '-ma)*(' b '-mb),time=' time1 ',time=' time2 ')'
say 'define rho=ab/sqrt(aa*bb)'
'define rho=ab/sqrt(aa*bb)'
'd rho'
say result
'set time ' time1 ' ' time2

