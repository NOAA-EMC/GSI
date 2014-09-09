function autoc(arg)
*
* figures out the autocorrelation
*
* v0.8 w. ebisuzaki

var=subwrd(arg,1)

'query dim'
diminfo = result
line5 = sublin(diminfo,5)
time1 = subwrd(line5,11)
time2 = subwrd(line5,13)
date1 = subwrd(line5,6)
date2 = subwrd(line5,8)

i=time1
time21=time2 - 1

r='data from ' date1 ' to ' date2
say r

'define acv1=' var
'define acv2=acv1(t+1)'

'set t 1'

'define acmean1=ave(acv1,t=' time1 ',t=' time21 ')'
'define acmean2=ave(acv2,t=' time1 ',t=' time21 ')'

'define acvar1=ave((acv1-acmean1)*(acv1-acmean1),t=' time1 ',t=' time21 ')'
'define acvar2=ave((acv2-acmean2)*(acv2-acmean2),t=' time1 ',t=' time21 ')'

'define acvarb=ave((acv1-acmean1)*(acv2-acmean2),t=' time1 ',t=' time21 ')'

'define beta=acvarb/sqrt(acvar1*acvar2)'
'd beta'
say 'lag 1 beta is ' r
'set t ' time1 ' ' time2
