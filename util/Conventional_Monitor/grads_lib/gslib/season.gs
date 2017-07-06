function season(arg)

* v1.0.1 wesley ebisuzaki
*
* to plot time series of monthly data with only 1 month
*
* run monmask mon
*  where mon = 1,2,..,12
*
* defines a new variable, "mmask"
*
* instead of "d tmp" use "d tmp*mmask"
* to get the time series with only one month
*
if (arg = '' | arg <= 0 | arg > 12)
   say 'usage: run season N, where N = 1,..,12'
   say ''
   say '  Defines a mask where one month is set to one and the others'
   say 'are undefined.  To display a time series with only may use, '
   say '    set lat 40'
   say '    set lon -90'
   say '    run monmask 5'
   say '    display (expression)*mmask'
   say ''
   exit
endif

monm1=arg-1

'query dim'
diminfo = result
line5 = sublin(diminfo,5)
time1 = subwrd(line5,6)
time2 = subwrd(line5,8)

'set time jan dec'
'define mmask=1'
'define mmasklng=const(mmask(t-' monm1 '),0,-u)'
'define mmasksht=const(mmask(t-' arg '),0,-u)'
'define mmask=mmasklng-mmasksht'
'define mmask=maskout(mmask,mmask-0.5)'
'modify mmask seasonal'


if (time2 = '=') 
  'set time ' time1
else
  'set time ' time1 ' ' time2
endif

'define out1=ave(in,t+0,t+2)'
'define out2=ave(out1*mmask,t-11,t+0)'
exit
