function name(arg)
*
arg1=subwrd(arg,1)
arg2=subwrd(arg,2)

'query time'
line1 = sublin(result,1)
time1=subwrd(line1,3)
time2=subwrd(line1,5)

if (time1 != time2)
   say 'climo.gs cannot be used with a time range'
   return
endif

day=substr(time1,4,2)
month=substr(time1,6,3)

mon.1=JAN
mon.2=FEB
mon.3=MAR
mon.4=APR
mon.5=MAY
mon.6=JUN
mon.7=JUL
mon.8=AUG
mon.9=SEP
mon.10=OCT
mon.11=NOV
mon.12=DEC

t=1
while (mon.t != month)
  t=t+1
endwhile

if (day > 14)
   s=t+1
   if (s = 13)
      s=1
   endif
   fday=day-15
else
   s=t
   t=t-1
   if (t = 0)
      t=12
   endif
   fday=15-day
endif
ffday=30-fday


say  time1
say arg2 ' =(' ffday '*' arg1 '(t=' t ') + ' fday '*' arg1 '(t=' s '))/30.0'
'define ' arg2 ' =(' ffday '*' arg1 '(t=' t ') + ' fday '*' arg1 '(t=' s '))/30.0'
return
