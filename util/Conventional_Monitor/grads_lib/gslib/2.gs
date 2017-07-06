*
function name(arg)
*
arg1=subwrd(arg,1)
arg2=subwrd(arg,2)
arg3=subwrd(arg,3)
arg4=subwrd(arg,4)

if (arg2 = '')
   say '2 field1 field2 [itime1] [itime2]'
   return
endif

if (arg4 = '')
   'q dims'
   line=sublin(result,5)
   itime=subwrd(line,9)
   if (itime = 'T')
      arg3=subwrd(line,11)
      arg4=subwrd(line,13)
   else
      arg3=itime
      arg4=itime
   endif
endif

itime=arg3
'set dbuff on'
while (itime <= arg4)
   'set t ' itime
   'set gxout shaded'
   'd ' arg1
   'run cbar.gs'
   'set gxout contour'
   'd ' arg2
   'swap'
   itime=itime+1
   prompt 'press (return) for itime=' itime ' '
   pull x
   if (x = 'quit' | x = 'end')
       'set dbuff off'
       'set t ' arg3 ' ' arg4
       return
   endif
endwhile
'set dbuff off'
'set t ' arg3 ' ' arg4
return
