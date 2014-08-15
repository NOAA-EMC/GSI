function name(arg)
*
* run dt variable scale base
* v1.1
*
contour=1
skip=1
repeat=0
pause=0
sec=0
script=''
notitle=0

i=1
wrd=subwrd(arg,i)
while ('a' % substr(wrd,1,1) = 'a-')
say wrd
   if (wrd = '-nocontour')
      contour=0
   endif
   if (wrd = '-contour')
      contour=1
   endif
   if (wrd = '-pause')
      pause=1
   endif
   if (wrd = '-notitle')
      notitle=1
   endif
   if (wrd = '-repeat')
      i = i + 1
      repeat=subwrd(arg,i)
   endif
   if (wrd = '-skip')
      i = i + 1
      skip=subwrd(arg,i)
   endif
   if (wrd = '-sec')
      i = i + 1
      sec=subwrd(arg,i)
   endif
   if (wrd = '-script')
      i = i + 1
      script=subwrd(arg,i)
   endif
   i = i + 1
   wrd=subwrd(arg,i)
endwhile

if (script = '')
  var=subwrd(arg,i)
  i=i+1
else
  var=script
endif

say 'script=' script
say 'var=' var

title=subwrd(arg,i)

if (title = '')
   title=var
endif


'query dim'
diminfo = result
line5 = sublin(diminfo,5)
time1 = subwrd(line5,11)
time2 = subwrd(line5,13)

if (time2 = '')
  say 'time must be varying'
  exit 8
endif

qshade=0
while (repeat >= 0)
   it=time1
   while (it <=time2)
      'set t 'it
      'set gxout shaded'
      'set grads off'
      'set background 99'
      'set line 98'
      'set annot 98'
      'set map 98'
      'set xlopts 98'
      'set ylopts 98'

      if (qshade = 1)
         'set clevs ' lev
         'set ccols ' color
      endif
      if (script = '')
         'display ' var
         'run /u/wx51we/home/grads/cbar98.gs'
      else
         'run ' script
      endif
      if (qshade = 0)
         'query shades'
         shdinfo = result
         nlevs = subwrd(shdinfo,5)
         rec = sublin(shdinfo,2)
         color = subwrd(rec,1)
         lev = ''
         n=2
         while (n <= nlevs)
            rec = sublin(shdinfo,n+1)
            color = color ' ' subwrd(rec,1)
            lev = lev ' ' subwrd(rec,2)
            n = n + 1
         endwhile 
         say shdinfo
         say 'color=' color
         say 'lev=' lev
         qshade=1
      endif
      if (contour = 1)
         'set gxout contour'
         'set clevs ' lev
         'set ccolor 98'
         'set ccols 98'
         if (script != '')
            'run ' script
         endif
      endif
      'q dim'
      rec = sublin(result,5)
      time=subwrd(rec,6)
      if (notitle = 0)
      'draw title ' title '   ' time
      endif
      'swap'
      if (pause != 0)
         'q pos'
         mousekey=subwrd(result,5)
         say result
         if (mousekey = 2) 
            it=time2
            repeat=0
         endif
         if (mousekey = 3) 
	    if (it < time1)
               it=time2-skip
            else
               it=it-skip-skip
            endif
         endif
      else
         if (sec != 0)
           '!sleep ' sec
         endif
      endif
 say 'it=' it
 say 'skip=' skip
      it=it+skip
   endwhile
   repeat = repeat - 1
   if (repeat >= 0 & sec != 0)
      '!sleep ' sec
      '!sleep ' sec
   endif
endwhile

'set t ' time1 ' ' time2
* 'set dbuff off'
