function name(arg)
*
* run wanim.gs seconds variable title
* ex .
*   set t 1 10
*   run setrgb2.gs
*   set dbuff on
*     [optional: set contour intervals and colors]
*   run wanim.gs 1 prmsl/100 this_is_a_title
*
* v0.9  w. ebisuzaki CPC/NCEP
*  requires rgbset2.gs and cbar98.gs
*  note: this script requires changes to work on other machines
*

sec=subwrd(arg,1)
var=subwrd(arg,2)
title=subwrd(arg,3)

'query dim'
diminfo = result
line5 = sublin(diminfo,5)
time1 = subwrd(line5,11)
time2 = subwrd(line5,13)
lev=''

it=time1
* 'set dbuff on'
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

  if (lev != '')
     'set clevs ' lev
     'set ccols ' color
  endif
  'display ' var
  'run /usr/local/lib/grads/cbar98.gs'
   if (lev = '') 
    'query shades'
     shdinfo = result
     nlevs = subwrd(shdinfo,5)
     rec = sublin(shdinfo,2)
     color = subwrd(rec,1)
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
   endif
  'set gxout contour'
  'set clevs ' lev
  'set ccolor 98'
  'set ccols 98'
  'display ' var
  'q dim'
  rec = sublin(result,5)
  time=subwrd(rec,6)
  'draw title ' title '   ' time
  'swap'
   if (sec != 0)
     if (sec < 1)
       '!usleep ' sec*1000000
     else
       '!sleep ' sec
     endif
   endif
   it=it+1
endwhile
'set t ' time1 ' ' time2
* 'set dbuff off'
