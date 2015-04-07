*
*  Script to plot a colorbar
*
*  The script will assume a colorbar is wanted even if there is 
*  not room -- it will plot on the side or the bottom if there is
*  room in either place, otherwise it will plot along the bottom and
*  overlay labels there if any.  This can be dealt with via 
*  the 'set parea' command.  In version 2 the default parea will
*  be changed, but we want to guarantee upward compatibility in
*  sub-releases.
*
function colorbar (args)
*
*  Check shading information
*
  'query shades'
  shdinfo = result
  if (subwrd(shdinfo,1)='None') 
    say 'Cannot plot color bar: No shading information'
    return
  endif
* 
*  Get plot size info
*
  'query gxinfo'
  rec2 = sublin(result,2)
  rec3 = sublin(result,3)
  rec4 = sublin(result,4)
  xsiz = subwrd(rec2,4)
  ysiz = subwrd(rec2,6)
  ylo = subwrd(rec4,4)
  xhi = subwrd(rec3,6)
  xlo = subwrd(rec3,4)
  yhi = subwrd(rec4,6)
  xd = xsiz - xhi
*
*
  cnum = subwrd(shdinfo,5)
*    ymid = ylo/2
*    ymid = ylo - 0.8
     ymid = ylo - 0.55
     yt = ymid + 0.2
*    say ymid
    yb = ymid
*   xmid = xsiz/2-0.00
    xmid = (xhi - xlo)/2 + xlo
    xwid =(xhi - xlo)/cnum
*    say xmid
*    say xwid
    if (xwid*cnum > xsiz*0.8)
      xwid = xsiz*0.8/cnum
    endif
    xl = xmid - xwid*cnum/2
    'set string 0 tc 5'
    vert = 0
*
*  Plot colorbar
*
  'set strsiz 0.07 0.07'
*  'set strsiz 0.15 0.15'
  num = 0
* yes_label to control label, so that every other color bar is labeled
  tmp=1
  while (num<cnum) 
    rec = sublin(shdinfo,num+2)
    col = subwrd(rec,1)
    hi = subwrd(rec,3)
    'set line 'col
    if (vert) 
      yt = yb + ywid
    else 
      xr = xl + xwid
    endif
    'draw recf 'xl' 'yb' 'xr' 'yt
    'set line 1'
    'draw rec 'xl' 'yb' 'xr' 'yt
*    
    'set strsiz .07'
*
    if (num<cnum-1 & tmp)
      if (vert) 
        'draw string '%(xr+0.05)%' 'yt' 'hi
	 tt1=0
      else
        'draw string 'xr' '%(yb-0.05)%' 'hi
*	say 'hi'
      endif
    endif
    if (tmp=1);tmp=0;else;tmp=1;endif
    num = num + 1
    if (vert); yb = yt;
    else; xl = xr; endif;
  endwhile
  'set strsiz .09'






