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
  yhi = subwrd(rec4,6)
  xlo = subwrd(rec3,4)
  xhi = subwrd(rec3,6)
  xd = xsiz - xhi
*
*  Decide if horizontal or vertical color bar
*  and set up constants.
*
  if (ylo<0.6 & xd<1.0) 
    say "Not enough room in plot for a colorbar"
    return
  endif
  cnum = subwrd(shdinfo,5)
  if (ylo<0.6 | xd>1.5)
    xl = xhi + xd/2 - 0.4
    xr = xl + 0.2
    xwid = 0.2
    ywid = 0.5
    if (ywid*cnum > yhi-ylo) 
      ywid = (yhi-ylo)/cnum
    endif
    ymid = (yhi+ylo)/2
    yb = ymid - ywid*cnum/2
    yt = ymid + ywid*cnum/2
    'set string 98 l 5'
    vert = 1
  else
*   ymid = ylo/2
    ymid = ylo/2 - 0.1
    yt = ymid + 0.2
    yb = ymid
    xmid = (xlo + xhi)/2
    xwid = 0.8
    if (xwid*cnum > xhi-xlo)
      xwid = (xhi-xlo)/cnum
    endif
    xl = xmid - xwid*cnum/2
    xr = xmid + xwid*cnum/2
    'set string 98 tc 5'
    vert = 0
  endif
  yt0=yt
  yb0=yb
  xl0=xl
  xr0=xr
*
*  Plot colorbar
*
  'set strsiz 0.12 0.13'
* i like smaller characters ... wne
*  tmp=xsiz/11
  tmp=xsiz/9
  'set strsiz ' 0.09*tmp ' ' 0.115*tmp
  num = 0
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
    if (num<cnum-1)
      if (vert) 
        'draw string '%(xr+0.05)%' 'yt' 'hi
      else
        'draw string 'xr' '%(yb-0.05)%' 'hi
      endif
    endif
    num = num + 1
    if (vert); yb = yt;
    else; xl = xr; endif;
  endwhile
  'set line 98'
  'draw line ' xl0 ' ' yt0 ' ' xr0 ' ' yt0
  'draw line ' xl0 ' ' yb0 ' ' xr0 ' ' yb0
  'draw line ' xl0 ' ' yb0 ' ' xl0 ' ' yt0
  'draw line ' xr0 ' ' yb0 ' ' xr0 ' ' yt0
