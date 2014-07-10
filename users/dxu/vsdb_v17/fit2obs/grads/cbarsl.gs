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
*
*       modifications by mike fiorino 940614
*                        steve lord 970119 (add optional contour percentages)
*
*      - the extreme colors are plotted as triangles
*        - the colors are boxed in white
*      - input arguments during a run execution:
* 
*     run cbar pctcon sf vert xmid ymid
*
*       pctcon - 0 or 1 - percentages of grid points in each contour level displayed
*       sf   - scale the whole bar 1.0 = original 0.5 half the size, etc.
*       vert - 0 FORCES a horizontal bar = 1 a vertical bar
*       xmid - the x position on the virtual page to center the bar
*       ymid - the y position on the virtual page to center the bar
*
*      if vert,xmid,ymid are not specified, they are selected
*       as in the original algorithm
*  

function colorbar (args)

pctcon=subwrd(args,1)
sf=subwrd(args,2)
vert=subwrd(args,3)
xmid=subwrd(args,4)
ymid=subwrd(args,5)

if(pctcon = ''); pctcon=0; endif
if(sf='');sf=1.0;endif

if(pctcon)
  res=read(pctcons)
  rc=sublin(res,1)
  if(rc != 0)
    if(rc = 1); say "OPEN ERROR FOR PCTCONS: FILE NOT FOUND"; endif
    if(rc = 8); say "PCTCONS OPEN FOR WRITE ONLY"; endif
    if(rc = 9); say "I/O ERROR FOR PCTCONS"; endif
    return
  endif
  pcts=sublin(res,2)
*  say "pcts="pcts
endif
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
  xd = xsiz - xhi

  ylolim=0.6*sf
  xdlim1=1.0*sf
  xdlim2=1.5*sf  
  barsf=0.8*sf
  yoffset=0.3*sf
  stroff=0.06*sf
  strxsiz=0.11*sf
  strysiz=0.12*sf
*
*  Decide if horizontal or vertical color bar
*  and set up constants.
*
  if (ylo<ylolim & xd<xdlim1) 
    say "Not enough room in plot for a colorbar"
    return
  endif
  cnum = subwrd(shdinfo,5)
*
*        logic for setting the bar orientation with user overides
*
  if (ylo<ylolim | xd>xdlim1)
    vchk = 1
    if(vert = 0) ; vchk = 0 ; endif
  else
    vchk = 0
    if(vert = 1) ; vchk = 1 ; endif
  endif
*
* vertical bar
*
  if (vchk = 1 )
    if(xmid = '') ; xmid = xhi+xd/2 ; endif
    xwid = 0.2*sf
    ywid = 0.5*sf    
    xl = xmid-xwid/2
    xr = xl + xwid
    if (ywid*cnum > ysiz*barsf) 
      ywid = ysiz*barsf/cnum
    endif
    if(ymid = '') ; ymid = ysiz/2 ; endif
    yb = ymid - ywid*cnum/2
    ybsav=yb
    xlsav=xl
    'set string 1 l 5'
    vert = 1
  else
*
*     horizontal bar
*
    ywid = 0.4
    xwid = 0.8

    if(ymid = '') ; ymid = ylo/2-ywid/2 ; endif
    yt = ymid + yoffset
    yb = ymid
    if(xmid = '') ; xmid = xsiz/2 ; endif
    if (xwid*cnum > xsiz*barsf)
      xwid = xsiz*barsf/cnum
    endif
    xl = xmid - xwid*cnum/2
    ybsav=yb
    xlsav=xl
    'set string 1 tc 5'
    vert = 0
  endif
*
*  Plot colorbar
*
  'set strsiz 'strxsiz' 'strysiz
  num = 0
  while (num<cnum) 
    rec = sublin(shdinfo,num+2)
    col = subwrd(rec,1)
    hi = subwrd(rec,3)
    if (vert) 
      yt = yb + ywid
    else 
      xr = xl + xwid
    endif

    if(num = 0 )
      if(vert = 1)
        xm=(xl+xr)*0.5
        'set line 'col
        'draw polyf 'xl' 'yt' 'xm' 'yb' 'xr' 'yt' 'xl' 'yt
        'set line 1 1 6'
        'draw line 'xl' 'yt' 'xm' 'yb
        'draw line 'xm' 'yb' 'xr' 'yt
        'draw line 'xr' 'yt' 'xl' 'yt
        xp=xr+stroff
        'draw string 'xp' 'yt' 'hi
      else
        ym=(yb+yt)*0.5
        'set line 'col
       'draw polyf 'xl' 'ym' 'xr' 'yb' 'xr' 'yt' 'xl' 'ym
        'set line 1 1 6'
        'draw line 'xl' 'ym' 'xr' 'yb
        'draw line 'xr' 'yb' 'xr' 'yt
        'draw line 'xr' 'yt' 'xl' 'ym
        yp=yb-stroff
        'draw string 'xr' 'yp' 'hi
      endif
    endif

    if(num!=0 & num!= cnum-1)
      'set line 'col
      'draw recf 'xl' 'yb' 'xr' 'yt
      'set line 1 1 6'
      'draw rec 'xl' 'yb' 'xr' 'yt
      if (num<cnum-1)
        if (vert) 
          xp=xr+stroff
          'draw string 'xp' 'yt' 'hi
        else
          yp=yb-stroff
          'draw string 'xr' 'yp' 'hi
        endif
      endif
    endif

    if(num = cnum-1 )
      if( vert = 1)
        'set line 'col
        'draw polyf 'xl' 'yb' 'xm' 'yt' 'xr' 'yb' 'xl' 'yb
        'set line 1 1 6'
        'draw line 'xl' 'yb' 'xm' 'yt
        'draw line 'xm' 'yt' 'xr' 'yb
        'draw line 'xr' 'yb' 'xl' 'yb
      else
        'set line 'col
        'draw polyf 'xr' 'ym' 'xl' 'yb' 'xl' 'yt' 'xr' 'ym        
        'set line 1 1 6'
        'draw line 'xr' 'ym' 'xl' 'yb
        'draw line 'xl' 'yb' 'xl' 'yt
        'draw line 'xl' 'yt' 'xr' 'ym
      endif
    endif

    num = num + 1
    if (vert); yb = yt;
    else; xl = xr; endif;
  endwhile

  if(pctcon) 
    yb=ybsav
    xl=xlsav 
    num=0
    while(num<cnum)
      pct = subwrd(pcts,num+1)
      if (vert) 
        ym = yb + ywid/2
        xp=xl-stroff
        'set string 1 r 5'
        'draw string 'xp' 'ym' 'pct
      else 
        xm = xl + xwid/2
        yp=yt+stroff
        'set string 1 bc 5'
        'draw string 'xm' 'yp' 'pct
      endif
      num = num + 1
      if (vert); yb = yb + ywid;
      else; xl = xl + xwid; endif;
    endwhile
  endif
return
