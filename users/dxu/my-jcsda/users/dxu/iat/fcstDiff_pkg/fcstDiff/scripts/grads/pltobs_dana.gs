*
*  Draws in situ  given lon, lat points
*  Script should be run ONLY after something has been displayed
*  so that the scaling environment is defined.
*
* this version modified to reinitialize environment and 
*     open dummy ctl file for example purposes

'reinit'
'set display color  white'
'c'
'open /climate/save/wx24ds/grads/examples/dummy.ctl'
* lat and lon range for data in sample file
'set lon -60 -40'
'set lat 45 55'
'set clevs 999'
* opening a file that does not exist.  can display lat or lon to get envir
'd lat'


* background map should now be set

'q gxinfo'
res=result
xline=sublin(result,3)
yline=sublin(result,4)
xmin=subwrd(xline,4)
xmax=subwrd(xline,6)
ymin=subwrd(yline,4)
ymax=subwrd(yline,6)


* this example plots sst from ascii file of ship and buoy data


* if there was already some shading, use that color scheme otherwise
*   set color scheme.
* while grads environment is being reinitialized in this sample script, 
*  this check for existing shading info is included for completeness
  'query shades'
  shdinfo = result
  if (subwrd(shdinfo,1)='None')
    say 'No shading information; Set up color scheme'

*  set up rainbow sequence of colors
    'set rgb  20 127   0 255'
    'set rgb  21 114  13 255'
    'set rgb  22 101  25 255'
    'set rgb  23  89  38 255'
    'set rgb  24  76  50 255'
    'set rgb  25  63  63 255'
    'set rgb  26  63 101 255'
    'set rgb  27  63 140 255'
    'set rgb  28  63 178 255'
    'set rgb  29  63 217 255'
    'set rgb  30  63 255 255'
    'set rgb  31  63 255 217'
    'set rgb  32  63 255 178'
    'set rgb  33  63 255 140'
    'set rgb  34  63 255 101'
    'set rgb  35  63 255  63'
    'set rgb  36 101 255  50'
    'set rgb  37 140 255  38'
    'set rgb  38 178 255  25'
    'set rgb  39 217 255  13'
    'set rgb  40 255 255   0'
    'set rgb  41 255 229   0'
    'set rgb  42 255 204   0'
    'set rgb  43 255 178   0'
    'set rgb  44 255 153   0'
    'set rgb  45 255 127   0'
    'set rgb  46 255 102   0'
    'set rgb  47 255  76   0'
    'set rgb  48 255  51   0'
    'set rgb  49 255  25   0'
    'set rgb  50 255   0   0'
    'set rgb  51 229  13   0'
    'set rgb  52 204  25   0'
    'set rgb  53 178  38   0'
    'set rgb  54 153  50   0'
    'set rgb  55 127  63   0'
    'set rgb 99 250 250 250'
    'set rgb 98 191 191 191'
    'set rgb 97 220 220 220'
    
*  assign colors
* ranging grey, purple, blue, cyan, green,yellow,orange,red
    _kcol.1 = 98
    _kcol.2 = 20
    _kcol.3 = 21
    _kcol.4 = 22
    _kcol.5 = 23
    _kcol.6 = 24
    _kcol.7 = 25
    _kcol.8 = 26
    _kcol.9 = 27
    _kcol.10= 28
    _kcol.11= 29
    _kcol.12= 30
    _kcol.13= 31
    _kcol.14= 32
    _kcol.15= 33
    _kcol.16= 34
    _kcol.17= 35
    _kcol.18= 36
    _kcol.19= 37
    _kcol.20= 38
    _kcol.21= 39
    _kcol.22= 40
    _kcol.23= 41
    _kcol.24= 42
    _kcol.25= 43
    _kcol.26= 44
    _kcol.27= 45
    _kcol.28= 46
    _kcol.29= 47
    _kcol.30= 48
    _kcol.31= 49
    _kcol.32= 50
    _kcol.33= 51
    _kcol.34= 52
    _kcol.35= 53
    _kcol.36= 54
    _kcol.37= 55
    
    
* set upper limit for sst->color
    _hi.1 =  -1.7
    _hi.2 =  -1
    _hi.3 =   0
    _hi.4 =   1
    _hi.5 =   2
    _hi.6 =   3
    _hi.7 =   4
    _hi.8 =   5
    _hi.9 =   6
    _hi.10=   7
    _hi.11=   8
    _hi.12=   9
    _hi.13=  10
    _hi.14=  11
    _hi.15=  12
    _hi.16=  13
    _hi.17=  14
    _hi.18=  15
    _hi.19=  16
    _hi.20=  17
    _hi.21=  18
    _hi.22=  19
    _hi.23=  20
    _hi.24=  21
    _hi.25=  22
    _hi.26=  23
    _hi.27=  24
    _hi.28=  25
    _hi.29=  26
    _hi.30=  27
    _hi.31=  28
    _hi.32=  29
    _hi.33=  30
    _hi.34=  31
    _hi.35=  32
    _hi.36=  33
    
* number of colors
    _cnum = 36

  else
* using colors from current plot
    _cnum = subwrd(shdinfo,5)
    num = 0
    while (num<_cnum)
      num=num+1
      rec = sublin(shdinfo,num+1)
      _kcol.num = subwrd(rec,1)
      lo.num = subwrd(rec,2)
      _hi.num = subwrd(rec,3)
*      say num' '_hi.num' '_kcol.num
    endwhile
  endif



* read and plot the observation points
    
while (1)
* read ascii file containing one record for each ob
* each record of this sample file contains:
*   stnid, lat, lon, sst, sst_anom, yr, mo, day, hr, station_type
*     station_typ in this file is 1=ship, 2=buoy, 3=buoy, 4=cman
*     type of buoy, drifter vs moored, is determined by id later
  res = read("/climate/save/wx24ds/grads/examples/pltobs_example.asc")
  rc = sublin(res,1)
  if (rc>0); break; endif;
  rec   = sublin(res,2)
  stnid = subwrd(rec,1)
  lat   = subwrd(rec,2)
  lon   = subwrd(rec,3)
  sst   = subwrd(rec,4)
  anom  = subwrd(rec,5)
  type  = subwrd(rec,10)
  'query w2xy 'lon' 'lat
  x = subwrd(result,3)
  y = subwrd(result,6)
  if (x='environment') 
    say 'No scaling Environment'
    break
  endif

* skip if point outside of plot region
   if(x<xmin|x>xmax|y<ymin|y>ymax);continue;endif

* determine color based on sst value
    num=1
    while (num<=_cnum)
      if (sst<_hi.num)
        break
      endif
      num=num+1
    endwhile

* set color of mark using 'set line'
'set line '_kcol.num

* choose mark style and size by station type:
*          ship:  plus sign
*    fixed buoy:  open circle
*       drifter:  closed circle
*          cman:  triangle

  if(type=1)
* ship
    pmark=1
    psize=0.12
  else
* cman
    if(type=4)
     pmark=8
     psize=0.08
    else
* buoy... determine drifter vs moored
    mid_digit=substr(stnid,3,1)
    if (mid_digit<5)
      pmark=2
      psize=0.10
    else
      pmark=3
      psize=0.08
    endif
    endif
  endif

* draw mark of type pmark and size psize 
'draw mark 'pmark' 'x' 'y' 'psize


* if desired, print stnid by setting variable pltsnid to 1
*   note: will often be hard to read as points overlap

pltstnid=0
if(pltstnid=1)
*set string size as suitable
  'set strsiz 0.09 0.10'
  'draw string 'x+0.05' 'y' 'stnid
endif
endwhile
*

'draw title EXAMPLE PLOT OF STATION LOCATIONS'

* plot color bar   (for demo, set bar to 0 if none wanted)
bar=1
if(bar)
  dum=cbarset()
endif

##  don't need following function if color bar won't be needed


* MODIFIED TO FORCE COLORS WHN SHDINFO IS UNDEFINED
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
*
*       - the extreme colors are plotted as triangles
*       - the colors are boxed in white
*       - input arguments in during a run execution:
*
*       run cbarn sf vert xmid ymid
*
*       sf   - scale the whole bar 1.0 = original 0.5 half the size, etc.
*       vert - 0 FORCES a horizontal bar = 1 a vertical bar
*       xmid - the x position on the virtual page the center the bar
*       ymid - the x position on the virtual page the center the bar
*
*       if vert,xmid,ymid are not specified, they are selected
*       as in the original algorithm
*

function cbarset (args)
if(args='args');args='';endif



sf=subwrd(args,1)
vert=subwrd(args,2)
xmid=subwrd(args,3)
ymid=subwrd(args,4)

*say 'args: 'args
*say 'sf: 'sf
*say 'vert: 'vert
*say 'xmid: 'xmid
*say 'ymid: 'ymid

if(sf='');sf=1.0;endif

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
  yoffset=0.2*sf
  stroff=0.05*sf
  strxsiz=0.12*sf
  strysiz=0.13*sf
*
*  Decide if horizontal or vertical color bar
*  and set up constants.
*
  if (ylo<ylolim & xd<xdlim1)
    say "Not enough room in plot for a colorbar"
    return
  endif
*
*       logic for setting the bar orientation with user overides
*
  if (ylo<ylolim | xd>xdlim1)
    vchk = 1
    if(vert = 0) ; vchk = 0 ; endif
  else
    vchk = 0
    if(vert = 1) ; vchk = 1 ; endif
  endif
*
*       vertical bar
*

  if (vchk = 1 )

    if(xmid = '') ; xmid = xhi+xd/2 ; endif
    xwid = 0.2*sf
    ywid = 0.5*sf

    xl = xmid-xwid/2
    xr = xl + xwid
    if (ywid*_cnum > ysiz*barsf)
      ywid = ysiz*barsf/_cnum
    endif
    if(ymid = '') ; ymid = ysiz/2 ; endif
    yb = ymid - ywid*_cnum/2
    'set string 1 l 5 0'
    vert = 1

  else

*
*       horizontal bar
*

    ywid = 0.4
    xwid = 0.8

    if(ymid = '') ; ymid = ylo/2-ywid/2 ; endif
    yt = ymid + yoffset
    yb = ymid
    if(xmid = '') ; xmid = xsiz/2 ; endif
    if (xwid*_cnum > xsiz*barsf)
      xwid = xsiz*barsf/_cnum
    endif
    xl = xmid - xwid*_cnum/2
    'set string 1 tc 5 0'
    vert = 0
  endif


*
*  Plot colorbar
*


  'set strsiz 'strxsiz' 'strysiz' 0'
  num = 0
  while (num<_cnum)
    num1=num+1
    col = _kcol.num1
    hi = _hi.num1
    if (vert)
      yt = yb + ywid
    else
      xr = xl + xwid
    endif

    if(num!=0 & num!= _cnum-1)
    'set line 'col
    'draw recf 'xl' 'yb' 'xr' 'yt
    'set line 1 1 5'
    'draw rec 'xl' 'yb' 'xr' 'yt
    if (num<_cnum-1)
      if (vert)
        xp=xr+stroff
        'draw string 'xp' 'yt' 'hi
      else
        yp=yb-stroff
        'draw string 'xr' 'yp' 'hi
      endif
    endif
    endif

    if(num = 0 )

      if(vert = 1)

        xm=(xl+xr)*0.5
        'set line 'col
        'draw polyf 'xl' 'yt' 'xm' 'yb' 'xr' 'yt' 'xl' 'yt

        'set line 1 1 5'
        'draw line 'xl' 'yt' 'xm' 'yb
        'draw line 'xm' 'yb' 'xr' 'yt
        'draw line 'xr' 'yt' 'xl' 'yt

      else

        ym=(yb+yt)*0.5
        'set line 'col
       'draw polyf 'xl' 'ym' 'xr' 'yb' 'xr' 'yt' 'xl' 'ym

        'set line 1 1 5'
        'draw line 'xl' 'ym' 'xr' 'yb
        'draw line 'xr' 'yb' 'xr' 'yt
        'draw line 'xr' 'yt' 'xl' 'ym


      endif

    endif

    if (num<_cnum-1)
      if (vert)
         xp=xr+stroff
        'draw string 'xp' 'yt' 'hi
      else
         yp=yb-stroff
        'draw string 'xr' 'yp' 'hi
      endif
    endif

    if(num = _cnum-1 )

      if( vert = 1)
        'set line 'col
        'draw polyf 'xl' 'yb' 'xm' 'yt' 'xr' 'yb' 'xl' 'yb

        'set line 1 1 5'
        'draw line 'xl' 'yb' 'xm' 'yt
        'draw line 'xm' 'yt' 'xr' 'yb
        'draw line 'xr' 'yb' 'xl' 'yb

      else

        'set line 'col
        'draw polyf 'xr' 'ym' 'xl' 'yb' 'xl' 'yt' 'xr' 'ym

        'set line 1 1 5'
        'draw line 'xr' 'ym' 'xl' 'yb
        'draw line 'xl' 'yb' 'xl' 'yt
        'draw line 'xl' 'yt' 'xr' 'ym

      endif

    endif

    if (num<_cnum-1)
      if (vert)
        xp=xr+stroff
        'draw string 'xp' 'yt' 'hi
      else
        yp=yb-stroff
       'draw string 'xr' 'yp' 'hi
      endif
    endif

    num = num + 1
    if (vert); yb = yt;
    else; xl = xr; endif;
  endwhile
return
