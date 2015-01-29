function main(inpargs)

'reinit'
'open plottrak.ctl'
'clear'
'reset'
'clear events'

'q dims'
drec = sublin(result,5)
_zdate = subwrd(drec,6)

_userid = subwrd(inpargs,1)
_basin  = subwrd(inpargs,2)
_rundir = subwrd(inpargs,3)
_netdir=_rundir               


*------------------------------------------*
* In this first section are many defaults
* which are set for various display 
* features.  Many of these can be reset
* using the various buttons on the 
* graphical interface....
*------------------------------------------*

_startdate = 9999
_freshcascade = yes
_plotfcst  = yes
_plotverif = yes
_trkfcsttype = single
_trkmrkrintrvl = every12
_trkmrkrint    = 12
_trkverfintrvl = every12
_trkverflen = next120
_legendloc = upright
_modcoltype = rainbow
_fcstlen = 999
_verifcol = 1
_mapcol = 1
_gridcol = 15
_gridstyle = 5
_zoom = no
_outcount = 0
_drawsig = yes

*------------------------------------------*

'set font 0'
'set lev 850'
'set mpdset hires'
'set map '_mapcol' 1 4'
'set grid on '_gridstyle' '_gridcol

rc = rdstorm()
rc = mainbox()
rc = gettrkopts()
rc = modnames()
rc = settrkcols()
rc = setbtncols()
_randnum = getrandom()

* 'set vpage 0 11 0 8.5'
'set parea 1.6 10.95 0.9 7.7'

while (1)

  'clear'
  rc = initmenu()
  rc = modelsoff()

  while (1)

    'q pos'
    mousenum = subwrd(result,5)
    widclass = subwrd(result,6)
    button   = subwrd(result,7)

*    say result

    if (button = 1)
*     "Main" dropmenu....
      item   = subwrd(result,8)
      choice = _mainfunc.item
      if (choice = "return")
        say "!!! Exiting script...."
        return
      else
        if (choice = "quit")
          say "!!! Quitting GrADS...."
          quit
        else
*          say 'calling process_main, choice= 'choice
          rc = process_main(choice)
        endif
       endif
    endif

    if (button = 2)
*     "Storms" dropmenu....
      item = subwrd(result,8)
*      say 'storms dropmenu, item= 'item
      if (item > 0)
        _freshcascade = yes
        _startdate = 9999
        rc = rddates(item)
        rc = redomenu()
        rc = modelsoff()
*        rc = printmodstat()
      endif
    endif

    if (button = 3)
*     "Dates" dropmenu....
      cnum = subwrd(result,9)
      citem = subwrd(result,10)
      if (citem > 0)
        _startdate = _ymdh.cnum.citem
        _zoom = no
        say '_startdate= '_startdate
      endif
    endif

    if (button = 4)
*     "Track" dropmenu....
      item = subwrd(result,8)
      rc = gettrkbtn(result)
      say ' '
      say 'trkfcsttype= '_trkfcsttype
      say 'trkverfintrvl= '_trkverfintrvl
      say 'trkverflen= '_trkverflen
      say 'trkmrkrint= '_trkmrkrint
    endif

    if (button = 5)
*     "Opts_1" dropmenu....
      item = subwrd(result,8)
      rc = getopts1(result)
*      say 'Opts_1 menu, item choice = 'item
    endif

*   NOTE: button 6 is used by the rband in rbandzoom.

    if (button = 7)
*     "Opts_2" dropmenu....
      rc = getopts2(result)
    endif

    if (button > 100 & button < 200)
      rc = clickmodel(button)
    endif

    if (button = 201)
      rc = makeplot()
    endif

    if (button = 202)
      rc = rbandzoom()
    endif

    if (button = 203)
      rc = zoomin5()
    endif
 
    if (button = 204)
      rc = zoomout5()
    endif
 
    if (button = 205)
      rc = zoomorig()
    endif
 
    if (button = 98)
      say '!!! Exiting script....'
      'redraw button 98 0'
      return
    endif

    if (button = 99)
      say '!!! QUITTING GrADS....'
      'redraw button 99 0'
      quit
    endif

  endwhile

endwhile

return

*---------------------------
*
*---------------------------
function makeplot()

if (_plotfcst = yes)
  rc = makeflist()
  if (rc = 99)
    return
  endif
endif
if (_plotverif = yes)
  rc = makevlist()
endif

if (_zoom = no)
  rc = getbounds()
*  say 'makeplot no: slat= '_southlat' nlat= '_northlat' wlon= '_westlon' elon= '_eastlon
  'set lat '_southlat' '_northlat
  'set lon '_westlon' '_eastlon
else
  if (_zoom = yes)
*    say 'makeplot yes: slat= '_southlat' nlat= '_northlat' wlon= '_westlon' elon= '_eastlon
    'set lat '_southlat' '_northlat
    'set lon '_westlon' '_eastlon
  else
    if (_zoom = original)
*      say 'makeplot orig: slat= '_southlat' nlat= '_northlat' wlon= '_westlon' elon= '_eastlon
      'set lat '_origslat' '_orignlat
      'set lon '_origwlon' '_origelon
    endif
  endif
endif

'clear graphics'
'set black -10000 10000'
'set grads off'
'd u'

rc = getgxinfo()
if (_plotfcst = yes)
  rc = drawfcst()
endif
if (_plotverif = yes)
  rc = drawverif()
endif

rc = drawtitle()
if (_drawsig = yes)
  rc = plotsig()
endif

rc = redomenu(yes)

return

*---------------------------
*
*---------------------------
function getbounds()

* This function looks through the forecast tracks points
* and/or the observed track points and figures out 
* which points are the farthest N,E,S and W.  Remember, 
* we are using degrees positive westward, the TPC standard.

_northlat = -99.0 
_southlat =  99.0
_westlon  =   0.0
_eastlon  = 361.0

fcstfile = _netdir%flist
verffile = _netdir%vlist

say 'fcstfile= 'fcstfile
say 'verffile= 'verffile

ict = 1

if (_plotfcst = yes)

  while (1)
  
    res = read(fcstfile)
    rc  = sublin(res,1)
    if(rc != 0)
      if(rc = 2)
        say 'End of forecast track file '
        say ' '
        break
      endif
      if(rc = 1); say 'rc=1: OPEN ERROR FOR 'fcstfile; endif
      if(rc = 8); say 'rc=8: 'fcstfile' OPEN FOR WRITE ONLY'; endif
      if(rc = 9); say 'rc=9: I/O ERROR FOR 'fcstfile; endif
      return 99
    endif

    rec = sublin(res,2)
*    say rec

    model = subwrd(rec,2)
    thour = subwrd(rec,3)
    ylat = subwrd(rec,4)
    xlon = subwrd(rec,5)

    if (ylat > _northlat)
      _northlat = ylat
    endif
    if (ylat < _southlat)
      _southlat = ylat
    endif
    if (xlon > _westlon)
      _westlon = xlon
    endif
    if (xlon < _eastlon)
      _eastlon = xlon
    endif

*    say ' '
*    say 'model= 'model' thour= 'thour' ylat= 'ylat' xlon= 'xlon
*    say '  northlat = '_northlat'  southlat = '_southlat
*    say '  westlon  = '_westlon'   eastlon  = '_eastlon

  endwhile

  ict = ict + 1

  rc = close(fcstfile)

endif

if (_plotverif = yes)

  while (1)

    res = read(verffile)
    rc  = sublin(res,1)
    if(rc != 0)
      if(rc = 2)
        say 'End of verification file '
        say ' '
        break
      endif
      if(rc = 1); say 'rc=1: OPEN ERROR FOR 'verffile; endif
      if(rc = 8); say 'rc=8: 'verffile' OPEN FOR WRITE ONLY'; endif
      if(rc = 9); say 'rc=9: I/O ERROR FOR 'verffile; endif
      return 99
    endif

    rec = sublin(res,2)

    thour = subwrd(rec,3)
    ylat = subwrd(rec,4)
    xlon = subwrd(rec,5)

    if (ylat > _northlat)
      _northlat = ylat
    endif
    if (ylat < _southlat)
      _southlat = ylat
    endif
    if (xlon > _westlon)
      _westlon = xlon
    endif
    if (xlon < _eastlon)
      _eastlon = xlon
    endif

*    say ' '
*    say 'CARQ:          thour= 'thour' ylat= 'ylat' xlon= 'xlon
*    say '  northlat = '_northlat'  southlat = '_southlat
*    say '  westlon  = '_westlon'   eastlon  = '_eastlon

  endwhile

  rc = close(verffile)
  
endif

* Now switch to positive degrees eastward, for the sake
* of matching up with the GrADS .ctl for the data.

_northlat = _northlat + 10
_southlat = _southlat - 10
_westlon  = _westlon  + 10
_eastlon  = _eastlon  - 10

if (_eastlon < 0)
  say ' '
  say '!!! eastlon being forced to be only at 0 degrees E'
  say ' '
  _eastlon = 0
endif

_westlon  = 360 - _westlon
_eastlon  = 360 - _eastlon

* say ' '
* say 'At end of getbounds....'
* say '  northlat = '_northlat'  southlat = '_southlat
* say '  westlon  = '_westlon'   eastlon  = '_eastlon

_origwlon = _westlon
_origelon = _eastlon
_orignlat = _northlat
_origslat = _southlat

return

*---------------------------
*
*---------------------------
function getgxinfo()

'q gxinfo'
rec = sublin(result,1)
_lastgraph = subwrd(rec,4)
xrec = sublin(result,3)
_xleft  = subwrd(xrec,4)
_xright = subwrd(xrec,6)
yrec = sublin(result,4)
_ylo = subwrd(yrec,4)
_yhi = subwrd(yrec,6)

return

*----------------------------
*
*----------------------------
function rbandzoom()

'q gxinfo'
rec = sublin(result,1)
_lastgraph = subwrd(rec,4)
xrec = sublin(result,3)
xl  = subwrd(xrec,4)
xr = subwrd(xrec,6)
yrec = sublin(result,4)
ylo = subwrd(yrec,4)
yhi = subwrd(yrec,6)

* 'q xy2w 'xl' 'ylo
* say result
* txlon1 = subwrd(result,3)
* ttlat1 = subwrd(result,6)
* 'q xy2w 'xr' 'yhi
* say result
* txlon2 = subwrd(result,3)
* ttlat2 = subwrd(result,6)
* 
* ttlon1 = 360 - txlon1
* ttlon2 = 360 - txlon2
* 
* say 'RBZ: xl= 'xl' ylo= 'ylo' xr= 'xr' yhi= 'yhi
* say 'RBZ: tlon1= 'ttlon1' tlon2= 'ttlon2' tlat1= 'ttlat1' tlat2= 'ttlat2
* 
* 'set line 2 1 6'
* 'draw mark 6 'xl' 'ylo' 0.2'
* 'draw mark 6 'xl' 'yhi' 0.2'
* 'draw mark 6 'xr' 'ylo' 0.2'
* 'draw mark 6 'xr' 'yhi' 0.2'

'set rband 6 box 'xl' 'ylo' 'xr' 'yhi
'query pos'
say result

vpx1 = subwrd(result,3)
vpx2 = subwrd(result,8)
vpy1 = subwrd(result,4)
vpy2 = subwrd(result,9)

widgetclass = subwrd(result,6)
if (widgetclass != 2)
* User clicked outside of plot area.  Ignore and return....
  'redraw button 202 0'
  return
endif

* say 'vpx1= 'vpx1' vpx2= 'vpx2' vpy1= 'vpy1' vpy2= 'vpy2

'q xy2w 'vpx1' 'vpy1
say result
rblon1 = subwrd(result,3)
rblat1 = subwrd(result,6)
'q xy2w 'vpx2' 'vpy2
say result
rblon2 = subwrd(result,3)
rblat2 = subwrd(result,6)

if (rblon1 = rblon2 | rblat1 = rblat2)
  return
endif

if (rblon1 < rblon2)
  _westlon = rblon1
  _eastlon = rblon2
else
  _westlon = rblon2
  _eastlon = rblon1
endif

if (rblat1 < rblat2)
  _southlat = rblat1
  _northlat = rblat2
else
  _southlat = rblat2
  _northlat = rblat1
endif

* say 'rblon1= 'rblon1'  rblat1= 'rblat1
* say 'rblon2= 'rblon2'  rblat2= 'rblat2

_zoom = yes

rc = makeplot()

return

*----------------------------
*
*----------------------------
function zoomin5()

tmpnlat = _northlat - 5
tmpslat = _southlat + 5
tmpwlon = _westlon + 5
tmpelon = _eastlon - 5

if (tmpnlat <= tmpslat | tmpelon <= tmpwlon)
  'set dialog 1 0 6 14 6'
  'q dialog !!! Cannot zoom in any further.... | <Hit enter to continue>'
  say result
  'redraw button 203 0'
  return
endif

_northlat = tmpnlat
_southlat = tmpslat
_westlon  = tmpwlon
_eastlon  = tmpelon

_zoom = yes

rc = makeplot()

return

*----------------------------
*
*----------------------------
function zoomout5()

tmpnlat = _northlat + 5
tmpslat = _southlat - 5
tmpwlon = _westlon - 5
tmpelon = _eastlon + 5

if (tmpnlat > 90 | tmpslat < -90 | tmpwlon < 0 | tmpelon > 359)
  'set dialog 1 0 6 14 6'
  'q dialog !!! Cannot zoom out any further.... | <Hit enter to continue>'
  say result
  'redraw button 204 0'
  return
endif

_northlat = tmpnlat
_southlat = tmpslat
_westlon  = tmpwlon
_eastlon  = tmpelon

_zoom = yes

rc = makeplot()

return

*----------------------------
*
*----------------------------
function zoomorig()

_northlat = _orignlat
_southlat = _origslat
_westlon  = _origwlon
_eastlon  = _origelon

_zoom = original

rc = makeplot()

return

*----------------------------
*
*----------------------------
function drawfcst()

fcstfile = _netdir%flist

modelct  = 0
oldmodel = XXXX
oldymdh  = 9999999999

while (1)

  res = read(fcstfile)
  rc  = sublin(res,1)
  if(rc != 0)
    if(rc = 2)
      say 'End of track file '
      say ' '
      break
    endif
    if(rc = 1); say 'rc=1: OPEN ERROR FOR 'fcstfile; endif
    if(rc = 8); say 'rc=8: 'fcstfile' OPEN FOR WRITE ONLY'; endif
    if(rc = 9); say 'rc=9: I/O ERROR FOR 'fcstfile; endif
    return 99
  endif

  rec = sublin(res,2)

  ymdh   = subwrd(rec,1)
  model  = subwrd(rec,2)
  fhr    = subwrd(rec,3)
  newlat = subwrd(rec,4)
  newlon = subwrd(rec,5)
  _atcfnum = subwrd(rec,6)

  modelsubstr = substr(model,1,3)
*  if (modelsubstr = 'AP0' | modelsubstr = 'AN0' | modelsubstr = 'AEM')
  if (modelsubstr = 'AP0' | modelsubstr = 'AN0')
*    modtype = ensemble
    modtype = regular
  else
    modtype = regular
  endif

  newlon = 360.0 - newlon

  if (model = oldmodel & ymdh = oldymdh)

    'query ll2xy 'lastlon' 'lastlat
    xdum  = sublin(result,1)
    xlast = subwrd(xdum,1)
    ylast = subwrd(xdum,2)

    'query ll2xy 'newlon' 'newlat
    xdum = sublin(result,1)
    xnew = subwrd(xdum,1)
    ynew = subwrd(xdum,2)

    oldbound = in
    newbound = in
    if (xlast > _xright | xlast < _xleft | ylast > _yhi | ylast < _ylo)
      oldbound = out
    endif
    if (xnew > _xright | xnew < _xleft | ynew > _yhi | ynew < _ylo)
      newbound = out
    endif

    if (oldbound = in | newbound = in)
      if (fhr <= _fcstlen)
*TM        if (modtype = ensemble)
        if (modtype = xxxemble)
          'set line 'trackcolor' 1 3'
        else
          'set line 'trackcolor' 1 7'
        endif
        'draw line 'xlast' 'ylast' 'xnew' 'ynew

        checkmark = math_fmod(fhr,_trkmrkrint)
*        say 'fhr= 'fhr' model= 'model' trkmrkrint= '_trkmrkrint' checkmark= 'checkmark
        if (checkmark = 0 & _trkmrkrintrvl != none)
          if (modtype = ensemble)
*TM            'set line 23 1 4'
            'set line 'trackcolor' 1 4'
            'draw mark 3 'xnew' 'ynew' 0.05'
          else
            'set string 'trackcolor' c 6'
            'set strsiz 0.14 0.14'
            'draw string 'xnew' 'ynew' 'modelct
          endif
        endif
      endif

    endif

  else

    modelct = modelct + 1

    if (_modcoltype = rainbow)
*TM      if (modtype = ensemble)
      if (modtype = xxxemble)
        trackcolor = 23
      else
        colorix = math_fmod(modelct,_numtrkcols)
        if (colorix = 0)
          ntix = _numtrkcols
          trackcolor = _trkcol.ntix
        else
          trackcolor = _trkcol.colorix
        endif
      endif
    else
      trackcolor = _modmonocol
    endif

    if (modelct = 1) 
      _ftitleymdh = ymdh
      _ftitlemodel = model
    endif

    'query ll2xy 'newlon' 'newlat
    xdum = sublin(result,1)
    xnew = subwrd(xdum,1)
    ynew = subwrd(xdum,2)

    if (_trkmrkrintrvl != none & modtype != ensemble)
      if (xnew <= _xright & xnew >= _xleft & ynew <= _yhi & ynew >= _ylo & fhr <= _fcstlen)
        'set string 'trackcolor' c 6'
        'set strsiz 0.14 0.14'
        'draw string 'xnew' 'ynew' 'modelct
      endif
    endif

    if (_legendloc != none & _trkmrkrintrvl != none & modtype != ensemble)
      rc = drawlegend(modelct,model,trackcolor,ymdh)
    endif

  endif
     
  oldmodel = model
  oldymdh  = ymdh
  lastlon  = newlon
  lastlat  = newlat

endwhile

rc = close(fcstfile)

return

*--------------------------
*
*--------------------------
function drawverif()

verffile = _netdir%vlist

oldmodel = XXXX
oldymdh  = 9999999999

'q dims'
xlin = sublin(result,2)
ylin = sublin(result,3)
xl1  = subwrd(xlin,6)
xl2  = subwrd(xlin,8)
yl1  = subwrd(xlin,6)
yl2  = subwrd(xlin,8)
xdiff = xl2 - xl1
ydiff = yl2 - yl1
if (xdiff > 50 & ydiff > 50)
  hurrsize = 0.30
else
  hurrsize = 0.30
endif

vct = 0

while (1)

  res = read(verffile)
  rc  = sublin(res,1)
  if(rc != 0)
    if(rc = 2)
      say 'End of verification file '
      say ' '
      break
    endif
    if(rc = 1); say 'rc=1: OPEN ERROR FOR 'verffile; endif
    if(rc = 8); say 'rc=8: 'verffile' OPEN FOR WRITE ONLY'; endif
    if(rc = 9); say 'rc=9: I/O ERROR FOR 'verffile; endif
    return 99
  endif

  rec = sublin(res,2)

  ymdh     = subwrd(rec,1)
  model    = subwrd(rec,2)
  fhr      = subwrd(rec,3)
  newlat   = subwrd(rec,4)
  newlon   = subwrd(rec,5)
  _atcfnum = subwrd(rec,6)
*  _stname  = subwrd(rec,7)  * This is now read in in rddates

  newlon = 360.0 - newlon

  if (oldymdh < 9999999999)

    'query ll2xy 'lastlon' 'lastlat
    xdum  = sublin(result,1)
    xlast = subwrd(xdum,1)
    ylast = subwrd(xdum,2)

    'query ll2xy 'newlon' 'newlat
    xdum = sublin(result,1)
    xnew = subwrd(xdum,1)
    ynew = subwrd(xdum,2)

    oldbound = in
    newbound = in
    if (xlast > _xright | xlast < _xleft | ylast > _yhi | ylast < _ylo)
      oldbound = out
    endif
    if (xnew > _xright | xnew < _xleft | ynew > _yhi | ynew < _ylo)
      newbound = out
    endif

*   To not draw lines between verif points, comment out the
*   draw line statement 3 lines down...

    if (oldbound = in | newbound = in)
      'set line '_verifcol' 1 4'
      'draw line 'xlast' 'ylast' 'xnew' 'ynew

      check12 = math_fmod(fhr,12)
      if (check12 = 0)
        'set line 1 1 4'
        'draw wxsym 40 'xnew' 'ynew' 'hurrsize' '_verifcol' 4'
      endif

    endif

  else

    'query ll2xy 'newlon' 'newlat
    xdum = sublin(result,1)
    xnew = subwrd(xdum,1)
    ynew = subwrd(xdum,2)

    vct = vct + 1
    if (vct = 1)
      _vtitleymdh = ymdh
    endif

    if (xnew <= _xright & xnew >= _xleft & ynew <= _yhi & ynew >= _ylo)
      'set line 1 1 4'
      'draw wxsym 40 'xnew' 'ynew' 'hurrsize' '_verifcol' 4'
    endif

  endif

  oldymdh = ymdh
  lastlon  = newlon
  lastlat  = newlat

endwhile

rc = close(verffile)

return

*---------------------*
*
*---------------------*
function initmenu()

modelwid=0.50
modelhgt=0.25

'set dropmenu 1 91 90 92 6 91 90 92 1 91 90 92 90 92 6'

'draw dropmenu 1 0.75 8.40 0.85 0.25 Main | '_mainstuff
'draw dropmenu 2 1.75 8.40 0.85 0.25 Storms | '_stormlist
'draw dropmenu 3 2.75 8.40 0.85 0.25 Dates | No storm picked yet'
'draw dropmenu 4 3.75 8.40 0.85 0.25 Track | '_trkstuff
'draw dropmenu 5 4.75 8.40 0.85 0.25 Opts_1 |Track forecasts |Intensity forecasts(N/A) |Plot forecasts? >19>|Plot observed? >20>|'
'draw dropmenu 7 5.75 8.40 0.85 0.25 Opts_2 |Model colors >21>|Observed color >22>|Model legend >24>|Map Color >22>|Grid Options >27>|Draw Signature? >19>|'

'draw dropmenu 15 cascade Every 6h |Every 12h |Every 24h |None '
'draw dropmenu 16 cascade Every 6h |Every 12h |Every 24h '
'draw dropmenu 17 cascade Every 6h |Every 12h |Every 24h '
'draw dropmenu 18 cascade Next 72h |Next 84h |Next 96h |Next 120h |Next 144h |Next 168h |Previous 72h |Previous 96h |Previous 120h |Previous 144h |Previous 168h |Beginning to now |Only now |Remainder of storm |Entire storm '
'draw dropmenu 19 cascade Yes |No '
'draw dropmenu 20 cascade Yes |No '
'draw dropmenu 21 cascade Rainbow | Monochrome >23>|'
'draw dropmenu 22 cascade black |white |red |green |blue |cyan |magenta |yellow |orange |purple |yellow/green |medium blue |dark yellow |aqua |dark purple |grey '
'draw dropmenu 23 cascade Douglas Fir |Steel |Ketchup |Mustard |Grape |Water |Bubble Gum |Charcoal '
'draw dropmenu 24 cascade Upper right |Lower right |Lower left |Upper left |None '
'draw dropmenu 25 cascade 72 |78 |84 |96 |120 |126 |144 |168 |All available '
'draw dropmenu 27 cascade Dashed Lines |Solid Lines |No Lines |- - GRID COLORS - - |black |white |red |green |blue |cyan |magenta |yellow |orange |purple |yellow/green |medium blue |dark yellow |aqua |dark purple |grey '

* 'set button  0 15 10  3  0  3 10  3 6'
***'set button  0  3 10  3  0 15 10  3 6'
'set button 1 40 41 42 5 40 42 41 6'

'draw button 101  0.30 7.60 'modelwid' 'modelhgt'  GFDL'
'draw button 102  0.85 7.60 'modelwid' 'modelhgt'  GFDT'
'draw button 103  0.30 7.30 'modelwid' 'modelhgt'  AEMN'
'draw button 104  0.85 7.30 'modelwid' 'modelhgt'  AVNO'
'draw button 105  0.30 7.00 'modelwid' 'modelhgt'   EMX'
'draw button 106  0.85 7.00 'modelwid' 'modelhgt'   UKX'
'draw button 107  0.30 6.70 'modelwid' 'modelhgt'  NGPS'
'draw button 108  0.85 6.70 'modelwid' 'modelhgt'   NGX'
'draw button 109  0.30 6.40 'modelwid' 'modelhgt'  LBAR'
'draw button 110  0.85 6.40 'modelwid' 'modelhgt'  BAMD'
'draw button 111  0.30 6.10 'modelwid' 'modelhgt'   NAM'
'draw button 112  0.85 6.10 'modelwid' 'modelhgt'  BAMS'
'draw button 113  0.30 5.80 'modelwid' 'modelhgt'  OFCL'
'draw button 114  0.85 5.80 'modelwid' 'modelhgt'  DSHP'
'draw button 115  0.30 5.50 'modelwid' 'modelhgt'  CLP5'
'draw button 116  0.85 5.50 'modelwid' 'modelhgt'  SHFR'
'draw button 117  0.30 5.20 'modelwid' 'modelhgt'  SHIP'
'draw button 118  0.85 5.20 'modelwid' 'modelhgt'  OHPC'
'draw button 119  0.30 4.90 'modelwid' 'modelhgt'  SHF5'
'draw button 120  0.85 4.90 'modelwid' 'modelhgt'  GUNS'
'draw button 121  0.30 4.60 'modelwid' 'modelhgt'  PARA'
'draw button 122  0.85 4.60 'modelwid' 'modelhgt'  APTS'
'draw button 123  0.30 4.30 'modelwid' 'modelhgt'  EEMN'
'draw button 124  0.85 4.30 'modelwid' 'modelhgt'  GFDN'
'draw button 125  0.30 4.00 'modelwid' 'modelhgt'   CMC'
'draw button 126  0.85 4.00 'modelwid' 'modelhgt'   UKM'

*--------

'set button 1 40 41 42 1 40 42 41 6'
'draw button 201  0.55 3.55 0.75 0.375 PLOT'
'draw button 202  0.55 3.15 1.05 0.250 ZOOM --->'
'draw button 203  0.55 2.85 1.05 0.250 ZOOM IN 5'
'draw button 204  0.55 2.55 1.05 0.250 ZOOM OUT 5'
'draw button 205  0.55 2.25 1.05 0.250 ORIGINAL'

*'set button  0  2 13 14  0  2 13 14 6'
'set button 1 50 51 52 1 50 52 51 6'
'draw button 98  0.30 0.15 'modelwid' 'modelhgt' EXIT'
'draw button 99  0.85 0.15 'modelwid' 'modelhgt' QUIT'

return

*---------------------*
*
*---------------------*
function redomenu(preserve)

modelwid=0.50
modelhgt=0.25

'clear dropmenu  1'
'clear dropmenu  2'
'clear dropmenu  3'
'clear dropmenu  4'
'clear dropmenu  5'
'clear dropmenu  7'
'clear dropmenu 15'
'clear dropmenu 16'
'clear dropmenu 17'
'clear dropmenu 18'
'clear dropmenu 19'
'clear dropmenu 20'
'clear dropmenu 21'
'clear dropmenu 22'
'clear dropmenu 23'
'clear dropmenu 24'
'clear dropmenu 25'
'clear dropmenu 27'

if (_freshcascade != yes)
  ymdix  = 34
  ymdmax = ymdix + _ymdct
  while (ymdix < ymdmax)
    ymdix = ymdix + 1
    'clear dropmenu 'ymdix
  endwhile
endif

'set dropmenu 1 91 90 92 6 91 90 92 1 91 90 92 90 92 6'

'draw dropmenu 1 0.75 8.40 0.85 0.25 Main | '_mainstuff
'draw dropmenu 2 1.75 8.40 0.85 0.25 Storms | '_stormlist
'draw dropmenu 3 2.75 8.40 0.85 0.25 Dates | '_ymdlist
'draw dropmenu 4 3.75 8.40 0.85 0.25 Track | '_trkstuff
'draw dropmenu 5 4.75 8.40 0.85 0.25 Opts_1 |Track forecasts |Intensity forecasts(N/A) |Plot forecasts? >19>|Plot observed? >20>|'
'draw dropmenu 7 5.75 8.40 0.85 0.25 Opts_2 |Model colors >21>|Observed color >22>|Model legend >24>|Map Color >22>|Grid Options >27>|Draw Signature? >19>|'

'draw dropmenu 15 cascade Every 6h |Every 12h |Every 24h |None '
'draw dropmenu 16 cascade Every 6h |Every 12h |Every 24h '
'draw dropmenu 17 cascade Every 6h |Every 12h |Every 24h '
'draw dropmenu 18 cascade Next 72h |Next 84h |Next 96h |Next 120h |Next 144h |Next 168h |Previous 72h |Previous 96h |Previous 120h |Previous 144h |Previous 168h |Beginning to now |Only now |Remainder of storm |Entire storm '
'draw dropmenu 19 cascade Yes |No '
'draw dropmenu 20 cascade Yes |No '
'draw dropmenu 21 cascade Rainbow | Monochrome >23>|'
'draw dropmenu 22 cascade black |white |red |green |blue |cyan |magenta |yellow |orange |purple |yellow/green |medium blue |dark yellow |aqua |dark purple |grey '
'draw dropmenu 23 cascade Douglas Fir |Steel |Ketchup |Mustard |Grape |Water |Bubble Gum |Charcoal '
'draw dropmenu 24 cascade Upper right |Lower right |Lower left |Upper left |None '
'draw dropmenu 25 cascade 72 |78 |84 |96 |120 |126 |144 |168 |All available '
'draw dropmenu 27 cascade Dashed Lines |Solid Lines |No Lines |- - GRID COLORS - - |black |white |red |green |blue |cyan |magenta |yellow |orange |purple |yellow/green |medium blue |dark yellow |aqua |dark purple |grey '

ymdix  = 34
ymdmax = ymdix + _ymdct
while (ymdix < ymdmax)
  ymdix = ymdix + 1
  'draw dropmenu 'ymdix' cascade '_hhstring.ymdix
endwhile 
_freshcascade = no

*'set button  0 15 10  3  0  3 10  3 6'
***'set button  0  3 10  3  0 15 10  3 6'
'set button 1 40 41 42 5 40 42 41 6'

'clear button 101'
'clear button 102'
'clear button 103'
'clear button 104'
'clear button 105'
'clear button 106'
'clear button 107'
'clear button 108'
'clear button 109'
'clear button 110'
'clear button 111'
'clear button 112'
'clear button 113'
'clear button 114'
'clear button 115'
'clear button 116'
'clear button 117'
'clear button 118'
'clear button 119'
'clear button 120'
'clear button 121'
'clear button 122'
'clear button 123'
'clear button 124'
'clear button 125'
'clear button 126'
'clear button 127'
'clear button 128'
'clear button 129'
'clear button 130'
'clear button 131'
'clear button 132'
'clear button 133'
'clear button 134'
'clear button 135'
'clear button 136'
'draw button 101  0.30 7.60 'modelwid' 'modelhgt'  GFDL'
'draw button 102  0.85 7.60 'modelwid' 'modelhgt'  GFDT'
'draw button 103  0.30 7.30 'modelwid' 'modelhgt'  AEMN'
'draw button 104  0.85 7.30 'modelwid' 'modelhgt'  AVNO'
'draw button 105  0.30 7.00 'modelwid' 'modelhgt'   EMX'
'draw button 106  0.85 7.00 'modelwid' 'modelhgt'   UKX'
'draw button 107  0.30 6.70 'modelwid' 'modelhgt'  NGPS'
'draw button 108  0.85 6.70 'modelwid' 'modelhgt'   NGX'
'draw button 109  0.30 6.40 'modelwid' 'modelhgt'  LBAR'
'draw button 110  0.85 6.40 'modelwid' 'modelhgt'  BAMD'
'draw button 111  0.30 6.10 'modelwid' 'modelhgt'   NAM'
'draw button 112  0.85 6.10 'modelwid' 'modelhgt'  BAMS'
'draw button 113  0.30 5.80 'modelwid' 'modelhgt'  OFCL'
'draw button 114  0.85 5.80 'modelwid' 'modelhgt'  DSHP'
'draw button 115  0.30 5.50 'modelwid' 'modelhgt'  CLP5'
'draw button 116  0.85 5.50 'modelwid' 'modelhgt'  SHFR'
'draw button 117  0.30 5.20 'modelwid' 'modelhgt'  SHIP'
'draw button 118  0.85 5.20 'modelwid' 'modelhgt'  OHPC'
'draw button 119  0.30 4.90 'modelwid' 'modelhgt'  SHF5'
'draw button 120  0.85 4.90 'modelwid' 'modelhgt'  GUNS'
'draw button 121  0.30 4.60 'modelwid' 'modelhgt'  PARA'
'draw button 122  0.85 4.60 'modelwid' 'modelhgt'  APTS'
'draw button 123  0.30 4.30 'modelwid' 'modelhgt'  EEMN'
'draw button 124  0.85 4.30 'modelwid' 'modelhgt'  GFDN'
'draw button 125  0.30 4.00 'modelwid' 'modelhgt'   CMC'
'draw button 126  0.85 4.00 'modelwid' 'modelhgt'   UKM'

'draw button 129  0.30 1.50 'modelwid' 'modelhgt'  HWRF'
'draw button 131  0.30 1.20 'modelwid' 'modelhgt'  YCNT'
'draw button 132  0.85 1.20 'modelwid' 'modelhgt'  YCNV'
*---------------------------

if (preserve = yes)
  m = 101
  while (m < 137)
    if (_modstatus.m = 1)
      'redraw button 'm' 1'
    else
      'redraw button 'm' 0'
    endif
    m = m + 1
  endwhile
endif

'set button 1 40 41 42 1 40 42 41 6'
'draw button 201  0.55 3.55 0.75 0.375 PLOT'
'draw button 202  0.55 3.15 1.05 0.250 ZOOM --->'
'draw button 203  0.55 2.85 1.05 0.250 ZOOM IN 5'
'draw button 204  0.55 2.55 1.05 0.250 ZOOM OUT 5'
'draw button 205  0.55 2.25 1.05 0.250 ORIGINAL'

*'set button  0  2 13 14  0  2 13 14 6'
'set button 1 50 51 52 1 50 52 51 6'
'draw button 98  0.30 0.15 'modelwid' 'modelhgt' EXIT'
'draw button 99  0.85 0.15 'modelwid' 'modelhgt' QUIT'

return

*---------------------*
*
*---------------------*
function modelsoff()

* Redraw all the buttons to the off position/color and set
* the model status array to off as well.

m = 101
while (m < 137)
  'redraw button 'm' 1'
  _modstatus.m = 1
  m = m + 1
endwhile

return

*---------------------*
*
*---------------------*
function printmodstat()

* Diagnostic function: print status of models....

m = 101
while (m < 137)
  say 'm= 'm' model= '_modname.m' modstatus= '_modstatus.m
  m = m + 1
endwhile

return

*---------------------*
*
*---------------------*
function clickmodel(m)

* First check to see if the input model number is 122, which is
* for "APTS", which means to toggle all ensemble members either
* on or off.  Some of the members may have been turned on or
* off separately, so we need to do a wholesale change:  First,
* check to see what we have just changed the value of the
* "APTS" button to, and then we change all ensemble members to 
* be the same as it.

if (m = 122)
* Check status of "APTS" button and switch it....
  if (_modstatus.122 = 0)
*     setting to 1 (OFF) for 122 (APTS)
    _modstatus.122 = 1
  else
*     setting to 0 (ON) for 122 (APTS)
    _modstatus.122 = 0
  endif
* Now force all the ensemble members to match the APTS button
  itemp = 127
  while (itemp <= 136)
    if (_modstatus.122 = 0)
*     setting to 1 (OFF) for m = 'itemp
      _modstatus.itemp = 0
    else
*       setting to 0 (ON) for m = 'itemp
      _modstatus.itemp = 1
    endif 
    itemp = itemp + 1
  endwhile

  rc = redomenu(yes)
else
  if (_modstatus.m = 0)
*    say '!!! clickmodel, setting to 1 (OFF) for m = 'm
    _modstatus.m = 1
  else
*    say '!!! clickmodel, setting to 0 (ON) for m = 'm
    _modstatus.m = 0
  endif
endif

return

*---------------------*
*
*---------------------*
function mainbox()
  _mainstuff="Clear | Print B&W Postscript | Print Color Postscript (black bg) | Print Color Postscript (white bg) | Print GIF (black bg) | Print GIF (white bg) | Print color EPS (black bg) | Print color EPS (white bg) | Exit Script | - - - - - - - | EXIT GrADS "
  _mainfunc.1="clr"
  _mainfunc.2="printbw"
  _mainfunc.3="printclbbg"
  _mainfunc.4="printclwbg"
  _mainfunc.5="printgifblk"
  _mainfunc.6="printgifwht"
  _mainfunc.7="printepsblk"
  _mainfunc.8="printepswht"
  _mainfunc.9="return"
  _mainfunc.10="null"
  _mainfunc.11="quit"
return

*---------------------*
*
*---------------------*
function gettrkopts()

_trkstuff="- - FORECAST - - | Marker Interval >15>| Single Forecast | Multiple Forecast >16>| Forecast Length >25>|- - OBSERVED - - | Plotting Interval >17>| Observed Length >18>|"
_trkfunc.1 = "null"
_trkfunc.2 = "null"
_trkfunc.3 = "single_fcst"
_trkfunc.4 = "null"
_trkfunc.5 = "null"
_trkfunc.6 = "null"
_trkfunc.7 = "null"
_trkfunc.8 = "null"

return

*---------------------*
*
*---------------------*
function gettrkbtn(btnstr)

* This function parses the button (q pos) string
* returned from the user to see what input he 
* entered under the "Track" button.

topbtn = subwrd(btnstr,8)
subbtn = subwrd(btnstr,10)

if (topbtn = 2)
  if (subbtn = 1)
    _trkmrkrintrvl = "every6"
    _trkmrkrint    = 6
  endif
  if (subbtn = 2)
    _trkmrkrintrvl = "every12"
    _trkmrkrint    = 12
  endif
  if (subbtn = 3)
    _trkmrkrintrvl = "every24"
    _trkmrkrint    = 24
  endif
  if (subbtn = 4)
    _trkmrkrintrvl = "none"
    _trkmrkrint    = 0
  endif
endif

if (topbtn = 3)
  _trkfcsttype = "single"
endif

if (topbtn = 4)
  if (subbtn = 1)
    _trkfcsttype = "mult06"
  endif
  if (subbtn = 2)
    _trkfcsttype = "mult12"
  endif
  if (subbtn = 3)
    _trkfcsttype = "mult24"
  endif
endif

if (topbtn = 5)
  if (subbtn = 1)
    _fcstlen = 72
  endif
  if (subbtn = 2)
    _fcstlen = 78
  endif
  if (subbtn = 3)
    _fcstlen = 84
  endif
  if (subbtn = 4)
    _fcstlen = 96
  endif
  if (subbtn = 5)
    _fcstlen = 120
  endif
  if (subbtn = 6)
    _fcstlen = 126
  endif
  if (subbtn = 7)
    _fcstlen = 144
  endif
  if (subbtn = 8)
    _fcstlen = 168
  endif
  if (subbtn = 9)
    _fcstlen = 999
  endif
endif

if (topbtn = 7)
  if (subbtn = 1)
    _trkverfintrvl = "every6"
  endif
  if (subbtn = 2)
    _trkverfintrvl = "every12"
  endif
  if (subbtn = 3)
    _trkverfintrvl = "every24"
  endif
endif

if (topbtn = 8)
  if (subbtn = 1)
    _trkverflen = "next72"
  endif
  if (subbtn = 2)
    _trkverflen = "next84"
  endif
  if (subbtn = 3)
    _trkverflen = "next96"
  endif
  if (subbtn = 4)
    _trkverflen = "next120"
  endif
  if (subbtn = 5)
    _trkverflen = "next144"
  endif
  if (subbtn = 6)
    _trkverflen = "next168"
  endif
  if (subbtn = 7)
    _trkverflen = "prev72"
  endif
  if (subbtn = 8)
    _trkverflen = "prev96"
  endif
  if (subbtn = 9)
    _trkverflen = "prev120"
  endif
  if (subbtn = 10)
    _trkverflen = "prev144"
  endif
  if (subbtn = 11)
    _trkverflen = "prev168"
  endif
  if (subbtn = 12)
    _trkverflen = "begtonow"
  endif
  if (subbtn = 13)
    _trkverflen = "onlynow"
  endif
  if (subbtn = 14)
    _trkverflen = "remainder"
  endif
  if (subbtn = 15)
    _trkverflen = "entire"
  endif
endif

return

*-----------------------*
*
*-----------------------*
function getopts1(btnstr)

topbtn = subwrd(btnstr,8)
subbtn = subwrd(btnstr,10)

if (topbtn = 1)
  _plottype = track
endif
if (topbtn = 2)
*  _plottype = intensity
  'set dialog 1 0 6 14 6'
  'q dialog Intensity plots not available with this version.... | <Hit enter to continue>'
  say result
endif
if (topbtn = 3)
  if (subbtn = 1)
    _plotfcst = yes
  endif
  if (subbtn = 2)
    _plotfcst = no
  endif
endif
if (topbtn = 4)
  if (subbtn = 1)
    _plotverif = yes
  endif
  if (subbtn = 2)
    _plotverif = no
  endif
endif

return

*-----------------------*
*
*-----------------------*
function getopts2(btnstr)

btn8  = subwrd(btnstr,8)
btn9  = subwrd(btnstr,9)
btn10 = subwrd(btnstr,10)
btn11 = subwrd(btnstr,11)
btn12 = subwrd(btnstr,12)

if (btn8 = 1)
* looking under the model colors sub-menu....
  if (btn9 = 21)
    if (btn10 = 1)
      _modcoltype = rainbow
      return
    endif
    if (btn10 = 2)
      _modcoltype = mono
      _modmonocol = _trkcol.btn12
      return
    endif
  endif
endif

if (btn8 = 2)
* looking under the observed colors sub-menu....
  _verifcol = btn10 - 1
  return
endif

if (btn8 = 3)
* looking under the legend location sub-menu
  if (btn10 = 1)
    _legendloc = upright
    return
  endif
  if (btn10 = 2)
    _legendloc = lowright
    return
  endif
  if (btn10 = 3)
    _legendloc = lowleft
    return
  endif
  if (btn10 = 4)
    _legendloc = upleft
    return
  endif
  if (btn10 = 5)
    _legendloc = none
    return
  endif
endif

if (btn8 = 4)
* looking under the map colors sub-menu....
  _mapcol = btn10 - 1
  'set map '_mapcol' 1 4'
  return
endif

if (btn8 = 5)
* looking under the grid options sub-menu....
  if (btn10 = 1)
    _gridstyle = 5
    'set grid on 5 '_gridcol
    return
  endif
  if (btn10 = 2)
    _gridstyle = 1
    'set grid on 1 '_gridcol
    return
  endif
  if (btn10 = 3)
    _gridstyle = -999
    'set grid off'
    return
  endif
  if (btn10 = 4)
    return
  endif
  if (btn10 > 4)
    _gridcol = btn10 - 5
    if (_gridstyle > -999)
      'set grid on '_gridstyle' '_gridcol
      return
    endif
  endif
endif

if (btn8 = 6)
* looking under the "draw signature?" sub-menu....
  if (btn10 = 1)
    _drawsig = yes
  endif
  if (btn10 = 2)
    _drawsig = no
  endif
endif

return

*-----------------------*
*
*-----------------------*
function process_main(choice)

* say 'beginning of process_main, choice= 'choice

if (choice = "clr")
  'clear graphics'
  rc = redomenu()
endif
if (choice = "printbw" | choice = "printclbbg" | choice = "printclwbg" | choice = "printgifblk" | choice = "printgifwht" | choice = "printepsblk" | choice = "printepswht")
  _hardcopy = choice
  rc = output()
endif

return

*-----------------------*
*
*-----------------------*
function output()

say ' '
say ' OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO'
say ' '
say ' Output file is being written.  Please wait....'
say ' '

ifile = _netdir%'hur.grads.meta'
'enable print 'ifile
'print'
'disable print'

_outcount = _outcount + 1

if (_hardcopy = "printbw")
  ofile = _netdir%_atcfname%'.'%_startdate%'.'%_randnum%'.'%_outcount%'.ps'
  '*/usrx/local/grads/bin/gxps -i 'ifile' -o 'ofile
endif
if (_hardcopy = "printclwbg")
  ofile = _netdir%_atcfname%'.'%_startdate%'.'%_randnum%'.'%_outcount%'.ps'
  '*/usrx/local/grads/bin/gxps -c -i 'ifile' -o 'ofile
endif
if (_hardcopy = "printclbbg")
  ofile = _netdir%_atcfname%'.'%_startdate%'.'%_randnum%'.'%_outcount%'.ps'
  '*/usrx/local/grads/bin/gxps -c -r -i 'ifile' -o 'ofile
endif
if (_hardcopy = "printgifblk")
  ofile = _netdir%_atcfname%'.'%_startdate%'.'%_randnum%'.'%_outcount%'.gif'
* '!/usrx/local/grads/bin/gxgif.orig -i 'ifile' -x 833 -y 644 -o 'ofile
  'printim 'ofile' gif x833 y644'
endif
if (_hardcopy = "printgifwht")
  ofile = _netdir%_atcfname%'.'%_startdate%'.'%_randnum%'.'%_outcount%'.gif'
* '!/usrx/local/grads/bin/gxgif.orig -i 'ifile' -x 833 -y 644 -r -o 'ofile
  'printim 'ofile' gif x833 y644 white'
endif
if (_hardcopy = "printepswht")
  ofile = _netdir%_atcfname%'.'%_startdate%'.'%_randnum%'.'%_outcount%'.eps'
  '!/usrx/local/grads/bin/gxeps -c -i 'ifile' -o 'ofile
endif
if (_hardcopy = "printepsblk")
  ofile = _netdir%_atcfname%'.'%_startdate%'.'%_randnum%'.'%_outcount%'.eps'
  '!/usrx/local/grads/bin/gxeps -c -r -i 'ifile' -o 'ofile
endif

say ' '
say ' Output image has been written to the following file: '
say ' '
say ' 'ofile
say ' '
say ' OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO'
say ' OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO'
say ' '

return

*-----------------------*
*
*-----------------------*
function getrandom()

'!'_rundir'make_random.sh >'_netdir'random_num.txt'
randfile=_netdir%'random_num.txt'

res = read(randfile)
rc  = sublin(res,1)
if (rc != 0)
  if (rc = 2)
    say 'End of random file: 'randfile
    say ' '
  endif
  if(rc = 1); say 'rc=1: OPEN ERROR FOR 'randfile; endif
  if(rc = 8); say 'rc=8: 'randfile' OPEN FOR WRITE ONLY'; endif
  if(rc = 9); say 'rc=9: I/O ERROR FOR 'randfile; endif
  return 99
endif
irand = sublin(res,2)

return irand

*-----------------------*
*
*-----------------------*
function rdstorm()

* Read in the list of storms & dates from the input file.
* Put the storms and associated dates into a string that
* will be displayed in a dropmenu.

stormfile=_netdir%'slist'
* '!ls -1 aal* aep* >'stormfile
*'!ls -1 aal* >'stormfile
*'!ls -1 a'_basin'* >'stormfile

'!'_rundir'get_storms.sh '_basin' >'stormfile

nst = 0
_stormlist = ""
while (1)
  res = read(stormfile)
  rc  = sublin(res,1)
  if(rc != 0)
    if(rc = 2)
      say 'End of storms file: 'stormfile
      say nst' storms are available...'
      say ' '
      break
    endif
    if(rc = 1); say 'rc=1: OPEN ERROR FOR 'stormfile; endif
    if(rc = 8); say 'rc=8: 'stormfile' OPEN FOR WRITE ONLY'; endif
    if(rc = 9); say 'rc=9: I/O ERROR FOR 'stormfile; endif
    return 99
  endif
  nst = nst + 1
  _stormfile.nst = sublin(res,2)
  stormrec       = sublin(res,2)
  _atcfid.nst = substr(_stormfile.nst,2,8)
  stormname   = subwrd(stormrec,2)
  if (nst = 1)
    _stormlist = _stormlist%_atcfid.nst%" "%stormname%" "
  else
    _stormlist = _stormlist%"|"%_atcfid.nst%" "%stormname%" "
  endif
endwhile

rc = close(stormfile)

return

*-------------------------*
*
*-------------------------*
function rddates(item)

* Run a unix script to awk out the CARQ records, picking
* out only the ones for 35 kt winds and sorting out any
* duplicates.  Dump this out into a file that we will read
* for the dates and will also be used for the observed
* positions.

_atcfname = _atcfid.item
afile     = _stormfile.item
scriptarg = _rundir%afile

'!'_rundir'list_vdates.sh 'scriptarg' >'_netdir'vlist.'item
veriffile=_netdir%'vlist.'item

_ymdct = 0
hhct   = 0
ndt    = 0
_ymdlist = ""
prevymd = 99999999
while (1)

  res = read(veriffile)
  rc  = sublin(res,1)
  if(rc != 0)
    if(rc = 2)
      say 'End of verif file: 'veriffile
      say ndt' cases are available for this storm...'
      say ' '
      break
    endif
    if(rc = 1); say 'rc=1: OPEN ERROR FOR 'veriffile; endif
    if(rc = 8); say 'rc=8: 'veriffile' OPEN FOR WRITE ONLY'; endif
    if(rc = 9); say 'rc=9: I/O ERROR FOR 'veriffile; endif
    return 99
  endif

  ndt   = ndt + 1
  drec  = sublin(res,2)
  yyyymmddhh = subwrd(drec,1)
  ymd   = substr(yyyymmddhh,1,8)
  hh    = substr(yyyymmddhh,9,2)
  _stname = subwrd(drec,2)
  _titleyear = substr(yyyymmddhh,1,4)

  if (ymd = prevymd)
    _hhstring.ymdix = _hhstring.ymdix%"|"%hh%" "
    hhct = hhct + 1 
    _ymdh.ymdix.hhct = yyyymmddhh
  else
    hhct = 1
    _ymdct = _ymdct + 1
    ymdix = 34 + _ymdct
    ymdstring = ymd' >'ymdix'>'
    _hhstring.ymdix = hh%" "
    _ymdlist = _ymdlist%ymdstring%"|"
    _ymdh.ymdix.hhct = yyyymmddhh

*    if (ndt = 1)
*      _ymdlist = _ymdlist%ymdstring
*    else
*      _ymdlist = _ymdlist%"|"%ymdstring
*    endif

  endif

  prevymd = ymd

endwhile

rc = close(veriffile)

_zoom = no

return

*-------------------------*
*
*-------------------------*
function makeflist()

* This whole script will work (for track, anyway) by using as 
* its input a file called flist (for the forecasts) and a file 
* called vlist (for the observed).  We need to use a unix 
* script to create these files on the fly.  This function will
* invoke a unix script to create just the flist file for now.
* There are two arguments sent to the get_mods.sh script.  The
* first, noted by the "-t", is for the forecast type, which 
* can be "single", "mult06", "mult12" or "mult24".  The second
* is the starting date for the forecast.  For example, if you 
* pick "mult06" for your type, this script will plot the 
* forecasts for your storm every 6h beginning with the starting
* date you have chosen.

* _trkfcsttype = single

mcount = 0
m = 101
modstring=' '
while (m < 137)
  if (_modstatus.m = 0)
    charmod = _modname.m
    modstring = modstring' '_modname.m
    mcount = mcount + 1
*    say '+++ YES: makeflist, modstring= 'modstring
*  else
*    say '---  NO: makeflist, m= 'm
  endif
  m = m + 1
endwhile

say ' '
say 'In makeflist....'
say '_trkfcsttype= '_trkfcsttype
say '_startdate=   '_startdate
say '_atcfname=    '_atcfname
say 'modstring=    'modstring
say ' '

if (_startdate = 9999)
  'set dialog 1 0 6 14 6'
  'q dialog !!! ERROR: You have not selected a starting date yet. | <Hit enter to continue>'
  'redraw button 201 0'
  'redraw button 202 0'
  'redraw button 203 0'
  'redraw button 204 0'
  'redraw button 205 0'
  say result
  return 99
endif

if (mcount > 0)
  if (_trkfcsttype = single)
    '! '_rundir'get_mods.sh -t '_trkfcsttype' -d '_startdate' -f '_atcfname' -u '_userid' 'modstring' >'_netdir'flist'
    return 0
  else
    if (mcount > 1)
      'set dialog 1 0 6 14 6'
      'q dialog !!! ERROR: Only 1 model can be selected for the multiple-time forecast option. | <Hit enter to continue>'
      say result
      'redraw button 201 0'
      'redraw button 202 0'
      'redraw button 203 0'
      'redraw button 204 0'
      'redraw button 205 0'
      return 99
    else
      '! '_rundir'get_mods.sh -t '_trkfcsttype' -d '_startdate' -f '_atcfname' -u '_userid' 'modstring' >'_netdir'flist'
      return 0
    endif
  endif
else
  'set dialog 1 0 6 14 6'
  'q dialog !!! ERROR: You have not selected any models to plot | <Hit enter to continue>'
  say result
  'redraw button 201 0'
  'redraw button 202 0'
  'redraw button 203 0'
  'redraw button 204 0'
  'redraw button 205 0'
  rc = modelsoff()
  return 99
endif

return

*-------------------------*
*
*-------------------------*
function makevlist()

* This script will use your options to go and pick through
* the atcfunix file and get the "CARQ" observation records.

say ' '
say 'in makevlist....'
say '_trkverfintrvl= '_trkverfintrvl
say '_trkverflen= '_trkverflen
say '_atcfname= '_atcfname
say '_startdate= '_startdate

'! '_rundir'get_verif.sh -i '_trkverfintrvl' -t '_trkverflen' -f '_atcfname' -d '_startdate' -u '_userid' >'_netdir'vlist'

return

*-------------------------*
*
*-------------------------*
function modnames()

_modname.101 = GFDL
_modname.102 = GFDT
_modname.103 = AEMN
_modname.104 = AVNO
_modname.105 =  EMX
_modname.106 =  UKX
_modname.107 = NGPS
_modname.108 =  NGX
_modname.109 = LBAR
_modname.110 = BAMD

_modname.111 =  NAM
_modname.112 = BAMS
_modname.113 = OFCL
_modname.114 = DSHP
_modname.115 = CLP5
_modname.116 = SHFR
_modname.117 = SHIP
_modname.118 = OHPC
_modname.119 = SHF5
_modname.120 = GUNS

_modname.121 = PARA
_modname.122 = APTS
_modname.123 = EEMN
_modname.124 = GFDN
_modname.125 =  CMC 
_modname.126 =  UKM

_modname.129 = HWRF
_modname.131 = YCNT
_modname.132 = YCNV

return

*-------------------------*
*
*-------------------------*
function settrkcols()

* This function sets the colors that will be used
* for the various track lines.  The colors were 
* chosen so that they would appear distinct enough
* with either a black or white background, and 
* were limited to only 7 colors so as not to have 
* too many that would be confused with each other.

* 21 = dark green
* 22 = bright steel blue
* 23 = tomato red
* 24 = mustard yellow
* 25 = purple
* 26 = light blue
* 27 = pink
* 28 = grey

'set rgb 21   50 150   0'
'set rgb 22   10  70 225'
'set rgb 23  200  25   0'
'set rgb 24  225 180   0'
'set rgb 25  170  40 190'
'set rgb 26    0 170 170'
'set rgb 27  255  15 150'
'set rgb 28  140 140 140'

_trkcol.1 = 21
_trkcol.2 = 22
_trkcol.3 = 23
_trkcol.4 = 24
_trkcol.5 = 25
_trkcol.6 = 26
_trkcol.7 = 27
_trkcol.8 = 28

_numtrkcols = 8

return

*-------------------------*
*
*-------------------------*
function setbtncols()

* This function sets the colors that will be used for the 
* colors of the buttons.

* Gray for dropmenu
* 'set rgb 90 100 100 100'
* 'set rgb 91 150 150 150'
* 'set rgb 92 200 200 200'

* Blue for dropmenu
'set rgb 90   0   0 100'
'set rgb 91   0  50 150'
'set rgb 92   0 100 250'

* Gray for model buttons....
'set rgb 40 100 100 100'
'set rgb 41  50  50  50'
'set rgb 42 200 200 200'

* Red for quit buttons....
'set rgb 50 175   0   0'
'set rgb 51 100   0   0'
'set rgb 52 255   0   0'

return
*-------------------------*
*
*-------------------------*
function drawlegend(modelct,model,trackcolor,ymdh)

* This function draws the legend of the models on 
* the plot.  The variables "_yhi", "_ylo", "_xleft"
* and "_xright" come from the function "getgxinfo".

if (_trkfcsttype = single)
  modstring = modelct%' '%model
  xroffset = 0
else
  mmddhh = substr(ymdh,5,6)
  modstring = modelct%' '%mmddhh
  xroffset = 0.22
endif

if (_legendloc = upright | _legendloc = upleft)

  yleghi = _yhi - 0.1
  ydecr  = (modelct - 1) * 0.2
  ystart = yleghi - ydecr
  
  if (_legendloc = upright)
    if (modelct > 9)
      xstart = _xright - (0.80 + xroffset)
    else
      xstart = _xright - (0.70 + xroffset)
    endif
  else
    if (modelct <= 9)
      xstart = _xleft + 0.12
    else
      xstart = _xleft + 0.02
    endif
  endif

  'set string 'trackcolor' l 6'
  'set strsiz 0.11 0.11'
  'draw string 'xstart' 'ystart' 'modstring

endif

if (_legendloc = lowright | _legendloc = lowleft)

  yleglo = _ylo + 0.1
  yincr  = (modelct - 1) * 0.2
  ystart = yleglo + yincr
 
  if (_legendloc = lowright)
    if (modelct > 9)
      xstart = _xright - (0.80 + xroffset)
    else
      xstart = _xright - (0.70 + xroffset)
    endif
  else
    if (modelct <= 9)
      xstart = _xleft + 0.12
    else
      xstart = _xleft + 0.02
    endif
  endif

  'set string 'trackcolor' l 6'
  'set strsiz 0.11 0.11'
  'draw string 'xstart' 'ystart' 'modstring

endif

return

*-------------------------*
*
*-------------------------*
function drawtitle()

'q gxinfo'
xdum=sublin(result,3)
ydum=sublin(result,4)
xl = subwrd(xdum,4)
xr = subwrd(xdum,6)
ylo = subwrd(ydum,4)
yhi = subwrd(ydum,6)
xdiff = xr - xl
ydiff = yhi - ylo
hsiz = ((xdiff * 0.02) + (ydiff * 0.025)) / 2
vsiz = hsiz

hsiz = 0.13
vsiz = 0.13

xstart = (xdiff/2) + xl
ystart = yhi + 0.40

cstr = _titleyear%' Tropical Cyclone Tracks'
'set string 1 c 5'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' 'cstr
*'draw string 'xstart' 'ystart + 0.25 ' HWRF: Operational Coupled Hurricane WRF (NCO PROD)'

cstr = 'Storm: '_atcfnum' ('_stname')'
ystart = ystart - 0.25
'set string 1 c 5'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' 'cstr

hsiz = 0.12
vsiz = 0.12

ystart = ylo
if (_plotfcst = yes)
  ystart = ystart - 0.35
  if (_trkfcsttype = single)
    cstr = 'Forecasts: Beginning '_ftitleymdh
  else
    cstr = 'Forecasts: Beginning '_ftitleymdh' for '_ftitlemodel' model'
  endif
  'set string 1 c 5'
  'set strsiz 'hsiz' 'vsiz
  'draw string 'xstart' 'ystart' 'cstr
endif

if (_trkverfintrvl = every6)
  vhrint = 6
endif
if (_trkverfintrvl = every12)
  vhrint = 12
endif
if (_trkverfintrvl = every24)
  vhrint = 24
endif

if (_plotverif = yes)
  ystart = ystart - 0.25
  if (_plotfcst = no)
    ystart = ystart - 0.10
  endif
  cstr = 'Observed: Beginning '_vtitleymdh', every 'vhrint' hours'
  'set string 1 c 5'
  'set strsiz 'hsiz' 'vsiz
  'draw string 'xstart' 'ystart' 'cstr
endif

return

*-----------------------------
*
*-----------------------------
function plotsig()

* This function will plot your signature in the
* lower left corner of the frame.  If the signature
* is too small or too big, change the value of
* charparm.

charparm = 0.013

'q gxinfo'
xdum=sublin(result,3)
ydum=sublin(result,4)
xl = subwrd(xdum,4)
xr = subwrd(xdum,6)
yhi = subwrd(ydum,6)
xdiff = xr - xl
hsiz = xdiff * charparm
vsiz = hsiz + 0.02

xstart = 10.95
ystart = 0.05

*say 'plotsig, hsiz= 'hsiz' vsiz= 'vsiz
*hsiz= 0.08453926 vsiz= 0.10453926
hsiz = 0.09
vsiz = 0.09

cstr = 'NCEP Hurricane Forecast Project'
'set string 7 br 4 0'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' 'cstr

return
