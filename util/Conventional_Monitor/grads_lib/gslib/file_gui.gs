*
*	file_gui.gs
*
*	simple gui for viewing a GrADS data file
*
*	usage:
*
*	run grads; set the window size and at the grads prompt
*		
*	   	run file_gui.gs fname
*
*	where fname is the name of the GrADS data descriptor file
*
*	example
*
*	 	run file_gui.gs nogaps.ctl
*
*	Preliminary version:	31 March 1991
*	
*	Developed by Brain Doty, UofMD
*	modified by Mike Fiorino, NASA GSFC
*
*

function main(args)

if(args='') 
 return
else
 'reinit'
 'open 'subwrd(args,1)
endif

*  Script to look at a GrADS data file 
  
  fnum = 1
  'set parea 2.0 9.0 2.0 7.5'

* Get info on the file 

  'q file 'fnum
  res = result
  dum = sublin(res,5)
  _znum = subwrd(dum,9)
  _tnum = subwrd(dum,12)
  xnum = subwrd(dum,3)
  ynum = subwrd(dum,6)
  dum = sublin(res,6)
  _vnum = subwrd(dum,5)
  'set x 1 'xnum
  'set y 1 'ynum
  'q dims'
  dum = sublin(result,2)
  _lnlo = subwrd(dum,6)
  _lnhi = subwrd(dum,8)
  dum = sublin(result,3)
  _ltlo = subwrd(dum,6)
  _lthi = subwrd(dum,8)

* Following assumes world covereage would start at 0 lon

  if (_lnlo = 0 & lnhi>350) 
    _lnhi = 360
  endif
  _ltd = (_lthi-_ltlo)/5
  _lnd = (_lnhi-_lnlo)/6
  _lnmin = _lnlo
  _lnmax = _lnhi
  _ltmin = _ltlo
  _ltmax = _lthi

* Get time limits in character form

  'set t '1
  _tc1 = mydate()
  'set t '_tnum
  _tc2 = mydate()

* Get world values for each level 

  i = 1
  _levs = ''
  while (i<=_znum) 
    'set z 'i
    _levs = _levs % ' ' % subwrd(result,4)
    i = i + 1
  endwhile

* Get each variable abbreviation

  i = 0
  _vars = ''
  while (i<_vnum) 
    dum = sublin(res,i+7)
    _vars = _vars % ' ' % subwrd(dum,1)
    j = i + 1
    _vname.j = subwrd(dum,4)' 'subwrd(dum,5)' 'subwrd(dum,6)' 'subwrd(dum,7)
    i = i + 1
  endwhile

* Set up the initial state and display the initial stuff

  _cvar = 1    
  _clev = 1
  _gxout = 'contour'
  _t1 = 1
  _t2 = 1
  _tclick = 2

  rc = dodisp ()
  rc = dobutn ()

  while (1) 
    rc = getbutn ()
    say 'rc getbutn = 'rc
    if (rc = 1)
      'clear'
      'quit'
    endif
    rc = dodisp()
    rc = dobutn ()
  endwhile

  return

*  function to display the current field 

function dodisp()
 
  if (_t1<_t2) 
    'set dbuff on'
    t = _t1
    while (t<=_t2) 
      'set t 't
      rc = disp()
      t = t + 1
      if (t>_t2)
        'set string 1 c 6'
        'set strsiz 0.2'
        'draw string 5.5 0.5 Click Mouse Button to Continue'
      endif
      'swap'
    endwhile
    'q pos'
    'set dbuff off'
  endif
  'set t '_t1
  rc = disp()
  _tclick = 2
return
 
function disp()
  'set gxout '_gxout
  'set lev 'subwrd(_levs,_clev)
  'set lon '_lnmin' '_lnmax
  'set lat '_ltmin' '_ltmax
  'set grads off'
  'd 'subwrd(_vars,_cvar)
  'draw title NMC Analysis (GRIB)'
  'set string 1 c 6'
  'set strsiz 0.2 0.22'
  'draw string 5.5 8.3 Prototype for NCAR/DSS CD-ROM'
  'set string 1 r 6'
  'set strsiz 0.15 0.17'
  'q gxinfo'
  dum = sublin(result,3)
  xlo = subwrd(dum,4)
  xhi = subwrd(dum,6)
  dum = sublin(result,4)
  ylo = subwrd(dum,4)
  yhi = subwrd(dum,6)
  'draw string 'xhi' '%(ylo-0.7)%' '%mydate()
  'set string 1 l 6'
  'draw string 'xlo' '%(ylo-0.7)%' '%_vname._cvar
  _tclick = 2

*  function to display the buttons

function dobutn ()

  rc = dolevs()
  rc = dovars()
  rc = dotime()
  rc = domisc()
  return

*  Function to display the time bar

function dotime()
  
  ylo = 2.5
  yhi = 6.5
  x = 10.0

  if(_tnum > 1)
    yd = 4.0/(_tnum-1)
  else
    yd=4.0
  endif
  'set line 0'
  'draw recf '%(x-0.5)%' '%(ylo-0.1)%' '%(x+0.5)%' '%(yhi+0.1)

  'set line 15 1 6'
  'draw line 'x' 'ylo' 'x' 'yhi

  i = 1
  while (i<=_tnum)
    y = ylo + (i-1)*yd
    if (i>=_t1 & i<=_t2) 
      'set line 1 1 12'
    else
      'set line 15 1 6'
    endif
    'draw line '%(x-0.03)%' 'y' '%(x+0.03)%' 'y
    i = i + 1
  endwhile
 
  'set string 1 c 3'
  'set strsiz 0.13 0.15'
  p1 = subwrd(_tc1,1)
  p2 = subwrd(_tc1,2) % ' ' % subwrd(_tc1,3)
  p3 = subwrd(_tc1,4)
  'draw string 'x' '%(ylo-0.2)%' 'p1
  'draw string 'x' '%(ylo-0.45)%' 'p2
  'draw string 'x' '%(ylo-0.7)%' 'p3
  p1 = subwrd(_tc2,1)
  p2 = subwrd(_tc2,2) % ' ' % subwrd(_tc2,3)
  p3 = subwrd(_tc2,4)
  'draw string 'x' '%(yhi+0.7)%' 'p1
  'draw string 'x' '%(yhi+0.45)%' 'p2
  'draw string 'x' '%(yhi+0.2)%' 'p3
    
return


*  function to display the levels buttons

function dolevs ()
  
  x = 0.8
  y = 4.5
  ylo = y - _znum*0.15
  yhi = y + _znum*0.15

  'set line 0'
  'draw recf '%(x-0.49)%' '%(ylo-0.1)%' '%(x+0.49)%' '%(yhi+0.1)

  i = 1
  'set line 15 1 6'
  'set string 15 c 6' 
  'set strsiz 0.15'
  while (i<=_znum) 
    if (i=_clev)
      'set string 1 c 6'
      'draw string 'x' '%(ylo+0.15)%' 'subwrd(_levs,i)
      'set string 15 c 6'
    else 
      'draw string 'x' '%(ylo+0.15)%' 'subwrd(_levs,i)
    endif
    'draw rec '%(x-0.4)%' 'ylo' '%(x+0.4)%' '%(ylo+0.3)
    i = i + 1
    ylo = ylo + 0.3
  endwhile
  return

function dovars ()
  
  x = 5.5
  y = 0.6
  xlo = x - _vnum*0.4
  xhi = x + _vnum*0.4

  'set line 0'
  'draw recf '%(xlo-0.1)%' '%(y-0.3)%' '%(xhi+0.1)%' '%(y+0.3)

  i = 1
  'set line 15 1 6'
  'set string 15 c 6' 
  'set strsiz 0.15'
  while (i<=_vnum) 
    if (i=_cvar)
      'set string 1 c 6'
      'draw string '%(xlo+0.4)%' 'y%' 'subwrd(_vars,i)
      'set string 15 c 6'
    else 
      'draw string '%(xlo+0.4)%' 'y%' 'subwrd(_vars,i)
    endif
    'draw rec 'xlo' '%(y-0.2)%' '%(xlo+0.8)%' '%(y+0.2)
    i = i + 1
    xlo = xlo + 0.8
  endwhile
  return

function domisc ()

*  do quit button

  'set line 15 1 6'
  'set string 1 c 6'
  'set strsiz 0.15'
  'draw rec 0.1 0.1 0.9 0.5'
  'draw string 0.5 0.3 QUIT'

* do display button

  'draw rec 9.1 0.1 10.9 1.1'
  'draw string 10.0 0.6 DISPLAY'

return

    
function getbutn()
    
  vlo = 5.5 - _vnum*0.4
  vhi = 5.5 + _vnum*0.4
  zlo = 4.5 - _znum*0.15
  zhi = 4.5 + _znum*0.15

  'q gxinfo'
  dum = sublin(result,3)
  xlo = subwrd(dum,4)
  xhi = subwrd(dum,6)
  dum = sublin(result,4)
  ylo = subwrd(dum,4)
  yhi = subwrd(dum,6)

  while (1) 
    'q pos'
    x = subwrd(result,3)
    y = subwrd(result,4)
  
*   Handle a click on the levs

    if (x>0.4 & x<1.2 & y>zlo & y<zhi) 
      _clev = int((y-zlo)/0.3)+1
      rc = dolevs()
    endif
  
*   Handle a click on the vars

    if (x>vlo & x<vhi & y>0.4 & y<0.8) 
      _cvar = int((x-vlo)/0.8)+1
      rc = dovars()
    endif

*   Handle quit button

    if (x>0.1 & x<0.9 & y>0.1 & y<0.5) 
      return(1)
    endif

*   Handle display button

    if (x>9.1 & x<10.9 & y>0.1 & y<1.1) 
      break;
    endif

*   Handle click on the map (zoom)
    
    if (x>xlo & x<xhi & y>ylo & y<yhi) 
      'q xy2w 'x' 'y
      lon = subwrd(result,3)
      lat = subwrd(result,6)
      _lnmin = lon-_lnd
      _lnmax = lon+_lnd
      _ltmin = lat-_ltd
      _ltmax = lat+_ltd
      if (_ltmin<_ltlo) 
        _ltmin = _ltlo
        _ltmax = _ltlo + _ltd*2
      endif
      if (_ltmax>_lthi) 
        _ltmax = _lthi
        _ltmin = _lthi - _ltd*2
      endif
      break
    endif

*   Handle time click(s)

    if (x>9.5 & x<10.5 & y>2.2 & y<6.8 )
      if(_tnum > 1) 
        td = 4.0/(_tnum-1)
      else
        td=4.0
      endif
      tim = (y-2.5)/td
      tim = int(tim+1.5)
      if (tim<1); tim = 1; endif;
      if (tim>_tnum); tim = _tnum; endif;
  
      if (_tclick=1) 
        _tclick = 2
        if (_t1<tim) 
          _t2 = tim
        else
          _t1 = tim
        endif
      else
        _tclick = 1
        _t1 = tim
        _t2 = tim
      endif
      rc = dotime()
    endif 
       
  endwhile

return(0)
      
* int function

function int(stuff)

  res = ''
  i = 1
  c = substr(stuff,i,1)
  while (c!='' & ('x'%c)!='x.') 
    res = res%c
    i = i + 1
    c = substr(stuff,i,1)
  endwhile
  return res

*  Function to reformat the GrADS date/time into something
*  more readable
 
function mydate
  'query time'
  sres = subwrd(result,3)
  i = 1
  while (substr(sres,i,1)!='Z')
    i = i + 1
  endwhile
  hour = substr(sres,1,i)
  isav = i
  i = i + 1
  while (substr(sres,i,1)>='0' & substr(sres,i,1)<='9')
    i = i + 1
  endwhile
  day = substr(sres,isav+1,i-isav-1)
  month = substr(sres,i,3)
  year = substr(sres,i+3,4)
  return (hour' 'month' 'day', 'year)
