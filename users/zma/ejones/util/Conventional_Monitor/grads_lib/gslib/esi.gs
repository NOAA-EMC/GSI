function main(args)

rc=startgs('sea ice editor')

'open plotice.ctl'
'q file'
card=sublin(result,5)
_xdim=subwrd(card,3)
_ydim=subwrd(card,6)
say 'grid dimension = '_xdim' '_ydim

_latbegn=50
_latendn=80

_latbegs=-80
_latends=-50

_lonbase=270
_hem='nps'

_qbutton=3
_hbutton=4

'set x 1 '_xdim
'set y 1 '_ydim
*
*	create a defined grid with the entire grid
*
'define ii=ice' 

izoom = 0

while(1)

  while(izoom = 0)
  
    'c'
    'set grads off'

    latbeg=_latbegn
    latend=_latendn

    if(_hem = 'sps')
      latbeg=_latbegs
      latend=_latends
    endif

    'set lat 'latbeg' 'latend
    'set lon 0 360'
    'set mproj '_hem
  
    lonbeg=_lonbase-180
    lonend=_lonbase+180
   
    if(lonend > 360) 
      lonend=lonend-360
      lonbeg=lonbeg-360
    endif

    'set mpvals 'lonbeg' 'lonend' 'latbeg' 'latend

    'set gxout fgrid'
    'set fgvals 0 0 1 4'
    'd ii'

    rc=dbutton(_qbutton,10,0.35,1.25,0.5,quit,3,0.25,5)

    if(_hem = 'nps') 
      rc=dbutton(_hbutton,1,0.35,1.25,0.5,SHEM,3,0.25,5)
    else
      rc=dbutton(_hbutton,1,0.35,1.25,0.5,NHEM,3,0.25,5)
    endif  

    izoom=pickrgn()
*
*	if zoom is 1 then zoom and edit ice by breaking
*	out of this zoom loop
*
    if(izoom = 1)
      break
    endif
*
*	switch to southern hemisphere
*
    if(izoom = 2 & _hem = 'nps')
       _hem = 'sps'
      izoom = 0
    endif
*
*	switch to northern hemisphere
*
    if (izoom = 2 & _hem = 'sps')
       _hem = 'nps'
       izoom = 0
    endif
*
*	if izoom = -999 then quit
*
    if(izoom = -999) 
      'c'
      rc=exitproc()
      say 'all done'
      return
    endif

  endwhile
*
*	now that we have zoomed; edit the ice
*
  rc=editice() 
  izoom=subwrd(rc,1)
  quitcode=subwrd(rc,2)
  if(quitcode = 1); break; endif

endwhile

return

function nint(i0)
  i0=i0+0.5
  i=0
  while(i<12)
    i=i+1
    if(substr(i0,i,1)='.')
      i0=substr(i0,1,i-1)
      break
    endif
  endwhile
return(i0)

function dbutton(bnum,xc,yc,dx,dy,string,scol,ssiz,sthk)
  'draw button 'bnum' 'xc' 'yc' 'dx' 'dy' dum'
  'set string 'scol' c 'sthk
  'set strsiz 'ssiz
  'draw string 'xc' 'yc' 'string
return

function ebutton(bnum,xc,yc,dx,dy)
  'set line 0 0 0'
  eps=0.05
  x1=xc-dx/2-eps
  x2=xc+dx/2+eps
  y1=yc-dy/2-eps
  y2=yc+dy/2+eps
  'draw recf 'x1' 'y1' 'x2' 'y2
return

function drawice

  'set grads off'
  'set gxout fgrid'
  'set fgvals 0 0 1 4'
  'd ii'

  'set gxout grid'
  'set ccolor 2'
  'set cthick 5'
  'set digsize 0.01'
  'd ii'

return

function pickrgn

  'q gxinfo'

  card=sublin(result,3)
  xg1=subwrd(card,4)
  xg2=subwrd(card,6)
  card=sublin(result,4)
  yg1=subwrd(card,4)
  yg2=subwrd(card,6)

*
*	point #1
*
  
ongrid=0
btype=0
while(ongrid != 1 )

  rc=chkmouse(xg1,xg2,yg1,yg2)
  ongrid=subwrd(rc,1)
  x=subwrd(rc,2)
  y=subwrd(rc,3)
  btype=subwrd(rc,4)
  bnum=subwrd(rc,5)
*
*	check if all done
*  
  if(bnum = _qbutton); return(-999) ; endif
*
*	check if switching hemispheres
*
  if(bnum = _hbutton); return(2) ; endif

*
*	check for rotation of the big grid
*
    if(btype = 3) 
    
      'q xy2w 'x' 'y 
      _lonbase=subwrd(result,3)
      if(_hem = 'sps') ; _lonbase=_lonbase-180 ; endif
      _lonbase= nint(_lonbase)
      if(_lonbase < 0) ; _lonbase=_lonbase+360 ; endif
      izoom = 0
      return(izoom)

    endif
*
*	lat lon check
*
  if(btype = 2)
    'q xy2w 'x' 'y
    lonb=subwrd(result,3)
    latb=subwrd(result,6)
    rc=drlatlon(5.5,8,latb,lonb)
    ongrid=0
  endif

endwhile   
*
*	picked point #1; erase buttons
*
    rc=ebutton(_qbutton,10,0.35,1.25,0.5)
    rc=ebutton(_hbutton,1,0.35,1.25,0.5)

    'q xy2w 'x' 'y
    lon1=subwrd(result,3)
    lat1=subwrd(result,6)

    lon1=nint(lon1)
    lat1=nint(lat1)

    'q w2xy 'lon1' 'lat1
    x=subwrd(result,3)
    y=subwrd(result,6)    

    'set line 2 1 10'
    'draw mark 1 'x' 'y' 0.2'
    
     rc=latlonlb(lat1,lon1)
     llat=subwrd(rc,1)
     llon=subwrd(rc,2)

    'set string 1 r 5'
    'set strsiz 0.15'
     xoffset = xg1-0.3
     yoffset = y+0.15
    'draw string 'xoffset' 'yoffset' 'llat

     yoffset = y-0.15
    'draw string 'xoffset' 'yoffset' 'llon  

*
*	point #2
*

ongrid=0
btype=0
while(ongrid != 1 )

  rc=chkmouse(xg1,xg2,yg1,yg2)
  ongrid=subwrd(rc,1)
  x=subwrd(rc,2)
  y=subwrd(rc,3)
  btype=subwrd(rc,4)
  bnum=subwrd(rc,5)
*
*	check if all done
*  
  if(bnum = _qbutton); return(-999) ; endif
*
*	check for rotation of the big grid
*
    if(btype = 3) 
    
      'q xy2w 'x' 'y 
      _lonbase=subwrd(result,3)
      _lonbase= nint(_lonbase)
      if(_lonbase < 0) ; _lonbase=_lonbase+360 ; endif
      izoom = 0
      return(izoom)

    endif
*
*	lat lon check
*
  if(btype = 2)
    'q xy2w 'x' 'y
    lonb=subwrd(result,3)
    latb=subwrd(result,6)
    rc=drlatlon(5.5,8,latb,lonb)
    ongrid=0
  endif

endwhile   


  'q xy2w 'x' 'y
  lon2=subwrd(result,3)
  lat2=subwrd(result,6)
  lon2=nint(lon2)
  lat2=nint(lat2)

  'q w2xy 'lon2' 'lat2
  x=subwrd(result,3)
  y=subwrd(result,6)    

  'draw mark 1 'x' 'y' 0.2'

   rc=latlonlb(lat2,lon2)
   llat=subwrd(rc,1)
   llon=subwrd(rc,2)

   xoffset = xg2+0.3
   yoffset = y+0.15

  'set string 1 l 5'
  'set strsiz 0.15'
  'draw string 'xoffset' 'yoffset' 'llat

   yoffset = y-0.15
  'draw string 'xoffset' 'yoffset' 'llon  

  rc=dbutton(1,0.5,0.35,1.0,0.5,zoom,2,0.2,8)
  rc=dbutton(2,2.0,0.35,1.5,0.5,unzoom,4,0.2,5)
*
*	longitude check crossing the date line/greenwhich
*
  if(lon1 < 0 & lon2 > 0 )
    lon1=360+lon1
  endif

  if(lon2 < 0 & lon1 > 0 )
    lon2=360+lon2
  endif 

*
*	put lat lon pairs in the right order and draw a box
*
  if( (lon1 >= lon2) & (_hem = 'nps') ) 
    temp=lon1
    lon1=lon2
    lon2=temp
  endif

  if( (lon1 >= lon2) & (_hem = 'sps') ) 
    temp=lon1
    lon1=lon2
    lon2=temp
  endif

  if( (lat1 > lat2) & (_hem = 'nps') )
    temp=lat1
    lat1=lat2
    lat2=temp
  endif

  if( (lat1 > lat2) & (_hem = 'sps') )
    temp=lat1
    lat1=lat2
    lat2=temp
  endif

  'q w2xy 'lon1' 'lat1
  xb1=subwrd(result,3)
  yb1=subwrd(result,6)

  'q w2xy 'lon1' 'lat2
  xb2=subwrd(result,3)
  yb2=subwrd(result,6)

  'q w2xy 'lon2' 'lat2
  xb3=subwrd(result,3)
  yb3=subwrd(result,6)

  'q w2xy 'lon2' 'lat1
  xb4=subwrd(result,3)
  yb4=subwrd(result,6)

  'set line 3 1 10'
  'draw line 'xb1' 'yb1' 'xb2' 'yb2
  'draw line 'xb2' 'yb2' 'xb3' 'yb3
  'draw line 'xb3' 'yb3' 'xb4' 'yb4
  'draw line 'xb4' 'yb4' 'xb1' 'yb1


  'q pos'
  bnum=subwrd(result,7)
  if(bnum = 2)
    izoom=0   
    return(izoom)
  else
    izoom=1
  endif    

*
*	clear and set region
*

  'c'
  _btlast=0

  'set grads off'
  'set mpvals 'lon1' 'lon2' 'lat1' 'lat2

  lon1=lon1-10
  lon2=lon2+10

   if(lon1<=0); lon1=lon1+360;endif
   if(lon2<=0); lon2=lon2+360;endif
   if(lon1 > lon2)
     lon1=0
     lon2=360
   endif

  'set lat 'lat1' 'lat2
  'set lon 'lon1' 'lon2

return(izoom)

function chkmouse(xg1,xg2,yg1,yg2)

  ongrid=0

  'q pos'
  x=subwrd(result,3)
  y=subwrd(result,4)
  btype=subwrd(result,5)
  bnum=subwrd(result,7)

  if( x >= xg1 & x <= xg2 & y >= yg1 & y <= yg2)
    ongrid=1
  else
    ongrid=0
  endif

return (ongrid' 'x' 'y' 'btype' 'bnum)

function drlatlon(x,y,lat,lon)

  'set line 0 0 10'
  lat=lat*10
  lon=lon*10
  lat=nint(lat)
  lon=nint(lon)

  lat=lat*0.1
  lon=lon*0.1

  dx=1.75
  dy=0.25
  x1=x-dx
  x2=x+dx
  y1=y-dy
  y2=y+dy

  'draw recf 'x1' 'y1' 'x2' 'y2

  'set line 1 1 10'
  'set strsiz 0.25'
  'set string 1 c 8'
   
   rc=latlonlb(lat,lon)
   llat=subwrd(rc,1)
   llon=subwrd(rc,2)

  'draw string  'x' 'y' 'llat' 'llon

return

function latlonlb(lat,lon)

   if(lat < 0)
     llat=-lat
     llat=llat'S'
   else
     llat=lat'N'
   endif

  if(lon < 0 & lon > -180.0 )
    llon=-lon
    llon=llon'W'
  endif
 
  if(lon <= -180.0) 
    llon=360+lon
    llon=llon'E'
  endif
  
  if(lon > 0 & lon <= 180.0)
    llon=lon'E'
  endif
  
  if(lon > 180.0) 
    llon=360-lon 
    llon=llon'W'
  endif

return (llat' 'llon)

function editice

  'q gxinfo'
  card=sublin(result,3)
  xg1=subwrd(card,4)
  xg2=subwrd(card,6)
  card=sublin(result,4)
  yg1=subwrd(card,4)
  yg2=subwrd(card,6)

  if( _btlast != 2 ) ; rc=drawice() ; endif

  rc=dbutton(1,1.0,0.35,1.50,0.5,unzoom,2,0.20,10)

  'q pos'

  x=subwrd(result,3)
  y=subwrd(result,4)
  btype=subwrd(result,5)
  bnum=subwrd(result,7)

  if( x >= xg1 & x <= xg2 & y >= yg1 & y <= yg2)
    ongrid=1
  else
    ongrid=0
  endif
 
  if(bnum = 3);quitcode=1;endif

  'q xy2gr 'x' 'y
   say 'xy2gr result = 'result

  igrid=subwrd(result,3)
  jgrid=subwrd(result,6)
  say 'igrid jgrid = 'igrid' 'jgrid
  if(igrid <= 0);igrid=igrid+_xdim;endif
  igrid=nint(igrid)
  jgrid=nint(jgrid)

  'q xy2w 'x' 'y
*
*	check which mouse button was clicked
*
  if(btype = '1' & ongrid = 1)
    'q defval ii 'igrid' 'jgrid
    dval=subwrd(result,3)
    if(dval = 0 ) 
      'set defval ii 'igrid' 'jgrid' 1'
      _btlast=1
    else
      _btlast=2
    endif
   endif

  if(btype = 2)
    'q xy2w 'x' 'y
    lon=subwrd(result,3)
    lat=subwrd(result,6)
    rc=drlatlon(5.5,8,lat,lon)
    _btlast=2
  endif

  if(btype = '3' & ongrid = 1)
    'q defval ii 'igrid' 'jgrid
    dval=subwrd(result,3)
    if(dval = 1)
      'set defval ii 'igrid' 'jgrid' 0'
      _btlast=3
    else
      _btlast=2
    endif
   endif
*
*	check which button was clicked
*
  if(bnum = 1); izoom = 0 ; endif 

return(izoom' 'quitcode)


function exitproc
*
*	exit processing
*
*	get the dtg
*
  dtg=gtim2dtg()
  say 'dtg = 'dtg
*
*	fwrite the grid
*
  'set gxout fwrite'
*
*	set the dimension environment to the entire grid
*
  'set x 1 '_xdim
  'set y 1 '_ydim

  'd ii'
*
*  	apend the dtg to the data file
*
  '!mv grads.fwrite seaice.'dtg'.gdat'

return

function gtim2dtg
*
*  convert current time to dtg character
*
  moname='JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC'
  'q time'
  t1=subwrd(result,3)
  cyr=substr(t1,11,2)
  nmo=substr(t1,6,3)
  cda=substr(t1,4,2)
  chr=substr(t1,1,2)
  i=1
  while (nmo!=subwrd(moname,i));i=i+1;endwhile
  imo=i
  cmo=substr(imo,1,2)
  if(imo<10);cmo='0'imo;endif

return (cyr%cmo%cda%chr)

function startgs(title)
  'reinit'
  'set strsiz 0.4 0.4'
  'set string 7 bc 12'
  'draw string 5.5 6 GrADS'
  'draw string 5.5 5 'title
  'set string 1 bc 12'
  'draw string 5.5 3.5 Click When Ready'
  'set string 1 l 1'
  'q bpos'
  'c'
return
