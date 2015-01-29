function clhilo(args)
*
*	plot H/L using Steve Lords' algorithm
*
  var=subwrd(args,1)
  maxmin=subwrd(args,2)
  fmt=subwrd(args,3)
  cint=subwrd(args,4)
  rad=subwrd(args,5)

  if(maxmin = "maxmin" | maxmin = "MAXMIN") ; mm=0.0 ; endif
  if(maxmin = "max" | maxmin = "MAX") ; mm=1.0 ; endif
  if(maxmin = "min" | maxmin = "MIN") ; mm=-1.0 ; endif

  if(fmt='') ; fmt=i5; endif
  if(rad = ''); rad=500.0; endif

  if(cint = '-999' | cint = '') 
    'set gxout stat'
    'd 'var
    dum=sublin(result,9)
    say "dum="dum
    cint=subwrd(dum,7)
    say "cint set to "cint
    'set gxout contour'
  endif
 
  udfile='udf.clhilo.vals'
  '!rm 'udfile
  say "rad,cint="rad","cint
* 
*	call the udf to calc H/L
*
 'd clhilo('var','mm','rad','cint','fmt')'
*
*	plot the H/L
*
  while (1)
    res=read(udfile)
    rc=sublin(res,1)
    if (rc != 0)
      if(rc = 1); say "open error for file "udfile; return; endif
      if(rc = 2); say "end of file for file "udfile; break; endif
      if(rc = 9); say "I/O error for file "udfile; return; endif
    endif
    dum=sublin(res,2)
    maxormin=subwrd(dum,1)
    lon=subwrd(dum,2)
    lat=subwrd(dum,3)
    val=subwrd(dum,4)

    'q w2xy 'lon' 'lat
    x=subwrd(result,3)
    y=subwrd(result,6)
    if(maxormin = "max"); str="H"; 'set string 4 c 6'; endif
    if(maxormin = "min"); str="L"; 'set string 2 c 6'; endif
    'set font 5'
    'set strsiz 0.1'
    'draw string 'x' 'y' 'str
    'set font 0'
    'set strsiz 0.05'
    'set string 1 tc 6'
    ytop=y-0.1
    'draw string 'x' 'ytop' 'val
    
  endwhile
  rc=close(udfile)
  say "End of clhilo"

return
  