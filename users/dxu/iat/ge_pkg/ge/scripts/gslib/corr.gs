function main(args)
dtg=subwrd(args,1)
*
*	demonstration of calculating the correlation coef between
*	two horizontal (lon,lat) grids
*
*	open a typical nwp file
*
if(dtg = '') ; 'quit' ; endif
'reinit'
fname='/d1/obs/fnmoc/dat/nogaps.25.'dtg'.ctl'
fn=ofile(fname)
if(fn = 0 ) ; return ; endif
*
*	set lat lon bounds
*
*	NOTE: the underscore denotes "global" script variables 
*	which do not have
*	to be passed to calling functions
*
_lat1=0
_lat2=60

_lon1=100
_lon2=180
*
*	set the dimension environment to 2-D lon,lat
*
'set lat '_lat1' '_lat2
'set lon '_lon1' '_lon2
'set lev 500'
'set t 1'
*
*	define script variables consisting of a string containing 
*	the GrADS "expression" (a grid)
*
*	in this case, compare the u comp at 850 with u at 200 mb
*
x1='u(lev=850)'
x2='u(lev=200)'
*
*	now try u at 500 between t=1 (analysis) and t=2 12 h forecast
*
*x1='u(t=1)'
*x2='u(t=2)'
*
*	calculate the unweighted and weighted corr coefficients
*
rhou=corru(x1,x2)
rhow=corrw(x1,x2)
*
*	contour plot of the fields; use q pos to continue after clicking
*	on the screen
*
'c'
'set gxout contour'
'set ccolor rainbow'
'set cint 5'
'd 'x1
'draw title field #1'
'q pos'
*
*	overlay second field
*
'set ccolor 1'
'set cint 5'
'd 'x2
'draw title field #2'
'q pos'
*
*	clear and look at a scatter plot
*
'c'
'set gxout scatter'
'd 'x1';'x2
*
*	put the coef in the scatter plot title, note the fancy use
*	of fonts and subscripts...
*
'draw title `3r`0`barea weighted`n = 'rhow' `3r`0`bunweighted`n = 'rhou  

return

function corrw(x1,x2)
*
*	calc correlation coefficient for 2-D horizontal fields using aave
*
*	for non horizontal fields use ave(ave...)
*
*	ASSUMES THE GRIDS HAVE THE SAME NUMBER OF DEFINED POINTS!!!!
*
*	based on formula on p.92 of Panofsky and Brier, 1968 "Some applications of
*	statistics to meteorology." Penn State Press
*
*
*	set dimension environment to 0-D (a point), as a consequence
*	all calcs are written to the string variable result
*
*'set x 1'
*'set y 1'
*
*	display (calc) the ave of field #1
*

'd aave('x1',lon='_lon1',lon='_lon2',lat='_lat1',lat='_lat2')'
ave1=subwrd(result,4)
*
*	field #2
*
'd aave('x2',lon='_lon1',lon='_lon2',lat='_lat1',lat='_lat2')'
ave2=subwrd(result,4)
*
*	cross product #1 * #2
*
'd aave('x1'*'x2',lon='_lon1',lon='_lon2',lat='_lat1',lat='_lat2')'
ave12=subwrd(result,4)
*
*	ave of the sqr  #1
*
'd aave('x1'*'x1',lon='_lon1',lon='_lon2',lat='_lat1',lat='_lat2')'
ave1s=subwrd(result,4)
*
*	ave of sqr of #2
'd aave('x2'*'x2',lon='_lon1',lon='_lon2',lat='_lat1',lat='_lat2')'
ave2s=subwrd(result,4)
*
*	the first part of the denominator
*
'd sqrt('ave1s'-'ave1'*'ave1')'
den1=subwrd(result,4)
*
*	the second part
*
'd sqrt('ave2s'-'ave2'*'ave2')'
den2=subwrd(result,4)
*
*	the numerator
*
'd 'ave12'-'ave1'*'ave2
num=subwrd(result,4)
*
*	the coefficient
*
'd 'num'/('den1'*'den2')'
corr=subwrd(result,4)
*
*	output to the terminal
*
say ' The area weighted correlation = 'corr
*
*	return the string to the main program
*
return(corr)


function corru(x1,x2)
*
*	use the new version of gxout stat to get the # of valid points 
*	(for info purposes only)
*	and the biased (/N) vice the unbiased (/(N-1)) averages
*
*	ASSUMES THE GRIDS HAVE THE SAME NUMBER OF DEFINED POINTS!!!!
*
*	imax is the number of lines produced by gxout stat so the loop
*	doesn't go forever...
*
*	based on formula on p.92 of Panofsky and Brier, 1968 "Some applications of
*	statistics to meteorology." Penn State Press
*

imax=12
'set gxout stat'
'd 'x1
result1=result
'd 'x2
result2=result
'd 'x1'*'x2
result3=result
*
*	parse the strings for the info
*
i=1
while(i<=imax)

  card1=sublin(result1,i)
  card2=sublin(result2,i)
  card3=sublin(result3,i)

*
*	get number of defined points
*

  if(subwrd(card1,2)='count')
    npt=subwrd(card1,8)
  endif
*
*	the sum and sumsqr / n 
*
  if(subwrd(card1,1)='Stats(sum,sumsqr)/n:')

    ave1b=subwrd(card1,2)
    avesq1b=subwrd(card1,3)

    ave2b=subwrd(card2,2)
    avesq2b=subwrd(card2,3)

    ave12b=subwrd(card3,2)
    avesq12b=subwrd(card3,3)
*
*	break the loop now that we have what we need
*
    break
  endif
  i=i+1
endwhile
*
*	turn off stat output and do the calculation using display
*
*	the result goes to the script variable result
*
'set gxout contour'
'd 'ave12b'-'ave1b'*'ave2b
num=subwrd(result,4)
'd sqrt('avesq1b'-'ave1b'*'ave1b')'
den1=subwrd(result,4)
'd sqrt('avesq2b'-'ave2b'*'ave2b')'
den2=subwrd(result,4)

'd 'num'/('den1'*'den2')'
corr=subwrd(result,4)

say ' The UNWEIGHTED correlation = 'corr
*
*	return to main program
*
return(corr)

*
*-------------------------- ofile ------------------
*
function ofile (fname)
  'query files'
  i = 0
  while (1)  
    if (subwrd(result,1)='No')       
      ret = 0
      break;
    endif
    rec = sublin(result,i*3+2)
    if (rec='') 
      ret = 0;
      break; 
    endif
    if (subwrd(rec,2)=fname)
      rec = sublin(result,i*3+1)
      ret = subwrd(rec,2)
      break;
    endif
    i = i + 1
  endwhile
  if (ret=0) 
    'open 'fname
    if (rc>0) 
      say "Error opening "fname
      return (0)
    endif
    rec = sublin(result,2)
    ret = subwrd(rec,8)
  endif
  return (ret)

