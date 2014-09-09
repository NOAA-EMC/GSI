function main(args)
*
*	script to mimimc ave with data from four sources
*
*	a count grid is created which contains the number
*	of defined data points that went into the sum
*
*	define four grids with jan precip from gla amip run
*

def=subwrd(args,1)
if(def = '') 
  'reinit'
*
*	open amip 10-year monthly mean file
*
  'open amav79018812sf.ctl'
*
*	set the grid bounds to the entire grid to demo
*	gxout fwrite
*
  'set x 1 72'
  'set y 1 46'
*
*	define the jan precip grids
*
  'define a1=preacc(time=jan79)'
  'define a2=preacc(time=jan80)'
  'define a3=preacc(time=jan81)'
  'define a4=preacc(time=jan82)'
*
*	maskout (set to undefined) all points less the 1 mm/day
*	to simulate undefined points at different locations 
*
  'define a1=maskout(a1,a1-1)'
  'define a2=maskout(a2,a2-1)'
  'define a3=maskout(a3,a3-1)'
  'define a4=maskout(a4,a4-1)'
*
*	use GrADS ave function to calulate jan mean precip 79-80
*	this function uses a count array in calculating the mean
*	at each grid point
*
*	the results of our "hand coded" calculation should be
*	very close to the GrADS result 
*
  'define abarave=ave(preacc,time=jan79,time=jan82,1yr)'

endif

*
*	use the GrADS consts function to create
*	a grid with 1 where data are defined
*	and 0 where date are undefined
*

'define b1=const(a1,1)'
'define b1=const(b1,0,-u)'

'define b2=const(a2,1)'
'define b2=const(b2,0,-u)'
 
'define b3=const(a3,1)'
'define b3=const(b3,0,-u)'

'define b4=const(a4,1)'
'define b4=const(b4,0,-u)'

*
*	now set the data grids to 0 at undefined points
*
'define a1=const(a1,0,-u)'
'define a2=const(a2,0,-u)'
'define a3=const(a3,0,-u)'
'define a4=const(a4,0,-u)'

*
*	sum the fields
*

'define asum=a1+a2+a3+a4'

*
*	sum the count grids
*

'define bcnt=b1+b2+b3+b4'
*
*	form the average
*	dividing by zero gives an undefined point in GrADS
*
'define abar=asum/bcnt'
*
*	use grid fill to display the results
*
'set gxout grfill'
'd abarave'
'd abar'
*
*	user fwrite to write the ave field as
*	ieee floating point numbers
* 
'set gxout fwrite'
'd abar'
*
*	shell out and rename the file
*
'!mv grads.fwrite jan7982precip.gdat'
 

return