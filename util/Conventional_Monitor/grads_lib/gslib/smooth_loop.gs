*
*	GrADS script to loop at a finer temporal resolution
*	than the data 
*
*	GrADS 1.3.7 command to reinitialize GrADS
* 
'reinit'
*
*	open a data file
*
'open ../data/amip/monthly/amav79018812sf.ctl'
*
*	define the display area
*
'set lat 0 60'
'set lon -140 -20'
*
*	dt 		the fine time increment
*	dtngps		the time increment of the data
dt=1
dtngps=12
*
*	pvar is the field to display
*
pvar='slp'

*
*	tbeg 	beginning time
*	tend	ending time
*
tbeg=0
tend=24
*
*	initialize the current time to the beginning time
*
t=tbeg
*
*	i1 and i2 are the time indices to interpolation betwee
*
i1=0
*
*	X command for doing double buffer graphics
*
'set dbuff on'
*
*	loop until the ending time
*
while(t <= tend)
*
*	use the mod function to see if we need to
*	reset the time indices
* 

  itest=mod(t,dtngps)

  if(itest = 0)
    i1=i1+1
    i2=i1+1 
  endif
*
*	calculate the interpolation coefficients
*
  bfact=(t-dtngps*(i1-1))/dtngps
  afact=1.0-bfact

*  say ' t = 't' afact = 'afact' bfact = 'bfact' i1 = 'i1' i2 = 'i2
*
*	define the interpolated field
*    
  'define q='afact'*'pvar'(t='i1')+'bfact'*'pvar'(t='i2')'
*
*	display the data to the unseen buffer
* 
  'set cint 4'
  'd q'
*
*	swap buffers to display the plot on the screen
* 
  'swap'
*
*	bump the time by dt
*
  t=t+dt

endwhile
*
*	pause until the use hits c for clear or p for print or q for quit
*
rc=pause()
*
*	turn double buffering off
*
'set dbuff off'


function pause()
  while(1)
   pull keycmd
   if(keycmd='c')
     'c'
     break
   endif
   if(keycmd='p')
     'print'
     'c'
     break
   endif
   if(keycmd='q')
     'c'
     break
     'quit'
   endif
 endwhile
return

function mod(i0,inc)
  if(inc!=0)
    imod=int(i0/inc)
  else
    imod=int(i0/1)
  endif
  imod=i0-imod*inc
return(imod)

function int(i0)
  i=0
  while(i<12)
    i=i+1
    if(substr(i0,i,1)='.')
      i0=substr(i0,1,i-1)
      break
    endif
  endwhile
return(i0)
