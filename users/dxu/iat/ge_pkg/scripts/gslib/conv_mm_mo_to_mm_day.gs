function main(args)
'reinit'
nd.1=31
nd.2=28
nd.3=31
nd.4=30
nd.5=31
nd.6=30
nd.7=31
nd.8=31
nd.9=30
nd.10=31
nd.11=30
nd.12=31
'set fwrite msu.dat'
'set gxout fwrite'
'open msunew.ctl'
'set x 1 144'
'set y 1 72'
i=1
while(i<=168)
  'set t 'i
  'q time'
  say 'time = 'sublin(result,1)
  date=subwrd(result,3)
  year=substr(date,9,4)
  mcode=mod(i,12)
  if(mcode = 0);mcode=12;endif
  nday=nd.mcode
  if(mod(year,4)=0 & mcode = 2);nday=nday+1;endif  
  say 'mcode = 'mcode' nd = 'nday
  'd pr/'nday
  i=i+1
endwhile
'disable fwrite'
exit

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

