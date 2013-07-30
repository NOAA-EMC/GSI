'set lat -90 90'
'set lon 0 360'

'set t 1 4'
'define gfsges=tozneclm.1'
'define prxges=tozneclm.3'

'set t 2 5'
'define gfsanl=tozneclm.2'
'define prxanl=tozneclm.4'

'set t 1 4'
'define gfsinc=gfsanl(t+1)-gfsges'
'define prxinc=prxanl(t+1)-prxges'


'set t 1'
'define gfsges0=ave(gfsges,t=1,t=4)'
'define gfsanl0=ave(gfsanl,t=2,t=5)'
'define gfsinc0=ave(gfsinc,t=1,t=4)'

'define prxges0=ave(prxges,t=1,t=4)'
'define prxanl0=ave(prxanl,t=2,t=5)'
'define prxinc0=ave(prxinc,t=1,t=4)'

'set lon 180'
'define zgfsges0=ave(gfsges0,lon=0,lon=360)'
'define zgfsanl0=ave(gfsanl0,lon=0,lon=360)'
'define zgfsinc0=ave(gfsinc0,lon=0,lon=360)'

'define zprxges0=ave(prxges0,lon=0,lon=360)'
'define zprxanl0=ave(prxanl0,lon=0,lon=360)'
'define zprxinc0=ave(prxinc0,lon=0,lon=360)'

