function fwrite(arg)
*
*
*
if (arg = '')
   say '  run fwrite.gs file expression'
   exit
else
   cmdline=arg
endif

file=subwrd(cmdline,1)
data=file '.dat'
ctl=file '.ctl'
exp=subwrd(cmdline,2)

if (exp = '')
   say '  run fwrite.gs file EXPRESSION'
   exit
endif

say 'output is in ' data ' and ' ctl
say 'evaluating ' exp

'query dim'
qdim = result  
say qdim

t=sublin(qdim,5)
if (subwrd(t,7) = 'to')
  say 'multi time'
else
  say 'single time'
endif

say t
exit  
proj='latlon'
latr='-90 90'
lonr='0 360'
maptype='lowres'
mpvals='off'

if (map = 'nps' | map = 'NPS')
  proj='nps'
  latr='20 90'
  lonr='-270 90'
endif
if (map = 'nps2' | map = 'NPS2')
  proj='nps'
  latr='20 90'
  lonr='-180 180'
endif
if (map = 'nps3' | map = 'NPS3')
  proj='nps'
  latr='20 90'
  lonr='-60 300'
endif
if (map = 'sps' | map = 'SPS')
  proj='sps'
  latr='-90 -20'
  lonr='-270 90'
endif
if (map = 'usa' | map = 'USA')
  maptype='mres'
  proj='latlon'
  latr='24 52'
  lonr='-127 -65'
endif
if (map = 'usa2' | map = 'USA2')
  maptype='mres'
  proj='nps'
  latr='15 80'
  lonr='-150 -45'
  mpvals='-125 -75 25 55'
endif
if (map = 'n_amer' | map = 'N_AMER')
  maptype='mres'
  proj='nps'
  latr='5 90'
  lonr='-270 90'
  mpvals='-135 -65 18 85'
endif
if (map = 's_amer' | map = 'S_AMER')
  maptype='mres'
  proj='latlon'
  latr='-60 20'
  lonr='-90 -30'
endif
if (map = 'africa' | map = 'AFRICA')
  maptype='mres'
  proj='latlon'
  latr='-40 50'
  lonr='-20 60'
endif
if (map = 'europe' | map = 'EUROPE')
  maptype='mres'
  proj='nps'
  latr='5 90'
  lonr='-180 180'
  mpvals='-10 50 30 75'
endif
if (map = 'euro2' | map = 'EURO2')
  maptype='mres'
  proj='nps'
  latr='5 90'
  lonr='-180 180'
  mpvals='-56 36 29 68'
endif
if (map = 'asia' | map = 'ASIA')
  maptype='mres'
  proj='latlon'
  latr='0 80'
  lonr='40 170'
endif
if (map = 'aust' | map = 'AUST' | map = 'oz')
  maptype='mres'
  proj='latlon'
  latr='-50 0'
  lonr='100 180'
endif
if (map = 'lola' | map = 'LOLA')
  proj='latlon'
endif
if (map = 'lola2' | map = 'LOLA2')
  proj='latlon'
  lonr='-180 180'
endif
if (map = 'c_pac' | map = 'C_PAC')
  proj='latlon'
  latr='-45 45'
  lonr='120 290'
endif
if (map = 'n_pac' | map = 'N_PAC')
  proj='nps'
  latr='0 90'
  lonr='90 270'
  mpvals='100 260 22 89'
endif
if (map = 'robinson' | map = 'ROBINSON')
  proj='robinson'
  latr='-90 90'
  lonr='-180 180'
endif
if (map = 'custom' | map = 'CUSTOM' )
* custom lon0 dlon lat0 dlat
  maptype='mres'
  proj='latlon'
  lon0=subwrd(cmdline,2)
  dlon=subwrd(cmdline,3)
  lat0=subwrd(cmdline,4)
  dlat=subwrd(cmdline,5)
  if (dlon <= 0)
     dlon=10
  endif
  if (dlat <= 0)
     dlat=10
  endif
  lon1=lon0 + dlon
  lat1=lat0 + dlat
  latr=lat0 ' ' lat1
  lonr=lon0 ' ' lon1
endif

'set mpdset ' maptype
'set mpvals ' mpvals

'set mproj ' proj
'set lat ' latr
'set lon ' lonr

