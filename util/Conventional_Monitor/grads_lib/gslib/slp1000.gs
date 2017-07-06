xxx=physcon()

yyy=slpf1000()

function slpf1000()
'set lev '1000
'd exp(log(100000.) + '_grav' * z / ('_rgas' * t))/100.'
return

function physcon
_grav=9.81
_rgas=287.
say "Physical constants:"
say "gravity="_grav
say "rgas="_rgas
return

