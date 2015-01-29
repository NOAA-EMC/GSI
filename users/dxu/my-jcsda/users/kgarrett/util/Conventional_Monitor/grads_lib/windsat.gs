'set vpage 0.0 11.0 0.0 8.5'
'set grads off'
'set gxout barb'
'enable print fig.meta'
'open satwind.ctl'

'set lon 180 360'
'set lat -65 65'

*'set lon 260 280' 
*'set lat 0 20' 

'set mproj off'
'set cthick 5'
'set digsize 0.10'

kpn = 1
kchk=1


'set ccolor 1' 
'd maskout(uo,po-850);vo'
'set ccolor 2'
'd maskout(maskout(uo,suf),po-850);vo' 
'set strsiz 0.15'
'set string 1 bl 5'
'draw string 1.0 8.0 The wind field (press >=850mb,black repre. filtered out)'
say 'do you want to continue (y/n)'

pull con
if (con = y)
'clear'
p=800 
p1=p-1
p2=p-50
kchk=1

while (kchk=1 & p >100)


'set ccolor 1'
'd maskout(maskout(uo,'p1'-po),po-'p2');vo'
'set ccolor 2'
'd maskout(maskout(maskout(uo,suf),'p1'-po),po-'p2');vo'
'set strsiz 0.15'
'set string 1 bl 5'
'draw string 1.0 8.0 The wind field ('p2'<=press <'p'mb ,black repre. filtered out by random)' 

say 'Do you want to continue ? (y/n)'
pull  con1
if  (con1 = n)
 kchk=0
else
 kchk=1
 'clear'
 if(p1 >=750 | p1 <500)
    p=p2
    p1=p-1
    p2=p-50
 else
      p=p2
      p1=p-1
      p2=p-100
 endif
endif
endwhile

endif
