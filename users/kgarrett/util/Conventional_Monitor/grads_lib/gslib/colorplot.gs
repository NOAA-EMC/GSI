*
* script to make a color postscript file
* and print it on ph200i
'enable print /tmp/meta.tmp'
'print'
'disable print'
*'! gxps -rc -i /tmp/meta.tmp -o /tmp/meta.tmp.psx'
*'! /usr/local/lib/grads/postfix /tmp/meta.tmp.psx /tmp/meta.ps'
*'! rm /tmp/meta.tmp.psx'
* '!lpr -Pph200i -h /tmp/meta.ps'
* '!lpr -Pcactek -h /tmp/meta.ps'
'! gxeps -rc -i /tmp/meta.tmp -o /tmp/meta.ps.1'
'! sed ''s/^.*black background.*$/%/'' </tmp/meta.ps.1 >/tmp/meta.ps'
'! rm /tmp/meta.ps.1'
'! lpr -Pcolors -h /tmp/meta.ps'

