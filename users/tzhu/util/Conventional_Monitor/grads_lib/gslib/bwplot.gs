*
* script to make a color postscript file
* and print it on ph200i
'enable print /tmp/meta.tmp'
'print'
'disable print'
'! gxps -rc -i /tmp/meta.tmp -o /tmp/meta.tmp.psx'
'! /usr/local/lib/grads/postfix /tmp/meta.tmp.psx /tmp/meta.tmp.ps'
'! rm /tmp/meta.tmp.psx'
'!lpr -Plocalps -h /tmp/meta.tmp.ps'
