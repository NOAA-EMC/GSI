*
* script to make a color postscript file
* and print it on ph200i
'enable print /tmp/meta.tmp'
'print'
'disable print'
'! gxeps -rc -i /tmp/meta.tmp -o /tmp/meta.ps.1'
'! sed ''s/^.*black background.*$/%/'' </tmp/meta.ps.1 >/tmp/meta.ps'
'! rm /tmp/meta.ps.1'
* '!lpr -Plocalps -h -s /tmp/meta.ps'
