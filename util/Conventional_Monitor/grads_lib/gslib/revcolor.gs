*
* script to make a color postscript file
* and print it on ph200i
'enable print /tmp/meta.tmp'
'print'
'disable print'
'! gxps -rc -i /tmp/meta.tmp -o /tmp/meta.tmp.ps'
'!lpr -Pph200i -h /tmp/meta.tmp.ps'
