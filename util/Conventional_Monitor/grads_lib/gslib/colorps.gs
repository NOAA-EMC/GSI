*
* script to make a color postscript file
* and print it on ph200i
'enable print /nfstmp/wx51we/meta.tmp'
'print'
'disable print'
'! gxeps -rc -i /nfstmp/wx51we/meta.tmp -o /nfstmp/wx51we/meta.ps.1'
'! sed ''s/^.*black background.*$/%/'' </nfstmp/wx51we/meta.ps.1 >/nfstmp/wx51we/meta.ps'
'! rm /nfstmp/wx51we/meta.ps.1'
'! rm /nfstmp/wx51we/meta.tmp'
