*
*  plot_fs_obsnum_comp.gs
*
*  Plots full spectrum (all channels) comparison
*  of observation and ges with bias correction 
*  between two or three data sources.
*
function plottime (args)

satype=subwrd(args,1)
data=subwrd(args,2)
exp1=subwrd(args,3)
exp2=subwrd(args,4)
exp3=subwrd(args,5)

'q file'
lin1=sublin(result,1)
satnam=subwrd(lin1,4)
satnum=subwrd(lin1,5)
nchan=subwrd(lin1,6)

*
* Set time and region
*
'set t 1'
'query time'
date1=subwrd(result,3)
say 'date1='date1

region=1
'set y 1'
'set z 1'


'!rm -f xsize.txt'
say 'data ='data
if (data = anl)
   '!cat ./'satype'_anl.ctl |grep "xdef" > xsize.txt'
   om=OmA
   ga=a

   'define avgbc1=omabc.1/count.1'
   'define sdvbc1='calcsdv(count.1,omabc.1,omabc2.1)
   'define avgnbc1=omanbc.1/count.1'
   'define sdvnbc1='calcsdv(count.1,omanbc.1,omanbc2.1)

   'define avgbc2=omabc.2/count.2'
   'define sdvbc2='calcsdv(count.2,omabc.2,omabc2.2)
   'define avgnbc2=omanbc.2/count.2'
   'define sdvnbc2='calcsdv(count.2,omanbc.2,omanbc2.2)

   if (exp3 !="")
      'define avgbc3=omabc.3/count.3'
      'define sdvbc3='calcsdv(count.3,omabc.3,omabc2.3)
      'define avgnbc3=omanbc.3/count.3'
      'define sdvnbc3='calcsdv(count.3,omanbc.3,omanbc2.3)
   endif

else
   '!cat ./'satype'.ctl |grep "xdef" > xsize.txt'
   om=OmF
   ga=g

   'define avgbc1=omgbc.1/count.1'
   'define sdvbc1='calcsdv(count.1,omgbc.1,omgbc2.1)
   'define avgnbc1=omgnbc.1/count.1'
   'define sdvnbc1='calcsdv(count.1,omgnbc.1,omgnbc2.1)

   'define avgbc2=omgbc.2/count.2'
   'define sdvbc2='calcsdv(count.2,omgbc.2,omgbc2.2)
   'define avgnbc2=omgnbc.2/count.2'
   'define sdvnbc2='calcsdv(count.2,omgnbc.2,omgnbc2.2)

   if (exp3 !="")
      'define avgbc3=omgbc.3/count.3'
      'define sdvbc3='calcsdv(count.3,omgbc.3,omgbc2.3)
      'define avgnbc3=omgnbc.3/count.3'
      'define sdvnbc3='calcsdv(count.3,omgnbc.3,omgnbc2.3)
   endif

endif

result=read(xsize.txt)
rc=sublin(result,1)
if (rc = 0)
   info=sublin(result,2)
   nx=subwrd(info,2)
   xs=subwrd(info,4)
   xe1=subwrd(info,5)
endif
result=close(xsize.txt)

xe=xs+xe1*nx


'clear'
'set grads off'
'set missconn on'
'set lon 'xs' 'xe
'set z 'region
'set mproj off'

'set string 1 l 5'
'set strsiz 0.11 0.11'
'set xlopts 1 4 0.11'
'set ylopts 1 2 0.09'

   'set parea 1.25 7.5 7.25 9.25'
   'set grads off'
   'set datawarn off'
   'set xlab on'
   'set ylpos 0 l'
 
   if (exp3 !="")
      'run ./setrange.gs 'count'.1 'count'.2 'count'.3 'count'.2' 
   else
      'run ./setrange.gs 'count'.1 'count'.2'
   endif

   'set ccolor 2'
   'set cmark 0'
   'd 'count'.1'
   'set ccolor 3'
   'set cmark 0'
   'd 'count'.2'
   if (exp3 !="")
      'set ccolor 4'
      'set cmark 0'
      'd 'count'.3'
   endif

   'set string 2'
   'set strsiz 0.13 0.13'
   'draw string 6.8 9.0 'exp1
   'set string 3'
   'draw string 6.8 8.8 'exp2
   if (exp3 !="")
      'set string 4'
      'draw string 6.8 8.6 'exp3
   endif
   'set strsiz 0.11 0.11'

   'draw title observation count'
   'draw xlab Channel'

   'set parea 1.25 7.5 4.00 6.00'
   'set grads off'
   'set datawarn off'
   'set xlab on'
   'set ylpos 0 l'

   'run ./setrange.gs 'avgbc1' 'avgbc2' 'sdvbc1' 'sdvbc2 
   'set ccolor 2'
   'set cstyle 1'
   'set cmark 0'
   'd 'avgbc1
   'set ccolor 3'
   'set cstyle 1'
   'set cmark 0'
   'd 'avgbc2
   if (exp3 !="")
      'set ccolor 4'
      'set cstyle 1'
      'set cmark 0'
      'd 'avgbc3
   endif

   'set ccolor 2'
   'set cstyle 3'
   'set cmark 0'
   'd 'sdvbc1
   'set ccolor 3'
   'set cstyle 3'
   'set cmark 0'
   'd 'sdvbc2
   if (exp3 !="")
      'set ccolor 4'
      'set cstyle 3'
      'set cmark 0'
      'd 'sdvbc3
   endif

   'set ccolor 1'
   'set cstyle 1'
   'set cmark 0'
   'd 'avgbc1'-'avgbc1

   'set line 1 1'
   'draw line 6.0 5.75 6.3 5.75'
   'set string 1'
   'draw string 6.5 5.75 avg om'ga'bc'
   'set line 1 3'
   'draw line 6.0 5.55 6.3 5.55'
   'set string 1'
   'draw string 6.5 5.55 sdv om'ga'bc'

   'draw title 'om' with BC'
   'draw xlab Channel'


   'set parea 1.25 7.5 .75 2.75'
   'set grads off'
   'set datawarn off'
   'set xlab on'
   'set ylpos 0 l'

   'run ./setrange.gs 'avgnbc1' 'avgnbc2' 'sdvnbc1' 'sdvnbc2
   'set ccolor 2'
   'set cstyle 1'
   'set cmark 0'
   'd 'avgnbc1
   'set ccolor 3'
   'set cstyle 1'
   'set cmark 0'
   'd 'avgnbc2
   if (exp3 !="")
      'set ccolor 4'
      'set cstyle 1'
      'set cmark 0'
      'd 'avgnbc3
   endif

   'set ccolor 2'
   'set cstyle 3'
   'set cmark 0'
   'd 'sdvnbc1
   'set ccolor 3'
   'set cstyle 3'
   'set cmark 0'
   'd 'sdvnbc2
   if (exp3 !="")
      'set ccolor 4'
      'set cstyle 3'
      'set cmark 0'
      'd 'sdvnbc3
   endif

   'set ccolor 1'
   'set cstyle 1'
   'set cmark 0'
   'd 'avgnbc1'-'avgnbc1

   'set line 1 1'
   'draw line 6.0 2.5 6.3 2.5'
   'set string 1'
   'set strsiz 0.11 0.11'
   'draw string 6.5 2.5 avg om'ga'nbc'
   'set line 1 3'
   'draw line 6.0 2.3 6.3 2.3'
   'set string 1'
   'draw string 6.5 2.3 sdv om'ga'nbc'

   'draw title 'om' without BC'
   'draw xlab Channel'

   'set string 1 l 6'
   'set strsiz 0.2 0.2'
   'draw string 0.2 10.80 platform:  'satype
   'draw string 0.2 10.35 valid   :  'date1
   'set parea off'

*if (exp3 !="")
*outfile=satype'_'exp1'_'exp2'_'exp3'_comp_'om'.png'
outfile=satype'.comp.png'
*else
*outfile=satype'_'exp1'_'exp2'_comp_'om'.png'
*endif
'printim 'outfile' x1100 y850 white'

return


*----------------------------------------------------------------
*  calcsdv
*
*  count  = the count field from a data file
*  field  = a grid from a data file, like omgbc
*  fldsqr = squares of same field, like omgbc2 
*
*----------------------------------------------------------------
function calcsdv(count,field,fldsqr)

   'define rterm1=1/count'
   'define rterm2=1/('count'-1)'
   'define svar=(('count'*'fldsqr' - 'field'*'field') * 'rterm1'*'rterm2')'
   'define sdv=sqrt('svar')'
   'define sdv=const('sdv',0,-u)'

return ( sdv )


endfile
