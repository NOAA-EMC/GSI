function loop (var)
'c'
'set vpage off'
* set up virtual page dimensions (two small plots on top, one larger plot on bottom)
_vp.1='0  4.25 6.5 11'
_vp.2='4.25 8.5 6.5 11'
_vp.3='0 8.5 0 6.5'

* check that window is portrait
'q gxinfo'
res=result
pline=sublin(res,2)
xsize=subwrd(pline,4)
ysize=subwrd(pline,6)
if(xsize>ysize); say 'this script needs to be run in portrait window.  exiting...';exit;endif

* get file info
'q files'
res=result
l1=sublin(res,1)
f1=subwrd(l1,4)
l2=sublin(res,4)
f2=subwrd(l2,4)
'q file'
qfile=result
sizeline=sublin(qfile,5)
zsize=subwrd(sizeline,9)
nvarline=sublin(qfile,6)
nvars=subwrd(nvarline,5)
say nvarline

* turn on print of statistical output with each display (in script mode, output goes to var "result")
'set stat on'

* start loop
ivar=1
qplot=''
backup=0
while(ivar<=nvars)
  if(ivar=0);say "cannot backup beyond first plot";ivar=1;endif
* get info for variable number ivar
  nline=6+ivar
  varline=sublin(qfile,nline)
  var=subwrd(varline,1)
  nlev=subwrd(varline,2)
  len=strlen(varline)
  dstart=wrdpos(varline,4)
  desc=substr(varline,dstart,len-dstart+1)
  if(substr(desc,1,3)='** ');
    desc=substr(varline,dstart+3,len-dstart-2)
  endif
  if(nlev=0);klev=1;else;klev=nlev;endif
* normally going to first level for variable, but if backing up bring us back to old variable, we want the last level for that var
  if(backup=0)
    ilev=1
  else
    ilev=klev
  endif
  while(ilev<=klev & ilev<=zsize & qplot != 'q')
  say ivar' 'ilev

  'c'
  rc=setup(1)
  'set z 'ilev
  res=result
  zlev=subwrd(res,4)
  plev.ilev=0
  'set gxout stat'
  'd 'var
  res=result
  line1=sublin(res,1)
  if(substr(line1,1,20)='Data Request Warning')
    say 'Need new descriptor file to display 'var' at level 'zlev
    ilev=ilev+1
    continue
  endif
  cline=sublin(res,7)
  if(substr(cline,1,11) != 'Undef count')
    say 'ERROR IN SCRIPT LOGIC. SKIP 'var' at level 'zlev
    ilev=ilev+1
    continue
  endif
  valid=subwrd(cline,8)
  if(valid=0)
    if(backup=1)
      ilev=ilev-1
      continue
    else
      say 'no 'var' for lev 'ilev'...skip to 'ilev+1
      ilev=ilev+1
      if(ilev>klev & nlev !=0 & klev < zsize)
        say 'not all levels available for var 'var'.'
        if(ivar!=nvar); say 'move on to next variable';endif
      endif
      continue
    endif
  endif
  backup=0
  if(nlev>0)
    levtitle=' at lev='zlev
  else
    levtitle=''
  endif
  
  noplot=0
  if(noplot=0)
  'set gxout shaded'
  'd 'var'.2 -' var'.1'
  res=result
  clin1=sublin(res,15)
  say clin1
  cbarn
  'draw title 'var levtitle'\file: 'f1
  keeplevs()
  rc=setup(2)
  'set ccols '_cols
  'set clevs '_levs
  'd 'var'.4 -'var'.3'
  res=result
  clin2=sublin(res,15)
  say clin2
  cbarn
  'draw title 'var levtitle'\file: 'f2
*  'cbarnv 0.8 0 4.25 7.00 1'
*  '/climate/save/wx24ds/grads/cbarnskip 2 0.8 0 4.25 7.00 1'
  'cbarnskip 2 0.8 0 4.25 7.00 1'
  rc=setup(3)
  'd 'var'.4 -'var'.3 -'var'.2 +'var'.1'
  minmax=sublin(result,8)
  say var' at lev='zlev'  'minmax
  'draw title difference\'desc'  'levtitle'\'minmax
  'cbarn'
*  'cbarnskip 2'
  else
    say '****  'var levtitle' ilev='ilev
  endif  * end of noplot
  plev.ilev=1
  say 'enter 'q' to quit, 'b' to backup one plot, otherwise continue'
  pull qplot; qplot=subwrd(qplot,1)
  if (qplot=q)
    break
  endif
  if (qplot=b)
    while(ilev>0)
      ilev=ilev-1
      if(plev.ilev=1);break;endif
    endwhile
    if(ilev=0);break;endif
  else
    ilev=ilev+1
  endif
  if(ilev>klev & nlev !=0  & klev < zsize)
    say 'not all levels available for var 'var'.'
  endif
  endwhile
  if(ilev=0)
    ivar=ivar-1
    backup=1
  else
    ivar=ivar+1
  endif
endwhile

function setup(args)
pnum=subwrd(args,1)
'set vpage '_vp.pnum
*'set xlint 5'
*'set ylint 5'
*'set xlopts 1 4 0.18'
*'set ylopts 1 4 0.18'
'set grads off'
'q time'
res=result
_tstamp=subwrd(res,3)
return

function keeplevs()
'query gxinfo'
gxinfo_rec1 = sublin(result,1)
grtyp=subwrd(gxinfo_rec1,4)
if ( grtyp = 'Shaded' | grtyp = 'GrFill' )
  'query shades'
  shdinfo = result
  if (subwrd(shdinfo,1)='None')
    say 'no shading info. use default'
  else
    cnum = subwrd(shdinfo,5)
    num = 0
    _cols = ''
    _levs = ''
    while (num<cnum)
      num=num+1
      rec = sublin(shdinfo,num+1)
      kcol.num = subwrd(rec,1)
      lo.num = subwrd(rec,2)
      hi.num = subwrd(rec,3)
      _cols = _cols' 'kcol.num
      if(num<cnum)
         _levs = _levs' 'hi.num
      endif
    endwhile
  endif
else
 say gxinfo_rec1
 say 'but script keeplevs only works for shaded or grfill.'
 if ( grtyp = 'Clear' ); say 'If screen is not clear, vpage may have been reset'; endif
endif
return

