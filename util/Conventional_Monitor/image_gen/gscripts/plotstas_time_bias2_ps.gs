*   the program plot surface pressure time series:
*   data count: no assimilated, no rejected by gross check, no rejected
*   by variational qc,  no monitored

function time_bias2_ps (args)

   'open ges_ps_stas.ctl'
   'open anl_ps_stas.ctl'

   'set grads off'
   debug=0

   'set t 1 last'
   'q time'
   dmy=sublin(result,1)
   ti=subwrd(dmy,5)
   say ti
   hh=substr(ti,1,2)
   dd=substr(ti,4,2)

   'q file'
   size=sublin(result,5)
   ixc=subwrd(size,3)
   iyc=subwrd(size,6)
   say ixc
   say 'iyc=' iyc

   iy=1

   while(iy <=iyc)

      say 'iy=' iy
      '!rm -f area.txt'
      '!cat ges_ps_stas.ctl |grep "region= 'iy' " > area.txt'
      result=read(area.txt)
      rc=sublin(result,1)
      area="uknown"
      if (rc = 0)
         info=sublin(result,2)
         area=substr(info,14,25)
      endif
      result=close(area.txt)
      ix=1

      while(ix <=ixc)

         '!rm -f info.txt'
         '!cat ges_ps_stas.ctl |grep "'ix' dtype=" > info.txt'
         result=read(info.txt)
         rc=sublin(result,1)
         iuse=0
         if (rc = 0)
            info=sublin(result,2)
            stype=subwrd(info,6)
            subtype=subwrd(info,8)
            iuse=subwrd(info,10)
         endif
         result=close(info.txt)

         plottime(ix,iy,stype,hh,dd,area,stype,subtype,iuse,debug)

         ix=ix+1
      endwhile

      iy=iy+1
   endwhile

   return
endfile


function plottime(ix,iy,stype,hh,dd,area,stype,subtype,iuse,debug)

   'clear'

   nfield=4
   field.1.1=bias2.1
   field.1.2=bias2.2
   field.1.3=bias2.3
   field.2.1=rms2.1
   field.2.2=rms2.2
   field.2.3=rms2.3
   field.3.1=bias3.1
   field.3.2=bias3.2
   field.3.3=bias3.3
   field.4.1=rms3.1
   field.4.2=rms3.2
   field.4.3=rms3.3

   title.1="o-g for rej. by GC"
   title.2="rms for rej. by GC"
   title.3="o-g for monitored"
   title.4="rms for monitored"

   nf=1
   while(nf <=nfield)
      y1=10.6-(nf-1)*2.5
      y2=y1-1.8
      ystring=y1+0.1
      say ' y1='y1
      say ' y2='y2
      say ' ystring='ystring
      'set t 1 last'
      'query time'
      'set y 'iy
      'set x 'ix
      'set z 1'
      'set gxout stat'

      'd 'field.nf.1
      rec8=sublin(result,8)
      minvar1=subwrd(rec8,4)
      maxvar1=subwrd(rec8,5)

      'd 'field.nf.2
      rec8=sublin(result,8)
      maxvar=subwrd(rec8,5)
      minvar=subwrd(rec8,4)
      if(minvar > minvar1)
         minvar=minvar1
      endif
      if(maxvar1 > maxvar)
         maxvar=maxvar1
      endif

      say ' 'minvar
      say ' 'maxvar
      yrange=maxvar-minvar
      dy=0.1*yrange
      minvar=minvar-dy
      maxvar=maxvar+dy
      say ' 'minvar
      say ' 'maxvar
      'set parea 1.0 8.0 'y2' 'y1
      'set gxout line'
      'set t 1 last'
      'set datawarn off'
      'set tlsupp year'
      'set grads off'
      'set y 'iy
      'set x 'ix
      'set z 1'
      'set vrange 'minvar' 'maxvar
      'set ccolor 1'
      'set cmark 0'
      'd  'field.nf.1
      'set ccolor 2'
      'set cmark 1'
      'd  'field.nf.2

      if(iuse = -1)
         datause='mon.'
      else
         datause='used'
      endif

      'draw string '1.1' 'ystring' ps'stype'-'subtype'('datause'):'title.nf' at 'area
      'set line 1 1'
      'draw line 1.1 0.6 1.4 0.6'
      'draw string 1.5 0.55  init. outloop'
      'set line 2 1'
      'draw line 3.1 0.6 3.4 0.6'
      'draw string 3.5 0.55  final outloop'
      nf=nf+1
   endwhile

   outfile='ps'stype'-'subtype'_bias2_region'iy'.png'
   'printim 'outfile' x950 y750 white'

   if(debug=1)
      say 'enter'
      pull
   endif

return
