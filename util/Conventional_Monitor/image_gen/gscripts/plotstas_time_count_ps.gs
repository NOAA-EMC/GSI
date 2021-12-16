*-------------------------------------------------------------------------
*   This script plots surface pressure time series:
*   data count: num assimilated, num rejected by gross check, num rejected
*   by variational qc,  num monitored
*-------------------------------------------------------------------------

function ps_time_count (args)

*   'reinit'
   'open ges_ps_stas.ctl'
   'open anl_ps_stas.ctl'
*   'open ps_stas_int.ctl'
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

   '!echo $CONMON_RESTRICT_PLOT_AREAS > rest.txt'
   rest=read(rest.txt)
   restrict=subwrd(rest,2)
   say 'restrict=' restrict

   iy=1
   while(iy <=iyc)

*     In order to save space skip regions N. Hemisphere(2), S. Hemisphere(3), and Canada(6)
      if ( restrict = 1 )
         if ( iy = 2 | iy = 3 | iy = 5 | iy = 6 )
            iy=iy+1
            continue
         endif
      endif

      say 'iy=' iy
      '!rm -f area.txt'

      if( iy <10)
         '!cat ges_ps_stas.ctl |grep "region=  'iy' " > area.txt'
      else
         '!cat ges_ps_stas.ctl |grep "region= 'iy' " > area.txt'
      endif
      result=read(area.txt)
      rc=sublin(result,1)
      area="uknown"
      if (rc = 0)
         info=sublin(result,2)
         area=substr(info,14,25)
      endif
      result=close(area.txt)
      say 'area = 'area
      ix=1
   
      while(ix <=ixc)

         '!rm -f info.txt'
         '!cat ges_ps_stas.ctl |grep "'ix' dtype=" > info.txt'
         result=read(info.txt)
         rc=sublin(result,1)
         iuse=0
         say 'dtype= 'dtype
         say 'rc   = 'rc

         if (rc = 0)
            info=sublin(result,2)
            stype=subwrd(info,6)
            subtype=subwrd(info,8)
            iuse=subwrd(info,10)
         endif
         say 'stype   = 'stype
         say 'subtype = 'subtype
         say 'iuse    = 'iuse
         result=close(info.txt)
         
         plottime(ix,iy,stype,hh,dd,area,stype,subtype,iuse,debug)

         ix=ix+1
      endwhile

      iy=iy+1
   endwhile

   return
endfile


*------------------------------------------------------------------
*  function plottime
*------------------------------------------------------------------
function plottime(ix,iy,stype,hh,dd,area,stype,subtype,iuse,debug)

   'clear'

   nfield=4
   field.1.1=count1.1
   field.1.2=count1.2
*   field.1.3=count1.3

   field.2.1=count_vqc1.1
   field.2.2=count_vqc1.2
*   field.2.3=count_vqc1.3

   field.3.1=count2.1
   field.3.2=count2.2
*   field.3.3=count2.3

   field.4.1=count3.1
   field.4.2=count3.2
*   field.4.3=count3.3

   title.1="ass. no."
   title.2="no. rej. by VQC"
   title.3="no. rej. by GC"
   title.4="no. monitored"

   'set t 1'
   'query time'
   fdmy=sublin(result,1)
   fti=subwrd(fdmy,5)

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
      dmy=sublin(result,1)
      ti=subwrd(dmy,5)

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

*      'set axlim 'minvar' 'maxvar
*      'set yaxis 'minvar' 'maxvar' 'dy
      'set vrange 'minvar' 'maxvar

      'set ccolor 1'
      'set cmark 0'
      'd  'field.nf.1

      'set ccolor 2'
      'set cmark 1'
      'd  'field.nf.2

*      'set ccolor 3'
*      'set cmark 2'
*      'd  'field.nf.3
*      if(iuse = 1);datause='assimilated';endif

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
*      'set line 3 1'
*      'draw line 5.1 0.6 5.4 0.6'
      'draw string 5.5 0.55    'fti'-'ti
      nf=nf+1
   endwhile

   outfile='ps'stype'-'subtype'_count_region'iy'_lev1.png'
   'printim 'outfile' x800 y700 white'

   if(debug=1)
      say 'enter'
      pull
   endif

return
