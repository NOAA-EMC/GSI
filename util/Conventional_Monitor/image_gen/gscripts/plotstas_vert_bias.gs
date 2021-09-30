************************
* plotstas_vert_bias.gs
************************

function vert_bias (args)

   hour=HOUR
   dday=DDAY
   dtype=DTYPE

   'rgbset2'
   'set background 99'
   'clear'
   'open ges_'dtype'_stas.ctl'
   'open anl_'dtype'_stas.ctl'
   'set grads off'
   debug=0

   'set t last'
   'q time'
   ti=sublin(result,1)
   dmy=subwrd(ti,3)
   say dmy
   hh=substr(dmy,1,2)
   dd=substr(dmy,4,2)

   if(dd !=dday | hh !=hour); say 'wrong hour or day'; exit; endif
   'q file'
   size=sublin(result,5)
   nxc=subwrd(size,3)
   nyc=subwrd(size,6)
   nzc=subwrd(size,9)

   '!echo $CONMON_RESTRICT_PLOT_AREAS > rest.txt'
   rest=read(rest.txt)
   restrict=subwrd(rest,2)
   say 'rs=' restrict

   iy=1
   while(iy <=nyc)

*     In order to save space skip certain redundant regions.
      if ( restrict = 1 )
         if ( iy = 2 | iy = 3 | iy = 5 | iy = 6 )
            iy=iy+1
            continue
         endif
      endif

      ix=1
      while(ix <=nxc)
         '!rm -f info.txt'
         '!cat ges_'dtype'_stas.ctl |grep "'ix' dtype=" > info.txt'
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

         if( iuse=-1 )
            datause='mon.'
         else
            datause='used'
         endif

         plot_vert(ix,iy,nzc,dmy,dtype,stype,subtype,datause,iuse,debug)

         ix=ix+1
      endwhile

      iy=iy+1
   endwhile

   return
endfile



function plot_vert(ix,iy,nzc,dmy,dtype,stype,subtype,datause,iuse,debug)

   'page'
   'clear'

   field.1.1=bias1.1
   field.1.2=bias1.2
   field.1.3=bias1.3
   field.2.1=rms1.1
   field.2.2=rms1.2
   field.2.3=rms1.3
   field.3.1=bias3.1
   field.3.2=bias3.2
   field.3.3=bias3.3
   field.4.1=rms3.1
   field.4.2=rms3.2
   field.4.3=rms3.3

   title.1="o-g(used)"
   title.2="rms(used)"
   title.3="o-g(monitored)"
   title.4="rms(monitored)"

   nfield=4
   nf=1
   xp=1
   yp=2
   while(nf <=nfield)

      say 'nf='nf
      'run setvpage  'xp' 'yp' 2  2 0.9'
      'set annot 98'
      'set line 98'
      'set map 98'

      'set xlopts 98'
      'set ylopts 98'
      'set datawarn off'
      'set t last'

      'set x 'ix
      say 'x='ix
      'set y 'iy
      'set z 1'
      'define vmax=max('field.nf.1',z=2,z='nzc')'
      'define vmin=min('field.nf.1',z=2,z='nzc')'
      'd vmax'

      say result
      cmax=subwrd(result,4)
      if(cmax=0); cmax=0.1;endif
      'd vmin'
      say result

      cmin=subwrd(result,4)
      if(cmin=0);cmin=-0.1;endif
      yrange=cmax-cmin
      say 'cmax='cmax

      dy=0.1*yrange
      cmax=cmax+dy
      cmin=cmin-dy

      say 'cmax='cmax
      'set z 2 'nzc
      'set vrange 'cmin' 'cmax
      'set ccolor 98'

      'd 'field.nf.1
      'set ccolor 2'
      'd 'field.nf.2
      'draw title 'dtype''stype'-'subtype':'title.nf' on 'dmy

      if( nf =2 );xp=xp+1;endif;
      yp=yp-1
      if(yp =0 ); yp=2;endif;

      nf=nf+1
   endwhile

   outfile=dtype''stype'-'subtype'_bias_vert_region'iy'.png'

   'printim 'outfile' x800 y650 white'
   if(debug=1)
      say 'enter'
      pull
   endif

return
