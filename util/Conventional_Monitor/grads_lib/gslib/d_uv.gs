function duv(arg)

*
* usage: run d_uv ugrd vgrd [color_key]
*
* this function does a d skip(ugrd,n);v
*  or                  d skip(ugrd,n);v;color_key
*
* where n is set to an appropriate value
*
* v1.1 w. ebisuzaki
* v1.2 4/6/98 revised empirical formula for skip
*

u = subwrd(arg,1)
v = subwrd(arg,2)
c = subwrd(arg,3)

* get lat/lon info

'query dims'
lons = sublin(result,2)
lats = sublin(result,3)
dx = subwrd(lons,13) - subwrd(lons,11)
dy = subwrd(lats,13) - subwrd(lats,11)

dn=dx
if ( dy > dx)
   dn = dy
endif

* some tunable parameters 
skip = dn / 50 + 0.5

if ( skip < 1 )
   skip=1
endif
if ( c != "" )
  c=';' c
endif
'd skip(' u ',' skip ');' v  c
