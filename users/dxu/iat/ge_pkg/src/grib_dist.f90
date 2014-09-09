SUBROUTINE grib_dist(dist,i1,j1,i2,j2)
!******************************************************
!
       pi=4.0*atan(1.0)
       fac=6370.0   ! is radius of earth in KM
       xlon1=pi*float(i1)/180.0
       xlon2=pi*float(i2)/180.0
       phi1=pi*float(91-j1)/180.0
       phi2=pi*float(91-j2)/180.0
       cosang=sin(phi1)*sin(phi2)+cos(phi1)*cos(phi2)*cos(xlon1-xlon2)
       dist=fac*acos(cosang)   ! is spherical dist in  KM 
       RETURN
       END SUBROUTINE grib_dist
