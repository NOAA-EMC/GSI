c        Just for testing getregionid subroutine
c        integer region_id(427734), gribid
c        real region_latlon(2,427734)
c        gribid=255
c        call getregionid(region_id,region_latlon,gribid)
c        do i = 1,427734
c          write(*,'(2i10,2f10.2)') i, region_id(i), 
c     &       region_latlon(1,i),region_latlon(2,i)
c        end do       
c        stop
c        end




        subroutine getregionid(region_id, region_latlon, gribid)

c  This subroutine is to retrieve resion id (28) at each grid
c  for any requested gribid. The 28 region ids are predfined
c  in GRIB#104. The algorithm is first get each grid point's
C  Gr(i,j), ie. xpts212 and ypts212 in the code, then retrieve
c  its earth lat and long, ie rlon and rlat in the code
c  then from rlong a rlat to get (i,j) in grib104,
c  then from this new (i,j) to get region id fors point 
c  if the point is outside grib104 domain, set it to be -1
c
c  Mar. 10, 2005   Original, Binbin Zhou, SAIC@EMC/NCEP/NOAA
c  Oct. 12, 2006   Modification: Add 255 grid, Binbin Zhou 
c
c  INPUT: gribid, integer, requested grib id number, such as 212, 223
c  OUTPUT: region_id(Ngrid), integer, region id for each grid point
c          in reference of grib104 
c
        integer region_id(*) 
        real region_latlon(2,*)
        integer gribid
        
        integer kpds212(200), kpds104(200)
        integer kgds212(200), kgds104(200)
        real, allocatable, dimension(:) :: xpts212,ypts212
        real, allocatable, dimension(:) :: rlon,rlat,crot,srot 
        real, allocatable, dimension(:) :: xpts104,ypts104
        integer kgds(200)
        character*50 gds(400)

        integer ig104(147,110)
        character*3 regions

        ! get kgds array with requested gribid as input
        if(gribid.ne.255) then
         call makgds(gribid, kgds212, gds, lengds, ier)
        else                           !specific for HYSPLIT smoke grib setting
         kgds212(1)=0
         kgds212(2)=801
         kgds212(3)=534
         kgds212(4)=0
         kgds212(5)=-175000
         kgds212(6)=0
         kgds212(7)=79950
         kgds212(8)=-55000
         kgds212(9)=65534
         kgds212(10)=65534
         kgds212(11)=64
         kgds212(12)=0
         kgds212(13)=0
         kgds212(14)=0
         kgds212(15)=167772159
         kgds212(16)=-268415528
         kgds212(17)=0
         kgds212(18)=0
         kgds212(19)=1
         kgds212(20)=255
         kgds212(21)=0
         kgds212(22)=0
         kgds212(23)=0
         kgds212(24)=0
         kgds212(25)=0
        end if

        write(*,*) 'NX,NY=',kgds212(2),kgds212(3)

        Ngrid=kgds212(2)*kgds212(3)
        allocate(xpts212(ngrid))
        allocate(ypts212(ngrid))
        allocate(rlon(ngrid))
        allocate(rlat(ngrid))
        allocate(crot(ngrid))
        allocate(srot(ngrid))
        allocate(xpts104(ngrid))
        allocate(ypts104(ngrid))


        iopt=1
        fill=-9999.0

        !from grid sequence # -> (i,j)
        do N=1, Ngrid
          xpts212(n)=MOD(n, KGDS212(2)) 
           if (xpts212(n).eq.0) then
            xpts212(n)= KGDS212(2)
            a=n/KGDS212(2)
            ypts212(n)=NINT(A)
           else
           a=n/KGDS212(2)+1
           ypts212(n)=NINT(A)
        end if
        end do

        !from (i,j) to retrieve lat and long at all (i,j) points
        call gdswiz(kgds212,iopt,Ngrid,fill,xpts212,ypts212,
     &       rlon,rlat,nret,lrot,crot,srot)
 

        gribid=104
        call makgds(gribid, kgds104, gds, lengds, ier)
       
        !from array of lat and long of all points to get xpts104,ypts104, i.e. (i,j) of thse points
        iopt=-1
        call gdswiz(kgds104,iopt,Ngrid,fill,xpts104,ypts104,
     &       rlon,rlat,nret,lrot,crot,srot)

        open(20, file='grid#104', status='old') 
        read(20, '(20I4)') ig104

        do n=1,Ngrid
          if(xpts104(n).lt.0.0.or.ypts104(n).lt.0.0) then
           region_latlon(1,n) = int(1000*rlat(n))/1000.0
           region_latlon(2,n) = int(1000*rlon(n))/1000.0
           region_id(n)= -1
          else
           region_id(n)=ig104(NINT(xpts104(n)),NINT(ypts104(n)))
           region_latlon(1,n) = int(1000*rlat(n))/1000.0
           region_latlon(2,n) = int(1000*rlon(n))/1000.0
          end if
        end do
  

        deallocate(xpts212)
        deallocate(ypts212)
        deallocate(rlon)
        deallocate(rlat)
        deallocate(crot)
        deallocate(srot)
        deallocate(xpts104)
        deallocate(ypts104)

        close(20)

        return
        end

