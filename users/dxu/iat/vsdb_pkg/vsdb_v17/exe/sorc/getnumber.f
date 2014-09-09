c         character*10 cx, cy
c         real ix,iy
c         cx='-5.6'
c         cy='5.6'
c         x=CharToReal(cx)
c         y=CharToReal(cy)
c         write(*,'(2f7.2)') x, y
c         stop
c         end


C This function is to get grib id  from region string x 
C such as  x = 'G211/region' or just x = 'G212', 
C Where 212 is grib id
C
C Author Binbin Zhou, 
c        Mar 20, 2005

        function ID (x)
        character(20) x, gribid
        integer p, length, y
        p = index(x,'/')
        length = len_trim(x)
        if (p.gt.0) then
         gribid = x (2:p-1)
        else
         gribid = x (2:length)
        end if

        call st_numb (gribid, y, ier)
        ID = y

        return
        end 

       

C This function is to get numerical number from a string x, 
C such as x = '100.05', '.5', '100', '100.', etc
C Author: Binbin ZHou
C         Mar 20, 2005
C
	function CharToReal(x)     
        real CharToReal
        character(*) x 
        character*20 p1, p2
        integer p, length, d1, d2, pp2
        real r1,r2       

        length = len_trim(x)
        p = index(x,'.')       !e.g.  x=.15: p=1
        if (p.eq.1) then
          p1 = '0'
          p2 = x(p+1:length)
          pp2= len_trim(p2)
        else if (p.gt.1) then   !e.g. x=12.5: p=3
          p1 = x (1:p-1)
          p2 = x(p+1:length)
          pp2= len_trim(p2)
        else
          p1 = x
          p2 = '0'
          pp2 = 0
        end if

        call st_numb (p1,d1,ier)
        call st_numb (p2,d2,ier)

        r1 = float(d2)

        if(pp2.eq.1) then
          r2 = r1 /10.0
        else if (pp2.eq.2) then
          r2 = r1 /100.0
        else if (pp2.eq.3) then
          r2 = r1 /1000.0
        else if (pp2.eq.4) then
          r2 = r1 /10000.0
        else if (pp2.eq.5) then
          r2 = r1 /100000.0
        else if (pp2.eq.6) then
          r2 = r1 /1000000.0
        end if
 
         if(x(1:1).eq.'-') then
           CharToReal = -1.0 * (abs(d1) + r2)
         else
           CharToReal = float(d1) + r2
         end if
   
c        if(d1 .gt. 0) then 
c          CharToReal = float(d1) + r2
c        else
c          CharToReal = -1.0 * (abs(d1) + r2)
c        end if

         return  
         end
     
