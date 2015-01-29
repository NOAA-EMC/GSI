C-----------------------------------------------------------------------------
      SUBROUTINE FITWAV_1D(x,ix,iy,ms,me)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                    C
C     USAGE: TRANSFER LATITUDE GRID TO WAVE COEFF,                   C
C            THEN TRANSFER BACK TO LATITUDE GRID,                    C
C            BY SELECTED ZONALWAVE GROUP.                                 C
C     CODE : F77 on IBMSP --- Yuejian Zhu (08/01/06)                 C
C                                                                    C
C     INPUT: x(ix,iy) - grided data with dimension                   C
C                       ix - longitude points                        C
C                       iy - latitude points                         C
C                                                                    C
C     OUTPUT:x(ix,iy) - grided data with dimension                   C
C                       ix - longitude points                        C
C                       iy - latitude points                         C
C                       atfer transfer truncation                    C
C            ms - start wave number                                  C
C                 ms=0 (zonal mean)                                  C
C            me - end   wave number                                  C
C                                                                    C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      parameter (nw=500)
      dimension x(ix,iy)
      dimension a(0:nw),b(0:nw)
      dimension xa(nw),xb(nw)
c
      x2pi = 2.0*3.1415926536
c     mnw=(nw+1)/2
      mnw=(ix+1)/2

      a = 0.0
      b = 0.0
      do j = 1, iy
       xa0 = 0.0
       do k = 1, ix
        xa0 = xa0 + x(k,j)
       enddo
       a0 = xa0/float(ix)
       do i = 1, mnw
        xa(i) = 0.
        xb(i) = 0.
        do k = 1, ix
         xa(i) = xa(i) + x(k,j)*cos(float(i*k)*x2pi/float(ix))
         xb(i) = xb(i) + x(k,j)*sin(float(i*k)*x2pi/float(ix))
        enddo
        a(i) = 2.0*xa(i)/float(ix)
        b(i) = 2.0*xb(i)/float(ix)
       enddo
       a(0) = a0
       b(0) = 0.0
c
       do k = 1, ix
        x(k,j) = 0.0
        do i = ms, me
         x(k,j) = x(k,j) + a(i)*cos(float(i*k)*x2pi/float(ix)) 
     .                   + b(i)*sin(float(i*k)*x2pi/float(ix))
        enddo
       enddo    
      enddo

      return
      end

