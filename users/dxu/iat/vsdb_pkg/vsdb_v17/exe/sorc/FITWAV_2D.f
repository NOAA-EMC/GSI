C---------------------------------------------------------------------------
      SUBROUTINE FITWAV_2D(field,ix,iy,ns,ne,iromb)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                    C
C     USAGE: TRANSFER GRID TO SPECTRAL COEFFICIENTS,                 C
C            THEN TRANSFER BACK TO GRID,                             C
C            BY SELECTED WAVE GROUP BASED ON N.                      C
C     CODE : F77 on IBMSP --- Yuejian Zhu (08/01/06)                 C
C                                                                    C
C     INPUT: field(ix,iy) - grided data with dimension               C
C                           ix - longitude points                    C
C                           iy - latitude points                     C
C                                                                    C
C     OUTPUT:field(ix,iy) - grided data with dimension               C
C                           ix - longitude points                    C
C                           iy - latitude points                     C
C                           atfer transfer truncation                C
C            ns - start wave number N                                C
C                 (M will be from 0 to N)                            C
C            ne - end   wave number N                                C
C                 (M will be from 0 to N)                            C
C            iromb - integer spectral domain shape                   C
C                 (0 for triangular, 1 for rhomboidal)               C
C                                                                    C
C                                                                    C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      parameter (maxwv=40,mx=(maxwv+1)*(maxwv+2)/2)
      dimension field(ix,iy),wave(2,mx),wavef(2,mx)
c
c     call sptez subroutine in /nwprod/lib/sorc/sp directory
c
      call sptez(iromb,maxwv,0,ix,iy,wave,field,-1)      
c
c     fill zero for wave number less than ns and great than ne 
c     assume one dimension data arrary wavef(*,ij) represents as
c     following example: iromb=0, maxwv=40
c        wavef(*,1) --> n=0, m=0
c        wavef(*,2) --> n=1, m=0
c        wavef(*,3) --> n=2, m=0
c        ......
c        wavef(*,42) --> n=1, m=1
c        ......
c        wavef(*,82) --> n=2, m=2
c        ......
c
      wavef = 0.0
      ij = 0
      do m = 1, maxwv+1
       do n = m, maxwv+1 
        ij = ij + 1
        if (n.gt.ns.and.n.le.(ne+1)) then
         wavef(1,ij) = wave(1,ij)
         wavef(2,ij) = wave(2,ij)
        endif
       enddo
      enddo
      call sptez(iromb,maxwv,0,ix,iy,wavef,field,1)      

      return
      end

