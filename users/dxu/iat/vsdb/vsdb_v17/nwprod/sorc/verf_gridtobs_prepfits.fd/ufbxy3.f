C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
c     SUBROUTINE ufbxy3(lunit,xyz,mate,i1,i2,nrep)
      SUBROUTINE ufbxy3(lunit,mate,i1,i2)
 
      use debug
      use guser
      CHARACTER*10 tgs(100)
c     DIMENSION xyz(i1,i2), mate(i2)
      DIMENSION mate(i2)
c     REAL*8 xyz(i1,i2)
      REAL*8 hdr(5,i2*2)
      CHARACTER*8 stnid
      EQUIVALENCE ( rstnid, stnid )
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
      nrep = 0
      indux = 0
      allocate(xyz(i1,i2))
 
C     CALL UFBTAB TO ITEMIZE THE HEADER CONTENTS OF A PREPBUFR FILE
C     -------------------------------------------------------------
 
c     print*,'before call to ufbtab'
      CALL ufbtab(lunit,hdr,5,i2*2,nhdr,'SID XOB YOB DHR ELV')
      PRINT *, ' TABULATED ', nhdr, ' UNMATED REPORTS'
 
C     COPY THE LON/LAT OF EACH UNIQUE REPORT - START AT NUMBER ONE
C     ------------------------------------------------------------
 
      do i=1,5
       print*,'hdr(i,1)=',hdr(i,1)
      enddo
      print*,'before xyz'
      xyz(1,1) = hdr(2,1)
      xyz(2,1) = hdr(3,1)
      print*,'after xyz'
      rstnid = hdr (1,1)
      iq = INDEX ( stnid, target )
      IF ( iq .ne. 0 ) indux = 1
      nrep = 1
      mate(nrep) = 0
 
      print*,'in ufbxy, before nhdr loop'
      DO n = 2, nhdr
        IF (hdr(1,n).ne.hdr(1,n-1).or.hdr(2,n).ne.hdr(2,n-1).or.
     +              hdr(3,n).ne.hdr(3,n-1).or.hdr(4,n).ne.hdr(4,n-1).or.
     +              hdr(5,n).ne.hdr(5,n-1)) THEN
c         print*,'nrep+1, i2=',nrep+1,i2
          IF (nrep+1.gt.i2) then
             call errmsg('UFBXY3 - TABLE TOO SMALL')
             call errexit(1)
          END IF
          xyz(1,nrep+1) = hdr(2,n)
          xyz(2,nrep+1) = hdr(3,n)
	  rstnid = hdr (1,n)
	  iq = INDEX ( stnid, target )
          nrep = nrep + 1
	  IF ( iq .ne. 0 .and. indux .eq. 0 ) indux = nrep
          mate(nrep) = 0
        ELSE
C         REPORT N-1 IS THE MATE OF REPORT N
C         THEREFORE, MARK REPORT NREP AS MATED
C         ------------------------------------
          mate(nrep) = 1
        END IF
      END DO
 
C     RETURN WITH THE UNIQUE TABLE
C     ----------------------------
 
      RETURN
      END
