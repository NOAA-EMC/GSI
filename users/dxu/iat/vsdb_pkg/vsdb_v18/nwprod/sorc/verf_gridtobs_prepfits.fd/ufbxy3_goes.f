C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE ufbxy3_goes(lunit,xyz,mate,i1,i2,nrep)
 
      CHARACTER*10 tgs(100)
c     DIMENSION xyz(i1,i2), mate(i2)
      DIMENSION mate(i2)
      REAL*8 xyz(i1,i2)
      REAL*8 hdr(5,i2*2)
      CHARACTER*8 target, stnid
      COMMON /debug/ target, indux
      EQUIVALENCE ( rstnid, stnid )
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
      nrep = 0
      indux = 0
 
C     CALL UFBTAB TO ITEMIZE THE HEADER CONTENTS OF A PREPBUFR FILE
C     -------------------------------------------------------------
 
c     print*,'before call to ufbtab'
c     CALL ufbtab(lunit,hdr,5,i2*2,nhdr,'SID XOB YOB DHR ELV')
      CALL ufbtab(lunit,hdr,5,i2*2,nhdr,'CLONH CLATH')
      PRINT *, ' TABULATED ', nhdr, ' UNMATED REPORTS'
 
C     COPY THE LON/LAT OF EACH UNIQUE REPORT - START AT NUMBER ONE
C     ------------------------------------------------------------
 
c     do i=1,5
c      print*,'hdr(i,1)=',hdr(i,1)
c     enddo
c     print*,'hdr(1,1),hdr(2,1)=',hdr(1,1),hdr(2,1)
c     xyz(1,1) = hdr(2,1)
c     xyz(2,1) = hdr(3,1)
      xyz(1,1) = hdr(1,1)
      xyz(2,1) = hdr(2,1)
      rstnid = hdr (1,1)
      iq = INDEX ( stnid, target )
      IF ( iq .ne. 0 ) indux = 1
      nrep = 1
      mate(nrep) = 0
 
      DO n = 2, nhdr
c       IF (hdr(1,n).ne.hdr(1,n-1).or.hdr(2,n).ne.hdr(2,n-1).or.
c    +              hdr(3,n).ne.hdr(3,n-1).or.hdr(4,n).ne.hdr(4,n-1).or.
c    +              hdr(5,n).ne.hdr(5,n-1)) THEN
c       print*,'hdr(1,n),hdr(1,n-1)=',hdr(1,n),hdr(1,n-1)
c       print*,'hdr(2,n),hdr(2,n-1)=',hdr(2,n),hdr(2,n-1)
c       print*,'hdr(1,n),hdr(2,n)=',hdr(1,n),hdr(2,n)
        IF (hdr(1,n).ne.hdr(1,n-1).or.hdr(2,n).ne.hdr(2,n-1)) THEN
c         print*,'nrep+1, i2=',nrep+1,i2
c         IF (nrep+1.gt.i2) CALL abort('UFBXY3 - TABLE TOO SMALL')
          IF (nrep+1.gt.i2) then
             print*,'UFBXY3 - TABLE TOO SMALL'
             CALL abort
          END IF
c         xyz(1,nrep+1) = hdr(2,n)
c         xyz(2,nrep+1) = hdr(3,n)
          xyz(1,nrep+1) = hdr(1,n)
          xyz(2,nrep+1) = hdr(2,n)
c  rstnid = hdr (1,n)
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
