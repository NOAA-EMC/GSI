C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      FUNCTION ireadpb(lunit)

      use observ
      use obstrs

      character*8 subset2

c     CHARACTER*8 sid
      REAL*8 hdn(10), can(255), obn(10,255), qmn(10,255), 
     +            tob(2,255,10),obn2(10,255),obn3(10,255),
     *            obn4(10,255),obn5(10,255),obn6(10,255),
     *            obn7(10,255),obn8(10,255)
 
      dimension mand(12)
c     equivalence(sid,hdr(1))

      DATA bmiss /10E10/
      DATA ievn /10/
      data mand /1000,925,850,700,600,500,400,300,250,200,150,100/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
C  READ AND UNPACK THE NEXT SUBSET FROM LUNIT
C  ------------------------------------------
 
      CALL readsb(lunit,ireadpb)
      IF (ireadpb.ne.0) RETURN
 
      CALL ufbint(lunit,hdr,10,1,iret,headr)
      CALL ufbint(lunit,cat,1,255,nlev,'CAT')
      CALL ufbint(lunit,qms,10,255,nlev,qmstr)
      CALL ufbint(lunit,obs,10,255,nlev,obstr)
      if(subset(:6).eq.'ADPSFC') then
      call ufbint(lunit,obs2,10,255,nlev2,obstr2)
      call ufbint(lunit,obs3,10,255,nlev3,obstr3)
      call ufbint(lunit,obs4,10,255,nlev4,obstr4)
      call ufbint(lunit,obs5,10,255,nlev5,obstr5)
      call ufbint(lunit,obs6,10,255,nlev6,obstr6)
      endif
      if(subset(:6).eq.'GPSIPW') then
      call ufbint(lunit,obs7,10,255,nlev7,obstr7)
      endif
      call ufbint(lunit,obn8,1,255,nlen8,obstr8)
      nlen8=1
      if(nlev3.eq.0) then
          obs3=bmiss
          nlev3=1
      endif
c     if(nlev4.eq.0) nlev4=1
      if(nlev4.eq.0) then
          obs4=bmiss
          nlev4=1
      endif
      if(nlev2.eq.0) then
          obs2=bmiss
          nlev2=1
      endif
      if(nlev5.eq.0) then
          obs5=bmiss
          nlev5=1
      endif
      if(nlev6.eq.0) then
          obs6=bmiss
          nlev6=1
      endif
      if(nlev7.eq.0) then
          obs7=bmiss
          nlev7=1
      endif
c     call ufbint(lunit,obs2,10,1,nlev,obstr)
      CALL ufbevn(lunit,tob,2,255,10,nlev,'TOB TPC')

      imand=0

      if(subset.eq.'ADPUPA') then
      do i=1,nlev
c      cat(i)=1.
       if(cat(i).eq.100000000000.) then
       do j=1,12
        if(nint(obs(1,i)).eq.mand(j)) then
         imand=1
         goto 90
        endif
       enddo
       imand=0
90     continue
       if(imand.eq.1) then
        cat(i)=1.
       else
        cat(i)=2.
       endif
       endif
      enddo
      endif

C     PEEL BACK EVENTS TO STORE SENSIBLE TEMP IN CASE TEMP IS VIRTUAL
C     ---------------------------------------------------------------

      DO i = 1, nlev
        DO j = 1, ievn
          IF (tob(2,i,j).eq.8) THEN
            obs(3,i) = tob(1,i,j+1)
            GO TO 10
          ELSE IF (tob(2,i,j).ge.bmiss) THEN
            GO TO 10
          END IF
        END DO
   10   CONTINUE
      END DO
 
C     CHECK TO SEE IF A COMPANION MASS OR WIND PIECE FOLLOWS
C     ------------------------------------------------------
   20 CALL ufbget(lunit,hdn,10,iret,headr)
      IF (iret.lt.0) THEN
        CALL readmg(lunit,subset2,idate,ireadpb)
        IF (ireadpb.ne.0.or.subset.ne.subset2) RETURN
        nsub = nmsub(lunit)
        GO TO 20
      END IF
 
      DO i = 1, 5
        IF (hdr(i).ne.hdn(i)) RETURN
      END DO
 
C     COMBINE A COMPANION PIECE WITH ITS MATE
C     ---------------------------------------
 
      CALL readsb(lunit,iret)
      IF (iret.ne.0) then
           call errmsg('READPB - SUBSET SYNC ERROR')
           call errexit(1)
      END IF
 
      CALL ufbint(lunit,can,1,255,nlen,'CAT')
      CALL ufbint(lunit,qmn,10,255,nlen,qmstr)
      CALL ufbint(lunit,obn,10,255,nlen,obstr)
      if(subset(:6).eq.'ADPSFC') then
      call ufbint(lunit,obn2,10,255,nlen2,obstr2)
      call ufbint(lunit,obn3,10,255,nlen3,obstr3)
      call ufbint(lunit,obn4,10,255,nlen4,obstr4)
      call ufbint(lunit,obn5,10,255,nlen5,obstr5)
      call ufbint(lunit,obn6,10,255,nlen6,obstr6)
      endif
      if(subset(:6).eq.'GPSIPW') then
      call ufbint(lunit,obn7,10,255,nlen7,obstr7)
      endif
      call ufbint(lunit,obs8,10,255,nlev8,obstr8)
      CALL ufbevn(lunit,tob,2,255,10,nlen,'TOB TPC')

C     PEEL BACK EVENTS TO STORE SENSIBLE TEMP IN CASE TEMP IS VIRTUAL
C     ---------------------------------------------------------------

      DO i = 1, nlen
        DO j = 1, ievn
          IF (tob(2,i,j).eq.8) THEN
            obn(3,i) = tob(1,i,j+1)
            GO TO 30
          ELSE IF (tob(2,i,j).ge.bmiss) THEN
            GO TO 30
          END IF
        END DO
   30   CONTINUE
      END DO
 
C     MERGE OR ADD THE NEW LEVELS TO THE ORIGINAL ONES
C     ------------------------------------------------
 
      DO 40 l2 = 1, nlen
        DO l1 = 1, nlev
          IF (obs(1,l1).eq.obn(1,l2)) THEN
            DO i = 1, 10
              IF (obs(i,l1).ge.bmiss) THEN
                obs(i,l1) = obn(i,l2)
                qms(i,l1) = qmn(i,l2)
              END IF
            END DO
            GO TO 40
          ELSE IF (obn(1,l2).gt.obs(1,l1).or.l1.eq.nlev) THEN
            nlev = nlev + 1
            DO i = 1, 10
              obs(i,nlev) = obn(i,l2)
              qms(i,nlev) = qmn(i,l2)
            END DO
            cat(nlev) = can(l2)
            GO TO 40
          END IF
        END DO
   40   CONTINUE
 
        if(subset(:6).eq.'ADPSFC') then

      DO 50 l2 = 1, nlen2
        DO l1 = 1, nlev2
          IF (obs2(1,l1).eq.obn2(1,l2)) THEN
            DO i = 1, 10
              IF (obs2(i,l1).ge.bmiss) THEN
                obs2(i,l1) = obn2(i,l2)
c               qms(i,l1) = qmn(i,l2)
              END IF
            END DO
            GO TO 50
          ELSE IF (obn2(1,l2).gt.obs2(1,l1).or.l1.eq.nlev2) THEN
            nlev2 = nlev2 + 1
            DO i = 1, 10
              obs2(i,nlev2) = obn2(i,l2)
c             qms(i,nlev2) = qmn(i,l2)
            END DO
            cat(nlev2) = can(l2)
            GO TO 50
          END IF
        END DO
   50   CONTINUE

      DO 60 l2 = 1, nlen3
        DO l1 = 1, nlev3
          IF (obs3(1,l1).eq.obn3(1,l2)) THEN
            DO i = 1, 10
              IF (obs3(i,l1).ge.bmiss) THEN
                obs3(i,l1) = obn3(i,l2)
c               qms(i,l1) = qmn(i,l2)
              END IF
            END DO
            GO TO 60
          ELSE IF (obn3(1,l2).gt.obs3(1,l1).or.l1.eq.nlev3) THEN
            nlev3 = nlev3 + 1
            DO i = 1, 10
              obs3(i,nlev3) = obn3(i,l2)
c             qms(i,nlev2) = qmn(i,l2)
            END DO
            cat(nlev3) = can(l2)
            GO TO 60
          END IF
        END DO
   60   CONTINUE

      DO 70 l2 = 1, nlen4
        DO l1 = 1, nlev4
          IF (obs4(1,l1).eq.obn4(1,l2)) THEN
            DO i = 1, 10
              IF (obs4(i,l1).ge.bmiss) THEN
                obs4(i,l1) = obn4(i,l2)
c               qms(i,l1) = qmn(i,l2)
              END IF
            END DO
            GO TO 70
          ELSE IF (obn4(1,l2).gt.obs4(1,l1).or.l1.eq.nlev4) THEN
            nlev4 = nlev4 + 1
            DO i = 1, 10
              obs4(i,nlev4) = obn4(i,l2)
c             qms(i,nlev2) = qmn(i,l2)
            END DO
            cat(nlev4) = can(l2)
            GO TO 70
          END IF
        END DO
   70   CONTINUE

      DO 80 l2 = 1, nlen5
        DO l1 = 1, nlev5
          IF (obs5(1,l1).eq.obn5(1,l2)) THEN
            DO i = 1, 10
              IF (obs5(i,l1).ge.bmiss) THEN
                obs5(i,l1) = obn5(i,l2)
c               qms(i,l1) = qmn(i,l2)
              END IF
            END DO
            GO TO 80
          ELSE IF (obn5(1,l2).gt.obs5(1,l1).or.l1.eq.nlev5) THEN
            nlev5 = nlev5 + 1
            DO i = 1, 10
              obs5(i,nlev5) = obn5(i,l2)
c             qms(i,nlev2) = qmn(i,l2)
            END DO
            cat(nlev5) = can(l2)
            GO TO 80
          END IF
        END DO
   80   CONTINUE

      DO 91 l2 = 1, nlen6
        DO l1 = 1, nlev6
          IF (obs6(1,l1).eq.obn6(1,l2)) THEN
            DO i = 1, 10
              IF (obs6(i,l1).ge.bmiss) THEN
                obs6(i,l1) = obn6(i,l2)
c               qms(i,l1) = qmn(i,l2)
              END IF
            END DO
            GO TO 91
          ELSE IF (obn6(1,l2).gt.obs6(1,l1).or.l1.eq.nlev6) THEN
            nlev6 = nlev6 + 1
            DO i = 1, 10
              obs6(i,nlev6) = obn6(i,l2)
c             qms(i,nlev2) = qmn(i,l2)
            END DO
            cat(nlev6) = can(l2)
            GO TO 91
          END IF
        END DO
   91   CONTINUE

      endif

      if(subset(:6).eq.'GPSIPW') then

      print*,'nlev7,nlen7=',nlev7,nlen7
      DO 92 l2 = 1, nlen7
        DO l1 = 1, nlev7
          print*,'l1,l2=',l1,l2
          IF (obs7(1,l1).eq.obn7(1,l2)) THEN
            DO i = 1, 10
              IF (obs7(i,l1).ge.bmiss) THEN
                obs7(i,l1) = obn7(i,l2)
c               qms(i,l1) = qmn(i,l2)
              END IF
            END DO
            GO TO 92
          ELSE IF (obn7(1,l2).gt.obs7(1,l1).or.l1.eq.nlev7) THEN
            nlev7 = nlev7 + 1
            DO i = 1, 10
              obs7(i,nlev7) = obn7(i,l2)
c             qms(i,nlev2) = qmn(i,l2)
            END DO
            cat(nlev7) = can(l2)
            GO TO 92
          END IF
        END DO
   92   CONTINUE
      endif

      DO 93 l2 = 1, nlen8
        DO l1 = 1, nlev8
          IF (obs8(1,l1).eq.obn8(1,l2)) THEN
            DO i = 1, 10
              IF (obs8(i,l1).ge.bmiss) THEN
                obs8(i,l1) = obn8(i,l2)
c               qms(i,l1) = qmn(i,l2)
              END IF
            END DO
            GO TO 93
          ELSE IF (obn8(1,l2).gt.obs8(1,l1).or.l1.eq.nlev8) THEN
            nlev8 = nlev8 + 1
            DO i = 1, 10
              obs8(i,nlev8) = obn8(i,l2)
c             qms(i,nlev2) = qmn(i,l2)
            END DO
            cat(nlev8) = can(l2)
            GO TO 93
          END IF
        END DO
   93   CONTINUE


 
C       RETURN WITH A COMBINED REPORT
C       -----------------------------
 
        RETURN
        END
