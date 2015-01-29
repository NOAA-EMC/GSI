C----------------------------------------------------------------------
C----------------------------------------------------------------------
      SUBROUTINE ufbin3a(lunit,usr,i1,i2,i3,iret,jret,str)
 
      COMMON /msgcwd/ nmsg(32), nsub(32), msub(32), inode(32), 
     +            idate(32)
      COMMON /usrint/ nval(32), inv(15000,32), val(15000,32)
      COMMON /usrstr/ nnod, ncon, nods(20), nodc(32), vals(32), 
     +            kons(32)
 
      CHARACTER*(*) str
      DIMENSION usr(i1,i2,i3)
      REAL*8 val, usr
 
      DATA bmiss /10E10/
 
C----------------------------------------------------------------------
C----------------------------------------------------------------------
 
C  CHECK THE FILE STATUS AND I-NODE
C  --------------------------------
 
      CALL status(lunit,lun,il,im)
      IF (il.ne.0) THEN
        IF (im.eq.0) GO TO 30
        IF (inode(lun).ne.inv(1,lun)) GO TO 40
 
C       PARSE THE INPUT STRING
C       ----------------------
 
        CALL string(str,lun,i1,0)
 
C       SET INITIAL VALUES FOR RETURNING ARGUMENTS
C       ------------------------------------------
 
        DO k = 1, i3
          DO j = 1, i2
            DO i = 1, i1
              usr(i,j,k) = bmiss
            END DO
          END DO
        END DO
 
        iret = 0
        jret = 0
 
C       LOOP OVER COND WINDOWS
C       ----------------------
 
        inc1 = 1
        inc2 = 1
 
   10   CALL conwin(lun,inc1,inc2,i2)
        IF (nnod.eq.0) THEN
          iret = i2
          RETURN
        ELSE IF (inc1.eq.0) THEN
          RETURN
        ELSE
          DO i = 1, nnod
            IF (nods(i).gt.0) THEN
              ins2 = inc1
              CALL getwin(nods(i),lun,ins1,ins2)
              IF (ins1.eq.0) RETURN
              GO TO 20
            END IF
          END DO
          ins1 = inc1
          ins2 = inc2
        END IF
 
C       READ PUSH DOWN STACK DATA INTO 3D ARRAYS
C       ----------------------------------------
 
   20   iret = iret + 1
c       print*,'iret,i2=',iret,i2
        IF (iret.le.i2) THEN
          DO i = 1, nnod
            do j=1,i3
            if(i.eq.3.and.j.le.4) then
            nnvn = nevn2(nods(i),lun,ins1,ins2,i1,i2,i3,usr(i,iret,1))
            print*,'i,iret,j=',i,iret,j
            if(iret.eq.2) then
            print*,'usr(i,iret,j),j=',usr(i,iret,j),j
            endif
            else
            nnvn = nevn(nods(i),lun,ins1,ins2,i1,i2,i3,usr(i,iret,1))
            endif
            enddo
            jret = max(jret,nnvn)
c           print*,'jret,nnvn=',jret,nnvn
          END DO
        END IF
 
C       DECIDE WHAT TO DO NEXT
C       ----------------------
 
        CALL nxtwin(lun,ins1,ins2)
        IF (ins1.gt.0.and.ins1.lt.inc2) GO TO 20
        IF (ncon.gt.0) GO TO 10
 
        RETURN
      END IF
      print*,'UFBIN3 - FILE IS CLOSED'
      CALL abort
   30 print*,'UFBIN3 - NO MESSAGE OPEN'
      CALL abort
   40 print*,'UFBIN3 - I-NODE MISMATCH'
      CALL abort
      END
