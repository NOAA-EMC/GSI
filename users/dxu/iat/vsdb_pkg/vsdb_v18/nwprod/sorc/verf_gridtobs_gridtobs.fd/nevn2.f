C----------------------------------------------------------------------
C----------------------------------------------------------------------
      FUNCTION nevn2(node,lun,inv1,inv2,i1,i2,i3,user)
 
      COMMON /usrint/ nval(32), inv(15000,32), val(15000,32)
 
      DIMENSION user(i1,i2,i3)
      REAL*8 val, user
 
      DATA bmiss /10E10/
 
C----------------------------------------------------------------------
C----------------------------------------------------------------------
 
      nevn2 = 0
 
C     FIND THE ENCLOSING EVENT STACK DESCRIPTOR
C     -----------------------------------------
 
      ndrs = lstjpb(node,lun,'DRS')
      IF (ndrs.le.0) RETURN
 
      invn = invwin(ndrs,lun,inv1,inv2)
      IF (invn.gt.0) THEN
 
        nevn2 = val(invn,lun)
        IF (nevn2.gt.i3) GO TO 10
 
C       SEARCH EACH STACK LEVEL FOR THE REQUESTED NODE AND COPY THE VALUE
C       -----------------------------------------------------------------
 
        n2 = invn + 1

c       print*,'nevn2=',nevn2
        DO l = 1, nevn2
          n1 = n2
          n2 = n2 + val(n1,lun)
c         print*,'n1,n2=',n1,n2
          DO n = n1, n2
c         if(l.eq.3) then
c         print*,'node,inv(n,lun)=',node,inv(n,lun)
c         endif
            IF (inv(n,lun).eq.node) then
              user(1,1,l) = val(n,lun)
c             if(l.eq.3) then
              print*,'n,l=',n,l
c             print*,'val(n,lun)=',val(n,lun)
              print*,'user(1,1,l),l=',user(1,1,l),l
c             endif
            endif
          END DO
        END DO
 
        RETURN
      END IF
      print*,'NEVN - CANT FIND THE EVENT STACK!!!!!!'
      CALL abort
   10 print*,'NEVN - TOO MANY EVENTS FOR USER ARRAY!' 
      CALL abort
      END
