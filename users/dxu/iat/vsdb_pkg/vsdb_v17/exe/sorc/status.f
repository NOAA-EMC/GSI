C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE status(lunit,lun,il,im)
                                                                        
      COMMON /stbfr/ iolun(10), iomsg(10)
                                                                        
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
                                                                        
      IF (lunit.gt.0.and.lunit.le.99) THEN
                                                                        
C       CLEAR THE STATUS INDICATORS                                          
C       ---------------------------                                          
                                                                        
        lun = 0
        il = 0
        im = 0
                                                                        
C       SEE IF THE UNIT IS DEFINED                                           
C       --------------------------                                           
                                                                        
        DO i = 1, 10
          IF (abs(iolun(i)).eq.lunit) lun = i
        END DO
                                                                        
C       IF NOT, CHECK FOR FILE SPACE - RETURN LUN=0 IF NO FILE SPACE         
C       ------------------------------------------------------------         
                                                                        
        IF (lun.eq.0) THEN
          DO i = 1, 10
            IF (iolun(i).eq.0) lun = i
            IF (iolun(i).eq.0) RETURN
          END DO
          RETURN
        END IF
                                                                        
C       IF FILE DEFINED RETURN STATUSES                                      
C       -------------------------------                                      
                                                                        
        il = sign(1,iolun(lun))
        im = iomsg(lun)
                                                                        
        RETURN
      END IF
      CALL bort('STATUS - ILLEGAL UNIT GIVEN')
      END
