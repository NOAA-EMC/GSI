      SUBROUTINE adddate(iy,im,id,ih,jh,idate)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    ADDDATE    UPDATES INPUT DATE BY HOURS TO OUTPUT DATE
C   PRGMMR: KATZ,B           ORG: W/NMC23    DATE: 96-09-03             
C                                                                       
C ABSTRACT: TAKES 8-DIGIT INPUT YEAR, MONTH, DAY, AND HOUR AND
C   A NUMBER OF HOURS TO UPDATE IT.  RETURNS AN UPDATED 8-DIGIT
C   YEAR, MONTH, DAY, AND HOUR.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-09-03  KATZ                                                 
C   97-06-16  ROGERS MODIFIED BERT'S CODE TO DEAL WITH 4 2-DIGIT DATE 
C                                                                       
C USAGE:    CALL ADDDATE(IY,IM,ID,IH,JH)
C   INPUT ARGUMENT LIST:                                               
C     IY       - INPUT YEAR 
C     IM       - INPUT MONTH
C     ID       - INPUT DAY 
C     IH       - INPUT HOUR
C     JH       - NUMBER OF HOURS TO UPDATE OR BACKDATE
C
C   OUTPUT ARGUMENT LIST:                                               
C     none     - IY,IM,ID & IH ARE MODIFIED PRIOR TO RETURN
C
C REMARKS:                                                              
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 77                                                
C   MACHINE:  CRAY                                                     
C                                                                       
C$$$                                                                    

      DIMENSION mon(12)

      DATA mon /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      PRINT *, ' start date= ', iy, im, id, ih

      ih = ih + jh

      IF (mod(iy,4).ne.0) mon(2) = 28
      IF (mod(iy,4).eq.0) mon(2) = 29

   10 IF (ih.lt.0) THEN
        ih = ih + 24
        id = id - 1
        IF (id.eq.0) THEN
          im = im - 1
          IF (im.eq.0) THEN
            im = 12
            iy = iy - 1
            IF (iy.lt.0) iy = 99
          END IF
          id = mon(im)
        END IF
        GO TO 10
      ELSE IF (ih.ge.24) THEN
        ih = ih - 24
        id = id + 1
        IF (id.gt.mon(im)) THEN
          id = 1
          im = im + 1
          IF (im.gt.12) THEN
            im = 1
            iy = mod(iy+1,100)
          END IF
        END IF
        GO TO 10
      END IF

      PRINT *, ' valid date= ', iy, im, id, ih

      RETURN
      END
