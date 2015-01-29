      SUBROUTINE W3FS23(IMON,IDAY,MON1,MON2,COEF1,COEF2,IERR)                   
C$$$  SUBROUTINE DOCUMENTATION BLOCK  ***                                       
C                                                                               
C SUBR:  W3FS23    WEIGHTING COEFFICIENTS FOR INTERPOLATION                     
C   PRGMMR: HENRICHSEN       ORG: NMC412      DATE:95-11-09                     
C                                                                               
C ABSTRACT: GIVEN A SPECIFIC DATE (IMON AND IDAY), SUB                          
C   W3FS23 RETURNS THE WEIGHTING COEFFICIENTS AND THE NUMBER OF THE             
C   TWO MONTHS NEEDED TO LINEARLY INTERPOLATE A DAILY NORMAL FROM A             
C   FILE OF MONTHLY NORMALS. THE MONTHLY NORMALS ARE PRESUMED TO BE             
C   VALID FOR THE 15TH OF THE MONTH.                                            
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   84-06-21  ORIGIONAL AUTHOR NORCROSS                                         
C   95-11-09  HENRICHSEN CONVERT TO RUN ON CRAY                                 
C                                                                               
C             AFTER EXECUTION OF SUB W3FS23 THE DAILY NORM                      
C   CAN BE CALCULATED AS FOLLOWS:                                               
C                                                                               
C     DAILY NORM = COEF1*(VALUE FOR MON1) + COEF2*(VALUE FOR MON2)              
C                                                                               
C USAGE: CALL W3FS23(IMON,IDAY,MON1,MON2,COEF1,COEF2,IERR)                      
C                                                                               
C   INPUT ARGUMENTS:                                                            
C     IMON   -  MONTH FOR WHICH NORM IS NEEDED                                  
C     IDAY   -  DAY OF MONTH FOR WHICH NORM IS NEEDED                           
C                                                                               
C   OUTPUT ARGUMENTS:                                                           
C     MON1   -  FIRST MONTH NEEDED FOR INTERPOLATION                            
C     MON2   -  SECOND MONTH NEEDED FOR INTERPOLATION                           
C     COEF1  -  FRACTIONAL PART OF THE FIRST MONTHLY VALUE                      
C     COEF2  -  FRACTIONAL PART OF THE SECOND MONTHLY VALUE                     
C     IERR   -  ERROR FLAG GIVING RETURN STATUS                                 
C                                                                               
C     ALL ARGUMENTS ARE INTEGER*4 EXCEPT COEF1 AND COEF2 WHICH                  
C     ARE REAL.                                                                 
C                                                                               
C   RETURN CONDITIONS:                                                          
C     IERR =   0  -  NORMAL RETURN                                              
C          = 100  -  VALUE FOR MONTH IS OUT OF RANGE                            
C          = 200  -  VALUE FOR IDAY IS OUT OF RANGE                             
C          = 300  -  BOTH MONTH AND DAY ARE OUT OF RANGE                        
C                                                                               
C   SUBPROGRAMS CALLED:  NONE                                                   
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: CRAY FORTRAN 77                                                   
C   MACHINE:  CRAY                                                              
C                                                                               
C$$$                                                                            
C                                                                               
         INTEGER    IMONDS(12)                                                  
         INTEGER    L1, L2                                                      
C                                                                               
         DATA  IMONDS/31,28,31,30,31,30,31,31,30,31,30,31/                      
C                                                                               
C****  SET VALUES FOR ERROR RETURN AND FOR THE 15TH OF THE MONTH                
C                                                                               
         IERR = 0                                                               
         MON1 = IMON                                                            
         MON2 = IMON                                                            
         COEF1 = 1.0                                                            
         COEF2 = 0.0                                                            
C                                                                               
C****  VALIDATE DATE, ALLOW FOR LEAP YEAR                                       
C                                                                               
         IMONDS(2) = 28                                                         
         IF (IMON.GT.12 .OR. IMON.LT.1)IERR = 100                               
         IF (IMON.EQ.2 .AND. IDAY.EQ.29) IMONDS(2)=29                           
         IF (IDAY.GT.IMONDS(IMON) .OR. IDAY.LT.1) IERR=IERR+ 200                
         IF (IERR.NE.0)THEN                                                     
C                                                                               
C            ERROR RETURN TO CALLING PROGRAM                                    
C                                                                               
         ELSE                                                                   
C                                                                               
C           DETERMINE POSITION OF IDAY IN THE MONTH                             
C             RELATIVE TO THE 15TH                                              
C                                                                               
           IF (IDAY.EQ.15) THEN                                                 
C                                                                               
C            RETURN TO CALLING PROGRAM                                          
C                                                                               
           ELSE                                                                 
             IF (IDAY.LT.15) THEN                                               
C                                                                               
C             IN FIRST HALF OF MONTH,                                           
C             INTERPOLATION USES PREVIOUS MONTH                                 
C                                                                               
              MON1 = IMON - 1                                                   
              IF (MON1.EQ.0) MON1=12                                            
C                                                                               
              L1 = (IMONDS(MON1) - 15) + IDAY                                   
              L2 = 15 - IDAY                                                    
             ELSE                                                               
C                                                                               
C             IN LAST HALF OF MONTH, INTERPOLATION USES VALUE FROM              
C              FOLLOWING MONTH                                                  
C                                                                               
              MON2 = IMON + 1                                                   
              IF (MON2.EQ.13) MON2=1                                            
C                                                                               
              L1 = IDAY - 15                                                    
              L2 = (IMONDS(MON1) - IDAY) + 15                                   
             ENDIF                                                              
C                                                                               
C           NOW CALCULATE WEIGHTING COEFFICIENTS                                
C                                                                               
              F1 = FLOAT(L1)                                                    
              F2 = FLOAT(L2)                                                    
              COEF1 = F2 / (F1 + F2)                                            
              COEF2 = F1 / (F1 + F2)                                            
           ENDIF                                                                
         ENDIF                                                                  
         RETURN                                                                 
         END                                                                    
