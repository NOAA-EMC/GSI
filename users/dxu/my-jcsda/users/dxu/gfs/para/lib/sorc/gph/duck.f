      SUBROUTINE DUCK  (IDOT,JDOT,KANG,KFLAG)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:  DUCK          MAKES NOAA LABEL FOR FAX & VARIAN CHARTS.
C   PRGMMR: HENRICHSEN       ORG: WNP12        DATE:1999-08-10
C
C ABSTRACT: MAKES THE NOAA DUCK LABEL FOR FAX AND VARIAN MAPS.
C
C PROGRAM HISTORY LOG:
C   87-06-25  ORIGINAL AUTHOR HENRICHSEN
C   90-01-26  HENRICHSEN ADD KFLAG=12 TO PUT ON NMC BACKUP RUN LABEL
C             INSTEAD OF THE NOAA LABEL.
C   90-12-18  HENRICHSEN  CONVERT TO FORTRAN 77
C   96-02-13  LIN; CONVERT TO CFT-77; CHANGE NMC TO NCEP; WORK WITH NEW
C                  PUTLAB 
C 1999-08-10  HENRICHSEN FIX AN ERROR WHEN KFLAG IS 10. CLEANUP, REWRITE
C             AND DOCUMENT FOR IBM SP. MODIFIED TO USE THE NEW NCEP LOGO
C             FOR KFLAG VALUES OF 0, 3, 4, 5, 6, 7 AND 10.
C
C USAGE:    CALL DUCK  (IDOT,JDOT,KANG,KFLAG)
C   INPUT ARGUMENT LIST:
C     IDOT     - THE I LOCATION OF LOWER LEFT CORNER OF LABEL IN DOTS
C     JDOT     - THE J LOCATION OF LOWER LEFT CORNER OF LABEL IN DOTS
C     KANG     - IS THE ROTATION FLAG, AND HAS VALUES OF 0,1,2 OR 3
C              - WHICH IS FOR ROTATION OF  0.0,90.0,180.0,270.0.
C
C     KFLAG    - IS THE OPTION FLAG, AND HAS VALUES OF 0 THRU 12
C              - 0 PLOT DUCK LABEL, TEXT & NCEP LOGO.
C              - 1 PLOT DUCK LABEL SIZE, 60X60, NO TEXT. 
C              - 2 PLOT ONLY TEXT. FOUNT SIZE 10X12 .
C              - 3 PLOT DUCK LABEL, SMALL TEXT & NCEP LOGO.
C              - 4 PLOT MEDIUM DUCK LABEL, TEXT & NCEP LOGO.
C              - 5 PLOT MEDIUM DUCK LABEL, SMALL TEXT & NCEP LOGO.
C              - 6 PLOT DUCK LABEL, MEDIUM TEXT. SIZE 30X30 & NCEP LOGO.
C              - 7 PLOT MEDIUM DUCK LABEL, MEDIUM TEXT & NCEP LOGO.
C              - 8 PLOT MEDIUM TEXT FOUNT SIZE 7X10.
C              - 9 PLOT MEDIUM DUCK LABEL NO TEXT.
C              -10 PLOT SMALL DUCK LABEL(DUCKLING), MEDIUM TEXT & NCEP LOGO.
C              -11 PLOT SMALL DUCK LABEL(DUCKLING) 28X30 NO TEXT.
C              -12 PLOT NMC BACKUP LABEL INSTEAD OF NOAA LABEL.
C
C   OUTPUT FILES:
C     FT06F001 - PRINT FILE OF MESSAGES FROM PROGRAM.
C     FT55F001 - INTERNAL SCRATCH FILE FOR FAX/VARIAN CHARACTERS
C              - AND LABELS.
C
C REMARKS: ROTATION IS NOT ALLOWED FOR KFLAG = 10, 11 AND 12.
C   THE BIG DUCK LABEL IS 60X60.
C   THE MEDIUM DUCK LABEL IS 30X30.
C   THE SMALL DUCK LABEL(DUCKLING) IS 28X30.
C   THE NCEP LOGO IS 2 TIMES 30X30 OR 30X60.
C   THE STANDARD TEXT SIZE IS 10X12.
C   THE MEDIUM TEXT IS 7X10 . 
C   THE SMALL TEXT IS 6X8 .
C 
C
C ATTRIBUTES:
C   LANGUAGE: IBM FORTRAN 90.
C   MACHINE:  IBM
C
C$$$
C
C
C

      CHARACTER*20 LABEL1
      DATA         LABEL1       /'US DEPT OF COMMERCE '/      

      CHARACTER*25 LABEL2
      DATA         LABEL2       /'NOAA/NWS/NCEP WASHINGTON '/      
      
C    THE NEXT TWO LABEL ARRAYS ARE THE SAME AS THE TWO ABOVE BUT
C    BACKWARDS.

      CHARACTER*20 LABEL3
      DATA         LABEL3       /'ECREMMOC FO TPED SU '/      

      CHARACTER*25 LABEL4
      DATA         LABEL4       /'NOTGNIHSAW PECN/SWN/AAON '/      
      
      CHARACTER*16 LABELB
      DATA         LABELB       /'NCEP BACKUP RUN '/      

      CHARACTER*1 LETERS(13)
      DATA         LETERS       /'M','N','O','P','Q',
     1                           'a','b','c','d','e','f',
     2                           'R','S'/      

      REAL      ANGS(12)
      DATA      ANGS           / 00.0,90.0,180.0,270.0,00.0,
     1                          00.0,00.0,00.0,00.0,00.0,00.0,00.0/      
      REAL      HGTS(5)
      DATA      HGTS           /1.0,4.0,10.0,19.0,11.0/
      
      INTEGER   IADD (66)
      DATA      IADD           /0,30,0,30,92,72,
     1                          30,30,0,0,14,34,
     2                          356,326,356,326,104,74,    
     3                          0,0,30,30,34,14,
     4                          0,0,0,0,62,42,
     5                          0,30,0,30,86,72,
     6                          0,0,0,0,56,42,
     7                          0,0,0,0,14,00,
     8                          0,0,0,0,00,00,
     9                          0,0,0,0,53,39,
     A                          0,0,0,0,00,00/      
      
      INTEGER   IIADD (6)
      INTEGER   INCEP
      
      INTEGER   INUMS(13)
      DATA      INUMS          /1,7,13,19,25,31,37,43,49,55,61,67,73/       
       
      INTEGER   IPRY2
      INTEGER   IPRY(2)
      DATA      IPRY2          /2/      
      
      INTEGER   JADD (66)
      DATA      JADD           /0,00,30,30,34,14,
     1                          0,30,0,30,92,72,
     2                          30,30,0,0,14,34,
     3                          356,326,356,326,104,74,    
     4                          0,0,0,0,17,0,
     5                          0,0,30,30,32,18,
     6                          0,0,0,0,17,3,
     7                          0,0,0,0,14,0,
     8                          0,0,0,0,00,0,
     9                          0,0,0,12,14,0,
     A                          0,0,0,12,00,0/      
      
      INTEGER   JJADD (6)
      INTEGER   JNECP

      LOGICAL   DKLNGM
      LOGICAL   DUKLNG
      LOGICAL   LABELM
      LOGICAL   LDUCK
      LOGICAL   LSMALL
      LOGICAL   LTEXT
      LOGICAL   MDUCK
      LOGICAL   MDUCKM
      LOGICAL   MEDIUM
      LOGICAL   LNCEP
      LOGICAL   ODKLNG
      LOGICAL   OMDUCK
      LOGICAL   STAND
      LOGICAL   STEXT
      

C
C----------------------------------------------------------------
C
              IFLAG = KFLAG
              IANG  = KANG
C
C       CHECK FOR LIMITS OF IFLAG
C       GOOD VALUES 0,1,2,3,4,5,6,7,8,9,10,11,12
C
             IF(IFLAG.LT.0)THEN
               IFLAG = 0
             ELSE IF(IFLAG.GT.12)THEN
              IFLAG = 0
             ENDIF
C
C       CHECK FOR LIMITS OF IANG
C       GOOD VALUES 0,1,2,3
C
             IF(IANG.LT.0)THEN
               IANG = 0
             ELSE IF(IANG.GT.3)THEN
               IANG = 0
             ENDIF
             
             IF(IFLAG.GE.10)IANG = 0

              DKLNGM = .FALSE.
              DUKLNG = .FALSE.
              LABELM = .FALSE.
              LDUCK  = .FALSE.
              LNCEP  = .FALSE.
              LTEXT  = .FALSE.
              LSMALL = .FALSE.
              MDUCKM = .FALSE.
              MEDIUM = .FALSE.
              OMDUCK = .FALSE.
              ODKLNG = .FALSE.
              STAND  = .FALSE.
              STEXT  = .FALSE.

C        SET DEFAULT VALUES!                    
              
             IPRY(1)  = IANG
             IPRY(2)  = IPRY2
             IIDOT  = IDOT
             JJDOT  = JDOT          
             KEY    = IANG + 1
             KEYANG = IANG + 1
             ANGL = ANGS(KEYANG)
             INCEP = 160
             JNCEP = -16 
             NIADD = 30
             NJADD = 0
                          
C       SET FONT SIZE OF TEXT.
                           
             HGT = HGTS(1)
                                       
             IF(IFLAG.EQ.0)THEN
                   STAND  = .TRUE.
                   
                   LNCEP = .TRUE.
                   IF(KEYANG.EQ.1)THEN
                     INCEP = 230
                     JNCEP = -16 
                     NIADD = 30
                     NJADD = 0
                   ELSE IF(KEYANG.EQ.2)THEN
                     INCEP = 0
                     JNCEP = 230
                     NIADD = 0
                     NJADD = 30
                   ELSE IF(KEYANG.EQ.3)THEN
                     INCEP = -70
                     JNCEP = 0
                     NIADD = -30
                     NJADD = 0                  
                   ELSE IF(KEYANG.EQ.4)THEN
                     INCEP = -16
                     JNCEP = -70
                     NIADD = 0
                     NJADD = -30                    
                   ENDIF  
             ELSE IF(IFLAG.EQ.1)THEN
                   LDUCK  = .TRUE.
             ELSE IF(IFLAG.EQ.2)THEN
                   LTEXT  = .TRUE.
             ELSE IF(IFLAG.EQ.3)THEN
                   HGT = HGTS(2)             
                   LSMALL = .TRUE.
                   LNCEP  = .TRUE.
                   IF(KEYANG.EQ.1)THEN
                     INCEP = 144
                     JNCEP = -16 
                     NIADD = 30
                     NJADD = 0
                   ELSE IF(KEYANG.EQ.2)THEN
                     INCEP = 0
                     JNCEP = 144
                     NIADD = 0
                     NJADD = 30
                   ELSE IF(KEYANG.EQ.3)THEN
                     INCEP = -70
                     JNCEP = 0
                     NIADD = -30
                     NJADD = 0                  
                   ELSE IF(KEYANG.EQ.4)THEN
                     INCEP = -16
                     JNCEP = -70
                     NIADD = 0
                     NJADD = -30                    
                   ENDIF
             ELSE IF(IFLAG.EQ.4)THEN 
                   KEY = 5                  
                   DKLNGM = .TRUE.
                   LNCEP  = .TRUE.
                   IF(KEYANG.EQ.1)THEN
                     INCEP = 230
                     JNCEP = -16 
                     NIADD = 30
                     NJADD = 0
                   ELSE IF(KEYANG.EQ.2)THEN
                     INCEP = 0
                     JNCEP = 230
                     NIADD = 0
                     NJADD = 30
                   ELSE IF(KEYANG.EQ.3)THEN
                     INCEP = -70
                     JNCEP = 0
                     NIADD = -30
                     NJADD = 0                  
                   ELSE IF(KEYANG.EQ.4)THEN
                     INCEP = -16
                     JNCEP = -70
                     NIADD = 0
                     NJADD = -30                    
                   ENDIF 
             ELSE IF(IFLAG.EQ.5)THEN
                   HGT = HGTS(2)
                   KEY = 5                   
                   STEXT  = .TRUE.
                   LNCEP  = .TRUE.
                   IF(KEYANG.EQ.1)THEN
                     INCEP = 144
                     JNCEP = -16 
                     NIADD = 30
                     NJADD = 0
                   ELSE IF(KEYANG.EQ.2)THEN
                     INCEP = 0
                     JNCEP = 144
                     NIADD = 0
                     NJADD = 30
                   ELSE IF(KEYANG.EQ.3)THEN
                     INCEP = -54
                     JNCEP = 0
                     NIADD = -30
                     NJADD = 0                  
                   ELSE IF(KEYANG.EQ.4)THEN
                     INCEP = -16
                     JNCEP = -60
                     NIADD = 0
                     NJADD = -30                    
                   ENDIF
             ELSE IF(IFLAG.EQ.6)THEN
                   HGT = HGTS(4)
                   LABELM = .TRUE.
                   LNCEP  = .TRUE.
                   IF(KEYANG.EQ.1)THEN
                     INCEP = 168
                     JNCEP = -16 
                     NIADD = 30
                     NJADD = 0
                   ELSE IF(KEYANG.EQ.2)THEN
                     INCEP = 0
                     JNCEP = 168
                     NIADD = 0
                     NJADD = 30
                   ELSE IF(KEYANG.EQ.3)THEN
                     INCEP = -70
                     JNCEP = 0
                     NIADD = -30
                     NJADD = 0                  
                   ELSE IF(KEYANG.EQ.4)THEN
                     INCEP = -16
                     JNCEP = -70
                     NIADD = 0
                     NJADD = -30                    
                   ENDIF
             ELSE IF(IFLAG.EQ.7)THEN
                   HGT = HGTS(4)
                   KEY = 7  
                   MDUCKM = .TRUE.
                   LNCEP  = .TRUE.
                   IF(KEYANG.EQ.1)THEN
                     INCEP = 168
                     JNCEP = -16 
                     NIADD = 30
                     NJADD = 0
                   ELSE IF(KEYANG.EQ.2)THEN
                     INCEP = 0
                     JNCEP = 168
                     NIADD = 0
                     NJADD = 30
                   ELSE IF(KEYANG.EQ.3)THEN
                     INCEP = -54
                     JNCEP = 0
                     NIADD = -30
                     NJADD = 0                  
                   ELSE IF(KEYANG.EQ.4)THEN
                     INCEP = -16
                     JNCEP = -60
                     NIADD = 0
                     NJADD = -30                    
                   ENDIF
             ELSE IF(IFLAG.EQ.8)THEN
                   HGT = HGTS(4)
                   KEY = 8                   
                   MEDIUM = .TRUE.
             ELSE IF(IFLAG.EQ.9)THEN
                   HGT = HGTS(4)
                   KEY = 9
                   OMDUCK = .TRUE.
             ELSE IF(IFLAG.EQ.10)THEN
                   HGT = HGTS(4)
                   KEY = 10             
                   DUKLNG = .TRUE.
                   LNCEP  = .TRUE.
                     INCEP = 160
                     JNCEP = -16 
                     NIADD = 30
                     NJADD = 0
             ELSE IF(IFLAG.EQ.11)THEN
                   HGT = HGTS(4)
                   KEY = 11
                   ODKLNG = .TRUE.
             ELSE IF(IFLAG.EQ.12)THEN
             
C             KFLAG = 12                   
C             PUT NMC BACKUP LABEL ON THE CHART.

              HGT  = HGTS(5)
 
              WRITE(6,FMT='('' SUB: DUCK 99:210 WILL MAKE BACKUP'',
     1        '' LABEL'',/,''  WITH ROTATION FLAG ='',I2,
     2        '' AND PLOTTING FLAG='',I3)')IANG,IFLAG

              CALL PUTLAB(IIDOT,JJDOT,
     1                 HGT,LABELB,ANGL,15,IPRY,0)
              RETURN
             ENDIF              

C              WHERE KEY IS IANG + 1

               K = INUMS(KEY)
               IK = K
 
C      LOAD ADDATIVE CONSTANTS INTO WORK ARRAYS.
 
             DO  I =1,6
                 IIADD(I) = IADD(IK)
                 JJADD(I) = JADD(IK)
                 IK = IK + 1
             ENDDO

             IEND   = 4
             IFONT  = 3
             INCRMT = 1
             ISTART = 1
             IFIX   = 0
             JFIX   = 0
             JMORE1 = 0
             JMORE2 = 0
             NUMCHR = 1
             
             
          IF(LDUCK)THEN
          
C             KFLAG = 1
C             WILL ONLY MAKE DUCK LABEL.   .
C             SO I MUST SUBTRACT 242 DOTS IF 180 OR 270 ROTATION

              IF(KEY.EQ.3)  IFIX = -242
              IF(KEY.EQ.4)  JFIX = -242
               
          ELSE IF(LTEXT)THEN
          
C             KFLAG = 2
C              WILL ONLY MAKE TEXT LABEL.   .
C              SO I MUST SUBTRACT 72 DOTS IF 0 OR 90 ROTATION

               IF(KEY.EQ.1)THEN
                 IFIX = -72
                 JFIX = -14
               ELSEIF(KEY.EQ.2)THEN
                 IFIX = -14
                 JFIX = -72
               ELSE
                IF(KEY.EQ.3)  JFIX = -14
                IF(KEY.EQ.4)  IFIX = -14
                GO TO 250
               ENDIF               
              GO TO 220 
                                          
          ELSE IF(LSMALL)THEN
          
C             KFLAG = 3 
C             WILL ONLY MAKE LABEL WITH SMALL TEXT
C             SO I MUST MAKE  NEW IIADD AND JJADD TABELS.
 
               IF(KEY.EQ.1)THEN
C               FOR 00 DEG
                 IIADD(5) = 84
               ELSEIF(KEY.EQ.2)THEN
C               FOR 90 DEG               
                 JJADD(5) = 84 
               ELSEIF(KEY.EQ.4)THEN
C               FOR 270 DEG               
                KK = INUMS(KEY)
                KFIX = -92
                DO  I =1,5
                 IF(I.EQ.5)  KFIX = -8
                 JJADD(I) = JJADD(I) + KFIX
                ENDDO                              
               ELSE
C               FOR 180 DEG                    
                 KFIX = -92
                 DO I =1,5
                   IF(I.EQ.5)  KFIX = -8
                   IIADD(I) = IIADD(I) + KFIX
                 ENDDO               
               ENDIF                            
          ELSE IF(LABELM)THEN
          
C             KFLAG = 6
C              WILL MAKE LARGE LABEL WITH MED. TEXT
C             SO I MUST MAKE  NEW IIADD AND JJADD TABELS.
              IF(KEY.EQ.1)THEN

C              00 DEG ROTATION.
               IK = INUMS(6)
                DO I =1,6
                 IIADD(I) = IADD(IK)
                 JJADD(I) = JADD(IK)
                 IK = IK + 1
                ENDDO
              ELSEIF(KEY.EQ.2)THEN

C               90 DEG ROTATION.
                 IIADD(5) = 17
                 IIADD(6) = 34
                 JJADD(5) = 86
                 JJADD(6) = 72
              ELSEIF(KEY.EQ.3)THEN

C               180 DEG ROTATION.
                 IIADD(1) = 203 + 74
                 IIADD(2) = 173 + 74
                 IIADD(3) = 203 + 74
                 IIADD(4) = 173 + 74
                 IIADD(5) = 14 + 74
                 IIADD(6) = 64
                 JJADD(5) = 17
                 JJADD(6) = 34
              
              ELSEIF(KEY.EQ.4)THEN 
 
C             270 DEG ROTATION.
                 IIADD(5) = 34
                 IIADD(6) = 17
                 JJADD(1) = 203 + 74
                 JJADD(2) = 173 + 74
                 JJADD(3) = 203 + 74
                 JJADD(4) = 173 + 74
                 JJADD(5) = 14 + 74
                 JJADD(6) = 64
                            
              ENDIF           
          ELSE IF(STAND)THEN
          
C             KFLAG = 0          
               GO TO 200
               
          ELSE IF(MEDIUM)THEN
          
C             KFLAG = 8          
              GO TO 100
          ELSE IF(STEXT)THEN
          
C             KFLAG = 5  
 
              IEND = 5
              ISTART = 5
              INCRMT = 1

               IF(IANG.EQ.0)THEN
               
C               00 DEG .
               
                 IIADD(5) = 54
                 IIADD(6) = 42                  
                 JJADD(5) = 17 
                 JJADD(6) = 03
                            
               ELSEIF(IANG.EQ.1)THEN 
                
C               90 DEG ROTATION. 

               IIADD(5) = 03
               IIADD(6) = 17
               JJADD(5) = 54
               JJADD(6) = 42
               JMORE1   = 0
               JMORE2   = 0
                               
              ELSEIF(IANG.EQ.2)THEN
              
C             180 DEG ROTATION.

                IIADD(5) = 84
                IIADD(6) = 66
                JJADD(5) = 03
                JJADD(6) = 17             
                IIADD(1) = 150 + 72
                JJADD(1) = 00
 
                                            
              ELSEIF(IANG.EQ.3)THEN
 
C             270 DEG ROTATION.

                IIADD(5) = 17
                IIADD(6) = 03
                JJADD(5) = 84                 
                JJADD(6) = 66                
                IIADD(1) = 00
                JJADD(1) = 150 + 72
             
                
              ENDIF
                          
          ELSE IF(DKLNGM.OR.MDUCKM.OR.OMDUCK)THEN
          
C             KFLAG = 4, 7 OR 9
C             COMES HERE WHEN YOU WANT TO USE THE
C             MEDIUM LABEL INSTEAD OF THE LARGE NOAA DUCK.

 
              IEND = 5
              ISTART = 5
              INCRMT = 1
              IF(OMDUCK)THEN
               GO TO 200
              ELSE

               IF(IANG.GT.0) GO TO 100
              ENDIF            
           
          ELSE IF(DUKLNG.OR.ODKLNG)THEN
          
C             KFLAG = 10 OR 11 
C             COMES HERE WHEN YOU WANT TO USE THE
C             DUCKLING LABEL INSTEAD OF THE LARGE NOAA DUCK.
 
              IEND   = 9
              IFONT  = 1
              INCRMT = 3
              ISTART = 6
              NUMCHR = 3

          ENDIF
         GO TO 200

 100      CONTINUE
C
C        COMES HERE WHEN YOU WANT TO ROTATE WITH MEDIUM DUCK AND OR
C        WITH OPTIONS IFLAG = 4, 7 OR 8.
C
           IF(IANG.EQ.0.AND.IFLAG.EQ.8) GO TO 220
           
           IF(DKLNGM)THEN

C        ROTATE WITH MEDIUM DUCK AND STANDARD TEXT OPTION IFLAG = 4

            IF(IANG.EQ.1)THEN
            
C           90 DEG ROTATION.
               IIADD(5) = 00
               IIADD(6) = 17
               JJADD(5) = 62
               JJADD(6) = 42           
            ELSEIF(IANG.EQ.2)THEN

C           180 DEG ROTATION.
               IIADD(1) = 326
               IIADD(5) = 104
               IIADD(6) = 74
               JJADD(1) = 00
               JJADD(5) = 00
               JJADD(6) = 17
            
            ELSEIF(IANG.EQ.3)THEN

C           270 DEG ROTATION.
               IIADD(1) = 0
               IIADD(5) = 17
               IIADD(6) = 00
               JJADD(1) = 326
               JJADD(5) = 104
               JJADD(6) = 74
           
            ENDIF 
          ELSE

C          ROTATE WITH MEDIUM DUCK WITH MEDIUM TEXT OPTION IFLAG = 7,8
          
           IF(IANG.EQ.1)THEN

C           90 DEG ROTATION.
               IIADD(5) = 03
               IIADD(6) = 17
               JJADD(5) = 62
               JJADD(6) = 42
               JMORE1   = -7
               JMORE2   =  0
               IF(MDUCKM)THEN
               ELSE
                 JJADD(5) = 14
                 JJADD(6) = 00
                  GO TO 220 
               ENDIF          
           ELSEIF(IANG.EQ.2)THEN

C           180 DEG ROTATION.

               IIADD(6) = 00
               JJADD(5) = 03
               JJADD(6) = 17
               IF(MEDIUM)THEN
                GO TO 250
               ELSE
                IIADD(5) = 14 + 60
                IIADD(6) = 60
                IIADD(1) = 173 + 74
                JJADD(1) = 00 
               ENDIF
                         
           ELSEIF(IANG.EQ.3)THEN
 
C           270 DEG ROTATION.
               IIADD(5) = 17
               IIADD(6) = 03
               JJADD(5) = 14
               JJADD(6) = 00
               IF(MEDIUM)THEN
                GO TO 250
               ELSE
                IIADD(1) = 0
                JJADD(1) = 173 + 74
                JJADD(5) = 14 + 60 
                JJADD(6) = 60
               ENDIF           
             
           ENDIF         
          ENDIF 

 200      CONTINUE
C
C        MAKE DUCK LABEL.   .
C        WHICH HAS 4 QUADS.
C
C          WRITE(6,FMT='('' DUCK: 200 LOOP ISTART='',I2,'' IEND='',
C    1        I2,'' INCRMT='',I2,'' IANG='',I2,/,'' KEY='',I2,
C    2        '' IFLAG='',I2,'' IFONT='',I2)')  
C    3        ISTART,IEND,INCRMT,IANG,KEY,IFLAG,IFONT
     
             DO I =ISTART,IEND,INCRMT
C
                  II = I
               IF(KEY.LE.04)GO TO 205
               IF(KEY.EQ.06)GO TO 205
                  II = 1
               IF(KEY.LT.10)GO TO 205
                  II = I - 5
 205           CONTINUE
                 ICOR = IIDOT + IIADD(II) + IFIX
                 JCOR = JJDOT + JJADD(II) + JFIX
C                 
C             WRITE(6,FMT='('' DUCK: INLOOP I='',I2,
C    1          '' ICOR/JCOR='',2I5,/,
C    2          '' II='',I3,'' IIADD(II)='',I4,'' IFIX='',I4,/,
C    3          '' JJADD(II)='',I4,'' JFIX='',I4,/,
C    4          '' NUMCHR='',I2,'' LETERS(I)='',A)')
C    5          I,ICOR,JCOR,II,IIADD(II),IFIX,
C    6          JJADD(II),JFIX,NUMCHR,LETERS(I)(1:1)
  
     
                 CALL PUTLAB(ICOR,JCOR,
     1               HGTS(IFONT),LETERS(I),ANGL,NUMCHR,IPRY,0)
             ENDDO
             
             IF(LDUCK.OR.OMDUCK.OR.ODKLNG) RETURN

             IF(IANG.GE.2)  GO TO 250

             
 220     CONTINUE
 
C         OUTPUT TEXT LABEL FOR ANGLES OF 0 AND 90 .

               ICOR = IIDOT+IIADD(5)+IFIX
                JCOR = JJDOT+JJADD(5)+JFIX+JMORE1

                
           CALL PUTLAB(ICOR,JCOR,
     1                 HGT,LABEL1,ANGL,19,IPRY,0)
              
                ICOR2 = IIDOT+IIADD(6)+IFIX
                JCOR2 = JJDOT+JJADD(6)+JFIX+JMORE2
           CALL PUTLAB(ICOR2,JCOR2,
     1                 HGT,LABEL2,ANGL,24,IPRY,0)                

C              WRITE(6,FMT='('' DUCK: PUTLAB TEXT ARGS:'',/,
C    1        '' HGT='',F6.2,'' ANGL='',F6.2,/,
C    2        '' IIDOT,JJDOT='',2I5,'' IIADD(5),JJADD(5)='',2I5,/,
C    3        '' IIADD(6),JJADD(6)='',2I5,'' IFIX,JFIX='',2I5,/,
C    4        '' JMORE1,JMORE2='',2I5)')       
C    5           HGT,ANGL,IIDOT,JJDOT,IIADD(5),JJADD(5),
C    6           IIADD(6),JJADD(6),IFIX,JFIX,JMORE1,JMORE2
 
          IF(LNCEP) THEN 
           
C           PUT ON THE NCEP LOGO...

                NICOR = ICOR + INCEP
                NJCOR = JCOR + JNCEP

                
C            WRITE(6,FMT='('' DUCK: AT NCEP ,ICOR/JCOR='',2I5,
C    1        '' NICOR='',I5,'' NJCOR='',I5,/,'' INCEP='',I5,
C    2        '' JNCEP='',I5,'' LETERS(12)='',A)')
C    3          ICOR,JCOR,NICOR,NJCOR,INCEP,JNCEP,LETERS(12)                 
    
     
             CALL PUTLAB(NICOR,NJCOR,
     1                 HGTS(3),LETERS(12),ANGL,1,IPRY,0)
                NICOR=NICOR + NIADD
                NJCOR=NJCOR + NJADD
             CALL PUTLAB(NICOR,NJCOR,
     1                 HGTS(3),LETERS(13),ANGL,1,IPRY,0) 
     
C            WRITE(6,FMT='('' DUCK:  AT NCEP '',
C    1        '' NICOR='',I5,'' NJCOR='',I5,/,'' NIADD='',I5,
C    2        '' NJADD='',I5,'' LETERS(13)='',A)')
C    3          NICOR,NJCOR,NIADD,NJADD,LETERS(13)
          ENDIF         


               RETURN
 250     CONTINUE

C   COMES HERE WHEN ROTATING 180 AND 270 AND USES BACKWARDS TEXT STRING

                ICOR = IIDOT+IIADD(5)+IFIX
                JCOR = JJDOT+JJADD(5)+JFIX                           
           CALL PUTLAB(ICOR,JCOR,
     1                 HGT,LABEL3,ANGL,19,IPRY,0)
                ICOR2 = IIDOT+IIADD(6)+IFIX
                JCOR2 = JJDOT+JJADD(6)+JFIX
           CALL PUTLAB(ICOR2,JCOR2,
     1                 HGT,LABEL4,ANGL,24,IPRY,0)


C              WRITE(6,FMT='('' DUCK: PUTLAB TEXT ARGS:'',/,
C     1        '' HGT='',F6.2,'' ANGL='',F6.2,/,
C     2        '' IIDOT,JJDOT='',2I5,'' IIADD(5),JJADD(5)='',2I5,/,
C     3        '' IIADD(6),JJADD(6)='',2I5,'' IFIX,JFIX='',2I5)')       
C     4           HGT,ANGL,IIDOT,JJDOT,IIADD(5),JJADD(5),   
C     5           IIADD(6),JJADD(6),IFIX,JFIX
     
          IF(LNCEP) THEN 
           
C           PUT ON THE NCEP LOGO...

                NICOR = ICOR + INCEP
                NJCOR = JCOR + JNCEP
                
                 
C            WRITE(6,FMT='('' DUCK: BACK AT NCEP ,ICOR/JCOR='',2I5,
C     1        '' NICOR='',I5,'' NJCOR='',I5,/,'' INCEP='',I5,
C     2        '' JNCEP='',I5,'' LETERS(12)='',A)')
C     3          ICOR,JCOR,NICOR,NJCOR,INCEP,JNCEP,LETERS(12)
                 
            CALL PUTLAB(NICOR,NJCOR,
     1                 HGTS(3),LETERS(12),ANGL,1,IPRY,0)
                NICOR=NICOR + NIADD
                NJCOR=NJCOR + NJADD
                
C            WRITE(6,FMT='('' DUCK: BACK AT NCEP '',
C     1        '' NICOR='',I5,'' NJCOR='',I5,/,'' NIADD='',I5,
C     2        '' NJADD='',I5,'' LETERS(13)='',A)')
C     3          NICOR,NJCOR,NIADD,NJADD,LETERS(13)
     
            CALL PUTLAB(NICOR,NJCOR,
     1                 HGTS(3),LETERS(13),ANGL,1,IPRY,0)
     
          ENDIF
    
      RETURN
      END
