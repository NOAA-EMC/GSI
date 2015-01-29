       SUBROUTINE CDC2ASC(NCHAR,C1EXTISP,C1ASCII,IERR)                          
C                                                                               
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    CDC2ASC     TRANSLATE CDC DISPLAY CODE TO ASCII                
C   PRGMMR: KRISHNA KUMAR       ORG: W/NP12     DATE: 1999-07-01                     
C                                                                               
C ABSTRACT: TO TRANSLATE FROM CDC EXTENDED DISPLAY CODE                         
C   (IN A CHARACTER*1 ARRAY) INTO AN ASCII ARRAY.                               
C   WHERE THE "EXTENDED" DISPLAY CODE IS CDC DISPLAY CODE WHICH HAS             
C   BEEN EXTENDED TO FILL AN 8-BIT BYTE.  THIS EXTENDED DISPLAY CODE            
C   IS FOUND IN THE IFID OF OUR FAX PRODUCTS.                                   
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   YY-MM-DD  ORIGINAL AUTHOR: DAVID SHIMOMURA                                  
C   90-03-19  SHIMOMURA   -- CONVERTED TO INTERGRAPH VAX (NAMED CDC2ASC)          
C   96-04-04  SHIMOMURA   -- CONVERTED TO CRAY                                    
C   96-04-05  HENRICHSEN  -- CLEANED UP AND CORRECTED AN ERROR IN DO LOOP.          
C 1999-07-01  KRISHNA KUMAR -- CONVERTED FROM CRAY TO IBM RS/6000.
C                                                                               
C USAGE:    CALL CDC2ASC(NCHAR,C1EXTISP,C1ASCII,IERR)                           
C   INPUT ARGUMENT LIST:                                                        
C     C*1 C1EXTISP(NCHAR) - SOURCE CHARACTER*1 ARRAY                            
C                             IN EXTENDED DISPLAY CODE                          
C                                                                               
C   OUTPUT ARGUMENT LIST:                                                       
C     C*1 C1ASCII(NCHAR) - DESTINATION CHARACTER*1 ARRAY IN ASCII               
C     INT IERR - ERRFLAG                                                        
C              = 0;  NORMAL RETURN                                              
C              = 1;  BAD VALUE GIVEN IN NCHAR                                   
C                                                                               
C   OUTPUT FILES:                                                               
C     FT06F001 - INCLUDE IF ANY PRINTOUT                                        
C                IF GIVEN VALUE OF NCHAR IS ZERO, THEN                          
C                      ERROR MESSAGE IS PRINTED                                 
C                                                                               
C REMARKS:                                                                      
C     THIS IS THE CONVERSE OF SUBR ASC2ISP().                                   
C                                                                               
C     CAUTION:  NO BOUNDS CHECKING ON DESTINATION ARRAY SIZE, SO                
C               IF YOU DO NOT HAVE NCHAR-BYTES ALLOCATED FOR THE                
C               DESTINATION ARRAY, THEN THIS WILL WRITE ALL OVER                
C               YOUR CODE.                                                      
C                                                                               
C     CAUTION:  THIS TRANSLATES THE COMMONLY USED ALPHANUMERICS                 
C               BUT NOT ALL THE OTHER SYMBOLS OF CDC DISPLAY CODE.              
C               THIS IS FOR USE IN TRANSLATING THE IFID OF OUR                  
C               FAX PRODUCTS.                                                   
C                                                                               
C     CAUTION:  IF YOU PRINT THE RESULTING ASCII STRING WITH FORMAT (A),        
C                  THEN THE X'7F' WILL NOT SHOW IN YOUR PRINTOUT                
C                  EVEN THOUGH IT IS THERE.                                     
C                                                                               
C      ... VARIATIONS FROM CDC DISPLAY CODE TO ASCII :                          
C      ... X'7C',X'7D',X'7E',X'7F' ARE PASSED THRU UNCHANGED;                   
C      ... X'FC',X'FD',X'FE',X'FF' ARE PASSED THRU UNCHANGED;                   
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: F90                                                        
C   MACHINE:  IBM                                                              
C                                                                               
C$$$                                                                            
C                                                                               
C                                                                               
       CHARACTER*1   C1EXTISP(NCHAR)                                            
       CHARACTER*1   C1ASCII(NCHAR)                                             
       INTEGER       IERR                                                       
C                                                                               
C                                                                               
       CHARACTER*1   CISP2ASTB(256)                                             
       INTEGER       KISP2ASTB(32)                                              
C      ... WHERE CISP2ASTB IS TRANSLATE TABLE OF ASCII                          
C      ...   FOR EACH CDC DISPLAY CODE POSITION THAT WE USE IN IFIDS            
C      ...   (I HAD TO MAKE IT TWICE AS BIG TO REACH THOSE SPECIAL              
C      ...   TRANSLATIONS WE USED TO MAKE THE 'FF'S FROM ASC2ISP()              
C      ...   TO MAKE IT THE CONVERSE.)                                          
C                                                                               
       EQUIVALENCE  (KISP2ASTB(1),CISP2ASTB(1))                                 
C                                                                               
C                                                                               
       DATA     KISP2ASTB /                                                     
     A      X'0041424344454647', X'48494A4B4C4D4E4F',                           
     1      X'5051525354555657', X'58595A3031323334',                           
     2      X'35363738392B2D2A', X'2F2829243D202C2E',                           
     3      X'3F5B5D3A3F3F2126', X'2020202020202020',                           
     4      X'2020202020202020', X'2020202020202020',                           
     5      X'2020202020202020', X'2020202020202020',                           
     6      X'2020202020202020', X'2020202020202020',                           
     7      X'2020202020202020', X'202020207C7D7E7F',                           
     8      14*X'2020202020202020',                                             
     F      X'2020202020202020', X'20202020FCFDFEFF' /                          
C                                                                               
C                                                                               
C      . . . .   S T A R T    . . . . . . . . . . . . . . . . . . . .           
C                                                                               
       IERR = 0                                                                 
C                                                                               
       NCH2DO = NCHAR                                                           
       IF(NCH2DO .LE. 0) THEN                                                   
         WRITE(6,FMT='(1H ,''CDC2ASC: ERROR! YOU GAVE ME A'',                   
     1       '' BAD CHAR COUNT. NCHAR='',I8)')NCHAR                             
         IERR = 1                                                               
       ELSE                                                                     
        DO  LA = 1,NCH2DO                                                       
          INX = 1 + MOVA2I(C1EXTISP(LA))
          C1ASCII(LA) = CISP2ASTB(INX)                                          
        ENDDO                                                                   
       ENDIF                                                                    
       RETURN                                                                   
       END                                                                      
