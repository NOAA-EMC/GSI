       SUBROUTINE ASC2CDC(NCHAR,C1ASCII,C1EXTISP,IERR)                          
C                                                                               
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    ASC2CDC     TRANSLATE ASCII INTO CDC DISPLAY CODE              
C   PRGMMR: KRISHNA KUMAR       ORG: W/NP12     DATE: 1999-07-01                     
C                                                                               
C ABSTRACT: TO TRANSLATE AN ASCII ARRAY INTO CDC EXTENDED DISPLAY CODE          
C   (IN A CHARACTER*1 ARRAY).                                                   
C   WHERE THE "EXTENDED" DISPLAY CODE IS CDC DISPLAY CODE WHICH IS              
C   EXTENDED TO FILL AN 8-BIT BYTE.  THIS EXTENDED DISPLAY CODE                 
C   IS FOUND IN THE IFID OF OUR FAX PRODUCTS.                                   
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   86-03-06  ORIGINAL AUTHOR: DAVID SHIMOMURA                                  
C             OF INTERGRAPH VAX VERSION (NAMED TRT2DCD)                         
C   96-04-04  SHIMOMURA --  CONVERTED TO CRAY                                    
C   96-04-05  HENRICHSEN    CLEANED UP AND CORRECTED AN ERROR IN DO LOOP.          
C   97-02-05  LUKE LIN      FIX THE TRANSLATION TABLE
C 1999-07-01  KRISHNA KUMAR CONVERTED FROM CRAY VERSION TO IBM RS/6000.
C                                                                               
C USAGE:    CALL ASC2CDC(NCHAR,C1ASCII,C1EXTISP,IERR)                           
C   INPUT ARGUMENT LIST:                                                        
C     C*1 C1ASCII(NCHAR) - SOURCE CHARACTER*1 ARRAY IN ASCII                    
C                                                                               
C   OUTPUT ARGUMENT LIST:                                                       
C     C*1 C1EXTISP(NCHAR) - DESTINATION CHARACTER*1 ARRAY                       
C                             IN EXTENDED DISPLAY CODE                          
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
C     THIS IS THE CONVERSE OF SUBR ISP2ASC().                                   
                                                                                
C     CAUTION:  NO BOUNDS CHECKING ON DESTINATION ARRAY SIZE, SO                
C               IF YOU DO NOT HAVE NCHAR-BYTES ALLOCATED FOR THE                
C               DESTINATION ARRAY, THEN THIS WILL WRITE ALL OVER                
C               YOUR CODE.                                                      
                                                                                
C     CAUTION:  THIS TRANSLATES THE COMMONLY USED ALPHANUMERICS                 
C               BUT NOT ALL THE OTHER SYMBOLS OF CDC DISPLAY CODE.              
C               THIS IS FOR USE IN TRANSLATING THE IFID OF OUR                  
C               FAX PRODUCTS.                                                   
                                                                                
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
C      ... TO TRANSLATE TEXT FROM ASCII INTO CDC DISPLAY CODE                   
C      ...   BUT IN EXTENDED DISPLAY CODE OF 8-BITS PER CHAR.                   
C                                                                               
       CHARACTER*1   C1ASCII(NCHAR)                                             
       CHARACTER*1   C1EXTISP(NCHAR)                                            
       INTEGER       IERR                                                       
C                                                                               
C                                                                               
C                                                                               
       INTEGER*8    KAS2ISPTB(32)   
       CHARACTER*1  CAS2ISPTB(256)                                              
C      ... WHERE CAS2ISPTB IS TRANSLATE TABLE OF CDC DISPLAY CODE               
C      ...   FOR EVERY ASCII POSITION                                           
       EQUIVALENCE  (KAS2ISPTB(1),CAS2ISPTB(1))                                 
C                                                                               
C                                                                               
       DATA     KAS2ISPTB /                                                     
     1      4*X'2D2D2D2D2D2D2D2D',                                              
     2        X'2D2D2D2D2B2D2D2D',    X'292A27252E262F28',                      
C??  A        X'1B1C1D1E1F202122',    X'2324333F3A2C3B2D',                      
     A        X'1B1C1D1E1F202122',    X'23242D3F3A2C3B2D',                      
C??  3        X'2D01020304050607',    X'09080A0B0C0D0E0F',                      
     3        X'2D01020304050607',    X'08090A0B0C0D0E0F',                      
C??  B        X'1011121314151617',    X'18191A2D2D2D3E2D',                      
     B        X'1011121314151617',    X'18191A2D2D2D2D2D',                      
     4        X'2D01020304050607',    X'08090A0B0C0D0E0F',                      
     C        X'1011121314151617',    X'18191A2D7C7D7E7F',                      
     5     14*X'2D2D2D2D2D2D2D2D',                                              
     8        X'2D2D2D2D2D2D2D2D',    X'2D2D2D2DFCFDFEFF' /                     
C                                                                               
C                                                                               
C      ... VARIATIONS FROM STD ASCII TO CDC DISPLAY CODE:                       
C      ... SMALL ALPHA IS TRNSLATED INTO CAPITAL LTRS;                          
C      ... 7C 7D 7E 7F AND  FC FD FE FF  ARE PASSED THRU UNCHANGED              
C                                                                               
C                                                                               
C      . . . .   S T A R T    . . . . . . . . . . . . . . . . . . . .           
C                                                                               
       IERR = 0                                                                 
C                                                                               
       NCH2DO = NCHAR                                                           
       IF(NCH2DO .LE. 0) THEN                                                   
         WRITE(6,FMT='(1H ,''ASC2CDC: ERROR! YOU GAVE ME A'',                   
     1       '' BAD CHAR COUNT. NCHAR='',I8)')NCHAR                             
         IERR = 1                                                               
       ELSE                                                                     
        DO  LA = 1,NCH2DO                                                       
         INX = 1 + MOVA2I(C1ASCII(LA))                                           
         C1EXTISP(LA) = CAS2ISPTB(INX)                                          
        ENDDO                                                                   
       ENDIF                                                                    
       RETURN                                                                   
       END                                                                      
