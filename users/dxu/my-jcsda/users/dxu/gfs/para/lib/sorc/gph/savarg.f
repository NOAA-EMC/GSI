      SUBROUTINE SAVARG(IPT,JPT,HEIGHT,TEXT,ANGLE,                              
     1                  NCHAR,IPRIOR,ITAG,KSAVUN)                               
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    SAVARG      WRITES THE PUTLAB ARGS INTO KSAVUN.                
C   PRGMMR: HENRICHSEN       ORG: W/NMC41    DATE: 95-11-07                     
C                .      .    .                                       .          
C ABSTRACT: WRITES THE PUTLAB ARGS INTO A TEMPORARILY DISK FILE,                
C   DSRN=KSAVUN.                                                                
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   YY-MM-DD  ORIGINAL AUTHOR SHIMOMURA                                         
C   89-07-24  HENRICHSEN DOCUMENT.                                              
C   95-11-07  HENRICHSEN MODIFY TO RUN ON CRAY                                  
C                                                                               
C USAGE     CALL SAVARG(IPT,JPT,HEIGHT,TEXT,ANGLE,                              
C    1                  NCHAR,IPRIOR,ITAG,KSAVUN)                               
C   INPUT ARGUMENT LIST:                                                        
C     IPT      - INTEGER ICOORDINATE FOR PUTLAB                                 
C     JPT      - INTEGER JCOORDINATE FOR PUTLAB                                 
C     HEIGHT   - REAL   HEIGHT OF CHARACTER FOR PUTLAB.                         
C     TEXT     - CHARACTER*1 ARRAY OF SIZE NCHAR THAT HOLDS TEXT.               
C     ANGLE    - REAL ROTATION ANGLE ARG FOR PUTLAB.                            
C     NCHAR    - INTEGER NUMBER OF CHARACTERS IN TEXT ARRAY.                    
C     IPRIOR   - INTEGER TWO WORD ARAY (PRIORITY FLAGS FOR PUTLAB).             
C     ITAG     - INTEGER ARG FOR PUTLAB.                                        
C     KSAVUN   - INTEGER UNIT NUMBER OF FILE TO WRITE ARG TO.                   
C                                                                               
C                                                                               
C   OUTPUT FILES:                                                               
C     FT06F001 - PRINT FILE.                                                    
C     FTXXF001 - FILE TO WRITE ARGS WHERE XX = KSAVUN.                          
C                                                                               
C REMARKS: A CRUTCH TO TEMPORARILY SAVE THE ARGUMENTS TO PUTLAB                 
C   IN A DISK FILE WHOSE DSRN IS SPECIFIED BY KSAVUN.                           
C   USES UNFORMATTED WRITE WITH LARGEST POSSIBLE LOGICAL                        
C   RECORD BEING 268 BYTES LONG.                                                
C   GIVEN ARGS(1) THRU (9) ARE IDENTICAL TO PUTLAB CALL SEQUENCE                
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: CRAY FORTRAN 77                                                   
C   MACHINE:  CRAY                                                              
C                                                                               
C$$$                                                                            
C                                                                               
C                                                                               
      REAL       ABUF(66)                                                       
C                                                                               
      CHARACTER*464 BTEXT                                                       
      CHARACTER*1 TEXT(NCHAR)                                                   
      INTEGER    IBCD(58)                                                       
      INTEGER    IBUF(66)                                                       
      INTEGER    JBUF(67)                                                       
      INTEGER    IPRIOR(2)                                                      
C                                                                               
      EQUIVALENCE  (BTEXT,IBCD)                                                 
      EQUIVALENCE  (JBUF(2),IBUF(1))                                            
      EQUIVALENCE  (JBUF(2),ABUF(1))                                            
C                                                                               
      IBUF(1) = IPT                                                             
      IBUF(2) = JPT                                                             
      ABUF(3) = HEIGHT                                                          
      ABUF(4) = ANGLE                                                           
      IBUF(5) = NCHAR                                                           
      IBUF(6) = IPRIOR(1)                                                       
      IBUF(7) = IPRIOR(2)                                                       
      IBUF(8) = ITAG                                                            
      NCH = NCHAR                                                               
      IF(NCH .LE. 0)THEN                                                        
            WRITE(6,FMT='('' SAVARG: ERROR NCHAR='',I4,'' IS OUT'',             
     1       '' OF RANGE, NOTHING WRITTEN TO FILE :'',I3)')                     
     2       NCHAR,KSAVUN                                                       
      ELSE                                                                      
       IF(NCH .GT. 464) NCH = 464                                               
       DO   J = 1,NCH                                                           
        BTEXT(J:J)  = TEXT(J)                                                   
       ENDDO                                                                    
       NWDS = NCH/8                                                             
       IRMNDR = MOD(NCH,8)                                                      
       IF(IRMNDR .NE. 0) NWDS = NWDS + 1                                        
C                                                                               
       DO   I = 1,NWDS                                                          
         IBUF(I+8) = IBCD(I)                                                    
       ENDDO                                                                    
C                                                                               
       NTWDS = NWDS + 8                                                         
       JBUF(1) = NTWDS                                                          
       N = NTWDS +1                                                             
       WRITE (KSAVUN) (JBUF(I),I=1,N)                                           
C                                                                               
      ENDIF                                                                     
      RETURN                                                                    
      END                                                                       
