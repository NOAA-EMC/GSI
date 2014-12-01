      SUBROUTINE GTARGS(KSAVUN,IPT,JPT,HEIGHT,IBCD,ANGLE,                       
     1                  NCHAR,IPRIOR,ITAG,IERR)                                 
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    GTARGS      READS IN THE PUTLAB ARGS SAVED IN KSAVUN           
C   PRGMMR: KRISHNA KUMAR     ORG: W/NP12    DATE: 1999-07-01                    
C                .      .    .                                       .          
C ABSTRACT: GETS THE PUTLAB ARGS WHICH WERE TEMPORARILY SAVED IN DISK           
C   FILE  DSRN=KSAVUN BY SUB SAVARG                                             
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   YY-MM-DD  ORIGINAL AUTHOR SHIMOMURA                                         
C   89-07-25  HENRICHSEN MOVE KSAVUN ARG TO FIRST ARG AND DOCUMENT.             
C   95-11-14  HENRICHSEN CONVERT TO RUN ON THE CRAY.                            
C 1999-07-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM RS/6000.
C                                                                               
C USAGE     CALL GTARGS(KSAVUN,IPT,JPT,HEIGHT,IBCD,ANGLE,                       
C    1                  NCHAR,IPRIOR,ITAG,IERR)                                 
C   INPUT ARGUMENT LIST:                                                        
C     KSAVUN   - INTEGER UNIT NUMBER OF FILE TO READ ARGS FROM.                 
C                                                                               
C   OUTPUT ARGUMENT LIST:                                                       
C     IPT      - INTEGER ICOORDINATE FOR PUTLAB                                 
C     IPT      - INTEGER JCOORDINATE FOR PUTLAB                                 
C     HEIGHT   - REAL HEIGHT OF CHARACTER FOR PUTLAB.                           
C     IBCD     - INTEGER ARRAY CONTAINING CHARACTER TEXT FOR PUTLAB.            
C     ANGLE    - REAL ROTATION ANGLE ARG FOR PUTLAB.                            
C     NCHAR    - INTEGER NUMBER OF CHARACTERS IN IBCD ARRAY.                    
C     IPRIOR   - INTEGER TWO WORD ARAY (PRIORITY FLAGS FOR PUTLAB).             
C     ITAG     - INTEGER ARG FOR PUTLAB.                                        
C     IERR     - = 0  FOR NORMAL RETURN                                         
C              - = 1  IF PARITY ERROR DURING READ                               
C              - = 2  IF END OF FILE ENCOUNTERED DURING READ                    
C              - = 3  IF BAD WORD COUNT WAS FOUND IN FILE                       
C                                                                               
C   INPUT FILES:                                                                
C     FTXXF001 - FILE CONTAING ARGS TO READ WHERE XX = KSAVUN.                  
C                                                                               
C   OUTPUT FILES:                                                               
C     FT06F001 - PRINT FILE.                                                    
C                                                                               
C REMARKS:                                                                      
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: F90.                                                  
C   MACHINE:  IBM                                                              
C                                                                               
C$$$                                                                            
C                                                                               
C                                                                               
      REAL       ABUF(66)                                                       
C                                                                               
      INTEGER    IBCD(58)                                                       
      INTEGER    IBUF(66)                                                       
      INTEGER    IPRIOR(2)                                                      
C                                                                               
      EQUIVALENCE  (IBUF(1),ABUF(1))                                            
C                                                                               
      IERR = 0                                                                  
      NTWDS = 0
      READ(KSAVUN,ERR=911,END=922) NTWDS,(IBUF(I),I=1,NTWDS)                    
C                                                                               
      IPT       = IBUF(1)                                                       
      JPT       = IBUF(2)                                                       
      HEIGHT    = ABUF(3)                                                       
      ANGLE     = ABUF(4)                                                       
      NCHAR     = IBUF(5)                                                       
      IPRIOR(1) = IBUF(6)                                                       
      IPRIOR(2) = IBUF(7)                                                       
      ITAG = IBUF(8)                                                            
      NWDS = NTWDS - 8                                                          
      IF(NWDS .LE. 0) GO TO 933                                                 
      IF(NWDS .GT. 58) GO TO 933                                                
C                                                                               
      DO  I = 1,NWDS                                                            
       IBCD(I) = IBUF(I+8)                                                      
      ENDDO                                                                     
      GO TO 999                                                                 
C                                                                               
  911 CONTINUE                                                                  
      IERR = 1                                                                  
      GO TO 999                                                                 
C                                                                               
  922 CONTINUE                                                                  
      IERR = 2                                                                  
      GO TO 999                                                                 
C                                                                               
  933  CONTINUE                                                                 
      IERR = 3                                                                  
      GO TO 999                                                                 
C                                                                               
  999 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
