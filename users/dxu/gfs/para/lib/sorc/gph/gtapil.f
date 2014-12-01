       SUBROUTINE GTAPIL(ISUBST,PILFIL,IHD1,IRTN)                               
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    GTAPIL      GETS THE AFOS PIL FROM PILLIST FILE.               
C   PRGMMR: KRISHNA KUMAR    ORG: W/NP12    DATE: 1999-07-01                    
C                                                                               
C ABSTRACT: GIVEN THE GRAPHIC SUBSET NUMBER AND THE UNIT NUMBER OF              
C   OF THE PILL LIST FILE IT WILL SEARCH THE PIL FILE FOR THE PIL               
C   NUMBER THAT MATCHES THE GRAPHICS SUBSET NUMBER GIVEN IN THE CALL-           
C   ING ARGUEMENT. IF A MATCH IS FOUND THE GRAPHICS PIL NUMBER                  
C   IS RETURNED IN IHD1(1:3),                                                   
C   THE PRODUCT ADDRESS IS RETURNED IN IHD1(10:10),                             
C   AND THE PRODUCT PRIORITY IS RETURNED IN IHD1(7:7)                           
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   95-03-29  ORIGINAL AUTHOR HENRICHSEN                                        
C   95-10-12  CONVERT TO RUN ON THE CRAY IE REMOVED LOGIC TO CONVERT            
C             PIL FROM EBCDIC TO ASCII.                                         
C 1999-07-01  CONVETED THIS CODE FROM CRAY TO IBM RS/6000.
C                                                                               
C USAGE:    CALL GTAPIL(ISUBST,PILFIL,IHD1,IRTN)                                
C   INPUT ARGUMENTS:                                                            
C     ISUBSET  - CHARACTER*4 WORD CONTAINING THE PIL NUMBER IN                  
C              - EBCDIC.                                                        
C     PILFIL   - INTEGER UNIT NUMBER OF THE PIL LIST FILE.                      
C                                                                               
C   OUTPUT ARGUMENT LIST:                                                       
C     IHD1     - CHARACTER*10 ARRAY THAT WILL HAVE THE FOLLOWING:               
C              - IHD1(1:3) AFOS PIL NUMBER IN ASCII.                            
C              - IHD1(4:6) AFOS PIL NUMBER IN ASCII.                            
C              - IHD1(7:7) INTEGER AFOS PIORITY.                                
C              - IHD1(10:10) INTEGER PRODUCT ADDRESS.                           
C     IRTN     - 0 GOOD RETURN.                                                 
C              - 1 UNABLE TO FIND THE SUBSET NUMBER PROVIDED.                   
C                                                                               
C   INPUT FILES:                                                                
C     FTXXF001 - THE PIL LIST FILE WHERE 'XX' IS PILFIL.                        
C                                                                               
C   OUTPUT FILES:                                                               
C     FT06F001 - PRINTOUT.                                                      
C                                                                               
C REMARKS: NONE                                                                 
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: F90.                                                   
C   MACHINE:  IBM                                                              
C                                                                               
C$$$                                                                            
C                                                                               
C                                                                               
      CHARACTER*80  CARD                                                        
      CHARACTER*10  IHD1                                                        
      CHARACTER*4   ISUBST                                                      
      CHARACTER*3   ASCPIL                                                      
      CHARACTER*3   CHPIL                                                       
C                                                                               
      INTEGER     IPIOR                                                         
      INTEGER     IPADD                                                         
      INTEGER     MASK4                                                         
      INTEGER     PILFIL                                                        
C                                                                               
      DATA        MASK4                /Z'F0'/                                  
C                                                                               
      LOGICAL     FOUND                                                         
C                                                                               
      REWIND PILFIL                                                             
         IRTN = 0                                                               
         FOUND = .FALSE.                                                        
C                                                                               
C . . . .READ PIL NUMBERS FROM PIL SUBSET FILE.....                             
C                                                                               
      READ (PILFIL,FMT='(A,I4,A)') CARD(1:40),NMPROD,CARD(45:80)                
      WRITE(6,FMT='('' GTAPIL: '',A,I4,A)')                                     
     1              CARD(1:40),NMPROD,CARD(45:80)                               
C                                                                               
C . . .  LOOP THROUGH FILE TO FIND PROPER SUBSET/PIL NUMBER                     
C                                                                               
            DO  K = 1,NMPROD                                                    
C                                                                               
               READ(PILFIL,FMT='(A,I2,A,I3)')                                   
     1         CARD(1:16),IPIOR,CARD(19:69),IPADD                               
C                                                                               
               IF(ISUBST(1:4).EQ.CARD(3:6)) THEN                                
                 FOUND = .TRUE.                                                 
               ELSE IF(ISUBST(1:4).EQ.CARD(8:11))THEN                           
                 FOUND = .TRUE.                                                 
               ELSE                                                             
               ENDIF                                                            
               IF(FOUND) THEN                                                   
C                                                                               
C               FOUND SUB SET # LOAD IHD1 WITH VALUES.                          
C                                                                               
                 CHPIL(1:3) = CARD(13:15)                                       
                 IHD1(1:3) = CHPIL(1:3)                                         
                 IHD1(4:6) = CHPIL(1:3)                                         
                 IHD1(07:07) = CHAR(IAND(ISHFT(IPIOR,4),MASK4))                 
                 IHD1(10:10) = CHAR(IPADD)                                      
                   WRITE(6,FMT='('' GTAPIL: SUBSET NUMBER ='',A,                
     1             '' PIL ='',A,'' PIORITY='',I2,'' PROD ADDRESS='',            
     2             I4,'' PRODUCT IS:'',A)')ISUBST(1:4),CHPIL(1:3),              
     3             IPIOR,IPADD,CARD(19:69)                                      
                   WRITE(6,FMT='('' GTAPIL: IHD1 ='',A)')IHD1(1:10)             
                 IRTN = 0                                                       
                 RETURN                                                         
               ENDIF                                                            
            ENDDO                                                               
C                                                                               
            WRITE(6,FMT='('' GTAPIL: SUBSET NUMBER '',A,'' NOT '',              
     1      ''IN FILE '',I2)') ISUBST(1:4),PILFIL                               
         IRTN = 1                                                               
C                                                                               
      RETURN                                                                    
      END                                                                       
