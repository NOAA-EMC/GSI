       SUBROUTINE AFNAIJ(ALAT,WLONG,IPIX,JPIX)                                  
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    AFNAIJ      TRANSFORM EARTH LAT/LON TO AFOS NA. CORR.          
C   PRGMMR: KRISHNA KUMAR     ORG: W/NP12    DATE: 1999-07-01                     
C                                                                               
C ABSTRACT: TRANSFORM EARTH COORDINATES FROM LAT/LONG TO NORTH                  
C   AMERICAN AFOS WINDOW IN PIXELS.  AFOS WINDOW IS 2048 BY 1536                
C   PIXELS.                                                                     
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   89-08-25  ORIGINAL HENRICHSEN                                               
C   95-09-01  HENRICHSEN    CONVERT TO RUN ON CRAY.                                           
C 1999-07-01  KRISHNA KUMAR CONVERT TO RUN ON IBM RS/6000.
C                                                                               
C USAGE:    CALL AFNAIJ(ALAT,WLONG,IPIX,JPIX)                                   
C   INPUT ARGUMENT LIST:                                                        
C     ALAT     - NORTH LATITUDE (IN DEGREES)                                    
C     WLONG    - WEST LONGITUDE (IN DEGREES)                                    
C                                                                               
C   OUTPUT ARGUMENT LIST:                                                       
C     IPIX     - I-COORDINATE (IN PIXELS) FOR AFOS N.AMER MAP                   
C     JPIX     - J-COORDINATE (IN PIXELS) FOR AFOS N.AMER MAP                   
C                                                                               
C                                                                               
C REMARKS: WHERE THE AFOS N.HEMI, IF YOU OVERLAY A 190.5 KM GRID                
C   ORIENTED AT 105.0W LONGITUDE, HAS BOUNDARIES:                               
C   FROM I = 1 TO I = 55.613 FOR 54.613 HALF BEDIENTS WIDE                      
C   WHICH WHEN MULTIPLIED BY .375 INCHES/ G.I.                                  
C   YIELDS 20.48 INCHES (FOR IPOLE = 27)                                        
C   FROM J = 4.0 TO J = 44.96  FOR 40.96 HALF BEDIENTS TALL                     
C   WHICH WHEN MULTIPLIED BY .375 INCHES/G.I                                    
C   YIELDS 15.36 INCHES TALL (FOR JPOLE = 49)                                   
C                                                                               
C   CALLS ON SUB W3FB04                                                         
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: F90                                                    
C   MACHINE:  IBM                                                              
C                                                                               
C$$$                                                                            
C                                                                               
C                                                                               
       REAL       ALAT                                                          
       REAL       DOTSGI                                                        
       DATA       DOTSGI  / 37.5 /                                              
       REAL       EDGBOT                                                        
       DATA       EDGBOT  / 4.0 /                                               
       REAL       EDGLEF                                                        
       DATA       EDGLEF  / 1.0 /                                               
C      ... WHERE THESE EDGES ARE IN SAME GRID UNITS AS 27,47 POLE COORD         
       REAL       GRLNKM                                                        
       DATA       GRLNKM / 190.5 /                                              
       REAL       VERTLN                                                        
       DATA       VERTLN / 105.0 /                                              
       REAL       WLONG                                                         
       REAL       XPOLE                                                         
       DATA       XPOLE / 27.0 /                                                
       REAL       YPOLE                                                         
       DATA       YPOLE / 49.0 /                                                
C                                                                               
       INTEGER    IPIX                                                          
       INTEGER    JPIX                                                          
C                                                                               
       CALL W3FB04(ALAT,WLONG,GRLNKM,VERTLN,RELI,RELJ)                          
C                                                                               
       XSTDG = RELI + XPOLE                                                     
       YSTDG = RELJ + YPOLE                                                     
       PIXI = DOTSGI * (XSTDG - EDGLEF)                                         
       PIXJ = DOTSGI * (YSTDG - EDGBOT)                                         
       IPIX = PIXI + SIGN(0.5,PIXI)                                             
       JPIX = PIXJ + SIGN(0.5,PIXJ)                                             
C                                                                               
       RETURN                                                                   
       END                                                                      
