      SUBROUTINE TRUIJ(ALAT,ALONG,XI,XJ,KEIL)                                   
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    TRUIJ       COMPUTE I/J FROM LAT/LONG                          
C   PRGMMR: KRISHNA KUMAR     ORG: W/NP12   DATE: 1999-07-01                     
C                                                                               
C ABSTRACT: CONPUTE THE GRID I/GRID J SPECIFIED BY KIEL FROM THE                
C     GIVEN LATITUDE/LONGITUDE.                                                 
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   UNKNOWN   ORIGINAL AUTHOR  DAVID SHIMOMURA                                  
C   88-06-24  GLORIA DENT PUT IN DOCUMENTATION BLOCK                            
C   89-07-20  HENRICHSEN CLEAN UP AND PUT IN GRAPHICS LOAD AND SOURCE.          
C   95-11-02  HENRICHSEN CONVERT TO RUN ON CRAY.                                
C 1999-07-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM RS/6000.
C                                                                               
C USAGE:    CALL TRUIJ  (ALAT,ALONG,XI,XJ,KEIL)                                 
C   INPUT ARGUMENT LIST:                                                        
C     ALAT     - REAL LATITUDE OF THE POINT                                     
C     ALONG    - REAL LONGITUDE OF THE POINT                                    
C     KEIL     - INTEGER FLAG WORD THAT HAS THE FOLLOWING VALUSE:               
C              - =1 FOR LFM GRID                                                
C              - =2 FOR STD NMC GRID                                            
C              - =3 FOR SRN HEMI 381KM GRID INT., 80W IS VERT. AT TOP,          
C              -     SRN HEMI LATS HAVE NEG. VALUES                             
C              - =4  (USED FOR PEATMOS POP) 190.5 KM GRID LENGTH,               
C              - =5 FOR SFC US 1/10M    ADDED  23 FEB  73                       
C              - =6 FOR LARGER AREA SFC US 1/10M ADDED 12 JUNE 75               
C              - =7 FOR LARGE NH 1/20M 105W FRONT 2/3   20 OCT 1975             
C              - =8 FOR LARGE NH 1/20M BACK PANEL SIDEWAYS 20 OCT 75            
C              - =9  FOR 65*65 N.HEMI 1/40M W/ 105W VERTICAL JAN 16 76          
C              - =10 FOR 47*51 N.HEMI 1/40M W/ 105W VERTICAL JAN 16 76          
C              - =11 FOR 51*51 LFM SUBSET 1/40M W/105W VERTICAL1/16/76          
C              - =12 FOR 53*57 FULL LFM GRID W/ 105W VERTICAL 7/22/76           
C              - =13 FOR 43*31 LFM SUBSET W/ 105W VERT 7/23/76                  
C              - =14 FOR 65*65 STD NMC GRID  NHEMI W/80W VERT 7/7/77            
C              - =15 FO 55*42 NA AFOS W/ 105W VERTICAL 16 MAR 81.               
C              - =16 FOR LARGE SH 1/20M WITH 60W VERTICAL 30 APR 81.            
C              - =17 FOR 87*71 NH 1/20M WITH 105W VERTICAL 9 JUN 82.            
C              - =18 FOR 48*44 NH 1/20M W/102.5W VERT                           
C              -    (DLY WEA MAP) 9/26/82                                       
C                                                                               
C   OUTPUT ARGUMENT LIST:                                                       
C     XI       - REAL THE GRID I SPECIFIED BY KEIL                              
C     XJ       - REAL THE GRID J SPECIFIED BY KEIL                              
C                                                                               
C   OUTPUT FILES:                                                               
C     FT06F001 - PRINT FILE.                                                    
C                                                                               
C REMARKS: THE RADIUS OF THE EARTH EQUAL 6371.2 KILOMETERS IS USED.             
C   KEIL =1  VERT MERIDIAN IS 105W. (LFM)                                       
C   KEIL =2  VERT MERIDIAN IS  80W.(STD NMC)                                    
C   KEIL =3  VERT MERIDIAN IS 100E.(SRN)                                        
C   KEIL =4  VERT MERIDIAN IS  98W. (PEATMOS)                                   
C   KEIL=5 AND =6  VERT MERIDIAN IS 195W.                                       
C   KEIL=7  VERT MERID IS 105W.                                                 
C   KEIL=8  VERT MERID IS 15W .                                                 
C   KEIL=9,10,11,12,13,15  HAVE VERT MERIDIAN AT 105W                           
C   KEIL=14 HAS VERT MERIDIAN AT 80W                                            
C   KEIL=16 HAS VERT MERIDIAN AT 60W                                            
C   KEIL=17 HAS VERT MERIDIAN AT 105W                                           
C   KEIL=18 HAS VERT MERIDIAN AT 102.5W                                         
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: F90                                                     
C   MACHINE:  IBM                                                              
C                                                                               
C$$$                                                                            
C                                                                               
C                                                                               
      REAL     ADDLNG(18)                                                       
      DATA     ADDLNG        /75.0, 100.0,  80.0,  82.0,                        
     1                       -15.0, -15.0,  75.0, 165.0, 5*75.0,                
     2                       100.0,  75.0,  60.0,  75.0, 77.5/                  
      REAL     ALAT                                                             
      REAL     ALONG                                                            
      REAL     CONVT                                                            
      DATA     CONVT         /1.745329E-02/                                     
      REAL     RE(18)                                                           
      DATA     RE            /62.40866, 31.20433, 31.20433, 62.40866,           
     1                        2*124.81733, 2*62.40866, 2*31.20433,              
     2                        3*62.40866, 31.20433, 62.40866,                   
     3                        62.40866,  93.61299, 62.40866/                    
      REAL     WLONG                                                            
      REAL     XI                                                               
      REAL     XIP(18)                                                          
      DATA     XIP           /24.0,  24.0, 24.0,  24.0,                         
     1                       -35.0, -11.0, 55.0, -15.0,                         
     2                        33.0,  24.0, 26.0,  27.0,                         
     3                        17.0,  33.0, 27.0,  55.0,                         
     4                        40.0, 21.0/                                       
      REAL     XJ                                                               
      REAL     XJP(18)                                                          
      DATA     XJP           /46.0, 26.0, 26.0, 46.0, 47.0, 47.0,               
     1                        51.0, 55.0, 33.0, 26.0, 46.0, 49.0,               
     2                        46.0, 33.0, 46.0, 65.0, 73.0, 48.0/               
      REAL     XLAT                                                             
C                                                                               
      INTEGER   MXKEIL                                                          
      DATA      MXKEIL       /18/                                               
C     ...WHERE KEIL IS MAX NO. OF GRIDS THIS S/R WORKS FOR                      
C                                                                               
      KEY = KEIL                                                                
      IF((KEY.LE.0) .OR. (KEY.GT.MXKEIL))THEN                                   
C                                                                               
C     ...COMES HERE IF GIVEN KEIL WAS OUT OF ALLOWABLE RANGE                    
C                                                                               
         WRITE(6,FMT='('' TRUIJ: ERROR THE VALUE OF KEIL ='',I4,                
     1                 '' IS OUT-OF-RANGE.'')')KEIL                             
        STOP                                                                    
      ELSE                                                                      
C                                                                               
C     ...OTHERWISE, KEIL IS W/I RANGE                                           
C                                                                               
         IF((KEY .EQ. 3) .OR. (KEY .EQ. 16))THEN                                
C                                                                               
C        ...COMES HERE FOR SRN HEMI ONLY...                                     
C                                                                               
          XLAT = -ALAT * CONVT                                                  
          WLONG = 360.0 - ALONG                                                 
          WLONG = (WLONG + ADDLNG(KEY)) * CONVT                                 
         ELSE                                                                   
          XLAT = ALAT * CONVT                                                   
          WLONG = (ALONG + ADDLNG(KEY)) * CONVT                                 
         ENDIF                                                                  
        R = (RE(KEY) *  COS(XLAT)) / (1.0 +  SIN(XLAT))                         
        XI = XIP(KEY) + R *  SIN(WLONG)                                         
        XJ = XJP(KEY) + R *  COS(WLONG)                                         
      ENDIF                                                                     
      RETURN                                                                    
      END                                                                       
