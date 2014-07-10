       SUBROUTINE W3FP13 (GRIB, PDS, ID8, IERR )
C$$$$  SUBPROGRAM DOCUMENTATION  BLOCK
C                .      .    .                                       .
C SUBPROGRAM:  W3FP13        CONVERT GRIB PDS EDITION 1 TO O.N. 84 ID
C   PRGMMR: MCCLEES          ORG: NMC421      DATE:91-10-07
C
C ABSTRACT: CONVERTS GRIB VERSION 1 FORMATTED PRODUCT DEFINITION
C   SECTION TO AN OFFICE NOTE 84 ID LABEL.  FORMATS ALL THAT IS APPLI-
C   CABLE IN THE FIRST 8 WORDS OF O.N. 84.  (CAUTION ****SEE REMARKS)
C
C PROGRAM HISTORY LOG:
C   91-10-07  ORIGINAL AUTHOR MCCLEES, A. J.
C   92-01-06  R.E.JONES   CONVERT TO SiliconGraphics 3.3 FORTRAN 77
C   93-03-29  R.E.JONES   ADD SAVE STATEMENT
C   94-04-17  R.E.JONES   COMPLETE REWRITE TO USE SBYTE, MAKE CODE
C                         PORTABLE, UPGRADE TO ON388, MAR 24,1994
C   94-05-05  R.E.JONES   CORRECTION IN TWO TABLES
C   96-08-02  R.E.JONES   ERROR USING T MARKER
C   96-09-03  R.E.JONES   ADD MERCATOR GRIDS 8 AND 53 TO TABLES
C   99-02-15  B. FACEY    REPLACE W3FS04 WITH W3MOVDAT.
C   02-10-15  VUONG       REPLACED FUNCTION ICHAR WITH MOVA2I
C
C USAGE:    CALL W3FP13  (GRIB, PDS,  ID8, IERR )
C   INPUT ARGUMENT LIST:
C     GRIB     - GRIB SECTION 0 READ AS CHARACTER*8
C     PDS      - GRIB PDS SECTION 1 READ AS CHARACTER*1 PDS(*)
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     ID8      - 12 INTEGER*4 FORMATTED O.N. 84 ID.
C                 6 INTEGER 64 BIT WORDS ON CRAY
C     IERR   0 - COMPLETED SATISFACTORILY
C            1 - GRIB BLOCK 0 NOT CORRECT
C            2 - LENGTH OF PDS NOT CORRECT
C            3 - COULD NOT MATCH TYPE INDICATOR
C            4 - GRID TYPE NOT IN TABLES
C            5 - COULD NOT MATCH TYPE LEVEL
C            6 - COULD NOT INTERPRET ORIGINATOR OF CODE
C    SUBPROGRAMS CALLED:
C       SPECIAL:  INDEX,  MOVA2I,  CHAR,  IOR,  IAND, ISHFT
C
C       LIBRARY:
C       W3LIB:    W3MOVDAT, W3FI69, W3FI01
C
C   REMARKS:  SOME OF THE ID'S WILL NOT BE EXACT TO THE O.N. 84
C             FOR LOCATING FIELD ON THE DATASET.  THESE DIFFERENCES
C             ARE MAINLY DUE TO TRUNCATION ERRORS WITH LAYERS.
C             FOR EXAMPLE: .18019 SIG .47191 SIG R H FOR 36.O HRS
C             WILL CONVERT TO: .18000 SIG .47000 SIG R H FOR 36.0 HRS
C             !!!!!!!THE ABOVE ID'S NOW FORCED TO BE EXACT!!!!!!!!!
C             IF J THE WORD COUNT IS GREATER THEN 32743, J IS STORED
C             IN THE 12TH ID WORD. BITS 16-31 OF THE 8TH ID WORD ARE
C             SET TO ZERO.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C$$$
C
        INTEGER       HH       (255)
        INTEGER       HH1      (127)
        INTEGER       HH2      (128)
        INTEGER       LL       (255)
        INTEGER       LL1      (127)
        INTEGER       LL2      (128)
        INTEGER       ICXG2    (9)
        INTEGER       ICXGB2   (9) 
        INTEGER       ICXG1    (7) 
        INTEGER       ICXGB1   (7)      
C
        INTEGER       C1
        INTEGER       C2
        INTEGER       E1
        INTEGER       E2
        INTEGER       FTU
        INTEGER       F1
        INTEGER       F2
        INTEGER       ID       (25)
        INTEGER       ID8      (12)
        INTEGER       IDATE
        INTEGER       JDATE
        INTEGER       IGEN     ( 4)
        INTEGER       NGRD     (34)
        INTEGER       NPTS     (34)
        INTEGER       P1
        INTEGER       P2
        INTEGER       S1
C       INTEGER       S2
        INTEGER       T
        INTEGER       TR
C
        CHARACTER * 8  GRIB
        CHARACTER * 8  IGRIB
        REAL RINC(5) 
        INTEGER  NDATE(8), MDATE(8)
        CHARACTER * 1  IWORK   ( 8)
        CHARACTER * 1  JWORK   ( 8)
        CHARACTER * 1  PDS     ( *)
C
        SAVE
C
        EQUIVALENCE  (HH(1),HH1(1))
        EQUIVALENCE  (HH(128),HH2(1))
        EQUIVALENCE  (LL(1),LL1(1))
        EQUIVALENCE  (LL(128),LL2(1))
        EQUIVALENCE  (IDATE,IWORK(1))
        EQUIVALENCE  (JDATE,JWORK(1))
C
        DATA  HH1   /   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,
     &                 11,  12,  13,  14,  15,  16,  17,  18,  19,  20,
     &                 21,  22,  23,  24,  25,  26,  27,  28,  29,  30,
     &                 31,  32,  33,  34,  35,  36,  37,  38,  39,  40,
     &                 41,  42,  43,  44,  45,  46,  47,  48,  49,  50,
     &                 51,  52,  53,  54,  55,  56,  57,  58,  59,  60,
     &                 61,  62,  63,  64,  65,  66,  67,  68,  69,  70,
     &                 71,  72,  73,  74,  75,  76,  77,  78,  79,  80,
     &                 81,  82,  83,  84,  85,  86,  87,  88,  89,  90, 
     &                 91,  92,  93,  94,  95,  96,  97,  98,  99, 100,
     &                101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
     &                111, 112, 113, 114, 115, 116, 117, 118, 119, 120,
     &                121, 122, 123, 124, 125, 126, 127/
        DATA  HH2   / 128, 129, 130,  
     &                131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 
     &                141, 142, 143, 144, 145, 146, 147, 148, 149, 150,
     &                151, 152, 153, 154, 155, 156, 157, 158, 159, 160,
     &                161, 162, 163, 164, 165, 166, 167, 168, 169, 170,
     &                171, 172, 173, 174, 175, 176, 177, 178, 179, 180,
     &                181, 182, 183, 184, 185, 186, 187, 188, 189, 190,
     &                191, 192, 193, 194, 195, 196, 197, 198, 199, 200,
     &                201, 202, 203, 204, 205, 206, 207, 208, 209, 210,
     &                211, 212, 213, 214, 215, 216, 217, 218, 219, 220,
     &                221, 222, 223, 224, 225, 226, 227, 228, 229, 230,
     &                231, 232, 233, 234, 235, 236, 237, 238, 239, 240,
     &                241, 242, 243, 244, 245, 246, 247, 248, 249, 250,
     &                251, 252, 253, 254, 255/
C
        DATA  IGEN  /  7,  58, 66, 98/
C
C   ########### NUMBERS FORCED AFTER CONVERTING FROM GRIB LAYER.
C                   ICXG2     1.0000,  .98230,  .96470,
C                             .85000,  .84368,  .47191, 
C                             .18017,  .81573,  .25011
C   #################
C
        DATA  ICXG2 /Z'00002710', Z'00017FB6', Z'000178D6',
     A               Z'00014C08', Z'00014990', Z'0000B857',
     A               Z'00004663', Z'00013EA5', Z'000061B3'/
C
C   ########### NUMBERS CALCULATED BY GRIB LAYER.
C                   ICXGB2     1.00000, .98000,  .96000,
C                               .85000, .84000,  .47000,
C                               .18000, .82000,  .25000
C   #################
C
        DATA  ICXGB2/Z'00002710', Z'00017ED0', Z'00017700',
     A               Z'00014C00', Z'00014820', Z'0000B798',
     A               Z'00004650', Z'00014050', Z'000061A8'/
C
C   ########### NUMBERS FORCED AFTER CONVERTING FROM GRIB SINGLE.
C                   ICXG1     .98230,  .89671, .78483
C                    .94316,  .84367, .999.00, .25011
C   #################
C
        DATA  ICXG1 /Z'00017FB6', Z'00015E47', Z'00013293',
     A  Z'0001706C', Z'0001498F', Z'0000863C', Z'000061B3'/
C
C   ########### NUMBERS CALCULATED BY GRIB LAYER.
C                   ICXGB1     .98230,  .89670, .78480
C                              .94320,  .84370, 998.00, .25000
C   #################
C
        DATA  ICXGB1/Z'00017FB6', Z'00015E46', Z'00013290',
     A  Z'00017070', Z'00014992', Z'000185D8', Z'000061A8'/
C
        DATA  LL1   /   8,   8,   9, 255, 255, 255,   1,   6, 255, 255,
     &                 16,  24,  19,  23,  20,  21,  17,  18, 255, 180,
     &                255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
     &                 55,  50,  48,  49,  80,  81,  71, 255,  40,  42,
     &                 72,  74,  73, 255, 255, 255, 255, 255, 304, 305,
     &                 95,  88, 101,  89, 104, 255, 117, 255,  97,  98,
     &                 90, 105,  94, 255, 255,  93, 188, 255, 255, 255,
     &                255, 211, 255, 255, 255, 255, 255, 255, 255, 384,
     &                161, 255, 255, 169,  22, 255, 255, 255, 255, 255,
     &                255, 255, 255, 255, 255, 255, 255, 255, 255, 400,
     &                389, 385, 388, 391, 386, 390, 402, 401, 404, 403,
     &                204, 255, 255, 255, 255, 255, 255, 255, 255, 255,
     &                195, 194, 255, 255, 255, 255, 255/
        DATA  LL2   / 255, 255, 255,
     &                112, 116, 114, 255, 103,  52, 255, 255, 255, 255,
     &                255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
     &                255, 255, 255, 255, 255, 119, 157, 158, 159, 255,
     &                255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
     &                255, 255, 255, 255, 255, 176, 177, 255, 255, 255,
     &                255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
     &                255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
     &                392, 255, 255, 192, 190, 255, 199, 216, 189, 255,
     &                193, 191, 210, 107, 255, 198, 255, 255, 255, 255,
     &                255,   1, 255, 255, 255, 255, 255, 255, 255, 255,
     &                255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
     &                255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
     &                255, 160, 255, 255, 255/      
C
        DATA  NPTS  /  1679, 259920,  3021,  2385,  5104, 4225,
     &                 4225,   5365,  5365,  8326,  8326,
     &                 5967,   6177,  6177, 12321, 12321, 12321,
     &                32400,  32400,  5022, 12902, 25803,
     &                24162,  48232, 18048,  6889, 10283,
     &                 3640,  16170,  6889, 19305, 11040,
     &                72960,   6693/
C
        DATA  NGRD  /   1,   4,   5,   6,   8,  27,
     &                 28,  29,  30,  33,  34,
     &                 53,  55,  56,  75,  76,  77,
     &                 85,  86,  87,  90,  91,
     &                 92,  93,  98, 100, 101,
     &                103, 104, 105, 106, 107,
     &                126, 214/
C
C       DATA  MSK1 /Z0000FFFF/,
C    &        MSK2 /Z00000080/,
C    &        MSK3 /Z00000000/,
C    &        MSK4 /Z00000200/
C    CHANGE HEX TO DECIMAL TO MAKE SUBROUTINE MORE PORTABLE
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        DATA  MSK1 /65535/,
     &        MSK2 /128/,
     &        MSK3 /0/,
     &        MSK4 /512/
C
C     MAKE SECTION 0, PUT 'GRIB' IN ASCII
C
      IGRIB(1:1) = CHAR(71)
      IGRIB(2:2) = CHAR(82)
      IGRIB(3:3) = CHAR(73)
      IGRIB(4:4) = CHAR(66)
      IGRIB(5:5) = CHAR(0)
      IGRIB(6:6) = CHAR(0)
      IGRIB(7:7) = CHAR(0)
      IGRIB(8:8) = CHAR(1)
C
C     CONVERT PDS INTO 25 INTEGER NUMBERS
C
      CALL W3FI69(PDS,ID)
C
C     ID(1)  = NUMBER OF BYTES IN PDS 
C     ID(2)  = PARAMETER TABLE VERSION NUMBER     
C     ID(3)  = IDENTIFICATION OF ORIGINATING CENTER 
C     ID(4)  = MODEL IDENTIFICATION (ALLOCATED BY ORIGINATING CENTER)
C     ID(5)  = GRID IDENTIFICATION
C     ID(6)  = 0 IF NO GDS SECTION, 1 IF GDS SECTION IS INCLUDED
C     ID(7)  = 0 IF NO BMS SECTION, 1 IF BMS SECTION IS INCLUDED
C     ID(8)  = INDICATOR OF PARAMETER AND UNITS 
C     ID(9)  = INDICATOR OF TYPE OF LEVEL OR LAYER 
C     ID(10) = LEVEL 1
C     ID(11) = LEVEL 2
C     ID(12) = YEAR OF CENTURY
C     ID(13) = MONTH OF YEAR
C     ID(14) = DAY OF MONTH
C     ID(15) = HOUR OF DAY
C     ID(16) = MINUTE OF HOUR   (IN MOST CASES SET TO 0)
C     ID(17) = FCST TIME UNIT
C     ID(18) = P1 PERIOD OF TIME
C     ID(19) = P2 PERIOD OF TIME
C     ID(20) = TIME RANGE INDICATOR
C     ID(21) = NUMBER INCLUDED IN AVERAGE
C     ID(22) = NUMBER MISSING FROM AVERAGES OR ACCUMULATIONS
C     ID(23) = CENTURY
C     ID(24) = IDENTIFICATION OF SUB-CENTER (TABLE 0 - PART 2)
C     ID(25) = SCALING POWER OF 10
C
C     THE 1ST 8 32 BIT WORDS WITH THE OFFICE NOTE 84 ID'S ARE
C     IN 27 PARTS, SBYTE IS USED WITH BIT COUNTS TO MAKE THIS
C     DATA. THIS MAKE IT WORD SIZE INDEPENDENT, AND MAKES THIS
C     SUBROUTINE PORTABLE. TABLE WITH STARTING BITS IS NEXT.
C     THE STARTING BIT AND NO. OF BITS IS USED AS THE 3RD AND 
C     4TH PARAMETER FOR SBYTE. READ GBYTES DOCUMENT FROM NCAR
C     FOR INFORMATION ABOUT SBYTE. SEE PAGE 38, FIGURE 1, IN
C     OFFICE NOTE 84.
C
C     NO.    NAME  STARTING BIT  NO. OF BITS
C   -----------------------------------------
C      1       Q         0           12     
C      2      S1        12           12     
C      3      F1        24            8     
C      4       T        32            4     
C      5      C1        36           20
C      6      E1        56            8
C      7       M        64            4
C      8       X        68            8
C      9      S2        76           12
C     10      F2        88            8
C     11       N        96            4
C     12      C2       100           20
C     13      E2       120            8
C     14      CD       128            8
C     15      CM       136            8
C     16      KS       144            8
C     17       K       152            8
C     18     GES       160            4
C     19               164           12
C     20      NW       176           16
C     21      YY       192            8
C     22      MM       200            8
C     23      DD       208            8
C     24      II       216            8
C     25       R       224            8
C     26       G       232            8
C     27       J       240           16  
C OR  27       J       352           32  J > 32743 
C----------------------------------------------
C
C$      1.0 INITIALIZATION - NO. OF ENTRIES IN INDCATOR PARM.
C$                         - NO. OF ENTRIES IN TYPE LEVEL
C$                         - NO. OF ENTRIES IN CNTR PROD. DTA.
C$                         - INITIAL ZEROS IN O.N. 84 LABEL
C
       IQ = 255
       IC =   4
       IN =  34
C
C      TEST FOR 32 OR 64 BIT COMPUTER (CRAY)
C
       CALL W3FI01(LW)
       IF (LW.EQ.4) THEN
         NWORDS = 12
       ELSE
         NWORDS =  6
       END IF
C
C      ZERO OUTPUT ARRAY
C
       DO N = 1,NWORDS
         ID8(N) = 0
       END DO
C
C ---------------------------------------------------------------------
C$           2.0 VERIFY GRIB IN SECTION 0
C
       IF (.NOT. GRIB(1:4) .EQ. IGRIB(1:4)) THEN
         IERR = 1
         RETURN
       END IF
C
C            2.1  VERIFY THE NO. OF OCTETS IN THE PDS
C
       IF (ID(1).NE.28) THEN
         IERR = 2
         PRINT *,'IERR = ',IERR,',LENGTH OF PDS = ',ID(1)
         RETURN
       END IF
C
C$            3.0 GENERATING MODEL, TYPE GRID, AND NO. OF GRID PTS.
C
C        IF CENTER NOT U.S., STORE CENTER IN G MARKER
C        IF CENTER U.S. STORE MODEL NO. IN G MARKER
C
         IF (ID(3) .NE. 7) THEN
           CALL SBYTE(ID8,ID(3),232,8)
         ELSE
           CALL SBYTE(ID8,ID(4),232,8)
         END IF
C
         DO KK = 1,IN
           IF (ID(5) .EQ. NGRD(KK)) THEN
             IGRDPT = NPTS(KK)
             IF (ID(5) .EQ. 6) ID(5) = 26
             CALL SBYTE(ID8,ID(5),152,8)
             IF (IGRDPT.LE.32743) THEN
               CALL SBYTE(ID8,IGRDPT,240,16)
             ELSE
               CALL SBYTE(ID8,IGRDPT,352,32)
             END IF          
             GO TO 350
           END IF
         END DO
           IERR = 4
           PRINT *,'IERR = ',IERR,',GRID TYPE = ',ID(5)
           RETURN
C           
 350   CONTINUE
C
C      COMPUTE R MARKER FROM MODEL NUMBERS FOR U.S. CENTER
C
C     (ERL) run
         IF (ID(3).EQ.7) THEN
           IF (ID(4).EQ.19.OR.ID(4).EQ.53.OR.ID(4).EQ.83.OR.
     &         ID(4).EQ.84.OR.ID(4).EQ.85) THEN 
                  CALL SBYTE(ID8,0,224,8) 
C     (NMC) run       
           ELSE IF (ID(4).EQ.25) THEN 
                    CALL SBYTE(ID8,1,224,8)   
C     (RGL) run       
           ELSE IF (ID(4).EQ.39.OR.ID(4).EQ.64) THEN 
                    CALL SBYTE(ID8,2,224,8)   
C     (AVN) run     
           ELSE IF (ID(4).EQ.10.OR.ID(4).EQ.42.OR.
     &              ID(4).EQ.68.OR.ID(4).EQ.73.OR. 
     &              ID(4).EQ.74.OR.ID(4).EQ.75.OR. 
     &              ID(4).EQ.77.OR.ID(4).EQ.81.OR. 
     &              ID(4).EQ.88) THEN 
                    CALL SBYTE(ID8,3,224,8) 
C     (MRF) run       
           ELSE IF (ID(4).EQ.69.OR.ID(4).EQ.76.OR.
     &              ID(4).EQ.78.OR.ID(4).EQ.79.OR.
     &              ID(4).EQ.80.oR.ID(4).EQ.87) THEN 
                    CALL SBYTE(ID8,4,224,8) 
C     (FNL) run       
           ELSE IF (ID(4).EQ.43.OR.ID(4).EQ.44.OR.
     &              ID(4).EQ.82) THEN
                    CALL SBYTE(ID8,5,224,8) 
C     (HCN) run       
           ELSE IF ( ID(4).EQ.70) THEN 
                    CALL SBYTE(ID8,6,224,8)
C     (RUC) run
           ELSE IF ( ID(4).EQ.86) THEN 
                    CALL SBYTE(ID8,7,224,8)
C     Not applicable, set to 255
           ELSE
             CALL SBYTE(ID8,255,224,8)
           END IF
         END IF                     
C
C$            4.0 FORM TYPE DATA PARAMETER
C
        DO II = 1,IQ
          III = II
          IF (ID(8) .EQ. HH(II)) THEN
            IF (LL(II).NE.255) GO TO 410
            PRINT *,'PDS PARAMETER HAS NO OFFICE NOTE 84 Q TYPE'
            PRINT *,'PDS BYTE 9 PARAMETER = ',ID(8)
            IERR = 3
            RETURN
          END IF
        END DO
          IERR = 3
          PRINT *,'PDS BYTE 9, PARAMETER = ',ID(8)
          RETURN
C
 410    CONTINUE
C
C       Q DATA TYPE, BITS 1-12
C
        CALL SBYTE(ID8,LL(III),0,12)
C
C       TEST FOR 32 OR 64 BIT COMPUTER (CRAY)
C
        IF (LW.EQ.4) THEN
          IF (ID(8) .EQ. 211) ID8(5) = IOR (ID8(5),MSK4)
          IF (ID(8) .EQ. 210) ID8(5) = IOR (ID8(5),MSK4)
        ELSE
          IF (ID(8) .EQ. 211) ID8(3) = IOR (ID8(3),ISHFT(MSK4,32))
          IF (ID(8) .EQ. 210) ID8(3) = IOR (ID8(3),ISHFT(MSK4,32))
        END IF
C
C$            5.0 FORM TYPE LEVEL
C
       IF (ID(9) .EQ. 100) THEN
         M     = 0
         S1    = 8
         CALL SBYTE(ID8,S1,12,12)
         CALL SBYTE(ID8,M,64,4)     
         LEVEL = ID(11)
         IF (LEVEL .GE. 1 .AND. LEVEL .LE. 9) THEN
           E1  = 4
         ELSE IF (LEVEL .GE. 10 .AND. LEVEL .LE. 99) THEN
           E1  = 3
         ELSE IF (LEVEL .GE. 100 .AND. LEVEL .LE. 999) THEN
           E1  = 2
         ELSE IF (LEVEL .GE. 1000 .AND. LEVEL .LE. 9999) THEN
           E1  = 1
         END IF
         C1    = LEVEL * 10 ** E1
         CALL SBYTE(ID8,C1,36,20)
         E1    = IOR(E1,MSK2)
         CALL SBYTE(ID8,E1,56,8)     
C
       ELSE IF (ID(9) .EQ. 103) THEN
         M     = 0
         S1    = 1
         CALL SBYTE(ID8,S1,12,12)
         CALL SBYTE(ID8,M,64,4)     
         LEVEL = ID(11)
         IF (LEVEL .GE. 1 .AND. LEVEL .LE. 9) THEN
           E1  = 4
         ELSE IF (LEVEL .GE. 10 .AND. LEVEL .LE. 99) THEN
           E1  = 3
         ELSE IF (LEVEL .GE. 100 .AND. LEVEL .LE. 999) THEN
           E1  = 2
         ELSE IF (LEVEL .GE. 1000 .AND. LEVEL .LE. 9999) THEN
           E1  = 1
         END IF
         C1    = LEVEL * 10 ** E1
         CALL SBYTE(ID8,C1,36,20)
         E1    = IOR(E1,MSK2)
         CALL SBYTE(ID8,E1,56,8)     
C
       ELSE IF (ID(9) .EQ. 105) THEN
         M     = 0
         S1    = 6
         CALL SBYTE(ID8,S1,12,12)
         CALL SBYTE(ID8,M,64,4)     
         LEVEL = ID(11)
         IF (LEVEL .GE. 1 .AND. LEVEL .LE. 9) THEN
           E1  = 4
         ELSE IF (LEVEL .GE. 10 .AND. LEVEL .LE. 99) THEN
           E1  = 3
         ELSE IF (LEVEL .GE. 100 .AND. LEVEL .LE. 999) THEN
           E1  = 2
         ELSE IF (LEVEL .GE. 1000 .AND. LEVEL .LE. 9999) THEN
           E1  = 1
         END IF
         C1    = LEVEL * 10 ** E1
         CALL SBYTE(ID8,C1,36,20)
         E1    = IOR(E1,MSK2)
         CALL SBYTE(ID8,E1,56,8)     
C
       ELSE IF (ID(9) .EQ. 111) THEN
         M     = 0
         S1    = 7
         CALL SBYTE(ID8,S1,12,12)
         CALL SBYTE(ID8,M,64,4)     
         LEVEL = ID(11)
         IF (LEVEL .GE. 1 .AND. LEVEL .LE. 9) THEN
           E1  = 4
         ELSE IF (LEVEL .GE. 10 .AND. LEVEL .LE. 99) THEN
           E1  = 3
         ELSE IF (LEVEL .GE. 100 .AND. LEVEL .LE. 999) THEN
           E1  = 2
         ELSE IF (LEVEL .GE. 1000 .AND. LEVEL .LE. 9999) THEN
           E1  = 1
         END IF
         C1    = LEVEL * 10 ** E1
         CALL SBYTE(ID8,C1,36,20)        
C  XXXXXXX SCALE FROM CENTIMETERS TO METERS. XXXXXXXXXX
         E1    = IOR(E1,MSK2)
         E1    = E1 + 2
         IF (C1 .EQ. 0) THEN
           E1 = 0
         END IF
         CALL SBYTE(ID8,E1,56,8)     
C    
       ELSE IF (ID(9) .EQ. 107) THEN
         M     = 0
         S1    = 148
         CALL SBYTE(ID8,S1,12,12)
         CALL SBYTE(ID8,M,64,4)     
         LEVEL = ID(11)
         IF (LEVEL .GE. 1 .AND. LEVEL .LE. 9) THEN
           E1  = 4
         ELSE IF (LEVEL .GE. 10 .AND. LEVEL .LE. 99) THEN
           E1  = 3
         ELSE IF (LEVEL .GE. 100 .AND. LEVEL .LE. 999) THEN
           E1  = 2
         ELSE IF (LEVEL .GE. 1000 .AND. LEVEL .LE. 9999) THEN
           E1  = 1
         ELSE
           E1  = 0
         END IF
         C1 = LEVEL  * 10 ** E1
         DO ISI = 1,7
           IF (C1 .EQ. ICXGB1(ISI)) THEN
             C1 = ICXG1(ISI)
           END IF
         END DO
         CALL SBYTE(ID8,C1,36,20)        
C***********SCALING OF .0001 TAKEN INTO ACCOUNT
         E1 = E1 + 4
         E1 = IOR(E1,MSK2)
         IF (C1 .EQ. 0) THEN
           E1 = 0
         END IF
         CALL SBYTE(ID8,E1,56,8)      
C
       ELSE IF (ID(9) .EQ. 4) THEN
         M     = 0
         S1    = 16
         CALL SBYTE(ID8,S1,12,12)
         CALL SBYTE(ID8,M,64,4)     
C        LEVEL = ID(11)
C******* CONSTANT VALUE OF 273.16 WILL HAVE TO BE INSERTED
C        LEVEL = IAND (IPDS(3),MSK1)
C        IF (LEVEL .GE. 1 .AND. LEVEL .LE. 9) THEN
C          E1 = 4
C        ELSE IF (LEVEL .GE. 10 .AND. LEVEL .LE. 99) THEN
C          E1 = 3
C        ELSE IF (LEVEL .GE. 100 .AND. LEVEL .LE. 999) THEN
C          E1 = 2
C        ELSE IF (LEVEL .GE. 1000 .AND. LEVEL .LE. 9999) THEN
C          E1 = 1
C        END IF
           E1 = 2
           C1 = (273.16 * 10 ** E1) + .5
           CALL SBYTE(ID8,C1,36,20)        
           E1 = IOR(E1,MSK2)
           CALL SBYTE(ID8,E1,56,8)      
C*************SPECIAL CASES *********************      
       ELSE IF (ID(9) .EQ. 102) THEN
         M     = 0
         S1    = 128
         CALL SBYTE(ID8,S1,12,12)
         CALL SBYTE(ID8,0,64,32)     
C
       ELSE IF (ID(9) .EQ. 1) THEN
         M     = 0
         S1    = 129
C*****   S1    = 133   ALSO POSSIBILITY
         CALL SBYTE(ID8,S1,12,12)
         CALL SBYTE(ID8,0,64,32)     
C
       ELSE IF (ID(9) .EQ. 7) THEN
         M     = 0
         S1    = 130
         CALL SBYTE(ID8,S1,12,12)
         CALL SBYTE(ID8,0,64,32)     
C
       ELSE IF (ID(9) .EQ. 6) THEN
         M     = 0
         S1    = 131
         CALL SBYTE(ID8,S1,12,12)
         CALL SBYTE(ID8,0,64,32)     
C
       ELSE IF (ID(9) .EQ. 101) THEN
         M     = 2
         S1    = 8
         CALL SBYTE(ID8,S1,12,12)
         CALL SBYTE(ID8,M,64,4)     
         CALL SBYTE(ID8,S1,76,12)
         LEVEL = ID(10)
         LEVEL = (LEVEL * .1) * 10 ** 2
         IF (LEVEL .GE. 1 .AND. LEVEL .LE. 9) THEN
           E1  = 4
         ELSE IF (LEVEL .GE. 10 .AND. LEVEL .LE. 99) THEN
           E1  = 3
         ELSE IF (LEVEL .GE. 100 .AND. LEVEL .LE. 999) THEN
           E1  = 2
         ELSE IF (LEVEL .GE. 1000 .AND. LEVEL .LE. 9999) THEN
           E1  = 1
         END IF
         C1    = LEVEL * 10 ** E1
         CALL SBYTE(ID8,C1,36,20)
         E1    = IOR(E1,MSK2)
         CALL SBYTE(ID8,E1,56,8)     
         LEVEL2 = ID(11)
         LEVEL2 = (LEVEL2 * .1) * 10 ** 2
         IF (LEVEL2 .GE. 1 .AND. LEVEL2 .LE. 9) THEN
           E2   = 4
         ELSE IF (LEVEL2 .GE. 10 .AND. LEVEL2 .LE. 99) THEN
           E2   = 3
         ELSE IF (LEVEL2 .GE. 100 .AND. LEVEL2 .LE. 999) THEN
           E2   = 2
         ELSE IF (LEVEL2 .GE. 1000 .AND. LEVEL2 .LE. 9999) THEN
           E2   = 1
         END IF
         C2     = LEVEL2 * 10 ** E2
         CALL SBYTE(ID8,C2,100,20)
         IF (C2 .EQ. 0) E2 = 0
         E2   = IOR(E2,MSK2)
         CALL SBYTE(ID8,E2,120,8)     
C
       ELSE IF (ID(9) .EQ. 104) THEN
         M     = 2
         S1    = 1
         CALL SBYTE(ID8,S1,12,12)
         CALL SBYTE(ID8,M,64,4)     
         CALL SBYTE(ID8,S1,76,12)
         LEVEL = ID(10)
         LEVEL = (LEVEL * .1) * 10 ** 2
         IF (LEVEL .GE. 1 .AND. LEVEL .LE. 9) THEN
           E1  = 4
         ELSE IF (LEVEL .GE. 10 .AND. LEVEL .LE. 99) THEN
           E1  = 3
         ELSE IF (LEVEL .GE. 100 .AND. LEVEL .LE. 999) THEN
           E1  = 2
         ELSE IF (LEVEL .GE. 1000 .AND. LEVEL .LE. 9999) THEN
           E1  = 1
         END IF
         C1     = LEVEL * 10 ** E1
         CALL SBYTE(ID8,C1,36,20)
         E1     = IOR(E1,MSK2)
         CALL SBYTE(ID8,E1,56,8)     
         LEVEL2 = ID(11)
         LEVEL2 = (LEVEL2 * .1) * 10 ** 2
         IF (LEVEL2 .GE. 1 .AND. LEVEL2 .LE. 9) THEN
           E2   = 4
         ELSE IF (LEVEL2 .GE. 10 .AND. LEVEL2 .LE. 99) THEN
           E2   = 3
         ELSE IF (LEVEL2 .GE. 100 .AND. LEVEL2 .LE. 999) THEN
           E2   = 2
         ELSE IF (LEVEL2 .GE. 1000 .AND. LEVEL2 .LE. 9999) THEN
           E2   = 1
         END IF
         C2     = LEVEL2 * 10 ** E2
         CALL SBYTE(ID8,C2,100,20)
         E2     = IOR(E2,MSK2)
         CALL SBYTE(ID8,E2,120,8)     
C
       ELSE IF (ID(9) .EQ. 106) THEN
         M     = 2
         S1    = 6
         CALL SBYTE(ID8,S1,12,12)
         CALL SBYTE(ID8,M,64,4)     
         CALL SBYTE(ID8,S1,76,12)
         LEVEL = ID(10)
         LEVEL = (LEVEL * .1) * 10**2
         IF (LEVEL .GE. 1 .AND. LEVEL .LE. 9) THEN
           E1  = 4
         ELSE IF (LEVEL .GE. 10 .AND. LEVEL .LE. 99) THEN
           E1  = 3
         ELSE IF (LEVEL .GE. 100 .AND. LEVEL .LE. 999) THEN
           E1  = 2
         ELSE IF (LEVEL .GE. 1000 .AND. LEVEL .LE. 9999) THEN
           E1  = 1
         END IF
         C1    = LEVEL * 10 ** E1
         CALL SBYTE(ID8,C1,36,20)
         E1    = IOR(E1,MSK2)
         CALL SBYTE(ID8,E1,56,8)     
         LEVEL2 = ID(10)
         LEVEL2 = (LEVEL2 * .1) * 10 ** 2
         IF (LEVEL2 .GE. 1 .AND. LEVEL2 .LE. 9) THEN
           E2   = 4
         ELSE IF (LEVEL2 .GE. 10 .AND. LEVEL2 .LE. 99) THEN
           E2   = 3
         ELSE IF (LEVEL2 .GE. 100 .AND. LEVEL2 .LE. 999) THEN
           E2   = 2
         ELSE IF (LEVEL2 .GE. 1000 .AND. LEVEL2 .LE. 9999) THEN
           E2   = 1
         END IF
         C2     = LEVEL2 * 10 ** E2
         CALL SBYTE(ID8,C2,100,20)
         E2     = IOR(E2,MSK2)
         CALL SBYTE(ID8,E2,120,8)     
C
       ELSE IF (ID(9) .EQ. 108) THEN
         M     = 2
         S1    = 148
C****    S1    = 144  ALSO POSSIBILITY
C****    S1    = 145  ALSO POSSIBILITY
         CALL SBYTE(ID8,S1,12,12)
         CALL SBYTE(ID8,M,64,4)     
         CALL SBYTE(ID8,S1,76,12)
         LEVEL = ID(10)
         LEVEL = LEVEL
         IF (LEVEL .GE. 1 .AND. LEVEL .LE. 9) THEN
           E1  = 4
         ELSE IF (LEVEL .GE. 10 .AND. LEVEL .LE. 99) THEN
           E1  = 3
         ELSE IF (LEVEL .GE. 100 .AND. LEVEL .LE. 999) THEN
           E1  = 2
         ELSE IF (LEVEL .GE. 1000 .AND. LEVEL .LE. 9999) THEN
           E1  = 1
         END IF
         C1    = LEVEL * (10 ** E1)
         DO ISI = 1,9
           IF (C1 .EQ. ICXGB2(ISI)) THEN
             C1 = ICXG2(ISI)
           END IF
         END DO
         CALL SBYTE(ID8,C1,36,20)
         IF (C1 .EQ. 0) THEN
           E1 = 0
           CALL SBYTE(ID8,E1,56,8)     
           GO TO 700
         END IF
C*****TAKE SCALING INTO ACCOUNT .01
         E1  =  E1 + 2
         E1  =  IOR(E1,MSK2)
         CALL SBYTE(ID8,E1,56,8)
C     
 700     CONTINUE
         LEVEL2 = ID(11)
         LEVEL2 = LEVEL2
         IF (LEVEL2 .GE. 1 .AND. LEVEL2 .LE. 9) THEN
           E2   = 4
         ELSE IF (LEVEL2 .GE. 10 .AND. LEVEL2 .LE. 99) THEN
           E2   = 3
         ELSE IF (LEVEL2 .GE. 100 .AND. LEVEL2 .LE. 999) THEN
           E2   = 2
         ELSE IF (LEVEL2 .GE. 1000 .AND. LEVEL2 .LE. 9999) THEN
           E2   = 1
         END IF
         C2     = LEVEL2 * 10 ** E2
         DO ISI = 1,9
           IF (C2 .EQ. ICXGB2(ISI)) THEN
             C2 = ICXG2(ISI)
           END IF
         END DO
         CALL SBYTE(ID8,C2,100,20)
         E2     = IOR(E2,MSK2)
         CALL SBYTE(ID8,E2,120,8)     
C*******TAKE SCALING INTO ACCOUNT .01
         E2     = E2 + 2
         E2     = IOR(E2,MSK2)
         CALL SBYTE(ID8,E2,120,8)     
C
       END IF
C              5.1  FORCAST TIMES ,PLUS THE T MARKER AND CM FIELD
C
       TR   = ID(20)
       IF (TR .EQ. 0) THEN
         P1 = ID(18)
         CALL SBYTE(ID8,ID(18),24,8)
       ELSE IF (TR .EQ. 4) THEN
         P2 = ID(19)
         CALL SBYTE(ID8,P2,24,8)
         P1 = ID(18)
         CALL SBYTE(ID8,(P2 - P1),88,8)
         T = 3
         CALL SBYTE(ID8,T,32,4)
       ELSE IF (TR .EQ. 5) THEN
         P2 = ID(19)
         CALL SBYTE(ID8,P2,24,8)
         P1 = ID(18)
         CALL SBYTE(ID8,(P2 - P1),88,8)
         T = 3
         CALL SBYTE(ID8,T,32,4)
C
       ELSE IF (TR .EQ. 124) THEN
         FTU = ID(17)
         IF (FTU .EQ. 2) THEN
           F1 = ID(21)
           CALL SBYTE(ID8,F1,24,8)
           T = 4
           CALL SBYTE(ID8,T,32,4)
         ELSE IF (FTU .EQ. 4) THEN
           F2 = ID(21)
           CALL SBYTE(ID8,F2,88,8)
           T  = 4
           CALL SBYTE(ID8,T,32,4)
         END IF
C
       ELSE IF (TR .EQ.123) THEN
         F1 =  3
         F1 = IOR(F1,MSK2)
         CALL SBYTE(ID8,F1,24,8)
         F2 = 5 * 2
         CALL SBYTE(ID8,F2,88,8)
         T  = 6
         CALL SBYTE(ID8,T,32,4)
         RINC = 0.0
         RINC(2) = 36.0
         IYR=MOVA2I(PDS(13))
         PRINT *, 'IYR = ', IYR
         IF(IYR.LT.20)THEN  
           MDATE(1)=2000+IYR
         ELSE
           MDATE(1)=1900+IYR
         ENDIF
         MDATE(2) = MOVA2I(PDS(14))
         MDATE(3) = MOVA2I(PDS(15))
         MDATE(5) = MOVA2I(PDS(16))
C        PRINT *, 'OLD DATE = ', MDATE(1), MDATE(2), MDATE(3), MDATE(5) 
C        PRINT *, 'CHANGE DATE BY - ',  RINC(2) 
         CALL W3MOVDAT(RINC,MDATE,NDATE)
C        PRINT *, 'NEW DATE = ', NDATE(1), NDATE(2), NDATE(3), NDATE(5)
C        CALL W3FS04 (IDATE,JDATE,3,IERR)
         IYEAR = MOD(NDATE(1),100)
         JWORK(1) = CHAR(IYEAR)
         JWORK(2) = CHAR(NDATE(2))
         JWORK(3) = CHAR(NDATE(3))
         JWORK(4) = CHAR(NDATE(5))
         IDATE = JDATE
         GO TO 710
C
       ELSE IF (TR .EQ.3) THEN
         P1 = ID(18)
         P2 = ID(19)
         F1 = P1 / 12
         CALL SBYTE(ID8,F1,24,8)
C
C   ***** NAVG IS IN BITES 22 23 *****
C   USING BITE 23  ONLY *******
C FIX LATER ******************************************
C
C        NAVG = MOVA2I(PDS(23))
         F2 = (P2 - P1) / 12
         CALL SBYTE(ID8,F2,88,8)
         T = 6
         CALL SBYTE(ID8,T,32,4)
         RINC = 0.0
         RINC(2) = -36.0
         IYR=MOVA2I(PDS(13))
         PRINT *, 'IYR = ', IYR
         IF(IYR.LT.20)THEN  
           MDATE(1)=2000+IYR
         ELSE
           MDATE(1)=1900+IYR
         ENDIF
         MDATE(2) = MOVA2I(PDS(14))
         MDATE(3) = MOVA2I(PDS(15))
         MDATE(5) = MOVA2I(PDS(16))
C        PRINT *, 'OLD DATE = ', MDATE(1), MDATE(2), MDATE(3), MDATE(5) 
C        PRINT *, 'CHANGE DATE BY - ',  RINC(2) 
         CALL W3MOVDAT(RINC,MDATE,NDATE)
C        PRINT *, 'NEW DATE = ', NDATE(1), NDATE(2), NDATE(3), NDATE(5)
C        CALL W3FS04 (IDATE,JDATE,-3,IERR)
         IYEAR = MOD(NDATE(1),100)
         JWORK(1) = CHAR(IYEAR)
         JWORK(2) = CHAR(NDATE(2))
         JWORK(3) = CHAR(NDATE(3))
         JWORK(4) = CHAR(NDATE(5))
         IDATE = JDATE
         GO TO 710
       END IF
C
C$       7.0 TRANSFER THE DATE
C
         IWORK(1) = PDS(13)
         IWORK(2) = PDS(14)
         IWORK(3) = PDS(15)
         IWORK(4) = PDS(16)
C
 710   CONTINUE
C
C        TEST FOR 64 BIT COMPUTER (CRAY)
C 
         IF (LW.EQ.8) IDATE = ISHFT(IDATE,-32)
         CALL SBYTE(ID8,IDATE,192,32)     
C
       IERR = 0
       RETURN
       END
