      SUBROUTINE IW3GET(LUGRBIX, LUGRB, LABEL, FLD, MTITLE, IERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: IW3GET        GET AND UNPACK CRAY GRIB FILES.
C   AUTHOR: KRISHNA KUMAR      ORG: W/NP12     DATE: 1999-07-01
C
C ABSTRACT: GET AND UNPACK CRAY GRIB FILES FROM AN OFFICE 84 6 WORD ID.
C           PLEASE READ THE REMARKS BEFORE USING IT.
C
C HISTORY LOG:
C   95-10-04  LUKE LIN
C   95-10-31  LUKE LIN      MODIFY FOR TESTING DIFFERENT GRID TYPE.
C   95-11-27  LUKE LIN      ADD AN OPTION FOR NOT TEST FCST HOUR -- CAC 96H/C
C                           AND MODIFY TO READ RGL(NGM) MODEL.
C   96-01-30  LUKE LIN      MODIFY TO CONVERT FROM 1 DEGREE TO 65*65 BY LINEAR.
C   96-02-14  LUKE LIN      MODIFY FOR NGM BOUNDARY LAYER RH CHART.
C   96-02-23  LUKE LIN      MODIFY FOR NGM SURFACE TO 500 MB LIFTED INDEX.
C   96-02-26  LUKE LIN      MODIFY FOR AVMER OR AVPOLAR AND OUTPUT WILL BE
C                           EITHER 2.5 OR 5.0 DEGREE DEPENDING ON OPTION
C   96-03-07  LUKE LIN      MODIFY FOR NGM SUPER C GRID.
C   96-03-29  LUKE LIN      MODIFY FOR AVPOLAR 2.5 DEGREE TO GET NH.
C   96-07-23  LUKE LIN      MODIFY FOR NGM LIFTED INDEX
C 1999-07-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM RS/6000.
C
C INPUT ARGUMENTS:
C   LUGRBIX    - AN UNIT NUMBER POINTS TO ONE DEGREE GRIB INDEX FILE.
C   LUGRB      - AN UNIT NUMBER POINTS TO ONE DEGREE GRIB FILE. SEE 
C                REMARK.
C   LABEL      - 12 WORDS LABEL ON INPUT.  WORDS 1-5 SHOULD CONTAIN
C                LABEL ID OF DESIRED FIELD FLD.
C
C OUTPUT ARGEMENTS:
C   FLD     -- BUFF CONTAINS UNPACK FIELD.
C   LABEL   -- ON OUTPUT THE FULL 12 WORDS OF THE LABEL WILL CONTAIN
C              APPROPRIATE VALUES.
C   MTITL   -- CHARACTER*132 MAP TITLE FROM INDEX FILE.
C              (1:86) IS MAP TITLE, FOLLOWINGS ARE MAP ID.
C   IERR    -- RETURN STATUS
C   IRETUR   - RETURN CONDITION;
C            0 - ALL OK
C            1 - W3FP13/GRIB BLOCK 0 NOT CORRECT
C            2 - W3FP13/LENGTH OF PDS NOT CORRECT
C            3 - W3FP13/COULD NOT MATCH TYPE INDICATOR
C            4 - W3FP13/GRID TYPE NOT IN TABLES
C            5 - W3FP13/COULD NOT MATCH TYPE LEVEL
C            6 - W3FP13/COULD NOT INTERPRET ORIGINATOR OF CODE
C            7 - GRIB TYPE ERROR -- UNKNOWN GRIB TYPE.
C           10 - UNCOGNIZE DATA TYPE FROM LABEL
C           11 - W3FT32/ MAPIN NOT RECOGNIZED
C           12 - W3FT32/ MAPOUT NOT RECOGNIZED
C           13 - W3FT32/ PARTICULAR POLA MAPOUT NOT RECOGNIZED
C           14 - W3FT32/ PARTICULAR LOLA MAPOUT NOT RECOGNIZED
C           15 - W3FT32/ PARTICULAR LOLA MAPIN NOT RECOGNIZED
C           16 - W3FT32/ PARTICULAR POLA MAPOUT NOT RECOGNIZED
C           17 - W3FT32/ PARTICULAR LOLA MAPIN NOT RECOGNIZED
C           18 - W3FT32/ PARTICULAR LOLA MAPOUT NOT RECOGNIZED
C           96 - GETGB/ERROR READING INDEX FILE
C           97 - GETGB/ERROR READING GRIB FILE
C           98 - GETGB/NUMBER OF DATA POINTS GREATER THAN JF
C           99 - GETGB/REQUEST NOT FOUND
C           -1 - GETGB/OTHER  W3FI63 GRIB UNPACKER RETURN CODE
C          101 - W3FP11/NON-FATAL ERROR, MTITLE NOT CORRECT
C
C USAGE:
C   INPUT FILES:
C     UNIT LUGRBIX -  INPUT ONE DEGREE GRIB INDEX FILE.  SEE REMARK.
C     UNIT LUGRB   -  INPUT ONE DEGREE GRIB FILE.  SEE REMARK.
C
C   OUTPUT FILES:
C     FT06F001 -   PRINT OUTPUT (STANDARD FORTRAN OUTPUT FILE)
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:     NONE
C
C     LIBRARY:
C       SPECIAL  - NONE
C       W3LIB    - W3FP11 , W3FP13, GETGB , GBYTES , W3FM08
C                - W3FI63 , W3FT43V,W3FT32, SBYTES
C
C   REMARKS:
C       ***** VERY IMPORTANT ******
C       O. THE INTENTION OF THIS SUBROUTINE IS ONLY FOR AD GRAPHIC SECTION
C          TO READ CRAY GRIB FILES FROM THEIR 84 OFFICE 6 WORD IDS.
C          IT IS NOT GENERAL ENOUGH FOR OTHER GROUP USERS.
C          IT CAN BE MODIFIED TO TAKE 28 BYTE PDS INSTEAD OF 6 WORD 84 ID.
C          BY THIS WAY, IT MAY BE MORE GENERAL THAN IT IS NOW.
C          THIS SUBROUTINE SHOULD NOT BE INCLUDED IN W3LIB AT PRESENT TIME.
C       1. USE ASSIGN COMMAND TO ASSIGN ONE-DEGREE GRIB INDEX TO UNIT LUGRBIX
C       2. USE ASSIGN COMMAND TO ASSIGN ONE-DEGREE GRIB TO UNIT LUGRB
C       FOR EXAMPLE:
C assign -a /com/avn/prod/avn.950912/gblav.T00Z.PGrbiF24 -s unblocked fort.31
C           where LUGRBIX=31
C assign -a /com/avn/prod/avn.950912/gblav.T00Z.PGrbF24 -s unblocked fort.11
C           where LUGRB=11
C       3. ONLY DEAL WITH INPUT DATA TYPE 26, AND 27.  OTHER DATA TYPE MAY
C          HAVE FATAL ERROR RETURN AT PRESENT TIME.
C       4. CONCERNING RGL CHARTS, THE 6TH WORD ID SHOULD BE '00000127'.
C       5. FOR NO FORECAST HOUR CHECKING SUCH AS CAC 96H/C, THE THIRD BYTE
C          OF THE 6TH WORD ID SHOULD BE '96'.
C
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
      PARAMETER (IPTS=65160,II=360,JJ=181,MM=361)
      PARAMETER (IPTS2=10512,II2=144,JJ2=73)
      PARAMETER (MXSIZE=66000)
      PARAMETER (MXSIZ2=MXSIZE*2)
      PARAMETER (IIK=289,JJK=145,NPTS=IIK*JJK)
      PARAMETER (IJK=145,LJK=73,MPTS=IJK*LJK)
C
      INTEGER         LUGI,LUGRBIX
      INTEGER         LUGB,LUGRB
      INTEGER         LABEL(12)
      REAL            FLD(*)
      CHARACTER * 132 MTITLE
      INTEGER         IERR
      INTEGER         IRET
      INTEGER         ITAU
C
      REAL            C(MXSIZE)
      LOGICAL         KBMS(MXSIZE)
      REAL            CC(II,JJ)
      REAL            CC2(II2,JJ2)
      REAL            D(MXSIZE)
      REAL            DD(361,181)
      REAL            D2(MXSIZE)
      REAL            DD2(145,73)
      REAL            DD2N(145,37)
      REAL            DD2N1(5365)
      REAL            EE(4225)
      REAL            FF(4225)
      REAL            BLOLA(NPTS), CLOLA(MPTS)
      REAL            GG(73,37)
      REAL            GG2(2701)
C
      INTEGER         IGRIB(16500)
      CHARACTER * 1   GRIB(MXSIZ2)
C
      INTEGER         IPDS(4)
      INTEGER         JPDS(4)
      INTEGER         JGDS(100)
      INTEGER         MPDS(25)
      INTEGER         KGDS(100)
      INTEGER         KPDS(25)
      CHARACTER * 1   PDS(28)
      CHARACTER * 1   PDSL(28)
C
C
       CHARACTER*1     CTEMP(8)
       INTEGER         ITEMP
C
       INTEGER         LIDREC(6)
       CHARACTER*8     CGRIB
       INTEGER         IMODE
C
      INTEGER      LABELP(6)
      CHARACTER*1  IFLAG
      CHARACTER*1  IDPDSC(28)
      INTEGER*8    IDPDS(4)
      INTEGER      MODELNH
      INTEGER      SFCMEANRH
      INTEGER      MSK1, MSK2, MSK3, MSK4
      INTEGER      MSKNH, MSKNGM
      INTEGER      ITYPE, IMODEL, FLAG
      INTEGER      CAC96HC
      INTEGER      BLRHHDS
      INTEGER      BLRHGRB
      INTEGER      MSKFFFF
      INTEGER      LFTFLG
      INTEGER      AVPOLRV
      INTEGER      AVTYPE
C
      SAVE
C
      EQUIVALENCE     (C(1),CC(1,1),CC2(1,1))
      EQUIVALENCE     (D(1),DD(1,1))
      EQUIVALENCE     (D2(1),DD2(1,1))
      EQUIVALENCE     (DD2N1(1),DD2N(1,1))
      EQUIVALENCE     (GRIB(1),IGRIB(1))
      EQUIVALENCE     (IPDS(1),PDS(1))
      EQUIVALENCE     (JPDS(1),PDSL(1),IGRIB(2))
      EQUIVALENCE     (CTEMP,ITEMP)
      EQUIVALENCE     (IDPDSC(1),IDPDS(1))
C
      DATA        MODELNH   /Z'00000000034E0000'/
      DATA        SFCMEANRH /Z'0000000005809100'/
      DATA        MSK1      /Z'00000000FFFFFF00'/
      DATA        MSK2      /Z'00000000000000FF'/
      DATA        MSK3      /Z'000000000000FFFF'/
      DATA        MSK4      /Z'0000000000FF0000'/
      DATA        MSKNH     /Z'000000000000084D'/
      DATA        MSKNGM    /Z'0000000000000127'/
      DATA        CAC96HC   /Z'0000000000960000'/
      DATA        BLRHHDS   /Z'346C006400000000'/
      DATA        BLRHGRB   /Z'346B265F00000000'/
      DATA        MSKFFFF   /Z'FFFFFFFF00000000'/
      DATA        LFTFLG    /Z'8365326400000000'/
      DATA        AVPOLRV   /Z'0000000000990000'/
      DATA        AVTYPE    / 130 /
C
C***********************************************************************
C
C
      IRET   = 0
      IERR = 0
      LUGI = LUGRBIX
      LUGB = LUGRB
C
      ITYPE = IAND(LABEL(5),MSK2)
      PRINT *, ' ITYPE=', ITYPE
      IMODEL = IAND(LABEL(6),MSK3)
      FLAG = IAND(LABEL(6),MSK4)
C
C     if (itype .eq. avtype) then
C         print *, ' got a avpolrv or avmerv data type....'
C     endif
C
      IF (FLAG .EQ. AVPOLRV) THEN
          LABEL(6) = IAND(LABEL(6),MSK3)
      ENDIF
C
         IF (IMODEL .EQ. MSKNH) THEN
              LABEL(7) = 0
              LABEL(8) = MODELNH
              IF (FLAG .NE. 0) LABEL(6) = MSKNH
         ELSE IF (IMODEL .EQ. MSKNGM) THEN
              LABEL(6) = 0
              LABEL(7) = 0
              LABEL(8) = 39
         ENDIF
C
              CALL SBYTES(LABELP,LABEL,0,32,0,8)
C             ..... PACK 8-WORD ID TO 4-WORD CRAY ID
              IFLAG    = CHAR(128)
C             .... 128 FOR GDS, 64 FOR BITMAP, 192 FOR BOTH
              ICENT = 20
              ISCALE = 0
              CALL W3FP12(LABELP, IFLAG, IDPDSC, ICENT, ISCALE, IERR)
C             .... CONVERT OFFICE 84' ID TO PDS .....
              IF (IERR .NE. 0) THEN
                  PRINT *, ' **FATAL RETURN FROM W3FP12 =', IERR
                  GO TO 999
              ENDIF
C
              WRITE(6,224)(LABEL(I),I=1,12)
  224         FORMAT(1X,'LABEL=',/,2(1X,6(Z16,1X),/))
              WRITE(6,226)(LABELP(I),I=1,4)
  226         FORMAT(1X,'LABELP=',/,(1X,4(Z16,1X),/))
              WRITE(6,227)(IDPDS(I),I=1,4)
  227         FORMAT(1X,'IDPDS=',/,(1X,4(Z16,1X),/))
         IF (IMODEL .EQ. MSKNH) THEN
              IDPDSC(4) = CHAR(02)
              IDPDSC(6) = CHAR(78)
              IDPDSC(7) = CHAR(03)
         ELSE IF (IMODEL .EQ. MSKNGM) THEN
              IDPDSC(4) = CHAR(02)
              IDPDSC(6) = CHAR(39)
              IDPDSC(7) = CHAR(06)
              IF (ITYPE .EQ. 101) THEN
                 IDPDSC(7) = CHAR(101)
                 PRINT *, ' TRY TO GET A NGM C-GRID'
              ENDIF
              IF (ITYPE .EQ. 104) THEN
                 IDPDSC(7) = CHAR(104)
                 PRINT *, ' TRY TO GET A NGM SUPER C-GRID'
              ENDIF
              ITEMP = IAND(IDPDS(2),MSKFFFF)
              IF (ITEMP .EQ. BLRHHDS) THEN
                 PRINT *,'  **GET NGM BOUNDARY LAYER RH'
                 ITEMP = BLRHGRB
                 IDPDSC(9) = CTEMP(1)
                 IDPDSC(10) = CTEMP(2)
                 IDPDSC(11) = CTEMP(3)
                 IDPDSC(12) = CTEMP(4)
              ELSE IF (ITEMP .EQ. LFTFLG) THEN
                 PRINT *,' **GET NGM SURFACE TO 500 LIFTED INDEX'
C                IDPDSC(12) = CHAR(00)
              ENDIF
         ENDIF
C
              IF ( IAND(LABEL(1),MSK1) .EQ. SFCMEANRH) THEN
C                IDPDSC(7) = CHAR(202)
                 IF (IMODEL .NE. MSKNGM) THEN
                    IDPDSC(11) = CHAR(44)
                 ENDIF
C                print *, ' get a sfc-500 mean rh'
              ENDIF
C             .... MODIFY PDS TO POINT TO 1-DEGREE GRIB
              DO I=1, 28
                 PDS(I) = IDPDSC(I)
              ENDDO
C
      JREW   = 0
      MPDS   = -1
      MPDS(3) = MOVA2I(PDS(7))
      MPDS(5) = MOVA2I(PDS(9))
      MPDS(6) = MOVA2I(PDS(10))
      MPDS(7) = MOVA2I(PDS(11)) * 256 + MOVA2I(PDS(12))
      IF (FLAG .NE. CAC96HC) THEN
         MPDS(14) = MOVA2I(PDS(19))
C     ..... FOR THE FORECAST HOUR CHECKING
      ENDIF
C
      IF (FLAG .EQ. AVPOLRV) THEN
         IDPDSC(7) = CHAR(03)
         MPDS(3) = 3
C     ..... FOR AVPOLARV
      ENDIF
C
C     READ I DEGREE GRIB FILE USING INDEX FILE
C
C     print *,' mpds='
C     write (*,203)(mpds(k),k=1,25)
C 203 format( 5(5(1x,z16),/) )
C     print *,' jgds='
C     write (*,204)(jgds(k),k=1,100)
C 204 format( 20(5(1x,z16),/) )
C
C
      print *, ' Pds = '
      write(*, 12)(ipds(i),i=1,4)
  12  format( 2x, 4(z16,1x))
C
      CALL GETGB1(LUGB,LUGI,MXSIZE,JREW,MPDS,JGDS,
     &      GRIB,KBYTES,KREW,KPDS,KGDS,KBMS,C,IRET)
C
      IF (IRET .NE. 0) THEN
          PRINT *,' **FATAL ERROR FROM GETGB.', IRET
          IERR = IRET
          GO TO 999 
      ENDIF
C
      print *,'after getgb  kpds = ',kpds
      print *, ' Jds = '
      write(*, 16)(jpds(i),i=1,4)
  16  format( 2x, 4(z16,1x))
C
      IF (KPDS(16) .EQ. 0) THEN
         ITAU = KPDS(14)
      ELSE IF (KPDS(16) .EQ. 4) THEN
         ITAU = KPDS(15)
      ELSE IF (KPDS(16) .EQ. 10) THEN
         ITAU = KPDS(14)
      ELSE
         PRINT *,' **ERROR - CAN NOT GET TAU.  KPDS(16)=',KPDS(16)
      ENDIF
C
      print *,' ITAU=', itau
      print *, ' Kds = '
      write(*, 14)(kpds(i),i=1,16)
  14  format( 4(2x, 4(z16,1x),/))
C
C     print *,'after getgb  mpds = ',mpds
C     PRINT *,'KBYTES = ',KBYTES
C     PRINT *,'RECORD NO. OF GRIB RECORD IN INPUT FILE = ',KREW
C
  700 CONTINUE
C
C     flip the grid so 1st point is S.pole at Greenwich
C
      IF (KPDS(3) .EQ. 3) THEN
C        PRINT *, ' GET A ONE DEGREE GRIB'
         DO N1=1,181
            DO M1=1,360
               DD(M1,181-N1+1) = CC(M1,N1)
            ENDDO
         ENDDO
C
         DO N1 = 1, 181
            DD(361,N1) = DD(1,N1)
         ENDDO
C
         IF (ITYPE .EQ. AVTYPE) THEN
C           print *, '  convert from one-degree to 5 degree ...'
            LL1 = 0
            II1 = 0
            DO L1 = 1, 361, 5
               LL1 = LL1 + 1
               DO I1 = 1, 181, 5
                  II1 = II1 + 1
                  GG(LL1,II1) = DD(L1,I1)
               ENDDO
            ENDDO
C           ... LOAD IT TO OUTPUT BUFFER
            DO III=1,2701
               FLD(III) = GG2(III)
            ENDDO
C
         ELSE IF (FLAG .EQ. AVPOLRV) THEN
C           .... CONVERT ONE DEGREE TO 2.5 DEGREE
            CALL W3FT3X2P5(DD,BLOLA,CLOLA)
C           DO NN=1, 5365
C              FLD(NN) = CLOLA(NN)
C           ENDDO
            DO NN=5221, 10585
               FLD(NN-5220) = CLOLA(NN)
            ENDDO
C           print *,' successfully get avpolar field from 1'
C           .... FOR N.H. ONLY
         ELSE
            CALL W3FT43V(DD,EE,1)
C           .... convert one degree to office 84
C           CALL W3FM08(EE,FF,65,65)
C           ... smoother.....
            IF (ITYPE .EQ. 27) THEN
               DO III=1,4225
                  FLD(III) = EE(III)
               ENDDO
C           ELSE IF (ITYPE .EQ. 26) THEN
C              MAPIN = 27
C              MAPOUT = 26
C              INTERP = 2
C              CALL W3FT32(EE, MAPIN, FLD, MAPOUT, INTERP, IRET)
C              .... CONVERT FROM 65*65 TO 53*57
C              IF (IRET .NE. 0) THEN
C                IERR = IRET + 10
C                PRINT *, ' FATAL ERROR FROM W3FT32 -- ', IERR
C                GO TO 999
C              ENDIF
            ELSE
               PRINT *, ' ** UNRECOGNIZED DATA TYPE -- ', ITYPE
               IERR = 10
               GO TO 999
            ENDIF
         ENDIF
C
      ELSE IF (KPDS(3) .EQ. 2) THEN
C        PRINT *, ' GET A 2.5 DEGREE GRIB'
         DO N1=1,73
            DO M1=1,144
               DD2(M1,73-N1+1) = CC2(M1,N1)
            ENDDO
         ENDDO
C
         DO N1 = 1, 73
            DD2(145,N1) = DD2(1,N1)
         ENDDO
C
         IF (ITYPE .EQ. AVTYPE) THEN
C           print *, '  convert from 2.5-degree to 5 degree ...'
            LL1 = 0
            II1 = 0
            DO L1 = 1, 145, 2
               LL1 = LL1 + 1
               DO I1 = 1, 73, 2
                  II1 = II1 + 1
                  GG(LL1,II1) = DD2(L1,I1)
               ENDDO
            ENDDO
C           ... LOAD IT TO OUTPUT BUFFER
            DO III=1,2701
               FLD(III) = GG2(III)
            ENDDO
         ELSE IF (FLAG .EQ. AVPOLRV) THEN
            DO N1=37, 73
               DO M1=1,145
                  DD2N(M1,N1-36) = DD2(M1,N1)
               ENDDO
            ENDDO
C           .... LOAD NH 2.5 DEGREE
            DO NN=1, 5365
               FLD(NN) = CLOLA(NN)
            ENDDO
C           print *,' successfully get avpolar field from 2.5.'
         ELSE
            DO N1=37, 73
               DO M1=1,145
                  DD2N(M1,N1-36) = DD2(M1,N1)
               ENDDO
            ENDDO
C           ..... N.H.
            CALL W3FT05V(DD2N,EE,1)
C           .... convert 2.5 degree to office 84 65*65
C           CALL W3FM08(EE,FF,65,65)
C           ... smoother.....
            IF (ITYPE .EQ. 27) THEN
               DO III=1,4225
                  FLD(III) = EE(III)
               ENDDO
C           ELSE IF (ITYPE .EQ. 26) THEN
C              MAPIN = 27
C              MAPOUT = 26
C              INTERP = 2
C              CALL W3FT32(EE, MAPIN, FLD, MAPOUT, INTERP, IRET)
C              .... CONVERT FROM 65*65 TO 53*57
C              IF (IRET .NE. 0) THEN
C                IERR = IRET + 10
C                PRINT *, ' FATAL ERROR FROM W3FT32 -- ', IERR
C                GO TO 999
C              ENDIF
            ELSE
               PRINT *, ' ** UNRECOGNIZED DATA TYPE -- ', ITYPE
               IERR = 10
               GO TO 999
            ENDIF
         ENDIF
C
      ELSE IF (KPDS(3) .EQ.27 .AND. ITYPE .EQ. 27) THEN
          DO III=1, 4225
             FLD(III) = C(III)
          ENDDO
      ELSE IF (KPDS(3) .EQ. 6 .AND. ITYPE .EQ. 26) THEN
          DO III=1, 2385
             FLD(III) = C(III)
          ENDDO
      ELSE IF (KPDS(3) .EQ. 26 .AND. ITYPE .EQ. 26) THEN
          DO III=1, 2385
             FLD(III) = C(III)
          ENDDO
      ELSE IF (KPDS(3) .EQ. 101 .AND. ITYPE .EQ. 101) THEN
          DO III=1, 10283
             FLD(III) = C(III)
          ENDDO
      ELSE IF (KPDS(3) .EQ. 104 .AND. ITYPE .EQ. 104) THEN
          DO III=1, 16170
             FLD(III) = C(III)
          ENDDO
      ELSE
           PRINT *,' GRID ID TYPE =', KPDS(3)
           PRINT *,' **ABEND, CANNOT PROCESS THIS GRID TYPE**'
           IERR = 7
           GO TO 999
      ENDIF
C
C     write(*,810)(buff(i),i=1,1440)
C810  format( 180(8(f10.3,2x),/))
C
C
      CALL W3FP11 (IGRIB,IGRIB(2),MTITLE,IRET)
C     .... to get a map title
      IF (IRET .NE. 0) THEN
         PRINT *, ' ** NOT A FATAL ERROR FROM W3FP11 -- ', IRET
         IERR = 101
      ENDIF
C
C     print *,' '
C     print *,title(1:86)
C     print *,' '
C
         CGRIB(1:1) = CHAR(71)
         CGRIB(2:2) = CHAR(82)
         CGRIB(3:3) = CHAR(73)
         CGRIB(4:4) = CHAR(66)
         CGRIB(5:5) = CHAR(0)
         CGRIB(6:6) = CHAR(0)
         CGRIB(7:7) = CHAR(0)
         CGRIB(8:8) = CHAR(1)
C
C        PDS(7) = CHAR(27)
         GRIB(15) = CHAR(27)
C
         CALL W3FP13(CGRIB,JPDS,LIDREC,IRET)
         IF (IRET .NE. 0) THEN
            PRINT *,' ERROR RETURN FROM W3FP13:', IRET
            IERR = IRET
         ELSE
            CALL GBYTES(LIDREC,LABEL,0,32,0,12)
            LABEL(1) = IOR(LABEL(1),ITAU)
C           print *,' label after ='
C           write (*,224)(label(k),k=1,12)
         ENDIF
C
C
  999 CONTINUE
      RETURN
      END
      SUBROUTINE W3FT3X2P5(ALOLA,BLOLA,CLOLA)
C$$$  SUBROUTINE DOCUMENTATION BLOCK  ***
C
C SUBROUTINE: W3FT3X2P5        1 DEG. TO 2.5 DEG GLOBAL GRID
C   AUTHOR:  JONES,R.E.        ORG:  W342         DATE: 85-04-10
C
C ABSTRACT:  CONVERT 361 * 181 1 DEG. LAT., LON. GLOBAL GRID TO 1.25
C   DEG. (289 * 145) GLOBAL GRID, AND THEN TO 2.5 DEG. (145 * 73)
C   GLOBAL GRID.
C
C PROGRAM HISTORY LOG:
C   85-04-10  R.E.JONES   VECTORIZED VERSION OF W3FT05
C   89-10-21  R.E.JONES   CHANGES TO INCREASE SPEED
C   91-05-03  R.E.JONES   CHANGE  TO CRAY CFT77 FORTRAN
C   91-05-03  R.E.JONES   CHANGE TO SiliconGraphic 3.3 FORTRAN 77
C   93-03-29  R.E.JONES   ADD SAVE STATEMENT
C
C USAGE:  CALL W3FT3X1P25(ALOLA,BLOLA,CLOLA)
C
C   INPUT ARGUMENTS:  ALOLA  - 361*181 GRID 1.0 DEG. LAT,LON GLOBAL GRID,
C                              65341 POINTS. 360 * 181 ONE DEGREE
C                              GRIB GRID 3 WAS FLIPPED, GREENWISH ADDED
C                              TO RIGHT SIDE TO MAKE 361 * 181. 
C
C   INPUT FILES:  NONE
C
C   OUTPUT ARGUMENTS: BLOLA - 289*145 GRID 1.25 DEG. GLOBAL GRID.
C                             41905 POINTS.
C
C   OUTPUT ARGUMENTS: CLOLA - 145*73 GRID 2.5 DEG. GLOBAL GRID.
C                             10585 points. 
C
C   OUTPUT FILES: ERROR MESSAGE TO FORTRAN OUTPUT FILE
C
C   WARNINGS:
C
C   SUBPROGRAMS CALLED:
C     UNIQUE :  NONE
C
C     LIBRARY:  ASIN , ATAN2
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
       PARAMETER   (II=289,JJ=145,NPTS=II*JJ)
       PARAMETER   (III=361,JJJ=181,IPTS=III*JJJ)
C
       REAL        ALOLA(III,JJJ), BLOLA(NPTS), CLOLA(*)
       REAL        W1(NPTS),       W2(NPTS),    ERAS(NPTS,4)
       REAL        XDELI(NPTS),    XDELJ(NPTS)
       REAL        XI2TM(NPTS),    XJ2TM(NPTS)
C
       INTEGER     IV(NPTS),       JV(NPTS),    JY(NPTS,4)
       INTEGER     IM1(NPTS),      IP1(NPTS),   IP2(NPTS)
C
       SAVE
C
       DATA  ISWT  /0/
C
      IF  (ISWT.EQ.1)  GO TO  2100
C
      DEGIN  = 1.00
      DEGOUT = 1.25
      RDEG   = DEGOUT / DEGIN
C
      KK = 0
      DO J = 1,145
        XJIN   = (J-1) * RDEG + 1.0
        DO I = 1,289
          KK      = KK + 1
          W1(KK)  = (I-1) * RDEG + 1.0
          W2(KK)  = XJIN
        END DO
      END DO
C
      ISWT   = 1
C
 1000 CONTINUE
        DO 1100 K = 1,NPTS
          IV(K)    = W1(K)
          JV(K)    = W2(K)
          XDELI(K) = W1(K) - IV(K)
          XDELJ(K) = W2(K) - JV(K)
          IP1(K)   = IV(K) + 1
          JY(K,3)  = JV(K) + 1
          JY(K,2)  = JV(K)
 1100   CONTINUE
C
      DO 1200 K = 1,NPTS
        IP2(K)   = IV(K) + 2
        IM1(K)   = IV(K) - 1
        JY(K,1)  = JV(K) - 1
        JY(K,4)  = JV(K) + 2
        XI2TM(K) = XDELI(K) * (XDELI(K) - 1.0) * .25
        XJ2TM(K) = XDELJ(K) * (XDELJ(K) - 1.0) * .25
 1200 CONTINUE
C
      DO 1300 KK = 1,NPTS
         IF (IV(KK).EQ.1) THEN
           IP2(KK) = 3
           IM1(KK) = III-1
         ELSE IF (IV(KK).EQ.(III-1)) THEN
           IP2(KK) = 2
           IM1(KK) = III-2
         ENDIF
 1300 CONTINUE
C
 1400 CONTINUE
C
      DO 1500 KK = 1,NPTS
        IF (JV(KK).LT.2.OR.JV(KK).GT.(JJJ-2)) XJ2TM(KK) = 0.0
 1500 CONTINUE
C
      DO 1600 KK = 1,NPTS
        IF (IP2(KK).LT.1)   IP2(KK) = 1
        IF (IM1(KK).LT.1)   IM1(KK) = 1
        IF (IP2(KK).GT.III) IP2(KK) = III
        IF (IM1(KK).GT.III) IM1(KK) = III
 1600 CONTINUE
C
 1700 CONTINUE
      DO 1800 KK = 1,NPTS
        IF (IV(KK).LT.1)    IV(KK)  = 1
        IF (IP1(KK).LT.1)   IP1(KK) = 1
        IF (IV(KK).GT.III)  IV(KK)  = III
        IF (IP1(KK).GT.III) IP1(KK) = III
 1800 CONTINUE
C
      DO 1900 KK = 1,NPTS
        IF (JY(KK,2).LT.1)   JY(KK,2) = 1
        IF (JY(KK,2).GT.JJJ) JY(KK,2) = JJJ
        IF (JY(KK,3).LT.1)   JY(KK,3) = 1
        IF (JY(KK,3).GT.JJJ) JY(KK,3) = JJJ
 1900 CONTINUE
C
      DO 2000 KK = 1,NPTS
        IF (JY(KK,1).LT.1)   JY(KK,1) = 1
        IF (JY(KK,1).GT.JJJ) JY(KK,1) = JJJ
        IF (JY(KK,4).LT.1)   JY(KK,4) = 1
        IF (JY(KK,4).GT.JJJ) JY(KK,4) = JJJ
 2000 CONTINUE
C
 2100 CONTINUE
C
C     QUADRATIC INTERPOLATION
C
      DO 2400 KK = 1,NPTS
        ERAS(KK,1)=(ALOLA(IP1(KK),JY(KK,1))-ALOLA(IV(KK),JY(KK,1)))
     &            * XDELI(KK) + ALOLA(IV(KK),JY(KK,1)) +
     &            ( ALOLA(IM1(KK),JY(KK,1)) - ALOLA(IV(KK),JY(KK,1))
     &            - ALOLA(IP1(KK),JY(KK,1))+ALOLA(IP2(KK),JY(KK,1)))
     &            * XI2TM(KK)
        ERAS(KK,2)=(ALOLA(IP1(KK),JY(KK,2))-ALOLA(IV(KK),JY(KK,2)))
     &            * XDELI(KK) + ALOLA(IV(KK),JY(KK,2)) +
     &            ( ALOLA(IM1(KK),JY(KK,2)) - ALOLA(IV(KK),JY(KK,2))
     &            - ALOLA(IP1(KK),JY(KK,2))+ALOLA(IP2(KK),JY(KK,2)))
     &            * XI2TM(KK)
        ERAS(KK,3)=(ALOLA(IP1(KK),JY(KK,3))-ALOLA(IV(KK),JY(KK,3)))
     &            * XDELI(KK) + ALOLA(IV(KK),JY(KK,3)) +
     &            ( ALOLA(IM1(KK),JY(KK,3)) - ALOLA(IV(KK),JY(KK,3))
     &            - ALOLA(IP1(KK),JY(KK,3))+ALOLA(IP2(KK),JY(KK,3)))
     &            * XI2TM(KK)
        ERAS(KK,4)=(ALOLA(IP1(KK),JY(KK,4))-ALOLA(IV(KK),JY(KK,4)))
     &            * XDELI(KK) + ALOLA(IV(KK),JY(KK,4)) +
     &            ( ALOLA(IM1(KK),JY(KK,4)) - ALOLA(IV(KK),JY(KK,4))
     &            - ALOLA(IP1(KK),JY(KK,4))+ALOLA(IP2(KK),JY(KK,4)))
     &            * XI2TM(KK)
 2400      CONTINUE
C
       DO 2500 KK = 1,NPTS
         BLOLA(KK) = ERAS(KK,2) + (ERAS(KK,3) - ERAS(KK,2))
     &             * XDELJ(KK)  + (ERAS(KK,1) - ERAS(KK,2)
     &             - ERAS(KK,3) + ERAS(KK,4)) * XJ2TM(KK)
 2500  CONTINUE
C
      CALL CUT2P5(BLOLA,CLOLA)
C
      RETURN
      END
      SUBROUTINE CUT2P5(BLOLA,CLOLA)
C
      REAL   BLOLA(289,145)
      REAL   CLOLA(*)      
C
      SAVE
C
C     CUT 1.25 DEG. GRID TO 2.5 DEG. GRID
C
      IJOUT = 0
      DO J = 1,145,2
        DO I = 1,289,2
          IJOUT        = IJOUT + 1
          CLOLA(IJOUT) = BLOLA(I,J)
        END DO
      END DO
C
      RETURN
      END
