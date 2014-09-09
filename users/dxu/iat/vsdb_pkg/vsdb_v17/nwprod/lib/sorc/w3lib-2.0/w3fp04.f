       SUBROUTINE W3FP04(IFLD,ALAT,ALON,TITLE,IDIM,CMIL,CMIR,
     &                   CMJB,CMJT,INUM,XFAC,IERR)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: W3FP04      PRINT ARRAY OF DATA POINTS AT LAT/LON POINTS
C   AUTHOR: HORODECK, J.     ORG: W324       DATE: 85-07-31
C
C ABSTRACT: GIVEN AN ARRAY OF METEOROLOGICAL DATA AND CORRESPONDING
C   LATITUDE/LONGITUDE POSITION FOR EACH DATA POINT, THESE DATA
C   VALUES ARE PRINTED AT THEIR APPROXIMATE LATITUDE/LONGITUDE
C   POSITIONS ON A POLAR STEREOGRAPHIC PROJECTION.
C
C PROGRAM HISTORY LOG:
C   80-01-15  J. HORODECK
C   85-07-31  R.E.JONES   CHANGE TO CDC FORTRAN 200
C   90-08-15  R.E.JONES   CHANGE TO CRAY CFT77 FORTRAN
C
C USAGE: CALL W3FP04(IFLD,ALAT,ALON,TITLE,IDIM,CMIL,CMIR,CMJB,CMJT,
C                    INUM,XFAC,IERR)
C
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     IFLD   ARG LIST  REAL OR INTEGER FULLWORD ARRAY OF DATA POINTS
C     ALAT   ARG LIST  REAL ARRAY OF LATITUDE POSITIONS (>0 FOR NH,
C                      <0 FOR SH) FOR THE DATA TO BE PLOTTED
C     ALON   ARG LIST  REAL ARRAY OF LONGITUDES (WEST OF GREENWICH)
C     TITLE  ARG LIST  INTEGER SIZE 10 ALPHANUMERIC ARRAY OF
C                      CHARACTERS FOR TITLE TO BE WRITTEN ON PRINTOUT
C     IDIM   ARG LIST  INTEGER NUMBER OF DATA VALUES TO PLOT (SIZE OF
C                      ARRAYS IFLD, ALAT AND ALON)
C     CMIL   ARG LIST  REAL LEFT SIDE OF GRID - MINIMUM COARSE MESH
C                      I COORDINATE (MINIMUM VALUE OF 1.0)
C     CMIR   ARG LIST  REAL RIGHT SIDE OF GRID - MAXIMUM COARSE MESH
C                      I COORDINATE (MAXIMUM VALUE OF 65.0)
C     CMJB   ARG LIST  REAL BOTTOM OF GRID - MINIMUM COARSE MESH
C                      J COORDINATE (MINIMUM VALUE OF 1.0)
C     CMJT   ARG LIST  REAL TOP OF GRID - MAXIMUM COARSE MESH J
C                      COORDINATE (MAXIMUM VALUE OF 65.0)
C     INUM   ARG LIST  INTEGER THREE DIGIT NUMBER FOR THE FOLLOWING:
C                      HUNDREDS DIGIT = TYPE OF DATA
C                        1 = FIXED POINT
C                        2 = FLOATING POINT
C                        3 = ALPHANUMERIC
C                      TENS DIGIT = HEMISPHERIC REFERENCE
C                        0 = NORTHERN HEMISPHERE
C                        1 = SOUTHERN HEMISPHERE
C                      UNITS DIGIT = NUMBER OF CHARACTERS TO PLOT
C                      MINIMUM = 1 CHARACTER
C                      MAXIMUM = 4 CHARACTERS
C     XFAC   ARG LIST  REAL MAP SCALE FACTOR (DESIRED MAP SCALE = XFAC
C                      * 1:30,000,000 (STANDARD NMC 65X65 GRID SCALE))
C
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C            TAPE6     PRINTED MAP WHEN EXECUTION IS SUCCESSFUL
C     IERR   ARG LIST  INTEGER RETURN CODE
C
C   SUBPROGRAMS CALLED:
C     NAMES                                                   LIBRARY
C     ------------------------------------------------------- --------
C     W3FB04 W3FB05 W3AI15                                    W3LIB
C
C   RESTRICTIONS: BECAUSE THIS CODE COULD PRODUCE CONSIDERABLE OUTPUT
C     THE SUBSET OF THE NMC 65X65 GRID WHICH CAN BE PRINTED IS A
C     FUNCTION OF THE MAP SCALE FACTOR, E.G. FOR XFAC=5 THE MAXIMUM
C     RANGE OF I AND J IS 27.0, FOR XFAC=2 THE RANGE IS 64.0.
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY Y-MP8/832
C
C$$$
C
       REAL            ALAT(IDIM), ALON(IDIM)
C
       INTEGER         IFLD(IDIM), TITLE(10)
       INTEGER         LINE(24), IL(17), IR(17), IJU(20), IJL(20)
C
       LOGICAL         A
C
C
       CHARACTER*1     KH(120,77), MEAN(4), KB, KM, KP, LC,
     &                 KK(5,77,20), DATA(4), L1, L2, L3,
     &                 IPOLE(4), KN, KS
       CHARACTER*4     L24, L116
       CHARACTER*8     IFMTT
       CHARACTER*24    FMT1
       CHARACTER*28    FMT2
       CHARACTER*24    FMT4
C
       EQUIVALENCE     (MEAN(1),IMEAN), (DATA(1),LFLD)
       EQUIVALENCE     (RFIELD,IFIELD)
       EQUIVALENCE     (IFMTT,IFMT)
C
       DATA   JJAA /116/
       DATA   JJBB / 77/
       DATA   FMT1 /"(6X,    ('+',I , X),//) "/
       DATA   FMT2 /"(' +',I3,1X,    A1,' +',I3) "/
       DATA   FMT4 /"(//, 6X,    ('+',I , X))"/
       DATA   KB   /' '/, KM/'-'/, KP/'+'/, LC/'X'/
       DATA   L1/'1'/, L2/'2'/, L3/'3'/, L24/'  24'/, L116/' 116'/
       DATA   IPOLE/'P','O','L','E'/,  KN/'N'/, KS/'S'/
C
 1001  FORMAT('1',16X,'PANEL #',I2,' OF ',I2,4X,10A8,/,/)
 1003  FORMAT(6X,116A1)
 2001  FORMAT(///,20X,'UPPER LEFT CORNER--LAT =',F6.2,' LON =',F7.2,'W'
     &        , 3X,'UPPER RIGHT CORNER--LAT =',F6.2,' LON =',F7.2,'W')
 2002  FORMAT(20X,'LOWER LEFT CORNER--LAT =',F6.2,' LON =',F7.2,'W'
     &        , 3X,'LOWER RIGHT CORNER--LAT =',F6.2,' LON =',F7.2,'W')
 2003  FORMAT(/,/, 16X, 'PANEL #', I2, ' OF ', I2, 4X, 10A8)
 9001  FORMAT(/,5X,'CMIL = ',F8.1,' CMIR = ',F8.1,' HIGH AND LOW'
     &         ,' VALUES REVERSED......RETURN......')
 9002  FORMAT(/,5X,'CMJB = ',F8.1,' CMJT = ',F8.1,' HIGH AND LOW'
     &         ,' VALUES REVERSED......RETURN......')
 9003  FORMAT(/,5X,F8.1,' IS ILLEGAL VALUE FOR LOW I. IT IS NOW 1.0')
 9004  FORMAT(/,5X,F8.1,' IS ILLEGAL VALUE FOR HIGH I. IT IS NOW 65.0')
 9005  FORMAT(/,5X,F8.1,' IS ILLEGAL VALUE FOR LOW J. IT IS NOW 1.0')
 9006  FORMAT(/,5X,F8.1,' IS ILLEGAL VALUE FOR HIGH J. IT IS NOW 65.0')
 9007  FORMAT(/,5X,'REQUESTED NUMBER OF CHARACTERS TO PLOT(',I2,' )IS'
     &         ,' NOT ALLOWED. FOUR(4) IS MAXIMUM. THATS ALL YOU GET')
 9008  FORMAT(/,5X,'REQUESTED SUBSET OF 65X65 GRID CANNOT CURRENTLY '
     &         ,'BE PLOTTED WITH MAP SCALE FACTOR',F5.1,/5X,'IF PLOT '
     &         ,'IS NECESSARY, CONTACT JOHN M. HORODECK,ESQ. NMC/DD'
     &         ,'/SEB FOR ASSISTANCE')
 9009  FORMAT(/,5X,I4,' IS INVALID HEMISPHERIC REFERENCE'
     &         , '......RETURN......')
 9010  FORMAT(/,5X,'HUNDREDS DIGIT OF INUM(INUM =',I4,') IS'
     &         , ' INVALID......RETURN......')
C
C    TEST I,J VALUES FOR RANGE AND ORDER
C
         IF  (CMIR.GT.CMIL)  GO TO 1
          IERR = 1
           PRINT 9001, CMIL, CMIR
           RETURN
    1    CONTINUE
         IF  (CMJT.GT.CMJB)  GO TO 2
           IERR = 1
           PRINT 9002, CMJB, CMJT
           RETURN
    2    CONTINUE
         IF  (CMIL.GE.1.0)  GO TO 3
           PRINT 9003, CMIL
           CMIL = 1.0
    3    CONTINUE
         IF  (CMIR.LE.65.0)  GO TO 4
           PRINT 9004, CMIR
           CMIR = 65.0
    4    CONTINUE
         IF  (CMJB.GE.1.0)  GO TO 5
           PRINT 9005, CMJB
           CMJB = 1.0
    5    CONTINUE
         IF  (CMJT.LE.65.0)  GO TO 6
           PRINT 9006, CMJT
           CMJT = 65.0
    6    CONTINUE
C
C    CALCULATE VARIOUS LIMITS
C
         LNUM = MOD(INUM,10)
         NREF = (MOD(INUM,100))/10
C
C    TEST FOR INCORRECT ARGUMENTS PASSED
C
         IF  (LNUM.LE.4)  GO TO 7
           PRINT 9007, LNUM
           LNUM = 4
    7    CONTINUE
         IF  (NREF.LE.1)  GO TO 8
           IERR = 1
           PRINT 9009, NREF
           RETURN
    8    CONTINUE
         IF  ((INUM/100).LE.3)  GO TO 81
           IERR = 1
           PRINT 9010, INUM
           RETURN
   81    CONTINUE
C
         LNUMP1 = LNUM + 1
         I1 = (CMIL-1.0)*XFAC + 1.0
         I2 = (CMIR-1.0)*XFAC + 1.0
         J1 = (CMJB-1.0)*XFAC + 1.0
         J2 = (CMJT-1.0)*XFAC + 1.0
C
C    WILL THIS PLOT BE TOO LARGE?
C
         IF  (((I2-I1).LT.139).AND.((J2-J1).LT.139))  GO TO 9
           IERR = 1
           PRINT 9008, XFAC
           RETURN
    9    CONTINUE
C
         OFFI   = I1 - 1
         OFFJ   = J1 - 1
         JJA    = (I2-I1)*5 + 1
         JJB    = (J2-J1)*4 + 1
         JJAM1  = JJA - 1
         JJBBM1 = JJBB - 1
         JJAAM1 = JJAA - 1
         JJAAPN = JJAA + LNUM
         IBEGIN = LNUMP1 + 1
         IPAGE  = (JJAM1/JJAA) + 1
         JPAGE  = (JJB/JJBB) + 1
         XMESH  = 381.0/XFAC
         XIP    = 32.0*XFAC + 1.0
         XJP    = 32.0*XFAC + 1.0
         IIXIP  = (XIP-OFFI)*5 - 4
         JJXJP  = (XJP-OFFJ)*4 - 3
C
C    PLOT DATA ONE PANEL AT A TIME IN SECTIONS
C
         DO  150  NX=1,IPAGE
         A = .FALSE.
C
C    SET LIMITS OF I TO BE PRINTED
C
           IL(NX) = I1 + (23*(NX-1))
           IF  (NX.NE.IPAGE)  IR(NX) = I1 + (23*NX)
           IF  (NX.EQ.IPAGE)  IR(NX) = I2
           IMAX   = IR(NX) - OFFI
           IMIN   = IL(NX) - OFFI
           M      = 0
C
C    FILL ARRAY WITH VALUES OF I TO BE PRINTED AT TOP OF PAGE
C
           DO  10  I = IMIN,IMAX
             M       = M + 1
             LINE(M) = I
   10      CONTINUE
C
C    CALCULATE WIDTH OF PANEL IN INTEGERS AND
C    CHARACTERS FROM WHICH DETERMINE FORMAT
C    FIELD COUNT AND CONVERT BINARY TO ASCII
C
C    PRINT TOP LINE OF I
C
           LA  = (IMAX-IMIN) + 1
           MMM = (LA*5) - 4
           IF  (LA.EQ.24)  GO TO 13
             CALL W3AI15(LA,IFMT,1,4,KP)
             FMT1(5:8)   = IFMTT(1:4)
             FMT4(9:12)  = IFMTT(1:4)
             CALL W3AI15(MMM,IFMT,1,4,KP)
             FMT2(13:16) = IFMTT(1:4)
             GO TO 16
   13      CONTINUE
             FMT1(5:8)   = L24
             FMT2(13:16) = L116
             FMT4(9:12)  = L24
   16      CONTINUE
           IF  (LA.LT.100) GO TO 19
             FMT1(15:15) = L3
             FMT1(17:17) = L1
             FMT4(19:19) = L3
             FMT4(21:21) = L1
             GO TO 22
   19      CONTINUE
             FMT1(15:15) = L2
             FMT1(17:17) = L2
             FMT4(19:19) = L2
             FMT4(21:21) = L2
   22      CONTINUE
           PRINT 1001, NX, IPAGE, TITLE
           WRITE(6,FMT1) (LINE(N), N=1,LA)
C
C    PREPARE TO PRINT SECTIONS OF EACH PANEL
C
           DO  140  JNX=1,JPAGE
C
C    SET LIMITS OF J TO BE PRINTED
C
             IJU(JNX) = J2 - (19*(JNX-1))
             IF  (JNX.NE.JPAGE) IJL(JNX) = J2 - (19*JNX)
             IF  (JNX.EQ.JPAGE) IJL(JNX) = J1
             JMAX = IJU(JNX) - OFFJ
             JMIN = IJL(JNX) - OFFJ
             JU   = JJB - (4*JMAX-3)
             JL   = JJB - (4*JMIN-3)
             NNN  = (JMAX-JMIN)*4 + 1
C
C    FILL CHARACTER ARRAY WITH BLANKS AND PUT X MARKERS IN CORNERS
C    IF FIRST PANEL BLANK ENTIRE AREA,
C    OTHERWISE TRANSFER FIRST INUM I BYTES TO LARGE ARRAY
C    AND BLANK REMAINING ARRAY
C
             DO  37  J=1,JJBB
               IF  (NX.NE.1)  GO TO 31
                 DO  28  I=1,JJAAPN
                   KH(I,J) = KB
   28            CONTINUE
                 GO TO 37
   31          CONTINUE
               DO  32  I=1,LNUMP1
                 KH(I,J) = KK(I,J,JNX)
   32          CONTINUE
               DO  34  I=IBEGIN,JJAAPN
                 KH(I,J) = KB
   34          CONTINUE
   37        CONTINUE
             IF  (JNX.NE.1) GO TO 40
               KH(1,JJBB) = LC
               KH(MMM,JJBB) = LC
 200   CONTINUE
   40        CONTINUE
             IF  (JNX.NE.JPAGE)  GO TO 50
               KH(1,1) = LC
               KH(MMM,1) = LC
   50        CONTINUE
C
C    LOOP TO PUT DATA IN CHARACTER ARRAY
C
             DO  110  I=1,IDIM
C
C    TEST FOR BAD GEOGRAPHY
C
               IF  ((ABS(ALAT(I)).GT.90.).OR.(ALON(I).LT.0.0).OR.(ALON
     A               (I).GT.360.0))  GO TO 90
C
C    CHANGE LAT,LON TO I,J
C
                 IF  (NREF.EQ.0)  GO TO 51
                   CALL W3FB04(ALAT(I),ALON(I),-XMESH,260.0,DELI,DELJ)
                   GO TO 52
   51            CONTINUE
                   CALL W3FB04(ALAT(I),ALON(I),XMESH,80.0,DELI,DELJ)
   52            CONTINUE
                 XI = XIP + DELI
                 XJ = XJP + DELJ
C
C    POSITION I,J COORDINATES IN CHARACTER ARRAY AND TEST
C    IF VALUES RETURNED ARE WITHIN LIMITS OF MAP AND WITHIN SECTIONS
C
                 II = 1.0 + (XI-OFFI-0.9001)*5.0
                 JJ = 1.0 + (XJ-OFFJ-0.8751)*4.0
                 IW = (JJAAM1*(NX-1)) + 1
                 IX = (JJAAM1*NX) + 1
                 IY = JJB - (JJBBM1*(JNX-1))
                 IF  (JNX.NE.JPAGE)  IZ = JJB - (JJBBM1*JNX)
                 IF  (JNX.EQ.JPAGE)  IZ = 1
                 IF  ((II.LT.1).OR.(II.GT.JJA))  GO TO 100
                 IF  ((JJ.LT.1).OR.(JJ.GT.JJB))  GO TO 100
                 IF  ((II.LT.IW).OR.(II.GT.IX))  GO TO 100
                 IF  ((JJ.GT.IY).OR.(JJ.LT.IZ))  GO TO 100
C
C    WRITE N+POLE IF IN THIS SECTION
C
                 IF  (.NOT.((IIXIP.GE.IW.AND.IIXIP.LE.IX).AND.
     A                 (JJXJP.LE.IY.AND.JJXJP.GE.IZ)))  GO TO 56
                   IIXXP = IIXIP - (JJAAM1*(NX-1))
                   JJXXP = JJXJP - (IZ-1)
                   IF  (NREF.EQ.0)  KH(IIXXP-1,JJXXP) = KN
                   IF  (NREF.EQ.1)  KH(IIXXP-1,JJXXP) = KS
                   KH(IIXXP,JJXXP) = KP
                   DO  53  L=1,4
                     KH(IIXXP+L,JJXXP) = IPOLE(L)
   53              CONTINUE
   56            CONTINUE
C
C    CONVERT CHARACTER ARRAY COORDINATES FROM
C    TOTAL MAP VALUES TO SECTION VALUES
C
                 II = II - (JJAAM1*(NX-1))
                 IF  (JNX.NE.JPAGE)  JJ = JJ - (IZ-1)
C
C    IF SPACE IS OCCUPIED SKIP THIS STATION
C
                 JNUM = LNUM + 1
                 DO  70  IK=1,JNUM
                   IN = IK - 1
                   IF  (KH(II+IN,JJ).EQ.KB)  GO TO 60
                     GO TO 110
   60              CONTINUE
   70            CONTINUE
C
C    PLACE VALUE TO BE PLOTTED IN CHARACTER ARRAY
C
                 IFIELD = IFLD(I)
C
C    TEST FOR TYPE OF DATA
C
                 IF  ((INUM/100).EQ.3)  GO TO 82
                   IF  ((INUM/100).EQ.1)  GO TO 73
                     JFLD = RFIELD
                     GO TO 76
   73              CONTINUE
                   JFLD = IFIELD
   76              CONTINUE
C
C    IF ORIGINALLY FIXED POINT OR HAS BEEN CONVERTED
C    FROM FLOATING POINT TO FIXED POINT
C
                   IF  ((JFLD/10000).GE.1) JFLD = MOD(JFLD,10000)
                   IIABS = IABS(JFLD)
                   CALL W3AI15(IIABS,IMEAN,1,LNUM,KP)
                   IF  (JFLD.LT.0)  KH(II,JJ) = KM
                   IF  (JFLD.GE.0)  KH(II,JJ) = KP
                   DO  79  IA=1,LNUM
                     KH(II+IA,JJ) = MEAN(IA)
   79              CONTINUE
                   GO TO 110
   82            CONTINUE
C
C    FOR ALPHANUMERIC DATA
C
                 LFLD = IFLD(I)
                 KH(II,JJ) = KP
                 DO  85  IQ=1,LNUM
                   KH(II+IQ,JJ) = DATA(IQ)
   85            CONTINUE
   90          CONTINUE
  100          CONTINUE
  110        CONTINUE
             JJN = 0
C
C    PRINT JTH ROW AND VALUES OF J
C
             DO  130  J=JU,JL,4
               JN = NNN - (4*JJN)
               IF  (A)  GO TO 115
                 JX = (JJB-J)/4 + 1
                 WRITE(6,FMT2) JX, (KH(I,JN), I=1,MMM), JX
  115          CONTINUE
               JJN = JJN + 1
               IF  (JN.NE.1)  GO TO 118
C
C    SAVE LAST INUM BYTES OF I
C
                 DO  117  L=1,JJBB
                   DO  116  I=116,JJAAPN
                     IA = I - 115
                     KK(IA,L,JNX) = KH(I,L)
  116              CONTINUE
  117            CONTINUE
                 A = .TRUE.
                 GO TO 140
  118          CONTINUE
               DO  120  IM=1,3
                 JN = JN - 1
                 PRINT 1003, (KH(I,JN), I=1,MMM)
  120          CONTINUE
               A = .FALSE.
  130        CONTINUE
  140      CONTINUE
           WRITE(6,FMT4) (LINE(N), N=1,LA)
C
C    CALCULATE AND PRINT LAT/LON AT CORNERS
C
           AL  = IL(NX)
           AR  = IR(NX)
           XI1 = ((AL-1.0)/XFAC + 1.0) - 33.0
           XI2 = ((AR-1.0)/XFAC + 1.0) - 33.0
           XJ1 = CMJB - 33.0
           XJ2 = CMJT - 33.0
           IF  (NREF.EQ.0)  GO TO 142
             CALL W3FB05(XI1,XJ1,-XMESH,260.0,ALAT1,ALON1)
             CALL W3FB05(XI1,XJ2,-XMESH,260.0,ALAT2,ALON2)
             CALL W3FB05(XI2,XJ2,-XMESH,260.0,ALAT3,ALON3)
             CALL W3FB05(XI2,XJ1,-XMESH,260.0,ALAT4,ALON4)
             GO TO 144
  142      CONTINUE
             CALL W3FB05(XI1,XJ1,XMESH,80.0,ALAT1,ALON1)
             CALL W3FB05(XI1,XJ2,XMESH,80.0,ALAT2,ALON2)
             CALL W3FB05(XI2,XJ2,XMESH,80.0,ALAT3,ALON3)
             CALL W3FB05(XI2,XJ1,XMESH,80.0,ALAT4,ALON4)
  144      CONTINUE
           PRINT 2001, ALAT2, ALON2, ALAT3, ALON3
           PRINT 2002, ALAT1, ALON1, ALAT4, ALON4
           PRINT 2003, NX, IPAGE, TITLE
  150    CONTINUE
         IERR = 0
         RETURN
       END
