      SUBROUTINE CNTOR(ZIN,IMZIN,JMZIN,NDVD,NDIV,Z,MAXZ,L,MAXL,
     &                 SHADIV,IRETUR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    CNTOR       CONTOURS ZIN(IMZIN,JMZIN)
C   PRGMMR: KRISHNA KUMAR      ORG: W/NP12    DATE: 1999-07-01
C
C ABSTRACT: CONTOURS THE INPUT FIELD ZIN(IMZIN,JMZIN) INTO IMAGE PLANE
C
C PROGRAM HISTORY LOG:
C   89-06-28  ORIGINAL AUTHOR LUKE LIN
C   94-11-01  HENRICHSEN ADD LOGIC TO CLEAR THE EDGES OF A FIELD.
C   94-12-20  LUKE LIN      CONVERT IT TO CFT-77.
C   96-04-22  LUKE LIN      MODIFY FOR SHADING.  
C   96-05-21  LUKE LIN      MODIFY FOR CONTOURING 129*129
C   96-06-04  LUKE LIN      MODIFY SHADING LOGIC AT THE EDGE FOR OPEN CONTOURS
C   96-06-14  LUKE LIN      CHANGE DASH LOGIC FROM INPUT
C   96-08-13  LUKE LIN      MODIFY THE WINDOW BOUNDARY FOR 129*129
C   96-09-18  LUKE LIN      MODIFY TO FIX LINE WEIGHT.
C   96-09-23  LUKE LIN      MODIFY TO ADD THE INTERVAL FOR SHADING.
C   96-03-12  LUKE LIN      INCREASE MAXPLOT TO 8000 FROM 4000 AND ADD
C                           CONTOUR RANGE CONTROL OPTIONS
C   96-03-21  LUKE LIN      MODIFY FOR BIG 250MB CONSTANT PRESSURE CHART
C 1999-07-01  KRISHNA KUMAR CONVERTED FROM CRAY VERSION TO IBM RS/6000
C                           ASSIGNED PROPER VALUES FOR XINDEF/INDEFF 
C                           CALCULATED USING RANGE FUNCTION FOR IBM
C                           RS/6000 
C
C USAGE:   CALL CNTOR(ZIN,IMZIN,JMZIN,NDVD,NDIV,Z,MAXZ,L,MAXL,IRETUR)
C   INPUT ARGUMENT LIST:
C     ZIN      - ZIN(IMZIN,JMZIN) CONTAINS GRID DATA.
C     IMZIN    - I DIMENSION OF ZIN ARRAY.
C     JMZIN    - J DIMENSION OF ZIN ARRAY.
C     NDVD     - INTERPOLATION OPTION
C              - =1   LINEAR INTERPOLATION
C              - =2   BI-QUADRATIC INTERPOLATION
C              - =3   CUBIC INTERPOLATION
C     NDIV     - FURTHER DIVISIONS OF A GRID INTERVAL
C              - =2,3,4,5,6 INTERPOLATE ORIGINAL ONE GRID INTO (NDIV)
C              - SUBGRID AND TRACE EVERY CONTOUR LINE
C     Z        - Z(MAXZ) WORKING ARRAY
C     MAXZ     - DIMENSION OF Z ARRAY
C     L        - L(MAXL) WORKING ARRAY
C     MAXL     - DIMENSION OF L ARRAY
C     SHADFG   - FLAG FOR SHAGING
C     SHADMK   - MASK CONSTANTS
C     SHADIV   - THE SKIP INTERVAL FOR SHADING
C
C   OUTPUT ARGUMENT LIST:
C     IRETUR   - RETURN CONDITION
C              - =0, NORMAL
C              - =2, LACK OF WORKING BIN TO INTERPOLATE
C              - 5 - FATAL ERR -- NOT ENOUGH WORKING BUFFER FOR OUT-DGN
C              - 933, NONFATAL ERR -- ATTEMP TO ADD BAD ELEMENT TO DGN
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
      PARAMETER (MAXPLT=8000, MAXDIM=8000)
C
      COMMON / CLRFLD / CLRLAT,CLRLOL,CLRUPR,LCLEAR,ECLEAR,NCLEAR
C
      REAL         CLRLAT
      INTEGER      CLRLOL(2),CLRUPR(2)
C
      LOGICAL      LCLEAR
      LOGICAL      NCLEAR
      LOGICAL      ECLEAR
C
      COMMON  /POLE/ XPOL,YPOL,GDTYPE
      INTEGER     GDTYPE
C     ...THE POLE POSITION IN GRID(65,65) IS AT GRID(33,33).
C     ...THE POLE POSITION IN GRID(53,45) IS AT GRID(27,49).
C
      REAL         ZIN  (IMZIN ,JMZIN),
     1             XP   (MAXPLT), YP   (MAXPLT)
      INTEGER      NI   (MAXPLT), NJ   (MAXPLT)
      LOGICAL      L1   (MAXDIM)
      LOGICAL      L    (MAXL, *)
      REAL         Z    (MAXZ, *)
      REAL       B(20,4), B1(20,3), B2(20,3)
      EQUIVALENCE  (XP(1), NI(1)),  (YP(1), NJ(1))
C
      COMMON / LNATTR / LNWEIGHT, LNSTYLE, LNCOLOR
      INTEGER      LNWEIGHT
      INTEGER      LNSTYLE
      INTEGER      LNCOLOR
C
      REAL         XINDEF
C
      LOGICAL      LDRAW, LHACHI
      LOGICAL      LL0,LL1,LL2,LL3,LLA,LLB,LLC,LLD,LLCENT
      INTEGER      NSHD,  NHCH
C
      REAL         XSHAD(3), YSHAD(3)
      REAL         CON1,CON2
C
      COMMON / DASH / LDOUBLE,DASHFG,DASHMK(2),IDASH,SHADNO,SHADMK(20)
      LOGICAL      LDOUBLE
      LOGICAL      DASHFG
      INTEGER      DASHMK
      INTEGER      IDASH
      INTEGER      SHADNO
      INTEGER      SHADMK
C
      INTEGER      SHADIV
C
      COMMON /RANG/   RANGFG, ICBEG, ICEND
      LOGICAL         RANGFG
      INTEGER         ICBEG, ICEND
C
      DATA  EPS / 1.E-50 /
      DATA  XINDEF   /1.0E307 /
C
C------------------------------------------------------------------
C
C        print *, ' imzin=', imzin
C        print *, ' jmzin=', jmzin
C        print *, ' maxz=', maxz
C        print *, ' maxl=', maxl
         print *, ' ndiv=', ndiv
         print *, ' ndvd=', ndvd
C
C        do i=1,129
C           print *,' zin,i=',imzin,' ',zin(imzin,i)
C           print *,' zin,j=',jmzin,' ',zin(i,jmzin)
C           print *,' zin,i=1',' ',zin(1,i)
C           print *,' zin,j=1',' ',zin(i,1)
C           print *,' zin,i=66',' ',zin(66,i)
C           print *,' zin,j=66',' ',zin(i,66)
C        enddo
c
         IF ( SHADNO .GT. 0 ) THEN
             NSHD = SHADNO
         ELSE 
             NSHD = 0
         ENDIF
C        .... FOR SHADING OPTIONS
C
         LNCOLOR = 3
         LNSTYLE = 0
         IF (LDOUBLE) THEN
            LNWEIGHT = 2
         ELSE
            LNWEIGHT = 0
         ENDIF
C
         IF (DASHFG) THEN
             LNSTYLE = 7
             IF (DASHMK(1) .LE. 3) THEN
                LNSTYLE = 1
             ELSE IF (DASHMK(1) .LE. 5) THEN
                LNSTYLE = 2
             ELSE IF (DASHMK(1) .EQ. 6) THEN
                LNSTYLE = 3
             ELSE IF (DASHMK(1) .LE. 10) THEN
                LNSTYLE = 4
             ELSE IF (DASHMK(1) .LE. 12) THEN
                LNSTYLE = 5
             ELSE IF (DASHMK(1) .LE. 20) THEN
                LNSTYLE = 6
             ELSE IF (DASHMK(1) .LE. 32) THEN
                LNSTYLE = 7
             ENDIF
C            print *, ' ** put lnstyle eq ',lnstyle
         ENDIF
C          ... FOR LINE INFORMATION
         NDV = NDIV
         NINTP = NDVD
C
         IMZ = (IMZIN-1)*NDV+1
         JMZ = (JMZIN-1)*NDV+1
         IF(IMZ.GT.MAXDIM.OR.JMZ.GT.MAXDIM) THEN
            PRINT *,'***ABEND - INCREASE MAXDIM OF SUBR CNTOR ***'
            IRETUR = 2
            RETURN
         ENDIF
C
         DDX = 1.0/FLOAT(NDV)
         DDY = 1.0/FLOAT(NDV)
C
C     ***  COPY  SOURCE DATA (ZIN) TO THE WORKING AREA  ***
         DO 50 J=1,JMZIN
            JJ=(J-1)*NDV+1
            DO 50 I=1,IMZIN
               II=(I-1)*NDV+1
               Z(II,JJ) = ZIN(I,J)
  50     CONTINUE
      IF(NDV .EQ. 1) GO TO 300
C     ***  INTERPOLATION FROM  ZIN(IMZIN,JMZIN)  TO  Z(IMZ,JMZ)  ***
      DO 80 N=1,NDV-1
             X = FLOAT(N) / FLOAT(NDV)
             IF (NINTP .EQ. 2) THEN
C                *** BI-QUADRATIC INTERPOLATION WEIGHT ***
                 B (N,1) = 0.25*X*(X-1.)
                 B (N,2) = -(X-1.)*(1.+0.25*X)
                 B (N,3) = X*(1.-0.25*(X-1.))
                 B (N,4) = 0.25*X*(X-1.)
             ELSE
C                *** CUBIC INTERPOLATION WEIGHT ***
                 B (N,1) = -      X*(X-1.)*(X-2.) / 6.
                 B (N,2) = (X+1.)  *(X-1.)*(X-2.) * 0.5
                 B (N,3) = - (X+1.)*X     *(X-2.) * 0.5
                 B (N,4) = (X+1.)*X*(X-1.)        / 6.
             ENDIF
             IF (NINTP .GE. 2) THEN
C                *** QUADRATIC INTERPOLATION WEIGHT ***
                 B1(N,1) = (X-1.)*(X-2.) * 0.5
                 B1(N,2) = -X   *(X-2.)
                 B1(N,3) = X*(X-1.)      * 0.5
                 X1 = X + 1.
                 B2(N,1) =  (X1-1.)*(X1-2.) * 0.5
                 B2(N,2) = - X1    *(X1-2.)
                 B2(N,3) = X1*(X1-1.)       * 0.5
             ENDIF
  80  CONTINUE
      DO 150 I=1,IMZIN-1
         IF(I .EQ .1) THEN
            DO 100 N=1,NDV-1
               II = (I-1)*NDV + N+1
               DO 90 J=1,JMZIN
                  JJ = (J-1)*NDV + 1
                  Z(II,JJ) = ZIN(I,J)*B1(N,1) + ZIN(I+1,J)*B1(N,2)
     *                      +ZIN(I+2,J)*B1(N,3)
  90          CONTINUE
 100        CONTINUE
         ELSE IF(I.GT.1 .AND. I.LT.IMZIN-1) THEN
            DO 120 N=1,NDV-1
               II = (I-1)*NDV + N+1
               DO 110 J=1,JMZIN
                  JJ = (J-1)*NDV + 1
                  Z(II,JJ) = ZIN(I-1,J)*B(N,1) + ZIN(I,J)*B(N,2)
     *                      +ZIN(I+1,J)*B(N,3) + ZIN(I+2,J)*B(N,4)
 110          CONTINUE
 120        CONTINUE
         ELSE IF(I. EQ. IMZIN-1) THEN
            DO 140 N=1,NDV-1
               II = (I-1)*NDV + N+1
               DO 130 J=1,JMZIN
                  JJ = (J-1)*NDV + 1
                  Z(II,JJ) = ZIN(I-1,J)*B2(N,1) + ZIN(I,J)*B2(N,2)
     *                      +ZIN(I+1,J)*B2(N,3)
 130          CONTINUE
 140        CONTINUE
         ENDIF
 150  CONTINUE
      DO 250 J=1,JMZIN-1
         IF(J .EQ. 1) THEN
            J1=(J-1)*NDV+1
            J2=J1+NDV
            J3=J2+NDV
            DO 180 N=1,NDV-1
               JJ = (J-1)*NDV + N+1
               DO 170 II=1,IMZ
                  Z(II,JJ) = Z(II,J1)*B1(N,1) + Z(II,J2)*B1(N,2)
     *                      +Z(II,J3)*B1(N,3)
 170           CONTINUE
 180        CONTINUE
         ELSE IF(J.GT.1 .AND. J.LT.JMZIN-1) THEN
            J1=(J-2)*NDV+1
            J2=J1+NDV
            J3=J2+NDV
            J4=J3+NDV
            DO 200 N=1,NDV-1
               JJ = (J-1)*NDV + N+1
               DO 190 II=1,IMZ
                  Z(II,JJ) = Z(II,J1)*B(N,1) + Z(II,J2)*B(N,2)
     *                      +Z(II,J3)*B(N,3) + Z(II,J4)*B(N,4)
 190           CONTINUE
 200        CONTINUE
         ELSE IF(J .EQ. JMZIN-1) THEN
            J1=(J-2)*NDV+1
            J2=J1+NDV
            J3=J2+NDV
            DO 220 N=1,NDV-1
               JJ = (J-1)*NDV + N+1
               DO 210 II=1,IMZ
                  Z(II,JJ) = Z(II,J1)*B2(N,1) + Z(II,J2)*B2(N,2)
     *                      +Z(II,J3)*B2(N,3)
 210           CONTINUE
 220        CONTINUE
         ENDIF
 250  CONTINUE
 300  CONTINUE
C
C     *****   DRAWING  CONTOUR  LINES  START  HERE  *****
C
        IMZ2 = IMZ*2 - 1
C
        ZMAX = -1.E 70
        ZMIN = 1.E 70
        DO 310 J=1,JMZ
           DO 310 I=1,IMZ
              ZMAX = MAX (Z(I,J), ZMAX)
              ZMIN = MIN (Z(I,J), ZMIN)
 310    CONTINUE
        NCS = INT (ZMIN)
        NCE = INT (ZMAX)
C
           WRITE(6,FMT='('' CNTOR: NCS='',I6,'' NCE='',I6)')NCS,NCE
C
C
        IF (GDTYPE .EQ. 27) THEN
          IF (IMZIN.EQ.65 .AND. JMZIN.EQ.65) THEN
             ISML = (46-1) * NDV +1
             JSML = (51-1) * NDV +1
             ISKP = (10-1) * NDV + 1
             JSKP = (8-1) * NDV +1
             ICOR = (13-1) * NDV +1
             CALL INDEFF(Z,IMZ,JMZ,ISML,JSML,
     1                 ISKP,JSKP,ICOR,XINDEF)
          ELSE IF (IMZIN.EQ.129 .AND. JMZIN.EQ.129) THEN
             ISML = (92-1) * NDV +1
             JSML = (102-1) * NDV +1
             ISKP = (20-1) * NDV + 1
             JSKP = (16-1) * NDV +1
             ICOR = (26-1) * NDV +1
             IF ( .NOT. NCLEAR) THEN
                CALL INDEFF(Z,IMZ,JMZ,ISML,JSML,
     1                    ISKP,JSKP,ICOR,XINDEF)
             ENDIF
          ENDIF
        ELSE
C
C        CHECK TO SEE IF THIS FIELD NEEDS TO BE CLEARED
C        AROUND THE EDGES.
C
          IF (ECLEAR)THEN
           MKEY   =  2
           WRITE(6,FMT='('' CNTOR: CLEARING EDGES OF FIELD WITH'',
     1        '' SUB FIELD CORNERS AT:'',4I4)')CLRLOL,CLRUPR
              CALL CLREDG(Z,XINDEF,CLRLOL,CLRUPR,MKEY,
     1                   IMZ,JMZ,IRTN)
          ELSE
          ENDIF
        ENDIF
C
      IF (RANGFG) THEN
C        .... CONTOUR RANGE CONTROL
         IF (ICBEG .GT. NCS) NCS = ICBEG
         IF (ICEND .LT. NCE) NCE = ICEND
         WRITE(6,FMT='('' CNTOR: NCS='',I6,'' NCE='',I6)')NCS,NCE
      ENDIF
C
      DO 900 NC = NCS, NCE
C
         ZC =  NC
         DO J=1,JMZ
            DO I=1,IMZ
              IF(Z(I,J) .GT. ZC) THEN
                   L1(I) = .TRUE.
               ELSE
                   L1(I) = .FALSE.
               ENDIF
            ENDDO
            IF (GDTYPE .EQ.27 .OR. ECLEAR) THEN
               DO  I=1,IMZ-1
                  IF (Z(I,J) .EQ. XINDEF) THEN
                     L(2*I-1,J) = .FALSE.
                     L(2*I,J) = .FALSE.
C                    L(2*I+1,J) = .FALSE.
                  ELSE
                     L(2*I,J) = L1(I) .NEQV. L1(I+1)
                  ENDIF
               ENDDO
            ELSE
               DO  I=1,IMZ-1
                     L(2*I,J) = L1(I) .NEQV. L1(I+1)
               ENDDO
            ENDIF
         ENDDO
         DO 380 I=1,IMZ
            II=2*I-1
            DO 350 J=1,JMZ
               IF(Z(I,J) .GT. ZC)  THEN
                  L1(J) = .TRUE.
               ELSE
                  L1(J) = .FALSE.
               ENDIF
 350        CONTINUE
            IF (GDTYPE .EQ.27 .OR. ECLEAR) THEN
                   DO 360 J=1,JMZ-1
                      IF (Z(I,J) .EQ. XINDEF) THEN
                         L(II,J) = .FALSE.
                         L(II-1,J) = .FALSE.
                         IF (J .GT. 1) L(II,J-1) = .FALSE.
C                        L(II-1,J-1) = .FALSE.
                      ELSE
                         L(II,J) = L1(J) .NEQV. L1(J+1)
                      ENDIF
 360               CONTINUE
            ELSE
               DO 365 J=1,JMZ-1
                     L(II,J) = L1(J) .NEQV. L1(J+1)
 365           CONTINUE
            ENDIF
 380     CONTINUE
C
         DO 800 J=1,JMZ
            IF(J .NE. JMZ) THEN
              IS = 0
              IE = IMZ2 + 1
            ELSE
              IS = 2
              IE = IMZ2 - 1
            ENDIF
         DO 800 ITEMP=IS,IE,2
            I = ITEMP
            IF(ITEMP .EQ. 0) I = 1
            IF(ITEMP .EQ. IMZ2+1) I=IMZ2
            IF(.NOT. L(I,J)) GO TO 800
C
C           ***  FINDING STARTING POINT OF CONTOUR (Z = ZC)  ***
            N = 1
            NI(1) = I
            NJ(1) = J
            IP = I
            JP = J
            IF(J.EQ.1 .OR. J.EQ.JMZ) THEN
                L(I,J) = .FALSE.
            ENDIF
            IF(I.EQ.1 .OR. I.EQ.IMZ2) THEN
                L(I,J) = .FALSE.
            ENDIF
            IPASS = 0
            JPASS = 0
C
 400        CONTINUE
C           ***  FINDING SUCCESSIVE POINT ON CONTOUR (Z = ZC)  ***
            IF(MOD(IP,2) .EQ. 0) THEN
                IZ = IP/2 + 1
                JZ = JP
                IF(JP .GE. 2 .AND. JPASS .GE. 0) THEN
                   IF(L(IP+1,JP-1).AND.L(IP,JP-1).AND.L(IP-1,JP-1)) THEN
C                     *** SADDLE POINT PROCESSING ***
                      ZCENT = (Z(IZ-1,JZ-1) + Z(IZ,JZ-1) +
     *                         Z(IZ-1,JZ ) + Z(IZ,JZ )) * 0.25
                      JJ = JP-1
                      IF (ZCENT.GT.ZC .NEQV. Z(IZ,JZ).GT.ZC) THEN
                         II=IP+1
                         GO TO 420
                      ELSE
                         II=IP-1
                         GO TO 420
                      ENDIF
                   ELSE
                      II=IP+1
                      JJ=JP-1
                      IF(L(II,JJ)) GO TO 420
                      II=IP
                      IF(L(II,JJ)) GO TO 420
                      II=IP-1
                      IF(L(II,JJ)) GO TO 420
                   ENDIF
                ENDIF
                IF(JP .LE. JMZ-1 .AND. JPASS .LE. 0) THEN
                   IF (L(IP+1,JP).AND.L(IP,JP+1).AND.L(IP-1,JP)) THEN
C                     *** SADDLE POINT PROCESSING ***
                      ZCENT = (Z(IZ-1,JZ ) + Z(IZ,JZ ) +
     *                         Z(IZ-1,JZ+1) + Z(IZ,JZ+1)) * 0.25
                      JJ = JP
                      IF (ZCENT.GT.ZC .NEQV. Z(IZ,JZ).GT.ZC) THEN
                         II=IP+1
                         GO TO 420
                      ELSE
                         II=IP-1
                         GO TO 420
                      ENDIF
                   ELSE
                      II=IP-1
                      JJ=JP
                      IF(L(II,JJ)) GO TO 420
                      II=IP
                      JJ=JP+1
                      IF(L(II,JJ)) GO TO 420
                      II=IP+1
                      JJ=JP
                      IF(L(II,JJ)) GO TO 420
                   ENDIF
                ENDIF
C
            ELSE
                IZ = (IP+1) / 2
                JZ = JP
                IF(IP .LT. IMZ2 .AND. IPASS .GE. 0) THEN
                   IF (L(IP+1,JP).AND.L(IP+2,JP).AND.L(IP+1,JP+1)) THEN
C                     *** SADDLE POINT PROCESSING ***
                      ZCENT = (Z(IZ ,JZ ) + Z(IZ+1,JZ ) +
     *                         Z(IZ ,JZ+1) + Z(IZ+1,JZ+1)) * 0.25
                      II = IP+1
                      IF (ZCENT.GT.ZC .NEQV. Z(IZ,JZ).GT.ZC) THEN
                         JJ=JP
                         GO TO 420
                      ELSE
                         JJ=JP+1
                         GO TO 420
                      ENDIF
                   ELSE
                      II=IP+1
                      JJ=JP+1
                      IF(L(II,JJ)) GO TO 420
                      II=IP+2
                      JJ=JP
                      IF(L(II,JJ)) GO TO 420
                      II=IP+1
                      IF(L(II,JJ)) GO TO 420
                   ENDIF
                ENDIF
                IF(IP .GT. 1 .AND. IPASS .LE. 0) THEN
                   IF (L(IP-1,JP).AND.L(IP-2,JP).AND.L(IP-1,JP+1)) THEN
C                     *** SADDLE POINT PROCESSING ***
                      ZCENT = (Z(IZ-1,JZ ) + Z(IZ ,JZ ) +
     *                         Z(IZ-1,JZ+1) + Z(IZ ,JZ+1)) * 0.25
                      II = IP-1
                      IF (ZCENT.GT.ZC .NEQV. Z(IZ,JZ).GT.ZC) THEN
                         JJ=JP
                         GO TO 420
                      ELSE
                         JJ=JP+1
                         GO TO 420
                      ENDIF
                   ELSE
                      II=IP-1
                      JJ=JP
                      IF(L(II,JJ)) GO TO 420
                      II=IP-2
                      IF(L(II,JJ)) GO TO 420
                      II=IP-1
                      JJ=JP+1
                      IF(L(II,JJ)) GO TO 420
                   ENDIF
                ENDIF
            ENDIF
C
            NMAX = N
            IF( NI(1).EQ.NI(NMAX) .AND. NJ(1).EQ.NJ(NMAX)
     1                                  .OR.  .NOT.L(I,J) )  THEN
C              ***  SUCCESSIVE POINT CAN NOT BE FOUND  ***
               GO TO 430
C
            ELSE
C              ***  ANOTHER BRANCH FROM POINT (I,J) EXISTS  ***
C              ***  REVERSE THE ORDER OF THE POINTS  ***
               II = NI(1)
               IP = NI(2)
               JJ = NJ(1)
               JP = NJ(2)
               NMAX2 = NMAX / 2
               DO 410 N=1,NMAX2
                   NEXC = (NMAX+1) - N
                   IEXC=NI(N)
                   NI(N)=NI(NEXC)
                   NI(NEXC)=IEXC
                   JEXC=NJ(N)
                   NJ(N)=NJ(NEXC)
                   NJ(NEXC)=JEXC
 410           CONTINUE
               N = NMAX - 1
            ENDIF
C
 420        CONTINUE
C           ***  (II,JJ) IS THE SUCCESSIVE POINT  ***
            N = N+1
      IF (N.GT.MAXPLT) THEN
         PRINT *,'***ABEND - INCREASE MAXPLT IN SUBR COTOR***'
         IRETUR = 2
         RETURN
      ENDIF
            NI(N) = II
            NJ(N) = JJ
            L(II,JJ) = .FALSE.
            IF(II .GT. IP) THEN
              IPASS =  1
            ELSE
              IPASS = -1
            ENDIF
            IF(JJ .LE. JP) THEN
               JPASS =  1
            ELSE
               JPASS = -1
            ENDIF
            IP = II
            JP = JJ
            GO TO 400
C
 430        CONTINUE
            IF(NMAX .EQ. 1) GO TO 700
C
C           ***  CONVERSION FROM (TOPOLOGICAL) TO (REAL) CONTOUR  ***
      IF (NMAX.GT.MAXPLT) THEN
         PRINT *,'***ABEND - INCREASE MAXPLT OF SUBR CNTOR ***'
         IRETUR = 2
         RETURN
      ENDIF
            DO 550 N=1,NMAX
               IIX = (NI(N)+1) / 2
               JJY = NJ(N)
               X = IIX - 1
               Y = JMZ - JJY
               IF(MOD(NI(N),2) .EQ. 0) THEN
                  XX = Z(IIX+1,JJY) - Z(IIX,JJY)
                  IF(ABS(XX) .GT. EPS)  THEN
                     X = X + (ZC - Z(IIX,JJY)) / XX
                  ELSE
                     X = X + 0.5
                  ENDIF
               ELSE
                  YY = Z(IIX,JJY+1) - Z(IIX,JJY)
                  IF(ABS(YY) .GT. EPS)  THEN
                     Y = Y - (ZC - Z(IIX,JJY)) / YY
                  ELSE
                     Y = Y - 0.5
                  ENDIF
               ENDIF
C              ***  SCALING THE COORDINATE (X, Y)  ***
               XP(N) = X*DDX + 1.0
               YP(N) = Y*DDY + 1.0
 550       CONTINUE
C
 600       CONTINUE
C          ***  PLOTTING CONTOUR (Z = ZC) WITHOUT ITS VALUE ***
           LINEP = LINEVU
           LINEVU = NC
C          PRINT 610,NMAX,ZC,LINEVU
 610       FORMAT(1H ,'   GET A NEW CONTOUR: ',I6,F8.1,2X,I6)
           CALL PLTVEC(XP,YP,NMAX,IRETUR)
C           .... PLOT VECTORS
 700       CONTINUE
C
 800     CONTINUE
 900  CONTINUE
C
      LINEP = LINEVU
C
C--------------------------------------------------------------
C
      IF (NSHD .LE. 0) RETURN
C     ....SHADING OPTIONS.....
C     ----------------------------------------------------------------
C
C     PRINT *,' ============ FOR SHADING ==============='
      IDASH=1
      X0 = 0.0
      Y0 = 0.0
      LNCOLOR = 4
      LNSTYLE = 2
      LNWEIGHT = 0
C     ... FOR LINE INFORMATION
C
C
      DO 1500 II=1, NSHD
         TMP1 = FLOAT(SHADMK(2*II))
         TMP2 = FLOAT(SHADMK(2*II-1))
         IF (TMP1 .GT. TMP2) THEN
             CON1 = TMP2
             CON2 = TMP1
         ELSE 
             CON1 = TMP1
             CON2 = TMP2
         ENDIF
C        PRINT *,' CON1/2=', CON1, ' ', CON2
         LHACHI = .TRUE.
C
         NHCH = 1 
C
C     ----------------------------------------------------------------
C
           DO 1420 J = NHCH+1, JMZ-1, SHADIV
C          DO 1420 J = NHCH+1, JMZ-1, NHCH
                N = 0
C               ... test the open edge
                Z0 = Z(1,J)
C               IF (Z0.GT.CON1 .AND. Z0.LE.CON2) THEN
C                  N = 1
C                  XP(N) = 0
C                  PRINT *,' GET A BEGIN OPEN EDGE'
C               ENDIF
C
                DO 1422 I=2,IMZ
                       Z0 = Z(I-1,J)
                       Z1 = Z(I  ,J)
                       IF (Z0.GT.CON1  .NEQV.  Z1.GT.CON1) THEN
                           XX = Z1 - Z0
                           IF (ABS(XX) .GT. EPS) THEN
                               XX = (CON1 - Z0) / XX
                           ELSE
                               XX = 0.5
                           END IF
                           N = N+1
                           IF (N.GT.MAXPLT) THEN
                              PRINT *,'*INCREASE MAXPLT OF SUBR CNTOR*'
                              IRETN=1
                              RETURN
                           ENDIF
                           XP(N) = XX + (I-2)
                       END IF
                       IF (Z0.GT.CON2  .NEQV.  Z1.GT.CON2) THEN
                           XX = Z1 - Z0
                           IF (ABS(XX) .GT. EPS) THEN
                               XX = (CON2 - Z0) / XX
                           ELSE
                               XX = 0.5
                           END IF
                           N = N+1
                           IF (N.GT.MAXPLT) THEN
                              PRINT *,'*INCREASE MAXPLT OF SUBR CNTOR*'
                              IRETN=1
                              RETURN
                           ENDIF
                           XP(N) = XX + (I-2)
                       END IF
C               ... test the open edge
                   IF (I .EQ. IMZ) THEN
                      Z0 = Z(IMZ-1,J)
                      IF (Z0.GT.CON1 .AND. Z0.LE.CON2) THEN
C                          PRINT *, ' GET A END OPEN EDGE '
                           N = N+1
                           IF (N.GT.MAXPLT) THEN
                              PRINT *,'*INCREASE MAXPLT OF SUBR CNTOR*'
                              IRETN=1
                              RETURN
                           ENDIF
                           XP(N) = (IMZ-1)
                      ENDIF
                   ENDIF
C
 1422           CONTINUE
                NMAX = N
C??             IF (NMAX .GE. 2) THEN
C??                 DO 1424 N=1,NMAX-1
C??                        IF (XP(N) .GT. XP(N+1)) THEN
C??                            XX = XP(N)
C??                            XP(N) = XP(N+1)
C??                            XP(N+1)= XX
C??                        END IF
C??24               CONTINUE
C???            END IF
C
                LDRAW = Z(1,J) .GT. CON1 .AND. Z(1,J) .LE. CON2
C               LDRAW = Z(IMZ,J) .GT. CON1 .AND. Z(IMZ,J) .LE. CON2
C               LDRAW = LDRAW .EQV. LHACHI
                Y     = (JMZ - J) * DDY  +  Y0
                IF (LDRAW) THEN
                   XSHAD(1) = X0 + 1.0
                   YSHAD(1) = Y + 1.0
C                  PRINT *, ' TRUE, START AT OPEN EDGE', XSHAD(1)
C               ELSE
C                  PRINT *, ' FALSE,  NOT START AT OPEN EDGE'
                ENDIF
C
C               PRINT *, ' NMAX----=', NMAX
C               IF (NMAX .GE. 1) THEN
C                  DO K=1,NMAX
C                     PRINT *,' XPOISTION=', XP(K)
C                  ENDDO
C               ENDIF
C
                IF (NMAX .GE. 1) THEN
                    DO 1426 N=1,NMAX
                           X = XP(N) * DDX + X0
                           IF (LDRAW) THEN
                              XSHAD(2) = X + 1.0
                              YSHAD(2) = Y + 1.0
                              CALL PLTVEC(XSHAD,YSHAD,2,IRETUR)
C       print *, 'Ashad a line:',xshad(1),yshad(1),xshad(2),yshad(2)
                              XSHAD(1) = X + 1.0
                              YSHAD(1) = Y + 1.0
                           ELSE
                              XSHAD(1) = X + 1.0
                              YSHAD(1) = Y + 1.0
                           END IF
                           LDRAW = .NOT. LDRAW
 1426               CONTINUE
                END IF
                IF (LDRAW) THEN
                   X = DDX * (IMZ-1)  +  X0
                   XSHAD(2) = X + 1.0
                   YSHAD(2) = Y + 1.0
                   CALL PLTVEC(XSHAD,YSHAD,2,IRETUR)
C       print *, 'Bshad a line:',xshad(1),yshad(1),xshad(2),yshad(2)
                   XSHAD(1) = X + 1.0
                   YSHAD(1) = Y + 1.0
                END IF
 1420      CONTINUE
 1500 CONTINUE
C
      RETURN
      END
