      SUBROUTINE BCNTOR(IMAGE,NOWIDTH,NOLINE,ZIN,IMZIN,JMZIN,
     &                 NDVD,NDIV,Z,MAXZ,L,MAXL,IL,IRETUR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    BCNTOR       CONTOURS ZIN(IMZIN,JMZIN)
C   PRGMMR: KRISHNA KUMAR     ORG: W/NP12  DATE:1999-07-01
C
C ABSTRACT: CONTOURS THE INPUT FIELD ZIN(IMZIN,JMZIN) INTO IMAGE PLANE
C
C PROGRAM HISTORY LOG:
C   89-06-28  ORIGINAL AUTHOR LUKE LIN
C   94-11-01  HENRICHSEN    ADD LOGIC TO CLEAR THE EDGES OF A FIELD.
C   94-12-20  LUKE LIN      CONVERT IT TO CFT-77.
C   96-04-22  LUKE LIN      MODIFY FOR SHADING.  
C   96-05-21  LUKE LIN      MODIFY FOR CONTOURING 129*129
C   96-06-04  LUKE LIN      MODIFY SHADING LOGIC AT THE EDGE FOR OPEN CONTOURS
C   96-06-14  LUKE LIN      CHANGE DASH LOGIC FROM INPUT
C   98-05-20  CHRIS CARUSO  A LOGICAL ARRAY OVERWRITE WAS FIXED ON IBM RS/6000 SYSTEM 
C   99-07-20  KRISHNA KUMAR ASSIGNED PROPER VALUES FOR INDEF/XINDEF USING RANGE
C                           FUNCTION AND MODIFIED TO RUN ON IBM RS/6000
C
C USAGE:   CALL BCNTOR(ZIN,IMZIN,JMZIN,NDVD,NDIV,Z,MAXZ,L,MAXL,IRETUR)
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
C
C   OUTPUT ARGUMENT LIST:
C     IRETUR   - RETURN CONDITION
C              - =0, NORMAL
C              - =2, LACK OF WORKING BIN TO INTERPOLATE
C              - 5 - FATAL ERR -- NOT ENOUGH WORKING BUFFER FOR OUTput
C              - 933, NONFATAL ERR -- ATTEMP TO ADD BAD ELEMENT TO output
C
C REMARKS:  NOTE:  THIS VERSION CONTAINS CODE TO HANDLE UNDEFINED DATA
C           AT GRID POINTS.  THE LIBRARY VERSION DOES NOT DO THIS.
C
C ATTRIBUTES:
C   LANGUAGE: IBM fortran 90
C   MACHINE:  IBM
C
C$$$
C
      integer il(15)

      PARAMETER (MAXPLT=4000, MAXDIM=4000)
C
C
      INTEGER       IMAGE(*)
C
      INTEGER       NOLINE
C       ... NO OF SCAN LINES IN THE BCAKGROUND FILE

      INTEGER       NOWIDTH
C       ... THE WIDTH OF THE BACKGROUND FILE IN WORDS
C
C
      COMMON / CLRFLD / CLRLAT,CLRLOL,CLRUPR,LCLEAR,ECLEAR
C
      REAL         CLRLAT
      INTEGER      CLRLOL(2),CLRUPR(2)
C
      LOGICAL      LCLEAR
      LOGICAL      ECLEAR
C
C     COMMON  /POLE/ XPOL,YPOL,GDTYPE
C     INTEGER*4      GDTYPE
C     ...THE POLE POSITION IN GRID(65,65) IS AT GRID(33,33).
C     ...THE POLE POSITION IN GRID(53,45) IS AT GRID(27,49).
C
      REAL         ZIN  (IMZIN ,JMZIN),
     1             XP   (MAXPLT), YP   (MAXPLT)
      INTEGER      NI   (MAXPLT), NJ   (MAXPLT)
      LOGICAL      L1   (MAXDIM)
      LOGICAL      L    (MAXL, *)
      REAL         Z    (MAXZ, *)
      integer      maxl, maxz, imzin, jmzin, ndvd, ndiv
      integer      imz, jmz, imz2
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
      DATA  EPS / 1.E-50 /
      DATA  XINDEF   /1.0E307 /
c      DATA  XINDEF   /Z'7FFFFFFFFFFFFFFF'/

      save
C
C------------------------------------------------------------------
C
        print*,' in bcntor'
        print *, ' imzin=', imzin
        print *, ' jmzin=', jmzin
        print *, ' maxz=', maxz
        print *, ' maxl=', maxl
        print *, ' ndiv=', ndiv
        print *, ' ndvd=', ndvd
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
             ELSEIF (DASHMK(1) .LE. 5) THEN
                LNSTYLE = 2
             ELSEIF (DASHMK(1) .EQ. 6) THEN
                LNSTYLE = 3
             ELSEIF (DASHMK(1) .LE. 10) THEN
                LNSTYLE = 4
             ELSEIF (DASHMK(1) .LE. 12) THEN
                LNSTYLE = 5
             ELSEIF (DASHMK(1) .LE. 20) THEN
                LNSTYLE = 6
             ELSEIF (DASHMK(1) .LE. 32) THEN
                LNSTYLE = 7
             ENDIF
         ENDIF
C          ... FOR LINE INFORMATION
         NDV = NDIV
         NINTP = NDVD
C
        ZMAX = -1.E 70
        ZMIN = 1.E 70
        DO J = 1,JMZIN
          DO I = 1,IMZIN
            IF(ZIN(I,J) .NE. XINDEF) THEN
              ZMAX = MAX (ZIN(I,J), ZMAX)
              ZMIN = MIN (ZIN(I,J), ZMIN)
            ENDIF
          ENDDO
        ENDDO
        NCS = INT (ZMIN)
        NCE = INT (ZMAX)
C
           WRITE(6,FMT='('' BCNTOR:NCS='',I6,'' NCE='',I6)')NCS,NCE
C
         IMZ = (IMZIN-1) * NDV + 1
         JMZ = (JMZIN-1) * NDV + 1
         IF(IMZ.GT.MAXDIM.OR.JMZ.GT.MAXDIM) THEN
            PRINT *,'***ABEND- INCREASE MAXDIM OF SUBR BCNTOR ***'
            IRETUR = 2
            RETURN
         ENDIF
C
         DDX = 1.0/FLOAT(NDV)
         DDY = 1.0/FLOAT(NDV)
C
C     ***  COPY  SOURCE DATA (ZIN) TO THE WORKING AREA  ***
         DO J = 1,JMZIN
           JJ = (J-1) * NDV + 1
           DO I = 1,IMZIN
              II = (I-1) * NDV + 1
              Z(II,JJ) = ZIN(I,J)
           ENDDO
         ENDDO
      IF(NDV .EQ. 1) GO TO 300
C     ***  INTERPOLATION FROM  ZIN(IMZIN,JMZIN)  TO  Z(IMZ,JMZ)  ***
      DO 80 N = 1, NDV - 1
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
      DO 150 I = 1,IMZIN-1
         IF(I .EQ .1) THEN
            DO N = 1,NDV-1
               II = (I-1) * NDV + N + 1
               DO J = 1,JMZIN
                  JJ = (J-1) * NDV + 1
                  Z(II,JJ) = ZIN(I,J)*B1(N,1) + ZIN(I+1,J)*B1(N,2)
     *                      +ZIN(I+2,J)*B1(N,3)
               ENDDO
            ENDDO
         ELSEIF(I.GT.1 .AND. I.LT.IMZIN-1) THEN
            DO N = 1,NDV-1
               II = (I-1) * NDV + N + 1
               DO J = 1,JMZIN
                  JJ = (J-1)*NDV + 1
                  Z(II,JJ) = ZIN(I-1,J)*B(N,1) + ZIN(I,J)*B(N,2)
     *                      +ZIN(I+1,J)*B(N,3) + ZIN(I+2,J)*B(N,4)
               enddo
            enddo
         ELSEIF(I. EQ. IMZIN-1) THEN
            DO N = 1,NDV-1
               II = (I-1) * NDV + N + 1
               DO J = 1,JMZIN
                  JJ = (J-1)*NDV + 1
                  Z(II,JJ) = ZIN(I-1,J)*B2(N,1) + ZIN(I,J)*B2(N,2)
     *                      +ZIN(I+1,J)*B2(N,3)
               enddo
            enddo
         ENDIF
 150  CONTINUE
      DO 250 J = 1,JMZIN-1
         IF(J .EQ. 1) THEN
            J1 = (J-1) * NDV + 1
            J2 = J1 + NDV
            J3 = J2 + NDV
            DO N = 1,NDV-1
               JJ = (J-1) * NDV + N + 1
               DO II = 1,IMZ
                  Z(II,JJ) = Z(II,J1)*B1(N,1) + Z(II,J2)*B1(N,2)
     *                      +Z(II,J3)*B1(N,3)
               enddo
            enddo
         ELSEIF(J.GT.1 .AND. J.LT.JMZIN-1) THEN
            J1 = (J-2) * NDV + 1
            J2 = J1 + NDV
            J3 = J2 + NDV
            J4 = J3 + NDV
            DO N = 1,NDV-1
               JJ = (J-1) * NDV + N + 1
               DO II = 1,IMZ
                  Z(II,JJ) = Z(II,J1)*B(N,1) + Z(II,J2)*B(N,2)
     *                      +Z(II,J3)*B(N,3) + Z(II,J4)*B(N,4)
               enddo
            enddo
         ELSEIF(J .EQ. JMZIN-1) THEN
            J1 = (J-2) * NDV + 1
            J2 = J1 + NDV
            J3 = J2 + NDV
            DO N = 1,NDV-1
               JJ = (J-1) * NDV + N + 1
               DO II = 1,IMZ
                  Z(II,JJ) = Z(II,J1)*B2(N,1) + Z(II,J2)*B2(N,2)
     *                      +Z(II,J3)*B2(N,3)
               enddo
            enddo
         ENDIF
 250  CONTINUE
 300  CONTINUE
C
C       ... REINITIALIZE XINDEF
C
C        IMZ = (IMZIN-1)*NDV+1
C        JMZ = (JMZIN-1)*NDV+1
C       DO J=1,JMZIN
C          DO I=1,IMZIN
C             IF (ZIN(I,J) .EQ. XINDEF) THEN
C                ILOC = (IMZIN-1)*NDV+1
C                JLOC = (JMZIN-1)*NDV+1
C                Z(ILOC,JLOC)= XINDEF
C                Z(ILOC+1,JLOC)= XINDEF
C             ENDIF
C          ENDDO
C       ENDDO
C
c
c ------------------------------------------------------------------
c Interpolation procedure ignored the presence of undefined points.
c Reintroduce undefined points into new data field.
c ------------------------------------------------------------------

      call indeff (zin,imzin,jmzin,z,imz,jmz,ndv,xindef)

      ZMAX = -1.E 70
      ZMIN =  1.E 70
      DO J = 1,JMZIN
        DO I = 1,IMZIN
          IF(ZIN(I,J) .NE. XINDEF) THEN
            ZMAX = MAX (ZIN(I,J), ZMAX)
            ZMIN = MIN (ZIN(I,J), ZMIN)
          ENDIF
        ENDDO
      ENDDO
      NCS = INT (ZMIN)
      NCE = INT (ZMAX)
C       IF (NCS .EQ. 0) THEN
C           NCS =1
C       ENDIF
c      WRITE(6,FMT='('' BCNTOR:NCS='',I6,'' NCE='',I6)')NCS,NCE

C
C     *****   DRAWING  CONTOUR  LINES  START  HERE  *****
C
      IMZ2 = IMZ*2 - 1
C
      DO 900 NC = NCS, NCE
C
         ZC =  NC
         DO J = 1,JMZ
            DO I = 1,IMZ
              IF(Z(I,J) .GT. ZC) THEN
                   L1(I) = .TRUE.
               ELSE
                   L1(I) = .FALSE.
               ENDIF
            ENDDO
            DO I = 1,IMZ-1
              IF (Z(I,J) .EQ. XINDEF) THEN
                 L(2*I-1,J) = .FALSE.
                 L(2*I,J) = .FALSE.
                 L(2*I+1,J) = .FALSE.
              ELSE
                 L(2*I,J) = L1(I) .NEQV. L1(I+1)
              ENDIF
           ENDDO
         ENDDO
         DO 380 I = 1,IMZ
            II = 2 * I - 1
            DO J = 1,JMZ
               IF(Z(I,J) .GT. ZC)  THEN
                  L1(J) = .TRUE.
               ELSE
                  L1(J) = .FALSE.
               ENDIF
            enddo
            DO J = 1,JMZ-1
              IF(Z(I,J) .EQ. XINDEF) THEN
                L(II,J) = .FALSE.
                if(ii.gt.1) then
                  L(II-1,J) = .FALSE.
                endif
                IF(J .GT. 1) then
                  L(II,J-1) = .FALSE.
                  if(ii.gt.1) then
                    L(II-1,J-1) = .FALSE.
                  endif
                endif
              ELSE
                L(II,J) = L1(J) .NEQV. L1(J+1)
              ENDIF
            enddo
 380     CONTINUE
C
         DO 800 J = 1,JMZ
           IF(J .NE. JMZ) THEN
             IS = 0
             IE = IMZ2 + 1
           ELSE
             IS = 2
             IE = IMZ2 - 1
           ENDIF
           DO 801 ITEMP = IS,IE,2
             I = ITEMP
             IF(ITEMP .EQ. 0) I = 1
             IF(ITEMP .EQ. IMZ2+1) I=IMZ2
             IF(.NOT. L(I,J)) GO TO 801
C
C           ***  FINDING STARTING POINT OF CONTOUR (Z = ZC)  ***
C
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
 400         CONTINUE
C
C           ***  FINDING SUCCESSIVE POINT ON CONTOUR (Z = ZC)  ***
C
             IF(MOD(IP,2) .EQ. 0) THEN
               IZ = IP/2 + 1
               JZ = JP
               IF(JP .GE. 2 .AND. JPASS .GE. 0) THEN
                 IF(L(IP+1,JP-1).AND.L(IP,JP-1).AND.
     *              L(IP-1,JP-1)) THEN
C
C                     *** SADDLE POINT PROCESSING ***
C
                    ZCENT = (Z(IZ-1,JZ-1) + Z(IZ,JZ-1) +
     *                       Z(IZ-1,JZ ) + Z(IZ,JZ )) * 0.25
                   JJ = JP-1
                   IF (ZCENT.GT.ZC .NEQV. Z(IZ,JZ).GT.ZC) THEN
                     ii = IP+1
                     GO TO 420
                   ELSE
                     ii = IP-1
                     GO TO 420
                   ENDIF
                 ELSE
                   ii = IP+1
                   jj = JP-1
                   IF(L(II,JJ)) GO TO 420
                   ii = IP
                   IF(L(II,JJ)) GO TO 420
                   ii = IP-1
                   IF(L(II,JJ)) GO TO 420
                 ENDIF
               ENDIF
               IF(JP .LE. JMZ-1 .AND. JPASS .LE. 0) THEN
                 IF (L(IP+1,JP).AND.L(IP,JP+1).AND.L(IP-1,JP)) THEN
C
C                    *** SADDLE POINT PROCESSING ***
C
                   ZCENT = (Z(IZ-1,JZ ) + Z(IZ,JZ ) +
     *                      Z(IZ-1,JZ+1) + Z(IZ,JZ+1)) * 0.25
                   JJ = JP
                   IF (ZCENT.GT.ZC .NEQV. Z(IZ,JZ).GT.ZC) THEN
                     ii = IP + 1
                     GO TO 420
                   ELSE
                     ii = IP - 1
                     GO TO 420
                   ENDIF
                 ELSE
                   ii = IP - 1
                   jj = JP
                   IF(L(II,JJ)) GO TO 420
                   ii = IP
                   jj = JP + 1
                   IF(L(II,JJ)) GO TO 420
                   ii = IP + 1
                   jj = JP
                   IF(L(II,JJ)) GO TO 420
                 ENDIF
               ENDIF
C
             ELSE
               IZ = (IP+1) / 2
               JZ = JP
               IF(IP .LT. IMZ2 .AND. IPASS .GE. 0) THEN
                 IF (L(IP+1,JP).AND.L(IP+2,JP).AND.L(IP+1,JP+1)) THEN
C
C                     *** SADDLE POINT PROCESSING ***
C
                   ZCENT = (Z(IZ ,JZ ) + Z(IZ+1,JZ ) +
     *                      Z(IZ ,JZ+1) + Z(IZ+1,JZ+1)) * 0.25
                   II = IP+1
                   IF (ZCENT.GT.ZC .NEQV. Z(IZ,JZ).GT.ZC) THEN
                     jj = JP
                     GO TO 420
                   ELSE
                     jj = JP+1
                     GO TO 420
                   ENDIF
                 ELSE
                   ii = IP+1
                   jj = JP+1
                   IF(L(II,JJ)) GO TO 420
                   ii = IP+2
                   jj = JP
                   IF(L(II,JJ)) GO TO 420
                   ii = IP+1
                   IF(L(II,JJ)) GO TO 420
                 ENDIF
               ENDIF
               IF(IP .GT. 1 .AND. IPASS .LE. 0) THEN
                 IF (L(IP-1,JP).AND.L(IP-2,JP).AND.L(IP-1,JP+1)) THEN
C
C                     *** SADDLE POINT PROCESSING ***
C
                   ZCENT = (Z(IZ-1,JZ ) + Z(IZ ,JZ ) +
     *                      Z(IZ-1,JZ+1) + Z(IZ ,JZ+1)) * 0.25
                   II = IP-1
                   IF (ZCENT.GT.ZC .NEQV. Z(IZ,JZ).GT.ZC) THEN
                     jj = JP
                     GO TO 420
                   ELSE
                     jj = JP+1
                     GO TO 420
                   ENDIF
                 ELSE
                   ii = IP-1
                   jj = JP
                   IF(L(II,JJ)) GO TO 420
                   ii = IP-2
                   IF(L(II,JJ)) GO TO 420
                   ii = IP-1
                   jj = JP+1
                   IF(L(II,JJ)) GO TO 420
                 ENDIF
               ENDIF
             ENDIF
C
             NMAX = N
             IF( NI(1).EQ.NI(NMAX) .AND. NJ(1).EQ.NJ(NMAX)
     1                                  .OR.  .NOT.L(I,J) )  THEN
C
C              ***  SUCCESSIVE POINT CAN NOT BE FOUND  ***
C
               GO TO 430
C
             ELSE
C
C              ***  ANOTHER BRANCH FROM POINT (I,J) EXISTS  ***
C              ***  REVERSE THE ORDER OF THE POINTS  ***
C
               II = NI(1)
               IP = NI(2)
               JJ = NJ(1)
               JP = NJ(2)
               NMAX2 = NMAX / 2
               DO 410 N = 1,NMAX2
                   NEXC = (NMAX+1) - N
                   IEXC = NI(N)
                   NI(N) = NI(NEXC)
                   NI(NEXC) = IEXC
                   JEXC = NJ(N)
                   NJ(N) = NJ(NEXC)
                   NJ(NEXC) = JEXC
 410           CONTINUE
               N = NMAX - 1
             ENDIF
C
 420         CONTINUE
C
C           ***  (II,JJ) IS THE SUCCESSIVE POINT  ***
C
             N = N + 1
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
 430         CONTINUE
             IF(NMAX .EQ. 1) GO TO 700
C
C           ***  CONVERSION FROM (TOPOLOGICAL) TO (REAL) CONTOUR  ***
C
             IF (NMAX.GT.MAXPLT) THEN
               PRINT *,'***ABEND - INCREASE MAXPLT OF SUBR BCNTOR **'
               IRETUR = 2
               RETURN
             ENDIF
             DO N = 1,NMAX
               IIX = (NI(N) + 1)/ 2
               JJY = NJ(N)
               X   = IIX - 1
               Y   = JMZ - JJY
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
C
C              ***  SCALING THE COORDINATE (X, Y)  ***
C
               XP(N) = X*DDX + 1.0
               YP(N) = Y*DDY + 1.0
             enddo
C
C          ***  PLOTTING CONTOUR (Z = ZC) WITHOUT ITS VALUE ***
c
             PRINT 610,NMAX,ZC
 610         FORMAT('   GET A NEW CONTOUR: ',I6,F8.1)
C
C           .... PLOT VECTORS
C
             CALL BPLTVEC(IMAGE,NOWIDTH,NOLINE,XP,YP,NMAX,IL,IRETUR)
 700         CONTINUE
C
 801       CONTINUE
 800     CONTINUE
 900  CONTINUE
C
C--------------------------------------------------------------
C
      IF (NSHD .LE. 0) RETURN
C     ....SHADING OPTIONS.....
C     ----------------------------------------------------------------
C
      PRINT *,' ============ FOR SHADING ==============='
      IDASH = 1
      X0 = 0.0
      Y0 = 0.0
      LNCOLOR = 4
      LNSTYLE = 2
      LNWEIGHT = 0
C
C     ... FOR LINE INFORMATION
C
      DO 1500 ii = 1, NSHD
         TMP1 = FLOAT(SHADMK(2*II))
         TMP2 = FLOAT(SHADMK(2*II-1))
         IF (TMP1 .GT. TMP2) THEN
             CON1 = TMP2
             CON2 = TMP1
         ELSE 
             CON1 = TMP1
             CON2 = TMP2
         ENDIF
         PRINT *,' CON1/2=', CON1, ' ', CON2
         LHACHI = .TRUE.
C
         NHCH = 1 
C
C     ----------------------------------------------------------------
C
           DO 1420 J = NHCH+1, JMZ-1, NHCH
                N = 0
c
C               ... test the open edge
c
                Z0 = Z(1,J)
C
                DO 1422 I = 2,IMZ
                       Z0 = Z(I-1,J)
                       Z1 = Z(I  ,J)
                       IF (Z0.GT.CON1  .NEQV.  Z1.GT.CON1) THEN
                           XX = Z1 - Z0
                           IF (ABS(XX) .GT. EPS) THEN
                               XX = (CON1 - Z0) / XX
                           ELSE
                               XX = 0.5
                           ENDIF
                           N = N+1
                           IF (N.GT.MAXPLT) THEN
                             PRINT *,'*INCREASE MAXPLT OF SUBR BCNTOR*'
                              IRETN=1
                              RETURN
                           ENDIF
                           XP(N) = XX + (I-2)
                       ENDIF
                       IF (Z0.GT.CON2  .NEQV.  Z1.GT.CON2) THEN
                           XX = Z1 - Z0
                           IF (ABS(XX) .GT. EPS) THEN
                               XX = (CON2 - Z0) / XX
                           ELSE
                               XX = 0.5
                           ENDIF
                           N = N+1
                           IF (N.GT.MAXPLT) THEN
                             PRINT *,'*INCREASE MAXPLT OF SUBR BCNTOR*'
                              IRETN = 1
                              RETURN
                           ENDIF
                           XP(N) = XX + (I-2)
                       ENDIF
c
C               ... test the open edge
c
                   IF (I .EQ. IMZ) THEN
                      Z0 = Z(IMZ-1,J)
                      IF (Z0.GT.CON1 .AND. Z0.LE.CON2) THEN
C                          PRINT *, ' GET A END OPEN EDGE '
                           N = N+1
                           IF (N.GT.MAXPLT) THEN
                             PRINT *,'*INCREASE MAXPLT OF SUBR BCNTOR*'
                              IRETN = 1
                              RETURN
                           ENDIF
                           XP(N) = (IMZ-1)
                      ENDIF
                   ENDIF
C
 1422           CONTINUE
                NMAX = N
C
                LDRAW = Z(1,J) .GT. CON1 .AND. Z(1,J) .LE. CON2
                Y     = (JMZ - J) * DDY  +  Y0
                IF (LDRAW) THEN
                   XSHAD(1) = X0 + 1.0
                   YSHAD(1) = Y + 1.0
                ENDIF
C
                IF (NMAX .GE. 1) THEN
                    DO 1426 N = 1,NMAX
                           X = XP(N) * DDX + X0
                           IF (LDRAW) THEN
                              XSHAD(2) = X + 1.0
                              YSHAD(2) = Y + 1.0
                              CALL BPLTVEC(IMAGE,NOWIDTH,NOLINE,
     &                                    XSHAD,YSHAD,2,IL,IRETUR)
                              XSHAD(1) = X + 1.0
                              YSHAD(1) = Y + 1.0
                           ELSE
                              XSHAD(1) = X + 1.0
                              YSHAD(1) = Y + 1.0
                           ENDIF
                           LDRAW = .NOT. LDRAW
 1426               CONTINUE
                ENDIF
                IF (LDRAW) THEN
                   X = DDX * (IMZ-1)  +  X0
                   XSHAD(2) = X + 1.0
                   YSHAD(2) = Y + 1.0
                   CALL BPLTVEC(IMAGE,NOWIDTH,NOLINE,XSHAD,YSHAD,
     &                         2,IL,IRETUR)
                   XSHAD(1) = X + 1.0
                   YSHAD(1) = Y + 1.0
                ENDIF
 1420      CONTINUE
 1500 CONTINUE
      RETURN
      END
