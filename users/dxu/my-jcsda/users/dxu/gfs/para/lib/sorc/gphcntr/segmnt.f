      SUBROUTINE SEGMNT(XA,YA,IS,IN,XD,YD,MPT,NOFF,DSEG,IOPT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    SEGMNT      DERIVE NEW NODE VECTORS.
C   PRGMMR: KRISHNA KUMAR      ORG: W/NP12  DATE: 1999-07-01
C
C ABSTRACT: DERIVE NEW NODE VECTORS ACCORDING TO THE OPTIONS FLAG.
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  ORIGINAL AUTHOR RALPH JONES
C   94-12-14  HENRICHSEN    ADD DOCBLOCK.
C   94-12-30  LUKE LIN      CONVERT IT TO CFT-77.
C   96-07-24  LUKE LIN      INCREASE BUFFER TO CONTAIN 4000 VERTICES.
C 1999-07-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM RS/6000
C
C USAGE:    CALL SEGMNT(XA,YA,IS,IN,XD,YD,MPT,NOFF,DSEG,IOPT)
C   INPUT ARGUMENT LIST:
C     INARG1   - GENERIC DESCRIPTION, INCLUDING CONTENT, UNITS,
C     INARG2   - TYPE.  EXPLAIN FUNCTION IF CONTROL VARIABLE.
C     XA(IN)   - REAL*4 X COORDINATE OF RAW (ORIGINAL) NODES
C     YA(IN)   - REAL*4 Y COORDINATE OF RAW (ORIGINAL) NODES
C     IS       - INTEGER*4 THE STARTING INDICES IN THE RAW NODE VECTOR
C     IN       - INTEGER*4 THE ENDING INDICES IN THE RAW NODE VECTOR
C     NOFF     - INTEGER*4 NUMBER OF RAW NODES LEFT UNADJUSTED
C              - AT THE BEGINNING AND END OF VECTOR
C     IOPT     - INTEGER* OPTINS FOR DERIVING NEW NODES WHERE:
C              - IOPT = 0   DERIVE SET OF EQUALLY
C              - SPACED (DSEG) NODES.
C              - IOPT > 0   RETAIN MODULO IOPT NODES
C              - FROM ORIGINAL SET.
C              - IOPT = 1 RETURNS ORIGINAL)
C              - IOPT < 0   SAME AS IOPT = 0 BUT DSEG
C              - VARIES WITH CONTOUR CURVATURE (NOT IMPLIMENTED)
C
C
C   OUTPUT ARGUMENT LIST:
C     XD(IN)   - REAL*4 X COORDINATE OF DERIVED (SEGMENTED) NODES.
C     YD(IN)   - REAL*4 Y COORDINATE OF DERIVED (SEGMENTED) NODES.
C     MPT      - INTEGER*4 NUMBER OF DERIVED NODES
C     DSEG     - REAL*4 INPUT: LENGTH OF INTERNODE SEGMENT
C              - IN UNITS OF MESH LENGTH
C
C   OUTPUT FILES:
C     FT06F001 - INCLUDE IF ANY PRINTOUT
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
C
      COMMON   /THINN / NOTHIN,MIDIST,MINPTS,RADIUS
C
C     ... NOTHIN: NO OF TIME TO BE THINNED; MIDIST: MIN. DISTANCE
C     ... BETWEEN TWO PTS;  MINPTS: MIN. PTS TO BE PROCESS.
C
      REAL XA(IN),YA(IN)    !  INPUT: COORDS OF RAW (ORIGINAL) NODES
      REAL XD(IN),YD(IN)    ! OUTPUT: COORDS OF DERIVED (SEGMENTED) NODE
C
      INTEGER   IS,IN      !  INPUT: THE STARTING AND ENDING INDICES
C                                    IN THE RAW NODE VECTORS
      INTEGER   MPT        ! OUTPUT: NUMBER OF DERIVED NODES
      INTEGER   NOFF       !  INPUT: NUMBER OF RAW NODES LEFT UNADJUSTED
C                                    AT THE BEGINNING AND END OF VECTOR
C
      REAL DSEG            !  INPUT: LENGTH OF INTERNODE SEGMENT
C                          !         IN UNITS OF MESH LENGTH
C
      INTEGER   IOPT       ! OPTIONS FOR DERIVING NEW NODES:
C
C                               IOPT = 0   DERIVE SET OF EQUALLY
C                                          SPACED (DSEG) NODES.
C                               IOPT > 0   RETAIN MODULO IOPT NODES
C                                          FROM ORIGINAL SET.
C                                          (IOPT = 1 RETURNS ORIGINAL)
C                               IOPT < 0   SAME AS IOPT = 0 BUT DSEG
C                                          VARIES WITH CONTOUR
C                                          CURVATURE (NOT IMPLIMENTED)
C
      LOGICAL   LCULL(4000)
      LOGICAL   ORG_CULL,FLIP
C
      REAL SN(4000),S2(4000),R(4000)
C
      LOGICAL   LOOK       ! SWITCH TO TURN ON/OFF DIAGNOSTICS
C
C
C
      LOOK = .FALSE.
      RAD_LIM = RADIUS
C
C
C
C     PRINT 500, IS,IN,IOPT,NOFF
C 500 FORMAT (//,' ----- CALLING SEGMENT: IS =',I4,'  IN =',I4,
C    1           '   IOPT =',I3,'   NOFF =',I3,/)
C
C
C*****************************************************************
C               FIND LENGTH OF THIS RAW CONTOUR
C*****************************************************************
C
      IST = IS+NOFF
      NST = IST-1
      INP = IN+1
      IN1 = IN-1
      INE1 = IN-NOFF
      INE2 = INE1-1
      INE = INE1+1
      IS1 = IS-1
      ISP = IS+1
C
      DLENMX = -1.0E+5
      DLENMN = -DLENMX
      MPT = 0
      MPMAX = 4000
      NPT = IN-IS+1
      NPMAX = 3*NOFF
      CLEN = 0.0
C
      DO I=IS,IN1  ! COMPUTE RAW SEGMENT LENGTHS AND SUM FOR CONT LENGTH
        DXD = XA(I+1)-XA(I)
        DYD = YA(I+1)-YA(I)
        DLEN = SQRT(DXD**2+DYD**2)
          IF(DLEN.GT.DLENMX) DLENMX = DLEN
          IF(DLEN.LT.DLENMN) DLENMN = DLEN
        CLEN = CLEN+DLEN
C
        LCULL(I) = .TRUE.
        SN(I) = DLEN
C
      ENDDO
C
      LCULL(IN) = .TRUE.
C
C       IF(LOOK) PRINT 200, NPT,CLEN,DLENMX,DLENMN
C 200   FORMAT ('  (RAW)  NPT=',I3,/,' ',
C    1              8X,'LENGTH = ',F10.1,'   MAX SEG = ',F8.2,
C    2                 '   MIN SEG =',F8.2)
C
C
C...  IF THIS CONTOUR IS TOO SHORT TO ADJUST, RETURN TO CALLING PRGM
C
      IF(NPT.LE.NPMAX) THEN
C       PRINT 501, NPT
        M = 0
        DO I=IS,IN
          M = M+1
          XD(M) = XA(I)
          YD(M) = YA(I)
        ENDDO
        MPT = M
        GO TO 2000
      END IF
C
  501 FORMAT (//,' CONTOUR TOO SHORT FOR S.R. SEGMENT:  NPT =',I3,/)
C
C*****************************************************************
C   DERIVE CONSTANT INTERVAL (APPROX) PLOT POINTS FROM RAW
C   CONTOUR LINE COORDINATES
C*****************************************************************
C
C
      S = 0.0
      B = 0.0
      D = DSEG
C
C
C... FILL BEGINNING OF DERIVED NODE VECTOR
C
      M = 0
      DO I=IS,NST
        M = M+1
        XD(M) = XA(I)    ! USE FIRST NOFF RAW NODES TO START
        YD(M) = YA(I)    ! DERIVED NODE VECTOR.
      ENDDO
C
C
C-------------------------------------------------------------------
C        DERIVE A NEW NODE VECTOR ACCORDING TO THE METHOD
C                     PRESCRIBED BY IOPT.
C-------------------------------------------------------------------
C
      IF(IOPT.EQ.0) THEN   ! SEGMENT THIS CONTOUR.
C
        N = NST
C
    8   CONTINUE
        SX = XA(N+1)-XA(N)
        SY = YA(N+1)-YA(N)
        SA = SQRT(SX**2+SY**2)
        S = SA
C
    9   CONTINUE
        BLAST = B
        B = S-D
        IF(B) 10,11,12
C
   10     CONTINUE
            N = N+1
            IF(N.EQ.INE1) THEN     ! USE RAW NODE NOFF FROM THE END
              M = M+1
              XD(M) = XA(N)
              YD(M) = YA(N)
              GO TO 14
            END IF
            D = -B
            GO TO 8
C
   11     CONTINUE
            M = M+1
            IF(M.GT.MPMAX) GO TO 14
            N = N+1
            XD(M) = XA(N)
            YD(M) = YA(N)
            IF(N.EQ.INE) GO TO 14
            GO TO 8
C
   12     CONTINUE
            DXL = D*SX/SA
            DYL = D*SY/SA
            IF(BLAST.GT.0.0) THEN
              XD(M+1) = XD(M)+DXL
              YD(M+1) = YD(M)+DYL
            ELSE
              XD(M+1) = XA(N)+DXL
              YD(M+1) = YA(N)+DYL
            END IF
            M = M+1
            IF(M.GT.MPMAX) GO TO 14
            S = B
            D = DSEG
            GO TO 9
C
   14   CONTINUE
C
      ELSE
C
        IF(IOPT.GT.0) THEN     ! KEEP FIRST AND LAST NOFF RAW NODES
C                                AND EVERY NMODTH NODE IN BETWEEN.
          NMOD = IOPT
          M = NOFF
          N = NST
C
          DO 16 I=N,INE1,NMOD
            M = M+1
            XD(M) = XA(I)
            YD(M) = YA(I)
   16     CONTINUE
C
        END IF
C
      END IF
C
C
C
C... IF IOPT < 0, CULL NODES ACCORDING TO CONTOUR CURVATURE
C
      IF(IOPT.LT.0) THEN
C
        M = NOFF
C
        DO I=NST,INE2
          DXD = XA(I+2)-XA(I)
          DYD = YA(I+2)-YA(I)
          DLEN = SQRT(DXD**2+DYD**2)
          S2(I) = DLEN
        ENDDO
C
        DO I=NST,INE2
C
          IP = I+1
          SIDE_SUM = SN(I)+SN(I+1)
          DIFF = SIDE_SUM-S2(I)
          IF(DIFF.LT.1.0E-6) THEN
            LCULL(IP) = .FALSE.
            GO TO 2002
          END IF
C
          CALL INSCRIBE (SN(I),SN(I+1),S2(I),R(IP))
C         PRINT 202, IP,SN(I),SN(I+1),S2(I),SIDE_SUM,DIFF,R(IP)
  202     FORMAT (' IP=',I3,'  SN1,SN2,S2:',3F8.4,
     1            '  SID,DIF,R:',F8.4,F10.5,F8.0)
C
          IF(R(IP).GT.RAD_LIM) THEN
            LCULL(IP) = .FALSE.
          END IF
C
 2002   CONTINUE
C
        ENDDO
C
C
C... RESTORE SELECTED "CULLED" NODES TO "KEEP" STATUS IF TOO MANY
C      CONSEQUITIVE NODES HAVE BEEN CHOSEN FOR CULLING
C
        KMOD = 5
        FLIP = .FALSE.
C
        DO I=IST,INE1
          ORG_CULL = LCULL(I)
          IF(LCULL(I)) THEN    ! NODE WILL BE "KEPT"
            IF(FLIP) FLIP = .FALSE.
          ELSE
            IF(.NOT.FLIP) THEN
              FLIP = .TRUE.
              NM = 0
            END IF
            NM = NM+1
            KEEP = MOD(NM,KMOD)
            IF(KEEP.EQ.0) LCULL(I) = .TRUE.
          END IF
C
C         PRINT 221, I,ORG_CULL,LCULL(I)
C 221     FORMAT (' ',10X,'I =',I4,2L4)
C
        ENDDO
C
C
        DO I=IST,INE1    ! CULL FLAGGED RAW NODES FROM DERIVED VECTOR
          IF(LCULL(I)) THEN
            M = M+1
            XD(M) = XA(I)
            YD(M) = YA(I)
          END IF
        ENDDO
C
      END IF
C
C
C...  FILL END OF DERIVED NODE VECTOR WITH LAST NOFF RAW NODES
C
      DO I=INE,IN
        M = M+1
        XD(M) = XA(I)
        YD(M) = YA(I)
      ENDDO
C
      MPT = M
C
C... DUMP RAW AND DERIVED NODES TO CHECK RESULTS
C
      M = 0
      K = 0
      DO I=IS,IN
        K = K+1
        IF(LCULL(I)) THEN
          M = M+1
C         PRINT 510, K,I,XA(I),YA(I),M,XD(M),YD(M)
C       ELSE
C         PRINT 510, K,I,XA(I),YA(I)
        END IF
      ENDDO
C
  510 FORMAT ('      K =',I4,2(I5,2F7.2,5X))
C
C     PRINT 1239, MPT
C1239 FORMAT (' NUMBER OF DERIVED NODES:  MPT = ',I5)
C
C
C
C
C*****************************************************************
C             FIND LENGTH OF THIS SEGMENTED CONTOUR
C*****************************************************************
C
      MPTM = MPT-1
C
      IF(DSEG.GT.0.0) THEN
C
        DLENMX = -1.0E+5
        DLENMN = -DLENMX
        CLEN = 0.0
C
C        DO 17 M=1,MPTM
C          DXD = XD(M+1)-XD(M)
C          DYD = YD(M+1)-YD(M)
C          DLEN = SQRT(DXD**2+DYD**2)
C            IF(DLEN.GT.DLENMX) DLENMX = DLEN
C            IF(DLEN.LT.DLENMN) DLENMN = DLEN
C          CLEN = CLEN+DLEN
C   17   CONTINUE
C
C        PRINT 201, MPT,CLEN,DLENMX,DLENMN
  201   FORMAT ('  (SEG)  MPT=',I3,/,' ',
     1          8X,'LENGTH = ',F10.1,'   MAX SEG = ',F8.2,
     2          '   MIN SEG =',F8.2)
C
      END IF
C
 2000 CONTINUE  ! BRANCH HERE TO BYPASS ADJUSTMENTS
C
      RETURN
      END
