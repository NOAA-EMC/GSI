      SUBROUTINE BPLTVEC(IMAGE,NOWIDTH,NOLINE,XP,YP,NMAX,IL,IRETUR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    BPLTVEC      PLOT VECTORS
C   PRGMMR: LUKE LIN         ORG: W/NMC41    DATE: 96-09-27
C
C ABSTRACT: PLOT VECTORS
C
C PROGRAM HISTORY LOG:
C   94-09-15  LUKE LIN
C   94-12-30  LUKE LIN      CONVERT IT CFT-77.
C   95-09-30  LUKE LIN      ADD LINE ATTRIBUTES
C   96-09-27  LUKE LIN      CHANGE FOR BEDIENT'S CNTR
C 1999-08-01  KRISHNA KUMAR CONVERTED FOR IBM RS/6000
C
C USAGE:    CALL BPLTVEC(XP,YP,NMAX,IRETUR)
C   INPUT ARGUMENT LIST:
C     XP       - REAL*4 XP(4000)
C     YP       - REAL*4 XP(4000)
C     NMAX     - THE NUMBER OF COMPLEX INCREMENT DONE
C
C   OUTPUT ARGUMENT LIST:
C     IRETUR   - REUTRN CONDITION
C              - =0, NORMAL
C              - =5, FATAL ERR -- NOT ENOUGH WORKING BUFFER FOR OUT-DGN
C              - =933, NONFATAL ERR -- ATTEMP TO ADD BAD ELEMENT TO DGN
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
      COMMON /DASH/LDOUBLE,DASHFG,DASHMK(2),IDASH,SHADNO,SHADMK(20)
      LOGICAL      LDOUBLE
      LOGICAL      DASHFG
      INTEGER      DASHMK
      INTEGER      IDASH
      INTEGER      SHADNO
      INTEGER      SHADMK
C
C
        INTEGER       IMAGE(*)
        INTEGER       IL(15)
C
        INTEGER       NOLINE
C       ... NO OF SCAN LINES IN THE BCAKGROUND FILE
        INTEGER       NOWIDTH
C       ... THE WIDTH OF THE BACKGROUND FILE IN WORDS
C
      REAL      XP(NMAX)
      REAL      YP(NMAX)
      INTEGER   NMAX
C
C
      COMMON / LNATTR / LNWEIGHT, LNSTYLE, LNCOLOR
      INTEGER      LNWEIGHT
      INTEGER      LNSTYLE
      INTEGER      LNCOLOR
C
C
      INTEGER      IJIJ(4)
      INTEGER      KODTED
      INTEGER      KPUTPEL1
C

C
       CHARACTER*1  NULL
C
       REAL         T1
       LOGICAL      LFLIP_J
C
      DATA          IBLANQ       / 0 /
C

C
C      . . .   S T A R T   . . .
C
       NULL = CHAR(0)
       IRETUR = 0
       IF (NMAX .LE. 1) RETURN
       T1 = FLOAT(IL(15))/1000.
C      PRINT *,' T1=',T1
C
C
      IF (LNWEIGHT .LE. 1) THEN
         LDOUBLE = .FALSE.
      ELSE
         LDOUBLE = .TRUE.
      ENDIF
C
      IF (LNSTYLE .EQ. 1) THEN
         DASHFG = .TRUE.
C        PRINT *, ' GET A DASHLINE'
         DASHMK(1) = 3
         DASHMK(2) = 2
      ELSE IF (LNSTYLE .EQ. 2) THEN
         DASHFG = .TRUE.
C        PRINT *, ' GET A DASHLINE'
         DASHMK(1) = 5
         DASHMK(2) = 3
      ELSE IF (LNSTYLE .EQ. 3) THEN
         DASHFG = .TRUE.
C        PRINT *, ' GET A DASHLINE'
         DASHMK(1) = 6
         DASHMK(2) = 4
      ELSE IF (LNSTYLE .EQ. 4) THEN
         DASHFG = .TRUE.
C        PRINT *, ' GET A DASHLINE'
         DASHMK(1) = 10
         DASHMK(2) = 7
      ELSE IF (LNSTYLE .EQ. 5) THEN
C        print *, '  ***decode lnstyle .eq. 5'
         DASHFG = .TRUE.
         DASHMK(1) = 12
         DASHMK(2) = 09
      ELSE IF (LNSTYLE .EQ. 6) THEN
C        print *, '  ***decode lnstyle .eq. 6'
         DASHFG = .TRUE.
         DASHMK(1) = 20
         DASHMK(2) = 15
      ELSE IF (LNSTYLE .EQ. 7) THEN
C        print *, '  ***decode lnstyle .eq. 7'
         DASHFG = .TRUE.
         DASHMK(1) = 32
         DASHMK(2) = 25
      ELSE
         DASHFG = .FALSE.
         DASHMK(1) = 0
         DASHMK(2) = 0
      ENDIF
C
      LFLIP_J = .TRUE.
C
      IF (DASHFG) THEN
          KPUTPEL1 = 0
      ELSE
          KPUTPEL1 = 1
      ENDIF
C
         IXOLD = XP(1)*T1
         IYOLD = YP(1)*T1
C
         DO 50 I=2, NMAX
                IXNEW = XP(I)*T1
                IYNEW = YP(I)*T1
                IJIJ(1) = IXOLD +  IL(11)
                IJIJ(2) = IYOLD -  IL(12)
                IJIJ(3) = IXNEW +  IL(11)
                IJIJ(4) = IYNEW -  IL(12)
C               PRINT *,' LINE=', IXOLD,'  ',IYOLD
C
                CALL BPDRLN(IJIJ,IBLANQ,KPUTPEL1,LFLIP_J,
     A                      IEXIT,IMAGE,NOWIDTH,NOLINE)
                IXOLD = IXNEW
                IYOLD = IYNEW
C
   50    CONTINUE
C
      RETURN
      END
