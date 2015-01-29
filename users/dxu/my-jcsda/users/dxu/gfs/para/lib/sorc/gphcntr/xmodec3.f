      SUBROUTINE MODEC3(VERTIC,NODES,ZD,ZT,IMAGE,
     1                  NOWIDTH,NOLINES,IRETN)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    MODEC3      GENERATE VARIAN RELATIVE VECTORS
C   PRGMMR: KRISHNA KUMAR       ORG: W/NP12    DATE: 1999-07-01
C
C ABSTRACT: GENERATE VARIAN RELATIVE VECTORS  ON FAX GRAPHIC PRODUCT.
C
C PROGRAM HISTORY LOG:
C   94-04-08  ORIGINAL AUTHOR  LUKE LIN.
C   94-06-30  HENRICHSEN ADDED LOGIC TO REMOVE FILLER CHARACTER.
C                        CHANGED METHOD OF INCREMENTING THE BYTE
C                        "ICNTOT" TO INCREMENT AND THEN LOAD.
C   94-12-30  LUKE LIN      CONVERT IT CFT-77.
C   95-08-16  LUKE LIN      ADD LINE ATTRIBUTES
C   96-04-19  LUKE LIN      MODIFY FOR CONTOUR PACKAGES
C   96-05-21  LUKE LIN      MODIFY FOR BACKGROUND OPTIONS.
C   96-05-28  LUKE LIN      MODIFY FOR CONTOUR ADJUSTMENTS.
C   96-06-20  LUKE LIN      MODIFY FOR INVOKING UOR_DOT.
C   96-10-01  LUKE LIN      MODIFY FOR AVN ON NGM BACKGROUND
C 1999-07-01  KRISHNA KUMAR CONVERTED FROM CRAY VERSION TO IBM RS/6000
C
C USAGE:    CALL MODEC3(VERTIC,NODES,ZD,ZT,IRETN)
C   INPUT ARGUMENT LIST:
C     VERTIC   - INTEGER*2 ARRAY CONTAINS CONTOUR VECTORS.
C     NODES    - NUMBER OF VERTICS.
C     ZD       - INTEGER*4  .. FOR ZOOM DISABLE
C              =0, WHEN ZOOM IN, IT WILL BE EXTENDED BY THE ZOOM-FACTOR.
C              =1, WHEN ZOOM IN, IT WILL REMAIN AT THE BASIC PRODUCT
C                  ZOOM ISZE AS DETERMINED BY ZOOM FACTOR.
C     ZT       - INTEGER VALUE FOR ZOOM THRESHOLD OF TEXT.
C              - MAY HAVE THE FOLLOWING VALUES:
C              - ZT = 0, DISPLAY AT ALL ZOOMS.
C              - ZT = 1, DISPLAY AT ZOOMS 4 AND ABOVE.
C              - ZT = 2, DISPLAY AT ZOOMS 9 AND ABOVE.
C              - ZT = 3, DISPLAY AT ZOOMS 16 AND ABOVE.
C     IMAGE    - THE BITPLANE
C     NOWIDTH  - THE WIDTH IN WORDS FOR THE OUTPUT CHART
C     NOLINES  - THE LENGTH FOR THE OUTPUT CHART
C     COMMON   - /ISPACE/LBLOCK,ICNTOT,LBNKFG
C
C   OUTPUT ARGUMENT LIST:
C     IRETN    - =0, NOMAL
C                =5, EXCEED THE ISPACE.
C                =12, DELTAS TOO LARGE FOR 12 BITS
C     COMMON   - /ISPACE/LBLOCK,ICNTOT,LBNKFG
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
C
C
      COMMON / ILCON / MAP(15)
      INTEGER       MAP
C
C      .... THIS SET IS FOR UOR_DOT
       COMMON /UOR2D/ UGRIDT1,UXPO,UYPO,UXADJUS,UYADJUS,UCU2GI,UORFG,
     1                IP,IPOPT
       REAL     UGRIDT1,UXPO,UYPO,UXADJUS,UYADJUS,UCU2GI
       LOGICAL  UORFG
C
        INTEGER       IMAGE(*)
C
        INTEGER       NOLINES
C       ... NO OF SCAN LINES IN THE BCAKGROUND FILE
        INTEGER       NOWIDTH
C       ... THE WIDTH OF THE BACKGROUND FILE IN WORDS
C
      COMMON / BOUND /   XMIN, YMIN, XMAX, YMAX, CUTWOW
      INTEGER            XMIN, YMIN, XMAX, YMAX, CUTWOW(4)
C
      COMMON / RANGE /   XLOW, YLOW, XHIGH, YHIGH, ZLOW, ZHIGH
      INTEGER            XLOW, YLOW, XHIGH, YHIGH, ZLOW, ZHIGH
C
      COMMON / TRANSF /  XUORPX, YUORPX
      DOUBLE PRECISION   XUORPX, YUORPX
C
      COMMON / LNATTR / LNWEIGHT, LNSTYLE, LNCOLOR
      INTEGER      LNWEIGHT
      INTEGER      LNSTYLE
      INTEGER      LNCOLOR
C
      COMMON /DASH/LDOUBLE,DASHFG,DASHMK(2),IDASH,SHADFG,SHADMK(20)
      LOGICAL      LDOUBLE
      LOGICAL      DASHFG
      INTEGER      DASHMK
      INTEGER      IDASH
      LOGICAL      SHADFG
      INTEGER      SHADMK
C
      INTEGER     VERTIC(*)
C
      INTEGER      MSK3F
      INTEGER      MSKFFF
      INTEGER      IFRAME(4)
      INTEGER      IXYXY(4)
      INTEGER      IXYNEW(4)
      INTEGER      BB
      INTEGER      IBEGIN
      INTEGER      IXOLD,IYOLD,IXNEW,IYNEW
C
      INTEGER      IJIJ(4)
      INTEGER      KODTED
      INTEGER      KPUTPEL1
C
      INTEGER      IUOPTN 
      INTEGER      IEXIT
      REAL         XUORS,YUORS
C 
      LOGICAL       OUTBND
      LOGICAL       PREOUT
      LOGICAL       CUROUT
      LOGICAL       INSAPT
      LOGICAL       NOTINB
C
      LOGICAL       LFLIP_J
C
       LOGICAL       IDHSAVE
       LOGICAL       IDLSAVE
       INTEGER       IDHMKSA(2)
C
C
       integer       iacc
       character*8   cacc
       equivalence  (iacc,cacc)
       character*6   bgname
C
C
      DATA          MSK3F        /Z'0000003F' /
      DATA          MSKFFF       /Z'00000FFF' /
C
      DATA          IBLANQ       / 0 /
C
C     ....... START .....
C
C
      IRETN = 0
      IBEGIN = 1
C
      IACC = MAP(1)
      BGNAME = CACC(1:6)
C     PRINT *, ' BGNAME =', BGNAME
C     ....set up the options
      IF (BGNAME.EQ.'NH4004' .OR. BGNAME.EQ.'SH4001') THEN
         IUOPTN = 5
      ELSE IF (BGNAME .EQ. 'NH2005') THEN
         IUOPTN = 3
         IF (UORFG .AND. UXPO.EQ.65.0 .AND. UYPO.EQ.65.0) THEN
C           ... FOR AVN ON NGM BACKGROUND
            IUOPTN = 5
         ENDIF
      ELSE IF (BGNAME .EQ. 'NH2007') THEN
         IUOPTN = 3
      ELSE IF (BGNAME .EQ. 'NH2501') THEN
         IUOPTN = 3
      ELSE IF (BGNAME .EQ. 'NH4006') THEN
         IUOPTN = 2
      ELSE IF (BGNAME .EQ. 'NH4005') THEN
         IUOPTN = 6
      ELSE IF (BGNAME .EQ. 'NH1302') THEN
         IUOPTN = 1
      ELSE IF (BGNAME .EQ. 'PNAM02') THEN
         IUOPTN = 4
      ELSE
         PRINT *, ' BACKGROUND NAME=',BGNAME
         PRINT *, ' FATAL ERROR IN MODEC3 , PLEASE DEFINE OPTION.'
         IUOPTN = 5
      ENDIF
C     PRINT *,' OPTION=',IUOPTN
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
      LFLIP_J = .FALSE.
      IF (DASHFG) THEN 
          KPUTPEL1 = 0
      ELSE
          KPUTPEL1 = 1
      ENDIF
C         print *,' cutwow=',cutwow(1),cutwow(2),cutwow(3),cutwow(4)
C         XUORS = FLOAT(CUTWOW(1))
C         YUORS = FLOAT(CUTWOW(2))
C         CALL UOR_DOT(XUORS,YUORS,IXOLD,IYOLD,IUOPTN)
C         XUORS = FLOAT(CUTWOW(3))
C         YUORS = FLOAT(CUTWOW(4))
C         CALL UOR_DOT(XUORS,YUORS,IXNEW,IYNEW,IUOPTN)
C         PRINT *, ' begin of modec3:', ixold, iyold, ixnew, iynew
C        print *, ' begin of modec3, icntot=',icntot
C        print *,' cut window=', cutwow(1),cutwow(2),cutwow(3),cutwow(4)
C        print 98,cutwow(1),cutwow(2),cutwow(3),cutwow(4)
C 98     format(2x,' cutwow=',4(2x,z16))
C
C         .... INITIALIZATION......
          OUTBND = .FALSE.
          PREOUT = .FALSE.
          CUROUT = .FALSE.
          INSAPT = .FALSE.
          NOTINB = .FALSE.
C        ..... CHECK BOUNDARY .....
C         IF (XLOW .LT. XMIN .OR. YLOW .LT. YMIN .OR. XHIGH .GT. XMAX
C    &        .OR. YHIGH .GT. YMAX ) THEN
C          print 97,xlow,ylow,xhigh,yhigh
C 97       format(2x,' x/y/low/high=',4(2x,i8))
C          print 96,xlow,ylow,xhigh,yhigh
C 96       format(2x,' x/y/low/high=',4(2x,z16))
C          print 95,xmin,ymin,xmax,ymax
C 95       format(2x,' x/y/min/max =',4(2x,i8))
C          print 94,xmin,ymin,xmax,ymax
C 94       format(2x,' x/y/min/max =',4(2x,z16))
          IF (XLOW .LT. CUTWOW(1) .OR. YLOW .LT. CUTWOW(2) .OR.
     &        XHIGH .GT. CUTWOW(3) .OR. YHIGH .GT. CUTWOW(4)) THEN
              OUTBND = .TRUE.
C             PRINT *, '  POINTS OUT OF BOUND',XMIN,XMAX,YMIN,YMAX
C             PRINT *, '  RANGE:             ',XLOW,XHIGH,YLOW,YHIGH
              IFRAME(1) = CUTWOW(1)
              IFRAME(2) = CUTWOW(2)
              IFRAME(3) = CUTWOW(3)
              IFRAME(4) = CUTWOW(4)
          ENDIF
C          print 93,vertic(1),vertic(2),vertic(3),vertic(4)
C 93       format(2x,' vertic      =',4(2x,i8))
C          print 92,vertic(1),vertic(2),vertic(3),vertic(4)
C 92       format(2x,' vertic      =',4(2x,z16))
C     .... MAKE SURE FIRST POINT IS INBOUND
          IF (OUTBND) THEN
  20         CONTINUE
             IF (IBEGIN .GT. NODES) THEN
                RETURN
             ELSE
                ITEMPX =  VERTIC(IBEGIN*2 -1)
                ITEMPY =  VERTIC(IBEGIN*2)
C               IF ( ITEMPX .LT. XMIN .OR. ITEMPY .LT. YMIN .OR.
C    &               ITEMPX .GT. XMAX .OR. ITEMPY .GT. YMAX) THEN
                IF (ITEMPX.LT.CUTWOW(1) .OR. ITEMPY.LT.CUTWOW(2) .OR.
     &              ITEMPX.GT.CUTWOW(3) .OR. ITEMPY.GT.CUTWOW(4)) THEN
                        IBEGIN = IBEGIN + 1
                        NOTINB = .TRUE.
C                       PRINT *, ' FIRST POINT OUT OF BOUND'
                ELSE
                        NOTINB = .FALSE.
                ENDIF
             ENDIF
             IF ( NOTINB ) GO TO 20
          ENDIF
C         .... CHECK FIRST POINT NOT IN BOUND, THEN CLIP IT ..
          IF (IBEGIN .GT. 1) THEN
C            .... PREVIOUS POINT IS OUT OF BOUND ....
C            IF (IBEGIN .GT. NODES - 1) THEN
C               RETURN
C            ENDIF
             IXYXY(1) = VERTIC(2*(IBEGIN -1) -1)
             IXYXY(2) = VERTIC(2*(IBEGIN -1))
             IXYXY(3) = VERTIC(2*IBEGIN -1)
             IXYXY(4) = VERTIC(2*IBEGIN)
             CALL FCLIP(IFRAME, IXYXY, IXYNEW, IRETNC)
             IF (IRETNC .EQ. 2) THEN
C               .... CLIPPER PT1 ....
                IBEGIN = IBEGIN - 1
                VERTIC(2*IBEGIN -1) = IXYNEW(1)
                VERTIC(2*IBEGIN) = IXYNEW(2)
             ENDIF
          ENDIF
C
C         IXOLD = NINT((VERTIC(2*IBEGIN -1)-XMIN) / XUORPX)
C         IYOLD = NINT((VERTIC(2*IBEGIN)-YMIN) / YUORPX)
          XUORS = FLOAT(VERTIC(2*IBEGIN -1))
          YUORS = FLOAT(VERTIC(2*IBEGIN))
          CALL UOR_DOT(XUORS,YUORS,IXOLD,IYOLD,IUOPTN)
C         PRINT *, ' GOT A NEW LINE:', IBEGIN, IXOLD, IYOLD
C
          IF (IXOLD .LE. 0 ) IXOLD = 1
          IF (IYOLD .LE. 0 ) IYOLD = 1
C
          BB     = 0
C         .... PROCESS EACH DELTA AND PUT OUT ONE OR TWO WORDS
          DO 500 IDX=IBEGIN+1, NODES
             ITEMPX =  VERTIC(IDX*2 -1)
             ITEMPY =  VERTIC(IDX*2)
             IF ( OUTBND ) THEN
C               IF ( ITEMPX .LT. XMIN .OR. ITEMPY .LT. YMIN .OR.
C    &               ITEMPX .GT. XMAX .OR. ITEMPY .GT. YMAX) THEN
                IF (ITEMPX.LT.CUTWOW(1) .OR. ITEMPY.LT.CUTWOW(2) .OR.
     &              ITEMPX.GT.CUTWOW(3) .OR. ITEMPY.GT.CUTWOW(4)) THEN
                     CUROUT = .TRUE.
C       PRINT *, ' CURRENT POINT OUT OF BOUND',IDX,ITEMPX,ITEMPY
                        IF ( .NOT. PREOUT) THEN
                           IXYXY(1) = VERTIC(2*(IDX -1) -1)
                           IXYXY(2) = VERTIC(2*(IDX -1))
                           IXYXY(3) = ITEMPX
                           IXYXY(4) = ITEMPY
                           CALL FCLIP(IFRAME, IXYXY, IXYNEW, IRETNC)
C       PRINT *,'CLIP1:',IRETNC,IXYNEW(1),IXYNEW(2),IXYNEW(3),IXYNEW(4)
                           IF (IRETNC .EQ. 4) THEN
C                          .... CLIPPER PT1 ....
                               ITEMPX = IXYNEW(3)
                               ITEMPY = IXYNEW(4)
                           ELSE
                               PRINT *,' ***ABEND FROM FCLIP1:',IRETNC
                               BB = 1
                               GO TO 200
                           ENDIF
                        ELSE
                           BB = 1
                           GOTO 200
                        ENDIF
                ELSE IF ( PREOUT) THEN
C               ....    PREVIOUS POINT IS OUT, BUT CURRENT IS IN ...
                      IXYXY(1) = VERTIC(2*(IDX -1) -1)
                      IXYXY(2) = VERTIC(2*(IDX -1))
                      IXYXY(3) = ITEMPX
                      IXYXY(4) = ITEMPY
                      CALL FCLIP(IFRAME, IXYXY, IXYNEW, IRETNC)
C       PRINT *,'CLIP2:',IRETNC,IXYNEW(1),IXYNEW(2),IXYNEW(3),IXYNEW(4)
                      IF (IRETNC .EQ. 2) THEN
C                        .... CLIPPER PT2 ....
                          BB = 1
                          ITEMPX = IXYNEW(1)
                          ITEMPY = IXYNEW(2)
                          INSAPT = .TRUE.
                      ELSE
                          PRINT *,' ***ABEND FROM FCLIP2:',IRETNC
                          BB = 1
                          GO TO 200
                      ENDIF
                ENDIF
             ENDIF
  150        CONTINUE
C            ....  DELA X/Y
C??          IXNEW = NINT((ITEMPX -XMIN) / XUORPX )
C??          IYNEW = NINT((ITEMPY -YMIN) / YUORPX )
             XUORS = FLOAT(ITEMPX)
             YUORS = FLOAT(ITEMPY)
             CALL UOR_DOT(XUORS,YUORS,IXNEW,IYNEW,IUOPTN)
C/           IF (IXNEW .GE. 2048 ) THE  IXNEW = 2047
C            IF (IXNEW .LE. 0) IXNEW = 1
C            IF (IYNEW .LE. 0) IYNEW = 1
C
             IF (BB.EQ.1) THEN
                BB = 0
             ELSE
                IJIJ(1) = IXOLD + MAP(11)
                IJIJ(2) = IYOLD + MAP(12)
                IJIJ(3) = IXNEW + MAP(11)
                IJIJ(4) = IYNEW + MAP(12)
C
                CALL BPDRLN(IJIJ,IBLANQ,KPUTPEL1,LFLIP_J,
     A                      IEXIT,IMAGE,NOWIDTH,NOLINES)
             ENDIF
C
             IF (IEXIT .NE. 0) THEN
                PRINT *,' ERROR OCCUR FROM BPDRLN: STATUS=', IEXIT
             ENDIF
C 
C            .... CHECK IF AN INSERT POINT HERE ....
  140        CONTINUE
             IF (INSAPT) THEN
                 INSAPT = .FALSE.
                 ITEMPX = IXYNEW(3)
                 ITEMPY = IXYNEW(4)
                 IXOLD = IXNEW
                 IYOLD = IYNEW
                 GO TO 150
             ENDIF
C
             IXOLD = IXNEW
             IYOLD = IYNEW
  200        CONTINUE
             IF (CUROUT) THEN
                PREOUT = .TRUE.
             ELSE
                PREOUT = .FALSE.
             ENDIF
C
             CUROUT = .FALSE.
  500    CONTINUE
      RETURN
      END
