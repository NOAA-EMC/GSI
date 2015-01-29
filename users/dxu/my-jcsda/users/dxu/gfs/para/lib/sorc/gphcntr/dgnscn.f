       SUBROUTINE DGNSCN(ELETYP,IMAGE,NOWIDTH,NOLINES,STATUS)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    DGNSCN      SCAN A DESIGN FILE FOR DESIRED ELEMENT.
C   PRGMMR: LUKE LIN         ORG: W/NMC41    DATE: 96-04-19
C
C ABSTRACT: SCAN THE DESIGN FILE FOR A DESIRED ELEMENT TYPE AND THEN
C   ENCODE IT TO UTF FORMAT.
C
C PROGRAM HISTORY LOG:
C   94-04-05  ORIGINAL AUTHOR LIN
C   94-11-28  HENRICHSEN/LIN ADD LOGIC TO USE OFF SET MODE FOR SOME
C                        LABELS.
C   94-12-21  LUKE LIN   CONVERT IT CFT-77.
C   95-01-20  LUKE LIN   MODIFY THE TEST OF THE END OF DESIGN FILE
C   96-04-19  LUKE LIN   MODIFY FOR CNTR PACKAGE.
C
C USAGE:    CALL DGNSCN(ELETYP,STATUS)
C   INPUT ARGUMENT LIST:
C     ELETYP   - INTEGER*4 ELEMENT TYPE WHERE:
C              - =  3 OR 4 LINE OR STRING.
C              - = 17 TEXT STRING.
C              - = 66 INFORMATION DATA.
C     IMAGE    - THE BIT PLANE
C     NOWIDTH  - THE NO OF WORDS FOR THE OUTPUT CHART'S WIDTH
C     NOLINES  - THE NO OF LINES FOR THE OUTPUT CHART'S LENGTH
C
C   OUTPUT ARGUMENT LIST:
C     STATUS   - INTEGER*4 RETURN CONDITIONS, WHERE:
C              - =0, NORMAL
C              - =3, ERROR WHILE DECODING INPUT DESIGN FILE,
C              - CANNOT FIND EOF
C              - =4, ERROR WHILE DECODING INPUT DESIGN FILE-,
C              - A NEGATIVE BLK
C
C   INPUT FILES:
C     DDNAME1  - GENERIC NAME & CONTENT
C
C   OUTPUT FILES:
C     FT06F001 - INCLUDE IF ANY PRINTOUT
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: MVS FORTRAN 77.
C   MACHINE:  NAS
C
C$$$
C
C
      COMMON / BOUND /   XMIN, YMIN, XMAX, YMAX, CUTWOW
      COMMON / DGNBIN /  VECBIN,MAXBIN,IMDEX,ITOTWD
      COMMON / RANGE /   XLOW, YLOW, XHIGH, YHIGH, ZLOW, ZHIGH
C
      COMMON / LNATTR / LNWEIGHT, LNSTYLE, LNCOLOR
      INTEGER      LNWEIGHT
      INTEGER      LNSTYLE
      INTEGER      LNCOLOR
C
       INTEGER       ELETYP
       INTEGER       IMAGE(*)
       INTEGER       NOWIDTH
       INTEGER       NOLINES
C
       INTEGER*8     IDREC(6)
C      ... 48-byte ID
C
       INTEGER       IC8FLG
       INTEGER       IDXTAB(4)
       INTEGER       MSK7F
       INTEGER       MSK0F
       INTEGER       MS00FF
       INTEGER       IX,IY,R,RB,ZT,ISIZE,ZD
       INTEGER       STATUS
       INTEGER       VERTIC(202)
       INTEGER       XLOW, YLOW, XHIGH, YHIGH, ZLOW, ZHIGH
       INTEGER       XMIN, YMIN, XMAX, YMAX, CUTWOW(4)
C
       INTEGER       ID2REC(24)
       INTEGER       INTEXT(128)
       INTEGER       VECBIN(409800)
       INTEGER       MSK330
       INTEGER       MSK33F
       INTEGER       I2MAX
       INTEGER       I2MIN
       INTEGER       MSK07,MSKF8
C
       LOGICAL       IDHSAVE
       LOGICAL       IDLSAVE
       INTEGER       IDHMKSA(2)
C
C      ... WHERE 409800 HALFWORDS = 400 VAX BLOCKS
C
       CHARACTER*1   CTEXT(256)
C
       DATA          IC8FLG      / 1194 /
       DATA          IDXTAB      / -5,-9,-14,-18/
       DATA          MSK7F       / Z'0000007F' /
       DATA          MSK0F       / Z'000000FF' /
       DATA          MS00FF      / Z'0000FFFF' /
       DATA          MSK330      / Z'000000007FFFFFFF' /
       DATA          MSK33F      / Z'FFFFFFFF80000000' /
       DATA          I2MAX       / 32767 /
       DATA          I2MIN       / -32768 /
       DATA          MSK07       / Z'0000000000000007' /
       DATA          MSKF8       / Z'00000000000000F8' /
C
       EQUIVALENCE   (IDREC(1), ID2REC(1))
C
       STATUS = 0
C      .... DECODE DESIGN FILE
       IMDEX = 0
C      print *, ' in dgnscn, itotwd=', itotwd
C      print *, ' in dgnscn,before=', dashmk(1),dashmk(2),idash
  400  CONTINUE
       IMDEX = IMDEX + 1
          IWORD0 = IAND(VECBIN(IMDEX),MS00FF)
C
       IF ( IWORD0 .EQ. -1 .OR. IWORD0 .EQ. 0) THEN
          WRITE(6,FMT='('' DGNSCN: HIT END-OF-DGN-FILE '')')
             IMDEX = IMDEX - 1
       ELSE IF (IMDEX .GT. ITOTWD) THEN
        WRITE(6,FMT='('' DGNSCN: ERROR WHILE DECODING DESIN FILE!'')')
             STATUS = 3
       ELSE
C
          ITYPE  = ISHFT(IWORD0,-8)
          ITYPE  = IAND(ITYPE,MSK7F)
          NOWORD = VECBIN(IMDEX + 1)
C         PRINT *,' GOT A TYPE=',ITYPE
C         PRINT *,' NO WORDS IN THIS BLOCK:',NOWORD
C         PRINT 99,VECBIN(IMDEX -1),VECBIN(IMDEX),VECBIN(IMDEX+1)
C99       FORMAT( 1H ,' VECBIN=', 3(1X,Z4))
          IF (NOWORD .LT. 0) THEN
             PRINT *,' FATAL ERROR-- NOWORD=', NOWORD
             PRINT *, ' IMDEX = ', IMDEX
             PRINT *, ' ITOTWD= ', ITOTWD
             STATUS = 4
             RETURN
          ENDIF
C
          IF (ITYPE .EQ. ELETYP) THEN
             K = 1
             LEVEL = IAND(IWORD0, MSK7F)
C            PRINT *, ' DECODING ELEMENT TYPE,LEVEL. =', ELETYP,LEVEL
             IF (ELETYP .EQ. 66 .AND. LEVEL.EQ.20) THEN
C            .... DECODE USER IMFORMATION DATA ...
                DO I= IMDEX+20, IMDEX+43
                   ID2REC(K) = VECBIN(I)
                   K = K + 1
                ENDDO
                CALL SBYTES(IDREC,ID2REC,0,16,0,24)
                CALL UNPKHD(IDREC)
C               ... UNPACK 48-byte ID HEADER ....
                RETURN
             ELSE IF (ELETYP .EQ. 3) THEN
C            ..... DECODE LINE ELEMENT....
C                print *,' process a line element'
                 ITEMPX = VECBIN(IMDEX+2)
                 ITEMPY = VECBIN(IMDEX+3)
                 XLOW = IOR(ISHFT(ITEMPX,16),ITEMPY)
                 IF (BTEST(XLOW,31))  THEN
C                    ... POSITIVE ...
                     XLOW = IAND(XLOW, MSK330)
                 ELSE
C                    ... NEGATIVE
                     XLOW = IOR(XLOW,MSK33F)
                 ENDIF
                 ITEMPX = VECBIN(IMDEX+4)
                 ITEMPY = VECBIN(IMDEX+5)
                 YLOW = IOR(ISHFT(ITEMPX,16),ITEMPY)
                 IF (BTEST(YLOW,31)) THEN
                     YLOW = IAND(YLOW, MSK330)
                 ELSE
                     YLOW = IOR(YLOW,MSK33F)
                 ENDIF
                 ITEMPX = VECBIN(IMDEX+8)
                 ITEMPY = VECBIN(IMDEX+9)
                 XHIGH = IOR(ISHFT(ITEMPX,16),ITEMPY)
                 IF (BTEST(XHIGH,31)) THEN
                     XHIGH = IAND(XHIGH, MSK330)
                 ELSE
                     XHIGH = IOR(XHIGH, MSK33F)
                 ENDIF
                 ITEMPX = VECBIN(IMDEX+10)
                 ITEMPY = VECBIN(IMDEX+11)
                 YHIGH = IOR(ISHFT(ITEMPX,16),ITEMPY)
                 IF (BTEST(YHIGH,31)) THEN
                     YHIGH = IAND(YHIGH, MSK330)
                 ELSE
                     YHIGH = IOR(YHIGH, MSK33F)
                 ENDIF
C
                 LNSTYLE = IAND(MSK07,VECBIN(IMDEX+18))
                 LNWEIGHT = ISHFT(IAND(MSKF8,VECBIN(IMDEX+18)),-3)
C                print *,' lnstyle and lnwei=', lnstyle, lnweight
C
                 NODES = 2
              DO I = 1, 2*NODES
                    ITEMPX = VECBIN(IMDEX+19+2*I-1)
                    ITEMPY = VECBIN(IMDEX+19+2*I)
                    ITEMPY = IAND(MS00FF,ITEMPY)
                    VERTIC(I) = IOR(ISHFT(ITEMPX,16),ITEMPY)
              IF (BTEST(VERTIC(I),31)) VERTIC(I)=IOR(VERTIC(I),MSK33F)
              ENDDO
                 ZT = 0
                 ZD = 0
                 CALL MODEC3(VERTIC,NODES,ZD,ZT,IMAGE,
     1                       NOWIDTH,NOLINES,IRETN)
C
             ELSE IF (ELETYP .EQ. 4) THEN
C            ..... DECODE LINE STRING ....
C                print *,' process a line string'
                 ITEMPX = VECBIN(IMDEX+2)
                 ITEMPY = VECBIN(IMDEX+3)
C                write(*,113)itempx,itempy,itempx,itempy
C113             format(1h ,' temp=',2(z16,3x),2(i10,2x))
C                ITEMPY = IAND(MS00FF,ITEMPY)
                 XLOW = IOR(ISHFT(ITEMPX,16),ITEMPY)
C                write(*,114)xlow,xlow
C114             format(1h ,' xylow/high=',z16, 2x, i10)
                 IF (BTEST(XLOW,31))  THEN
C                    ... POSITIVE ...
C                    XLOW = IBCLR(XLOW,31)
                     XLOW = IAND(XLOW, MSK330)
                 ELSE
C                    ... NEGATIVE
C                    XLOW = IBSET(XLOW,31)
                     XLOW = IOR(XLOW,MSK33F)
                 ENDIF
C                write(*,114)xlow,xlow
C                .... FLIP THE FIRST SIGINIFICANT BIT
                 ITEMPX = VECBIN(IMDEX+4)
                 ITEMPY = VECBIN(IMDEX+5)
C                write(*,113)itempx,itempy,itempx,itempy
C                ITEMPY = IAND(MS00FF,ITEMPY)
                 YLOW = IOR(ISHFT(ITEMPX,16),ITEMPY)
C                write(*,114)ylow,ylow
                 IF (BTEST(YLOW,31)) THEN
C                    YLOW = IBCLR(YLOW,31)
                     YLOW = IAND(YLOW, MSK330)
                 ELSE
C                    YLOW = IBSET(YLOW,31)
                     YLOW = IOR(YLOW,MSK33F)
                 ENDIF
C                write(*,114)ylow,ylow
                 ITEMPX = VECBIN(IMDEX+8)
                 ITEMPY = VECBIN(IMDEX+9)
C                write(*,113)itempx,itempy,itempx,itempy
C                ITEMPY = IAND(MS00FF,ITEMPY)
                 XHIGH = IOR(ISHFT(ITEMPX,16),ITEMPY)
C                write(*,114)xhigh,xhigh
                 IF (BTEST(XHIGH,31)) THEN
C                    XHIGH = IBCLR(XHIGH,31)
                     XHIGH = IAND(XHIGH, MSK330)
                 ELSE
C                    XHIGH = IBSET(XHIGH,31)
                     XHIGH = IOR(XHIGH, MSK33F)
                 ENDIF
C                write(*,114)xhigh,xhigh
                 ITEMPX = VECBIN(IMDEX+10)
                 ITEMPY = VECBIN(IMDEX+11)
C                write(*,113)itempx,itempy,itempx,itempy
C                ITEMPY = IAND(MS00FF,ITEMPY)
                 YHIGH = IOR(ISHFT(ITEMPX,16),ITEMPY)
C                write(*,114)yhigh,yhigh
                 IF (BTEST(YHIGH,31)) THEN
C                    YHIGH = IBCLR(YHIGH,31)
                     YHIGH = IAND(YHIGH, MSK330)
                 ELSE
C                    YHIGH = IBSET(YHIGH,31)
                     YHIGH = IOR(YHIGH, MSK33F)
                 ENDIF
C                write(*,114)yhigh,yhigh
C                PRINT *,' LOW,HIGH=',XLOW,YLOW,XHIGH,YHIGH
C
                 LNSTYLE = IAND(MSK07,VECBIN(IMDEX+17))
                 LNWEIGHT = ISHFT(IAND(MSKF8,VECBIN(IMDEX+17)),-3)
C                print *,' cntr style and lnwei=', lnstyle, lnweight
C
                 NODES = VECBIN(IMDEX+18)
              DO I = 1, 2*NODES
                    ITEMPX = VECBIN(IMDEX+18+2*I-1)
                    ITEMPY = VECBIN(IMDEX+18+2*I)
                    ITEMPY = IAND(MS00FF,ITEMPY)
                    VERTIC(I) = IOR(ISHFT(ITEMPX,16),ITEMPY)
              IF (BTEST(VERTIC(I),31)) VERTIC(I)=IOR(VERTIC(I),MSK33F)
              ENDDO
                 ZT = 0
                 ZD = 0
                 CALL MODEC3(VERTIC,NODES,ZD,ZT,IMAGE,
     1                       NOWIDTH,NOLINES,IRETN)
             ENDIF
          ENDIF
       IMDEX = IMDEX + NOWORD + 1
       GO TO 400
C
       ENDIF
C      print *, ' in dgnscn,after =', dashmk(1),dashmk(2),idash
       RETURN
       END
