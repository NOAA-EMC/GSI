       SUBROUTINE CNTRI(IRET_CNT, IMAGE, IMAGSIZ_WRDS,IWINDOW,
     1                 MAP, LABEL, INDEX,NFLDS,
     2                 FLD1, DASH1, OFSET1, SHAD1,
     3                 FLD2, DASH2, OFSET2, SHAD2,
     4                 FLD3, DASH3, OFSET3, SHAD3,
     5                 FLD4, DASH4, OFSET4, SHAD4)
C                .      .    .                                       .
C MAIN PROGRAM:  CNTRI       CONTOUR THE INPUT FIELDS.
C   PRGMMR: KRISHNA KUMAR    ORG: NP12    DATE:1999-07-01
C
C ABSTRACT: CONTOUR THE INPUT FIELDS.
C
C PROGRAM HISTORY LOG:
C   96-04-08  ORIGIONAL AUTHOR LUKE LIN  
C   96-04-19  LUKE LIN      MODIFY FOR IMAGE AGRUMENTS.
c   98-05-21  Chris Caruso  remove unused variables and fix
c                           a logic error inside loop where
c                           ldouble is being set (if ldouble
c                           is set to true in any loop iteration,
c                           subsequent loop iterations can't
c                           reset it to false if needed.  add
c                           additional then-else to if block
c                           to fix this).
C 1999-07-01  KRISHNA KUMAR CONVERTED THIS CODE FOR IBM RS/6000 
C
C USAGE:
C   INPUT FILES:
C
C   OUTPUT FILES:
C     Fort.81 - HOLDS THE COMPLETED VARIAN/FAX MAPS.
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C      GRAPHICS  - 
C
C   EXIT STATES:
C     ISTOP=   0 - SUCCESSFUL RUN
C          =   2 - MAPS MAPS NOT POSTED CHECK PARM FOR AN "I"
C          =     - IN THE 1ST BYTE.
C          =   3 - NO AFOS MAPS PROCESSED CHECK DATA CARDS AND PRINT
C          =   4 - ERROR FROM W3FQ06.
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
      COMMON /ILCON/ IL(15)
C
      INTEGER    LMAX
      PARAMETER (LMAX=1024)

      INTEGER    LMAX2
      PARAMETER (LMAX2 = 2*LMAX)
C
      INTEGER     IRET_CNT
      INTEGER     IMAGE(IMAGSIZ_WRDS)
C
      INTEGER     IWINDOW(30)
      INTEGER     MAP(15)
      INTEGER     LABEL(LMAX2)
C
      INTEGER     INDEX(6)
      INTEGER     NFLDS
      REAL        FLD1(*)
      INTEGER     DASH1(2), OFSET1(2), SHAD1(20)
      REAL        FLD2(*)
      INTEGER     DASH2(2), OFSET2(2), SHAD2(20)
      REAL        FLD3(*)
      INTEGER     DASH3(2), OFSET3(2), SHAD3(20)
      REAL        FLD4(*)
      INTEGER     DASH4(2), OFSET4(2), SHAD4(20)
C
      COMMON / DGNBIN / VECBIN,MAXBIN,IMDEX,ITOTWD
      INTEGER    VECBIN(409800)
C
      COMMON / DASH / LDOUBLE,DASHFG,DASHMK(2),IDASH,SHADNO,SHADMK(20)
      LOGICAL      LDOUBLE
      LOGICAL      DASHFG
      INTEGER      DASHMK
      INTEGER      IDASH
      INTEGER      SHADNO
      INTEGER      SHADMK
C
      LOGICAL      WORKL(648400)
      REAL         WORKZ(324200)
C
C     ... WORKZ AND WORKL ARE NEEDED BY BCNTOR ....
C
      REAL       FLDCNT(16900)
      REAL       T1
C
C--------------------- PROGRAM STARTS -------------------------------
C
      DO I = 1, 15
        IL(I) = MAP(I)
      ENDDO
C
      MAXBIN = 409800
      NINDX = 0
      NDVD = 2
      NDIV = 4
      IEXIT = 0
      T1 = FLOAT(MAP(15))/ 1000.
      PRINT *,' ***T1=', T1
C
      ILMAX = IABS(INDEX(4))
      JLMAX = IABS(INDEX(1))
      PRINT *,' ILMAX=', ILMAX
      PRINT *,' JLMAX=', JLMAX
C
      IJTEMP = ILMAX * JLMAX
      PRINT *,' IJTEMP=', IJTEMP
C
      NOLINES = IWINDOW(4)
      NOWIDTH = IWINDOW(15)
      NOPIXELS = IWINDOW(16)
C
C     PRINT *,'SHAD1=',SHAD1(1),' ',SHAD1(2),' ',SHAD1(3),' ',SHAD1(4)
C     PRINT *,'SHAD2=',SHAD2(1),' ',SHAD2(2),' ',SHAD2(3),' ',SHAD2(4)
C
C     INITIALIZATION
C
      IMZ = (ILMAX-1)*NDIV + 1
      IML = 2*IMZ-1
C
      LDOUBLE = .FALSE. 
      IDASH = 0
C 
      DO I = 1, NFLDS
C
        IF (I .EQ. 1) THEN
           IF (DASH1(1).EQ.0) THEN
               DASHFG = .FALSE.
               DASHMK(1) = 0
               DASHMK(2) = 0
           ELSE
               DASHFG = .TRUE.
               DASHMK(1) = DASH1(1)
               DASHMK(2) = DASH1(2)
           ENDIF
           PRINT *,' DASH1=',DASH1(1),' ',DASH1(2)
           LTEMP = 0
           DO II = 1, 20
              IF (SHAD1(II).EQ.0) GOTO 310
              SHADMK(II) = SHAD1(II)
              LTEMP = II
           ENDDO
  310      CONTINUE
           SHADNO = LTEMP/2
           PRINT *,' SHADNO=', SHADNO
           PRINT *,' SHADMK=',SHAD1(1),' ',SHAD1(2)
C
           DO K = 1, IJTEMP
              FLDCNT(K) = FLD1(K)
           ENDDO               
C
           IOFSET = IABS(OFSET1(1))
           IF (IOFSET .EQ. 2) THEN
              LDOUBLE=.TRUE.
           ELSE
              LDOUBLE=.FALSE.
           ENDIF
C
C
        ELSEIF (I .EQ. 2) THEN
           IF (DASH2(1).EQ.0) THEN
               DASHFG = .FALSE.
               DASHMK(1) = 0
               DASHMK(2) = 0
           ELSE
               DASHFG = .TRUE.
               DASHMK(1) = DASH2(1)
               DASHMK(2) = DASH2(2)
           ENDIF
           PRINT *,' DASH2=',DASH2(1),' ',DASH2(2)
           LTEMP = 0
           DO II = 1, 20
              IF (SHAD2(II).EQ.0) GOTO 320
              SHADMK(II) = SHAD2(II)
              LTEMP = II
           ENDDO
  320      CONTINUE
           SHADNO = LTEMP/2
           PRINT *,' SHADNO=', SHADNO
           PRINT *,' SHADMK=',SHAD2(1),' ',SHAD2(2)
C
           DO K = 1, IJTEMP
              FLDCNT(K) = FLD2(K)
           ENDDO
C
           IOFSET = IABS(OFSET2(1))
           IF (IOFSET .EQ. 2) THEN
              LDOUBLE=.TRUE.
           ELSE
              LDOUBLE=.FALSE.
           ENDIF
C
        ELSEIF (I .EQ. 3) THEN
           IF (DASH3(1).EQ.0) THEN
               DASHFG = .FALSE.
               DASHMK(1) = 0
               DASHMK(2) = 0
           ELSE
               DASHFG = .TRUE.
               DASHMK(1) = DASH3(1)
               DASHMK(2) = DASH3(2)
           ENDIF
           LTEMP = 0
           DO II = 1, 20
              IF (SHAD3(II).EQ.0) GOTO 330
              SHADMK(II) = SHAD3(II)
              LTEMP = II
           ENDDO
  330      CONTINUE
           SHADNO = LTEMP/2
C
           DO K = 1, IJTEMP
              FLDCNT(K) = FLD3(K)
           ENDDO
C
           IOFSET = IABS(OFSET3(1))
           IF (IOFSET .EQ. 2) THEN
              LDOUBLE=.TRUE.
           ELSE
              LDOUBLE=.FALSE.
           ENDIF
C
C
        ELSEIF (I .EQ. 4) THEN
           IF (DASH4(1).EQ.0) THEN
               DASHFG = .FALSE.
               DASHMK(1) = 0
               DASHMK(2) = 0
           ELSE
               DASHFG = .TRUE.
               DASHMK(1) = DASH4(1)
               DASHMK(2) = DASH4(2)
           ENDIF
           LTEMP = 0
           DO II = 1, 20
              IF (SHAD4(II).EQ.0) GOTO 340
              SHADMK(II) = SHAD4(II)
              LTEMP = II
           ENDDO
  340      CONTINUE
           SHADNO = LTEMP/2
C
           DO K = 1, IJTEMP
              FLDCNT(K) = FLD4(K)
           ENDDO
C
           IOFSET = IABS(OFSET4(1))
           IF (IOFSET .EQ. 2) THEN
              LDOUBLE=.TRUE.
           ELSE
              LDOUBLE=.FALSE.
           ENDIF
C
C
        ENDIF
C
C ................ WORK ON CONTOURS .................
C
       CALL BCNTOR(IMAGE,NOWIDTH,NOLINES,FLDCNT,ILMAX,JLMAX,
     &             NDVD,NDIV,WORKZ,IMZ,WORKL,IML,MAP,IEXIT)
C
      ENDDO
C
      RETURN
      END
