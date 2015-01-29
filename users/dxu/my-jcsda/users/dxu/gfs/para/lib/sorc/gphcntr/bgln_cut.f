       SUBROUTINE BGLN_CUT(INPLINBUF,IMAGE,JOFDEST,IWINDOW,MAPCON_PUR,
     1                     LCKPRNTQ,IRET_CUT)
C                                                   7-MAR-1996/DSS
C      ... THIS SUBR BGLN_CUT() IS ONLY CALLED FROM GETBGND()
C      ... WHEN GETBGND() HAS READ IN ONE SCANLINE FROM THE MAP BGND
C      ... FILE.PUR; FOR THE PURPOSE OF MOVING THE REQUESTED PORTION
C      ... OF THAT ONE SCANLINE, AFTER OPTIONAL TRANSFORMATIONS, INTO
C      ... THE IMAGE SCANLINE.

C      ... Krishna Kumar converted this code for IBM/RS 6000
C      ... Subroutine MOVBITZ a cray specific routine has now been
C      ... replaced by a new source code with the same name (MOVBITZ
C      ... developed by George Vandenburg) on the IBM RS/6000.
C      ...        S/R  CNTR()

C      ...                GETBGND()

C      ...                   BGLN_CUT()
C      ...                      HASHBG()                    
C      ...                      MOVBITZ()  (gphlib.source)
C      ...                      GBYTES()   (W3LIB)



C      ----------------------------------------------------------------
C      ...  CALL BGLN_CUT(INPLINBUF,IMAGE,JOFDEST,IWINDOW,MAPCON_PUR,
C      ... 1              LCKPRNTQ,IRET_CUT)

       INTEGER     NWRDPLIN_MXMX   	!... LONGEST LINE LENGTH OF ANY
       PARAMETER  (NWRDPLIN_MXMX=66)   	!...    MAP  (IN LONGWORDS)
 
       INTEGER     INPLINBUF(NWRDPLIN_MXMX)

       INTEGER     IMAGE(*)
       INTEGER     JOFDEST
       INTEGER     IWINDOW(30)
       INTEGER     MAPCON_PUR(10)
       LOGICAL     LCKPRNTQ
       INTEGER     IRET_CUT
C      -----------------------------------------------------------------

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... INTEGER       IWINDOW(30)
C      ... IWINDOW(1) = (J_BG_SKP)              !... NSKIP BG AT START
C      ... IWINDOW(2) = (J_BG_ORG) 		!... J0 BG IN IMAGE WORK 
C      ... IWINDOW(4) = (J_FR_MAX)  = 1876      !... ENTIRE IMAGE WORK-J
C      ... IWINDOW(5) = (IPXL_BG_SKP)        !... DISCARD PXL @ LN START
C      ... IWINDOW(6) = IOPTNBITS
C      ... IWINDOW(7) = (IPXL_BG_ORG)         	!... I0 BG IN IMAGE WORK
C      ... IWINDOW(16) = (IPXL_FR_MAX) = 1728   !... MAX PXL IN RESULT
C      ... IWINDOW(15) = (IWRD_FR_MAX) = 27     !... ENTIRE IMAGE WORK-I
C      ... IWINDOW(17) = MAPBGNAME = nh2500x
C      ... IWINDOW(18) = (J_BG_SPA)          !... SPACE FOR BG W/I FRAME
C      ... IWINDOW(19) = (IPXL_BG_SPA)       !... SPACE FOR BG W/I FRAME
C      ... IWINDOW(20) = (IPXL_BG_CUT)       !... PXLS TO USE FROM BG LN
       
       INTEGER     OPTIONS
       PARAMETER  (OPTIONS=6)

       INTEGER    J_FR_MAX
       PARAMETER (J_FR_MAX=4)
       INTEGER    IWRD_FR_MAX
       PARAMETER (IWRD_FR_MAX=15)
       INTEGER    IPXL_FR_MAX
       PARAMETER (IPXL_FR_MAX=16)

       INTEGER    J_BG_SPA
       PARAMETER (J_BG_SPA=18)
       INTEGER    IPXL_BG_SPA
       PARAMETER (IPXL_BG_SPA=19)

       INTEGER    J_BG_SKP
       PARAMETER (J_BG_SKP=1)
       INTEGER    IPXL_BG_SKP
       PARAMETER (IPXL_BG_SKP=5)
       INTEGER    IPXL_BG_CUT
       PARAMETER (IPXL_BG_CUT=20)

       INTEGER    J_BG_ORG
       PARAMETER (J_BG_ORG=2)
       INTEGER    IPXL_BG_ORG
       PARAMETER (IPXL_BG_ORG=7)

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       INTEGER       IOPTNBITS


C      ... WHICH SINGLE WORD SUBSCRIPT POINTS TO STARTING WORD
C      ... OF THIS IMAGE(I,JROW) ???


       INTEGER     NWRDWRK      !... SPACE FOR DOUBLE-SCALED LONGEST LN
       PARAMETER  (NWRDWRK=2*NWRDPLIN_MXMX)

       INTEGER     NGRPWRK     		 !... = 2112 = 2*8*2*66
       PARAMETER  (NGRPWRK=2*8*NWRDWRK)  !... 4-BIT GROUP PER INT

       INTEGER     LINEWORK(NGRPWRK)
C      ... WHERE LINEWORK IS FOR UNPACKED DEST FROM GBYTES ...
C   
       INTEGER      NBYTWRK
       PARAMETER   (NBYTWRK=8*NWRDWRK)

       INTEGER      LINEWORK2(NWRDWRK)
       CHARACTER*1  CLINEWORK2(NBYTWRK)
       EQUIVALENCE (LINEWORK2(1),CLINEWORK2(1))
C
C      ... translate table for doubling pixels within a 4-pxl group
       INTEGER    LTTBLI(16)
       DATA       LTTBLI       / X'00', X'03', X'0C', X'0F',
     1                           X'30', X'33', X'3C', X'3F',
     2                           X'C0', X'C3', X'CC', X'CF',
     3                           X'F0', X'F3', X'FC', X'FF' /
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
       INTEGER   MSKFILL(8)
       DATA      MSKFILL   / X'FFFFFFFFFFFFFFFF',
     1                       X'FF00000000000000',
     2                       X'FFFF000000000000',
     3                       X'FFFFFF0000000000',
     4                       X'FFFFFFFF00000000',
     5                       X'FFFFFFFFFF000000',
     6                       X'FFFFFFFFFFFF0000',
     7                       X'FFFFFFFFFFFFFF00' / 
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       INTEGER     KHASHBIT
       DATA        KHASHBIT    / X'0001' /

       INTEGER     KDUBLBIT
       DATA        KDUBLBIT    / X'0002' /

       INTEGER     K2PANLBIT
       DATA        K2PANLBIT   / X'0004' /



       INTEGER     MSK4BITS
       DATA        MSK4BITS    / X'000F' /

       INTEGER     ISRCBIT1
       INTEGER     NGRPS2DO
C      ... CONSTANTS FOR GBYTES ...
       INTEGER     KBITPGRP
       DATA        KBITPGRP     / 4 /
       INTEGER     KPADBITS
       DATA        KPADBITS     / 0 /
C      ..............................
C
C      . . . . .   S T A R T   . . . . . . . . . . . . . . . . . .
C
       IRET_CUT = 0
       LMT_NLINES_LIB   = MAPCON_PUR(5)
       LMT_NWRDPLIN_LIB = MAPCON_PUR(6)
       LMT_NPXLPLIN_LIB = MAPCON_PUR(7)
       IOPTNBITS = IWINDOW(OPTIONS)

       IF(LCKPRNTQ) THEN
         WRITE(6,105)(MAPCON_PUR(I),I=5,7),IOPTNBITS
  105    FORMAT(1H ,'BGLN_CUT:LIMITS FOR NLINES=',I8,
     1              ';  FOR NWRDPLIN=',I4,';  FOR NPIXELS=',I6,
     2         /1H ,7X,'IOPTNBITS= X',Z16.16)
       ENDIF

       IF(IAND(IOPTNBITS,KDUBLBIT) .NE. 0) THEN
          GO TO 420
       ENDIF
C      ... OTHERWISE,
C        ... NON-DOUBLED BACKGROUND ...
C        ... TO CLIP THE DESIRED PORTION OF INPUT SCANLINE
C        ...   AND TO MOVE TO BGND SPACE WITHIN IMAGE LINE ...
         IJTHDEST = IWINDOW(IWRD_FR_MAX) * (JOFDEST-1)
         ISRCBIT1 = IWINDOW(IPXL_BG_SKP)      !... BIT=0 IS LEFT-MOST BIT
         IDSTBIT1 = IWINDOW(IPXL_BG_ORG)
 
         NBITMOVE1 = IWINDOW(IPXL_BG_CUT)
         IF(NBITMOVE1 .GT. IWINDOW(IPXL_BG_SPA)) THEN
           NBITMOVE1 = IWINDOW(IPXL_BG_SPA)
         ENDIF
         IF(LCKPRNTQ) THEN
           WRITE(6,135) NBITMOVE1,ISRCBIT1,IDSTBIT1,IJTHDEST
  135      FORMAT(1H ,'BGLN_CUT:IN PARA FOR NON-DOUBLED BGND, TO CLIP;',
     1           /1H ,7X,'NBITMOVE1 (MUST BE NON-ZERO) =',I6,
     2           /1H ,7X,'ISRCBIT1 =',I8,';   IDSTBIT1 =',I6,
     3           /1H ,7X,'IJTHDEST =',I8)
C          ... which showed bad value in IJTHDEST = -27 ...
         ENDIF

         IF(NBITMOVE1 .GT. 0) THEN
           CALL MOVBITZ(INPLINBUF,ISRCBIT1,NBITMOVE1,
     1                 IMAGE(IJTHDEST+1),IDSTBIT1)
         ENDIF

         IF(IAND(IOPTNBITS,K2PANLBIT) .NE. 0) THEN
C          ... IF 2-PANL, PUT A DUPLICATE ALONG SIDE ...             
           IDSTBIT2 = IWINDOW(IPXL_BG_ORG) + NBITMOVE1
 
           NBITMOVE2 = IWINDOW(IPXL_BG_CUT)
           IF((NBITMOVE1+NBITMOVE2) .GT. IWINDOW(IPXL_BG_SPA)) THEN
             NBITMOVE2 = IWINDOW(IPXL_BG_SPA) - NBITMOVE1
           ENDIF
           IF(NBITMOVE2 .GT. 0) THEN
             CALL MOVBITZ(INPLINBUF,ISRCBIT1,NBITMOVE2,
     1                    IMAGE(IJTHDEST+1),IDSTBIT2)
           ENDIF
         ENDIF
         GO TO 444

  420  CONTINUE
C        ... COMES HERE TO DOUBLE-SCALE THE BACKGROUND ...
C        ... TO CLIP THE DESIRED PORTION OF INPUT SCANLINE
C        ... UNPACK THAT PORTION INTO 4-BIT GROUPS PER INT
C        ... AND MOVE INTO LINEWORK
         IJTHDEST = IWINDOW(IWRD_FR_MAX) * (JOFDEST-1)

         ISRCBIT1 = IWINDOW(IPXL_BG_SKP)      !... BIT=0 IS LEFT-MOST BIT
         IDSTBIT1 = 0
 
         NBITMOVE1 = IWINDOW(IPXL_BG_CUT)
         IF(NBITMOVE1 .GT. IWINDOW(IPXL_BG_SPA)) THEN
           NBITMOVE1 = IWINDOW(IPXL_BG_SPA)
         ENDIF
         IF(NBITMOVE1 .LE. 0) THEN
C          ... SET ERROR CODE AND JUMP OUT,
           IRET_CUT = 1
           GO TO 999
         ENDIF
         NGRPS2DO = NBITMOVE1/KBITPGRP
         IF(MOD(NBITMOVE1,KBITPGRP) .NE. 0) THEN
           NGRPS2DO = NGRPS2DO + 1
         ENDIF

         CALL GBYTES(INPLINBUF,LINEWORK,ISRCBIT1,KBITPGRP,
     1                 KPADBITS,NGRPS2DO)
C        ...
C        ... NOW I HAVE 4PIXELS IN LOW-ORDER OF EACH INT WORD OF
C        ...    LINEWORK(1) TO (NGRPS2DO)
C        ...
C        ... TRANSLATE 4BITS INTO 8BITS IN ORDER TO DOUBLE EACH PIXEL
C        ...   AND STASH INTO BYTE ARRAY
         DO  IGR = 1,NGRPS2DO
           INDX = 1 + IAND(LINEWORK(IGR),MSK4BITS)
           CLINEWORK2(IGR) = CHAR(LTTBLI(INDX))
         ENDDO
C        ... CAN I ZERO FILL THE LAST LONG_WORD OF DATA?
         NWRD_DONE = NGRPS2DO/8
         NBY_RMNDR = MOD(NGRPS2DO,8)
         IF(NBY_RMNDR .NE. 0) THEN
           NWRD_DONE = NWRD_DONE + 1
         ENDIF
         LINEWORK2(NWRD_DONE) = 
     1                  IAND(LINEWORK2(NWRD_DONE),MSKFILL(NBY_RMNDR+1))
C
C        ... NOW I HAVE THE CUT-OUT PORTION OF THE BG LINE
C        ...   WITH EACH PIXEL DOUBLED IN BYTE(1) TO BYTE(NGRPS2DO)
C        ...   OF C*1 CLINEWORK2(1) TO (NGRPS2DO)
C        ...   WHICH IS EQUIV TO INT LINEWORK2(1) TO (NWRD_DONE)

C        ... NEXT: MOVE THE FIRST 2*IWINDOW(IPXL_BG_CUT) 
C        ...   PIXELS FROM LINEWORK2 TO THE IMAGE BG SPACE

         IJTHDEST = IWINDOW(IWRD_FR_MAX) * (JOFDEST-1)
         IDSTBIT1 = IWINDOW(IPXL_BG_ORG)
 
         NBITMOVE3 = 2*IWINDOW(IPXL_BG_CUT)  	!... DUBL PXL 
         IF(NBITMOVE3 .GT. IWINDOW(IPXL_BG_SPA)) THEN
           NBITMOVE3 = IWINDOW(IPXL_BG_SPA)
         ENDIF
         IF(NBITMOVE3 .LE. 0) THEN
           IRET_CUT = 2
           GO TO 999
         ENDIF

         CALL MOVBITZ(LINEWORK2,0,NBITMOVE3,
     1                IMAGE(IJTHDEST+1),IDSTBIT1)

         IF(IAND(IOPTNBITS,KHASHBIT) .NE. 0) THEN
           CALL HASHBG(IMAGE,JOFDEST,IWINDOW(IWRD_FR_MAX))
         ENDIF

C        ... THEN REPEAT THIS DOUBLED LINE, TO RESCALE IN VERTICAL, TOO
         JOFDEST = JOFDEST + 1
C        ... I NEED A TEST FOR WHETHER THIS JOFDEST EXCEEDS LIMIT ...

         IJTHDEST = IWINDOW(IWRD_FR_MAX) * (JOFDEST-1)
         CALL MOVBITZ(LINEWORK2,0,NBITMOVE3,
     1               IMAGE(IJTHDEST+1),IDSTBIT1)

         IF(IAND(IOPTNBITS,KHASHBIT) .NE. 0) THEN
           CALL HASHBG(IMAGE,JOFDEST,IWINDOW(IWRD_FR_MAX))
         ENDIF

         GO TO 444

C        --------------------------------------------------------------

         IF(IAND(IOPTNBITS,K2PANLBIT) .NE. 0) THEN
C          ... IF 2-PANL, PUT A DUPLICATE ALONG SIDE ...             
           IDSTBIT2 = IWINDOW(IPXL_BG_ORG) + NBITMOVE1
 
           NBITMOVE2 = IWINDOW(IPXL_BG_CUT)
           IF((NBITMOVE1+NBITMOVE2) .GT. IWINDOW(IPXL_BG_SPA)) THEN
             NBITMOVE2 = IWINDOW(IPXL_BG_SPA) - NBITMOVE1
           ENDIF
           IF(NBITMOVE2 .GT. 0) THEN
             CALL MOVBITZ(INPLINBUF,ISRCBIT,NBITMOVE2,
     1                    IMAGE(IJTHDEST+1),IDSTBIT2)
           ENDIF
         ENDIF
         GO TO 444

  444  CONTINUE
       GO TO 999

  999  CONTINUE
       RETURN
       END

       SUBROUTINE HASHBG(IMAGE,JOFDEST,NWRD_WID_FR)
C                                                7-MAR-1996/DSS
       integer     image(*)
       integer     jofdest
       integer     nwrd_wid_fr

       INTEGER     MSKHASH_ODD 			!... EVERY-OTHER BIT ON
       DATA        MSKHASH_ODD    / X'AAAAAAAAAAAAAAAA' /
       INTEGER     MSKHASH_EVEN
       DATA        MSKHASH_EVEN   / X'5555555555555555' /

       MASK = MSKHASH_ODD
       IF(MOD(JOFDEST,2) .EQ. 0) THEN
              MASK = ISHFT(MASK,-1)
       ENDIF

       M1 = 1 + NWRD_WID_FR*(JOFDEST-1)
       M2 = M1 + NWRD_WID_FR - 1
C      ... THIS CAN BE IMPROVED BY TESTING FOR MARGINS ...
       DO  IW = M1,M2
         IMAGE(IW) = IAND(IMAGE(IW),MASK)
       ENDDO
       RETURN
       END
