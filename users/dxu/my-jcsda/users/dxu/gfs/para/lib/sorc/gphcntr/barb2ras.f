      SUBROUTINE BARB2RAS(LCKPRNTQ,IJWORD,ITXTWORD,JWINDRAS,
     1                    IDOTDEL,JDOTDEL,IDOT_WID,JDOT_HGT,IRET_B2R)
C     ================================================================
      IMPLICIT INTEGER(A-W)

      integer   IRVR
      EXTERNAL  IRVR				!... INT FUNCTION IRVR
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      EXTERNAL     wndbarb
      COMMON /VECT/IBARBDEF
      INTEGER      IBARBDEF(9,36)
      CHARACTER*72 CBARBDEF(36)
      EQUIVALENCE (IBARBDEF(1,1),CBARBDEF(1))

C     ... WHERE IBARBDEF CONTAINS WIND VECTOR DEFINITIONS 
C     ...   FOR 36 DIRECTIONS

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     USAGE:  CALL BARB2RAS(LCKPRNTQ,IJWORD,ITXTWORD,JWINDRAS,
C ...        1              IDOTDEL,JDOTDEL,IDOT_WID,JDOT_HGT,IRET_B2R)
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      LOGICAL    LCKPRNTQ
      INTEGER    IJWORD
      INTEGER    ITXTWORD
      INTEGER    JWINDRAS(30)
C     ...WHERE  30 WORDS PER WIND VECTOR...
      INTEGER    IDOTDEL,JDOTDEL  	
C     ...   TO POSITION THE POINT OF THE WINDSTAFF AT THE DESIRED DOT
C     ...   WHEN VIEWED UPSIDE_DOWN WITH ORIGIN IN UPPER-LEFT, THEN
C     ... WHERE IDOTDEL: LEFTWARD (TOWARD FAXEDGE) 
C               JDOTDEL: UPWARD (TOWARD smaller values of SCANLINES)
C               IF IDOTDEL,JDOTDEL = (0,0) THEN
C                 THE SYMBOL IS POSITIONED IN LOWER-RIGHT QUADRANT
C                 AT THE DESIRED DOT.
C     ...   
      INTEGER    IDOT_WID,JDOT_HGT
      INTEGER    IRET_B2R

      INTEGER    KVECPRI
      DATA       KVECPRI            /3/
C     ...WHERE KVECPRI IS PRIORITY OF WIND VECTORS ...


      integer    DD
      integer    VCUT
      INTEGER    JST

      INTEGER    ADR(8)
      DATA       ADR            / -1,-1, 0, 1, 1, 1, 0,-1 /
      INTEGER    MOV(8)       !... 1  2  3  4  5  6  7  8 ...
      DATA       MOV            /  0, 1, 1, 1, 0,-1,-1,-1 /
C     ... WHERE ADR AND MOV ARE USED WHEN DECODING THE UNIT-VECTOR
C     ...   CODED VECTOR DEFINITION

      INTEGER    BIT      
      DATA       BIT         / Z'80000000' /

      INTEGER    CROTAT(2)
      integer    ERAS
      INTEGER    ERACE(36)
      DATA       ERACE    /
     A           Z'0001FFFF',Z'00000FFF',Z'00000003',
     B           Z'00000003',Z'00000003',Z'00000003',
     C           Z'00000003',Z'00000003',Z'00000003',
     D           Z'00000003',Z'00000003',Z'00000003',
     E           Z'00000003',Z'00000FFF',Z'0001FFFF',
     F           Z'0001FFFF',Z'0001FFFF',Z'000FFFFF',
     G           Z'0001FFFF',Z'00000FFF',Z'00000003',
     H           Z'00000003',Z'00000003',Z'00000003',
     I           Z'00000003',Z'00000003',Z'00000003',
     J           Z'00000003',Z'00000003',Z'00000003',
     K           Z'00000003',Z'00000FFF',Z'0001FFFF',
     L           Z'0001FFFF',Z'0001FFFF',Z'000FFFFF' /
      INTEGER    ERASEC
      DATA       ERASEC            /Z'00000080'/
      INTEGER    ERASET(2)

      INTEGER    ERRAY(10)
      INTEGER    FORBIT(2)
      INTEGER    ICHARA
      INTEGER    IDIS(2)
      INTEGER    IREVP
      DATA       IREVP             /5/
C ... IREVP IS THE (PRIORITY +1) USED FOR REVERSING A CHARACTER
      INTEGER    MAXPRI
      DATA       MAXPRI            /5/
      INTEGER    MSK8
      DATA       MSK8              /Z'0000000F'/
      INTEGER    MSKA1
      DATA       MSKA1             /Z'FF000000'/
      INTEGER    PRSNAP(3)
      INTEGER    SMASK(2)
      INTEGER    SYMW(2)

      INTEGER    K16THBIT
      DATA       K16THBIT       / X'0000000000008000' /
      INTEGER    KSIGNEXT
      DATA       KSIGNEXT       / X'FFFFFFFFFFFF0000' /
      INTEGER    MSK32B
      DATA       MSK32B         / X'00000000FFFFFFFF' /
      INTEGER    MSK16B
      DATA       MSK16B         / X'000000000000FFFF' /
      INTEGER    MSKIDOT
      DATA       MSKIDOT        / X'00001FFF' /
      INTEGER    MSKJDOT
      DATA       MSKJDOT        / X'00007FFF' /
      INTEGER    MSKPRIOR
      DATA       MSKPRIOR       / X'00000007' /
      INTEGER    MSKARROWUP
      DATA       MSKARROWUP     / X'00010000' /
      INTEGER    ICOUNTPR 
      DATA       ICOUNTPR       / 0 /


      INTEGER      IDOTS,JDOTS
      INTEGER      IACC
      INTEGER      IPRIOR

      LOGICAL      LEXFONTMODE
      LOGICAL      LSRNHEMI
      CHARACTER*1  SNAP(12)



      SAVE
C     -----------------------------------------------------------------

C      ... I EXPECT TO BE CALLED ONLY FOR WIND-VECTOR ITEM FROM LABEL-
C      ...   ARRAY

       IRET_B2R = 0

       IDOTS = IAND(IJWORD,MSKIDOT)
       JDOTS = IAND(MSKJDOT,(ISHFT(IJWORD,-17)))

       IPRIOR = IAND(MSKPRIOR,(ISHFT(IJWORD,-13)))

       IF((IAND(IJWORD,MSKARROWUP)) .NE. 0) THEN
         LEXFONTMODE = .TRUE.
       ELSE
         LEXFONTMODE = .FALSE.
       ENDIF

      
       IF(IPRIOR .NE. KVECPRI) THEN  	!... ERROR NOT A WIND-VECTOR
         ICOUNTPR = ICOUNTPR + 1
         IF(ICOUNTPR .LE. 20) THEN
           WRITE(6,FMT='(''PRTITLE::BARB2RAS:ERROR -- GIVEN A NON-'',
     1                   ''WIND-VECTOR ITEM'',
     2                   /1H ,7X, ''LABITEM=HEX '',Z8.8,1X,Z8.8)') 
     A                 IJWORD,ITXTWORD
         ENDIF
         IRET_B2R = 1
         GO TO 999
       ENDIF

C      ... OTHERWISE, THIS IS A WIND-VECTOR ITEM ...
C      ***DO WINDS

       LSRNHEMI = .FALSE.


C


      VCUT = IAND(ITXTWORD,MSK16B)      !... LO 16bits (signed) of txt
      IF(IAND(VCUT,K16THBIT) .NE. 0) THEN
        VCUT = IOR(VCUT,KSIGNEXT)
      ENDIF
      		
      DD = IAND((ISHFT(ITXTWORD,-16)),MSK16B)
      	                        	!... HI 16bits from LABEL txtwrd

      DDS = DD - 1
      IF((DDS .LT. 0) .OR.
     1   (DDS .GT. 35)) THEN
         ICOUNTPR = ICOUNTPR + 1
         IF(ICOUNTPR .LE. 20) THEN
           WRITE(6,FMT='(''PRTITLE::BARB2RAS:ERROR -- GIVEN AN '',
     1                   ''INVALID DD='',I8,
     2                   /1H ,7X, ''LABITEM=HEX '',Z8.8,1X,Z8.8)') 
     A                 DD,IJWORD,ITXTWORD
         ENDIF
        IRET_B2R = 2
        GO TO 999
      ENDIF
C   COMPUTE DISPLACEMENT FROM FIRST VECTOR DO DEFINITION
C     ...     					!...  VDIS=DDS*65
C     ...WHERE 65 BYTES PER VECTOR DEFINITION
C     USAGE:  CALL BARB2RAS(LCKPRNTQ,IJWORD,ITXTWORD,JWINDRAS,
C ...        1              IDOTDEL,JDOTDEL,IDOT_WID,JDOT_HGT,IRET_B2R)

      IDOTDEL = mova2i(cbarbdef(DD)(1:1))		!... VTD(VDIS+1)
      IDOTDEL = IDOTDEL - 1

      JDOTDEL = mova2i(cbarbdef(DD)(2:2))		!... VTD(VDIS+2)
      JST = JDOTDEL
      IDOT_WID = mova2i(cbarbdef(DD)(3:3))	!... VTD(VDIS+3)
      JDOT_HGT = mova2i(cbarbdef(DD)(4:4))	!... VTD(VDIS+4)
      VMAX    = mova2i(cbarbdef(DD)(5:5))	!... VTD(VDIS+5)

      IF(LCKPRNTQ) THEN
        icountpr = icountpr + 1
        if(icountpr .LT. 20) then

          PRINT  611, IDOTDEL, JDOTDEL,IDOT_WID,JDOT_HGT,VMAX, DD, VCUT
  611   FORMAT(1H ,'prtitle::barb2ras:CONSTANTS FOR BLDG VECTOR ARE...',
     1             ' IDOTDEL=', I3, 3X, 'JDOTDEL=', I3, 
     2        /1H , 7X, 'IDOT_WID=', I3,
     2              3X, 'JDOT_HGT=', I3, 3X, 'VMAX=', I4, 3X, 
     3        /1H , 7X, 'DD=', I3, 3X, 'VCUT=', I4)
        endif
      ENDIF

      IF(VCUT .LT. 0) THEN 		!... VCUT is negative valued ??
C       ... SOUTHERN HEMISPHERE VECTOR. SET FLAG>...
        LSRNHEMI = .TRUE.
        VCUT = iabs(VCUT)
      endif

      IF((VCUT .GT. VMAX) .OR.
     1   (VCUT .EQ. 0))    then
        IRET_B2R = 3
        GO TO 999
      ENDIF

C     ... OTHERWISE, (VCUT .GT. 0) SO ...

C     ... TO ZERO JWINDRAS ...
      DO  L=1,30
        JWINDRAS(L)=0
      ENDDO

C     ... BUILD VECTOR DEFINITION IN JWINDRAS ARRAY ...
C ... ...      VDIS=VDIS+6
      vdis = 9
C     ...WHICH MOVES POINTER PAST THE FIRST 5 CONSTANTS TO THE VECTOR
      VDISST = VDIS

      VNBYT = (VCUT + 1) / 2
      VDISEN = VDISST + VNBYT - 1
      VBIT = ISHFT(BIT,-IDOTDEL)
      IF(VBIT .EQ. 0) then
        VBIT=ISHFT(BIT,-IDOTDEL)
      ENDIF
C     ...FORCE THIS MACHINE TO TRY AGAIN...

      IF(LCKPRNTQ) THEN
        IF(icountpr .LE. 6) then
        PRINT  622, VDISST,VDISEN,VBIT,BIT
  622   FORMAT(1H , 'prtitle::BARB2RAS:VDISST=', I3, ';  VDISEN=', I3, 
     1         ';  VBIT=HEX ', Z8.8, ';  BIT=HEX ', Z8.8)
        endif
      ENDIF

      DO  I = 1,3
        PRSNAP(I) = 0
      ENDDO

      ISNP = 0
      JWINDRAS(JST) = VBIT

      DO  76  VBYTE = VDISST,VDISEN
        ERAS = mova2i(cbarbdef(DD)(VBYTE:VBYTE))	!... VTD(VBYTE)
        ISNP = ISNP + 1
        IF(ISNP .LE. 10) GO TO 637
C       ...OTHERWISE, SNAP LINE IS FULL SO PRINT IT OUT...
        IF(LCKPRNTQ) THEN
          IF(icountpr .GT. 6) GO TO 634
          PRINT  633, (PRSNAP(I),I=1,3)
  633     FORMAT(1H , 20X, 3Z8)
        ENDIF
  634   CONTINUE
        DO  I = 1,3
          PRSNAP(I) = 0
        ENDDO

        ISNP = 1
  637   CONTINUE
        SNAP(ISNP) = cbarbdef(DD)(VBYTE:VBYTE)	!... VTD(VBYTE)
        FORBIT(2) = IAND(ERAS,MSK8)
C       ...WHICH SAVES RIGHTMOST 4BITS OF ORIG 8BIT BYTE IN (2)
        FORBIT(1) = ISHFT(ERAS,-4)
C       ...WHICH SAVES 1ST 4BITS OF 8BIT BYTE
        DO 74 K=1,2
          SHF = FORBIT(K) + 1
          JST = ADR(SHF) + JST
          SSF = MOV(SHF)

          if(ssf .GT. 0) then
            VBIT = ISHFT(VBIT,-1)
          else if(ssf .LT. 0) then
            VBIT = ISHFT(VBIT,1)
          endif

          JWINDRAS(JST) = IOR(JWINDRAS(JST),VBIT)
          VCUT = VCUT - 1
          IF (VCUT .LE. 0) GO TO 76
   74   CONTINUE

   76 CONTINUE
C     ... in the old IBM code, it was working with 32 bits,
C     ...   so I will try left-justifying the 32-bits in the 64-bit word
      do  i = 1,30
        jwindras(i) = ishft(jwindras(i),32)
      enddo

      IF(LSRNHEMI) THEN
C...    ... FOR SOUTHERN HEMISPHERE REVERSE VECTOR WITHIN JWINDRAS ...
        DO I=1,30
          JWINDRAS(I) = IRVR(JWINDRAS(I),IDOT_WID)
        ENDDO
        IDOTDEL = IDOT_WID - IDOTDEL - 1
        IF(IDOTDEL .LT. 0) THEN
          IDOTDEL = 0
        ENDIF
      ENDIF
C     ... RETURN THE BIT-MAPPED WIND VECTOR IN JWINDRAS TO THE CALLER ...

      IF(LCKPRNTQ) THEN
C       ... failed here on too-long format ...
        icountpr = icountpr + 1
        if(icountpr .LT. 20) then
          PRINT  1030, SAVKF, (JWINDRAS(I),I=1,30)
 1030     FORMAT(1H ,'barb2ras:SAVKF=',I4,
     1             ';  VECTOR IN JWINDRAS FOLLOWS...',
     1      /1H ,(1X, Z16))
        endif
      ENDIF

C   FILL IN AREAS NEEDED TO PUT VECTOR IN JTABLE
      MDIS = SAVKF - 1
C     ...WHERE SAVKF IS DECREMENTED BECAUSE IT IS INCR IN PARA 52
      KKT=2
      SYMW(2)=4
      SMASK(2) = ERACE(DD)
      ERASET(2)=0
      SAVIX = IDOT
      SAVJX = JETHRO
      VEXIT = 1
      CROTO = 0
      CROTAT(2) = 0
      GO TO 999
C     *     *     *     *     *     *     *     *     *     *     *
  999 continue
      RETURN
      END
