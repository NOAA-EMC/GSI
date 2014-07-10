      SUBROUTINE W3FI61 (LOC,ICAT,AREG,IBCKUP,IDATYP,IERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:  W3FI61        BUILD 40 CHAR COMMUNICATIONS PREFIX
C   PRGMMR: CAVANAUGH        ORG: NMC421      DATE:91-07-24
C
C ABSTRACT: USING INFORMATION FROM THE USER, BUILD A 40 CHARACTER
C   COMMUNICATIONS PREFIX AND PLACE IN INDICATED LOCATION.
C
C PROGRAM HISTORY LOG:
C   91-06-21  CAVANAUGH
C   91-09-20  R.E.JONES   CHANGES FOR SiliconGraphics 3.3 FORTRAN 77
C   93-03-29  R.E.JONES   ADD SAVE STATEMENT
C   94-04-28  R.E.JONES   CHANGE FOR CRAY 64 BIT WORD SIZE AND
C                         FOR ASCII CHARACTER SET COMPUTERS
C   02-10-15  VUONG       REPLACED FUNCTION ICHAR WITH MOVA2I
C
C USAGE:    CALL W3FI61 (LOC,ICAT,AREG,IBCKUP,IDATYP,IERR)
C   INPUT ARGUMENT LIST:
C     ICAT     - CATALOG NUMBER
C     AREG     - AFOS REGIONAL ADDRESSING FLAGS (6 POSITIONS)
C                   SELECT ANY OR ALL OF THE FOLLOWING. SELECTIONS
C                   WILL AUTOMATICALLY BE LEFT JUSTIFIED AND BLANK
C                   FILLED TO 6 POSITIONS.
C                IF BULLETINS AND/OR MESSAGES ARE NOT TO BE ROUTED
C                   TO AFOS, THEN LEAVE THE FIELD FILLED WITH BLANKS.
C               E - EASTERN REGION
C               C - CENTRAL REGION
C               W - WESTERN REGION
C               S - SOUTHERN REGION
C               A - ATLANTIC REGION
C               P - PACIFIC REGION
C     IERR     - ERROR RETURN
C     IBCKUP   - BACKUP INDICATOR W/HEADER KEY
C                0 = NOT A BACKUP
C                1 = FD BACKUP
C                2 = DF BACKUP
C                    BACK UP IS ONLY PERMITTED FOR FD AND DF BULLETINS
C     IDATYP   - DATA TYPE INDICATOR
C                0  = EBCIDIC DATA
C                11 = BINARY DATA
C                12 = PSUEDO-ASCII DATA
C                3  = ASCII DATA
C
C   OUTPUT ARGUMENT LIST:
C     LOC      - NAME OF THE ARRAY TO RECEIVE THE COMMUNICATIONS PREFIX
C
C REMARKS: ERROR RETURNS
C                  IERR  = 0   NORMAL RETURN
C                        = 1   INCORRECT BACKUP FLAG
C                        = 2   A REGIONAL ADDRESSING FLAG IS
C                              NON-BLANK AND NON-STANDARD ENTRY
C                        = 3   DATA TYPE IS NON-STANDARD ENTRY
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY C916-128, CRAY Y-MP8/864, CRAY Y-MP EL2/256
C
C$$$
      INTEGER        LOC(*)
      INTEGER        ICAT,IBCKUP,IDATYP
      INTEGER        IERR,IHOLD
C
      CHARACTER*6    AREG
      CHARACTER*8    AHOLD
      CHARACTER*6    ARGNL
      CHARACTER*1    BLANK
C
      LOGICAL        IBM370
C
      EQUIVALENCE    (IHOLD,AHOLD)
C
      SAVE
C
      DATA  ARGNL /'ECWSAP'/
C
C     BLANK WILL BE 40 HEX OR DECIMAL 64 ON AN IBM370 TYPE
C     COMPUTER, THIS IS THE EBCDIC CHARACTER SET.
C     BLANK WILL BE 20 HEX OR DECIMAL 32 ON A COMPUTER WITH THE
C     ASCII CHARACTER SET. THIS WILL BE USED TO TEST FOR CHARACTER 
C     SETS TO FIND IBM370 TYPE COMPUTER.
C
      DATA  BLANK /' '/
      DATA  IBM370/.FALSE./
C
C  ----------------------------------------------------------------
C
C     TEST FOR CRAY 64 BIT COMPUTER, LW = 8
C
      CALL W3FI01(LW)
C
C     TEST FOR EBCDIC CHARACTER SET
C
      IF (MOVA2I(BLANK).EQ.64) THEN
        IBM370 = .TRUE.
      END IF
C
      IERR      = 0
      INOFST    = 0
C BYTE  1                    SOH -  START OF HEADER
      CALL SBYTE (LOC,125,INOFST,8)
      INOFST    = INOFST + 8
C BYTE  2                    TRANSMISSION PRIORITY
      CALL SBYTE (LOC,1,INOFST,8)
      INOFST    = INOFST + 8
C BYTE  3-7                  CATALOG NUMBER
      IF (ICAT.GT.0) THEN
        IF (LW.EQ.4) THEN
          KK        = ICAT / 10
          CALL W3AI15 (KK,IHOLD,1,4,'-')
          IF (.NOT.IBM370) CALL W3AI39(IHOLD,4)
          CALL SBYTE (LOC,IHOLD,INOFST,32)
          INOFST    = INOFST + 32
          KK        = MOD(ICAT,10)
          CALL W3AI15 (KK,IHOLD,1,4,'-')
          IF (.NOT.IBM370) CALL W3AI39(IHOLD,4)
          CALL SBYTE (LOC,IHOLD,INOFST,8)
          INOFST    = INOFST + 8
        ELSE
          CALL W3AI15 (ICAT,IHOLD,1,8,'-')
          IF (.NOT.IBM370) CALL W3AI39(IHOLD,8)
          CALL SBYTE (LOC,IHOLD,INOFST,40)
          INOFST    = INOFST + 40
        END IF
      ELSE
          CALL SBYTE (LOC,-252645136,INOFST,32)
          INOFST    = INOFST + 32
          CALL SBYTE (LOC,240,INOFST,8)
          INOFST    = INOFST + 8
      END IF
C BYTE  8-9-10              BACK-UP FLAG FOR FD OR DF BULLETINS
C                                    0 = NOT A BACKUP
C                                    1 = FD
C                                    2 = DF
      IF (IBCKUP.EQ.0) THEN
C                              NOT A BACKUP
          CALL SBYTE (LOC,4210752,INOFST,24)
          INOFST    = INOFST + 24
      ELSE IF (IBCKUP.EQ.1) THEN
C                              BACKUP FOR FD
          CALL SBYTE (LOC,12764868,INOFST,24)
          INOFST    = INOFST + 24
      ELSE IF (IBCKUP.EQ.2) THEN
C                              BACKUP FOR DF
          CALL SBYTE (LOC,12764358,INOFST,24)
          INOFST    = INOFST + 24
      END IF
C BYTE  11                   BLANK
      CALL SBYTE (LOC,64,INOFST,8)
      INOFST    = INOFST + 8
C BYTE  12                   DATA TYPE
      IF (IDATYP.EQ.0) THEN
      ELSE IF (IDATYP.EQ.11) THEN
      ELSE IF (IDATYP.EQ.12) THEN
      ELSE IF (IDATYP.EQ.3) THEN
      ELSE
          IERR  = 3
          RETURN
      END IF
      CALL SBYTE (LOC,IDATYP,INOFST,8)
      INOFST    = INOFST + 8
C BYTES 13-18                AFOS REGIONAL ADDRESSING FLAGS
      CALL SBYTE (LOC,1077952576,INOFST,32)
      INOFST    = INOFST + 32
      CALL SBYTE (LOC,1077952576,INOFST,16)
      KRESET    = INOFST + 16
      INOFST    = INOFST - 32
      DO 1000 J = 1, 6
          DO 900 K = 1, 6
              IF (AREG(J:J).EQ.ARGNL(K:K)) THEN
C                 PRINT *,AREG(J:J),ARGNL(K:K),' MATCH'
                  IHOLD    = 0
                  IF (LW.EQ.4) THEN
                    AHOLD(4:4) = AREG(J:J)
                    IF (.NOT.IBM370) CALL W3AI39(IHOLD,4)
                  ELSE
                    AHOLD(8:8) = AREG(J:J)
                    CALL W3AI39(IHOLD,8)
                  END IF
                  CALL SBYTE (LOC,IHOLD,INOFST,8)
                  INOFST   = INOFST + 8
                  GO TO 1000
              ELSE IF (AREG(J:J).EQ.' ') THEN
C                 PRINT *,'BLANK SOURCE '
                  GO TO 1000
              END IF
  900     CONTINUE
          IERR  = 2
          RETURN
 1000 CONTINUE
      INOFST   = KRESET
C BYTES 19-39                UNUSED (SET TO BLANK)
      DO 1938 I = 1, 20, 4
          CALL SBYTE (LOC,1077952576,INOFST,32)
          INOFST    = INOFST + 32
 1938 CONTINUE
C BYTE  39                   MUST BE A BLANK
      CALL SBYTE (LOC,64,INOFST,8)
          INOFST    = INOFST + 8
C BYTE  40                   MUST BE A BLANK
      CALL SBYTE (LOC,64,INOFST,8)
C  ----------------------------------------------------------------
      RETURN
      END
