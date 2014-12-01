      SUBROUTINE FORMAT(IHD3,AFSFIL,IRETN)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FORMAT      FORMATS AND WRITES AN AFOS PRODUCT.
C   PRGMMR: KRISHNA KUMAR      ORG: W/NP12   DATE: 1999-07-01
C
C ABSTRACT: WRITES AN AFOS PRODUCT TO THE OUTPUT FILE.
C
C PROGRAM HISTORY LOG:
C   94-06-29  ORIGINAL AUTHOR  LUKE LIN
C   95-01-04  LUKE LIN       CONVERT IT CFT-77.
C   95-01-06  LUKE LIN       REPLACE FFPUT BY UNBLOCKED WRITE.
C 1999-07-01  KRISHNA KUMAR  CONVERTED THIS CODE FROM CRAY 
C                            TO IBM RS/6000.
C
C USAGE:    CALL FORMAT(IHD3,AFSFIL,IRETN)
C   INPUT ARGUMENT LIST:
C     IHD3     - CHARACTER*23 ARRAY THAT CONTAINS THE AFOS PRODUCT
C              - COMMUNICATIONS HEADER.
C     AFSFIL   - AFOS OUTPUT FILE UNIT NUMBER
C     COMMON   - /ISPACE/LBLOCK,ICNTOT,LBNKFG
C     LBLOCK   - CHARACTER*1 16384 BYTE ARRAY THAT CONTAINS THE AFOS
C              - DATA IN UGF FORMAT.
C     ICNTOT   - INTEGER*4 WORD THAT CONTAINS THE NUMBER OF BYTES IN
C              - LBLOCK ARRAY.
C     LBNKFG   - INTEGER*4 FLAG WORD THAT TELLS IF A FILLER CHARATER
C              - WAS THE LAST BYTE IN LBLOCK ARRAY.
C              - =-1 THEN FILLER CHARATER.
C              - =0 THEN NO FILLER CHARATER.
C
C   OUTPUT ARGUMENT LIST:
C     IRETN    - =0, NORMAL
C                =1, EXCEED MAXIMUM AFOS BLOCKS
C
C
C   OUTPUT FILES:
C     FT06F001 - PRINT FILE.
C     FTXXF001 -  FILE THAT CONTAINS THE COMPLETED AFOS MAPS
C              -  WHERE XX = AFSFIL.
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
C
      COMMON  /ISPACEAF/LBLOCK,ICNTOT,LBNKFG
C
      CHARACTER*1280   KBLOCK
      INTEGER*8        KBLOCK8(160)
      EQUIVALENCE      (KBLOCK,KBLOCK8(1))
C
      CHARACTER*23 IHD3
C
      CHARACTER*7 TSTPAT
      INTEGER*8   ITSTPAT
      EQUIVALENCE (TSTPAT,ITSTPAT)
C
      CHARACTER*4 IBEG
      INTEGER*8   IIBEG
      EQUIVALENCE (IBEG,IIBEG)
C
      CHARACTER*2 DLEFF
      INTEGER*8   IDLEFF
      EQUIVALENCE (DLEFF,IDLEFF)
      CHARACTER*2 DLEDLE
      INTEGER*8   IDLEDLE
      EQUIVALENCE (DLEDLE,IDLEDLE)
C
      CHARACTER*1 DLE
      CHARACTER*1 ETX
      CHARACTER*1 FSTLST
      CHARACTER*1 ILAST
      CHARACTER*1 LBLOCK(16384)
C
      INTEGER*8   IOVFLOW(7)
      CHARACTER*1 OVFLOW(49)
      EQUIVALENCE (IOVFLOW(1),OVFLOW(1))
C
      CHARACTER*1 PAD
      CHARACTER*1 ZERO
C
      INTEGER   AFSBLK
      INTEGER   AFSFIL
      INTEGER   MAXBYT
      INTEGER   NUMBIG
      INTEGER   NUMBYT
C
C      NUMBYT IS BYTE COUNTER THAT RANGES FROM 1 TO 256 WHICH IS THE
C      MAX NUMBER OF BYTES IN AN AFOS UGF RECORD.
C
      LOGICAL     MAXBLK
C
      DATA       IDLEDLE / Z'1010000000000000' /
      DATA       IDLEFF  / Z'100C000000000000' /
      DATA       IIBEG   / Z'0100000000000000' /
      DATA       ITSTPAT / Z'C500003205CE3700' /
      DATA       IOVFLOW    /Z'C500003205004441',Z'5441204F56455246',
     1                       Z'4C4F57202D544849',Z'53204D4150204D41',
     2                       Z'59204E4F54204245',Z'20434F4D504C4554',
     3                       Z'4500000000000000'/
      DATA        MAXBYT   /256/
C     DATA        PAD    / ZFF /
C     DATA        ZERO   / Z00 /
C     DATA        DLE    / Z10 /
C     DATA        ETX    / Z83 /
C     DATA        FSTLST / ZC0 /
C     DATA        ILAST  / Z80 /
C
      ZERO = CHAR(00)
      PAD  = CHAR(255)
      DLE  = CHAR(16)
      ETX  = CHAR(131)
      FSTLST = CHAR(192)
      ILAST = CHAR(128)
C
C     CHECK TO SEE IF LBNKFG IS -1 IF .TURE. THE SUBTRACT ONE FROM
C     ICNTOT AND RESET LBNKFG TO 0.
C
      IF(LBNKFG.EQ.-1)THEN
       ICNTOT = ICNTOT - 1
       LBNKFG = 0
       WRITE(6,FMT='('' FORMAT: BLANK FILLER FLAG WAS ON.'')')
      ENDIF
C
C
      IRETN = 0
      NOBLOK = 0
      MAXBLK=.FALSE.
       WRITE(6,FMT='('' FORMAT: AFOS PRODUCT HAS'',I6,
     1    '' BYTES.'')')ICNTOT
C
      IF(ICNTOT.GT.14848) THEN
       WRITE(6,FMT='('' FORMAT: THIS AFOS PRODUCT HAS'',I6,
     1    '' BYTES. WHICH EXCEEDS 64 BLOCKS.'')')ICNTOT
C
         ICNTOT=14848
         MAXBLK=.TRUE.
         IRETN = 1
      ENDIF
C
C
      JCNTOT=1
      NUMBIG=0
       WRITE(6,FMT='('' FORMAT: WRITING AFOS MAP TO FT'',I2,
     1    ''F001'')')AFSFIL
C
      AFSBLK=1
C
         KBLOCK(1:23) = IHD3(1:23)
C
      NUMBYT=24
      JCNTOT=24
      DO 100 LOOP=1,ICNTOT
C     ...TEST FOR DATA OVERFLOW
          IF(.NOT.(MAXBLK)) GO TO 110
C
          IF(LOOP.EQ.13) THEN
C            ...PUT OVERFLOW MSG ON MAP
             DO 182 I3=1,49
                IF(NUMBYT.EQ.(MAXBYT+1)) THEN
                    IF(AFSBLK.EQ.5) THEN
C     ...OTHERWISE 1280 BYTE COMMS RECORD IS FULL AND MUST BE OUTPUT
                       NUMBIG=NUMBIG+1
                       AFSBLK=0
                       JCNTOT=1
C                      CALL FFPUT(KBLOCK,AFSFIL,1280)
  149                  FORMAT( 80 ( 16(1X,Z2),/))
                       WRITE(AFSFIL)KBLOCK8
C
                       NOBLOK = NOBLOK + 1
                    ENDIF
                    AFSBLK=AFSBLK+1
                    NUMBYT=1
C
C                  LOAD BEGINNING OF RECORD FLAG.
C
                       KBLOCK(JCNTOT:JCNTOT+3)=IBEG(1:4)
                       JCNTOT=JCNTOT+4
C
                    NUMBYT=5
                ENDIF
C
                KBLOCK(JCNTOT:JCNTOT)=OVFLOW(I3)
                JCNTOT=JCNTOT+1
                NUMBYT=NUMBYT+1
  182        CONTINUE
          ENDIF
C
  110     CONTINUE
          IF(NUMBYT.EQ.(MAXBYT+1)) THEN
                    IF(AFSBLK.EQ.5) THEN
C     ...   OTHERWISE 1280 BYTE COMMS RECORD IS FULL AND MUST BE OUTPUT
                       NUMBIG=NUMBIG+1
                       AFSBLK=0
                       JCNTOT=1
C                      CALL FFPUT(KBLOCK,AFSFIL,1280)
                       WRITE(AFSFIL)KBLOCK8
                       NOBLOK = NOBLOK + 1
C                      PRINT *,' NO OF BLOCK=', NOBLOK, AFSFIL
C                      WRITE(6,149) (KBLOCK(MM),MM=1,1280)
                    ENDIF
                    AFSBLK=AFSBLK+1
                    NUMBYT=1
C
C                  LOAD BEGINNING OF RECORD FLAG.
C
                       KBLOCK(JCNTOT:JCNTOT+3)=IBEG(1:4)
                       JCNTOT=JCNTOT+4
                    NUMBYT=5
                ENDIF
C
  120     CONTINUE
          IF(LBLOCK(LOOP).EQ.ETX) THEN
              KBLOCK(JCNTOT:JCNTOT) = DLEFF(1:1)
              JCNTOT=JCNTOT+1
              NUMBYT=NUMBYT+1
              IF(NUMBYT.EQ.(MAXBYT+1)) THEN
                    IF(AFSBLK.EQ.5) THEN
C                 THE 1280 BYTE COMMS RECORD IS FULL AND MUST BE OUTPUT
                       NUMBIG=NUMBIG+1
                       AFSBLK=0
                       JCNTOT=1
C                      CALL FFPUT(KBLOCK,AFSFIL,1280)
                       WRITE(AFSFIL)KBLOCK8
                       NOBLOK = NOBLOK + 1
C                      PRINT *,' NO OF BLOCK=', NOBLOK, AFSFIL
C                      WRITE(6,149) (KBLOCK(MM),MM=1,1280)
                    ENDIF
                    AFSBLK=AFSBLK+1
                    NUMBYT=1
C
C                  LOAD BEGINNING OF RECORD FLAG.
C
                       KBLOCK(JCNTOT:JCNTOT+3)=IBEG(1:4)
                       JCNTOT=JCNTOT+4
                    NUMBYT=5
              ENDIF
C
              KBLOCK(JCNTOT:JCNTOT) = DLEFF(2:2)
              JCNTOT=JCNTOT+1
              NUMBYT=NUMBYT+1
              GO TO 100
C
          ELSE IF(LBLOCK(LOOP).EQ.DLE) THEN
              KBLOCK(JCNTOT:JCNTOT) = DLEDLE(1:1)
              JCNTOT=JCNTOT+1
              NUMBYT=NUMBYT+1
              IF(NUMBYT.EQ.(MAXBYT+1)) THEN
                    IF(AFSBLK.EQ.5) THEN
C     ...   OTHERWISE 1280 BYTE COMMS RECORD IS FULL AND MUST BE OUTPUT
                       NUMBIG=NUMBIG+1
                       AFSBLK=0
                       JCNTOT=1
C
C                      CALL FFPUT(KBLOCK,AFSFIL,1280)
                       WRITE(AFSFIL)KBLOCK8
                       NOBLOK = NOBLOK + 1
C                      PRINT *,' NO OF BLOCK=', NOBLOK, AFSFIL
C                      WRITE(6,149) (KBLOCK(MM),MM=1,1280)
                    ENDIF
                    AFSBLK=AFSBLK+1
                    NUMBYT=1
C
C                  LOAD BEGINNING OF RECORD FLAG.
C
                       KBLOCK(JCNTOT:JCNTOT+3)=IBEG(1:4)
                       JCNTOT=JCNTOT+4
                    NUMBYT=5
              ENDIF
C
              KBLOCK(JCNTOT:JCNTOT) = DLEDLE(2:2)
              JCNTOT=JCNTOT+1
              NUMBYT=NUMBYT+1
              GO TO 100
C
          ELSE
              KBLOCK(JCNTOT:JCNTOT)=LBLOCK(LOOP)
              JCNTOT=JCNTOT+1
              NUMBYT=NUMBYT+1
              GO TO 100
          ENDIF
  100 CONTINUE
C     ...NOW ADD FORMAT VERSION INDICATOR IN UPPER LEFT OF MAP
C     ...SKIP IF DATA OVERFLOW
      IF(.NOT. MAXBLK) THEN
         DO  I3=1,7
              IF(NUMBYT.EQ.(MAXBYT+1)) THEN
                    IF(AFSBLK.EQ.5) THEN
C                  OUTPUT A 1280 BYTE RECORD BECAUSE IT IS FULL.
                       NUMBIG=NUMBIG+1
                       AFSBLK=0
                       JCNTOT=1
C                      CALL FFPUT(KBLOCK,AFSFIL,1280)
                       WRITE(AFSFIL)KBLOCK8
                       NOBLOK = NOBLOK + 1
                    ENDIF
                    AFSBLK=AFSBLK+1
                    NUMBYT=1
C
C                  LOAD BEGINNING OF RECORD FLAG.
C
                       KBLOCK(JCNTOT:JCNTOT+3)=IBEG(1:4)
                       JCNTOT=JCNTOT+4
                    NUMBYT=5
                ENDIF
C
            KBLOCK(JCNTOT:JCNTOT)=TSTPAT(I3:I3)
            JCNTOT=JCNTOT+1
            NUMBYT=NUMBYT+1
         ENDDO
      ENDIF
      IF(NUMBYT.EQ.(MAXBYT+1)) THEN
C        THIS BLOCK IS EXACTLY FULL, SO PUT ETX IN NEXT BLOCK
         IF(AFSBLK.EQ.5) THEN
            NUMBIG=NUMBIG+1
            AFSBLK=0
            JCNTOT=1
            NOBLOK = NOBLOK + 1
C            CALL FFPUT(KBLOCK,AFSFIL,1280)
             WRITE(AFSFIL)KBLOCK8
C                      WRITE(6,149) (KBLOCK(MM),MM=1,1280)
         ENDIF
         AFSBLK=AFSBLK+1
C
C                  LOAD BEGINNING OF RECORD FLAG.
C
                       KBLOCK(JCNTOT:JCNTOT+3)=IBEG(1:4)
                       JCNTOT=JCNTOT+4
         NUMBYT=5
      ENDIF
      JCNTOT=JCNTOT-(NUMBYT-3)
      KBLOCK(JCNTOT:JCNTOT)=ILAST
      JCNTOT=JCNTOT+(NUMBYT-3)
      KBLOCK(JCNTOT:JCNTOT)=ETX
      JCNTOT=JCNTOT+1
      IF(MOD(JCNTOT,2).EQ.0) THEN
         KBLOCK(JCNTOT:JCNTOT)=PAD
         JCNTOT=JCNTOT+1
      ENDIF
C
C       CHECK TO SEE IF WE ARE STILL WORKING ON THE 1ST 1280
C       BYTE BUFFER.
C
      IF(NUMBIG.EQ.0)THEN
C
C       WORKING ON 1ST 1280 BYTE BUFFER SO,
C       CHECK TO SEE IF WE ARE STILL WORKING ON THE 1ST 256
C       BYTE RECORD IN THIS 1280 BYTE BUFFER.
C
        IF(AFSBLK.EQ.1)THEN
C
C         LOAD FIRST AND LAST FLAG INTO THE 3RD BYTE OF KBLOCK.
C
         KBLOCK(3:3)=FSTLST(1:1)
        ENDIF
      ENDIF
C
C         FILL REST OF KBLOCK WITH ZERO.
C
      DO  JJ=JCNTOT,1280
         KBLOCK(JJ:JJ)=ZERO
      ENDDO
C
                       NOBLOK = NOBLOK + 1
         WRITE(6,FMT='('' FORMAT: NUMBER OF BLOCKS='',I4)')NOBLOK
C
C     CALL FFPUT(KBLOCK,AFSFIL,1280)
      WRITE(AFSFIL)KBLOCK8
C
C
C
      IRECD=5*NUMBIG+AFSBLK
C
      WRITE(6,FMT='('' FORMAT: THIS MAP HAS'',I3,'' RECORDS.'')')
     1  IRECD
C
      RETURN
      END
