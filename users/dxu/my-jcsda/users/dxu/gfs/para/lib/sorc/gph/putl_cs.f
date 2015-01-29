      SUBROUTINE PUTL_CS(HEIGHT,KROT_PRI,ICMD,NCHAR,CTEXT)
C                                               2-NOV-1995/DSS
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    PUTL_CS     PUTLAB SUBR TO PRINT ITS CALL SEQ
C   PRGMMR: KRISHNA KUMAR      ORG: W/NP12   DATE: 1999-07-01
C
C ABSTRACT: CALLED ONLY FROM PUTLAB() TO PRINT THE CONTENTS OF
C   THE CALL SEQUENCE OF PUTLAB(); DISPLAYS ONLY THE FIRST 8 CHARACTERS
C   OF CTEXT.
C
C PROGRAM HISTORY LOG:
C   95-11-02  DAVID SHIMOMURA
C 1999-07-01  KRISHNA KUMAR    CONVERTED THIS CODE FROM CRAY TO
C                              IBM RS/6000.
C
C USAGE:    CALL PUTL_CS(HEIGHT,KROT_PRI,ICMD,NCHAR,CTEXT)
C   INPUT ARGUMENT LIST:  (SEE DOCBLOCK OF PUTLAB())
C     HEIGHT   - REAL      
C     KROT_PRI(2) - INT 
C     ICMD     - INT
C     NCHAR    - INT
C     CTEXT    - C*(*) CTEXT
C
C     (MORE INPUT ARGS VIA COMMON /PUTWHERE/ ... )
C     COMMON  /PUTWHERE/ LBLTAP,IERPUT,LCKPT_PUT,LCKPRNQQ,
C                        IJPXL_GIVN(2),IJPXL_LBL(2),IJPXL_NEXCH(2),
C                        NCALLS_PUT,ICOUNT_FONT(MAXFONT)
C
C     LCKPT_PUT - INT; CHECKPOINT LOCATION IN PUTLAB() FROM WHICH CALLED
C     IJPXL_GIVN(2) - INT; CONTAINING (IPT,JPT) OF PUTLAB CALL SEQ
C
C   OUTPUT FILES:
C     U:6 - INCLUDE IF ANY PRINTOUT
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C     CALLS ON SUBR XASC2ASC() TO TRANSLATE BYTES INTO PRINTABLE ASCII
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
C     ... TO PRINT GIVEN CALL SEQ OF SUBR PUTLAB() ...
C     ...     Split this out of S/R PUTL700() ...
C     ... SHOULD THIS BE CHANGED TO PRINT ENTIRE CTEXT???  NO,
C     ...     SAMPLE IS ENOUGH.

C     
      INTEGER      MAXFONT
      PARAMETER   (MAXFONT=63)
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      COMMON /PUTWHERE/ LBLTAP,IERPUT,LCKPT_PUT,LCKPRNQQ,
     1                  IJPXL_GIVN(2),IJPXL_LBL(2),IJPXL_NEXCH(2),
     2                  NCALLS_PUT,ICOUNT_FONT(MAXFONT)

      INTEGER      LBLTAP
      INTEGER      IERPUT
      INTEGER      LCKPT_PUT
      LOGICAL      LCKPRNQQ

      INTEGER      IWORD,JWORD
      EQUIVALENCE (IJPXL_LBL(1),IWORD)
      EQUIVALENCE (IJPXL_LBL(2),JWORD)

      INTEGER      NEWI,NEWJ
      EQUIVALENCE (IJPXL_NEXCH(1),NEWI)
      EQUIVALENCE (IJPXL_NEXCH(2),NEWJ)
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  

      REAL           HEIGHT
      INTEGER        KROT_PRI(2)
      INTEGER        NCHAR
      CHARACTER*(*)  CTEXT

      REAL           ANGLE
      INTEGER        ICMD

      INTEGER        IPT
      INTEGER        JPT

       INTEGER       MAGNCH
       LOGICAL       LANYTEXQ

       INTEGER       I8ACC
       CHARACTER*8   C8ACC
       EQUIVALENCE  (I8ACC,C8ACC)

       INTEGER       I8MQ
       CHARACTER*8   C8MQ
       EQUIVALENCE  (I8MQ,C8MQ)

C      . . . . . . .   S T A R T   . . . . . . . . . . . . . . . . .
C
       IPT = IJPXL_GIVN(1)
       JPT = IJPXL_GIVN(2)
C        ... IN ORDER TO PRINT THE FIRST 8 CHARS OF CTEXT, IN HEX,
C        ...   I NEED TO MOVE IT INTO AN INTEGER WORD
         MAGNCH = IABS(NCHAR)
         I8ACC = 0
         I8MQ = 0
         NCSTR = LEN(CTEXT)
         LANYTEXQ = .FALSE.
         NCH2PR = 8
         IF(NCSTR .LE. 0) THEN
C          ... NO LENGTH FROM GIVEN CTEXT()
           LANYTEXQ = .FALSE.
         ELSE IF(NCSTR .GT. 8) THEN
           NCH2PR = 8
           LANYTEXQ = .TRUE.
         ELSE
           NCH2PR = NCSTR
           LANYTEXQ = .TRUE.
         ENDIF
      
         IF(MAGNCH .LE. 0) THEN
C          ... case of nchar=0; which is for one char from int word
           LANYTEXQ = .TRUE.
           nch2pr = 8
           c8acc(1:) = ctext(1:8)
         ELSE
C          ... ABS(NCHAR) .GT. 0 ...
           IF(LANYTEXQ) THEN
             IF(MAGNCH .LE. 8) THEN
               C8ACC(1:) = CTEXT(1:MAGNCH)
               NCH2PR = MAGNCH 
             ELSE
               C8ACC(1:8) = CTEXT(1:8)
               NCH2PR = 8
             ENDIF
           ENDIF
         ENDIF         


         CALL XASC2ASC(C8ACC(1:NCH2PR),C8MQ(1:NCH2PR),NCHMOVED,IRETN)

         WRITE(6,7115)LCKPT_PUT,IPT,JPT,HEIGHT,KROT_PRI(1),KROT_PRI(2),
     1                ICMD,NCHAR,I8ACC,C8MQ(1:NCH2PR)
 7115    FORMAT(1H ,'PUTL_CS:AT',I5,'; ORG I,J=(',I7,',',I7,
     1               '); HGT=',F7.2,' KROT_PRI=',I6,',',I6,
     A         /1H ,'       ICMD=',I4,'; NCHAR=',I5,
     B              '; CTEXT=HEX ',Z16.16,';  ="',A,'"')

      RETURN
      END
