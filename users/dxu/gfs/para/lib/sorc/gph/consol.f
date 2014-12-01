      SUBROUTINE CONSOL(JBUFF)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    CONSOL      BUILDS A MESSAGE LINE FOR MSGLOG FILE.
C   PRGMMR: HENRICHSEN       ORG: W/NP21     DATE: 1999-07-06
C
C ABSTRACT: ADDS THE TIME AND JOBSTEP NAME TO THE MESSAGE AND WRITES
C   TO A MESSAGE LOG FILE.
C
C
C PROGRAM HISTORY LOG:
C   96-04-11  ORIGINAL AUTHOR  HENRICHSEN
C   97-11-25  LUKE LIN         ADD SAVE STATEMENT
C   98-05-15  BILL FACEY       CONVERT TO F90; REPLACED W3FQ02
C                              WITH W3LOCDAT; CHANGED DATE FORMAT 
C 1999-06-11  PETER HENRICHSEN MADE Y2K COMPLIANT, MODIFY TO RUN ON THE IBM,
C                              REPLACED CALL TO INT2CH WITH BIN2CH.
C 1999-07-061 PETER HENRICHSEN MODIFIED TO ADJUSTED THE SIZE OF THE JBUFF
C             VARIABLE TO BE CONTROLLED BY THE USER.   
C
C USAGE:    CALL CONSOL(JBUFF)
C   INPUT ARGUMENT LIST:
C     JBUFF    -  CHARACTER(80) ARRAY THAT CONTAINS THE MESSAGE IN
C              -  HOLLERTH TERMINATED BY A COLON.
C              -  A 'COLON' TERMINATES THE MESSAGE TEXT.
C              -  IF THE COLON IS OMITTED THE MESSAGE TERMINATES
C              -  AFTER 80 BYTES.
C              -  A 'SEMI-COLON' SERVES AS LINE TERMINATOR WITH IN A
C              -  MULITI-LINE MESSAGE. TWO OR MORE ADJACENT SEMI-
C              -  COLONS DENOTE NULL (BLANK) LINES.
C
C   INPUT FILES:
C     FT75F001 - JOBSMSG FILE
C
C   OUTPUT FILES:
C     FT06F001 - PRINT FILE
C     FT75F001 - JOBSMSG FILE
C
C ATTRIBUTES:
C   LANGUAGE: IBM FORTRAN 90.
C   MACHINE:  IBM
C
C$$$
C
C
C
      CHARACTER*(*) JBUFF
      CHARACTER(80) LBUFF
      CHARACTER(80) KBUFF
      INTEGER      IBUFF(10)
      CHARACTER(80) LINES(3)
      CHARACTER(40 )KINES(6)
      DATA         KINES
     1    /'BYTES(09-12) IS THE IS CUR PAGE # BYTES(',
     2     '21-24) IS LINE # OF THE NXT LN TO WRITE ',
     3     'BYTES(33-36) IS THE LINE ADDRESS OF CURR',
     4     'ENT PAGE.                               ',
     5     'BYTES(45-48) IS THE MAX NUMBER OF PAGES ',
     6     'IN THE FILE.                            '/
C    1    /'1234567890123456789012345678901234567890'/
      CHARACTER(80) POINTR
      INTEGER      CONTRL(10)
      CHARACTER(80) CHTITL
      CHARACTER(80) CHDATA
      INTEGER      DATALN(10)
C
      CHARACTER(60) CHTITS
      DATA         CHTITS
     1/'JOBSMSG LOG ..YYYY/MM/DD HHMMSSZ    PAGE     OF      PAGES. '/
C
C    1/'123456789012345678901234567890123456789012345678901234567890'/
C
      CHARACTER(40) CBLANK
      DATA         CBLANK /'                                        '/
C    1                    /'1234567890123456789012345678901234567890'/
      CHARACTER(48) CHCNTL
      DATA         CHCNTL
     1/'CUR PGE=   1 NXT LN=   6 PGE LN=   5 MAX PG=  20'/
C    1/'123456789012345678901234567890123456789012345678901234567890'/
      CHARACTER(32) CHLAST
      DATA         CHLAST  /' THE CONTROL RECORD.            '/
C    1                     /'12345678901234567890123456789012'/
C
      CHARACTER(18) CHTIME
      DATA          CHTIME  /'YYYY/MM/DD HHMMSSZ'/
      CHARACTER(8) CHWORK
C                           /'1234567890123456789012'/
C
      CHARACTER(4)  CHBYT4

      CHARACTER(4)  PAGES(20)
      DATA          PAGES    /'  01','  02','  03','  04',
     1                        '  05','  06','  07','  08',
     2                        '  09','  10','  11','  12',
     3                        '  13','  14','  15','  16',
     4                        '  17','  18','  19','  20'/

      CHARACTER(1)  BLANK
      DATA          BLANK    /' '/
      CHARACTER(1)  LF
      CHARACTER(1 ) LZERO
      DATA          LZERO    /'0'/
C
      INTEGER    CTRLIN
      DATA       CTRLIN      /001/
C
      INTEGER    KTIME (8)
      INTEGER    MAXCHR
      DATA       MAXCHR      /80/
      INTEGER    MAXPGE
      DATA       MAXPGE      /20/
C
      INTEGER    LOGFIL
      DATA       LOGFIL      /75/
      INTEGER    NEXT
      INTEGER    NXWRIT      
C
C      THE LOGICAL FLAGS
C
      LOGICAL    LCONTU
      LOGICAL    LLCONT
      LOGICAL    LPRINT
      LOGICAL    LPROB
      LOGICAL    LUPCON
      LOGICAL    LSTOP
      LOGICAL    LUPDAT
      LOGICAL    LUPFST
C
      EQUIVALENCE(IBUFF,KBUFF)
      EQUIVALENCE(LINES,KINES)
      EQUIVALENCE(POINTR,CONTRL)
      EQUIVALENCE(CHDATA,DATALN)
C
      SAVE

      OPEN(UNIT=LOGFIL,ACCESS='DIRECT',RECL=80)

       LF = CHAR(10)
       LPRINT= .FALSE.
       LSTOP = .FALSE.
       LUPCON= .FALSE.
       LUPDAT= .FALSE.
       LUPFST= .FALSE.
       LPROB = .FALSE.
C
C      1ST SEARCH FOR END OF LINE. THE LINE ENDS WITH A :  OR
C      WHEN A THE LAST NO BLANK FOLLOWED BY BLANKS.
C
        ILEN = LEN_TRIM(JBUFF)
         WRITE(6,FMT='('' CONSOL: NUMBER OF CHARACTERS IN '',
     1   ''JBUFF IS ='',I4,/,'' JBUFF=>'',A,''<'')')
     2   ILEN,JBUFF(1:ILEN)
      

        DO  I = ILEN,1,-1
            IF(JBUFF(I:I).EQ.':')THEN
               LBUFF(1:I) = JBUFF(1:I)
             NUMCH = I
C            WRITE(6,FMT='('' CONSOL: FOUND A COLAN AT '',I2,
C    1      '' LBUFF IS'',A)')I,LBUFF(1:NUMCH)
             GO TO 5
 
            ENDIF
        ENDDO 

            LBUFF(1:ILEN) = JBUFF(1:ILEN)
            NUMCH = ILEN
 
C            WRITE(6,FMT='('' CONSOL: DID NOT FIND A COLAN!'',/,
C    1      '' LBUFF IS'',A)')LBUFF(1:ILEN)        
   5    CONTINUE         

C23456         

C      SET NAMKEY
          NAMKEY =  4
          MAMKEY =  NAMKEY


C        GET SYSTEM Z TIME.

          CALL W3LOCDAT(KTIME)

C      WHERE KTIME HAS THE FOLLOWING:

C        WRITE(6,FMT='('' CONSOL: THE TIME FROM W3LOCDAT:'',
C     1  8(I8))')KTIME         

          IYEAR =  KTIME(1)
C         IYEAR =  KTIME(1)-(KTIME(1)/100) * 100
          IMONTH = KTIME(2)
          IDAY   = KTIME(3)
          IHOUR  = KTIME(5) + (-1*(KTIME(4)/100))
          IMIN   = KTIME(6)
          ISEC   = KTIME(7) 

C          CONVERT Z TIME TO HOLLERTH
  

C     CHARACTER(18) CHTIME  /'YYYY/MM/DD HHMMSSZ'/
C                             123456789012345678
C
          WRITE(UNIT=CHTIME,FMT='(I4,''/'',I2,''/'',I2,'' '',
     1    3(I2),''Z'',A)')IYEAR,IMONTH,IDAY,IHOUR,IMIN,ISEC

C
            DO  I = 1,18
               IF(I.EQ.11)THEN
C
C              THE 11TH POSITION SHOULD BE A BLANK.
C
               ELSE
C                CHECK FOR BLANKS AND REPLACE WITH '0'.
                 IF(CHTIME(I:I).EQ.BLANK)THEN
                     CHTIME(I:I) = LZERO
                 ENDIF
               ENDIF
C
            ENDDO
C          WRITE(6,FMT='('' CONSOL: THE TIME IS:'',A)')CHTIME(1:18)
C
          CHTITS(15:32) = CHTIME(1:18)
          
C        READ IN CONTROL POINTERS.

        READ(UNIT=LOGFIL,REC=CTRLIN,ERR=10)CONTRL
        
C       CONTRL(10) IS EQUIVALENCED TO POINTR(1:80)
C
          WRITE(6,FMT='('' CONSOL: READ IN THE CONTROL LINE'',
     1   '' OF :'',/,'' '',A)')POINTR(1:80)
           IF(POINTR(21:24).EQ.'****')THEN
             POINTR(21:24) = '0006'
             WRITE(6,FMT='('' CONSOL: THE CONTROL LINE IS '',
     1       '' NOW :'',/,'' '',A)')POINTR(1:80)             
           ENDIF
                
C
        GOTO 20
 10     CONTINUE
C
C        THIS IS A EMPTY FILE FILL CONTROL LINE WITH VALID VALUES.
C
              POINTR(1:48) = CHCNTL(1:48)
              POINTR(49:80)= CHLAST(1:32)
              POINTR(80:80)= LF
C
          WRITE(6,FMT='('' CONSOL: EMPTY FILE MAKE THE CONTROL'',
     1   '' LINE :'',/,'' '',A)')POINTR(1:80)

             WRITE(UNIT=LOGFIL,REC=CTRLIN)CONTRL
            DO  K = 1,03
              IPOINT = CTRLIN + K
              KBUFF(1:80) = LINES(K)(1:80)
              KBUFF(80:80) = LF
             WRITE(UNIT=LOGFIL,REC=IPOINT)IBUFF
C             WRITE(6,FMT='('' CONSOL: WRITING TO RECORD # '',I2,
C     1       '' LINES('',I1,'') =:'',A)')IPOINT,K,KBUFF(1:80)
            ENDDO
C
C          CHECK TO SEE IF THE CONTROL LINE AS BEEN CLOBBERED.
C          IF IT HAS RESTOR IT WITH VALID VALUES.
C
 20         IF(POINTR(1:8).EQ.CHCNTL(1:8))THEN
C
C             THE CONTROL LINE IS OK.
C
C        WRITE(6,FMT='('' CONSOL: THE CONTROL LINE IS OK!'')')
            ELSE
C
C            RESTOR CONTROL LINE WITH VALID VALUES.
C
              POINTR(1:48) = CHCNTL(1:48)
              POINTR(49:80)= CHLAST(1:32)
              POINTR(80:80)= LF
C
         WRITE(6,FMT='('' CONSOL: RESTORING THE CONTROL LINE'',
     1   '' TO :'',/,'' '',A)')POINTR(1:80)
C
C   SAMPLE POINTER LINE(LINE ONE IN DATA SET):
C   CUR PGE=   6 NXT LN= 102 PGE LN=  95 MAX PG=  20 THE CONTROL RECORD
C   BYTES(09-12) IS THE IS THE CURRENT PAGE NUMBER (I4).
C   BYTES(21-24) IS THE LINE NUMBER OF THE NEXT LINE TO WRITE (I4).
C   BYTES(33-36) IS THE LINE ADDRESS OF CURRENT PAGE (I4).
C   BYTES(45-48) IS THE MAX NUMBER OF PAGES IN THE FILE (I4).
C
            ENDIF
C
C         GET CURRENT PAGE NUMBER.
C
             CHBYT4 = POINTR(9:12)
C         WRITE(6,FMT='('' CONSOL: GOT PAGE # FROM POINTR(9:12) '',
C     1   A)')POINTR(9:12)
             READ(UNIT=CHBYT4,FMT='(I4)')IPAGE
             CHBYT4 = POINTR(9:12)
             
C         WRITE(6,FMT='('' CONSOL: GOT NEXT LINE # FROM '',
C     1   ''POINTR(21:24)'',A)')POINTR(21:24)
                  
             CHBYT4 = POINTR(21:24)
C
C          GET LINE NUM WHERE TO WRITE MESSAGE.
C
             READ(UNIT=CHBYT4,FMT='(I4)')IRITE
             CHBYT4 = POINTR(45:48)
C
C          GET NUMBER OF PAGES IN FILE.
C
             READ(UNIT=CHBYT4,FMT='(I4)')IMAXPG
             IF(IPAGE.GT.MAXPGE)THEN
                   LPROB = .TRUE.
                   POINTR(9:12) = PAGES(1)(1:4)
                   POINTR(21:24)= PAGES(6)(1:4)
                   POINTR(45:48)= PAGES(20)(1:4)
                   IPAGE = 1
                   IRITE = 6
                   IMAXPG = MAXPGE
             ENDIF
C
C               CACULATE LAST RECORD.
C
               LASTRC = IMAXPG*18 + 4
C
C               SET RETUTN TO FIRST PAGE POINTER
C
               KRETRN = IMAXPG + 1
C
 25         CONTINUE
C
C           COMES HERE FOR INITALIZING THE PAGE LINE.
C
               CHDATA = CBLANK//CBLANK
C
C              NOW  CHDATA IS FULL OF BLANKS
C
C
C
               IPUTIT = (IPAGE-1)*18 + 5
               IENDPG = IPUTIT + 18
C
C       LOAD PAGE NUMBER INTO PAGE CHTITS LINE.
C
            CHTITS(43:44) = PAGES(IPAGE)(3:4)
            CHTITS(51:52) = PAGES(IMAXPG)(3:4)
            CHTITL(1:60)  = CHTITS(1:60)
            CHTITL(61:80) = CBLANK(1:20)
            CHDATA(1:80)  = CHTITL(1:80)
C
C        FINISHED WITH PAGE LINE SO I MUST WRITE IT ******
C
   36  CONTINUE
C
C        COMES HERE TO WRITE THE DATA LINE TO THE FILE.
C
            CHDATA(80:80) = LF
        WRITE(UNIT=LOGFIL,REC=IPUTIT)DATALN
C       WRITE(UNIT=LOGFIL,REC=IPUTIT,FMT='(A)')CHDATA(1:80)
C
          IF(LPRINT)THEN
C             WRITE(6,FMT='('' CONSOL:I JUST WROTE '',/,'' '',A,/,
C     1     '' TO RECORD '',I4,'' WHEN IRITE='',I4,'' IPAGE='',I4)')
C     2         CHDATA(1:80),IPUTIT,IRITE,IPAGE
          ENDIF
C
          IF(LSTOP) GO TO 470
C
C        FILL CHDATA ARRAY WITH BLANKS.
C
              CHDATA = CBLANK//CBLANK
C
          IF(LCONTU)THEN
             IRITE = IRITE + 1
             IF(IRITE.EQ.IENDPG)THEN
               KPAGE = IPAGE + 1
               IF(KPAGE.GT.IMAXPG)THEN
               ELSE
C
C               THIS WAS NOT LAST PAGE SO I TURN ON LUPCON FLAG.
C
                LUPCON = .TRUE.
                WRITE(6,FMT='('' I HAVE TURNED ON THE LUPCON FLAG'')')
               ENDIF
             ENDIF
          ENDIF
          IF(IRITE.EQ.IENDPG)THEN
             IPAGE = IPAGE + 1
             LUPDAT = .TRUE.
            IF(IPAGE.GT.IMAXPG)THEN
              WRITE(6,FMT='('' CONSOL:I HAVE FILLED THE LAST PAGE '',
     1       ''AND THE NEXT PAGE WOULD BE:'',I4,'' AND IRITE'',I4)')
     2         IPAGE,IRITE
C
               IPAGE = 1
               IRITE = 5
              WRITE(6,FMT='('' CONSOL:SO I MUST  SET THE PAGE TO '',
     1         I2,'' AND IRITE TO '',I2)')IPAGE,IRITE
              LPRINT = .TRUE.
             LUPFST = .TRUE.
            ENDIF
C
C        I MUST GO BACK AND INITIALIZE THE PAGE LINE AND
C         THEN WRITE IT OUT.
C
             GO TO 25
          ENDIF
C
C        I AM NOW WORKING ON A LINE OF TEXT.
C
        IPUTIT = IRITE
C
             CHDATA(1:7) = CHTIME(12:18)
C
             CALL SETNAM(CHDATA,NAMKEY)
              K = (NAMKEY + 1)*9
              NUMBYT = K + 1
          IF(LCONTU)THEN
                 IF(LBUFF(MEND:MEND).EQ.':')THEN
                  MEND  = MEND -1
                 ENDIF
             LSTBYT = NUMBYT + MEND - 1
C
C           THIS WAS A CONTINUATION LINE LOAD TEXT INTO CHDATA &
C           WRITE OUT TO FILE.
C
             IF(LLCONT) THEN
C
C             THIS CONTINUATION WAS BECAUSE OF A SEMICOLN.
C
                  ITOTBY = NUMBYT + MEND
                  IF(ITOTBY.GT.MAXCHR)THEN
C
C                   SPLIT THE LINE
C
                    IDIF = ITOTBY - MAXCHR
                    IEND    = MEND - IDIF
                      LSTBYT = NUMBYT + IEND - 1
                     IDIF = ITOTBY - LSTBYT
                     CHDATA(NUMBYT:LSTBYT) = LBUFF(1:IEND)
C
C                    SAVE THE REST OF LINE FOR NEXT PASS.
C
                     LBUFF(1:IDIF) = LBUFF(IEND+1:MEND)
                     MEND = IDIF
                     NAMKEY = MAMKEY
                     LCONTU = .TRUE.
                     LPRINT = .TRUE.
                  ELSE
                   CHDATA(NUMBYT:LSTBYT) = LBUFF(1:MEND)
                    LCONTU= .FALSE.
                    LSTOP = .TRUE.
                  ENDIF
                LLCONT = .FALSE.
             ELSE
               LCONTU= .FALSE.
               LSTOP = .TRUE.
               CHDATA(NUMBYT:LSTBYT) = LBUFF(1:MEND)
             ENDIF
          ELSE
C
C           THIS WAS NOT A CONTINUATION LINE SO SEARCH FOR END
C
C           DO 50  I = MAXCHR,1,-1
            DO 50  I = NUMCH,1,-1
C
C             SEARCH FOR 1ST NON BLANK STARTING FROM END OF LBUFF
C
               IF(LBUFF(I:I).EQ.BLANK)THEN
               ELSE
C
C                FOUND 1ST NON BLANK.
C
                  KKEND = I
                 IF(LBUFF(I:I).EQ.':')THEN
                  KKEND = I -1
                 ELSE
                 ENDIF
                  ITOTBY = NUMBYT + KKEND
                     KBUFF  = CBLANK//CBLANK
                    KBUFF(1:KKEND) = LBUFF(1:KKEND)
                     IIEND  = KKEND
                    DO  J = 1,IIEND
C
C                     SEARCH FOR A SIMICOLN TO SEE IF THIS LINE IS
C                     TO BE SPLIT.
C
                      IF(KBUFF(J:J).EQ.';')THEN
                       IEND = J
                       KEND = J -2
                       LSTBYT = NUMBYT + KEND
                       IDIF = ITOTBY - LSTBYT
                       CHDATA(NUMBYT:LSTBYT) = LBUFF(1:IEND)
C
C                         SAVE THE REST OF LINE FOR NEXT PASS.
C
                         LBUFF(1:IDIF) = LBUFF(IEND+1:KKEND)
                       MEND = IDIF
                       NAMKEY = MAMKEY
                       LCONTU = .TRUE.
                       LLCONT = .TRUE.
                       GO TO 36
                      ELSE
                      ENDIF
                    ENDDO
C
C                     DID NOT FIND A SIMICOLN SO SEE IF IT IS TOO LONG
C                     AND MUST BE BE SPLIT.
C
                  IF(ITOTBY.GT.MAXCHR)THEN
C
C                   SPLIT THE LINE
C
                    IDIF = ITOTBY - MAXCHR
                    IEND    = KKEND - IDIF
                      LSTBYT = NUMBYT + IEND - 1
                     IDIF = ITOTBY - LSTBYT
                     CHDATA(NUMBYT:LSTBYT) = LBUFF(1:IEND)
C
C                    SAVE THE REST OF LINE FOR NEXT PASS.
C
                     LBUFF(1:IDIF) = LBUFF(IEND+1:KKEND)
                     MEND = IDIF
                     NAMKEY = MAMKEY
                     LCONTU = .TRUE.
                  ELSE
C
C                    SHORT ENOUGH LOAD THE LINE INTO CHDATA
C
                   IEND = KKEND
                   LSTBYT = NUMBYT + (IEND - 1)
                    CHDATA(NUMBYT:LSTBYT) = LBUFF(1:IEND)
                   LCONTU= .FALSE.
                   LSTOP = .TRUE.
                  ENDIF
                GO TO 36
               ENDIF
  50        CONTINUE
          ENDIF
C
C         CONTINUATION LINE GO AND WRITE IT!
C
         GO TO 36
C
 400    CONTINUE
               IPAGE = IPAGE + 1
 410         IF(IPAGE.GE.KRETRN)IPAGE = 1
C    1/'CUR PGE=   1 NXT LN=   6 PGE LN=   5 MAX PG=  20'/
C    1/'123456789012345678901234567890123456789012345678901234567890'/
               NPAGE     = (IPAGE - 1)*18 + 5
               NXWRIT = NPAGE + 1
               
C         WRITE(6,FMT='('' CONSOL: BEFORE BIN2CH NXWRIT ='',
C     1       I8)')NXWRIT
 415           CALL BIN2CH(NXWRIT,CHWORK,4,'A999')
C        WRITE(6,FMT='('' CONSOL: AFTER BIN2CH CHWORK ='',
C     1       A)')CHWORK(1:8) 
               POINTR(21:24) = CHWORK(1:4)
C
 420           CALL BIN2CH(IPAGE,CHWORK,4,'A999')
               POINTR(09:12) = CHWORK(1:04)
               CALL BIN2CH(NPAGE,CHWORK,4,'A999')
               POINTR(33:36) = CHWORK(1:04)
C
 450     CONTINUE
C
C
C
C        WRITE OUT CONTROL LINE
C
        WRITE(UNIT=LOGFIL,REC=CTRLIN)CONTRL
C       WRITE(UNIT=LOGFIL,REC=CTRLIN,FMT='(A)')POINTR(1:80)
C
C         WRITE(6,FMT='('' JUST WROTE THIS CONTROL RECORD TO:'',
C     1     '' FT75F001.'',/,'' '',A)')POINTR(1:80)
               GO TO 800
 470        CONTINUE
              NEXT = IPUTIT + 1
C             WRITE(6,FMT='('' CONSOL: BEFORE BIN2CH NEXT ='',
C     1       I8)')NEXT
               CALL BIN2CH(NEXT,CHWORK,4,'A999')
C           WRITE(6,FMT='('' CONSOL: THE NEXT LINE NUMBER TO WRITE '',
C     1     ''TO IS:'',A)')CHWORK(1:4)
               POINTR(21:24) = CHWORK(1:4)
        IF(LUPDAT)THEN
           WRITE(6,FMT='('' CONSOL: LUPDAT IS ON, POINTR RECORD IS :''
     1     ,/,'' '',A)')POINTR(1:80)
          IF(LUPFST)THEN
           NPAGE  = 5
           LUPFST = .FALSE.
           GO TO 420
          ELSE
           IF(LUPCON)THEN
             NXWRIT = NEXT
         WRITE(6,FMT='('' CONSOL: NXWRIT ='',
     1       I8)')NXWRIT
             NPAGE  = (IPAGE-1)*18+5
             LUPCON = .FALSE.
             WRITE(6,FMT='('' CONSOL: LUPCON IS ON, NXWRIT IS'',I4,
     1      '' NPAGE IS'',I4,'' IPAGE IS'',I4)')NXWRIT,NPAGE,IPAGE
             GO TO 415
           ENDIF
           GO TO 410
          ENDIF
        ENDIF
        IF(NEXT.EQ.IENDPG)GO TO 400
               GO TO 450
 800        CONTINUE
      RETURN
      END
      SUBROUTINE SETNAM(CHDATA,KEY)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    SETNAM      INITIALIZES THE MESSAGES FOR A PROGRAM.
C   PRGMMR: HENRICHSEN       ORG: W/NP21     DATE: 1999-06-11
C
C ABSTRACT: PLACES THE JOBSTEP, JOBNAME OR PROCSTEP NAME IN
C   CHDATA ARRAY STARTING AT BYTE 9.
C
C PROGRAM HISTORY LOG:
C   83-12-12  ORIGINAL AUTHOR  HENRICHSEN
C   93-07-07  HENRICHSEN CONVERT TO FORTRAN 77.
C   96-04-17  HENRICHSEN CONVERT TO RUN ON CRAY
C 1999-06-11  HENRICHSEN CONVERT TO RUN ON IBM
C
C
C
C USAGE:  CALL SETNAM(CHDATA,KEY)
C
C   INPUT ARGUMENTS:
C     CHDATA   -   CHARACTER(80) ARRAY THAT HAS THE TIME IN THE FIRST
C              -   SEVEN BYTES IN CHARACTER FORMAT.
C     KEY      -   AN INTEGER*4 FLAG USED TO DETERMINE WHAT NAME WILL
C              -   PLACED IN CHDATA(9:20).
C              -   IF = 1, USE JOB NAME.
C              -   IF = 2, USE STEP NAME.
C              -   IF = 3, USE PROC STEP NAME.
C              -   IF = 4, USE JOB NAME AND STEP NAME.
C              -   IF = 5, USE JOB NAME ,STEP NAME AND PROC NAME.
C
C
C   INPUT FILES:  NONE
C
C   INPUT VIA COMMON: NONE
C
C   OUTPUT ARGUMENTS:
C     CHDATA   - CHARACTER(80) ARRAY THAT NOW HAS NAME(S) STARTING IN
C              - 9TH BYTE TERMINATED BY A COLN AT END.
C     KEY      - INTEGER*4 FLAG RETURNED AS 1,2,0R 3 AND IS USED TO
C              - SET STARTING BYTE LOCATION IN INDATA FOR USER MESSAGE
C
C   OUTPUT FILES:
C     FT06F001 -   PRINT FILE
C
C
C ATTRIBUTES:
C   LANGUAGE: IBM FORTRAN 90.
C   MACHINE:  IBM
C
C$$$
C
C
C . . . INITIALIZES THE OUTPUT MESSAGES WITH JOB, STEP OR PROCSTEP NAME.
C
C
      CHARACTER(80)  CHDATA
      CHARACTER(80)  PGMNAM
      CHARACTER*40  BLNK40
      DATA          BLNK40/'                                        '/
      CHARACTER*28  LWORK
      DATA          LWORK      /'                            '/
      CHARACTER*24  JNAMES
      DATA          JNAMES    /'                        '/
      CHARACTER*24  STEPNM
      CHARACTER*24  JOBNAM
      DATA          JOBNAM    /'                        '/
      CHARACTER*8   BUF
      DATA          BUF        /'        '/
      CHARACTER*8   BLANKS
      DATA          BLANKS     /'        '/


      INTEGER       ILOC(3)
      DATA          ILOC       /1,9,17/
      LOGICAL       FOUNDN

      PGMNAM = BLNK40//BLNK40


      CALL GETARG(0,PGMNAM)
      JCHARS = LEN_TRIM(PGMNAM) 
    
C        WRITE(6,FMT='('' SETNAM: PGMNAM=>'',A,''<'',
C    1   '' JCHARS ='',I4)')PGMNAM(1:JCHARS),JCHARS
          FOUNDN = .FALSE.
          DO I = JCHARS,1,-1
               J = I - 1          
C         WRITE(6,FMT='('' SETNAM: IN LOOP,I=>'',I4,''<'',
C    1   '' JCHARS ='',I4,'' J='',I4)')I,JCHARS,J

             IF(PGMNAM(J:J).EQ.'.')THEN
               IEND = J-1
               ISTR = IEND - 7
C              WRITE(6,FMT='('' SETNAM: FOUND A "."  AT LOCATION:'',
C    1        I2,'' SO PROGRAM NAME IS :'',/,'' '',A)')
C    2        J,PGMNAM(ISTR:IEND)
             ELSE
               IEND = JCHARS
               ISTR = JCHARS-7
             ENDIF
              IF(IEND.LE.0)THEN
               BUF(1:8) =  'NAME_MSG'
               FOUNDN = .TRUE.
               GOTO 10
              ELSE
               IF(ISTR.LE.0)THEN
                ISTR = 1
               ENDIF
                NUMC = IEND - ISTR +1
              ENDIF
               BUF(1:NUMC) = PGMNAM(ISTR:IEND)
               FOUNDN = .TRUE.
               GOTO 10
      
          ENDDO
        IF(FOUNDN)THEN
        ELSE
          BUF(1:8) = PGMNAM(73:80)
        ENDIF
 10     JNAMES(9:16) = BUF(1:8)
        CALL GETENV('LOADL_JOB_NAME',JOBNAM)
         ILEN = LEN_TRIM(JOBNAM)   
C       WRITE(6,FMT='('' SETNAM: JOBNAM=>'',A,''<'',
C    1   '' ILEN='',I4)')JOBNAM(1:8),ILEN
C23456      
        IF(JOBNAM(1:8).EQ.BLANKS)THEN
         JNAMES(1:8) = 'MSG-JOBNM'
        ELSE
         JNAMES(1:8) = JOBNAM(1:8)
        ENDIF
C        WRITE(6,FMT='('' SETNAM: JNAMES='',A)')JNAMES(1:16)
C
          LUPEND = 1
      IF(KEY.LT.4)THEN
        IF(KEY.LT.1)THEN
           KEY = 1
        ENDIF
C
          LOC = ILOC(KEY)
        CHDATA(9:16) = JNAMES(LOC:LOC+7)
        CHDATA(17:17) = ':'
C
      ELSE
          IF(KEY.EQ.4)THEN
            ISTOP  = 2
          ELSE
            ISTOP  = 3
          ENDIF
C
C     LOAD JNAME  INTO MESSAGES..............................
C
            LOCEND  = ILOC(ISTOP) + 7
            STEPNM(1:LOCEND) = JNAMES(1:LOCEND)
C
          IF(JNAMES(17:24).EQ.BLANKS(1:8))THEN
           LUPEND = 2
          ELSE
           LUPEND = ISTOP
          ENDIF
C
              KK    = 1
              KSTRT = 0
              IADD  = 1
          DO I = 1,LUPEND
               KSTRT = KSTRT + IADD
               KEND  = KSTRT + 7
             DO  K = KSTRT,KEND
                LWORK (KK:KK) = STEPNM(K:K)
                    KK = KK + 1
             ENDDO
                LWORK (KK:KK) = BLANKS(1:1)
                    KK = KK + 1
           KSTRT = KEND
          ENDDO
        LWORK(KK-1:KK-1) = ':'
        CHDATA(9:36) = LWORK(1:28)
      ENDIF
      KEY = LUPEND
      RETURN
      END
