      SUBROUTINE W3FI85(ISTEP,IUNITB,IUNITD,IBFSIZ,ISECT1,ISECT3,
     *    JIF,JDESC,NEWNR,IDATA,RDATA,ATEXT,KASSOC,
     *    KIF,KDESC,NRDESC,ISEC2D,ISEC2B,
     *    KDATA,KARY,KBUFR,IERRTN)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    W3FI85      GENERATE BUFR MESSAGE
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 93-12-03
C
C ABSTRACT: USING INFORMATION AVAILABLE IN SUPPLIED ARRAYS, GENERATE
C   A BUFR MESSAGE (WMO CODE FM94).  THERE MAY BE  A SECTION 2
C   INCLUDED IN THE BUFR MESSAGE IF THE USER FOLLOWS PROPER PROCEDURE.
C   MESSAGES ARE CONSTRUCTED IN ACCORDANCE WITH BUFR EDITION 2. ENTRIES
C   FOR SECTION 1 MUST BE PASSED TO THIS ROUTINE IN THE ISECT1 ARRAY.
C   ENTRIES FOR SECTION 3 MUST BE PASSED TO THIS ROUTINE IN ISECT3.
C
C
C       IN THE EVENT THAT THE USER REQUESTS A REDUCTION OF REPORTS
C   IN A BUFR MESSAGE IF A PARTICULAR MESSAGE BECOMES OVERSIZED, THE
C   POSSIBILITY EXISTS OF THE LAST BLOCK OF DATA PRODUCING AN OVERSIZED
C   MESSAGE. THE USER MUST VERIFY THAT ISECT3(6) DOES IN FACT EQUAL
C   ZERO TO ASSURE THAT ALL OF THE DATA HAS BEEN INCLUDED AS OUTPUT.
C
C PROGRAM HISTORY LOG:
C   93-09-29  CAVANAUGH
C   94-03-22  J. HOPPA  - CORRECTED AN ERROR WHEN WRITING THE
C                         DESCRIPTORS INTO THE BUFR MESSAGE
C   94-03-31  J. HOPPA  - ADDED THE SUBSET NUMBER TO THE PARAMETER LIST
C                         OF SUBROUTINE FI8501
C   94-04-15  J. HOPPA  - ADDED KBUFR TO THE PARAMETER LIST OF
C                         SUBROUTINE FI8502
C   94-04-20  J. HOPPA  - ADDED THE KDATA PARAMETER COUNTER TO THE
C                         PARAMETER LIST OF SUBROUTINE FI8501
C   95-04-29  J. HOPPA  - CHANGED NQ AND N TO KARY(2)
C                       - CHANGED JK TO KARY(11)
C                       - ADDED AN ASSIGNMENT TO KARY(2) SO HAVE
C                         SOMETHING TO PASS TO SUBROUTINES
C                       - DELETED JK AND LL FROM CALL TO FI8501
C
C USAGE: CALL W3FI85(ISTEP,IUNITB,IUNITD,IBFSIZ,ISECT1,ISECT3,
C    *    JIF,JDESC,NEWNR,IDATA,RDATA,ATEXT,KASSOC,
C    *    KIF,KDESC,NRDESC,ISEC2D,ISEC2B,
C    *    KDATA,KARY,KBUFR,IERRTN)
C   INPUT ARGUMENT LIST:
C     ISTEP    - KEY FOR SELECTION OF PROCESSING STEP
C            1  = PROCESS INTEGER/TEXT ARRAY INTO KDATA
C            2  = PROCESS REAL/TEXT ARRAY INTO KDATA
C            3  = CONSTRUCT BUFR MESSAGE
C     IUNITB   - UNIT NUMBER OF DEVICE CONTAINING TABLE B
C     IUNITD   - UNIT NUMBER OF DEVICE CONTAINING TABLE D
C     IBFSIZ   - SIZE IN BYTES OF BUFR MESSAGE ARRAY (KBUFR)
C                  SHOULD BE A MULTIPLE OF WORD SIZE.
C     ISECT1   - CONTAINS INFORMATION TO ENTER INTO SECTION 1
C          ( 1) EDITION NUMBER
C          ( 2) BUFR MASTER TABLE NUMBER
C                   0 = METEOROLOGICAL
C                   OTHERS NOT YET DEFINED
C          ( 3) ORIGINATING CENTER - SUBCENTER NUMBER
C          ( 4) ORIGINATING CENTER NUMBER
C          ( 5) UPDATE SEQUENCE NUMBER
C          ( 6) OPTIONAL SECTION FLAG
C                   SHOULD BE SET TO ZERO UNLESS USER
C                   WRITE ADDITIONAL CODE TO ENTER LOCAL
C                   INFORMATION INTO SECTION 3
C          ( 7) BUFR MESSAGE TYPE
C          ( 8) BUFR MESSAGE SUB_TYPE
C          ( 9) MASTER TABLE VERSION NUMBER
C          (10) LOCAL TABLE VERSION NUMBER
C          (11) YEAR OF CENTURY    - REPRESENTATIVE OF DATA
C          (12) MONTH              - REPRESENTATIVE OF DATA
C          (13) DAY                - REPRESENTATIVE OF DATA
C          (14) HOUR               - REPRESENTATIVE OF DATA
C          (15) MINUTE             - REPRESENTATIVE OF DATA
C          (16)-(20)  UNUSED
C
C     ISECT3   - VALUES TO BE INSERTED INTO SECTION 3, AND
C                TO CONTROL REPORT REDUCTION FOR OVERSIZED MESSAGES
C          (1)  NUMBER OF SUBSETS
C                   DEFINES THE NUMBER OF SUBSETS BEING PASSED TO THE
C                   ENCODER ROUTINE FOR INCLUSION INTO A BUFR MESSAGE.
C                   IF THE USER HAS SPECIFIED THE USE OF THE
C                   SUBSET/REPORT REDUCTION ACTIVATION SWITCH, THEN
C                   A PART OF THOSE SUBSETS MAY BE USED FOR THE CURRENT
C                   MESSAGE AND THE REMAINDER RETAINED FOR A
C                   SUBSEQUENT MESSAGE.
C          (2)  OBSERVED FLAG
C                   0 = OBSERVED DATA
C                   1 = OTHER DATA
C          (3)  COMPRESSED FLAG
C                   0 = NONCOMPRESSED
C                   1 = COMPRESSED
C          (4)  SUBSET/REPORT REDUCTION ACTIVATION SWITCH
C                   USED TO CONTROL THE NUMBER OF REPORTS ENTERED INTO
C                   A BUFR MESSAGE WHEN MAXIMUM MESSAGE SIZE IS EXCEEDED
C                   0 = OPTION NOT ACTIVE
C                   1 = OPTION IS ACTIVE. UNUSED SUBSETS WILL BE
C                       SHIFTED TO LOW ORDER POSITIONS OF ENTRY ARRAY.
C                   2 = OPTION IS ACTIVE. UNUSED SUBSETS WILL REMAIN
C                       IN ENTRY POSITIONS.
C
C                       NOTE:- IF THIS FLAG IS SET TO ANY OTHER
C                       VALUES, PROGRAM WILL BE TERMINATED WITH AN
C                       ERROR CONDITION.
C          (5)  NUMBER OF REPORTS TO DECREMENT BY, IF OVERSIZED MESSAGE
C                   (MINIMUM VALUE = ONE).  IF ZERO IS ENTERED, IT WILL
C                   BE REPLACED BY ONE.
C          (6) NUMBER OF UNUSED REPORTS RETURNED TO USER
C          (7) NUMBER OF REPORTS INCLUDED IN MESSAGE
C          (8) NUMBER OF TABLE B ENTRIES AVAILABLE TO DECODER
C          (9) NUMBER OF TABLE D ENTRIES AVAILABLE TO DECODER
C         (10) TEXT INPUT FLAG
C                   0  = ASCII INPUT
C                   1  = EBCIDIC INPUT
C
C     JIF      - JDESC INPUT FORMAT FLAG
C                      0  = F X Y
C                      1  = DECIMAL FORMAT
C     JDESC    - LIST OF DESCRIPTORS TO GO INTO SECTION 3
C                 EACH DESCRIPTOR = F * 16384 + X * 256 + Y
C                     THEY MAY OR MAY NOT BE AN EXACT MATCH OF THE
C                     WORKING DESCRIPTOR LIST IN KDESC.  THIS SET OF
C                     DESCRIPTORS MAY CONTAIN SEQUENCE DESCRIPTORS TO
C                     PROVIDE ADDITIONAL COMPRESSION WITHIN THE BUFR
C                     MESSAGE.  THERE MAY BE AS FEW AS ONE SEQUENCE
C                     DESCRIPTOR, OR AS MANY DESCRIPTORS AS THERE ARE
C                     IN KDESC.
C     NEWNR    - NR OF DESCRIPTORS IN JDESC
C     IDATA    - INTEGER ARRAY DIMENSIONED BY THE NUMBER OF
C                DESCRIPTORS TO BE USED
C     RDATA    - REAL ARRAY DIMENSIONED BY THE NUMBER OF
C                DESCRIPTORS TO BE USED
C     ATEXT    - ARRAY CONTAINING ALL TEXT DATA ASSOCIATED WITH A
C                SPECIFIC REPORT.  ALL DATA IDENTIFIED AS TEXT DATA MUST
C                BE IN ASCII.
C     KASSOC   - INTEGER ARRAY DIMENSIONED BY THE NUMBER OF DESCRIPTORS
C                TO BE USED, CONTAINING THE ASSOCIATED FIELD VALUES
C                FOR ANY ENTRY IN THE DESCRIPTOR LIST.
C     KIF      - KDESC INPUT FORMAT FLAG
C                      0  = F X Y
C                      1  = DECIMAL FORMAT
C     KDESC    - LIST OF DESCRIPTORS TO GO INTO SECTION 3
C                     FULLY EXPANDED SET OF WORKING DESCRIPTORS. THERE
C                     SHOULD BE AN ELEMENT DESCRIPTOR FOR EVERY DATA
C                     ENTRY, BUT THERE SHOULD BE
C                            NO SEQUENCE DESCRIPTORS
C     NRDESC   - NR OF DESCRIPTORS IN KDESC
C     ISEC2D - DATA OR TEXT TO BE ENTERED INTO SECTION 2
C     ISEC2B - NUMBER OF BYTES OF DATA IN ISEC2D
C
C   OUTPUT ARGUMENT LIST:
C     KDATA    - SOURCE DATA ARRAY . A 2-DIMENSION INTEGER ARRAY
C                      WHERE KDATA(SUBSET,PARAM)
C                             SUBSET = SUBSET NUMBER
C                             PARAM  = PARAMETER NUMBER
C     KARY     - WORKING ARRAY FOR MESSAGE UNDER CONSTRUCTION
C            (1) UNUSED
C            (2) PARAMETER POINTER
C            (3) MESSAGE BIT POINTER
C            (4) DELAYED REPLICATION FLAG
C                   0 = NO DELAYED REPLICATION
C                   1 = CONTAINS DELAYED REPLICATION
C            (5) BIT POINTER FOR START OF SECTION 4
C            (6) UNUSED
C            (7) NR OF BITS FOR PARAMETER/DATA PACKING
C            (8) TOTAL BITS FOR ASCII DATA
C            (9) SCALE CHANGE VALUE
C           (10) INDICATOR (USED IN W3FI85)
C                                1 = NUMERIC DATA
C                                2 = TEXT DATA
C           (11) POINTER TO CURRENT POS IN KDESC
C           (12) UNUSED
C           (13) UNUSED
C           (14) UNUSED
C           (15) DATA TYPE
C           (16) UNUSED
C           (17) UNUSED
C           (18) WORDS ADDED FOR TEXT OR ASSOCIATED FIELDS
C           (19) LOCATION FOR TOTAL BYTE COUNT
C           (20) SIZE OF SECTION 0
C           (21) SIZE OF SECTION 1
C           (22) SIZE OF SECTION 2
C           (23) SIZE OF SECTION 3
C           (24) SIZE OF SECTION 4
C           (25) SIZE OF SECTION 5
C           (26) NR BITS ADDED BY TABLE C OPERATOR
C           (27) BIT WIDTH OF ASSOCIATED FIELD
C           (28) JDESC INPUT FORM FLAG
C                      0 = DESCRIPTOR IN F X Y FORM
C                                  F IN JDESC(1,I)
C                                  X IN JDESC(2,I)
C                                  Y IN JDESC(3,I)
C                      1 = DESCRIPTOR IN DECIMAL FORM IN JDESC(1,I)
C           (29) KDESC INPUT FORM FLAG
C                      0 = DESCRIPTOR IN F X Y FORM
C                                  F IN KDESC(1,I)
C                                  X IN KDESC(2,I)
C                                  Y IN KDESC(3,I)
C                      1 = DESCRIPTOR IN DECIMAL FORM IN KDESC(1,I)
C           (30) BUFR MESSAGE TOTAL BYTE COUNT
C     KBUFR    - ARRAY TO CONTAIN COMPLETED BUFR MESSAGE
C     IERRTN   - ERROR RETURN FLAG
C     KSEQ     - WORKING ARRAY FOR TABLE D INITIAL SEARCH KEY
C     KNUM     - WORKING ARRAY FOR TABLE D NUMBER OF DESC'S IN SEQ
C     KLIST    - WORKING ARRAY FOR TABLE D SEQUENCES
C     ANAME    - TABLE B  DESCRIPTOR NAMES
C     AUNITS   - TABLE B  DESCRIPTOR UNITS
C     LDESC    - TABLE B  DECIMAL EQUIV OF F X Y VALUES
C     KSCALE   - TABLE B  STANDARD SCALE VALUES
C     KFRVAL   - TABLE B  REFERENCE VALUES
C     KRFVSW   - TABLE B  SWITCHES TO INDICATE IF HAVE NEW/OLD REF VAL
C     NEWRFV   - TABLE B  NEW REFERENCE VALUES
C     KWIDTH   - ARRAY OF BIT WIDTHS FOR EACH ENTRY IN TABLE B
C
C REMARKS:
C         IERRTN    = 0    NORMAL RETURN, BUFR MESSAGE RESIDES IN KBUFR
C                          IF ISECT3(4)= 0, ALL REPORTS HAVE BEEN
C                                           PROCESSED INTO A BUFR
C                                           MESSAGE
C                          IF ISECT3(4)= 1, A BUFR MESSAGE HAS BEEN
C                                          GENERATED WITH ALL OR PART OF
C                                          THE DATA PASSED TO THIS
C                                          ROUTINE. ISECT3(6) CONTAINS
C                                          THE NUMBER OF REPORTS THAT
C                                          WERE NOT USED BUT ARE BEING
C                                          HELD FOR THE NEXT MESSAGE.
C                   = 1    BUFR MESSAGE CONSTRUCTION WAS HALTED
C                          BECAUSE CONTENTS EXCEEDED MAXIMUM SIZE
C                          (ONLY WHEN ISECT3(4) = 0)
C                   = 2    BUFR MESSAGE CONSTRUCTION WAS HALTED
C                          BECAUSE OF ENCOUNTER WITH A DESCRIPTOR
C                          NOT FOUND IN TABLE B.
C                   = 3    ROUTINE WAS CALLED WITH NO SUBSETS
C                   = 4    ERROR OCCURED WHILE READING TABLE B
C                   = 5    AN ATTEMPT WAS MADE TO EXPAND JDESC
C                          INTO KDESC, BUT A DESCRIPTOR INDICATING
C                          DELAYED REPLICATION WAS ENCOUNTERED
C                   = 6    ERROR OCCURED WHILE READING TABLE D
C                   = 7    DATA VALUE COULD NOT BE CONTAINED
C                          IN SPECIFIED BIT WIDTH
C                   = 8    DELAYED REPLICATION NOT PERMITTED
C                          IN COMPRESSED DATA FORMAT
C                   = 9    AN OPERATOR DESCRIPTOR 2 04 YYY OPENING
C                          AN ASSOCIATED FIELD (YYY NOT EQ ZERO)
C                          WAS NOT FOLLOWED BY THE DEFINING DESCRIPTOR
C                          0 31 021 (7957 DECIMAL).
C                   = 10   DELAYED REPLICATION DESCRIPTOR WAS NOT
C                          FOLLOWED BY DESCRIPTOR FOR DELAYED
C                          REPLICATION FACTOR.
C                                0 31 001
C                                0 31 002
C                                0 31 011
C                                0 31 012
C                   = 11   ENCOUNTERED A REFERENCE VALUE THAT FORCED A
C                          DATA ELEMENT TO BECOME NEGATIVE
C                   = 12   NO MATCHING TABLE D ENTRY FOR SEQUENCE
C                          DESCRIPTOR.
C                   = 13   ENCOUNTERED A NON-ACCEPTABLE DATA ENTRY FLAG.
C                          ISECT3(6) SHOULD BE 0 OR 1.
C                   = 14   CONVERTING DESCRIPTORS FXY->DECIMAL,
C                          NUMBER TO CONVERT = 0
C                   = 15   NO DESCRIPTORS SPECIFIED FOR SECTION 3
C                   = 16   INCOMPLETE TABLE B, NUMBER OF DESCRIPTORS
C                          IN TABLE B DOES NOT MATCH NUMBER OF
C                          DESCRIPTORS NEEDED TO CONSTRUCT BUFR MESSAGE
C                   = 20   INCORRECT ENTRY OF REPLICATION OR SEQUENCE
C                          DESCRIPTOR IN LIST OF REFERENCE VALUE CHANGES
C                   = 21   INCORRECT OPERATOR DESCRIPTOR IN LIST OF
C                          REFERENCE VALUE CHANGES
C                   = 22   ATTEMPTING TO ENTER NEW REFERENCE VALUE INTO
C                          TABLE B, BUT DESCRIPTOR DOES NOT EXIST IN
C                          CURRENT MODIFIED TABLE B
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916-128, Y-MP8/864, Y-MP EL92/256
C
C$$$
C
      REAL           RDATA(*)
C
      INTEGER        IDATA(*),LOWEST,MAXVAL,JSTART
      INTEGER        KARY(*),MISG,LL
      INTEGER        KDESC(3,*),KASSOC(*)
      INTEGER        IBITS(32)
      INTEGER        ZEROS(255)
      INTEGER        INDEXB(16383)
      CHARACTER*9    CCITT
      CHARACTER*4    AHOLD(2)
      CHARACTER*1    ATEXT(*)
      LOGICAL*1      TEXT
      LOGICAL*1      MSGFLG,DUPFLG
C  =====================================
C      INFORMATION REQUIRED FOR CONSTRUCTION OF BUFR MESSAGE
      INTEGER        ISECT1(*)
      INTEGER        ISEC2B,ISEC2D(255)
      INTEGER        ISECT3(*)
      INTEGER        JDESC(3,*)
      INTEGER        NEWNR
      INTEGER        KDATA(500,*)
      INTEGER        KBUFR(*)
C  =====================================
C                   TABLE B INFORMATION
      INTEGER        LDESC(800),KT(800)
      INTEGER        KSCALE(800)
      INTEGER        KRFVAL(800),KRFVSW(800),NEWRFV(800)
      INTEGER        KWIDTH(800)
      CHARACTER*40   ANAME(800)
      CHARACTER*25   AUNITS(800)
C  =====================================
C                   TABLE D INFORMATION
      INTEGER        KSEQ(300),KNUM(300)
      INTEGER        KLIST(300,10)
C  =====================================
      SAVE
C
      DATA  CCITT /'CCITT IA5'/
      DATA  IBITS /         1,          3,          7,         15,
     *                     31,         63,        127,        255,
     *                    511,       1023,       2047,       4095,
     *                   8191,      16383,      32767,      65535,
     *             Z'0001FFFF',Z'0003FFFF',Z'0007FFFF',Z'000FFFFF',
     *             Z'001FFFFF',Z'003FFFFF',Z'007FFFFF',Z'00FFFFFF',
     *             Z'01FFFFFF',Z'03FFFFFF',Z'07FFFFFF',Z'0FFFFFFF',
     *             Z'1FFFFFFF',Z'3FFFFFFF',Z'7FFFFFFF',Z'FFFFFFFF'/
      DATA  LL    /0/
      DATA  MISG  /99999/
      DATA  ZEROS /255*0/
C  =====================================
C                               THERE MUST BE DESCRIPTORS IN JDESC
C                               AND A COUNT IN NEWNR
C  =====================================
      IF (NEWNR.EQ.0) THEN
          IERRTN  = 15
          RETURN
      END IF
C  =====================================
C     IF INPUT FORM IS F X Y SEGMENTS THEN
C                      CONVERT INPUT FORM OF JDESC FROM FXY TO DECIMAL
C  =====================================
      IF (JIF.EQ.0) THEN
C                            CONVERT TO DECIMAL
          CALL FI8505(JIF,JDESC,NEWNR,IERRTN)
          IF (IERRTN.NE.0) THEN
              RETURN
          END IF
      END IF
C  =====================================
C     IF PROCESSING DELAYED REPLICATION, MUST RELOAD
C            KDESC FROM JDESC
C  =====================================
      IF (KARY(4).NE.0) THEN
          NRDESC  = 0
      END IF
C  =====================================
C     IF ONLY HAVE JDESC, NEWNR CREATE KDESC, NRDESC
C  =====================================
C                      IF ONLY HAVE JDESC, NEWNR CREATE KDESC, NRDESC
      IF (NRDESC.EQ.0) THEN
          DO 50 I = 1, NEWNR
              KDESC(1,I)  = JDESC(1,I)
   50     CONTINUE
          NRDESC  = NEWNR
          KIF     = 1
      ELSE IF (NRDESC.NE.0) THEN
C                      KDESC ALL READY EXISTS
          IF (KIF.EQ.0) THEN
C                      CONVERT INPUT FORM OF KDESC FROM FXY TO DECIMAL
              CALL FI8505(KIF,KDESC,NRDESC,IERRTN)
              IF (IERRTN.NE.0) THEN
                  RETURN
              END IF
          END IF
      END IF
C  =====================================
C     READ IN TABLE B SUBSET, IF NOT ALL READY IN PLACE
C  =====================================
      IF (ISECT3(8).EQ.0) THEN
          CALL FI8512(IUNITB,ISECT3,KDESC,NRDESC,KARY,IERRTN,
     *               LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KWIDTH,KRFVSW,
     *               IUNITD,KSEQ,KNUM,KLIST,INDEXB)
          IF (IERRTN.NE.0) GO TO 9000
      END IF
C  =====================================
C     ROUTE TO SELECTED PROCESSING
C  =====================================
      KSUB  = ISECT3(1)
      IF (ISTEP.EQ.1) THEN
C                          PROCESSING INTEGER DATA INPUT
          CALL FI8508(ISTEP,IUNITB,IDATA,KDESC,NRDESC,ATEXT,KSUB,KARY,
     *            KDATA,LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KRFVSW,ISECT3,
     *            KWIDTH,KASSOC,IUNITD,KSEQ,KNUM,KLIST,IERRTN,INDEXB)
          RETURN
      ELSE IF (ISTEP.EQ.2) THEN
C                          PROCESSING REAL DATA INPUT
          CALL FI8509(ISTEP,IUNITB,RDATA,KDESC,NRDESC,ATEXT,KSUB,KARY,
     *            KDATA,LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KRFVSW,ISECT3,
     *            KWIDTH,KASSOC,IUNITD,KSEQ,KNUM,KLIST,IERRTN,INDEXB)
          RETURN
      ELSE IF (ISTEP.NE.3) THEN
          IERRTN  = 20
          RETURN
      END IF
C  =====================================
C     IF INDICATING ZERO SUBSETS, HAVE AN ERROR CONDITION
C  =====================================
      IF (ISECT3(1).LE.0) THEN
          IERRTN  = 3
          RETURN
      END IF
C  =====================================
C     SET FOR BUFR MESSAGE
C  =====================================
C
C                                 CLEAR OUTPUT AREA
C                      BYTES IN EACH FULL WORD
      KWORD  = 4
C
C                               GET NUMBER OF SUBSETS
C
      MXRPTS    = ISECT3(1)
      ISECT3(7) = ISECT3(1)
      ISECT3(6) = ISECT3(1)
C
C                       RE-START POINT FOR PACKING FEWER SUBSETS ?
C
    5 CONTINUE
C
      KARY(18) = 0
      KARY(26) = 0
C  =====================================
C     ENTER 'BUFR'          -  SECTION 0
C                      CONSTRUCT UNDER RULES OF EDITION 2
C  =====================================
      KARY(3)        = 0
      NBUFR          = 1112884818
      CALL SBYTE (KBUFR,NBUFR,KARY(3),32)
      KARY(3)        = KARY(3) + 32
C                             SAVE POINTER FOR TOTAL BYTE COUNT
C                                   IN MESSAGE
      KARY(19)       = KARY(3)
      KARY(3)        = KARY(3) + 24
C                    SET EDITION NR IN PLACE
      CALL SBYTE (KBUFR,2,KARY(3),8)
      KARY(3)        = KARY(3) + 8
      KARY(20)       = 8
C     PRINT *,'SECTION 0'
C  =====================================
C     COMPLETE ENTRIES FOR  -  SECTION 1
C  =====================================
C  ----- 1,3              SECTION COUNT
      KARY(21)       = 18
      CALL SBYTE (KBUFR,KARY(21),KARY(3),24)
      KARY(3)        = KARY(3) + 24
C  ----- 4                  RESERVED
      CALL SBYTE (KBUFR,0,KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 5               ORIGINATING SUB-CENTER
      CALL SBYTE (KBUFR,ISECT1(3),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 6               ORIGINATING CENTER
      CALL SBYTE (KBUFR,ISECT1(4),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 7               UPDATE SEQUENCE NUMBER
      CALL SBYTE (KBUFR,ISECT1(5),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 8
C                        INDICATE NO SECTION 2
      CALL SBYTE (KBUFR,ISECT1(6),KARY(3),1)
      KARY(3)        = KARY(3) + 1
      CALL SBYTE (KBUFR,0,KARY(3),7)
      KARY(3)        = KARY(3) + 7
C  ----- 9            BUFR MESSAGE TYPE
      CALL SBYTE (KBUFR,ISECT1(7),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 10            BUFR MESSAGE SUB-TYPE
      CALL SBYTE (KBUFR,ISECT1(8),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 11            VERSION OF MASTER TABLE
      CALL SBYTE (KBUFR,ISECT1(9),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 12            VERSION OF LOCAL TABLE
      CALL SBYTE (KBUFR,ISECT1(10),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 13            YEAR
      CALL SBYTE (KBUFR,ISECT1(11),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 14            MONTH
      CALL SBYTE (KBUFR,ISECT1(12),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ---- 15             DAY
      CALL SBYTE (KBUFR,ISECT1(13),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 16            HOUR
      CALL SBYTE (KBUFR,ISECT1(14),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 17            MINUTE
      CALL SBYTE (KBUFR,ISECT1(15),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 18            FILL
      CALL SBYTE (KBUFR,0,KARY(3),8)
      KARY(3)        = KARY(3) + 8
C     PRINT *,'SECTION 1'
C  =====================================
C     SKIP                  -  SECTION 2
C  =====================================
      IF (ISECT1(6).NE.0) THEN
C                  BUILD SECTION COUNT
          KARY(22) = 4 + ISEC2B
          IF (MOD(KARY(22),2).NE.0) KARY(22) = KARY(22) + 1
C                        INSERT SECTION COUNT
          CALL SBYTE (KBUFR,KARY(22),KARY(3),24)
          KARY(3)  = KARY(3) + 24
C                        INSERT RESERVED POSITION
          CALL SBYTE (KBUFR,0,KARY(3),8)
          KARY(3)  = KARY(3) + 8
C                        INSERT SECTION 2 DATA
          CALL SBYTES(KBUFR,ISEC2D,KARY(3),8,0,ISEC2B)
          KARY(3)  = KARY(3) + (ISEC2B * 8)
          IF (MOD(ISEC2B,2).NE.0) THEN
              CALL SBYTE (KBUFR,0,KARY(3),8)
              KARY(3)  = KARY(3) + 8
          END IF
      ELSE
          KARY(22)       = 0
      END IF
C  =====================================
C     MAKE PREPARATIONS FOR SECTION 3 DESCRIPTORS
C  =====================================
      KARY(23)        = 7 + NEWNR*2 + 1
C                             SECTION 3 SIZE
      CALL SBYTE (KBUFR,KARY(23),KARY(3),24)
      KARY(3)         = KARY(3) + 24
C                             RESERVED BYTE
      CALL SBYTE (KBUFR,0,KARY(3),8)
      KARY(3)         = KARY(3) + 8
C                           NUMBER OF SUBSETS
      CALL SBYTE (KBUFR,ISECT3(1),KARY(3),16)
      KARY(3)         = KARY(3) + 16
C                          SET OBSERVED DATA SWITCH
      CALL SBYTE (KBUFR,ISECT3(2),KARY(3),1)
      KARY(3)         = KARY(3) + 1
C                          SET COMPRESSED DATA SWITCH
      CALL SBYTE (KBUFR,ISECT3(3),KARY(3),1)
      KARY(3)         = KARY(3) + 1
      CALL SBYTE (KBUFR,0,KARY(3),6)
      KARY(3)         = KARY(3) + 6
C  =====================================
C     DESCRIPTORS         -  SECTION 3
C  =====================================
      DO 37 KH = 1, NEWNR
C         PRINT *,'INSERTING',JDESC(1,KH),' INTO SECTION 3'
          CALL SBYTE (KBUFR,JDESC(1,KH),KARY(3),16)
          KARY(3)         = KARY(3) + 16
   37 CONTINUE
C                          FILL TO TWO BYTE BOUNDARY
      CALL SBYTE (KBUFR,0,KARY(3),8)
      KARY(3)        = KARY(3) + 8
C     PRINT *,'SECTION 3'
C  =====================================
C     INITIALIZE FOR        -  SECTION 4
C  =====================================
C                              SAVE POINTER TO COUNT POSITION
C     PRINT *,'START OF SECTION 4',KARY(3)
      KARY(5)        = KARY(3)
      KARY(3)        = KARY(3) + 24
      CALL SBYTE (KBUFR,0,KARY(3),8)
      KARY(3)        = KARY(3) + 8
C                              SKIP TO FIRST DATA POSITION
C  =====================================
C     BIT PATTERNS          -  SECTION 4
C  =====================================
      KEND4  = IBFSIZ * 8 - 32
C                          PACK ALL DATA INTO BUFR MESSAGE
C
      IF (ISECT3(3).EQ.0) THEN
C                        **********************************************
C                        *                                            *
C                        *      PROCESS AS NON-COMPRESSED MESSAGE     *
C                        *                                            *
C                        **********************************************
          CALL FI8506(ISTEP,ISECT3,KARY,JDESC,NEWNR,KDESC,NRDESC,
     *           LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KWIDTH,KRFVSW,NEWRFV,
     *           KSEQ,KNUM,KLIST,IBFSIZ,
     *           KDATA,KBUFR,IERRTN,INDEXB)
          IF (IERRTN.NE.0) THEN
              IF (IERRTN.EQ.1) GO TO 5500
              RETURN
          END IF
      ELSE
C                        **********************************************
C                        *                                            *
C                        *      PROCESS AS COMPRESSED MESSAGE         *
C                        *                                            *
C                        **********************************************
          KARY(18)  = 0
C                          MUST LOOK AT EVERY DESCRIPTOR IN KDESC
          KARY(11)  = 1
 3000     CONTINUE
          IF (KARY(11).GT.NRDESC) THEN
              GO TO 5200
          ELSE
C         DO 5000 JK = 1, NRDESC
C                              RE-ENTRY POINT FOR INSERTION OF
C                              REPLICATION OR SEQUENCES
 4000         CONTINUE
C                             ISOLATE TABLE
              KFUNC      = KDESC(1,KARY(11)) / 16384
C                             ISOLATE CLASS
              KCLASS     = MOD(KDESC(1,KARY(11)),16384) / 256
              KSEG       = MOD(KDESC(1,KARY(11)),256)
              KARY(2) = KARY(11) + KARY(18)
              IF (KFUNC.EQ.1) THEN
C                            DELAYED REPLICATION NOT ALLOWED
C                            IN COMPRESSED MESSAGE
                  IF (KSEG.EQ.0) THEN
                      IERRTN  = 8
                      RETURN
                  END IF
C                            REPLICATION DESCRIPTOR
                  CALL FI8501(KARY,ISTEP,KCLASS,KSEG,IDATA,RDATA,
     *                  KDATA,LL,KDESC,NRDESC,IERRTN)
C                 GO TO 4000
              ELSE IF (KFUNC.EQ.2) THEN
                  CALL FI8502(*4000,KBUFR,KCLASS,KSEG,
     *                     KDESC,NRDESC,I,ISTEP,
     *            KARY,KDATA,ISECT3,KRFVSW,NEWRFV,LDESC,IERRTN,INDEXB)
                  IF (IERRTN.NE.0) THEN
                      RETURN
                  END IF
                  GO TO 5000
              ELSE IF (KFUNC.EQ.3) THEN
                  CALL FI8503(KARY(11),KDESC,NRDESC,
     *                     ISECT3,IUNITD,KSEQ,KNUM,KLIST,IERRTN)
                  IF (IERRTN.NE.0) THEN
                      RETURN
                  END IF
                  GO TO 4000
              END IF
C                      FALL THRU WITH ELEMENT DESCRIPTOR
C                      POINT TO CORRECT TABLE B ENTRY
              L  = INDEXB(KDESC(1,KARY(11)))
              IF (L.LT.0) THEN
                  IERRTN  = 2
C                 PRINT *,'W3FI85 - IERRTN = 2'
                  RETURN
              END IF
C
              IF (AUNITS(L)(1:9).EQ.CCITT) THEN
                  TEXT  = .TRUE.
              ELSE
                  TEXT  = .FALSE.
              END IF
              KARY(7) = KWIDTH(L)
C
              IF (TEXT) THEN
C                                     PROCESS TEXT DATA
                  KBZ = KARY(3) + (ISECT3(1) + 1) * KARY(7) + 6
                  IF (KBZ.GT.KEND4) THEN
                      GO TO 5500
                  END IF
C                             NBINC IS NUMBER OF CHARS
                  NBINC     = KARY(7) / 8
C                              LOWEST = 0
                  CALL SBYTES(KBUFR,ZEROS,KARY(3),8,0,NBINC)
                  KARY(3)   = KARY(3) + KARY(7)
                  CALL SBYTE (KBUFR,NBINC,KARY(3),6)
                  KARY(3)   = KARY(3) + 6
C                               HOW MANY FULL WORDS
                  NKPASS    = KARY(7) / 32
C                           HOW MANY BYTES IN PARTIAL WORD
                  KREM      = MOD(KARY(7),32)
C                 KSKIP     = KARY(7) - 32
                  DO 4080 NSS = 1, ISECT3(1)
C                            POINT TO TEXT FOR THIS SUBSET
                      KARY(2)   = KARY(11) + KARY(18)
                      IF (NKPASS.GE.1) THEN
C                          PROCESS TEXT IN A SUBSET
                          DO 4070 NPP = 1, NKPASS
C                                 PROCESS FULL WORDS
                              IF (ISECT3(10).EQ.1) THEN
                                  CALL W3AI38 (KDATA(NSS,KARY(2)),4)
                              END IF
                              CALL SBYTE (KBUFR,KDATA(NSS,KARY(2)),
     *                               KARY(3),32)
                              KARY(3)  = KARY(3) + 32
C                                 POINT TO NEXT DATA WORD FOR MORE TEXT
                              KARY(2) = KARY(2) + 1
 4070                     CONTINUE
                      END IF
C                          PROCESS PARTIALS - LESS THAN 4 BYTES
                      IF (KREM.GT.0) THEN
                          IF (ISECT3(10).EQ.1) THEN
                              CALL W3AI38 (KDATA(NSS,KARY(2)),4)
                          END IF
                          CALL SBYTE (KBUFR,KDATA(NSS,KARY(2)),
     *                               KARY(3),KREM)
                          KARY(3)  = KARY(3) + KREM
                      END IF
 4080             CONTINUE
C                              ADJUST EXTRA WORD COUNT
                  IF (KREM.GT.0) THEN
                      KARY(18)  = KARY(18) + NKPASS
                  ELSE
                      KARY(18)  = KARY(18) + NKPASS - 1
                  END IF
C  -------------------------------------------------------------
                  GO TO 5000
              ELSE
                  KARY(2) = KARY(11) + KARY(18)
                  KARY(7) = KWIDTH(L) + KARY(26)
C
C                               NON TEXT/NUMERIC DATA
C
C                             PROCESS ASSOCIATED FIELD DATA
                  IF (KARY(27).GT.0.AND.KDESC(1,KARY(11)).NE.7957) THEN
                      DUPFLG  = .TRUE.
                      DO 4130 J = 2, ISECT3(1)
                          IF (KDATA(J,KARY(2)).NE.KDATA(1,KARY(2)))THEN
                              DUPFLG = .FALSE.
                              GO TO 4131
                          END IF
 4130                 CONTINUE
 4131                 CONTINUE
                      IF (DUPFLG) THEN
C                                  ALL VALUES ARE EQUAL
                          KBZ = KARY(3) + KARY(7) + 6
                          IF (KBZ.GT.KEND4) THEN
                              GO TO 5500
                          END IF
                          NBINC  = 0
C                                 ENTER COMMON VALUE
                          IF (KDATA(1,KARY(2)).EQ.MISG) THEN
                              CALL SBYTE(KBUFR,IBITS(KARY(7)),
     *                               KARY(3),KARY(27))
                          ELSE
                              CALL SBYTE(KBUFR,KDATA(1,KARY(2)),
     *                               KARY(3),KARY(27))
                          END IF
                          KARY(3)  = KARY(3) + KARY(27)
C                                       ENTER NBINC
                          CALL SBYTE (KBUFR,NBINC,KARY(3),6)
                          KARY(3)  = KARY(3) + 6
                      ELSE
C                               MIX OF MISSING AND VALUES
C                               GET LARGEST DIFFERENCE VALUE
                          MSGFLG = .FALSE.
                          DO 4132 J = 1, ISECT3(7)
                              IF (KDATA(J,KARY(2)).EQ.MISG) THEN
                                  MSGFLG = .TRUE.
                                  GO TO 4133
                              END IF
 4132                     CONTINUE
 4133                     CONTINUE
                          DO 4134 J = 1, ISECT3(7)
                              IF (KDATA(J,KARY(2)).LT.IBITS(KARY(27))
     *                                .AND.KDATA(J,KARY(2)).GE.0.AND.
     *                                KDATA(J,KARY(2)).NE.MISG) THEN
                                  LOWEST = KDATA(J,KARY(2))
                                  MAXVAL = KDATA(J,KARY(2))
                                  JSTART = J + 1
                                  GO TO 4135
                              END IF
 4134                     CONTINUE
 4135                     CONTINUE
                          DO 4136 J = JSTART, ISECT3(7)
                              IF (KDATA(J,KARY(2)).NE.MISG) THEN
                                 IF (KDATA(J,KARY(2)).LT.LOWEST) THEN
                                          LOWEST = KDATA(J,KARY(2))
                                 ELSE IF(KDATA(J,KARY(2)).GT.MAXVAL)THEN
                                          MAXVAL = KDATA(J,KARY(2))
                                 END IF
                              END IF
 4136                     CONTINUE
                          MXDIFF  = MAXVAL - LOWEST
C                              FIND NBINC
                          MXBITS  = KARY(27)
                          DO 4142 LJ = 1, MXBITS
                              NBINC = LJ
                              IF (MXDIFF.LT.IBITS(LJ)) THEN
                                  GO TO 4143
                              END IF
 4142                     CONTINUE
 4143                     CONTINUE
                          KBZ = KARY(3) + MXBITS + 6 + ISECT3(1) * NBINC
                          IF (KBZ.GT.KEND4) THEN
                              GO TO 5500
                          END IF
                          IF (NBINC.GT.MXBITS) THEN
                              IERRTN  = 3
                              RETURN
                          END IF
C                               ENTER LOWEST
                          CALL SBYTE(KBUFR,LOWEST,KARY(3),MXBITS)
                          KARY(3)  = KARY(3) + MXBITS
                          CALL SBYTE(KBUFR,NBINC,KARY(3),6)
                          KARY(3)  = KARY(3) + 6
C                               GET DIFFERENCE VALUES
                          IF (MSGFLG) THEN
                              DO 4144 M = 1, ISECT3(1)
                                  IF (KDATA(M,KARY(2)).EQ.MISG) THEN
                                      KT(M)  = IBITS(NBINC)
                                  ELSE
                                      KT(M)  = KDATA(M,KARY(2)) - LOWEST
                                  END IF
 4144                         CONTINUE
                          ELSE
                              DO 4146 M = 1, ISECT3(1)
                                  KT(M)  = KDATA(M,KARY(2)) - LOWEST
 4146                         CONTINUE
                          END IF
C                                ENTER DATA VALUES
                          CALL SBYTES(KBUFR,KT,KARY(3),NBINC,
     *                                          0,ISECT3(1))
                          KARY(3)  = KARY(3) + ISECT3(1) * NBINC
                      END IF
                      KARY(18)  = KARY(18) + 1
                  END IF
C  ---------------------------------------------------
C                            STANDARD DATA
C  ---------------------------------------------------
                  KARY(2) = KARY(11) + KARY(18)
                  MXBITS = KARY(7) + KARY(26)
                  DUPFLG = .TRUE.
                  DO 4030 J = 2, ISECT3(7)
                      IF (KDATA(J,KARY(2)).NE.KDATA(1,KARY(2))) THEN
                          DUPFLG = .FALSE.
                          GO TO 4031
                      END IF
 4030             CONTINUE
 4031             CONTINUE
                  IF (DUPFLG) THEN
C                                  ALL VALUES ARE EQUAL
                      KBZ = KARY(3) + KARY(7) + 6
                      IF (KBZ.GT.KEND4) THEN
                          GO TO 5500
                      END IF
                      NBINC  = 0
C                                 ENTER COMMON VALUE
                      IF (KDATA(1,KARY(2)).EQ.MISG) THEN
                          CALL SBYTE(KBUFR,IBITS(MXBITS),
     *                               KARY(3),MXBITS)
                      ELSE
                          CALL SBYTE(KBUFR,KDATA(1,KARY(2)),
     *                               KARY(3),MXBITS)
                      END IF
                      KARY(3)  = KARY(3) + KARY(7)
C                                       ENTER NBINC
                      CALL SBYTE (KBUFR,NBINC,KARY(3),6)
                      KARY(3)  = KARY(3) + 6
                  ELSE
C                               MIX OF MISSING AND VALUES
C                               GET LARGEST DIFFERENCE VALUE
                      MSGFLG = .FALSE.
                      DO 4032 J = 1, ISECT3(7)
                          IF (KDATA(J,KARY(2)).EQ.MISG) THEN
                              MSGFLG = .TRUE.
                              GO TO 4033
                          END IF
 4032                 CONTINUE
 4033                 CONTINUE
                      DO 4034 J = 1, ISECT3(7)
                          IF (KDATA(J,KARY(2)).NE.MISG) THEN
                              LOWEST = KDATA(J,KARY(2))
                              MAXVAL = KDATA(J,KARY(2))
C                             PRINT *,' '
C                             PRINT *,'START VALUES',LOWEST,MAXVAL,
C    *                            'J=',J,' KARY(2)=',KARY(2)
                              GO TO 4035
                          END IF
 4034                 CONTINUE
 4035                 CONTINUE
                      DO 4036 J = 1, ISECT3(1)
                          IF (KDATA(J,KARY(2)).NE.MISG) THEN
                             IF (KDATA(J,KARY(2)).LT.LOWEST) THEN
                                          LOWEST = KDATA(J,KARY(2))
C                                PRINT *,'NEW LOWEST=',LOWEST,J
                             ELSE IF (KDATA(J,KARY(2)).GT.MAXVAL) THEN
                                      MAXVAL = KDATA(J,KARY(2))
C                                PRINT *,'NEW MAXVAL=',MAXVAL,J
                             END IF
                          END IF
 4036                 CONTINUE
                      MXDIFF  = MAXVAL - LOWEST
C                              FIND NBINC
                      DO 4042 LJ = 1, MXBITS
                          NBINC = LJ
                          IF (MXDIFF.LT.IBITS(LJ)) GO TO 4043
                          IF (NBINC.EQ.MXBITS) GO TO 4043
 4042                 CONTINUE
 4043                 CONTINUE
                      KBZ = KARY(3) + MXBITS  + 38 + ISECT3(1) * NBINC
                      IF (KBZ.GT.KEND4) THEN
                          GO TO 5500
                      END IF
C                     PRINT 4444,KARY(11),KDESC(1,KARY(11)),LOWEST,
C    *                 MAXVAL,MXDIFF,KARY(7),NBINC,ISECT3(1),ISECT3(7)
C4444                 FORMAT(9(1X,I8))
C                               ENTER LOWEST
C                                 ADJUST WITH REFERENCE VALUE
                      IF (KRFVSW(L).EQ.0) THEN
                          JRV  = KRFVAL(L)
                      ELSE
                          JRV  = NEWRFV(L)
                      END IF
                      LVAL  = LOWEST - JRV
                      CALL SBYTE(KBUFR,LVAL,KARY(3),MXBITS)
                          KARY(3)  = KARY(3) + MXBITS
                      IF (NBINC.GT.MXBITS) THEN
                          IERRTN  = 3
                          RETURN
                      END IF
                      CALL SBYTE(KBUFR,NBINC,KARY(3),6)
                      KARY(3)  = KARY(3) + 6
C                                GET DIFFERENCE VALUES
                      IF (MSGFLG) THEN
                          DO 4044 M = 1, ISECT3(1)
                              IF (KDATA(M,KARY(2)).EQ.MISG) THEN
                                  KT(M)  = IBITS(NBINC)
                              ELSE
                                  KT(M)  = KDATA(M,KARY(2)) - LOWEST
                              END IF
 4044                     CONTINUE
                      ELSE
                          DO 4046 M = 1, ISECT3(1)
                              KT(M)  = KDATA(M,KARY(2)) - LOWEST
 4046                     CONTINUE
                      END IF
C                                ENTER DATA VALUES
                      CALL SBYTES(KBUFR,KT,KARY(3),NBINC,
     *                                             0,ISECT3(1))
                      KARY(3)  = KARY(3) + ISECT3(1) * NBINC
                  END IF
                  GO TO 5000
              END IF
C  -------------------------------------------------------------
 5000         CONTINUE
              KARY(11) = KARY(11) + 1
              GO TO 3000
          ENDIF
 5200     CONTINUE
      END IF
      ISECT3(6) = 0
      GO TO 6000
 5500 CONTINUE
C                       THE SEGMENT OF CODE BETWEEN STATEMENTS
C                       5500-6000 ARE ACTIVATED IF AND WHEN THE
C                       MAXIMUM MESSAGE SIZE HAS BEEN EXCEEDED
C
C           ARE WE REDUCING IF OVERSIZED  ???
      IF (ISECT3(4).NE.0) THEN
C                                   INCREMENT REDUCTION COUNT
          ISECT3(6)  = ISECT3(6) + ISECT3(5)
C                                   REDUCE NUMBER TO INCLUDE
          ISECT3(7)  = ISECT3(1) - ISECT3(5)
          ISECT3(1)  = ISECT3(7)
          PRINT *,'REDUCED BY ',ISECT3(5),' ON THIS PASS'
          GO TO 5
      ELSE
          IERRTN = 1
          RETURN
      END IF
 6000 CONTINUE
C  ---------------------------------------------------------------
C                                 FILL IN SECTION 4 OCTET COUNT
      NBUFR  = MOD((KARY(3) - KARY(5)),16)
C                                 MAY BE NECESSARY TO ADJUST COUNT
      IF (NBUFR.NE.0) THEN
          KARY(3)   = KARY(3) + 16 - NBUFR
      END IF
      KARY(24)  = (KARY(3) - KARY(5)) / 8
      CALL SBYTE (KBUFR,KARY(24),KARY(5),24)
C     PRINT *,'SECTION 4'
C  =====================================
C     ENDING KEY  '7777'    -  SECTION 5
C  =====================================
      KARY(25)    = 4
      NBUFR       = 926365495
      CALL SBYTE (KBUFR,NBUFR,KARY(3),32)
      KARY(3)     = KARY(3) + 32
C                  CONSTRUCT TOTAL BYTE COUNT FOR SECTION 0
      ITOTAL      = KARY(3) / 8
      CALL SBYTE (KBUFR,ITOTAL,32,24)
      KARY(30)    = ITOTAL
C     WRITE (6,8601) ITOTAL
 8601 FORMAT (1X,22HTHIS MESSAGE CONTAINS ,I10,6H BYTES)
C  =======================================
C                 KBUFR CONTAINS A COMPLETED MESSAGE
      IF (ISECT3(4).NE.0.AND.ISECT3(5).NE.0) THEN
C                 ADJUST KDATA ARRAY
          NR   = MXRPTS - ISECT3(1)
          ISECT3(7) = ISECT3(7) + 1
          DO 7500 I = 1, NR
              DO 7000 J = 1, NRDESC
                  KDATA(I,J)  = KDATA(ISECT3(7),J)
 7000         CONTINUE
              ISECT3(7) = ISECT3(7) + 1
 7500     CONTINUE
          KARY(14)  = NR
      ELSE
          ISECT3(7) = ISECT3(1)
      END IF
C  =======================================
      IERRTN = 0
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE FI8501(KARY,ISTEP,KCLASS,KSEG,IDATA,RDATA,
     *                  KDATA,NSUB,KDESC,NRDESC,IERRTN)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI8501      PERFORM REPLICATION OF DESCRIPTORS
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 93-12-03
C
C ABSTRACT: HAVE ENCOUNTERED A REPLICATION DESCRIPTOR . IT MAY INCLUDE
C           DELAYED REPLICATION OR NOT.  THAT DECISION SHOULD HAVE BEEN
C           MADE PRIOR TO CALLING THIS ROUTINE.
C
C PROGRAM HISTORY LOG:
C   93-12-03  CAVANAUGH
C   94-03-25  HOPPA       ADDED LINE TO INITIALIZE NXTPTR TO CORRECT
C                         AN ERROR IN THE STANDARD REPLICATION.
C   94-03-28  HOPPA       CORRECTED AN ERROR IN THE STANDARD REPLICATION
C                         THAT WAS ADDING EXTRA ZEROS TO THE BUFR
C                         MESSAGE AFTER THE REPLICATED DATA.
C   94-03-31  HOPPA       ADDED THE SUBSET NUMBER TO THE PARAMETER LIST.
C                         CORRECTED THE EQUATION FOR THE NUMBER OF
C                         REPLICATIONS WITH DELAYED REPLICATION.
C                         (ISTART AND K DON'T EXIST)
C   94-04-19  HOPPA       SWITCHED THE VARIABLES NEXT AND NXTPRT
C   94-04-20  HOPPA       ADDED THE KDATA PARAMETER COUNTER TO THE
C                         PARAMETER LIST.  IN THE ASSIGNMENT OF NREPS
C                         WHEN HAVE DELAYED REPLICATION, CHANGED INDEX
C                         IN KDATA FROM N TO K.
C   94-04-29  HOPPA     - REMOVED N AND K FROM THE INPUT LIST
C                       - CHANGED N TO KARY(11) AND K TO KARY(2)
C
C USAGE:    CALL FI8501(KARY,ISTEP,KCLASS,KSEG,IDATA,RDATA,
C    *                  KDATA,N,NSUB,KDESC,NRDESC,IERRTN)
C   INPUT ARGUMENT LIST:
C     ISTEP    -
C     KCLASS   -
C     KKSEG    -
C     IDATA    -
C     RDATA    -
C     KDATA    -
C     N        - CURRENT POSITION IN DESCRIPTOR LIST
C     NSUB     - CURRENT SUBSET
C     KDESC    - LIST OF DESCRIPTORS
C     NRDESC   - NUMBER OF DESCRIPTORS IN KDESC
C
C   OUTPUT ARGUMENT LIST:
C     N        - CURRENT POSITION IN DESCRIPTOR LIST
C     KDESC    - MODIFIED LIST OF DESCRIPTORS
C     NRDESC   - NEW NUMBER OF DESCRIPTORS IN KDESC
C     IERRTN   - ERROR RETURN VALUE
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916-128, Y-MP8/864, Y-MP EL92/256
C
C$$$
C
      REAL         RDATA(*)
C
      INTEGER      IDATA(*),NREPS,KARY(*)
      INTEGER      KCLASS,KSEG
      INTEGER      KDESC(3,*),NRDESC,KDATA(500,*)
      INTEGER      IERRTN
      INTEGER      ITAIL(1600)
      INTEGER      IHOLD(1600),ISTEP
C
      SAVE
C
C                    TEST KFUNC FOR DESCRIPTOR TYPE
C                    DO REPLICATION
C  ****************************************************************
      IERRTN  = 0
C                            REPLICATION DESCRIPTOR
C                                 STANDARD REPLICATION WILL SIMPLY
C                                 BE PROCESSED FROM ITS DESCRIPTOR
C                                 PARTS
C
C                            DELAYED REPLICATION DESCRIPTOR
C                                 MUST BE FOLLOWED BY ONE OF THE
C                                 DESCRIPTORS FOR A DELAYED
C                                 REPLICATION FACTOR
C                                       0 31 001  (7937 DECIMAL)
C                                       0 31 002  (7938 DECIMAL)
C                                       0 31 011  (7947 DECIMAL)
C                                       0 31 012  (7948 DECIMAL)
      IF (KSEG.NE.0) THEN
C                       HAVE NUMBER OF REPLICATIONS AS KSEG
          NREPS  = KSEG
          IPUT   = KARY(11)
          NEXT   = IPUT + 1
          NXTPTR = IPUT + 1 + KCLASS
      ELSE IF (KSEG.EQ.0) THEN
          IF (KDESC(1,KARY(11)+1).EQ.7937.OR.
     *                    KDESC(1,KARY(11)+1).EQ.7938.OR.
     *                    KDESC(1,KARY(11)+1).EQ.7947.OR.
     *                    KDESC(1,KARY(11)+1).EQ.7948) THEN
C             PRINT *,'HAVE DELAYED REPLICATION'
              KARY(4)  = 1
C                                  MOVE REPLICATION DEFINITION
              KDESC(1,KARY(11))  = KDESC(1,KARY(11)+1)
C                                  MUST DETERMINE HOW MANY REPLICATIONS
              IF (ISTEP.EQ.1) THEN
                  NREPS = IDATA(KARY(11))
              ELSE IF (ISTEP.EQ.2) THEN
                  NREPS = RDATA(KARY(11))
              ELSE
                  NREPS = KDATA(NSUB,KARY(2))
              END IF
              IPUT      = KARY(11) + 1
              NXTPTR    = IPUT + KCLASS + 1
              NEXT      = IPUT + 1
C                              POINT TO REPLICATION DESCRIPTOR
          END IF
      ELSE
          IERRTN  = 10
          RETURN
      END IF
C                                 EXTRACT DESCRIPTORS TO BE REPLICATED
C                     IF NREPS = 0, THIS LIST OF DESCRIPTORS IS NOT TO
C                        BE USED IN DEFINING THE DATA,
C                     OTHERWISE
C                        IT WILL BE USED TO DEFINE THE DATA
      IF (NREPS.NE.0) THEN
          DO 1000 IJ = 1, KCLASS
              IHOLD(IJ)  = KDESC(1,NEXT)
              NEXT  = NEXT + 1
 1000     CONTINUE
C                      SKIP THE NUMBER OF DESCRIPTORS DEFINED BY KCLASS
      END IF
C                                   SAVE OFF TAIL OF DESC STREAM
C               START AT FIRST POSITION OF TAIL
      IGOT    = 0
      DO 1100 IJ = NXTPTR, NRDESC
          IGOT         = IGOT + 1
          ITAIL(IGOT)  = KDESC(1,IJ)
 1100 CONTINUE
C                                   INSERT ALL REPLICATED DESC'S
      IF (NREPS.NE.0) THEN
          DO 1300 KR = 1, NREPS
              DO 1200 KD = 1, KCLASS
                  KDESC(1,IPUT) = IHOLD(KD)
                  IPUT          = IPUT + 1
 1200         CONTINUE
 1300     CONTINUE
      END IF
C                                   RESTORE TAIL
      DO 1400 ITL = 1, IGOT
          KDESC(1,IPUT) = ITAIL(ITL)
          IPUT          = IPUT + 1
 1400 CONTINUE
C
C                               RESET NUMBER OF DESCRIPTORS IN KDESC
      NRDESC  = IPUT - 1
C  ****************************************************************
      RETURN
      END
      SUBROUTINE FI8502(*,KBUFR,KCLASS,KSEG,KDESC,NRDESC,I,ISTEP,
     *          KARY,KDATA,ISECT3,KRFVSW,NEWRFV,LDESC,IERRTN,INDEXB)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI8502      PROCESS AN OPERATOR DESCRIPTOR
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 93-12-03
C
C ABSTRACT: HAVE ENCOUNTERED AN OPERATOR DESCRIPTOR
C
C
C PROGRAM HISTORY LOG:
C   93-12-03  CAVANAUGH
C   94-04-15  J. HOPPA  - ADDED KBUFR TO INPUT PARAMETER LIST.
C                       - ADDED BLOCK OF DATA TO CORRECTLY USE SBYTE
C                         WHEN WRITING A 205YYY DESCRIPTOR TO THE
C                         BUFR MESSAGE.
C                         THE PREVIOUS WAY DIDN'T WORK BECAUSE KDATA
C                         WAS GETTING INCREMETED BY THE KSUB VALUE,
C                         NOT THE PARAM VALUE.
C   94-04-29  J. HOPPA  - CHANGED K TO KARY(2)
C                       - REMOVED A LINE THAT BECAME OBSOLETE WITH
C                         ABOVE CHANGE
C   94-05-18  J. HOPPA  - ADDED A KARY(2) INCREMENT
C
C USAGE:    CALL FI8502(*,KCLASS,KSEG,KDESC,NRDESC,I,ISTEP,
C    *          KARY,KDATA,ISECT3,KRFVSW,NEWRFV,LDESC,IERRTN,INDEXB)
C   INPUT ARGUMENT LIST:
C     KCLASS   -
C     KSEG     -
C     KDESC    -
C     NRDESC   -
C     I        -
C     ISTEP    -
C     KARY     -
C
C   OUTPUT ARGUMENT LIST:
C     KDESC    -
C     NRDESC   -
C     KARY     -
C     IERRTN   - ERROR RETURN VALUE
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916-128, Y-MP8/864, Y-MP EL92/256
C
C$$$
C
      INTEGER      KCLASS,KSEG,ZEROES(255)
      INTEGER      KRFVSW(*),NEWRFV(*),LDESC(*)
      INTEGER      I,KDESC(3,*),KDATA(500,*),ISECT3(*)
      INTEGER      NRDESC
      INTEGER      KARY(*)
      INTEGER      IERRTN
      INTEGER      NLEFT
C
      SAVE
C
      DATA  ZEROES/255*0/
C
C  ****************************************************************
      IERRTN  = 0
C                            OPERATOR DESCRIPTOR
      IF (KCLASS.EQ.1) THEN
C                  BITS ADDED TO DESCRIPTOR WIDTH
          IF (ISTEP.EQ.3) THEN
              IF (KSEG.NE.0) THEN
                  KARY(26)  = KSEG - 128
              ELSE
                  KARY(26)  = 0
              END IF
          END IF
      ELSE IF (KCLASS.EQ.2) THEN
C                  NEW SCALE VALUE
          IF (ISTEP.EQ.3) THEN
              IF (KSEG.EQ.0) THEN
                  KARY(9)  = 0
              ELSE
                  KARY(9)  = KSEG - 128
              END IF
          END IF
      ELSE IF (KCLASS.EQ.3) THEN
C                  CHANGE REFERENCE VALUE
C                                  MUST ACCEPT INTO OUTPUT THE
C                                  REFERENCE VALUE CHANGE AND ACTIVATE
C                                  THE CHANGE WHILE PROCESSING
          IF (ISTEP.EQ.3) THEN
C                       HAVE OPERATOR DESCRIPTOR FOR REFERENCE VALUES
              IF (KSEG.EQ.0) THEN
                  DO 100 IQ = 1, ISECT3(8)
C                                RESET ALL NEW REFERENCE VALUES
                      KRFVSW(IQ)  = 0
  100             CONTINUE
              END IF
  200         CONTINUE
C                                GET NEXT DESCRIPTOR
              KARY(11)  = KARY(11) + 1
              IF (KDESC(1,KARY(11)).GT.16383) THEN
C                                NOT AN ELEMENT DESCRIPTOR
                  NFUNC  = KDESC(1,KARY(11)) / 16384
                  IF (NFUNC.EQ.1.OR.NFUNC.EQ.3) THEN
                      IERRTN  = 20
                      PRINT *,'INCORRECT ENTRY OF REPLICATION OR ',
     *                     'SEQUENCE DESCRIPTOR IN LIST OF ',
     *                     'REFERENCE VALUE CHANGES'
                      RETURN
                  END IF
                  NCLASS = (KDESC(1,KARY(11)) - NFUNC*16384) / 256
                  IF (NCLASS.EQ.3) THEN
                      NSEG  = MOD(KDESC(1,KARY(11)),256)
                      IF (NSEG.EQ.255) THEN
                          RETURN
                      END IF
                  END IF
                  IERRTN  = 21
                  PRINT *,'INCORRECT OPERATOR DESCRIPTOR ENTRY ',
     *                    'IN LIST OF REFERENCE VALUE CHANGES'
                  RETURN
              END IF
C                               ELEMENT DESCRIPTOR W/NEW REFERENCE VALUE
C                               FIND MATCH FOR CURRENT DESCRIPTOR
              IQ  = INDEXB(KDESC(1,KARY(11)))
              IF (IQ.LT.1) THEN
                  IERRTN  = 22
                  PRINT *,'ATTEMPTING TO ENTER NEW REFERENCE VALUE ',
     *                'INTO TABLE B, BUT DESCRIPTOR DOES NOT EXIST IN ',
     *                'CURRENT MODIFIED TABLE B'
                  RETURN
              END IF
          END IF
      ELSE IF (KCLASS.EQ.4) THEN
C                  SET/RESET ASSOCIATED FIELD WIDTH
          IF (ISTEP.EQ.3) THEN
              KARY(27)  = KSEG
          END IF
      ELSE IF (KCLASS.EQ.5) THEN
C                  SET TO PROCESS TEXT/ASCII DATA
C                                  SET TO TEXT
C                                      PROCESS TEXT

          KARY(2)  = KARY(11) + KARY(18)
          IF (ISTEP.EQ.3) THEN
C                            KSEG TELLS HOW MANY BYTES EACH ITERATION
              IF (MOD(KSEG,4).NE.0) THEN
                  ITER  = KSEG / 4 + 1
              ELSE
                  ITER   = KSEG / 4
              END IF
C                             POINT AT CORRECT KDATA WORD
              IF (ISECT3(3).NE.0) THEN
C                           COMPRESSED
C  ---------------------------------------------------
                  CALL SBYTES(KBUFR,ZEROES,KARY(3),32,0,ITER)
                  KARY(3)  = KARY(3) + KSEG * 8
C
                  CALL SBYTE (KBUFR,KSEG*8,KARY(3),6)
                  KARY(3)  = KARY(3) + 6
C                             TEXT ENTRY BY SUBSET
                  DO 2000 M = 1, ISECT3(1)
                      JAY  = KARY(3)
C                                 NUMBER OF SUBSETS
                      DO 1950 KL  = 1, ITER
C                                 NUMBER OF WORDS
                          KK  = KARY(2) + KL - 1
                          IF (ISECT3(10).EQ.1) THEN
                              CALL W3AI38(KDATA(M,KK),4)
                          END IF
                          CALL SBYTE (KBUFR,KDATA(M,KK),JAY,32)
                          JAY  = JAY + 32
 1950                 CONTINUE
                      KARY(3)  = KARY(3) + KSEG * 8
 2000             CONTINUE
C  ---------------------------------------------------
              ELSE
C                           NOT COMPRESSED

C       CALL SBYTE FOR EACH KDATA VALUE (4 CHARACTERS PER VALUE).
C        AN ADDITIONAL CALL IS DONE IF HAVE A VALUE WITH LESS THAN
C        4 CHARACTERS.
                  NBIT = 32
                  NLEFT = MOD(KSEG,4)
                  DO 3000 J=KARY(2),ITER+KARY(2)-1
                      IF((J.EQ.(ITER+KARY(2)-1)).AND.(NLEFT.NE.0))THEN
                          NBIT = 8 * NLEFT
                      ENDIF
                      IF (ISECT3(10).NE.0) THEN
                          CALL W3AI38 (KDATA(I,J),4)
                      END IF
                      CALL SBYTE(KBUFR,KDATA(I,J),KARY(3),NBIT)
                      KARY(3) = KARY(3) + NBIT
 3000             CONTINUE

C                           ADJUST FOR EXTRA WORDS
                  KARY(18)  = KARY(18) + ITER - 1
              END IF
              KARY(2) = KARY(2) + ITER
          END IF
      ELSE IF (KCLASS.EQ.6) THEN
C                  SET TO SKIP PROCESSING OF NEXT DESCRIPTOR
C                  IF IT IS NOT IN BUFR TABLE B
C                  DURING THE ENCODING PROCESS, THIS HAS NO MEANING
C                  ELIMINATE IN PROCESSING
C                  MOVE DESCRIPTOR LIST UP ONE POSITION AND RESTART
C                  PROCESSING AT SAME LOCATION.
          KM  = I - 1
          DO 9000 KL = I+1, NRDESC
              KM         = KM + 1
              KDESC(1,KM)  = KDESC(1,KL)
 9000     CONTINUE
          NRDESC  = KM
          RETURN 1
      END IF
C  ****************************************************************
      RETURN
      END
      SUBROUTINE FI8503(I,KDESC,NRDESC,
     *                     ISECT3,IUNITD,KSEQ,KNUM,KLIST,IERRTN)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI8503      EXPAND SEQUENCE DESCRIPTOR
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 93-12-03
C
C ABSTRACT: HAVE ENCOUNTERED A SEQUENCE DESCRIPTOR.  MUST PERFORM
C           PROPER REPLACMENT OF DESCRIPTORS IN LINE.
C
C PROGRAM HISTORY LOG:
C   93-12-03  CAVANAUGH
C   YY-MM-DD  MODIFIER1   DESCRIPTION OF CHANGE
C
C USAGE:    CALL FI8503(I,KDESC,NRDESC,
C    *                     ISECT3,IUNITD,KSEQ,KNUM,KLIST,IERRTN)
C   INPUT ARGUMENT LIST:
C     I        - CURRENT POSITION IN DESCRIPTOR LIST
C     KDESC    - LIST OF DESCRIPTORS
C     NRDESC   - NUMBER OF DESCRIPTORS IN KDESC
C     IUNITD   -
C     KSEQ     -
C     KNUM     -
C     KLIST    -
C
C   OUTPUT ARGUMENT LIST:
C     I        - CURRENT POSITION IN DESCRIPTOR LIST
C     KDESC    - MODIFIED LIST OF DESCRIPTORS
C     NRDESC   - NEW NUMBER OF DESCRIPTORS IN KDESC
C     IERRTN   - ERROR RETURN VALUE
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916-128, Y-MP8/864, Y-MP EL92/256
C
C$$$
C
      INTEGER      I
      INTEGER      KDESC(3,*)
      INTEGER      NRDESC
      INTEGER      ISECT3(*)
      INTEGER      IUNITD
      INTEGER      KSEQ(*)
      INTEGER      KNUM(*)
      INTEGER      KLIST(300,*)
      INTEGER      IERRTN
      INTEGER      ITAIL(1600)
C     INTEGER      IHOLD(200)
C
      SAVE
C
C  ****************************************************************
      IERRTN  = 0
C                            READ IN TABLE D IF NEEDED
      IF (ISECT3(9).EQ.0) THEN
          CALL FI8513 (IUNITD,ISECT3,KSEQ,
     *                                     KNUM,KLIST,IERRTN)
          IF (IERRTN.NE.0) THEN
C             PRINT *,'EXIT  FI8503A'
              RETURN
          END IF
      END IF
C                           HAVE TABLE D
C
C                    FIND MATCHING SEQUENCE DESCRIPTOR
      DO 100 L = 1, ISECT3(9)
          IF (KDESC(1,I).EQ.KSEQ(L)) THEN
C   JEN - DELETE NEXT PRINT LINE
C             PRINT *,'FOUND ',KDESC(1,I)
C                              HAVE A MATCH
              GO TO 200
          END IF
  100 CONTINUE
      IERRTN  = 12
      RETURN
  200 CONTINUE
C                    REPLACE SEQUENCE DESCRIPTOR WITH IN LINE SEQUENCE
      IPUT    = I
C                           SAVE TAIL
      ISTART  = I + 1
      KK  = 0
      DO 400 IJ  = ISTART, NRDESC
          KK  = KK + 1
          ITAIL(KK)  = KDESC(1,IJ)
  400 CONTINUE
C                           INSERT SEQUENCE OF DESCRIPTORS AT
C                                    CURRENT LOCATION
      KL  = 0
      DO 600 KQ = 1, KNUM(L)
          KDESC(1,IPUT)  = KLIST(L,KQ)
          IPUT = IPUT + 1
  600 CONTINUE

C                           RESTORE TAIL
      DO 800 KL = 1, KK
          KDESC(1,IPUT) = ITAIL(KL)
          IPUT  = IPUT + 1
  800 CONTINUE
C                            RESET NUMBER OF DESCRIPTORS IN KDESC
      NRDESC  = IPUT - 1
C  JEN - DELETE NEXT PRINT LINE
C     PRINT *,' NRDESC IS ',NRDESC

C                           RESET CURRENT POSITION & RETURN
      RETURN
      END
      SUBROUTINE FI8505(MIF,MDESC,NR,IERRTN)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI8505      CONVERT DESCRIPTORS FXY TO DECIMAL
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 93-12-03
C
C ABSTRACT: CONSTRUCT DECIMAL DESCRIPTOR VALUES FROM F X AND Y SEGMENTS
C
C PROGRAM HISTORY LOG:
C   93-12-03  CAVANAUGH
C   YY-MM-DD  MODIFIER1   DESCRIPTION OF CHANGE
C   YY-MM-DD  MODIFIER2   DESCRIPTION OF CHANGE
C
C USAGE:    CALL FI8505(MIF,MDESC,NR,IERRTN)
C   INPUT ARGUMENT LIST:
C     MIF      - INPUT FLAG
C     MDESC    - LIST OF DESCRIPTORS IN F X Y FORM
C     NR       - NUMBER OF DESCRIPTORS IN MDESC
C
C   OUTPUT ARGUMENT LIST:
C     MDESC    - LIST OF DESCRIPTORS IN DECIMAL FORM
C     IERRTN   - ERROR RETURN VALUE
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916-128, Y-MP8/864, Y-MP EL92/256
C
C$$$
C
      INTEGER      MDESC(3,*), NR
C
      SAVE
C
      IF (NR.EQ.0) THEN
          IERRTN  = 14
          RETURN
      END IF
C
      DO 100 I = 1, NR
          MDESC(1,I)  = MDESC(1,I) * 16384 + MDESC(2,I) * 256
     *                     + MDESC(3,I)
C   JEN - DELETE NEXT PRINT LINE
C     PRINT *,MDESC(2,I),MDESC(3,I),' BECOMES ',MDESC(1,I)
  100 CONTINUE
      MIF  = 1
      RETURN
      END
      SUBROUTINE FI8506(ISTEP,ISECT3,KARY,JDESC,NEWNR,KDESC,NRDESC,
     *           LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KWIDTH,KRFVSW,NEWRFV,
     *           KSEQ,KNUM,KLIST,IBFSIZ,
     *           KDATA,KBUFR,IERRTN,INDEXB)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI8506      PROCESS DATA IN NON-COMPRESSED FORMAT
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 93-12-03
C
C ABSTRACT: PROCESS DATA INTO NON-COMPRESSED FORMAT FOR INCLUSION INTO
C           SECTION 4 OF THE BUFR MESSAGE
C
C PROGRAM HISTORY LOG:
C   93-12-03  CAVANAUGH
C   94-03-24  J. HOPPA   - CHANGED THE INNER LOOP FROM A DO LOOP TO A
C                          GOTO LOOP SO NRDESC ISN'T A SET VALUE.
C                        - CORRECTED A VALUE IN THE CALL TO FI8503.
C   94-03-31  J. HOPPA   - CORRECTED AN ERROR IN SENDING THE SUBSET
C                          NUMBER RATHER THAN THE DESCRIPTOR NUMBER
C                          TO SUBROUTINE FI8501.
C                        - ADDED THE SUBSET NUMBER TO THE FI8501
C                          PARAMETER LIST.
C   94-04015  J. HOPPA   - ADDED LINE TO KEEP THE PARAMETER POINTER
C                          KARY(2) UP TO DATE.  THIS VARIABLE IS USED
C                          IN SUBROUTINE FI8502.
C                        - ADDED KBUFR TO THE PARAMETER LIST IN THE CALL
C                          TO SUBROUTINE FI8502.
C                        - CORRECTED AN INFINITE LOOP WHEN HAVE AN
C                          OPERATOR DESCRIPTOR THAT WAS CAUSED BY
C                          A CORRECTION MADE 94-03-24
C   94-04-20  J. HOPPA   - ADDED K TO CALL TO SUBROUTINE W3FI01
C   94-04-29  J. HOPPA   - CHANGED N TO KARY(11) AND K TO KARY(2)
C                        - REMOVED K AND N FROM THE CALL TO FI8501
C   94-05-03  J. HOPPA   - ADDED AN INCREMENT TO KARY(11) TO PREVENT
C                          AND INFINITE LOOP WHEN HAVE A MISSING VALUE
C   94-05-18  J. HOPPA   - CHANGED SO INCREMENTS KARY(2) AFTER EACH
C                          CALL TO SBYTE AND DELETED
C                          KARY(2) = KARY(11) + KARY(18)
C
C
C USAGE     CALL FI8506(ISTEP,ISECT3,KARY,JDESC,NEWNR,KDESC,NRDESC,
C    *           LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KWIDTH,KRFVSW,NEWRFV,
C    *           KSEQ,KNUM,KLIST,
C    *           KDATA,KBUFR,IERRTN,INDEXB)
C
C   INPUT ARGUMENT LIST:
C     ISTEP    -
C     ISECT3   -
C     KARY     -
C     JDESC    -
C     NEWNR    -
C     KDESC    -
C     NRDESC   -
C     LDESC    -
C     ANAME    -
C     AUNITS   -
C     KSCALE   -
C     KRFVAL   -
C     KWIDTH   -
C     KRFVSW   -
C     NEWRFV   -
C     KSEQ     -
C     KNUM     -
C     KLIST    -
C
C   OUTPUT ARGUMENT LIST:
C     KDATA    -
C     KBUFR    -
C     IERRTN   -
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916-128, Y-MP8/864, Y-MP EL92/256
C
C$$$
C
C  -------------------------------------------------------------
      INTEGER        ISTEP,INDEXB(*)
      INTEGER        KBUFR(*)
      INTEGER        ISECT3(*)
      INTEGER        KARY(*)
      INTEGER        NRDESC,NEWNR,KDESC(3,*),JDESC(3,*)
      INTEGER        KDATA(500,*)
      INTEGER        KRFVSW(*),KSCALE(*),KRFVAL(*),KWIDTH(*),NEWRFV(*)
      INTEGER        IERRTN
      INTEGER        LDESC(*)
      INTEGER        IBITS(32)
      INTEGER        MISG
      INTEGER        KSEQ(*),KNUM(*),KLIST(300,*)
      CHARACTER*40   ANAME(*)
      CHARACTER*25   AUNITS(*)
      CHARACTER*9    CCITT
      LOGICAL        TEXT
C
      SAVE
C  -------------------------------------------------------------
      DATA  IBITS /         1,          3,          7,         15,
     *                     31,         63,        127,        255,
     *                    511,       1023,       2047,       4095,
     *                   8191,      16383,      32767,      65535,
     *             Z'0001FFFF',Z'0003FFFF',Z'0007FFFF',Z'000FFFFF',
     *             Z'001FFFFF',Z'003FFFFF',Z'007FFFFF',Z'00FFFFFF',
     *             Z'01FFFFFF',Z'03FFFFFF',Z'07FFFFFF',Z'0FFFFFFF',
     *             Z'1FFFFFFF',Z'3FFFFFFF',Z'7FFFFFFF',Z'FFFFFFFF'/
      DATA  CCITT /'CCITT IA5'/
      DATA  MISG /99999/
C  -------------------------------------------------------------
       KEND = IBFSIZ * 8 - 32
C                        **********************************************
C                        *                                            *
C                        *      PROCESS AS NON-COMPRESSED MESSAGE     *
C                        *                                            *
C                        *   I POINTS TO SUBSET                       *
C                        *   N POINTS TO DESCRIPTOR                   *
C                        *   K ADJUSTS N TO CORRECT DATA ENTRY        *
C                        *                                            *
C                        **********************************************
      DO 4500 I = 1, ISECT3(1)
C                                  OUTER LOOP FOR EACH SUBSET
C                              DO UNTIL ALL DESCRIPTORS HAVE
C                                  BEEN PROCESSED
C                              SET ADDED BIT FOR WIDTH TO 0
          KARY(26)  = 0
C                             SET ASSOCIATED FIELD WIDTH TO 0
          KARY(27)  = 0
          KARY(18)  = 0
C                    IF MESSAGE CONTAINS DELAYED REPLICATION
C                      WE NEED TO EXPAND THE ORIGINAL DESCRIPTOR LIST
C                      TO MATCH THE INPUT DATA.
C                      START WITH JDESC
          IF (KARY(4).NE.0) THEN
              DO 100 M = 1, NEWNR
                  KDESC(1,M) = JDESC(1,M)
  100         CONTINUE
              NRDESC  = NEWNR
          END IF
          KARY(11) = 1
          KARY(2) = 1
 4300     CONTINUE
          IF(KARY(11).GT.NRDESC) GOTO 4305
C                                  INNER LOOP FOR PARAMETER
 4200         CONTINUE
C             KARY(2) = KARY(11) + KARY(18)
C             PRINT *,'LOOKING AT DESCRIPTOR',KARY(11),
C    *                         KDESC(1,KARY(11)),
C    *                         KARY(2),KDATA(I,KARY(2))
C
C                                  PROCESS ONE DESCRIPTOR AT A TIME
C
C                             ISOLATE TABLE
C
              KFUNC      = KDESC(1,KARY(11)) / 16384
C                             ISOLATE CLASS
              KCLASS     = MOD(KDESC(1,KARY(11)),16384) / 256
              KSEG       = MOD(KDESC(1,KARY(11)),256)
              IF (KFUNC.EQ.1) THEN
C                            REPLICATION DESCRIPTOR
                  CALL FI8501(KARY,ISTEP,KCLASS,KSEG,IDATA,RDATA,
     *                  KDATA,I,KDESC,NRDESC,IERRTN)
                  IF (IERRTN.NE.0) THEN
                      RETURN
                  END IF
                  GO TO 4200
              ELSE IF (KFUNC.EQ.2) THEN
C                            OPERATOR DESCRIPTOR
                  CALL FI8502(*4200,KBUFR,KCLASS,KSEG,
     *                                            KDESC,NRDESC,I,ISTEP,
     *            KARY,KDATA,ISECT3,KRFVSW,NEWRFV,LDESC,IERRTN,INDEXB)
                  IF (IERRTN.NE.0) THEN
                      RETURN
                  END IF
                  KARY(11) = KARY(11) + 1
                  GO TO 4300
              ELSE IF (KFUNC.EQ.3) THEN
C                            SEQUENCE DESCRIPTOR
                  CALL FI8503(KARY(11),KDESC,NRDESC,
     *                     ISECT3,IUNITD,KSEQ,KNUM,KLIST,IERRTN)
                  IF (IERRTN.NE.0) THEN
                      RETURN
                  END IF
                  GO TO 4200
              END IF
C                                 FALL THRU WITH ELEMENT DESCRIPTOR
C                                 FIND MATCHING TABLE B ENTRY
              LK  = INDEXB(KDESC(1,KARY(11)))
              IF (LK.LT.1) THEN
C                               FALL THRU WITH NO MATCHING B ENTRY
                  PRINT *,'FI8506 3800',KARY(11),KDESC(1,KARY(11)),
     *                     NRDESC,LK,LDESC(LK)
                  IERRTN  = 2
                  RETURN
              END IF
C
              IF (AUNITS(LK).EQ.CCITT) THEN
                  TEXT  = .TRUE.
              ELSE
                  TEXT  = .FALSE.
              END IF
C
              IF (TEXT) THEN
                  JWIDE  = KWIDTH(LK)
 3775             CONTINUE
                  IF (JWIDE.GT.32) THEN
                      IF(ISECT3(10).NE.0) THEN
                          CALL W3AI38 (KDATA(I,KARY(2)),4)
                      END IF
                      IF ((KARY(3)+32).GT.KEND) THEN
                          IERRTN = 1
                          RETURN
                      END IF
                      CALL SBYTE (KBUFR,KDATA(I,KARY(2)),KARY(3),32)
                      KARY(3)  = KARY(3) + 32
C                                 ADD A WORD HERE ONLY
                      KARY(18)  = KARY(18) + 1
C                     KARY(2)  = KARY(11) + KARY(18)
                      KARY(2) = KARY(2) + 1
                      JWIDE  = JWIDE - 32
                      GO TO 3775
                  ELSE IF (JWIDE.EQ.32) THEN
                      IF(ISECT3(10).NE.0) THEN
                          CALL W3AI38 (KDATA(I,KARY(2)),4)
                      END IF
                      IF ((KARY(3)+32).GT.KEND) THEN
                          IERRTN = 1
                          RETURN
                      END IF
                      CALL SBYTE (KBUFR,KDATA(I,KARY(2)),KARY(3),32)
                      KARY(3)  = KARY(3) + 32
                      KARY(2) = KARY(2) + 1
                      JWIDE  = JWIDE - 32
                  ELSE IF (JWIDE.GT.0) THEN
                      IF(ISECT3(10).NE.0) THEN
                          CALL W3AI38 (KDATA(I,KARY(2)),4)
                      END IF
                      IF ((KARY(3)+JWIDE).GT.KEND) THEN
                          IERRTN = 1
                          RETURN
                      END IF
                      CALL SBYTE (KBUFR,KDATA(I,KARY(2)),KARY(3),JWIDE)
                      KARY(3)  = KARY(3) + JWIDE
                      KARY(2) = KARY(2) + 1
                  END IF
              ELSE
C                               NOT TEXT
                  IF (KARY(27).NE.0.AND.KDESC(1,KARY(11)).NE.7957) THEN
C                                 ENTER ASSOCIATED FIELD
                      IF ((KARY(3)+KARY(27)).GT.KEND) THEN
                          IERRTN = 1
                          RETURN
                      END IF
                      CALL SBYTE (KBUFR,KDATA(I,KARY(2)),KARY(3),
     *                     KARY(27))
                      KARY(3)  = KARY(3) + KARY(27)
                      KARY(18)  = KARY(18) + 1
C                     KARY(2)  = KARY(11) + KARY(18)
                      KARY(2) = KARY(2) + 1
                  END IF
C
                  JWIDE  = KWIDTH(LK) + KARY(26)
                  IF (KDATA(I,KARY(2)).EQ.MISG) THEN
C                                 MISSING DATA, SET ALL BITS ON
                      IF ((KARY(3)+JWIDE).GT.KEND) THEN
                          IERRTN = 1
                          RETURN
                      END IF
                      CALL SBYTE (KBUFR,IBITS(JWIDE),KARY(3),JWIDE)
                      KARY(3)  = KARY(3) + JWIDE
                      KARY(2) = KARY(2) + 1
                      KARY(11) = KARY(11) + 1
                      GO TO 4300
                  END IF
C                                 CAN DATA BE CONTAINED IN SPECIFIED
C                                          BIT WIDTH, IF NOT - ERROR
                  IF (KDATA(I,KARY(2)).GT.IBITS(JWIDE)) THEN
                      IERRTN = 1
                      RETURN
                  END IF
C                                 ADJUST WITH REFERENCE VALUE
                  IF (KRFVSW(LK).EQ.0) THEN
                      JRV  = KRFVAL(LK)
                  ELSE
                      JRV  = NEWRFV(LK)
                  END IF
C
                  KDATA(I,KARY(2)) = KDATA(I,KARY(2)) - JRV
C                                      IF NEW VALUE IS NEGATIVE - ERROR
                  IF (KDATA(I,KARY(2)).LT.0) THEN
                      IERRTN  = 11
                      RETURN
                  END IF
C                                 PACK DATA INTO OUTPUT ARRAY
                  IF ((KARY(3)+JWIDE).GT.KEND) THEN
                      IERRTN = 1
                      RETURN
                  END IF
                  CALL SBYTE (KBUFR,KDATA(I,KARY(2)),KARY(3),JWIDE)
                  KARY(2) = KARY(2) + 1
                  KARY(3)  = KARY(3) + JWIDE
              END IF
          KARY(11) = KARY(11) + 1
          GOTO 4300
 4305     CONTINUE
C                              RESET ALL REFERENCE VALUES TO ORIGINAL
          DO 4310 LX = 1, ISECT3(8)
              KRFVSW(LX)  = 0
 4310     CONTINUE
 4500 CONTINUE
      RETURN
      END
      SUBROUTINE FI8508(ISTEP,IUNITB,IDATA,KDESC,NRDESC,ATEXT,KSUB,KARY,
     *            KDATA,LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KRFVSW,ISECT3,
     *            KWIDTH,KASSOC,IUNITD,KSEQ,KNUM,KLIST,IERRTN,INDEXB)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI8508      COMBINE INTEGER/TEXT DATA
C   PRGMMR: CAVANAUGH             W/NMC42    DATE: 93-12-03
C
C ABSTRACT: CONSTRUCT INTEGER SUBSET FROM REAL AND TEXT DATA
C
C PROGRAM HISTORY LOG:
C   93-12-03  CAVANAUGH
C   YY-MM-DD  MODIFIER1   DESCRIPTION OF CHANGE
C   94-03-31  HOPPA       ADDED KSUB TO FI8501 PARAMETER LIST.
C   94-04-18  HOPPA       ADDED DUMMY VARIABLE IDUM TO FI8502 PARAMETER
C                         LIST.
C   94-04-20  HOPPA       ADDED DUMMY VARIABLE LL TO FI8501 PARAMETER
C                         LIST.
C   94-04-29  HOPPA     - CHANGED I TO KARY(11)
C                       - ADDED A KARY(2) ASSIGNMENT SO HAVE SOMETHING
C                         TO PASS TO SUBROUTINES ** TEST THIS **
C                       - REMOVED I AND LL FROM CALL TO FI8501
C   94-05-13  HOPPA     - ADDED CODE TO CALCULATE KWORDS WHEN KFUNC=2
C   94-05-18  HOPPA     - DELETED KARY(2) ASSIGNMENT
C
C
C USAGE:    CALL FI8508(ISTEP,IUNITB,IDATA,KDESC,NRDESC,ATEXT,KSUB,KARY,
C    *            KDATA,LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KRFVSW,ISECT3,
C    *            KWIDTH,KASSOC,IUNITD,KSEQ,KNUM,KLIST,IERRTN,INDEXB)
C   INPUT ARGUMENT LIST:
C     ISTEP    -
C     IUNITB   - UNIT NUMBER OF DEVICE CONTAINING TABLE B
C     IDATA    - INTEGER WORKING ARRAY
C     KDESC    - EXPANDED DESCRIPTOR SET
C     NRDESC   - NUMBER OF DESCRIPTORS IN KDESC
C     ATEXT    - TEXT DATA FOR CCITT IA5 AND TEXT OPERATOR FIELDS
C     KSUB     - SUBSET NUMBER
C     KARY     - WORKING ARRAY
C     ISECT3   -
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     KDATA    - ARRAY CONTAINING INTEGER SUBSETS
C     LDESC    - LIST OF TABLE B DESCRIPTORS (DECIMAL)
C     ANAME    - LIST OF DESCRIPTOR NAMES
C     AUNITS   - UNITS FOR EACH DESCRIPTOR
C     KSCALE   - BASE 10 SCALE FACTOR FOR EACH DESCRIPTOR
C     KRFVAL   - REFERENCE VALUE FOR EACH DESCRIPTOR
C     KRFVSW   -
C     NEWRFV   -
C     KWIDTH   - STANDARD BIT WIDTH TO CONTAIN EACH VALUE
C                FOR SPECIFIC DESCRIPTOR
C     KASSOC   -
C     IERRTN   - ERROR RETURN FLAG
C
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916-128, Y-MP8/864, Y-MP EL92/256
C
C$$$
C                         TAKE EACH NON-TEXT ENTRY OF SECTION 2
C                               ACCEPT IT
C
C                         TAKE EACH TEXT ENTRY
C                               INSERT INTO INTEGER ARRAY,
C                               ADDING FULL WORDS AS NECESSARY
C                               MAKE SURE ANY LAST WORD HAS TEXT DATA
C                               RIGHT JUSTIFIED
C  ---------------------------------------------------------------------
C                          PASS BACK CONVERTED ENTRY TO LOCATION
C                          SPECIFIED BY USER
C
C                          REFERENCE VALUE WILL BE APPLIED DURING
C                          ENCODING OF MESSAGE
C  ---------------------------------------------------------------------
      INTEGER          IUNITB,IUNITD,KSEQ(*),KNUM(*),KLIST(300,*)
      INTEGER          KDESC(3,*),NRDESC,KASSOC(*)
      INTEGER          IDATA(*),ISTEP
      INTEGER          KDATA(500,*)
      INTEGER          KARY(*),INDEXB(*)
      INTEGER          KSUB,K
      INTEGER          LDESC(*)
      INTEGER          IBITS(32)
      INTEGER          KSCALE(*)
      INTEGER          KRFVAL(*)
      INTEGER          KRFVSW(*)
      INTEGER          KWIDTH(*)
      INTEGER          MISG
      INTEGER          MPTR,ISECT3(*)
      CHARACTER*1      ATEXT(*)
      CHARACTER*1      AHOLD1(256)
      INTEGER          IHOLD4(64)
      CHARACTER*25     AUNITS(*)
      CHARACTER*25     CCITT
      CHARACTER*40     ANAME(*)
C
      SAVE
C
      EQUIVALENCE      (AHOLD1,IHOLD4)
C
C  =====================================
      DATA  CCITT /'CCITT IA5                '/
      DATA  IBITS /         1,          3,          7,         15,
     *                     31,         63,        127,        255,
     *                    511,       1023,       2047,       4095,
     *                   8191,      16383,      32767,      65535,
     *             Z'0001FFFF',Z'0003FFFF',Z'0007FFFF',Z'000FFFFF',
     *             Z'001FFFFF',Z'003FFFFF',Z'007FFFFF',Z'00FFFFFF',
     *             Z'01FFFFFF',Z'03FFFFFF',Z'07FFFFFF',Z'0FFFFFFF',
     *             Z'1FFFFFFF',Z'3FFFFFFF',Z'7FFFFFFF',Z'FFFFFFFF'/
      DATA  MISG  /99999/
C
      IF (ISECT3(8).EQ.0) THEN
          CALL FI8512(IUNITB,ISECT3,KDESC,NRDESC,KARY,IERRTN,
     *               LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KWIDTH,KRFVSW,
     *               IUNITD,KSEQ,KNUM,KLIST,INDEXB)
          IF (IERRTN.NE.0) THEN
              RETURN
          END IF
      END IF
C                         HAVE TABLE B AVAILABLE NOW
C
C                         LOOK AT EACH DATA ENTRY
C                              CONVERT NON TEXT
C                              MOVE TEXT
C
      KPOS  = 0
      MPTR  = 0
      KARY(11) = 0
 1000 CONTINUE
      KARY(11) = KARY(11) + 1
      IF (KARY(11).GT.NRDESC) GO TO 1500
C
C                  RE-ENTRY POINT FOR REPLICATION AND SEQUENCE DESCR'S
C
  500 CONTINUE
      KFUNC  = KDESC(1,KARY(11)) / 16384
      KL  = KDESC(1,KARY(11)) - 16384 * KFUNC
      KCLASS  = KL / 256
      KSEG    = MOD(KL,256)
C     KARY(2) = KARY(11) + KARY(18)
      IF (KFUNC.EQ.1) THEN
C                              REPLICATION DESCRIPTOR
          CALL FI8501(KARY,ISTEP,KCLASS,KSEG,IDATA,RDATA,
     *                  KDATA,KSUB,KDESC,NRDESC,IERRTN)
          IF (IERRTN.NE.0) THEN
              RETURN
          END IF
          GO TO 500
      ELSE IF (KFUNC.EQ.2) THEN
          IF (KCLASS.EQ.5) THEN
C                        HANDLE TEXT OPERATORS
CC
              KAVAIL  = IDATA(KARY(11))
C                                 UNUSED POSITIONS IN LAST WORD
              KREM  = MOD(KAVAIL,4)
              IF (KREM.NE.0) THEN
                  KWORDS  = KAVAIL / 4 + 1
              ELSE
                  KWORDS  = KAVAIL / 4
              END IF
CC
              JWIDE  = KSEG * 8
              GO TO 1200
          END IF
      ELSE IF (KFUNC.EQ.3) THEN
C                          SEQUENCE DESCRIPTOR - ERROR
          CALL FI8503(KARY(11),KDESC,NRDESC,
     *                     ISECT3,IUNITD,KSEQ,KNUM,KLIST,IERRTN)
          IF (IERRTN.NE.0) THEN
              RETURN
          END IF
          GO TO 500
      ELSE
C
C                         FIND MATCHING DESCRIPTOR
C
          K  = INDEXB(KDESC(1,KARY(11)))
          IF (K.LT.1) THEN
              PRINT *,'FI8508-NOT FOUND',KARY(11),KDESC(1,KARY(11)),
     *           ISECT3(8),LDESC(K)
              IERRTN  = 2
              RETURN
          END IF
C                           HAVE MATCHING DESCRIPTOR
  200     CONTINUE
          IF (AUNITS(K)(1:9).NE.CCITT(1:9)) THEN
              IF (KARY(27).NE.0) THEN
                  IF (KDESC(1,KARY(11)).LT.7937.OR.
     *                           KDESC(1,KARY(11)).GT.8191) THEN
C                        ASSOC FLD FOR ALL BUT CLASS 31
                      KPOS  = KPOS + 1
                      IF (KASSOC(KARY(11)).EQ.IBITS(KARY(27))) THEN
                          KDATA(KSUB,KPOS)  = MISG
                      ELSE
                          KDATA(KSUB,KPOS)  = KASSOC(KARY(11))
                      END IF
                  END IF
              END IF
C                        IF NOT MISSING DATA
              IF (IDATA(KARY(11)).EQ.99999) THEN
                  KPOS  = KPOS + 1
                  KDATA(KSUB,KPOS)  = MISG
              ELSE
C                           PROCESS INTEGER VALUES
                  KPOS  = KPOS + 1
                  KDATA(KSUB,KPOS) = IDATA(KARY(11))
              END IF
          ELSE
C                         PROCESS TEXT
C                                 NUMBER OF BYTES REQUIRED BY TABLE B
              KREQ    = KWIDTH(K) / 8
C                                   NUMBER BYTES AVAILABLE IN ATEXT
              KAVAIL  = IDATA(KARY(11))
C                                 UNUSED POSITIONS IN LAST WORD
              KREM  = MOD(KAVAIL,4)
              IF (KREM.NE.0) THEN
                  KWORDS  = KAVAIL / 4 + 1
              ELSE
                  KWORDS  = KAVAIL / 4
              END IF
C                                 MOVE TEXT CHARACTERS TO KDATA
              JWIDE   = KWIDTH(K)
              GO TO 1200
          END IF
      END IF
      GO TO 1000
 1200 CONTINUE
  300 CONTINUE
      NPTR  = MPTR
      DO 400 IJ = 1, KWORDS
              KPOS  = KPOS + 1
              CALL GBYTE(ATEXT,KDATA(KSUB,KPOS),NPTR,32)
              NPTR  = NPTR + 32
  400     CONTINUE
      MPTR  = MPTR + JWIDE
      GO TO 1000
 1500 CONTINUE
      RETURN
      END
      SUBROUTINE FI8509(ISTEP,IUNITB,RDATA,KDESC,NRDESC,ATEXT,KSUB,KARY,
     *            KDATA,LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KRFVSW,ISECT3,
     *            KWIDTH,KASSOC,IUNITD,KSEQ,KNUM,KLIST,IERRTN,INDEXB)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI8509      CONVERT REAL/TEXT INPUT TO INTEGER
C   PRGMMR: CAVANAUGH             W/NMC42    DATE: 93-12-03
C
C ABSTRACT: CONSTRUCT INTEGER SUBSET FROM REAL AND TEXT DATA
C
C PROGRAM HISTORY LOG:
C   93-12-03  CAVANAUGH
C   94-03-31  HOPPA       ADDED KSUB TO THE FI8501 PARAMETER LIST.
C   94-04-18  HOPPA       ADDED DUMMY VARIABLE IDUM TO FI8502 PARAMETER
C                         LIST.
C   94-04-20  HOPPA       ADDED DUMMY VARIABLE LL TO FI8501 PARAMETER
C                         LIST.
C   94-04-29  HOPPA     - CHANGED I TO KARY(11)
C                       - ADDED A KARY(2) ASSIGNMENT SO HAVE SOMETHING
C                         TO PASS TO SUBROUTINES ** TEST THIS **
C                       - REMOVED I AND LL FROM CALL TO FI8501
C   94-05-18  HOPPA     - DELETED KARY(2) ASSIGNMENT
C
C USAGE:    CALL FI8509(ISTEP,IUNITB,RDATA,KDESC,NRDESC,ATEXT,KSUB,KARY,
C    *            KDATA,LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KRFVSW,ISECT3,
C    *            KWIDTH,KASSOC,IUNITD,KSEQ,KNUM,KLIST,IERRTN,INDEXB)
C   INPUT ARGUMENT LIST:
C     IUNITB   - UNIT NUMBER OF DEVICE CONTAINING TABLE B
C     RDATA    - REAL WORKING ARRAY
C     KDESC    - EXPANDED DESCRIPTOR SET
C     NRDESC   - NUMBER OF DESCRIPTORS IN KDESC
C     ATEXT    - TEXT DATA FOR CCITT IA5 AND TEXT OPERATOR FIELDS
C     KSUB     - SUBSET NUMBER
C     KARY     - WORKING ARRAY
C     ISECT3   -
C     IUNITD   -
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     KDATA    - ARRAY CONTAINING INTEGER SUBSETS
C     LDESC    - LIST OF TABLE B DESCRIPTORS (DECIMAL)
C     ANAME    - LIST OF DESCRIPTOR NAMES
C     AUNITS   - UNITS FOR EACH DESCRIPTOR
C     KSCALE   - BASE 10 SCALE FACTOR FOR EACH DESCRIPTOR
C     KRFVAL   - REFERENCE VALUE FOR EACH DESCRIPTOR
C     KRFVSW   -
C     NEWRFV   -
C     KASSOC   -
C     KWIDTH   - STANDARD BIT WIDTH TO CONTAIN EACH VALUE
C                FOR SPECIFIC DESCRIPTOR
C     IERRTN   - ERROR RETURN FLAG
C     KSEG     -
C     KNUM     -
C     KLIST    -
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916-128, Y-MP8/864, Y-MP EL92/256
C
C$$$
C                         TAKE EACH NON-TEXT ENTRY OF SECTION 2
C                               SCALE IT
C                               ROUND IT
C                               CONVERT TO INTEGER
C
C                         TAKE EACH TEXT ENTRY
C                               INSERT INTO INTEGER ARRAY,
C                               ADDING FULL WORDS AS NECESSARY
C                               MAKE SURE ANY LAST WORD HAS TEXT DATA
C                               RIGHT JUSTIFIED
C                          PASS BACK CONVERTED ENTRY TO LOCATION
C                          SPECIFIED BY USER
C
C                          REFERENCE VALUE WILL BE APPLIED DURING
C                          ENCODING OF MESSAGE
C  ---------------------------------------------------------------------
      REAL              RDATA(*)
      INTEGER          IUNITB,IUNITD,KSEQ(*),KNUM(*),KLIST(300,*)
      INTEGER          IBITS(32),INDEXB(*)
      INTEGER          KDESC(3,*),ISTEP
      INTEGER          KDATA(500,*)
      INTEGER          KASSOC(*)
      INTEGER          KARY(*)
      INTEGER          KSUB,K
      INTEGER          LDESC(*)
      INTEGER          NRDESC
      INTEGER          IERRTN
      INTEGER          KSCALE(*)
      INTEGER          KRFVAL(*)
      INTEGER          KRFVSW(*)
      INTEGER          KWIDTH(*)
      INTEGER          MPTR,ISECT3(*)
      INTEGER          MISG
      CHARACTER*1      AHOLD1(256)
      INTEGER          IHOLD4(64)
      CHARACTER*1      ATEXT(*)
      CHARACTER*25     AUNITS(*)
      CHARACTER*25     CCITT
      CHARACTER*40     ANAME(*)
C
      SAVE
C  =====================================
      EQUIVALENCE      (AHOLD1,IHOLD4)
C
      DATA   IBITS/         1,          3,          7,         15,
     *                     31,         63,        127,        255,
     *                    511,       1023,       2047,       4095,
     *                   8191,      16383,      32767,      65535,
     *             Z'0001FFFF',Z'0003FFFF',Z'0007FFFF',Z'000FFFFF',
     *             Z'001FFFFF',Z'003FFFFF',Z'007FFFFF',Z'00FFFFFF',
     *             Z'01FFFFFF',Z'03FFFFFF',Z'07FFFFFF',Z'0FFFFFFF',
     *             Z'1FFFFFFF',Z'3FFFFFFF',Z'7FFFFFFF',Z'FFFFFFFF'/
C
      DATA  CCITT /'CCITT IA5                '/
      DATA  MISG  /99999/
C  =====================================
C
      IF (ISECT3(8).EQ.0) THEN
          CALL FI8512(IUNITB,ISECT3,KDESC,NRDESC,KARY,IERRTN,
     *               LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KWIDTH,KRFVSW,
     *               IUNITD,KSEQ,KNUM,KLIST,INDEXB)
          IF (IERRTN.NE.0) THEN
              RETURN
          END IF
      END IF
C                         HAVE TABLE B AVAILABLE NOW
C
C                         LOOK AT EACH DATA ENTRY
C                              CONVERT NON TEXT
C                              MOVE TEXT
C
      KPOS  = 0
      MPTR  = 0
      KARY(11) = 0
 1000 CONTINUE
      KARY(11) = KARY(11) + 1
      IF (KARY(11).GT.NRDESC) GO TO 1500
C                         RE-ENRY POINT FOR REPLICATION AND
C                          SEQUENCE DESCRIPTORS
  500 CONTINUE
      KFUNC  = KDESC(1,KARY(11)) / 16384
      KL  = KDESC(1,KARY(11)) - 16384 * KFUNC
      KCLASS  = KL / 256
      KSEG    = MOD(KL,256)
C     KARY(2) = KARY(11) + KARY(18)
      IF (KFUNC.EQ.1) THEN
C                              REPLICATION DESCRIPTOR
          CALL FI8501(KARY,ISTEP,KCLASS,KSEG,IDATA,RDATA,
     *                  KDATA,KSUB,KDESC,NRDESC,IERRTN)
          IF (IERRTN.NE.0) THEN
              RETURN
          END IF
          GO TO 500
      ELSE IF (KFUNC.EQ.2) THEN
C                        HANDLE OPERATORS
          IF (KCLASS.EQ.5) THEN
C                                   NUMBER BYTES AVAILABLE IN ATEXT
              KAVAIL  = RDATA(KARY(11))
C                                 UNUSED POSITIONS IN LAST WORD
              KREM  = MOD(KAVAIL,4)
              IF (KREM.NE.0) THEN
                  KWORDS  = KAVAIL / 4 + 1
              ELSE
                  KWORDS  = KAVAIL / 4
              END IF
              JWIDE  = KSEG * 8
              GO TO 1200
          ELSE IF (KCLASS.EQ.2) THEN
              IF (KSEG.EQ.0) THEN
                  KARY(9) = 0
              ELSE
                  KARY(9) = KSEG - 128
              END IF
              GO TO 1200
          END IF
      ELSE IF (KFUNC.EQ.3) THEN
C                          SEQUENCE DESCRIPTOR - ERROR
          CALL FI8503(KDESC,NRDESC,
     *                     ISECT3,IUNITD,KSEQ,KNUM,KLIST,IERRTN)
          IF (IERRTN.NE.0) THEN
              RETURN
          END IF
          GO TO 500
      ELSE
C
C                         FIND MATCHING DESCRIPTOR
C
          K  = INDEXB(KDESC(1,KARY(11)))
          IF (K.LT.1) THEN
              IERRTN  = 2
C             PRINT *,'FI8509 - IERRTN = 2'
              RETURN
          END IF
C                           HAVE MATCHING DESCRIPTOR
  200     CONTINUE
          IF (AUNITS(K)(1:9).NE.CCITT(1:9)) THEN
              IF (KARY(27).NE.0) THEN
                  IF (KDESC(1,KARY(11)).LT.7937.OR.
     *                           KDESC(1,KARY(11)).GT.8191) THEN
C                        ASSOC FLD FOR ALL BUT CLASS 31
                      KPOS  = KPOS + 1
                      IF (KASSOC(KARY(11)).EQ.IBITS(KARY(27))) THEN
                          KDATA(KSUB,KPOS)  = MISG
                      ELSE
                          KDATA(KSUB,KPOS)  = KASSOC(KARY(11))
                      END IF
                  END IF
              END IF
C                        IF NOT MISSING DATA
              IF (RDATA(KARY(11)).EQ.99999.) THEN
                  KPOS  = KPOS + 1
                  KDATA(KSUB,KPOS)  = MISG
              ELSE
C                           PROCESS REAL VALUES
                  IF (KSCALE(K).NE.0) THEN
C                                    SCALING ALLOWING FOR CHANGE SCALE
                      SCALE  = 10. **(IABS(KSCALE(K)) + KARY(9))
                      IF (KSCALE(K).LT.0) THEN
                          RDATA(KARY(11)) = RDATA(KARY(11)) / SCALE
                      ELSE
                          RDATA(KARY(11)) = RDATA(KARY(11)) * SCALE
                      END IF
                  END IF
C                          PERFORM ROUNDING
                  RDATA(KARY(11)) = RDATA(KARY(11)) +
     *                              SIGN(0.5,RDATA(KARY(11)))
C                          CONVERT TO INTEGER
                  KPOS  = KPOS + 1
                  KDATA(KSUB,KPOS) = RDATA(KARY(11))
C
              END IF
          ELSE
C                       PROCESS TEXT
C                                NUMBER OF BYTES REQUIRED BY TABLE B
              KREQ    = KWIDTH(K) / 8
C                                   NUMBER BYTES AVAILABLE IN ATEXT
              KAVAIL  = RDATA(KARY(11))
C                                 UNUSED POSITIONS IN LAST WORD
              KREM  = MOD(KAVAIL,4)
              IF (KREM.NE.0) THEN
                  KWORDS  = KAVAIL / 4 + 1
              ELSE
                  KWORDS  = KAVAIL / 4
              END IF
C                                 MOVE TEXT CHARACTERS TO KDATA
              JWIDE   = KWIDTH(K)
              GO TO 1200
          END IF
      END IF
      GO TO 1000
 1200 CONTINUE
  300 CONTINUE
      NPTR  = MPTR
      DO 400 IJ = 1, KWORDS
          KPOS  = KPOS + 1
          CALL GBYTE(ATEXT,KDATA(KSUB,KPOS),NPTR,32)
          NPTR  = NPTR + 32
  400 CONTINUE
      MPTR  = MPTR + JWIDE
      GO TO 1000
 1500 CONTINUE
C     DO 2000 I = 1, KPOS
C2000 CONTINUE
      RETURN
      END
      SUBROUTINE FI8511(ISECT3,KARY,JIF,JDESC,NEWNR,
     *                        KIF,KDESC,NRDESC,IERRTN)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI8511      REBUILD KDESC FROM JDESC
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 93-12-03
C
C ABSTRACT: CONSTRUCT WORKING DESCRIPTOR LIST FROM LIST OF DESCRIPTORS
C           IN SECTION 3.
C
C PROGRAM HISTORY LOG:
C   93-12-03  CAVANAUGH
C   YY-MM-DD  MODIFIER1   DESCRIPTION OF CHANGE
C
C USAGE:    CALL FI8511(ISECT3,KARY,JIF,JDESC,NEWNR,
C    *                        KIF,KDESC,NRDESC,IERRTN)
C   INPUT ARGUMENT LIST:
C     IUNITD   - UNIT NUMBER OF TABLE D
C     ISECT3   -
C     KARY     - UTILITY - ARRAY SEE MAIN ROUTINE
C     JIF      - DESCRIPTOR INPUT FORM FLAG
C     JDESC    - LIST OF DESCRIPTORS FOR SECTION 3
C     NEWNR    - NUMBER OF DESCRIPTORS IN JDESC
C     KSEQ     - SEQUENCE DESCRIPTOR KEY
C     KNUM     - NR OF DESCRIPTORS IN SEQUENCE
C     KLIST    - LIST OF DESCRIPTORS IN SEQUENCE
C
C   OUTPUT ARGUMENT LIST:
C     KIF      - DESCRIPTOR FORM
C     KDESC    - WORKING LIST OF DESCRIPTORS
C     NRDESC   - NUMBER OF DESCRIPTORS IN KDESC
C     IERRTN   - ERROR RETURN
C                 IERRTN = 0  NORMAL RETURN
C                 IERRTN = 5  FOUND DELAYED REPLICATION DURING
C                             EXPANSION
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916-128, Y-MP8/864, Y-MP EL92/256
C
C$$$
C
      INTEGER      JDESC(3,*), NEWNR, KDESC(3,*), NRDESC
      INTEGER      KARY(*),IERRTN,KIF,JIF
      INTEGER      ISECT3(*)
C
      SAVE
C
      IF (NEWNR.EQ.0) THEN
          IERRTN  = 3
          RETURN
      END IF
C
      NRDESC = NEWNR
      IF (JIF.EQ.0) THEN
          JIF    = 1
          DO  90 I = 1, NEWNR
             KDESC(1,I) = JDESC(1,I)*16384 + JDESC(2,I)*256 + JDESC(3,I)
             JDESC(1,I) = JDESC(1,I)*16384 + JDESC(2,I)*256 + JDESC(3,I)
   90     CONTINUE
      ELSE
          DO 100 I = 1, NEWNR
              KDESC(1,I)  = JDESC(1,I)
  100     CONTINUE
          NRDESC  = NEWNR
      END IF
      KIF    = 1
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE FI8512(IUNITB,ISECT3,KDESC,NRDESC,KARY,IERRTN,
     *               LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KWIDTH,KRFVSW,
     *               IUNITD,KSEQ,KNUM,KLIST,INDEXB)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI8512      READ IN TABLE B
C   PRGMMR: CAVANAUGH       ORG: W/NMC42    DATE: 93-12-03
C
C ABSTRACT: READ IN TAILORED SET OF TABLE B DESCRIPTORS
C
C PROGRAM HISTORY LOG:
C   93-12-03  CAVANAUGH
C   YY-MM-DD  MODIFIER1   DESCRIPTION OF CHANGE
C   94-04-18  HOPPA       AN ERROR HAS BEEN CORRECTED TO PREVENT LATER
C                         SEARCHING TABLE B IF THERE ARE ONLY OPERATOR
C                         DESCRIPTORS IN THE DESCRIPTOR LIST.
C   94-05-17  HOPPA       CHANGED THE LOOP FOR EXPANDING SEQUENCE
C                         DESCRIPTORS FROM A DO LOOP TO A GOTO LOOP
C
C USAGE:    CALL FI8512(IUNITB,ISECT3,KDESC,NRDESC,KARY,IERRTN,
C    *               LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KWIDTH,KRFVSW,
C    *               IUNITD,KSEQ,KNUM,KLIST,INDEXB)
C   INPUT ARGUMENT LIST:
C     IUNITB   - UNIT WHERE TABLE B ENTRIES RESIDE
C     KDESC    - WORKING DESCRIPTOR LIST
C     NRDESC   - NUMBER OF DESCRIPTORS IN KDESC
C     IUNITD   - UNIT WHERE TABLE D ENTRIES RESIDE
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     KARY     -
C     IERRTN   -
C     LDESC    - DESCRIPTORS IN TABLE B (DECIMAL VALUES)
C     ANAME    - ARRAY CONTAINING NAMES OF DESCRIPTORS
C     AUNITS   - ARRAY CONTAINING UNITS OF DESCRIPTORS
C     KSCALE   - SCALE VALUES FOR EACH DESCRIPTOR
C     KRFVAL   - REFERENCE VALUES FOR EACH DESCRIPTOR
C     WIDTH    - BIT WIDTH OF EACH DESCRIPTOR
C     KRFVSW   - NEW REFERENCE VALUE SWITCH
C     KSEQ     - SEQUENCE DESCRIPTOR
C     KNUM     - NUMBER OF DESCRIPTORS IN SEQUENCE
C     KLIST    - SEQUENCE OF DESCRIPTORS
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916-128, Y-MP8/864, Y-MP EL92/256
C
C$$$
C
      INTEGER    KARY(*),LDESC(*),KSCALE(*),KRFVAL(*),KWIDTH(*)
      INTEGER    KDESC(3,*), NRDESC, IUNITB, IERRTN, KRFVSW(*)
      INTEGER    ISECT3(*),KEY(3,1600),INDEXB(*)
      INTEGER    IUNITD,KSEQ(*),KNUM(*),KLIST(300,*)
      CHARACTER*40   ANAME(*)
      CHARACTER*25   AUNITS(*)
C
      INTEGER    MDESC(800),MR,I,J
C
      SAVE
C
C  ===================================================================
      IERRTN  = 0
      DO 100 I = 1, 30
          KARY(I)  = 0
  100 CONTINUE
C INITIALIZE DESCRIPTOR POINTERS TO MISSING
      DO 105 I = 1, 16383
          INDEXB(I) = -1
  105 CONTINUE
C
C  ===================================================================
C                                MAKE A COPY OF THE DESCRIPTOR LIST
C                                   ELIMINATING REPLICATION/OPERATORS
      J  = 0
      DO 110 I = 1, NRDESC
          IF (KDESC(1,I).GE.49152.OR.KDESC(1,I).LT.16384) THEN
              J  = J + 1
              KEY(1,J)  = KDESC(1,I)
          END IF
  110 CONTINUE
      KCNT  = J
C  ===================================================================
C                                REPLACE ALL SEQUENCE DESCRIPTORS
C  JEN - FIXED NEXT BLOCK
C     DO 300 I = 1, KCNT
      I = 1
  300 IF(I.LE.KCNT)THEN
  200     CONTINUE
          IF (KEY(1,I).GE.49152) THEN
              CALL FI8503(I,KEY,KCNT,
     *                     ISECT3,IUNITD,KSEQ,KNUM,KLIST,IERRTN)
              IF (IERRTN.NE.0) THEN
                  RETURN
              END IF
              GO TO 200
          END IF
          I=I+1
          GOTO 300
      ENDIF
C 300 CONTINUE
C  ===================================================================
C                                ISOLATE SINGLE COPIES OF DESCRIPTORS
      MR  = 1
C        THE FOLLOWING LINE IS TO PREVENT LATER SEARCHING TABLE B WHEN
C        HAVE ONLY OPERATOR DESCRIPTORS
      IF(KCNT.EQ.0) GOTO 9000
      MDESC(MR)  = KEY(1,1)
      DO 500 I = 2, KCNT
          DO 400 J = 1, MR
              IF (KEY(1,I).EQ.MDESC(J)) THEN
                  GO TO 500
              END IF
  400     CONTINUE
          MR  = MR + 1
          MDESC(MR)  = KEY(1,I)
  500 CONTINUE
C  ===================================================================
C                                SORT INTO ASCENDING ORDER
C                                READ IN MATCHING ENTRIES FROM TABLE B
      DO 700 KCUR = 1, MR
          NEXT  = KCUR + 1
          IF (NEXT.LE.MR) THEN
              DO 600 LR  = NEXT, MR
                  IF (MDESC(KCUR).GT.MDESC(LR)) THEN
                      IHOLD       = MDESC(LR)
                      MDESC(LR)   = MDESC(KCUR)
                      MDESC(KCUR) = IHOLD
                  END IF
  600         CONTINUE
          END IF
  700 CONTINUE
C  ===================================================================
      REWIND IUNITB
C
C                             READ IN A MODIFIED TABLE B -
C                             MODIFIED TABLE B CONTAINS ONLY
C                             THOSE DESCRIPTORS ASSOCIATED WITH
C                             CURRENT DATA.
C
      KTRY  = 0
      DO 1500 NRTBLB = 1, MR
 1000     CONTINUE
 1001     FORMAT (I1,I2,I3,A40,A25,I4,8X,I7,I5)
          READ (IUNITB,1001,END=2000,ERR=8000)KF,KX,KY,ANAME(NRTBLB),
     *     AUNITS(NRTBLB),KSCALE(NRTBLB),KRFVAL(NRTBLB),KWIDTH(NRTBLB)
          KRFVSW(NRTBLB) = 0
          LDESC(NRTBLB)  = KX*256 + KY
C
          IF (LDESC(NRTBLB).EQ.MDESC(NRTBLB)) THEN
C             PRINT *,'1001',NRTBLB,LDESC(NRTBLB)
C             PRINT *,LDESC(NRTBLB),ANAME(NRTBLB),KSCALE(NRTBLB),
C    *               KRFVAL(NRTBLB),KWIDTH(NRTBLB)
              KTRY  = KTRY + 1
              INDEXB(LDESC(NRTBLB)) = KTRY
C             PRINT *,'INDEX(',LDESC(NRTBLB),' = ',KTRY
          ELSE IF (LDESC(NRTBLB).GT.MDESC(NRTBLB)) THEN
C             PRINT *,'FI8512 - IERRTN=2'
              IERRTN  = 2
              RETURN
          ELSE
              GO TO 1000
          END IF
 1500 CONTINUE
      IF (KTRY.NE.MR) THEN
          PRINT *,'DO NOT HAVE A COMPLETE SET OF TABLE B ENTRIES'
          IERRTN  = 2
          RETURN
      END IF
C     DO 1998 I = 1, 16383, 30
C         WRITE (6,1999) (INDEXB(I+J),J=0,23)
C1998 CONTINUE
C1999 FORMAT(30(1X,I3))
C
 2000 CONTINUE
      IERRTN     = 0
      ISECT3(8)  = MR
      GO TO 9000
 8000 CONTINUE
      IERRTN = 4
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE FI8513 (IUNITD,ISECT3,KSEQ,KNUM,KLIST,IERRTN)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI8513      READ IN TABLE D
C   PRGMMR: CAVANAUGH             W/NMC42    DATE: 93-12-03
C
C ABSTRACT: READ IN TABLE D
C
C PROGRAM HISTORY LOG:
C   93-12-03  CAVANAUGH
C   YY-MM-DD  MODIFIER1   DESCRIPTION OF CHANGE
C
C USAGE:    CALL FI8513 (IUNITD,ISECT3,KSEQ,KNUM,KLIST,IERRTN)
C   INPUT ARGUMENT LIST:
C     IUNITD   - UNIT NUMBER OF INPUT DEVICE
C     KARY     - WORK ARRAY
C
C   OUTPUT ARGUMENT LIST:
C     KSEQ     - KEY FOR SEQUENCE DESCRIPTORS
C     KNUM     - NUMBER IF DESCRIPTORS IN LIST
C     KLIST    - DESCRIPTORS LIST
C     IERRTN   - ERROR RETURN FLAG
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916-128, Y-MP8/864, Y-MP EL92/256
C
C$$$
C
      INTEGER      IUNITD, ISECT3(*)
      INTEGER      KSEQ(*),KNUM(*),KLIST(300,*)
      INTEGER      KKF(10),KKX(10),KKY(10),KF,KX,KY
C
      SAVE
C
      REWIND IUNITD
      J     = 0
      IERRTN  = 0
 1000 CONTINUE
      READ (IUNITD,1001,END=9000,ERR=8000)KF,KX,KY,
     *                KKF(1),KKX(1),KKY(1),
     *                KKF(2),KKX(2),KKY(2),
     *                KKF(3),KKX(3),KKY(3),
     *                KKF(4),KKX(4),KKY(4),
     *                KKF(5),KKX(5),KKY(5),
     *                KKF(6),KKX(6),KKY(6),
     *                KKF(7),KKX(7),KKY(7),
     *                KKF(8),KKX(8),KKY(8),
     *                KKF(9),KKX(9),KKY(9),
     *                KKF(10),KKX(10),KKY(10)
 1001 FORMAT (11(I1,I2,I3,1X),3X)
      J  = J + 1
C                             BUILD SEQUENCE KEY
      KSEQ(J)  = 16384*KF + 256*KX + KY
      DO 2000 LM = 1, 10
C                             BUILD KLIST
          KLIST(J,LM) = 16384*KKF(LM) + 256*KKX(LM) + KKY(LM)
          IF(KLIST(J,LM).NE.0) THEN
              KNUM(J)  = LM
          END IF
 2000 CONTINUE
      GO TO 1000
 8000 CONTINUE
      IERRTN  = 6
 9000 CONTINUE
      ISECT3(9) = J
      RETURN
      END
