      SUBROUTINE MERGES(ITAPE,IERROR)
C                                                       19-DEC-1995/DSS
C     READ THE INPUT FILE from UNIT=55; BUFFERED 2 RECORDS AT A TIME; 
C     SORTING AS WE GO; 
C     AND SPLITTING INTO TWO EQUAL FILES AND COUNTING THE RECORDS
C     INPUT FILE IS PRESUMED TO BE UNIT=55; 
C     UNITS 60,61,62,63 ARE SCRATCH;
C     RESULTS WILL BE FOUND ON UNIT =60, OR =62; WHICHEVER UNIT NUMBER
C       RETURNED IN "ITAPE".
C
C     CAUTION: IN THIS VERSION, I HAVE REMOVED THE WORKSPACE BUFF
C       FROM THE CALL SEQUENCE, AND HAVE MADE IT A LOCAL ARRAY.
C
C     ... Where is the pointer into the input Tape55 positioned at
C     ...   completion??
C     ...   The input Tape55 is read in blocks of 1024 long_words;
C     ...   no partial blocks; so any partially filled data block
C     ...   is expected to be zero filled to the blocksize.
C
C     ...      If it read in the "LEND" record, then
C     ...         the pointer is left at the record beyond the "LEND"
C     ...            record; ready to read whatever follows the "LEND"
C     ...            record;
C     ...      Else if it hit the physical End-of-File, then
C     ...         the pointer is left wherever the system leaves it
C     ...            after End-of-File read on an END=123 jump-out;
C     ...            (Seems to me that a physical E-O-F ending should
C     ...            set some flag, so that the caller will not try to
C     ...            call again.)
C     ...   . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     ...   So, what should I do if CSORT() finds a zero terminator
C     ...   in some records other than the last data record before
C     ...   the "LEND" record?
C
C     ...      A solution would be:
C     ...         (a.) Modify record-counts to ensure that the record- 
C     ...              count agrees with where the zero-terminator was 
C     ...              found;
C
C     ...         (b.) Read input records into a void until the
C     ...              logical "LEND" record is read; 
C     ...              or else until the Physical E-O-F is hit;
C     ...         
C     ...   . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     ...   So, what should I do if there is a "LEND" record without
C     ...   a zero-terminator preceding it???
C     ...      A solution would be:
C     ...         (c.) Keep some flags to indicate what the preceding
C     ...              record terminated on; 
C
C     ...         (d.) So that when a file end is encountered, and
C     ...              if the preceding LABEL record did not zero-
C     ...              terminate, then an extra zero-filled entire
C     ...              LABEL-array could be inserted before doing
C     ...              the usual "LEND" or E-O-F processing.
C     ...   
C     ...   . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     ...   Why am I so concerned about zero-terminator?
C     ...      Because I have a hypothesis:
C     ...      CNTRI() comes in after SORT/MERGE and counts the
C     ...      items in the sorted LABEL file by looking for the
C     ...      zero-terminator.
C     ...      Therefore, I must ensure that a zero-terminator exists.
C     ...   . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     ...   CAUTION: This CRAY version ...
C     ...      defines BUFF as an internal-to-this-subroutine array;
C     ...      equivalence to a bigbuff;
C     ...      so that I could write six blocks out with one command;
C     ...   . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     ...   George Vandenburg & Krishna Kumar converted this code for IBM/RS6000
C     ...   1999/07/01

      INTEGER      LMAX
      PARAMETER   (LMAX=1024)
      INTEGER      LMAX6
      PARAMETER   (LMAX6=LMAX*6)

      INTEGER      ITAPE     		!... UNIT NO. OF SORTED RESULTS
      INTEGER      IERROR    		!... RETURN CODE

      INTEGER      BIGBUFF(LMAX6)
      INTEGER      BUFF(LMAX,6)    	!... WORK SPACE FOR SORT/MERGE
      EQUIVALENCE (BIGBUFF,BUFF)

      INTEGER      BUFA
      INTEGER      BUFB

      INTEGER      MSKSORK
      DATA         MSKSORK    / X'FFFFFFFF' /

      INTEGER      MINUS7
      DATA         MINUS7     / X'FFFFFFF9' /

      INTEGER      JSORK
      INTEGER      KSORK
      INTEGER      LSORK

      INTEGER      NITEMS_DONE
      INTEGER      IRET_SOR

      INTEGER      NOF6REC_END
      INTEGER      LUNIN
      INTEGER      LUNOUT
      LOGICAL      LEOFQ   		!... used in MERGE part
      LOGICAL      LEND_55QQ            !... on "LEND" in SORT part
      LOGICAL      LEOF_55QQ        	!... Hit physical E-O-F in SORT
      LOGICAL      LZERO_TERM55QQ  	!... CSORT terminated on word=0 

      CHARACTER*8  C8DIGITS

      SAVE

C     . . . . . .   S T A R T   . . . . . . . . . . . . . . . . . .
ckumar
      print*,'Entering merges '
ckumar

      IERROR = 0
      LEND_55QQ = .FALSE.
      LEOF_55QQ = .FALSE.
      LZERO_TERM55QQ = .FALSE.

      NOF6REC_END = 0
C     ... AFTER READ UNIT=55 OF LABEL-RECORDS INTO BUFF,
C     ...   (WHERE BUFF CAN ONLY HOLD UP TO 6 LABEL-ARRAY RECORDS
C     ...    AT A TIME);
C
C     ... IF NOF6REC_END == 0, THEN
C     ...   NO "LEND" READ WAS READ, AND
C     ...   NO PHYSICAL E-O-F WAS HIT; 
C
C     ... ELSE IF NOF6REC_END IS NON-ZERO, THEN
C     ...  "LEND" RECORD WAS READ; OR E-O-F WAS HIT;
C     ...   AND NOF6REC_END CONTAINS
C     ...         NO. OF LABEL-ARRAY DATA RECORDS (=[1:6]) IN BUFF
C     ...         TO BE PROCESSED

      NTOTRECINP = 0
C     ...WHERE NTOTRECINP COUNTS TOTAL NO. OF RECORDS FOUND ON TAPE55

      NWRD2SORT=0
C     ... WHERE NWRD2SORT IS COUNT OF LONG WORDS TO SORT.   
C     ... TO TELL CSORT() THE NO. OF LONG WORDS TO SORT;
C     ... AND DEPENDS UPON NO. OF RECORDS IN BUFF;  NWRD2SORT IS 
C     ... INCREMENTED BY LMAX (WHICH IS LABEL-RECORD SIZE IN LONG WORDS)

      LCKPT = 1
      L0 = 55
      L1 = 60
      L2 = 61
      REWIND 60
      REWIND 61
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     ...THE FOLLOWING DO 9 LOOP IS USED ONLY FOR READING THE VERY
C     ...   FIRST 6 RECORDS FROM TAPE55...
      DO 9 J=1,6
        jsave = j
        LUNIN = L0
        READ(L0,ERR=920,END=12)(BUFF(I,J),I=1,LMAX)

    7   CONTINUE
        BUFA = BUFF(1,J)
C       ... TO TEST FOR "LEND" ...
        JSORK = IAND((ISHFT(BUFA,-32)),MSKSORK)
        IF(JSORK .EQ. MINUS7) THEN
C         ... THIS IS A LOGICAL E-O-F RECORD ON TAPE55
          LEND_55QQ = .TRUE.
          BUFF(1,J) = 0     		!... TO ZERO THE "LEND" ITEM
          GO TO 13
        ENDIF
C
        NWRD2SORT = NWRD2SORT + LMAX
        NTOTRECINP = NTOTRECINP + 1
 9    CONTINUE
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     ...WHEN IT FALLS THRU THIS LOOP, IT HAS READ 6 RECORDS COMPLETELY
      GO TO 14
C
  12  CONTINUE
C     ...COMES TO 12 ON E-O-F HIT,  DURG FIRST 6 RECORDS OF TAPE55
      LEOF_55QQ = .TRUE.
      GO TO 13

 13   CONTINUE
C     ...COMES TO 13 ON E-O-F HIT, OR "LEND" HIT, DURG FIRST 6 RECORDS 
C     ...    OF TAPE55
      NOF6REC_END = jsave - 1
      IF(NTOTRECINP .EQ. 0) THEN
        IERROR = 61
        GO TO 900
      ENDIF
      GO TO 14

C     . . . .   TO SORT ONE BATCH OF LABEL-ARRAY RECORDS OF UP-TO-SIX
C     . . . .   RECORDS ACCUMULATED IN BUFF;  THIS IS THE ONLY PLACE
C     . . . .   THAT THE SORTER IS CALLED.
C     . . . .   NWRD2SORT IS MODULO 1024 LONG-WORDS -- (DOES NOT REFINE
C     . . . .     COUNT TO THE LOCATION OF THE ZERO-TERMINATOR WORD).
C     . . . .   NWRD2SORT DOES NOT INCLUDE IN COUNT THE WORDS IN A
C     . . . .    "LEND" RECORD.

   14 CONTINUE
ckumar
ckumar Changed the name of SSORT to CSORT -see comments of CSORT!!! 
ckumar
      CALL CSORT(BUFF,NWRD2SORT,NITEMS_DONE,IRET_SOR)
      IF(IRET_SOR .NE. 0) THEN
        IF (IRET_SOR .NE. -1) THEN
        WRITE(6,FMT='(1H ,''MERGES::CSORT: NOT-NORMAL RETURN CODE='',I5,
     1        /1H ,6X,'' NWORDS-TO-SORT='',I6,''; NWORDS-SORTED='',I6,
     2             ''; NRECS-INP='',I5)')
     A          IRET_SOR,NWRD2SORT,NITEMS_DONE,NTOTRECINP
        ENDIF

        IF(IRET_SOR .EQ. -1) THEN
          LZERO_TERM55QQ = .TRUE.
C         ... BUT WHERE IS THE ZERO TERMINATOR WORD LOCATED?
          LOC0WRD = NITEMS_DONE + 1
          LOC0BIN6 = 1 + (NITEMS_DONE/LMAX)
          NOF6REC_END = LOC0BIN6

          LOC0WRDWILBL = 1 + (MOD(NITEMS_DONE,LMAX)) 
          NTHBIGBLOK = (NTOTRECINP-1)/6
          NTHLBLBIN = 6*NTHBIGBLOK + LOC0BIN6

          WRITE(6,FMT='(1H ,''MERGES::CSORT: FOUND A ZERO-TERMINATOR-'',
     1                  ''WORD LOCATED IN LABEL-REC-NUM='',I5,
     2                 /1H ,15X,''in WORD NO.'',I6,''; LEOF= .'',L1,
     3                  ''.  LEND= .'',L1,''.'')')
     A            NTHLBLBIN,LOC0WRDWILBL,LEOF_55QQ,LEND_55QQ

          CALL MVTOLEND(LZERO_TERM55QQ,LEND_55QQ,LEOF_55QQ,
     1                  NREC_TRASHED,IRET_MVTO)

          WRITE(6,FMT='(1H ,''MERGES::MVTOLEND: RETURNED WITH '',
     1               ''NREC_TRASHED='',I8,''; IRET_MVTO='',I3,
     2                 /1H ,7X,''NOF6REC_END='',I3)')
     A            NREC_TRASHED,IRET_MVTO,NOF6REC_END
          GO TO 24
        ENDIF
      ENDIF

      LCKPT = 2
      IF(NOF6REC_END .NE. 0) THEN
C       ... THIS BATCH OF SORTED DATA IS THE LAST BATCH AT END OF FILE;
C       ... THIS BATCH OF SORTED DATA IN BIGBUFF DOES NOT HAVE A
C       ...    ZERO-TERMINATION FLAG;
C       ... ONE OF THE END-OF-FILE CONDITIONS HAS BEEN ENCOUNTERED
C       ...    BEFORE CSORT(); SO I HAVE SOME GOOD SORTED DATA BINS
C       ...    TO OUTPUT;  BUT I WANT TO ADD A ZERO RECORD;
        IF(NOF6REC_END .LE. 5) THEN
C          ... THERE IS SPACE FOR A ZERO RECORD WITHIN CURRENT BIGBUFF
           NOF6REC_END = NOF6REC_END + 1
           DO  I = 1,LMAX
             BUFF(I,NOF6REC_END) = 0
           ENDDO
           NTOTRECINP = NTOTRECINP + 1
           GO TO 24
C          ... WHICH WILL WRITE OUT THIS LAST BATCH, INCLUDING THE ZERO

        ELSE
C          ... NOF6REC_END IS .GE. 6; SO NO ROOM IN BIGBUFF FOR ZERO REC
C          ... SO WRITE OUT THE COMPLETELY FULL BIGBUFF,
           LCKPT = 143
           LUNOUT = L1
           print *,' MERGES WRITE TO L1 ',l1,lmax6
cgeorge 
cgeorge  must write 1024 or less word records
cgeorge
            do lb=1,lmax6,1024
               lss=(lb-1)
               WRITE(L1,ERR=930)(BIGBUFF(I),I=lb,lb+1023)
            enddo
cgeorge     WRITE(L1,ERR=930)(BIGBUFF(I),I=1,LMAX6)

           GO TO 232
C          ... WHERE AT 232 WILL WRITE A ZERO RECORD
        ENDIF
           
      ENDIF
C     ... OTHERWISE, THE END OF FILE HAS NOT YET BEEN ENCOUNTERED,
C                   (NEITHER THE LOGICAL E-O-F,  NOR THE PHYSICAL E-O-F)
C     ...    SO, I NOW HAVE IN THE WORK AREA, 6 BINS OF SORTED DATA,

      NWRD2SORT = 0
      LCKPT = 145
      LUNOUT = L1
cgeorge
cgeorge must write 1024 or less word records
cgeorge
             do lb=1,lmax6,1024
                lss=(lb-1)
                WRITE(L1,ERR=930)(BIGBUFF(I),I=lb,lb+1023  )
             enddo
cgeorge      WRITE(L1,ERR=930)(BIGBUFF(I),I=1,LMAX6)

C     ... WHERE THE ABOVE REPLACED WITH ONE WRITE
C     ...   WHAT USED TO BE 6 WRITES OF LMAX-SIZED RECORDS

 15   CONTINUE
      DO 18 J=1,6
        jsave = j

        LCKPT = 154
        LUNIN = L0

        READ(L0,ERR=920,END=19)(BUFF(I,J),I=1,LMAX)

C       ... TO TEST FOR "LEND"  ...
        BUFA = BUFF(1,J)
        JSORK = IAND((ISHFT(BUFA,-32)),MSKSORK)
        IF(JSORK .EQ. MINUS7) THEN
C         ... THIS WAS A LOGICAL END-OF-FILE RECORD...
          LEND_55QQ = .TRUE.
          BUFF(1,J) = 0
          GO TO 20
        ENDIF

        NWRD2SORT = NWRD2SORT + LMAX
        NTOTRECINP = NTOTRECINP + 1
 18   CONTINUE
C     ...  WHERE 18 IS ENDDO ON J=1,6 . . . . . . . . . .

      GO TO 23
C
  19  CONTINUE
C     ... COMES HERE ON HIT E-O-F FROM WITHIN J=1,6  DO 18 LOOP
      LEOF_55QQ = .TRUE.
      GO TO 20

 20   CONTINUE
      NOF6REC_END = jsave - 1
      IF(NWRD2SORT .EQ. 0) GO TO 26

 23   CONTINUE
      L =L1
      L1=L2
      L2=L
      GO TO 14
C
C     *     *     *     *     *     *     *     *     *     *     *
C     ...COMES TO 24 TO OUTPUT THE SORTED RECORDS FROM BUFF
C     ...NO MORE RECORDS ARE TO BE READ FROM TAPE55 BECAUSE E-O-F WAS
C     ...ENCOUNTERED DURG INPUT OF THESE RECORDS WHICH WERE THEN SORTED
C
  232 CONTINUE
C     ... comes to 232 at end of file, 
C     ... if zero-terminator flag is missing from the input;
C     ... and BIGBUFF is empty;
C     ... to output an all-zero record at the logical end of file
C     ... which will ensure the existence of a zero-terminator flag;
      do  i = 1,LMAX
        BUFF(I,1) = 0
      ENDDO

      NTOTRECINP = NTOTRECINP + 1
      NOF6REC_END = 1

      LHOLD =L1
      L1=L2
      L2=LHOLD
      GO TO 24     
C     . . . . . . . . . . . . . . . . . . . . . . . . . . .
 24   CONTINUE
      LCKPT = 24
      LUNOUT = L1

      NWORDS = NOF6REC_END * LMAX
      IF(NWORDS .GT. 0) THEN
           write(0,*)' MERGES WRITE TO L1 ',l1,nwords,' ',3
cgeorge
cgeorge must write 1024 or less word records
cgeorge  
        do lb=1,nwords,1024
           lss=(lb-1)
           WRITE(L1,ERR=930)(BIGBUFF(I),I=lb,lb+1023)  
        enddo
cgeorge        WRITE(L1,ERR=930)(BIGBUFF(I),I=1,NWORDS)
      ENDIF
C
C     ... FINISHED SORT COUNT SPLIT PASS ...
C
 26   CONTINUE

      WRITE(6,FMT='(1H ,''MERGES:AT 26, NTOTRECINP='',I6,
     2                  '';  LEOF= .'',L1,''.  LEND= .'',L1,''.'')')
     A            NTOTRECINP,LEOF_55QQ,LEND_55QQ

      MLNTH = 6
      ITAPE = 60
      ENDFILE L1
      REWIND L1
      IF (MLNTH .GE. NTOTRECINP) GO TO 999

C     ...WHICH IS NORMAL EXIT FOR MOST CODES WHICH USUALLY HAVE 6 BINS
C     ...   OR LESS OF LABEL BINS
C     ...ONLY THOSE W/ EXCESSIVE LABEL ITEMS, SUCH AS THOSE USING ZPLOT
C     ...   LINE ELEMENTS WOULD PROCEED INTO MERGES WHICH FOLLOWS...
C     *     *     *     *     *     *     *     *     *     *     *
C
      ENDFILE L2
      REWIND L2
      PRINT 102,NTOTRECINP
  102 FORMAT(1H , 'MERGES:INPUT T55 RECORD COUNT TOTAL =', I4)
C
C     ... MORE THAN 6 RECORDS IN FILES SO MERGES 
C     ...   UNTIL  MLNTH .GE. NTOTRECINP
C
C     ...BUFF( ,1) AND ( ,2) ARE LHS INPUT BINS FILLED FROM UNIT L1
C     ...BUFF( ,3) AND ( ,4) ARE RHS INPUT BINS FILLED FROM UNIT L2
C     ...BUFF( ,5) AND ( ,6) ARE RESULTING MERGED DATA OUTPUT ON L3
C     ...ITEMS IN THE BINS ARE REFERENCED BY THE FOLLOWING SUBSCRIPTS
C     ...    (K,I1) FOR WORKING BIN OF LHS INPUT DATA
C     ...    (L,I2+2) FOR WORKING BIN OF RHS INPUT DATA
C            (M,I3+4) FOR WORKING BIN OF MERGED DATA
C     ...WHERE I1, I2, I3 ALTERNATE BETWEEN 1 AND 2
C     ...   TO POINT TO WORKING BIN OR TO I/O BUFFER BIN
C
      LINUSV = 60
C     ...WHERE LINUSV = LEFT INPUT UNIT SAVED FOR L1 INITIALIZATION
      LOTUSV = 62
C     ...WHERE LOTUSV IS LEFT OUTPUT UNIT SAVED FOR L3 INITIALIZATION
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C                 M E R G E 
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C     ...BEGIN OUTERMOST LOOP TO MERGE...
C     ...LOOPS BACK TO 40 FROM THE VERY END OF SUBROUTINE
C
 40   CONTINUE
      L5 = 1
      LEOFQ = .FALSE.
C     ...WHERE LEOFQ FLAGS END-OF-FILE CONDITION ENCOUNTERED

      NLNTH = MLNTH
C     ...WHERE NLNTH IS THE GIVEN SORTED VECTOR LENGTH (IN RECORDS)
C     ...   WHICH IS 6 RECORDS ON THE FIRST PASS
      MLNTH = 2*MLNTH
C     ...WHERE MLNTH IS THE RESULTING MERGED VECTOR LENGTH
C     ...   WHICH IS 12 RECORDS DURING THE FIRST PASS
      L1 = LINUSV
      L2 = L1+1
      L3 = LOTUSV
      LA = LOTUSV + 1
C     ...WHERE LA IS THE ALTERNATE OUTPUT UNIT
      ILCNT = 0
      LCKPT = 52
      LUNIN = L1

      READ(L1,ERR=920,END=910)(BUFF(I,1),I=1,LMAX)

      LCKPT=53
      LUNIN = L2

      READ(L2,ERR=920,END=910)(BUFF(I,3),I=1,LMAX)

   42 CONTINUE
      IDLHO = 42
      INLHS = 2
      IDRHO = 420
      INRHS = 4
C     ...WHICH ISSUED THE RDS W/O DELAY...
      REWIND L3
      REWIND LA
      LAACTQ = 0
C     ...TO FLAG WHETHER UNIT LA IS ACTIVE...
C
C     MERGES EACH LIST LENGTH NLNTH
C
      I3 = 1
 421  CONTINUE
      I1 = 1
      I2 = 1
      IACNT = 0
      IBCNT = 0
      K = 1
      L = 1
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C
C     MAIN MERGES LOOP
C     ...   DO 47 LOOP THRU 1024 WORDS OF RECORD...
C
 422  CONTINUE
      DO 47 M=1,LMAX
        MSAVE=M
        BUFA = BUFF(K,I1)
        KSORK = IAND((ISHFT(BUFA,-32)),MSKSORK)

        BUFB = BUFF(L,I2+2)
        LSORK = IAND((ISHFT(BUFB,-32)),MSKSORK)

        IF(KSORK .GT. LSORK) GO TO 46
C
C       ...COMES HERE TO TRANSFER LHS ITEM INTO MERGED BIN
        BUFF(M,I3+4) = BUFF(K,I1)
        K = K+1
        IF (K .GT. LMAX) GO TO 43
        IF (BUFF(K,I1) .NE. 0) GO TO 47
C       ...OTHERWISE, IT FOUND THE TERMINATING ZERO.
C       ...THE SUCCEEDING READ ON L1 MUST HAVE BEEN AN E-O-F...
        LCKPT = 40
        LUNIN = L1
        LEOFQ = .TRUE.

        READ(L1,ERR=920,END=49)(BUFF(I,INLHS),I=1,LMAX)

  811   PRINT  813, L1, LCKPT
  813   FORMAT(1H , 'MERGES:ERROR STOP ON EXTRANEOUS ZERO IN UNIT',
     1              I3,'; AT LCKPT=', I4)
        IERROR = 71
        GO TO 900
C
 43     CONTINUE
        LCKPT = 54
        LUNIN = L1
        LEOFQ = .TRUE.

        READ(L1,ERR=920,END=49)(BUFF(I,INLHS),I=1,LMAX)

   44   CONTINUE
        LEOFQ = .FALSE.
        IDLHO = 44
        INLHS = I1
        IACNT = IACNT+1
        IF (IACNT.GE.NLNTH) GO TO 49
        I1 = MOD(I1,2)+1
C       ...WHICH SWINGS THE GATE OF LHS INPUT TO ALTERNATE BIN
        K = 1
C       ...WHICH RESETS THE K INDEX FOR ITEMS W/I LHS BIN
        GO TO 47
C
   46   CONTINUE
C       ...COMES TO 46 TO TRANSFER RHS ITEM INTO MERGED BIN
        BUFF(M,I3+4)=BUFF(L,I2+2)
        L = L+1
        IF(L .GT. LMAX) GO TO 466
        IF (BUFF(L,I2+2) .NE. 0) GO TO 47
C       ...OTHERWISE IT FOUND THE TERMINATING ZERO.
        LCKPT = 42
        LUNIN = L2
        LEOFQ = .TRUE.

        READ(L2,ERR=920,END=50)(BUFF(I,INRHS),I=1,LMAX)

        PRINT 813, L2,LCKPT
        IERROR = 72
        GO TO 900
C
  466   CONTINUE
        LCKPT = 55
        LUNIN = L2
        LEOFQ = .TRUE.

        READ(L2,ERR=920,END=50)(BUFF(I,INRHS),I=1,LMAX)

  468   CONTINUE
        LEOFQ = .FALSE.
        IDRHO = 468
        INRHS = I2+2
        IBCNT = IBCNT+1
        IF (IBCNT.GE.NLNTH) GO TO 50
        I2 = MOD(I2,2)+1
C       ...WHICH SWINGS THE GATE OF THE RHS INPUT TO ALTERNATE BIN
        L = 1
C       ...WHICH RESETS L INDEX FOR ITEMS W/I RHS BIN...
        GO TO 47
 47   CONTINUE
C     ...WHERE 47 IS ENDDO OF INNER MERGES LOOP....
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

C     ...WHEN IT FALLS THRU 47 CONTINUE, ONE MERGED BIN OF LMAX ITEMS IS
C     ...   FULL AND READY TO BE OUTPUT ONTO UNIT L3
      LCKPT = 56
      LUNOUT = LA

      IF(LAACTQ .EQ. 0) GO TO 48
        WRITE(LA,ERR=930)(BUFF(I,IOTMG),I=1,LMAX)
   48 CONTINUE

      IOTMG = I3+4
      IDMHO = 48
      LA=L3
      LAACTQ = IDMHO
      I3 = MOD(I3,2)+1
C     ...WHICH SWINGS THE GATE OF THE MERGING BIN TO USE ALTERNATE BIN
C     ...   SO MERGING WILL BE FILLING OTHER BIN WHILE WRITING FR IOTMG
      ILCNT = ILCNT+1
      IF (ILCNT .LT. MLNTH) GO TO 422
      IERROR = 52
      GO TO 900
C
   49 CONTINUE
C     ...COMES TO 49 WHEN LHS INPUT EOF ENCOUNTERED, OR COMPLETE LHS
C     ...   INPUT VECTOR OF LENGTH NLNTH WAS PROCESSED...
C     ...NEXT...FINISH OFF RHS INPUT VECTOR TO COMPLETE THIS MERGED
C     ...   VECTOR BY COPYING THE RMNDR OF RHS VECTOR
      M = MSAVE
      IF(M .GE. LMAX) GO TO 55
      M = M+1
C
   51 CONTINUE
      DO  54  N = M,LMAX
C       ... THE FIRST TIME THRU, IT CONTINUES FILLING THE MERGING BIN 
C       ... FROM THE RHS WORKING BIN I2+2.
        BUFF(N,I3+4) = BUFF(L,I2+2)
        L = L + 1
        IF (L .GT. LMAX) GO TO 52
        IF(BUFF(L,I2+2) .NE. 0) GO TO 54
C       ...OTHERWISE, IT ENCOUNTERED TERMINATING ZERO OF RHS INPUT DATA
        LCKPT = 44
        LUNIN = L2

        READ(L2,ERR=920,END=57)(BUFF(I,INRHS),I=1,LMAX)

  831   PRINT  813, L2,LCKPT
        IERROR = 73
        GO TO 900
C
   52   CONTINUE
C       ...COMES TO 52 WHEN RHS WORKING BIN IS EXHAUSTED,
C       ...   SO SWITCH TO ALTERNATE RHS BIN...
        LCKPT = 57
        LUNIN = L2

        READ(L2,ERR=920,END=57)(BUFF(I,INRHS),I=1,LMAX)

   53   CONTINUE
        IDRHO = 53
        INRHS = I2 + 2
        I2 = MOD(I2,2) + 1
        L = 1
   54 CONTINUE
C     ...WHEN IT FALLS THRU 54 ENDDO, ONE MERGED BIN IS READY FOR WRS

   55 CONTINUE
      LCKPT = 58
      LUNOUT = LA

      IF(LAACTQ .EQ. 0) GO TO 56
        WRITE(LA,ERR=930)(BUFF(I,IOTMG),I=1,LMAX)
   56 CONTINUE

      IOTMG = I3 + 4
      IDMHO = 56
      LA = L3
      LAACTQ = IDMHO
      I3 = MOD(I3,2) + 1
      M = 1
      ILCNT = ILCNT + 1
      IF(ILCNT .LT. MLNTH) GO TO 51

C     ...OTHERWISE, THE END OF THIS MERGED VECTOR IS UPON US
      LCKPT = 561
      LUNOUT = LA
      IF(LAACTQ .EQ. 0) GO TO 562
        WRITE(LA,ERR=930)(BUFF(I,IOTMG),I=1,LMAX)
  562 CONTINUE

      LAACTQ = 0
C     ...DELAYED FOR COMPLETION OF WRS BEFORE SWITCHING TO OTHER L3 UNIT
      ILCNT = 0
      L5 = MOD(L5,2) + 1
      L3 = LOTUSV + L5 - 1
      L1 = LINUSV
      IF(LEOFQ) GO TO 51
C     ...IF NO MORE DATA ON L1 LHS INPUT, THEN  GO COPY FROM L2 RHS ONLY
C     ...OTHERWISE, GO TO 421 TO WORK W/ NEXT SET OF INPUT VECTORS
      GO TO 421
C     *     *     *     *     *     *     *     *     *     *     *
C
   57 CONTINUE
C     ...COMES TO 57 WHEN NO MORE DATA AT ALL ON RHS INPUT FILE L2.
C     ...THIS END WAS ENCOUNTERED DURG COPYING OUT OF RHS AFTER
C     ...PREVIOUSLY LHS FILE HAD FINISHED ON END OF INPUT VECTOR OR EOF
      IF(LEOFQ) GO TO 837
C     ...IF LHS HAD HIT EOF ALSO, THEN NO MORE DATA AT ALL.
C     ...CLEANUP AND PREPARE FOR NEXT PASS W/ DOUBLED VECTORS
C     ...OTHERWISE, LHS L1 FILE HAS LEFTOVERS TO BE COPIED OUT...
      LEOFQ = .TRUE.
      LCKPT = 61
      IF(N .NE. LMAX) THEN
        IERROR = 61
        GO TO 900
      ENDIF
      LUNOUT = LA

      IF(LAACTQ .EQ. 0) GO TO 838
        WRITE(LA,ERR=930)(BUFF(I,IOTMG),I=1,LMAX)
  838 CONTINUE

      IOTMG = I3 + 4
      IDMHO = 838
      LA=L3
      LAACTQ = IDMHO
      I3 = MOD(I3,2) + 1
      M=1
      ILCNT = 0
      L5 = MOD(L5,2) + 1
      L3 = LOTUSV + L5 - 1
      L = 1
      L1 = LINUSV
      I1 = 1
      K = 1
      GO TO 751
C
C     *     *     *     *     *     *     *     *     *     *     *
C
   50 CONTINUE
C     ...COMES TO 50 WHEN RHS INPUT EOF ENCOUNTERED OR COMPLETE RHS INPU
C     ...   VECTOR OF LENGTH NLNTH WAS PROCESSED...
C     ...NEXT... FINISH OFF LHS  BY COPYING RMNDR  OF LHS INPUT VECTOR
      M = MSAVE
      IF(M .GE. LMAX) GO TO 755
      M = M + 1
  751 CONTINUE
      DO  754  N = M,LMAX
C       ...THE FIRST TIME THRU CONTINUE FILLING MERGED BIN FROM LHS BIN
        BUFF(N,I3+4) = BUFF(K,I1)
        K = K + 1
        IF(K .GT. LMAX) GO TO 752

        IF(BUFF(K,I1) .NE. 0) GO TO 754
C       ...OTHERWISE, ENCOUNTERED TERMINATING ZERO IN LHS BIN...
        LCKPT = 74
        LUNIN = L1

        READ(L1,ERR=920,END=757)(BUFF(I,INLHS),I=1,LMAX)

  851   PRINT 813,L1,LCKPT
        IERROR = 74
        GO TO 900

  752   CONTINUE
        LCKPT = 75
        LUNIN = L1

        READ(L1,ERR=920,END=757)(BUFF(I,INLHS),I=1,LMAX)

  753   CONTINUE
        IDLHO = 753
        INLHS = I1
        I1 = MOD(I1,2)+1
        K = 1
  754 CONTINUE
C     ...WHEN IT FALLS THRU 754 ENDDO, ONE MERGED BIN IS READY FOR WRS

  755 CONTINUE
      LCKPT = 76
      LUNOUT = LA

      IF(LAACTQ .EQ. 0) GO TO 756
        WRITE(LA,ERR=930)(BUFF(I,IOTMG),I=1,LMAX)
  756 CONTINUE

      IOTMG = I3 + 4
      IDMHO = 756
      LA = L3
      LAACTQ = IDMHO
      I3 = MOD(I3,2) + 1
      M = 1
      ILCNT = ILCNT + 1
      IF(ILCNT .LT. MLNTH) GO TO 751
C     .....OTHERWISE, END OF THIS MERGED VECTOR IS UPON US,
C     ...   SO SWITCH TO OTHER OUTPUT UNIT AND PREPARE FOR NEW MERGED V
      LUNOUT = LA

      IF(LAACTQ .EQ. 0) GO TO 762
        WRITE(LA,ERR=930)(BUFF(I,IOTMG),I=1,LMAX)
  762 CONTINUE

      LAACTQ = 0
      ILCNT = 0
      L5 = MOD(L5,2) + 1
      L3 = LOTUSV + L5 - 1
      L1 = LINUSV
      IF(LEOFQ) GO TO 751
C     ....WHICH CONTINUES COPYING FROM LHS ONLY, SINCE RHS HIT EOF
C     ...OTHERWISE, LOOP TO BEGIN NEXT SET OF INPUT AND OUTPUT VECTORS
      GO TO 421
C     *     *     *     *     *     *     *     *     *     *     *
  757 CONTINUE
C
C     ...COMES HERE TO 757 WHEN NO MORE DATA AT ALL ON LHS INPUT FILE
C     ...   LHS END WAS ENCOUNTERED DURING COPYING OUT LHS AFTER
C     ...   PREVIOUSLY RHS HAD FINISHED ON END OF INPUT VECTOR OR EOF
      IF(LEOFQ) GO TO 837
C     ...IF THE RHS HAD HIT EOF ALSO, THEN NO MORE DATA AT ALL. CLEANUP
C     ...OTHERWISE, RHS HAS LEFTOVER BINS OF INFO TO BE COPIED OUT...
      LEOFQ = .TRUE.
      LCKPT = 77
      IF(N .NE. LMAX) THEN
        IERROR = 77
        GO TO 900
      ENDIF
      LUNOUT = LA

      IF(LAACTQ .EQ. 0) GO TO 758
        WRITE(LA,ERR=930)(BUFF(I,IOTMG),I=1,LMAX)
  758 CONTINUE

      IOTMG = I3 + 4
      IDMHO = 758
      LA = L3
      LAACTQ = IDMHO
      I3 = MOD(I3,2) + 1
      M = 1
      ILCNT = 0
      L5 = MOD(L5,2) + 1
      L3 = LOTUSV + L5 - 1
      L = 1
      L1 = LINUSV
      I1 = 1
      K = 1
      GO TO 51
C     ...WHICH TRANSFERS TO 51 TO COPY BINS FROM RHS UNIT L2
C     *     *     *     *     *     *     *     *     *     *     *
C
C     ONE MERGES PASS FINISHED.  TEST FOR MLNTH .GE. NTOTRECINP
C
  837 CONTINUE
      IF(N .LT. LMAX) BUFF(N+1,I3+4) = 0
      LCKPT=59
      LUNOUT = LA

      IF(LAACTQ .EQ. 0) GO TO 58
        WRITE(LA,ERR=930)(BUFF(I,IOTMG),I=1,LMAX)
   58 CONTINUE

      IDMHO = 58
      IOTMG = I3+4
      LA = L3
      LAACTQ = IDMHO
      ILCNT = ILCNT + 1
      LUNOUT = LA

      WRITE(LA,ERR=930)(BUFF(I,IOTMG),I=1,LMAX)

   59 CONTINUE
      ITAPE= 60
      LAACTQ = 0
      IF(L3 .EQ. 62) ITAPE = 62
      ENDFILE L3
      REWIND L3
      IF (MLNTH .GE. NTOTRECINP) GO TO 999    	!... * * * RETURN * * *
C     ...WHICH IS FINAL NORMAL EXIT OUT OF MERGES
C
      L5 = MOD(L5,2)+1
      L3 = LOTUSV + L5 - 1
      LCKPT=60
      LA = L3
      ENDFILE L3
      REWIND L3
      L1 = LOTUSV
      LOTUSV = LINUSV
      LINUSV = L1
C     ...WHICH INTERCHANGED INPUT AND OUTPUT UNITS...
      GO TO 40
C
C     *     *     *     *     *     *     *     *     *     *     *
C
  900 CONTINUE
C     ... COMES HERE INSTEAD OF STOPPING WITHIN THE CODE, LIKE IT USED 
      WRITE(6,905) IERROR
  905 FORMAT(1H ,'MERGES:* * * FAILED WITH ERROR CODE=',I5,' * * * *')
C     ... CONVERT INTEGER-VALUE IN IERROR INTO A CHAR STRING:C8DIGITS
      C8DIGITS(1:) = ' '

      WRITE(C8DIGITS(1:8),907) IERROR
  907 FORMAT(I5)
C     STOP C8DIGITS
      STOP 
C
C
  910 CONTINUE
      PRINT 915,LCKPT
  915 FORMAT (1H ,'MERGES: FAILED ON EMPTY LIST FILE. LCKPT=',I4)
      IERROR = 50
      GO TO 900
C
  920 CONTINUE
      PRINT 925,LUNIN,LCKPT
  925 FORMAT(1H , 'MERGES: FAILED ON READ PARITY ERROR ON UNIT=',I3,
     1            ';  LCKPT=', I4)
      IERROR = 51
      GO TO 900
C
  930 CONTINUE
      PRINT 935,LUNOUT,LCKPT
  935 FORMAT(1H , 'MERGES: FAILED ON WRITE PARITY ERROR ON UNIT=',I3,
     1            ';  LCKPT=', I4)
      IERROR = 51
      GO TO 900
C
  999 CONTINUE
ckumar
      print*,'Exiting merges'
ckumar
      RETURN
      END
