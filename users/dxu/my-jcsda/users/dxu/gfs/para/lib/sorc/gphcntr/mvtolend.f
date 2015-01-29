      SUBROUTINE MVTOLEND(LZERO_TERM55QQ,LEND_55QQ,LEOF_55QQ,
     1                    NREC_TRASHED,IRET_MVTO)
C                                                       18-DEC-1995/DSS
C     TO REPOSITION INPUT TAPE-55 FILE POINTER SO THAT THE POINTER WILL 
C     BE BEYOND THE LOGICAL END-OF-FILE ( "LEND" ) RECORD.
C 
C     BY READING THE INPUT FILE from UNIT=55 INTO VOID UNTIL THE "LEND"
C     RECORD IS READ (WHICH IS THE NORMALLY EXPECTED EXIT FROM THIS 
C     SUBR MVTOLEND(); 
C     INPUT FILE IS PRESUMED TO BE UNIT=55; 
C
C     A LIMITED-USE SUBR TO BE CALLED ONLY FROM SUBR MERGES(), ONLY 
C     AFTER MERGES() CALLED CSORT(); AND THE RETURN CODE FROM CSORT()
C     INDICATES THE CASE IN WHICH A ZERO-TERMINATOR WORD WAS 
C     ENCOUNTERED IN CSORT();
C     TO ENSURE THAT THE TAPE-55 IS REPOSITIONED FOR READING THE NEXT
C     PRODUCT'S TAPE-55.
C
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
C     ...   . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

      INTEGER      LMAX
      PARAMETER   (LMAX=1024)

      INTEGER      L0
      PARAMETER   (L0=55)   		!... INPUT UNIT=L0=55

      LOGICAL      LZERO_TERM55QQ  	! <= CSORT terminated on word=0 
      LOGICAL      LEND_55QQ            ! <==> "LEND" was found 
      LOGICAL      LEOF_55QQ        	! <==> Hit phys E-O-F 
      INTEGER      IRET_MVTO    	!   =>  RETURN CODE

      INTEGER      IVOID(LMAX)


      INTEGER      MSKSORK
      DATA         MSKSORK    / X'FFFFFFFF' /

      INTEGER      MINUS7
      DATA         MINUS7     / X'FFFFFFF9' /

      INTEGER      LONGWORD1
      INTEGER      JSORK


      SAVE

C     . . . . . .   S T A R T   . . . . . . . . . . . . . . . . . .

      IRET_MVTO = 0
      IF(LEOF_55QQ) THEN
        GO TO 999
      ELSE IF(LEND_55QQ) THEN
        GO TO 999
      ENDIF
C ...      LZERO_TERM55QQ
      NREC_TRASHED = 0 
  200 CONTINUE
      READ(L0,ERR=920,END=12) (IVOID(I),I=1,LMAX)
      NREC_TRASHED = NREC_TRASHED + 1
C     ... TO TEST FOR "LEND" ...
      LONGWORD1 = IVOID(1)
      JSORK = IAND((ISHFT(LONGWORD1,-32)),MSKSORK)
      IF(JSORK .EQ. MINUS7) THEN
C         ... THIS IS A LOGICAL E-O-F RECORD ON TAPE55
          LEND_55QQ = .TRUE.
          GO TO 13
      ENDIF
C     ... OTHERWISE, THIS RECORD IS A DATA RECORD, WHICH EXISTS IN THE
C     ...    BADLY FORMATTED TAPE-55 BEYOND THE ZERO-TERMINATOR;
C     ...    AND I WANT TO SKIP THIS RECORD
      GO TO 200

   12 CONTINUE
C     ... COMES HERE AFTER HITTING PHYSICAL E-O-F ...
C     ...   actually this is abnormal;  I should find LEOF before EOF;
      LEOF_55QQ = .TRUE.       	! => Hit phys E-O-F
      IRET_MVTO = -1
      GO TO 999

   13 CONTINUE
C     ... I HAVE JUST FOUND THE "LEND" RECORD WHICH I WAS LOOKING FOR
C     ... ANY OTHER CLEANUP TO DO???
      LEND_55QQ = .TRUE.            ! => found "LEND"
      IRET_MVTO = 1
 
      GO TO 999

  920 CONTINUE
      IRET_MVTO = -2
      WRITE(6,925)L0
  925 FORMAT(1H ,'MVTOLEND:FAILED ON PARITY ERROR WHILE SCANNING ',
     1           'UNIT=',I5,
     2      /1H ,7X,'  WHILE LOOKING FOR "LEND" RECORD')
      GO TO 999

  999 CONTINUE
      RETURN
      END
