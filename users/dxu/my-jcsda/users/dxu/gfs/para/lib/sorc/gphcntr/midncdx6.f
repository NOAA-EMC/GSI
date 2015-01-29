       SUBROUTINE MIDNCDX6(I8RAST,KDIMRAS,IFIRSTQ,ILASTQ,LDUBLQ,IRETN)
C                                                        2-APR-1996/DSS
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    MIDNCDX6    ENCODE NMC 6-BIT RLE -- MID-LVL SECTION
C   PRGMMR: KRISHNA KUMAR     ORG: W/NP12    DATE: 1999-07-01
C
C ABSTRACT: TO ENCODE A GIVEN PURE UNPACKED RASTER (RAW) SCANLINE
C   INTO NMC 6-BIT RLE CODE IN EXTENDED-TO-BYTE FORMAT.  THIS SUBR
C   IS A MID-LEVEL ROUTINE WHICH CALLS ON SUBR PURE_X6B() TO PERFORM
C   THE ACTUAL PIXEL-BY-PIXEL ENCODE. THIS SUBR IS CALLED FROM
C   SUBR OTRNCDX6() WHICH IS THE HIGHER LEVEL SECTION.
C   RESULTS WILL BE BUFFERED OUT VIA SUBR BUFO1920()
C   IN 1920-BYTE RECORDS
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  ORIGINAL AUTHOR: DAVID SHIMOMURA
C   90-06-04  SHIMOMURA --     TO INSERT SOME CHECKOUT PRINTS, LOOKING FOR 
C                              LOGIC ERROR WHICH PLACED END-OF-SCANLINE 
C                              ONE BYTE BEFORE WHERE IT SHUD.  FIXED NOW.
C   92-05-10  SHIMOMURA --     TO ADD OPTION TO REPEAT EACH OUTPUT SCANLINE
C                              WHICH WOULD STRETCH MAP IN THE VERTICAL.
C   94-04-19  SHIMOMURA --     REPROGRAMMED FROM INTERGRAPH VAX FORTRAN 
C                              INTERGRAPH UNIX FORTRAN
C   96-04-02  SHIMOMURA --     REPROGRAMMED FROM INTERGRAPH UNIX FORTRAN
C                              TO CRAY FORTRAN.
C 1999-07-01  KRISHNA KUMAR -- CONVERTED THIS CODE FROM CRAY TO 
C                              IBM RS/6000
C                              
C USAGE:    CALL MIDNCDX6(I8RAST,KDIMRAS,IFIRSTQ,ILASTQ,LDUBLQ,IRETN)
C
C   INPUT ARGUMENT LIST:
C
C     INT  I8RAST(KDIMRAS)     	! (2)=> UNCOMPRESSED RASTER
C     INT  KDIMRAS 		! (3)=> NO. I*8 WORDS IN I8RAST
C     INT  IFIRSTQ     		! (4)=><= USER CALLS WITH=1 
C                ... TO REQUEST INITIALIZE COUNTERS FOR A NEW MAP;
C                ... THIS MID_PAKRAST RESETS TO =0 AFTER INITIALIZATION;
C                ... AND AFTER END-OF-MAP IS OUTPUT, SETS TO =1;
C
C     INT  ILASTQ      		! (5)=> USER CALLS WITH 
C                ... ILASTQ=0  FOR NOT END OF MAP YET;
C                ... USER MUST SET ILASTQ=1  WHEN SHE WANTS TO CLOSE OUT 
C                ...    THE RUNNING MAP WITH THE END-OF-MAP FLAG BYTE
C                ...    AND FLUSH OUT THE LAST PARTIAL BUFFER.
C                ... WHEN ILASTQ=1, THIS MID_PAKRAST IGNORES ANY
C                ...    GIVEN I8RAST DATA, AND GOES DIRECTLY TO 
C                ...    PUTTING END-OF-MAP FLAG OUT.
C     LOGICAL LDUBLQ                ! (6)=> USER CALLS WITH
C                ... LDUBLQ = .TRUE.  IF SHE WANTS DUPLICATE SCANLINE
C                                          OUTPUT;
C                ... LDUBLQ = .FALSE. IF SHE WANTS NO DUPLICATING,
C                                       WHICH MEANS NO VERTICAL STRETCH.
C
C   OUTPUT ARGUMENT LIST:
C
C     INT  IRETN    	! (8)<= RETURN CODE
C                 ... =0  NORMAL RETURN
C                 ... =1  AFTER END-OF-MAP FLAGGED
C                 ... =2  ERROR DURING WRITE
C
C   OUTPUT FILES:
C     FT06F001 - INCLUDE IF ANY PRINTOUT
C
C REMARKS: 
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
C
C
C
C      . . . . . . . . . .   A R G S    . . . . . . . . . . . . . . . .
C
       INTEGER    I8RAST(KDIMRAS)     	! (2)=> UNCOMPRESSED RASTER
C ...       INTEGER    KDIMRAS 		! (3)=> NO. I*8 WORDS IN I8RAST
       INTEGER    IFIRSTQ      		! (4)=><= USER CALLS WITH=1 
C                ... TO REQUEST INITIALIZE COUNTERS FOR A NEW MAP;
C                ... THIS MID_PAKRAST RESETS TO =0 AFTER INITIALIZATION;
C                ... AND AFTER END-OF-MAP IS OUTPUT, SETS TO =1 .
       INTEGER    ILASTQ      		! (5)=> USER CALLS WITH 
C                ... ILASTQ=0  FOR NOT END OF MAP YET;
C                ... USER MUST SET ILASTQ=1  WHEN SHE WANTS TO CLOSE OUT 
C                ...    THE RUNNING MAP WITH THE END-OF-MAP FLAG BYTE
C                ...    AND FLUSH OUT THE LAST PARTIAL BUFFER.
C                ... WHEN ILASTQ=1, THIS MID_PAKRAST IGNORES ANY
C                ...    GIVEN I8RAST DATA, AND GOES DIRECTLY TO 
C                ...    PUTTING END-OF-MAP FLAG OUT.
       LOGICAL    LDUBLQ                ! (6)=> USER CALLS WITH
C                ... LDUBLQ = .TRUE.  IF SHE WANTS DUPLICATE SCANLINE
C                                          OUTPUT;
C                ... LDUBLQ = .FALSE. IF SHE WANTS NO DUPLICATING,
C                                       WHICH MEANS NO VERTICAL STRETCH.
C
       INTEGER    IRETN    		! (8)<= RETURN CODE
C                                            ... =0  NORMAL RETURN
C                                            ... =1  AFTER E-O-M FLAGGED
C                                            ... =2  ERROR DURING WRITE
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C 
C
       integer       jlinewrk(131)
       character*1   CLINEWRK(1048)
       equivalence  (jlinewrk(1),clinewrk(1))
C      ... MAX SCANLINE IN BEDIENT'S FAX SYSTEM IS 4192 BITS
C      ...   =1048 DIBYTES (WHERE A 'DIBYTE' = 4 BITS).
C      ... IF WE HAD 1048 "AS-IS" DIBYTES ENCODED IN EXTENDED
C      ...   6-BIT FAX CODE, THEN WE NEED 1048 BYTES.
C
C
       INTEGER   MAPSTART
       DATA      MAPSTART      / X'3F3F3F3F00000000' /

       INTEGER   KSTRIPSTART
       DATA      KSTRIPSTART    / X'3F3F3F3E30000000' /
C
       INTEGER      KEOM
       DATA         KEOM    / X'33' /
C      ...                 (0011 0011)BINARY IS END-OF-MAP
C
       integer  iby
       integer  icmd_bfow
       integer  iret_bf
C
       CHARACTER*1  NULL
C
C      . . .   S T A R T   . . .
C
       IRETN = 0
       NULL = CHAR(0)

       IF(LDUBLQ) THEN
         IDOUBLE = 2
       ELSE
         IDOUBLE = 1
       ENDIF

C      ---------------------------------------------------------------
       IF(IFIRSTQ .EQ. 1) THEN
C        ... INITIALIZE FOR A NEW MAP

C        ... make an empty IFID of 64 chars ...
         DO  I = 1,8
           jlinewrk(i) = 0   		!... 8 wrds * 8 bytes/wrd=64byt
         ENDDO
         jlinewrk(1) = MAPSTART

C        ... to force the first X6B dibyte data byte = X'30' endline,
         jlinewrk(9) = 0
         clinewrk(65) = char(48) 	!... =X'30'
         iby = 65
         icmd_bfow = 0  		!... open & init

         call BUFO1920(clinewrk,iby,icmd_bfow,iret_bf)

         if(iret_bf .NE. 0) then
           go to 900
         endif
C
         IFIRSTQ = 0
         go to 999

C      ----------------------------------------------------------------
       else IF(IFIRSTQ .EQ.-1) THEN
C        ... INITIALIZE FOR A strip-title block header
C        ...    I AM ASSUMING THE LAST PARTIAL BUFFER OF THE MAIN BODY
C        ...    (CONTAINING EOM FLAG) HAS ALREADY BEEN WRITTEN OUT

         jlinewrk(1) = KSTRIPSTART
         iby = 5
         IDOUBLE = 1
         IFIRSTQ = 0
         GO TO 600

       ENDIF
C      ----------------------------------------------------------------
C
       IF(ILASTQ .EQ. 1) THEN
C        ... TO PUT THE END-OF-MAP BYTE INTO OUTPUT BUFFER
         CLINEWRK(1) = CHAR(KEOM)
         IBY = 1
         IDOUBLE = 1
         GO TO 600   		!... jump out to CLEANUP.
       ENDIF
C 
       call pure_x6b(I8RAST,kdimras,CLINEWRK,iby)
C
C
  600  CONTINUE
C      ... COMES HERE TO MOVE CODED SCANLINE 
C      ...   FROM C*1 CLINEWRK(1) to (IBY) 
C      ...   INTO  OUTPUT BUFFER

       DO 633 IDITTO = 1,IDOUBLE

         icmd_bfow = 1  		!... buffer-out the data

         call BUFO1920(clinewrk,iby,icmd_bfow,iret_bf)

         if(iret_bf .NE. 0) then
           go to 900
         endif
C        ... ONE SCANLINE IN CLINEWRK HAS BEEN buffered out
  633  CONTINUE
C      ... DUPLICATE SCANLINE HAS ALSO BEEN MOVED, IF OPTION SET
C
C
       IBY = 0
       IF(ILASTQ .EQ. 0) GO TO 999
C      ... OTHERWISE, END-OF-MAP WAS buffered out,
C      ...   SO CLEAN UP.

         icmd_bfow = 2  		!... flush any partial buffer

         call BUFO1920(clinewrk,iby,icmd_bfow,iret_bf)

         if(iret_bf .eq. -1) then   	!... normal retn fr flush
           go to 634
         else if(iret_bf .NE. 0) then
           go to 900
         endif

C
  634  continue
C      WRITE(6,635)
C 635  FORMAT(1H ,'MIDNCDX6:CLEANED_UP AT END-OF-MAP')
       IFIRSTQ = 1
C      ... SO IT WON'T ADD ON TO BUFFER ACCIDENTALLY
       IRETN = 1
       GO TO 999
C
C
  900  CONTINUE
C      ... COMES HERE IF ERR DURING WRITE
       IRETN = 2
       GO TO 999
C
C
  999  CONTINUE
       RETURN
       END
