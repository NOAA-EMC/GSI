       SUBROUTINE bufo1920(CDATA,NCHARS,ICMD,IERR)
C                                                29-MAY-1996/DSS
C      ... IN ORDER TO feed data into reblkfx4() which reads 1920-byte
C      ... records instead of 512-byte records, 
C      ... I copied ~/ncod/bufow512.f into ~/ncod/v4/bufo1920.f
C      ...   in order to change the output record size.
C
C                                                 8-Apr-1996/dss
C      ... copied ~/cra/bg/bufowx6b.f  into ~/ncod/bufow512.f
C      ...    in order to replace direct-access write with
C      ...    a simpler unformatted write; to see if the simpler write
C      ...    will be O.K. in the CRAY world.
C      ...    Application is the same -- to write a temporary .x6b file
C
C      ... A STRIPPED-DOWN BUFFER-OUTER          14-JUN-1995/DSS
C      ... TO BUFFER-OUT CHARACTER*1 ARRAYS OF VRBL LENGTH
C      ... IN FIXED RECORD SIZE 1920-BYTE BUFFERS
C
C      GIVEN:
C      (1) CHARACTER*1  CDATA(NCHARS) ... GIVEN CHARACTER-STRING DATA
C                                   TO BE BUFFERED OUT
C      (2) NCHARS ... NUMBER OF BYTES OF DATA IN CDATA.
C      (3) ICMD   ... WHAT TO DO;
C              =0     INITIALIZE 
C                       (This does OPEN the X6B output file.) 
C                       IF YOU GAVE ME SOME DATA, THEN
C                         I WILL BUFFER-OUT THAT.
C              =1     BUFFER-OUT THE GIVEN CDATA.
C              =2     FLUSH THE PARTIALLY FILLED BUFFER.
C              =999   FLUSH THE PARTIALLY FILLED BUFFER
C                     AND CLOSE THE OUTPUT FILE
C
C       RETURN CODE:
C       (6) IERR = 0  NORMAL RETURN
C                = -1;  normal return from FLUSH
C                = NON-ZERO  ERROR.
C
C
C       SEE COMMON /arbfox6b/
C
C       CALLS ON BF1920WR() ... TO DO THE ACTUAL WRITE 
C                XMOVEX() ... TO MOVE BYTES FROM GIVEN CDATA
C                                   INTO THE OUTPUT BUFFER
C                              (XMOVEX IS FOUND IN W3-LIBRARY)
C
C                INTEGER FUNCTION LASTCH()   
C       *     *     *     *     *     *     *     *     *     *     *
C
       INTEGER      NBYTPWRD
       PARAMETER   (NBYTPWRD=8)          !... FOR CRAY
       INTEGER      MXBFSZ_BYT
       PARAMETER   (MXBFSZ_BYT=1920)
       INTEGER      MXBFSZ_WRD
       PARAMETER   (MXBFSZ_WRD=MXBFSZ_BYT/NBYTPWRD)
C
       EXTERNAL     LASTCH
       INTEGER      LASTCH

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
       COMMON /arbfox6b/ LUNX6B,LUX6BOPNQ,NBUFX6B,IPTR_X6BF
C
       INTEGER         LUNX6B
       LOGICAL         LUX6BOPNQ
       INTEGER         NBUFX6B
       INTEGER         IPTR_X6BF
C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
       CHARACTER*1    CDATA(NCHARS)
       INTEGER        ICMD
       INTEGER        IERR
C
       INTEGER        IOUTBUF(MXBFSZ_WRD)
       CHARACTER*1    COUTBUF(MXBFSZ_BYT)
       EQUIVALENCE   (IOUTBUF,COUTBUF)
C
       INTEGER        IOSOPEN

       SAVE           IOUTBUF
C
C      . . .  S T A R T   . . .
C
       IERR = 0
C
       IF(ICMD .EQ. 0) THEN
C        ... INITIALIZE ...
         NBUFX6B = 0
         IPTR_X6BF = 0

         OPEN(LUNX6B,FORM='UNFORMATTED',STATUS='UNKNOWN',
     1        IOSTAT=IOSOPEN,ERR=900)

         LUX6BOPNQ = .TRUE.
         rewind   LUNX6B 
C
         DO  I = 1,MXBFSZ_WRD
            IOUTBUF(I) = 0
         ENDDO
C
C
         IF(NCHARS .GT. 0) GO TO 210
C        ... TO OUTPUT GIVEN CDATA EVEN ON INITIALIZATION CALL ...
C        ... OTHERWISE, END OF INITIALIZATION CALL
         GO TO 999
C
       ELSE IF(ICMD .EQ. 2) THEN
C        ... FLUSH ANY PARTIAL BUFFER ...

         CALL BF1920WR(IOUTBUF,IRET_WR)
         IF(IRET_WR .NE. 0) THEN
           IERR = IRET_WR
         ELSE
C          ... normal return from BF1920WR for FLUSH ...
           IERR = -1
         ENDIF
C        WRITE(6,FMT='(1H ,''bufo1920::BF1920WR: ON FLUSH, '',
C    1                     ''PTR_X6BF='',I6,
C    2                /1H ,7X,''IRET_WR='', I4,''; IERR='',I4)')
C    A           IPTR_X6BF,IRET_WR,IERR
         GO TO 999

       ELSE IF(ICMD .EQ. 999) THEN
C        ... FLUSH ANY PARTIAL BUFFER ...

         CALL BF1920WR(IOUTBUF,IRET_WR)
         IF(IRET_WR .NE. 0) THEN
           IERR = IRET_WR
           GO TO 999
         ENDIF
C        ... OTHERWISE, IRET_WR = 0;
         IERR = -1
         CLOSE(UNIT=LUNX6B)

         GO TO 999
       ENDIF
C
C      ... OTHERWISE, WE WILL TRY TO BUFFER-OUT CDATA IF THERE IS ANY
C
C
  210  CONTINUE
C
C      ... COMES HERE TO BUFFER-OUT (CDATA(I),I=1:NCHARS)
C
       IF(NCHARS .LE. 0) GO TO 999
C
C      ... WILL (CDATA(I),I=1:NCHARS) FIT WITHIN THE CURRENT PARTIAL 
C      ...   BUFFER?
       IPTR_CDA = 0
       NREMAIN = NCHARS
  211  CONTINUE
         IF((IPTR_X6BF+NREMAIN) .LE. MXBFSZ_BYT) THEN
C          ... THIS IS CASE OF SIMPLY DROP INTO OUTPUT BUFFER
           M1 = IPTR_X6BF + 1
           M2 = IPTR_X6BF + NREMAIN
           NBMOVING = NREMAIN
           N1 = IPTR_CDA + 1
           N2 = IPTR_CDA + NBMOVING

           CALL XMOVEX(COUTBUF(M1),CDATA(N1),NBMOVING)

           NREMAIN = NREMAIN - NBMOVING
           IPTR_CDA = N2
           IPTR_X6BF = M2

           GO TO 999

         ELSE 
C          ... CASE WHERE GIVEN STRING WILL OVERFLOW PARTIAL BUFFER
           NEMPTY_BYT = MXBFSZ_BYT - IPTR_X6BF
           IF(NEMPTY_BYT .LE. 0) THEN
C            ... WRITE THE FULL BUFFER BEFORE DOING ANYTHING,

             CALL BF1920WR(IOUTBUF,IRET_WR)
             IF(IRET_WR .NE. 0) THEN
               IERR = IRET_WR
               GO TO 999
             ENDIF


           ELSE
C            ... COMES HERE WITH NEMPTY_BYT IN RANGE FROM [1:1920]
C            ... AND (IPTR_X6BF+NREMAIN) .GT. 1920
             M1 = IPTR_X6BF + 1
             M2 = MXBFSZ_BYT
             NBMOVING = M2 - M1 + 1
             N1 = IPTR_CDA + 1
             N2 = IPTR_CDA + NBMOVING

             CALL XMOVEX(COUTBUF(M1),CDATA(N1),NBMOVING)

C            ... THIS FILLS CURRENT BUFFER TO BRIM, SO FLUSH IT OUT
             IPTR_CDA = N2
             IPTR_X6BF = M2
             NREMAIN = NREMAIN - NBMOVING

             CALL BF1920WR(IOUTBUF,IRET_WR)
             IF(IRET_WR .NE. 0) THEN
               IERR = IRET_WR
               GO TO 999
             ENDIF

           ENDIF             
         ENDIF
       GO TO 211
C
  900  CONTINUE
C      ... COMES HERE ON FAILURE TO OPEN .X6B OUTPUT FILE ...

       WRITE(6,905)LUNX6B,IOSOPEN
  905  FORMAT(1H ,'bufo1920: FAILED TO OPEN TEMPORARY OUTPUT FILE FOR',
     1            ' X6B DATA;'
     2       /1H ,'     UNIT=',I5,';   IOSTAT=',I8)
       IERR = 9
       GO TO 999
 
  999  CONTINUE
       RETURN
       END
       SUBROUTINE BF1920WR(IOUTBUF,IRET_WR)
C      ... changed from 512-byte to 1920-byte rec siz 29-May-1996/dss
C      ... CALLED ONLY FROM bufo1920()              14-JUN-1995/DSS
C      ... TO WRITE, UNFORMATTED, THE BUFFER TO OUTPUT UNIT=LUNX6B
C      ... THIS IS A SUBSTITUTE FOR ASSIGNED GO TO 600
C      ... WHICH THE CRAY COMPILER OBJECTS TO.
C       *     *     *     *     *     *     *     *     *     *     *
C
       INTEGER      NBYTPWRD
       PARAMETER   (NBYTPWRD=8)          !... FOR cray
       INTEGER      MXBFSZ_BYT
       PARAMETER   (MXBFSZ_BYT=1920)
       INTEGER      MXBFSZ_WRD
       PARAMETER   (MXBFSZ_WRD=MXBFSZ_BYT/NBYTPWRD)
C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
       COMMON /arbfox6b/ LUNX6B,LUX6BOPNQ,NBUFX6B,IPTR_X6BF
C
       INTEGER         LUNX6B
       LOGICAL         LUX6BOPNQ
       INTEGER         NBUFX6B
       INTEGER         IPTR_X6BF
C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
       INTEGER   IOUTBUF(MXBFSZ_WRD)
       INTEGER   IRET_WR
C
C
       IRET_WR = 0
       IF (.NOT. LUX6BOPNQ) GO TO 930
       IF (IPTR_X6BF .LE. 0) GO TO 666
       IF (IPTR_X6BF .GE. MXBFSZ_BYT) GO TO 650
C      ... OTHERWISE, WE HAVE A PARTIALLY FULL IOUTBUF TO WRITE;
C      ... ZERO OUT TO RECORD BNDRY ...
C
  650  CONTINUE
C
       irecno = NBUFX6B + 1

       WRITE(LUNX6B,ERR=920) (IOUTBUF(I),I=1,MXBFSZ_WRD)
C
       NBUFX6B = NBUFX6B + 1
C
  666  CONTINUE
C
       DO  I = 1,MXBFSZ_WRD
         IOUTBUF(I) = 0
       ENDDO
C
       IPTR_X6BF = 0
C
       GO TO 999
C
C
  920  CONTINUE
       WRITE(6,925) LUNX6B,NBUFX6B
  925  FORMAT(1H ,'BF1920WR:PARITY ERROR WRITING UNIT=',I3,
     1            '  NBUFX6B=',I4)
C
       IRET_WR = 2
       GO TO 999
C
  930  CONTINUE
       WRITE(6,935) LUNX6B
  935  FORMAT(1H ,'BF1920WR:ERROR. CANNOT WRITE. UNIT =',I3,
     1            '  NOT OPEN')
       IRET_WR = 3
       GO TO 999

  999  CONTINUE
       RETURN
       END
