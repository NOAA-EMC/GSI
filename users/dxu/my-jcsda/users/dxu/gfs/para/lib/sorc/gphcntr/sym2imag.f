C
       SUBROUTINE SYM2IMAG(IPL,JPL,IROTRA,NEWPXLHGT,NEWPXLWID,LERASE,
     1                     IMAGE,MAXIWORD,MAXJSLINE)
C                                                13-MAR-1996/DSS
C      ... MODIFIED TO ADD IMAGE TO CALL SEQ INSTEAD OF DEFINING
C      ...    AS COMMON BLOCK
C                      TO BE ABLE TO WORK WITH VARIOUS DIMENSIONS,
C                      MAYBE THE IMAGE SHOULD BE ONE-DIMENSIONED
C                      AND I SHOULD COMPUTE WHICH WORD BY KNOWING
C                      MAXIWORD,MAXJSLINE
C                      BUT FOR SIMPLE DEMO, KEEP IT SIMPLE
C       . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C ...        INTEGER    MAXIWORD
C ...        PARAMETER (MAXIWORD=47)
C ...        INTEGER    MAXJSLINE
C ...        PARAMETER (MAXJSLINE=3240)

C ...        COMMON  /PICTURE/IMAGE
        INTEGER          IMAGE(MAXIWORD,MAXJSLINE)
C       . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
        integer    kwrdszbyts
        parameter (kwrdszbyts=8)   		!... CRAY long int
        integer    kwrdszbits
        parameter (kwrdszbits=8*kwrdszbyts)    	!... CRAY 64-bit wrd

       INTEGER   IPL,JPL
       INTEGER   IROTRA(NEWPXLHGT)
       INTEGER   NEWPXLWID
       LOGICAL   LERASE    			!... <= .T. erase under 

       LOGICAL   LNEED2
       INTEGER   IBGWORK(2)
       INTEGER   MSKLEFT
       INTEGER   MSKRIGHT
       INTEGER   NOTMSKLEFT
       INTEGER   NOTMSKRIGHT

       IPLM1 = IPL - 1
       ITHWORD = IPLM1 / kwrdszbits   		!... [0,NWORDS]
       ITHWORD = ITHWORD + 1
       ITHBITINWORD = MOD(IPLM1,kwrdszbits)   	!... BIT [0:63]
       IF((ITHWORD .LT. 1) .OR.
     1    (ITHWORD .GT. MAXIWORD)) THEN
          GO TO 999
C         ... FOR KEEP IT SIMPLE STAGE, JUMP OUT IF OUT OF BOUNDS ...
       ENDIF
       LNEED2 = .FALSE.
       IF((ITHBITINWORD + NEWPXLWID) .GT. kwrdszbits) THEN
C        ... WE WILL NEED ADJACENT WORD ON THE SAME SCANLINE ...
         LNEED2 = .TRUE.
         IF((ITHWORD + 1) .GT. MAXIWORD) THEN
           GO TO 999
C          ... FOR KEEP IT SIMPLE STAGE, JUMP OUT IF AT EDGE ...
         ENDIF
       ENDIF
       JCURR = JPL - 1
       DO LINSYM = 1,NEWPXLHGT
         JCURR = JCURR + 1
         IBGWORK(1) = 0
         IBGWORK(2) = 0
         IF((JCURR .GT. 0) .AND.
     1      (JCURR .LE. MAXJSLINE)) THEN
           IBGWORK(1) = IMAGE(ITHWORD,JCURR)
           IF(LNEED2) THEN
             IBGWORK(2) = IMAGE(ITHWORD+1,JCURR)
           ENDIF
        
         
           LINEDEF = IROTRA(LINSYM)
           IRIGHT = 0
           MSKSYM = MASK(NEWPXLWID)
           LINEDEF = IAND(LINEDEF,MSKSYM)        
           ILEFT = ISHFT(LINEDEF,-ITHBITINWORD)
           MSKLEFT = ISHFT(MSKSYM,-ITHBITINWORD)
           NBITOFF = (ITHBITINWORD + NEWPXLWID) - kwrdszbits
           IF(NBITOFF .GT. 0) THEN
             LEFSHIF = NEWPXLWID - NBITOFF
             IF((LEFSHIF .GT. 0) .AND.
     1          (LEFSHIF .LT. NEWPXLWID)) THEN
               IRIGHT = ISHFT(LINEDEF,LEFSHIF)
               MSKRIGHT = ISHFT(MSKSYM,LEFSHIF)
             ELSE
               IRIGHT = 0
               MSKRIGHT = 0
             ENDIF
           ENDIF

c           IF(LERASE) THEN
c             NOTMSKLEFT  = COMPL(MSKLEFT)
c             NOTMSKRIGHT = COMPL(MSKRIGHT)
c
c             IBGWORK(1) = IAND(NOTMSKLEFT,IBGWORK(1))
c             IBGWORK(2) = IAND(NOTMSKRIGHT,IBGWORK(2))
c           ENDIF

           IBGWORK(1) = IOR(ILEFT,IBGWORK(1))
           IBGWORK(2) = IOR(IRIGHT,IBGWORK(2))
           IMAGE(ITHWORD,JCURR) = IBGWORK(1)
           IF(NBITOFF .GT. 0) THEN
             IMAGE(ITHWORD+1,JCURR) = IBGWORK(2)
           ENDIF
         ENDIF
       ENDDO
  999  CONTINUE
       RETURN
       END
C
