       subroutine GENANMSK(npixelon,iandmaskra,idimen_wrds)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    GENANMSK    MAKE AN AND-MASK ARRAY WITH NPIXELS ON
C   PRGMMR: KRISHNA KUMAR       ORG: W/NP12    DATE: 1999-07-01
C
C ABSTRACT: TO SET A STRING OF BITS "ON" IN THE INT ARRAY: 
C   IANDMASKRA(idimen_wrds) STARTING FROM THE HIGH-ORDER END OF
C   THE FIRST WORD OF IANDMASKRA AND CONTINUING UNTIL THE SPECIFIED
C   BIT-COUNT HAS BEEN SATISFIED;    USED IN SUBR RAS2X6B().
C
C PROGRAM HISTORY LOG:
C   96-04-22  ORIGINAL AUTHOR(S)'S NAME: DAVID SHIMOMURA
C 1999-07-01  KRISHNA KUMAR    CONVERTED THIS CODE TO IBM RS/6000
C
C USAGE:    CALL GENANMSK(npixelon,iandmaskra,idimen_wrds)
C   INPUT ARGUMENT LIST:
C     NPIXELON - COUNT OF BITS TO SET "ON"
C
C   OUTPUT ARGUMENT LIST:
C     INT IANDMASKRA(idimen_wrds) -- ARRAY IN WHICH TO SET THE
C                     "NPIXELON" NUMBER OF BITS ON.
C
C
C REMARKS: 
C     IF GIVEN BIT-COUNT IS .LE. 0; THEN 
C        IT RETURNS WITH IANDMASKRA() ALL ZERO
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$

       INTEGER    NBITSPERWORD
       PARAMETER (NBITSPERWORD=64)   		!... CRAY

       integer    npixelon			!... ARG(1)
       integer    iandmaskra(idimen_wrds)   	!... ARG(2)(3)

       integer    npxls
       integer    lmtpxls
       integer    nffwrds
       integer    npxl_rem

C      ... initialize to all bits off.
       do  i = 1,idimen_wrds
         iandmaskra(i) = 0    		!... all bits off
       enddo

C      ... check value of npixelon for reasonable bounds ...
       npxls   = npixelon
       lmtpxls = NBITSPERWORD*idimen_wrds

       if(npxls .GT. lmtpxls) then
         npxls = lmtpxls
       else if(npxls .LE. 0) then
         npxls = 0
         go to 999
       endif

       nffwrds  = npxls / NBITSPERWORD
       if(nffwrds .GT. 0) then
         do i = 1,nffwrds
           iandmaskra(i) = -1   	!... set all bits on in word
         enddo
       endif

       npxl_rem = mod(npxls,NBITSPERWORD)
       if(npxl_rem .GT. 0) then
         nffwrds = nffwrds + 1
         iandmaskra(nffwrds) = MASK(npxl_rem)   	!... set bits on
       endif
C      .... checkout print ...
C ...       write(6,245)npxls,idimen_wrds,nffwrds
C 245  format(1h ,'genanmsk: generated a string of',I6,' bits "on"',
C ...     1       /1h ,'        in array dimensioned   ',I6,' words',
C ...     2       /1h ,'        within which on bits in',I6,' words')

  999  continue
       return
       end   
