      SUBROUTINE GBYTE(IPACKD,IUNPKD,NOFF,NBITS)
C
C THIS PROGRAM WRITTEN BY.....
C             DR. ROBERT C. GAMMILL, CONSULTANT
C             NATIONAL CENTER FOR ATMOSPHERIC RESEARCH
C             MAY 1972
C
C             CHANGES FOR SiliconGraphics IRIS-4D/25
C             SiliconGraphics 3.3 FORTRAN 77
C             March 1991, RUSSELL E. JONES
C             NATIONAL WEATHER SERVICE
C
C THIS IS THE FORTRAN VERSION OF GBYTE
C
C***********************************************************************
C
C SUBROUTINE GBYTE (IPACKD,IUNPKD,NOFF,NBITS)
C
C PURPOSE                TO UNPACK A BYTE INTO A TARGET WORD.  THE
C                        UNPACKED BYTE IS RIGHT-JUSTIFIED IN THE
C                        TARGET WORD, AND THE REMAINDER OF THE
C                        WORD IS ZERO-FILLED.
C
C USAGE                  CALL GBYTE(IPACKD,IUNPKD,NOFF,NBITS)
C
C ARGUMENTS
C
C ON INPUT               IPACKD
C                          THE WORD OR ARRAY CONTAINING THE BYTE TO BE
C                          UNPACKED.
C
C                        IUNPKD
C                          THE WORD WHICH WILL CONTAIN THE UNPACKED
C                          BYTE.
C
C                        NOFF
C                          THE NUMBER OF BITS TO SKIP, LEFT TO RIGHT,
C                          IN 'IPACKD' IN ORDER TO LOCATE THE BYTE
C                          TO BE UNPACKED.
C
C                        NBITS
C                          NUMBER OF BITS IN THE BYTE TO BE UNPACKED.
C                          MAXIMUM OF 64 BITS ON 64 BIT MACHINE, 32
C                          BITS ON 32 BIT MACHINE.
C
C ON OUTPUT              IUNPKD
C                          CONTAINS THE REQUESTED UNPACKED BYTE.
C***********************************************************************

      INTEGER    IPACKD(*)
      INTEGER    IUNPKD
      INTEGER    MASKS(64)
C
      SAVE
C
      DATA IFIRST/1/
      IF(IFIRST.EQ.1) THEN
         CALL W3FI01(LW)
         NBITSW = 8 * LW
         JSHIFT = -1 * NINT(ALOG(FLOAT(NBITSW)) / ALOG(2.0))
         MASKS(1) = 1
         DO I=2,NBITSW-1
            MASKS(I) = 2 * MASKS(I-1) + 1
         ENDDO
         MASKS(NBITSW) = -1
         IFIRST = 0
      ENDIF
C
C NBITS MUST BE LESS THAN OR EQUAL TO NBITSW                                   
C
      ICON   = NBITSW - NBITS
      IF (ICON.LT.0) RETURN
      MASK   = MASKS(NBITS)
C
C INDEX TELLS HOW MANY WORDS INTO THE ARRAY 'IPACKD' THE NEXT BYTE
C APPEARS.         
C
      INDEX  = ISHFT(NOFF,JSHIFT)
C
C II TELLS HOW MANY BITS THE BYTE IS FROM THE LEFT SIDE OF THE WORD.
C
      II     = MOD(NOFF,NBITSW)
C
C MOVER SPECIFIES HOW FAR TO THE RIGHT NBITS MUST BE MOVED IN ORDER            
C
C    TO BE RIGHT ADJUSTED.                                                 
C
      MOVER = ICON - II
C
      IF (MOVER.GT.0) THEN
        IUNPKD  = IAND(ISHFT(IPACKD(INDEX+1),-MOVER),MASK)
C
C THE BYTE IS SPLIT ACROSS A WORD BREAK.                 
C
      ELSE IF (MOVER.LT.0) THEN                      
        MOVEL = - MOVER                                                       
        MOVER = NBITSW - MOVEL                                                
        IUNPKD  = IAND(IOR(ISHFT(IPACKD(INDEX+1),MOVEL),
     &          ISHFT(IPACKD(INDEX+2),-MOVER)),MASK)
C
C THE BYTE IS ALREADY RIGHT ADJUSTED.
C  
      ELSE                              
        IUNPKD  = IAND(IPACKD(INDEX+1),MASK) 
      ENDIF
C   
      RETURN
      END
