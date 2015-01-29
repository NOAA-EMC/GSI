        SUBROUTINE OTRNCDX6(CCODE,i8sline,nwdpln,IRETN)
C                                                       29-May-1996/dss
C       ... COPIED ~/ncod/otr_ncd6.f to ~/ncod/v4/otrncdx6.f
C       ...    for a CRAY version which outputs 1920-byte records 
C       ...    instead of the old 512-byte records.
C
C       ... Mods to SAVE some vrbls                      4-Apr-1995/dss
C
C       ... Reprogramming from VAX to UNIX              19-Apr-1994/dss
C       ... COPIED ~/tiff/outer_pakrast2 into ~/volash/otr_pkras2.f
C       ...    since VAX names are too long for UNIX.
C       ...    This was called from main = LOOK_PURAS
C
C       ... COPIED [006300.TIFF]OUTER_PAKRAST.FOR       10-MAY-1993/DSS
C       ...    INTO OUTER_PAKRAST2()
C       ...    IN ORDER TO ADD AN OPTION TO REPEAT THE SCANLINE OUTPUT
C       ...    SO THAT A MAP COULD BE STRETCHED IN THE VERTICAL;
C       ...    THAT L*4 LDUBLQ IS PASSED INTO THIS VIA COMMON /RASTCON/
C       ...    AND IS PASSED THROUGH TO MIDNCDX6() IN ADDITIONAL
C       ...    ARGUMENT IN CALL SEQ. 

C       ... COPIED [006300.IBMFAX]PUTNRAST_PAK.FOR      2-MAR-1990/DSS
C       ... IN ORDER TO STRIP IT DOWN TO AN ENVELOPE OF PAKRAST
C       ... TO BE CALLED BY LOOKTIF
C       ... SINCE I ALREADY HAVE THE ENTIRE UNPACKED SCANLINE
C       ... IN ... I8SLINE ...
C
C       ... TO PUT PIXELS INTO UNPACKED RASTER          24-JUL-1989/DSS
C       ... AND THEN ENCODE IN RUN-LENGTH CODE OF NMC EXTENDED 6-BIT 
C       ... FAX CODE VIA CALL PAKRAST TO MAKE OUTPUT LOOK LIKE
C       ... JOHN SIMMONS'  FILENAME.SPL FILE.
C        
C       ... Output file is X6B code buffered out in 1920-byte buffers
C       ... OPENed and CLOSEd in here, but written in MID_PKRAST2()
C       ... KRISHNA KUMAR converted this code from CRAY to IBM RS/6000
C
C       USAGE:    call OTRNCDX6(CCODE,i8sline,nwdpln,IRETN)
C       GIVEN:
C       ... (1) CCODE='INIT '  TO OPEN "OUTRAS',
C                                             A JOHN SIMMONS' LOOKALIKE
C                    ='CLOSE' TO CLOSE OUTRAS FILE
C       ...          ='ENDLN' TO PAK THIS CURRENT RASTER
C                    ='ENDMP' TO TELL PAKRAST TO END THIS MAP
C                    ='STRIP' TO START THE STRIP-TITLES BLOCK
C           (2),(3) INTEGER  I8SLINE(nwdpln)
C                  ...   SHOULD NOT EXCEED 131 I*4 WORDS = 4192 BITS
C                  ...   which on CRAY:     66 I*8 words = 4224 bits
C       RESULTS:
C       ... (4) IRETN ... RETURN CODE
C                    =0   NORMAL
C                    =-1  END-OF-MAP
C                    = POSITIVE NUMBERS ARE ERROR RETURNS.
C
C       ---------------------------------------------------------------
C       USAGE:    call OTRNCDX6(CCODE,i8sline,nwdpln,IRETN)
        CHARACTER*5 CCODE
        INTEGER     I8SLINE(nwdpln)
        INTEGER     IRETN
C
C       ---------------------------------------------------------------
C       ...   FOR COMMON USED BY SUBR BUFO1920()
        COMMON  /ARBFOX6B/ LUNX6B,LUX6BOPNQ,NBUFX6B,IPTR_X6BF
        INTEGER            LUNX6B
        LOGICAL            LUX6BOPNQ
        INTEGER            NBUFX6B
        INTEGER            IPTR_X6BF
C       ---------------------------------------------------------------
C       ...   THIS IS NOT USED BY ANYBODY OUTSIDE ?????????????? ...

        COMMON  /RASTCON/NRASTERS,LDUBLQ,CPREVCODE
        INTEGER       NRASTERS  	!... initialized & INCR herein
        LOGICAL       LDUBLQ
        CHARACTER*4   CPREVCODE
C
C       ----------------------------------------------------------------
C
        INTEGER     LIMITNWD
        DATA        LIMITNWD   / 66 /  		!... FOR INT*8 WORDS
C ...        INT*4   LIMITNWD / 131 /
C
        INTEGER     lastch    		!... declare function
C
        INTEGER     IFIRSTQ
        INTEGER     ILASTQ

        integer      intword
        character*1  cdata(8)
        equivalence (intword,cdata(1))

        integer      nchars_bfow
        integer      icmd_bfow
        INTEGER      IRET_BF
C
C ...        SAVE         IFIRSTQ,ILASTQ ...
        SAVE
C
C       . . . . . . . .  S T A R T  . . . . . . . . . . . . . . .

        IRETN = 0

C       WRITE(6,105)CCODE
C 105   FORMAT(1H ,'OTRNCDX6:CALLED WITH CCODE= ',A)
C
C       . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C       . . . . . . .   CASE "ENDL"   . . . . . . . . . . . . . . . . .

        IF(CCODE(1:4) .EQ. 'ENDL') THEN
C           ... OUTPUT THIS UNPACKED SCANLINE ...

            CALL MIDNCDX6(i8sline,nwdpln,IFIRSTQ,ILASTQ,
     1                      LDUBLQ,IERROR)

            IF(IERROR .NE. 0) GO TO 950
            NRASTERS = NRASTERS + 1
C           IF(MOD(NRASTERS,100) .EQ. 0) THEN
C             WRITE(6,355)NRASTERS
C 355         FORMAT(1H ,'--------------RASTER',I7,'  ----------------')
C           ENDIF
C           IF(LDUBLQ) THEN
C             NRASTERS = NRASTERS + 1
C             IF(MOD(NRASTERS,100) .EQ. 0) THEN
C               WRITE(6,355)NRASTERS
C             ENDIF
C           ENDIF

            GO TO 998

C       . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C       . . . . . . .   CASE "ENDM"   . . . . . . . . . . . . . . . . .
C
        ELSE IF(CCODE(1:4) .EQ. 'ENDM') THEN
C           ... THIS IS END MAP
            ILASTQ = 1

            CALL MIDNCDX6(i8sline,nwdpln,IFIRSTQ,ILASTQ,
     1                      LDUBLQ,IERROR)

            IF(IERROR .NE. 1) GO TO 950
C           ... WHERE IERROR=1 IF CLEANED UP W/ END-OF-MAP ...
            IRETN = -1
            GO TO 998
C
C       . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C       . . . . . . .   CASE "INIT"   . . . . . . . . . . . . . . . . .
C
        ELSE IF(CCODE(1:4) .EQ. 'INIT') THEN
C         ... INITIALIZE OUTPUT FOR PACKED RASTER (SIMMONS') OUTPUT ...
          IF(LUNX6B .LE. 0) GO TO 930
C         ... OTHERWISE, LUNX6B .GT. 0; SO,

          IFIRSTQ = 1    	!... TO OPEN & INIT
          ILASTQ = 0
C         ... WHERE IFIRSTQ AND ILASTQ ARE MIDNCDX6 ARGS ...
          ldublq = .false.
C
          CALL MIDNCDX6(i8sline,nwdpln,IFIRSTQ,ILASTQ,LDUBLQ,IERROR)

          IF(IERROR .EQ. 0) THEN
            WRITE(6,365)
  365       FORMAT(1H ,'OTRNCDX6::MIDNCDX6: OPENED OUTPUT FILE FOR ',
     1                 'EXTENDED NMC 6-BIT DATA')
          ELSE
            GO TO 900
          ENDIF
C
          NRASTERS = 0

C       . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C       . . . . . . .   CASE "STRIP"   . . . . . . . . . . . . . . . . .
C
        ELSE IF(CCODE(1:4) .EQ. 'STRI') THEN
C         ... INITIALIZE  FOR STRIP-TITLE BLOCK  ...
          IF(LUNX6B .LE. 0) GO TO 930
C         ... OTHERWISE, LUNX6B .GT. 0; SO,

          IFIRSTQ = -1    	!... TO INIT FOR STRIP-TITLES
          ILASTQ = 0
C         ... WHERE IFIRSTQ AND ILASTQ ARE MIDNCDX6 ARGS ...
          ldublq = .false.
C
          CALL MIDNCDX6(i8sline,nwdpln,IFIRSTQ,ILASTQ,LDUBLQ,IERROR)

          IF(IERROR .EQ. 0) THEN
            WRITE(6,375)
  375       FORMAT(1H ,'OTRNCDX6::MIDNCDX6: PREPARED OUTPUT FOR ',
     1                 'STRIP-TITLE BLOCK')
          ELSE
            GO TO 900
          ENDIF
C
          NRASTERS = 0

C       . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C       . . . . . . .   CASE "CLOS"   . . . . . . . . . . . . . . . . .
C
        ELSE IF(CCODE(1:4) .EQ. 'CLOS') THEN
C         ... CLOSE OUTPUT FOR X6B OUTPUT ...
          intword = 0
          nchars_bfow = 0
          icmd_bfow = 999  		!... FLUSH & CLOSE x6b file
          call BUFO1920(cdata,nchars_bfow,icmd_bfow,iret_bf)
C
          GO TO 998

C       . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C       . . . . . . .   CASE DEFAULT  . . . . . . . . . . . . . . . . .

        ELSE
C         ... I DON'T RECOGNIZE THIS CODE ...
          IRETN = 5
          GO TO 998
        ENDIF
C       . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C       . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
        GO TO 998
C
  900   CONTINUE
        WRITE(6,905)LUNX6B
  905   FORMAT(1H ,'OTRNCDX6:FATAL ERROR. UNABLE TO OPEN OUTPUT',
     A             ' FILE',
     1        /1H ,'   FOR ENCODED X6B DATA ON UNIT NO.',I8)
        IRETN = 1
        GO TO 999
C
C
  930   CONTINUE
        WRITE(6,935)LUNX6B
  935   FORMAT(1H ,'OTRNCDX6:FATAL ERROR. UNIT NUMBER FOR OUTPUT',
     A        /1H ,' FILE WAS NOT SET IN MAIN PROGRAM. LUNX6B=HEX',
     1             Z16.16)
        IRETN = 3
        GO TO 999
C
  940   CONTINUE
        WRITE(6,945)LUNX6B
  945   FORMAT(1H ,'OTRNCDX6:FATAL ERROR. UNABLE TO CLOSE ',
     1             'OUTPUT FILE ON UNIT NO.',I4)
        IRETN = 4
        GO TO 999
C
  950   CONTINUE
        WRITE(6,955)IERROR
  955   FORMAT(1H ,'OTRNCDX6::MIDNCDX6 FAILED WITH IERROR=',I5)
        IRETN = 6
        GO TO 999
C
  998   CONTINUE
        CPREVCODE(1:4) = CCODE(1:4)
        GO TO 999
C
  999   CONTINUE
        RETURN
        END
