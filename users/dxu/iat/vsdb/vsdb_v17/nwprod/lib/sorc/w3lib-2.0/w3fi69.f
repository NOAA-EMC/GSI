      SUBROUTINE W3FI69 (PDS, ID)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    W3FI69      CONVERT PDS TO 25, OR 27 WORD ARRAY
C   PRGMMR: R.E.JONES        ORG: W/NMC42    DATE: 91-05-14
C
C ABSTRACT: CONVERTS AN EDITION 1 GRIB PRODUCE DEFINITION SECTION (PDS)
C   TO A 25, OR 27 WORD INTEGER ARRAY.
C 
C PROGRAM HISTORY LOG:
C   91-05-14  R.E.JONES 
C   92-09-25  R.E.JONES   CHANGE LEVEL TO USE ONE OR TWO WORDS
C   93-01-08  R.E.JONES   CHANGE FOR TIME RANGE INDICATOR IF 10
C   93-03-29  R.E.JONES   ADD SAVE STATEMENT
C   93-10-21  R.E.JONES   CHANGES FOR ON388 REV. OCT 9,1993, NEW
C                         LEVELS 125, 200, 201.
C   94-04-14  R.E.JONES   CHANGES FOR ON388 REV. MAR 24,1994, NEW
C                         LEVELS 115, 116.
C   94-12-04  R.E.JONES   CHANGES FOR 27 WORD INTEGER ARRAY IF
C                         PDS IS GREATER THAN 28 BYTES. 
C   95-09-07  R.E.JONES   CHANGES FOR LEVEL 117, 119. 
C   98-12-21  Gilbert    Replaced Function ICHAR with mova2i.
C
C USAGE:    CALL W3FI69 (PDS, ID)
C   INPUT ARGUMENT LIST:
C     PDS      - 28 TO 100 CHARACTER PRODUCT DEFINITION SECTION 
C                (PDS) 
C   OUTPUT ARGUMENT LIST:
C     ID       - 25, OR 27 WORD INTEGER ARRAY
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO, Indy
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY C916/256, J916/2048
C
C$$$
C
      INTEGER        ID(*)
C
      CHARACTER * 1  PDS(*)
C
      SAVE
C
C     ID(1)  = NUMBER OF BYTES IN PDS 
C     ID(2)  = PARAMETER TABLE VERSION NUMBER     
C     ID(3)  = IDENTIFICATION OF ORIGINATING CENTER 
C     ID(4)  = MODEL IDENTIFICATION (ALLOCATED BY ORIGINATING CENTER)
C     ID(5)  = GRID IDENTIFICATION
C     ID(6)  = 0 IF NO GDS SECTION, 1 IF GDS SECTION IS INCLUDED
C     ID(7)  = 0 IF NO BMS SECTION, 1 IF BMS SECTION IS INCLUDED
C     ID(8)  = INDICATOR OF PARAMETER AND UNITS 
C     ID(9)  = INDICATOR OF TYPE OF LEVEL OR LAYER 
C     ID(10) = LEVEL 1
C     ID(11) = LEVEL 2
C     ID(12) = YEAR OF CENTURY
C     ID(13) = MONTH OF YEAR
C     ID(14) = DAY OF MONTH
C     ID(15) = HOUR OF DAY
C     ID(16) = MINUTE OF HOUR   (IN MOST CASES SET TO 0)
C     ID(17) = FCST TIME UNIT
C     ID(18) = P1 PERIOD OF TIME
C     ID(19) = P2 PERIOD OF TIME
C     ID(20) = TIME RANGE INDICATOR
C     ID(21) = NUMBER INCLUDED IN AVERAGE
C     ID(22) = NUMBER MISSING FROM AVERAGES OR ACCUMULATIONS
C     ID(23) = CENTURY
C     ID(24) = IDENTIFICATION OF SUB-CENTER (TABLE 0 - PART 2)
C     ID(25) = SCALING POWER OF 10
C     ID(26) = FLAG BYTE, 8 ON/OFF FLAGS
C              BIT NUMBER  VALUE  ID(26)   DEFINITION   
C              1           0      0      FULL FCST FIELD
C                          1      128    FCST ERROR FIELD
C              2           0      0      ORIGINAL FCST FIELD
C                          1      64     BIAS CORRECTED FCST FIELD
C              3           0      0      ORIGINAL RESOLUTION RETAINED
C                          1      32     SMOOTHED FIELD
C              NOTE: ID(26) CAN BE THE SUM OF BITS 1, 2, 3.
C              BITS 4-8 NOT USED, SET TO ZERO.
C              IF ID(1) IS 28, YOU DO NOT NEED ID(26) AND ID(27).
C     ID(27) = UNUSED, SET TO 0 SO PDS BYTE 30 IS SET TO ZERO.$
C
        ID(1)  = mova2i(PDS(1)) * 65536 + mova2i(PDS(2)) * 256 +
     &           mova2i(PDS(3))
        ID(2)  = mova2i(PDS(4))
        ID(3)  = mova2i(PDS(5))
        ID(4)  = mova2i(PDS(6))
        ID(5)  = mova2i(PDS(7)) 
        ID(6)  = IAND(ISHFT(mova2i(PDS(8)),-7),1)
        ID(7)  = IAND(ISHFT(mova2i(PDS(8)),-6),1)
        ID(8)  = mova2i(PDS(9))
        ID(9)  = mova2i(PDS(10))
        I9     = mova2i(PDS(10))
C 
C       TEST ID(9) FOR 1-100, 102,103, 105, 107, 109,
C       111,113,115,117,119,160,200,201, IF TRUE, SET ID(10) TO 0,
C       AND STORE 16 BIT VALUE (BYTES 11 & 12) THE LEVEL IN ID(11).
C
        IF ((I9.GE.1.AND.I9.LE.100).OR.I9.EQ.102.OR.
     &       I9.EQ.103.OR.I9.EQ.105.OR.I9.EQ.107.OR.
     &       I9.EQ.109.OR.I9.EQ.111.OR.I9.EQ.113.OR.
     &       I9.EQ.115.OR.I9.EQ.117.OR.I9.EQ.119.OR.
     &       I9.EQ.125.OR.I9.EQ.160.OR.I9.EQ.200.OR.
     &       I9.EQ.201) THEN
          LEVEL  = mova2i(PDS(11)) * 256 + mova2i(PDS(12))
          IF (IAND(LEVEL,32768).NE.0) THEN
            LEVEL = -IAND(LEVEL,32767)
          END IF
          ID(10) = 0
          ID(11) = LEVEL
        ELSE
          ID(10) = mova2i(PDS(11))
          ID(11) = mova2i(PDS(12))
        END IF
        ID(12) = mova2i(PDS(13))
        ID(13) = mova2i(PDS(14))
        ID(14) = mova2i(PDS(15))
        ID(15) = mova2i(PDS(16))
        ID(16) = mova2i(PDS(17))
        ID(17) = mova2i(PDS(18))
        ID(18) = mova2i(PDS(19))
        ID(19) = mova2i(PDS(20))
        ID(20) = mova2i(PDS(21))
C
C       IF TIME RANGE IDICATOR IS 10, P1 IS PACKED INTO
C       PDS BYTES 19-20. PUT THEM IN P1 AND SET P2 TO ZERO.
C
        IF (ID(20).EQ.10) THEN
          ID(18) = ID(18) * 256 + ID(19)
          ID(19) = 0
        END IF
        ID(21) = mova2i(PDS(22)) * 256 + mova2i(PDS(23))
        ID(22) = mova2i(PDS(24))
        ID(23) = mova2i(PDS(25))
        ID(24) = mova2i(PDS(26))
        ISCALE = mova2i(PDS(27)) * 256 + mova2i(PDS(28))
        IF (IAND(ISCALE,32768).NE.0) THEN
          ISCALE = -IAND(ISCALE,32767)
        END IF 
        ID(25) = ISCALE
        IF (ID(1).GT.28) THEN
          ID(26) = mova2i(PDS(29))
          ID(27) = mova2i(PDS(30))
        END IF
C      
      RETURN 
      END
