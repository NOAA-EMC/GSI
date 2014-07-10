      SUBROUTINE W3FC05(U, V, DIR, SPD)                                 11260000
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    11270000
C                .      .    .                                       .  11280000
C SUBPROGRAM:  W3FC05        EARTH U,V WIND COMPONENTS TO DIR AND SPD   11290000
C   PRGMMR: KEYSER           ORG: W/NMC22     DATE: 92-10-21            11300000
C                                                                       11310000
C ABSTRACT: GIVEN THE TRUE (EARTH ORIENTED) WIND COMPONENTS             11320000
C   COMPUTE THE WIND DIRECTION AND SPEED.                               11330000
C   INPUT WINDS AT THE POLE ARE ASSUMED TO FOLLOW THE WMO               11340000
C   CONVENTIONS, WITH THE OUTPUT DIRECTION COMPUTED IN ACCORDANCE       11350000
C   WITH WMO STANDARDS FOR REPORTING WINDS AT THE POLE.                 11360000
C   (SEE OFFICE NOTE 241 FOR WMO DEFINITION.)                           11370000
C                                                                       11380000
C PROGRAM HISTORY LOG:                                                  11390000
C   92-10-21  D.A.KEYSER  ADDED 1.E-3 TO DIRECTION TO ALLOW TRUNCATION  11391000
C                         TO NEAREST WHOLE DEGREE TO BE CORRECT         11391100
C                         (KEEPS AGREEMENT BETWEEN CRAY & NAS VERSIONS) 11391200
C   91-03-05  R.E.JONES   CHANGES FOR CRAY CFT77 FORTRAN                11391300
C   88-10-19  CHASE, P.   ALLOW OUTPUT VALUES TO OVERLAY INPUT          11392000
C   81-12-30  STACKPOLE, JOHN                                           11400000
C                                                                       11430000
C USAGE:    CALL W3FC05 (U, V, DIR, SPD)                                11440000
C                                                                       11450000
C   INPUT ARGUMENT LIST:                                                11460000
C     U        - REAL   EARTH-ORIENTED U-COMPONENT                      11470001
C     V        - REAL   EARTH-ORIENTED V-COMPONENT                      11480001
C                                                                       11490000
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)                  11500000
C     DIR      - REAL   WIND DIRECTION, DEGREES.  VALUES WILL           11510001
C                BE FROM 0 TO 360 INCLUSIVE.                            11520000
C     SPD      - REAL   WIND SPEED IN SAME UNITS AS INPUT               11530001
C                                                                       11540000
C   INPUT FILES:   NONE                                                 11550000
C                                                                       11560000
C   OUTPUT FILES:   NONE                                                11570000
C                                                                       11580000
C   SUBPROGRAMS CALLED:                                                 11590000
C     LIBRARY:                                                          11600000
C       COMMON   - SQRT, ATAN2                                          11610000
C                                                                       11620000
C REMARKS:  IF SPEED IS LESS THAN 1E-10 THEN DIRECTION WILL BE SET      11630000
C   TO ZERO.                                                            11640000
C                                                                       11650000
C ATTRIBUTES:                                                           11660000
C   LANGUAGE: CRAY CFT77 FORTRAN                                        11670000
C   MACHINE:  CRAY Y-MP8/832                                            11680000
C                                                                       11690000
C$$$                                                                    11700000
C                                                                       11710000
C     VARIABLES.....                                                    11720000
C                                                                       11730000
      REAL   U, V, DIR, SPD, XSPD                                       11740000
C                                                                       11750000
C     CONSTANTS.....                                                    11760000
C                                                                       11770000
      DATA  SPDTST/1.0E-10/                                             11780000
      DATA  RTOD  /57.2957795/                                          11790000
      DATA  DCHALF/180.0/                                               11800000
C                                                                       11810000
      XSPD = SQRT(U * U + V * V)                                        11820000
      IF (XSPD .LT. SPDTST) THEN                                        11830000
         DIR = 0.0                                                      11840000
      ELSE                                                              11850000
         DIR = ATAN2(U,V) * RTOD + DCHALF + 1.E-3                       11860000
      ENDIF                                                             11870000
      SPD = XSPD                                                        11880000
      RETURN                                                            11890000
      END                                                               11900000
