      SUBROUTINE W3FT33(AIN,OUT,NSFLAG)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    W3FT33      THICKEN THINNED WAFS GRIB GRID 37-44
C   PRGMMR: RALPH PETTERSON  ORG: W/NMCXX    DATE: 94-11-13
C
C ABSTRACT: SUBROUTINE THICKENS ONE THINNED WAFS GRIB GRID TO A
C   REAL ARRAY OF 5329 NUMBERS (73,73) 1.25 DEGREE GRID.
C
C PROGRAM HISTORY LOG:
C   94-??-??  RALPH PETERSON
C   94-11-07  R.E.JONES        ADD DOC BLOCK, CHANGE CALL TO 3
C                              PARAMETERS. REPLACE COS WITH TABLE
C                              LOOKUP.
C   95-06-02  RALPH PETERSON   CHANGES TO CORRECT MISS-POSITION
C                              BETWEEN + OR - 8.75 N/S.
C   95-06-03  R.E.JONES        CHANGES SO 8 ROWS WITH 73 VALUES
C                              ARE NOT THICKENED, 10% FASTER.  
C
C USAGE:    CALL W3FT33(AIN, OUT, NSFLAG)
C   INPUT ARGUMENT LIST:
C     AIN      - REAL 3447 WORD ARRAY WITH UNPACKED THINNED WAFS
C                GRIB TYPE 37-44.
C     NSFLAG   - INTEGER =  1  AIN IS WAFS GRIB GRID 37-40  N. HEMI.
C                        = -1  AIN IS WAFS GRIB GRID 41-44  S. HEMI.   
C
C   OUTPUT ARGUMENT LIST:  
C     OUT      - REAL (73,73) WORD ARRAY WITH THICKENED WAFS GRIB
C                GRID 37-44.
C
C REMARKS: THE POLE POINT FOR U AND V WIND COMPONENTS WILL HAVE ONLY
C   ONE POINT. IF YOU NEED THE POLE ROW CORRECTED SEE PAGE 9 SECTION
C   1 IN OFFICE NOTE 388. YOU NEED BOTH U AND V TO MAKE THE 
C   CORRECTION.
C 
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 5.2 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO, Indy
C
C$$$
C         
         PARAMETER (NX=73,NY=73)
         PARAMETER (NIN=3447)
C
         REAL      AIN(*)
         REAL      OUT(NX,NY)
C
         INTEGER   IPOINT(NX)
C
         SAVE
C
      DATA  IPOINT/
     & 73, 73, 73, 73, 73, 73, 73, 73, 72, 72, 72, 71, 71, 71, 70,
     & 70, 69, 69, 68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60,
     & 59, 58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 43,
     & 42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 25, 23, 22,
     & 20, 19, 17, 16, 14, 12, 11,  9,  8,  6,  5,  3,  2/
C
         NXM   = NX - 1
         FNXM  = FLOAT(NXM)
C
C        TEST FOR GRIDS (37-40)
C
         IF (NSFLAG.GT.0) THEN
C
C          DO NOT THICKEN 8 ROWS WITH 73 VALUES, MOVE DATA 
C          TO OUT ARRAY. GRIDS (37-40) N. 
C
           IS = 0
           DO J = 1,8
             DO I = 1,NX
               IS       = IS + 1
               OUT(I,J) = AIN(IS)
             END DO
           END DO
C
           IE = NX * 8
           DO J = 9,NY
             NPOINT   = IPOINT(J)
             IS       = IE + 1
             IE       = IS + NPOINT - 1
             DPTS     = (FLOAT(NPOINT)-1.) / FNXM
             PW       = 1.0
             PE       = PW + DPTS
             OUT(1,J) = AIN(IS)
             VALW     = AIN(IS)
             VALE     = AIN(IS+1)
             DVAL     = (VALE-VALW)
             DO I = 2,NXM
               WGHT     = PE -FLOAT(IFIX(PE))
               OUT(I,J) = VALW + WGHT * DVAL
               PW       = PE
               PE       = PE + DPTS
               IF (IFIX(PW).NE.IFIX(PE)) THEN
                 IS   = IS + 1
                 VALW = VALE
                 VALE = AIN(IS+1)
                 DVAL = (VALE - VALW)
               END IF
             END DO
             OUT(NX,J) = AIN(IE)
           END DO
C
         ELSE
C
C         DO NOT THICKEN 8 ROWS WITH 73 VALUES, MOVE DATA  
C         TO OUT ARRAY. GRIDS (41-44) S.
C
           IS = NIN - (8 * NX)
           DO J = 66,NY
             DO I = 1,NX
               IS       = IS + 1
               OUT(I,J) = AIN(IS)
             END DO
           END DO
C
           IE = 0
           DO J = 1,65
             NPOINT   = IPOINT(74-J)
             IS       = IE + 1
             IE       = IS + NPOINT - 1
             DPTS     = (FLOAT(NPOINT)-1.) / FNXM
             PW       = 1.0
             PE       = PW + DPTS
             OUT(1,J) = AIN(IS)
             VALW     = AIN(IS)
             VALE     = AIN(IS+1)
             DVAL     = (VALE-VALW)
             DO I = 2,NXM
               WGHT     = PE -FLOAT(IFIX(PE))
               OUT(I,J) = VALW + WGHT * DVAL
               PW       = PE
               PE       = PE + DPTS
               IF (IFIX(PW).NE.IFIX(PE)) THEN
                 IS   = IS + 1
                 VALW = VALE
                 VALE = AIN(IS+1)
                 DVAL = (VALE - VALW)
               END IF
             END DO
             OUT(NX,J) = AIN(IE)
           END DO
         END IF
C
         RETURN
         END
