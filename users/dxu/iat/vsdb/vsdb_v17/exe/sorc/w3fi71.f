      SUBROUTINE w3fi71(igrid,igds,ierr)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    W3FI71      MAKE ARRAY USED BY GRIB PACKER FOR GDS
C   PRGMMR: R.E.JONES        ORG: W/NMC42    DATE: 93-03-26
C
C ABSTRACT: W3FI71 MAKES A 18 OR 91 WORD INTEGER ARRAY USED BY W3FI72
C   GRIB PACKER TO MAKE THE GRID DESCRIPTION SECTION (GDS) - SECTION 2.
C
C PROGRAM HISTORY LOG:
C   92-02-21  R.E.JONES
C   92-07-01  M. FARLEY    ADDED REMARKS FOR 'IGDS' ARRAY ELEMENTS.
C                          ADDED LAMBERT CONFORMAL GRIDS AND ENLARGED
C                          IDGS ARRAY FROM 14 TO 18 WORDS.
C   92-10-03  R.E.JONES    ADDED CORRECTIONS TO AWIPS GRIB TABLES
C   92-10-16  R.E.JONES    ADD GAUSSIAN GRID 126 TO TABLES
C   92-10-18  R.E.JONES    CORRECTIONS TO LAMBERT CONFORMAL TABLES
C                          AND OTHER TABLES
C   92-10-19  R.E.JONES    ADD GAUSSIAN GRID  98 TO TABLES
C   93-01-25  R.E.JONES    ADD ON84 GRIDS 87, 106, 107 TO TABLES
C   93-03-10  R.E.JONES    ADD ON84 GRIDS 1, 55, 56 TO TABLES
C   93-03-26  R.E.JONES    ADD GRIB GRIDS 2, 3 TO TABLES
C   93-03-29  R.E.JONES    ADD SAVE STATEMENT
C   93-06-15  R.E.JONES    ADD GRIB GRIDS 37 TO 44 TO TABLES
C   93-09-29  R.E.JONES    GAUSSIAN GRID DOCUMENT NOT CORRECT,
C                          W3FI74 WILL BE CHANGED TO AGREE WITH
C                          IT. GAUSSIAN GRID 98 TABLE HAS WRONG
C                          VALUE. 
C   93-10-12  R.E.JONES    CHANGES FOR ON388 REV. OCT 8,1993 FOR
C                          GRID 204, 208.
C   93-10-13  R.E.JONES    CORRECTION FOR GRIDS 37-44, BYTES 7-8,
C                          24-25 SET TO ALL BITS 1 FOR MISSING.
C   93-11-23  R.E.JONES    ADD GRIDS 90-93 FOR ETA MODEL
C                          ADD GRID 4 FOR 720*361 .5 DEG. GRID
C   94-04-12  R.E.JONES    CORRECTION FOR GRID 28
C   94-06-01  R.E.JONES    ADD GRID 45, 288*145 1.25 DEG. GRID
C   94-06-22  R.E.JONES    ADD GRIDS 94, 95 FOR ETA MODEL 
C   95-04-11  R.E.JONES    ADD GRIDS 96, 97 FOR ETA MODEL 
C   95-05-19  R.E.JONES    ADD 20 KM AWIPS GRID 215
C   95-05-23  M.E.BALDWIN  ADD GRIDS 250-255 FOR ETA POST
C   95-10-06  E.ROGERS     ADD GRID 216 (EXPANED ALASKA AWIPS GRID)
C   96-09-20  M.E.BALDWIN  ADD CENTRAL LAT-LON TO GDS FOR ETA GRIDS
C   96-12-11  M.E.BALDWIN  ADD GRIDS 194,196,218 FOR ETA MODEL
C   97-10-27  Y.J.ZHU      ADD GRIDS 256 for LATLON NH 20-80
C                          ADD GRID  257 for LATLON SH 20-80
C                          ADD GRID  258 for LATLON tropical 20-20
C
C USAGE:    CALL W3FI71 (IGRID, IGDS, IERR)
C   INPUT ARGUMENT LIST:
C     IGRID       - GRIB GRID NUMBER, OR OFFICE NOTE 84 GRID NUMBER
C
C   OUTPUT ARGUMENT LIST:
C     IGDS      - 18 OR 91 WORD INTEGER ARRAY WITH INFORMATION
C                 TO MAKE A GRIB GRID DESCRIPTION SECTION.
C     IERR       - 0  CORRECT EXIT
C                  1  GRID TYPE IN IGRID IS NOT IN TABLE
C
C REMARKS:
C    1) OFFICE NOTE GRID TYPE 26 IS 6 IN GRIB, 26 IS AN
C       INTERNATIONAL EXCHANGE GRID.
C
C    2) VALUES RETURNED IN 18 OR 91 WORD INTEGER ARRAY IGDS
C       VARY DEPENDING ON GRID REPRESENTATION TYPE.
C
C       LAT/LON GRID:
C           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
C           IGDS( 2) = PV, PL OR 255
C           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
C           IGDS( 4) = NO. OF POINTS ALONG A LATITUDE
C           IGDS( 5) = NO. OF POINTS ALONG A LONGITUDE MERIDIAN
C           IGDS( 6) = LATITUDE OF ORIGIN (SOUTH - IVE)
C           IGDS( 7) = LONGITUDE OF ORIGIN (WEST -IVE)
C           IGDS( 8) = RESOLUTION FLAG (CODE TABLE 7)
C           IGDS( 9) = LATITUDE OF EXTREME POINT (SOUTH - IVE)
C           IGDS(10) = LONGITUDE OF EXTREME POINT (WEST - IVE)
C           IGDS(11) = LATITUDE INCREMENT
C           IGDS(12) = LONGITUDE INCREMENT
C           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
C           IGDS(14) = ... THROUGH ...
C           IGDS(18) =   ... NOT USED FOR THIS GRID
C           IGDS(19) - IGDS(91) FOR GRIDS 37-44, NUMBER OF POINTS
C                      IN EACH OF 73 ROWS.
C
C       GAUSSIAN GRID:
C           IGDS( 1) = ... THROUGH ...
C           IGDS(10) =   ... SAME AS LAT/LON GRID
C           IGDS(11) = NUMBER OF LATITUDE LINES BETWEEN A POLE
C                      AND THE EQUATOR
C           IGDS(12) = LONGITUDE INCREMENT
C           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
C           IGDS(14) = ... THROUGH ...
C           IGDS(18) =   ... NOT USED FOR THIS GRID
C
C       SPHERICAL HARMONICS:
C           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
C           IGDS( 2) = PV, PL OR 255
C           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
C           IGDS( 4) = J - PENTAGONAL RESOLUTION PARAMETER
C           IGDS( 5) = K - PENTAGONAL RESOLUTION PARAMETER
C           IGDS( 6) = M - PENTAGONAL RESOLUTION PARAMETER
C           IGDS( 7) = REPRESENTATION TYPE (CODE TABLE 9)
C           IGDS( 8) = REPRESENTATION MODE (CODE TABLE 10)
C           IGDS( 9) = ... THROUGH ...
C           IGDS(18) =   ... NOT USED FOR THIS GRID
C
C       POLAR STEREOGRAPHIC:
C           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
C           IGDS( 2) = PV, PL OR 255
C           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
C           IGDS( 4) = NO. OF POINTS ALONG X-AXIS
C           IGDS( 5) = NO. OF POINTS ALONG Y-AXIS
C           IGDS( 6) = LATITUDE OF ORIGIN (SOUTH -IVE)
C           IGDS( 7) = LONGITUTE OF ORIGIN (WEST -IVE)
C           IGDS( 8) = RESOLUTION FLAG (CODE TABLE 7)
C           IGDS( 9) = LONGITUDE OF MERIDIAN PARALLEL TO Y-AXIS
C           IGDS(10) = X-DIRECTION GRID LENGTH (INCREMENT)
C           IGDS(11) = Y-DIRECTION GRID LENGTH (INCREMENT)
C           IGDS(12) = PROJECTION CENTER FLAG (0=NORTH POLE ON PLANE,
C                                              1=SOUTH POLE ON PLANE,
C           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
C           IGDS(14) = ... THROUGH ...
C           IGDS(18) =   .. NOT USED FOR THIS GRID
C
C       MERCATOR:
C           IGDS( 1) = ... THROUGH ...
C           IGDS(12) =   ... SAME AS LAT/LON GRID
C           IGDS(13) = LATITUDE AT WHICH PROJECTION CYLINDER
C                        INTERSECTS EARTH
C           IGDS(14) = SCANNING MODE FLAGS
C           IGDS(15) = ... THROUGH ...
C           IGDS(18) =   .. NOT USED FOR THIS GRID
C
C       LAMBERT CONFORMAL:
C           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
C           IGDS( 2) = PV, PL OR 255
C           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
C           IGDS( 4) = NO. OF POINTS ALONG X-AXIS
C           IGDS( 5) = NO. OF POINTS ALONG Y-AXIS
C           IGDS( 6) = LATITUDE OF ORIGIN (SOUTH -IVE)
C           IGDS( 7) = LONGITUTE OF ORIGIN (WEST -IVE)
C           IGDS( 8) = RESOLUTION FLAG (CODE TABLE 7)
C           IGDS( 9) = LONGITUDE OF MERIDIAN PARALLEL TO Y-AXIS
C           IGDS(10) = X-DIRECTION GRID LENGTH (INCREMENT)
C           IGDS(11) = Y-DIRECTION GRID LENGTH (INCREMENT)
C           IGDS(12) = PROJECTION CENTER FLAG (0=NORTH POLE ON PLANE,
C                                              1=SOUTH POLE ON PLANE,
C           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
C           IGDS(14) = NOT USED
C           IGDS(15) = FIRST LATITUDE FROM THE POLE AT WHICH THE
C                      SECANT CONE CUTS THE SPERICAL EARTH
C           IGDS(16) = SECOND LATITUDE ...
C           IGDS(17) = LATITUDE OF SOUTH POLE (MILLIDEGREES)
C           IGDS(18) = LONGITUDE OF SOUTH POLE (MILLIDEGREES)
C
C       ARAKAWA SEMI-STAGGERED E-GRID ON ROTATED LAT/LON GRID
C           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
C           IGDS( 2) = PV, PL OR 255
C           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6) [201]
C           IGDS( 4) = NI  - TOTAL NUMBER OF ACTUAL DATA POINTS
C                            INCLUDED ON GRID
C           IGDS( 5) = NJ  - DUMMY SECOND DIMENSION; SET=1
C           IGDS( 6) = LA1 - LATITUDE  OF FIRST GRID POINT
C           IGDS( 7) = LO1 - LONGITUDE OF FIRST GRID POINT
C           IGDS( 8) = RESOLUTION AND COMPONENT FLAG (CODE TABLE 7)
C           IGDS( 9) = LA2 - NUMBER OF MASS POINTS ALONG 
C                            SOUTHERNMOST ROW OF GRID
C           IGDS(10) = LO2 - NUMBER OF ROWS IN EACH COLUMN
C           IGDS(11) = DI  - LONGITUDINAL DIRECTION INCREMENT
C           IGDS(12) = DJ  - LATITUDINAL  DIRECTION INCREMENT
C           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
C           IGDS(14) = CLAT - LATITUDE OF CENTRAL POINT
C           IGDS(15) = CLON - LONGITUDE OF CENTRAL POINT
C           IGDS(16) = ... THROUGH ...
C           IGDS(18) = ... NOT USED FOR THIS GRID (SET TO ZERO)
C
C       ARAKAWA FILLED E-GRID ON ROTATED LAT/LON GRID
C           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
C           IGDS( 2) = PV, PL OR 255
C           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6) [202]
C           IGDS( 4) = NI  - TOTAL NUMBER OF ACTUAL DATA POINTS
C                            INCLUDED ON GRID
C           IGDS( 5) = NJ  - DUMMY SECOND DIMENTION; SET=1
C           IGDS( 6) = LA1 - LATITUDE LATITUDE OF FIRST GRID POINT
C           IGDS( 7) = LO1 - LONGITUDE OF FIRST GRID POINT
C           IGDS( 8) = RESOLUTION AND COMPONENT FLAG (CODE TABLE 7)
C           IGDS( 9) = LA2 - NUMBER OF (ZONAL) POINTS IN EACH ROW
C           IGDS(10) = LO2 - NUMBER OF (MERIDIONAL) POINTS IN EACH
C                            COLUMN
C           IGDS(11) = DI  - LONGITUDINAL DIRECTION INCREMENT
C           IGDS(12) = DJ  - LATITUDINAL  DIRECTION INCREMENT
C           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
C           IGDS(14) = ... THROUGH ...
C           IGDS(18) = ... NOT USED FOR THIS GRID
CC
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO, Indy
C   LANGUAGE: IBM VS FORTRAN, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916-128, Y-MP8/864, CRAY Y-MP EL92/256
C
C$$$
C
      INTEGER igrid
      INTEGER igds(*)
      INTEGER grd1(18)
      INTEGER grd2(18)
      INTEGER grd3(18)
      INTEGER grd4(18)
      INTEGER grd5(18)
      INTEGER grd6(18)
      INTEGER grd21(18)
      INTEGER grd22(18)
      INTEGER grd23(18)
      INTEGER grd24(18)
      INTEGER grd25(18)
      INTEGER grd26(18)
      INTEGER grd27(18)
      INTEGER grd28(18)
      INTEGER grd29(18)
      INTEGER grd30(18)
      INTEGER grd33(18)
      INTEGER grd34(18)
      INTEGER grd37(91)
      INTEGER grd38(91)
      INTEGER grd39(91)
      INTEGER grd40(91)
      INTEGER grd41(91)
      INTEGER grd42(91)
      INTEGER grd43(91)
      INTEGER grd44(91)
      INTEGER grd45(18)
C     INTEGER       GRD50 (18)
      INTEGER grd55(18)
      INTEGER grd56(18)
      INTEGER grd61(18)
      INTEGER grd62(18)
      INTEGER grd63(18)
      INTEGER grd64(18)
      INTEGER grd85(18)
      INTEGER grd86(18)
      INTEGER grd87(18)
      INTEGER grd90(18)
      INTEGER grd91(18)
      INTEGER grd92(18)
      INTEGER grd93(18)
      INTEGER grd94(18)
      INTEGER grd95(18)
      INTEGER grd96(18)
      INTEGER grd97(18)
      INTEGER grd98(18)
      INTEGER grd99(18)
      INTEGER grd100(18)
      INTEGER grd101(18)
      INTEGER grd103(18)
      INTEGER grd104(18)
      INTEGER grd105(18)
      INTEGER grd106(18)
      INTEGER grd107(18)
      INTEGER grd126(18)
      INTEGER grd192(18)
      INTEGER grd194(18)
      INTEGER grd196(18)
      INTEGER grd201(18)
      INTEGER grd202(18)
      INTEGER grd203(18)
      INTEGER grd204(18)
      INTEGER grd205(18)
      INTEGER grd206(18)
      INTEGER grd207(18)
      INTEGER grd208(18)
      INTEGER grd209(18)
      INTEGER grd210(18)
      INTEGER grd211(18)
      INTEGER grd212(18)
      INTEGER grd213(18)
      INTEGER grd214(18)
      INTEGER grd215(18)
      INTEGER grd216(18)
      INTEGER grd218(18)
      INTEGER grd236(18)
      INTEGER grd250(18)
      INTEGER grd251(18)
      INTEGER grd252(18)
      INTEGER grd253(18)
      INTEGER grd254(18)
      INTEGER grd255(18)
      INTEGER grd256(18)
      INTEGER grd257(18)
      INTEGER grd258(18)
C
      SAVE
C
      DATA grd1 /0, 255, 1, 73, 23, -48090, 0, 128, 48090, 0, 513669, 
     +            513669, 22500, 64, 0, 0, 0, 0/
      DATA grd2 /0, 255, 0, 144, 73, 90000, 0, 128, -90000, -2500, 2500,
     +            2500, 0, 0, 0, 0, 0, 0/
      DATA grd3 /0, 255, 0, 360, 181, -90000, 0, 128, 90000, -1000, 
     +            1000, 1000, 0, 0, 0, 0, 0, 0/
      DATA grd4 /0, 255, 0, 720, 361, 90000, 0, 128, -90000, -500, 500,
     +            500, 0, 0, 0, 0, 0, 0/
      DATA grd5 /0, 255, 5, 53, 57, 7647, -133443, 8, -105000, 190500, 
     +            190500, 0, 64, 0, 0, 0, 0, 0/
      DATA grd6 /0, 255, 5, 53, 45, 7647, -133443, 8, -105000, 190500, 
     +            190500, 0, 64, 0, 0, 0, 0, 0/
      DATA grd21 /0, 255, 0, 37, 37, 0, 0, 128, 90000, 180000, 2500, 
     +            5000, 64, 0, 0, 0, 0, 0/
      DATA grd22 /0, 255, 0, 37, 37, 0, -180000, 128, 90000, 0, 2500, 
     +            5000, 64, 0, 0, 0, 0, 0/
      DATA grd23 /0, 255, 0, 37, 37, -90000, 0, 128, 0, 180000, 2500, 
     +            5000, 64, 0, 0, 0, 0, 0/
      DATA grd24 /0, 255, 0, 37, 37, -90000, -180000, 128, 0, 0, 2500, 
     +            5000, 64, 0, 0, 0, 0, 0/
      DATA grd25 /0, 255, 0, 72, 19, 0, 0, 128, 90000, 355000, 5000, 
     +            5000, 64, 0, 0, 0, 0, 0/
      DATA grd26 /0, 255, 0, 72, 19, -90000, 0, 128, 0, 355000, 5000, 
     +            5000, 64, 0, 0, 0, 0, 0/
      DATA grd27 /0, 255, 5, 65, 65, -20286, -125000, 8, -80000, 381000,
     +            381000, 0, 64, 0, 0, 0, 0, 0/
      DATA grd28 /0, 255, 5, 65, 65, 20286, 145000, 8, 100000, 381000, 
     +            381000, 128, 64, 0, 0, 0, 0, 0/
      DATA grd29 /0, 255, 0, 145, 37, 0, 0, 128, 90000, 360000, 2500, 
     +            2500, 64, 0, 0, 0, 0, 0/
      DATA grd30 /0, 255, 0, 145, 37, -90000, 0, 128, 0, 360000, 2500, 
     +            2500, 64, 0, 0, 0, 0, 0/
      DATA grd33 /0, 255, 0, 181, 46, 0, 0, 128, 90000, 360000, 2000, 
     +            2000, 64, 0, 0, 0, 0, 0/
      DATA grd34 /0, 255, 0, 181, 46, -90000, 0, 128, 0, 360000, 2000, 
     +            2000, 64, 0, 0, 0, 0, 0/
      DATA grd37 /0, 33, 0, 65535, 73, 0, -30000, 128, 90000, 60000, 
     +            1250, 65535, 64, 0, 0, 0, 0, 0, 73, 73, 73, 73, 73, 
     +            73, 73, 73, 72, 72, 72, 71, 71, 71, 70, 70, 69, 69, 
     +            68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60, 59, 
     +            58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 
     +            43, 42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 
     +            25, 23, 22, 20, 19, 17, 16, 14, 12, 11, 9, 8, 6, 5, 3,
     +            2/
      DATA grd38 /0, 33, 0, 65535, 73, 0, 60000, 128, 90000, 150000, 
     +            1250, 65535, 64, 0, 0, 0, 0, 0, 73, 73, 73, 73, 73, 
     +            73, 73, 73, 72, 72, 72, 71, 71, 71, 70, 70, 69, 69, 
     +            68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60, 59, 
     +            58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 
     +            43, 42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 
     +            25, 23, 22, 20, 19, 17, 16, 14, 12, 11, 9, 8, 6, 5, 3,
     +            2/
      DATA grd39 /0, 33, 0, 65535, 73, 0, 150000, 128, 90000, -120000, 
     +            1250, 65535, 64, 0, 0, 0, 0, 0, 73, 73, 73, 73, 73, 
     +            73, 73, 73, 72, 72, 72, 71, 71, 71, 70, 70, 69, 69, 
     +            68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60, 59, 
     +            58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 
     +            43, 42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 
     +            25, 23, 22, 20, 19, 17, 16, 14, 12, 11, 9, 8, 6, 5, 3,
     +            2/
      DATA grd40 /0, 33, 0, 65535, 73, 0, -120000, 128, 90000, -30000, 
     +            1250, 65535, 64, 0, 0, 0, 0, 0, 73, 73, 73, 73, 73, 
     +            73, 73, 73, 72, 72, 72, 71, 71, 71, 70, 70, 69, 69, 
     +            68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60, 59, 
     +            58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 
     +            43, 42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 
     +            25, 23, 22, 20, 19, 17, 16, 14, 12, 11, 9, 8, 6, 5, 3,
     +            2/
      DATA grd41 /0, 33, 0, 65535, 73, -90000, -30000, 128, 0, 60000, 
     +            1250, 65535, 64, 0, 0, 0, 0, 0, 2, 3, 5, 6, 8, 9, 11,
     +            12, 14, 16, 17, 19, 20, 22, 23, 25, 26, 28, 29, 30, 
     +            32, 33, 35, 36, 38, 39, 40, 42, 43, 44, 45, 47, 48, 
     +            49, 50, 51, 52, 54, 55, 56, 57, 58, 59, 60, 60, 61, 
     +            62, 63, 64, 65, 65, 66, 67, 67, 68, 69, 69, 70, 70, 
     +            71, 71, 71, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73
     +            /
      DATA grd42 /0, 33, 0, 65535, 73, -90000, 60000, 128, 0, 150000, 
     +            1250, 65535, 64, 0, 0, 0, 0, 0, 2, 3, 5, 6, 8, 9, 11,
     +            12, 14, 16, 17, 19, 20, 22, 23, 25, 26, 28, 29, 30, 
     +            32, 33, 35, 36, 38, 39, 40, 42, 43, 44, 45, 47, 48, 
     +            49, 50, 51, 52, 54, 55, 56, 57, 58, 59, 60, 60, 61, 
     +            62, 63, 64, 65, 65, 66, 67, 67, 68, 69, 69, 70, 70, 
     +            71, 71, 71, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73
     +            /
      DATA grd43 /0, 33, 0, 65535, 73, -90000, 150000, 128, 0, -120000,
     +            1250, 65535, 64, 0, 0, 0, 0, 0, 2, 3, 5, 6, 8, 9, 11,
     +            12, 14, 16, 17, 19, 20, 22, 23, 25, 26, 28, 29, 30, 
     +            32, 33, 35, 36, 38, 39, 40, 42, 43, 44, 45, 47, 48, 
     +            49, 50, 51, 52, 54, 55, 56, 57, 58, 59, 60, 60, 61, 
     +            62, 63, 64, 65, 65, 66, 67, 67, 68, 69, 69, 70, 70, 
     +            71, 71, 71, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73
     +            /
      DATA grd44 /0, 33, 0, 65535, 73, -90000, -120000, 128, 0, -30000,
     +            1250, 65535, 64, 0, 0, 0, 0, 0, 2, 3, 5, 6, 8, 9, 11,
     +            12, 14, 16, 17, 19, 20, 22, 23, 25, 26, 28, 29, 30, 
     +            32, 33, 35, 36, 38, 39, 40, 42, 43, 44, 45, 47, 48, 
     +            49, 50, 51, 52, 54, 55, 56, 57, 58, 59, 60, 60, 61, 
     +            62, 63, 64, 65, 65, 66, 67, 67, 68, 69, 69, 70, 70, 
     +            71, 71, 71, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73
     +            /
      DATA grd45 /0, 255, 0, 288, 145, 90000, 0, 128, -90000, -1250, 
     +            1250, 1250, 0, 0, 0, 0, 0, 0/
      DATA grd55 /0, 255, 5, 87, 71, -10947, -154289, 8, -105000, 
     +            254000, 254000, 0, 64, 0, 0, 0, 0, 0/
      DATA grd56 /0, 255, 5, 87, 71, 7647, -133443, 8, -105000, 127000,
     +            127000, 0, 64, 0, 0, 0, 0, 0/
      DATA grd61 /0, 255, 0, 91, 46, 0, 0, 128, 90000, 180000, 2000, 
     +            2000, 64, 0, 0, 0, 0, 0/
      DATA grd62 /0, 255, 0, 91, 46, 0, -180000, 128, 90000, 0, 2000, 
     +            2000, 64, 0, 0, 0, 0, 0/
      DATA grd63 /0, 255, 0, 91, 46, 0, -90000, 128, 0, 180000, 2000, 
     +            2000, 64, 0, 0, 0, 0, 0/
      DATA grd64 /0, 255, 0, 91, 46, -90000, -180000, 128, 0, 0, 2000, 
     +            2000, 64, 0, 0, 0, 0, 0/
      DATA grd85 /0, 255, 0, 360, 90, 500, 500, 128, 89500, 359500, 
     +            1000, 1000, 64, 0, 0, 0, 0, 0/
      DATA grd86 /0, 255, 0, 360, 90, -89500, 500, 128, -500, 359500, 
     +            1000, 1000, 64, 0, 0, 0, 0, 0/
      DATA grd87 /0, 255, 5, 81, 62, 22876, -120491, 8, -105000, 68153,
     +            68153, 0, 64, 0, 0, 0, 0, 0/
      DATA grd90 /0, 255, 201, 12902, 1, 182, -149887, 136, 92, 141, 
     +            577, 538, 64, 5200, 24900, 0, 0, 0/
      DATA grd91 /0, 255, 202, 25803, 1, 182, -149887, 136, 183, 141, 
     +            577, 538, 64, 5200, 24900, 0, 0, 0/
      DATA grd92 /0, 255, 201, 24162, 1, 9678, -128826, 136, 127, 191, 
     +            278, 263, 64, 4100, 26300, 0, 0, 0/
      DATA grd93 /0, 255, 202, 48323, 1, 9678, -128826, 136, 253, 191, 
     +            278, 263, 64, 4100, 26300, 0, 0, 0/
      DATA grd94 /0, 255, 201, 48916, 1, 9678, -128826, 136, 181, 271, 
     +            194, 185, 64, 4100, 26300, 0, 0, 0/
      DATA grd95 /0, 255, 202, 97831, 1, 9678, -128826, 136, 361, 271, 
     +            194, 185, 64, 4100, 26300, 0, 0, 0/
      DATA grd96 /0, 255, 201, 41630, 1, -3441, -148799, 136, 160, 261,
     +            333, 308, 64, 5000, 24900, 0, 0, 0/
      DATA grd97 /0, 255, 202, 319, 261, -3441, -148799, 136, 319, 261,
     +            333, 308, 64, 5000, 24900, 0, 0, 0/
      DATA grd98 /0, 255, 4, 192, 94, 88542, 0, 128, -88542, -938, 47, 
     +            1875, 0, 0, 0, 0, 0, 0/
      DATA grd99 /0, 255, 201, 45873, 1, 17912, -98303, 136, 156, 295, 
     +            97, 95, 64, 3300, 27700, 0, 0, 0/
      DATA grd100 /0, 255, 5, 83, 83, 17108, -129296, 8, -105000, 91452,
     +            91452, 0, 64, 0, 0, 0, 0, 0/
      DATA grd101 /0, 255, 5, 113, 91, 10528, -137146, 8, -105000, 
     +            91452, 91452, 0, 64, 0, 0, 0, 0, 0/
      DATA grd103 /0, 255, 5, 65, 56, 22405, -121352, 8, -105000, 91452,
     +            91452, 0, 64, 0, 0, 0, 0, 0/
      DATA grd104 /0, 255, 5, 147, 110, -268, -139475, 8, -105000, 
     +            90755, 90755, 0, 64, 0, 0, 0, 0, 0/
      DATA grd105 /0, 255, 5, 83, 83, 17529, -129296, 8, -105000, 90755,
     +            90755, 0, 64, 0, 0, 0, 0, 0/
      DATA grd106 /0, 255, 5, 165, 117, 17533, -129296, 8, -105000, 
     +            45373, 45373, 0, 64, 0, 0, 0, 0, 0/
      DATA grd107 /0, 255, 5, 120, 92, 23438, -120168, 8, -105000, 
     +            45373, 45373, 0, 64, 0, 0, 0, 0, 0/
      DATA grd126 /0, 255, 4, 384, 190, 89277, 0, 128, -89277, -938, 95,
     +            938, 0, 0, 0, 0, 0, 0/
      DATA grd192 /0, 255, 201, 45903, 1, 29366, -126316, 136, 151, 305,
     +            67, 66, 64, 4000, 24500, 0, 0, 0/
      DATA grd194 /0, 255, 201, 46963, 1, 46155, -172712, 136, 163, 289,
     +            99, 97, 64, 6300, 21000, 0, 0, 0/
      DATA grd196 /0, 255, 201, 45903, 1, 23476, -96745, 136, 151, 305,
     +            67, 66, 64, 3400, 27400, 0, 0, 0/
      DATA grd201 /0, 255, 5, 65, 65, -20826, -150000, 8, -105000, 
     +            381000, 381000, 0, 64, 0, 0, 0, 0, 0/
      DATA grd202 /0, 255, 5, 65, 43, 7838, -141028, 8, -105000, 190500,
     +            190500, 0, 64, 0, 0, 0, 0, 0/
      DATA grd203 /0, 255, 5, 45, 39, 19132, -185837, 8, -150000, 
     +            190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA grd204 /0, 255, 1, 93, 68, -25000, 110000, 128, 60644, 
     +            -109129, 160000, 160000, 20000, 64, 0, 0, 0, 0/
      DATA grd205 /0, 255, 5, 45, 39, 616, -84904, 8, -60000, 190500, 
     +            190500, 0, 64, 0, 0, 0, 0, 0/
      DATA grd206 /0, 255, 3, 51, 41, 22289, -117991, 8, -95000, 81271,
     +            81271, 0, 64, 0, 25000, 25000, 0, 0/
      DATA grd207 /0, 255, 5, 49, 35, 42085, -175641, 8, -150000, 95250,
     +            95250, 0, 64, 0, 0, 0, 0, 0/
      DATA grd208 /0, 255, 1, 29, 27, 9343, -167315, 128, 28092, -
     +            145878, 80000, 80000, 20000, 64, 0, 0, 0, 0/
      DATA grd209 /0, 255, 3, 101, 81, 22289, -117991, 8, -95000, 40635,
     +            40635, 0, 64, 0, 25000, 25000, 0, 0/
      DATA grd210 /0, 255, 1, 25, 25, 9000, -77000, 128, 26422, -58625,
     +            80000, 80000, 20000, 64, 0, 0, 0, 0/
      DATA grd211 /0, 255, 3, 93, 65, 12190, -133459, 8, -95000, 81271,
     +            81271, 0, 64, 0, 25000, 25000, 0, 0/
      DATA grd212 /0, 255, 3, 185, 129, 12190, -133459, 8, -95000, 
     +            40635, 40635, 0, 64, 0, 25000, 25000, 0, 0/
      DATA grd213 /0, 255, 5, 129, 85, 7838, -141028, 8, -105000, 95250,
     +            95250, 0, 64, 0, 0, 0, 0, 0/
      DATA grd214 /0, 255, 5, 97, 69, 42085, -175641, 8, -150000, 47625,
     +            47625, 0, 64, 0, 0, 0, 0, 0/
      DATA grd215 /0, 255, 3, 369, 257, 12190, -133459, 8, -95000, 
     +            20318, 20318, 0, 64, 0, 25000, 25000, 0, 0/
      DATA grd216 /0, 255, 5, 139, 107, 30000, -173000, 8, -135000, 
     +            45000, 45000, 0, 64, 0, 0, 0, 0, 0/
      DATA grd218 /0, 255, 3, 737, 513, 12190, -133459, 8, -95000, 
     +            10159, 10159, 0, 64, 0, 25000, 25000, 0, 0/
      DATA grd236/ 0, 255, 3, 151,113,  16281, -126138,   8,  -95000,
     &   40635,  40635, 0, 64, 0, 25000, 25000, 0, 0/
      DATA grd250 /0, 255, 0, 181, 91, 15000, -140000, 128, 60000, 
     +            -50000, 500, 500, 64, 0, 0, 0, 0, 0/
      DATA grd251 /0, 255, 0, 121, 71, 35000, -180000, 128, 70000, 
     +            -120000, 500, 500, 64, 0, 0, 0, 0, 0/
      DATA grd252 /0, 255, 0, 101, 71, 20000, -160000, 128, 55000, 
     +            -110000, 500, 500, 64, 0, 0, 0, 0, 0/
      DATA grd253 /0, 255, 0, 111, 71, 25000, -85000, 128, 60000, 
     +            -30000, 500, 500, 64, 0, 0, 0, 0, 0/
      DATA grd254 /0, 255, 0, 61, 41, 35000, -80000, 128, 55000, -50000,
     +            500, 500, 64, 0, 0, 0, 0, 0/
      DATA grd255 /0, 255, 0, 151, 76, 10000, 180000, 128, 85000, 
     +            -30000, 1000, 1000, 64, 0, 0, 0, 0, 0/
      DATA grd256 /0, 255, 0, 145, 25, 20000, 0, 128, 80000, 360000, 
     +            2500, 2500, 64, 0, 0, 0, 0, 0/
      DATA grd257 /0, 255, 0, 145, 25, -80000, 0, 128, -20000, 360000, 
     +            2500, 2500, 64, 0, 0, 0, 0, 0/
      DATA grd258 /0, 255, 0, 145, 17, -20000, 0, 128, 20000, 360000, 
     +            2500, 2500, 64, 0, 0, 0, 0, 0/
C
      ierr = 0
C     
      DO 10 i = 1, 18
        igds(i) = 0
   10 CONTINUE
C     
      IF (igrid.ge.37.and.igrid.le.44) THEN
        DO 20 i = 19, 91
          igds(i) = 0
   20   CONTINUE
      END IF
C     
      IF (igrid.eq.1) THEN
        DO 30 i = 1, 14
          igds(i) = grd1(i)
   30   CONTINUE
C     
      ELSE IF (igrid.eq.2) THEN
        DO 40 i = 1, 14
          igds(i) = grd2(i)
   40   CONTINUE
C     
      ELSE IF (igrid.eq.3) THEN
        DO 50 i = 1, 14
          igds(i) = grd3(i)
   50   CONTINUE
C     
      ELSE IF (igrid.eq.4) THEN
        DO 60 i = 1, 14
          igds(i) = grd4(i)
   60   CONTINUE
C     
      ELSE IF (igrid.eq.5) THEN
        DO 70 i = 1, 14
          igds(i) = grd5(i)
   70   CONTINUE
C     
      ELSE IF (igrid.eq.6) THEN
        DO 80 i = 1, 14
          igds(i) = grd6(i)
   80   CONTINUE
C     
      ELSE IF (igrid.eq.21) THEN
        DO 90 i = 1, 14
          igds(i) = grd21(i)
   90   CONTINUE
C     
      ELSE IF (igrid.eq.22) THEN
        DO 100 i = 1, 14
          igds(i) = grd22(i)
  100   CONTINUE
C     
      ELSE IF (igrid.eq.23) THEN
        DO 110 i = 1, 14
          igds(i) = grd23(i)
  110   CONTINUE
C     
      ELSE IF (igrid.eq.24) THEN
        DO 120 i = 1, 14
          igds(i) = grd24(i)
  120   CONTINUE
C     
      ELSE IF (igrid.eq.25) THEN
        DO 130 i = 1, 14
          igds(i) = grd25(i)
  130   CONTINUE
C     
      ELSE IF (igrid.eq.26) THEN
        DO 140 i = 1, 14
          igds(i) = grd26(i)
  140   CONTINUE
C     
      ELSE IF (igrid.eq.27) THEN
        DO 150 i = 1, 14
          igds(i) = grd27(i)
  150   CONTINUE
C     
      ELSE IF (igrid.eq.28) THEN
        DO 160 i = 1, 14
          igds(i) = grd28(i)
  160   CONTINUE
C     
      ELSE IF (igrid.eq.29) THEN
        DO 170 i = 1, 14
          igds(i) = grd29(i)
  170   CONTINUE
C     
      ELSE IF (igrid.eq.30) THEN
        DO 180 i = 1, 14
          igds(i) = grd30(i)
  180   CONTINUE
C     
      ELSE IF (igrid.eq.33) THEN
        DO 190 i = 1, 14
          igds(i) = grd33(i)
  190   CONTINUE
C     
      ELSE IF (igrid.eq.34) THEN
        DO 200 i = 1, 14
          igds(i) = grd34(i)
  200   CONTINUE
C     
      ELSE IF (igrid.eq.37) THEN
        DO 210 i = 1, 91
          igds(i) = grd37(i)
  210   CONTINUE
C     
      ELSE IF (igrid.eq.38) THEN
        DO 220 i = 1, 91
          igds(i) = grd38(i)
  220   CONTINUE
C     
      ELSE IF (igrid.eq.39) THEN
        DO 230 i = 1, 91
          igds(i) = grd39(i)
  230   CONTINUE
C     
      ELSE IF (igrid.eq.40) THEN
        DO 240 i = 1, 91
          igds(i) = grd40(i)
  240   CONTINUE
C     
      ELSE IF (igrid.eq.41) THEN
        DO 250 i = 1, 91
          igds(i) = grd41(i)
  250   CONTINUE
C     
      ELSE IF (igrid.eq.42) THEN
        DO 260 i = 1, 91
          igds(i) = grd42(i)
  260   CONTINUE
C     
      ELSE IF (igrid.eq.43) THEN
        DO 270 i = 1, 91
          igds(i) = grd43(i)
  270   CONTINUE
C     
      ELSE IF (igrid.eq.44) THEN
        DO 280 i = 1, 91
          igds(i) = grd44(i)
  280   CONTINUE
      ELSE IF (igrid.eq.45) THEN
        DO 290 i = 1, 14
          igds(i) = grd45(i)
  290   CONTINUE
C     
C     ELSE IF (IGRID.EQ.50) THEN
C     DO 150 I = 1,14
C     IGDS(I) = GRD50(I)
C150  CONTINUE
C     
      ELSE IF (igrid.eq.55) THEN
        DO 300 i = 1, 14
          igds(i) = grd55(i)
  300   CONTINUE
C     
      ELSE IF (igrid.eq.56) THEN
        DO 310 i = 1, 14
          igds(i) = grd56(i)
  310   CONTINUE
C     
      ELSE IF (igrid.eq.61) THEN
        DO 320 i = 1, 14
          igds(i) = grd61(i)
  320   CONTINUE
C     
      ELSE IF (igrid.eq.62) THEN
        DO 330 i = 1, 14
          igds(i) = grd62(i)
  330   CONTINUE
C     
      ELSE IF (igrid.eq.63) THEN
        DO 340 i = 1, 14
          igds(i) = grd63(i)
  340   CONTINUE
C     
      ELSE IF (igrid.eq.64) THEN
        DO 350 i = 1, 14
          igds(i) = grd64(i)
  350   CONTINUE
C     
      ELSE IF (igrid.eq.85) THEN
        DO 360 i = 1, 14
          igds(i) = grd85(i)
  360   CONTINUE
C     
      ELSE IF (igrid.eq.86) THEN
        DO 370 i = 1, 14
          igds(i) = grd86(i)
  370   CONTINUE
C     
      ELSE IF (igrid.eq.87) THEN
        DO 380 i = 1, 14
          igds(i) = grd87(i)
  380   CONTINUE
C     
      ELSE IF (igrid.eq.90) THEN
        DO 390 i = 1, 18
          igds(i) = grd90(i)
  390   CONTINUE
C     
      ELSE IF (igrid.eq.91) THEN
        DO 400 i = 1, 18
          igds(i) = grd91(i)
  400   CONTINUE
C     
      ELSE IF (igrid.eq.92) THEN
        DO 410 i = 1, 18
          igds(i) = grd92(i)
  410   CONTINUE
C     
      ELSE IF (igrid.eq.93) THEN
        DO 420 i = 1, 18
          igds(i) = grd93(i)
  420   CONTINUE
C     
      ELSE IF (igrid.eq.94) THEN
        DO 430 i = 1, 18
          igds(i) = grd94(i)
  430   CONTINUE
C     
      ELSE IF (igrid.eq.95) THEN
        DO 440 i = 1, 18
          igds(i) = grd95(i)
  440   CONTINUE
C     
      ELSE IF (igrid.eq.96) THEN
        DO 450 i = 1, 18
          igds(i) = grd96(i)
  450   CONTINUE
C     
      ELSE IF (igrid.eq.97) THEN
        DO 460 i = 1, 18
          igds(i) = grd97(i)
  460   CONTINUE
C     
      ELSE IF (igrid.eq.98) THEN
        DO 470 i = 1, 14
          igds(i) = grd98(i)
  470   CONTINUE
C     
      ELSE IF (igrid.eq.99) THEN
        DO i = 1, 18
          igds(i) = grd99(i)
        END DO
C     
      ELSE IF (igrid.eq.100) THEN
        DO 480 i = 1, 14
          igds(i) = grd100(i)
  480   CONTINUE
C     
      ELSE IF (igrid.eq.101) THEN
        DO 490 i = 1, 14
          igds(i) = grd101(i)
  490   CONTINUE
C     
      ELSE IF (igrid.eq.103) THEN
        DO 500 i = 1, 14
          igds(i) = grd103(i)
  500   CONTINUE
C     
      ELSE IF (igrid.eq.104) THEN
        DO 510 i = 1, 14
          igds(i) = grd104(i)
  510   CONTINUE
C     
      ELSE IF (igrid.eq.105) THEN
        DO 520 i = 1, 14
          igds(i) = grd105(i)
  520   CONTINUE
C     
      ELSE IF (igrid.eq.106) THEN
        DO 530 i = 1, 14
          igds(i) = grd106(i)
  530   CONTINUE
C     
      ELSE IF (igrid.eq.107) THEN
        DO 540 i = 1, 14
          igds(i) = grd107(i)
  540   CONTINUE
C     
      ELSE IF (igrid.eq.126) THEN
        DO 550 i = 1, 14
          igds(i) = grd126(i)
  550   CONTINUE
C     
      ELSE IF (igrid.eq.192) THEN
        DO i = 1, 18
          igds(i) = grd192(i)
        END DO
C     
      ELSE IF (igrid.eq.194) THEN
        DO i = 1, 18
          igds(i) = grd194(i)
        END DO
C     
      ELSE IF (igrid.eq.196) THEN
        DO i = 1, 18
          igds(i) = grd196(i)
        END DO
C     
      ELSE IF (igrid.eq.201) THEN
        DO 560 i = 1, 14
          igds(i) = grd201(i)
  560   CONTINUE
C     
      ELSE IF (igrid.eq.202) THEN
        DO 570 i = 1, 14
          igds(i) = grd202(i)
  570   CONTINUE
C     
      ELSE IF (igrid.eq.203) THEN
        DO 580 i = 1, 14
          igds(i) = grd203(i)
  580   CONTINUE
C     
      ELSE IF (igrid.eq.204) THEN
        DO 590 i = 1, 14
          igds(i) = grd204(i)
  590   CONTINUE
C     
      ELSE IF (igrid.eq.205) THEN
        DO 600 i = 1, 14
          igds(i) = grd205(i)
  600   CONTINUE
C     
      ELSE IF (igrid.eq.206) THEN
        DO 610 i = 1, 18
          igds(i) = grd206(i)
  610   CONTINUE
C     
      ELSE IF (igrid.eq.207) THEN
        DO 620 i = 1, 14
          igds(i) = grd207(i)
  620   CONTINUE
C     
      ELSE IF (igrid.eq.208) THEN
        DO 630 i = 1, 14
          igds(i) = grd208(i)
  630   CONTINUE
C     
      ELSE IF (igrid.eq.209) THEN
        DO 640 i = 1, 18
          igds(i) = grd209(i)
  640   CONTINUE
C     
      ELSE IF (igrid.eq.210) THEN
        DO 650 i = 1, 14
          igds(i) = grd210(i)
  650   CONTINUE
C     
      ELSE IF (igrid.eq.211) THEN
        DO 660 i = 1, 18
          igds(i) = grd211(i)
  660   CONTINUE
C     
      ELSE IF (igrid.eq.212) THEN
        DO 670 i = 1, 18
          igds(i) = grd212(i)
  670   CONTINUE
C     
      ELSE IF (igrid.eq.213) THEN
        DO 680 i = 1, 14
          igds(i) = grd213(i)
  680   CONTINUE
C     
      ELSE IF (igrid.eq.214) THEN
        DO 690 i = 1, 14
          igds(i) = grd214(i)
  690   CONTINUE
C     
      ELSE IF (igrid.eq.215) THEN
        DO 700 i = 1, 18
          igds(i) = grd215(i)
  700   CONTINUE
C     
      ELSE IF (igrid.eq.216) THEN
        DO 710 i = 1, 18
          igds(i) = grd216(i)
  710   CONTINUE
C     
      ELSE IF (igrid.eq.218) THEN
        DO 720 i = 1, 18
          igds(i) = grd218(i)
  720   CONTINUE
C
      ELSE IF (IGRID.EQ.236) THEN
        DO I = 1,18
          IGDS(I) = GRD236(I)
        ENDDO
C     
      ELSE IF (igrid.eq.250) THEN
        DO 730 i = 1, 14
          igds(i) = grd250(i)
  730   CONTINUE
C     
      ELSE IF (igrid.eq.251) THEN
        DO 740 i = 1, 14
          igds(i) = grd251(i)
  740   CONTINUE
C     
      ELSE IF (igrid.eq.252) THEN
        DO 750 i = 1, 14
          igds(i) = grd252(i)
  750   CONTINUE
C     
      ELSE IF (igrid.eq.253) THEN
        DO 760 i = 1, 14
          igds(i) = grd253(i)
  760   CONTINUE
C     
      ELSE IF (igrid.eq.254) THEN
        DO 770 i = 1, 14
          igds(i) = grd254(i)
  770   CONTINUE
C     
      ELSE IF (igrid.eq.255) THEN
        DO 780 i = 1, 14
          igds(i) = grd255(i)
  780   CONTINUE
C     
      ELSE IF (igrid.eq.256) THEN
        DO 790 i = 1, 14
          igds(i) = grd256(i)
  790   CONTINUE
C     
      ELSE IF (igrid.eq.257) THEN
        DO 800 i = 1, 14
          igds(i) = grd257(i)
  800   CONTINUE
C     
      ELSE IF (igrid.eq.258) THEN
        DO 810 i = 1, 14
          igds(i) = grd258(i)
  810   CONTINUE
      ELSE
        ierr = 1
      END IF
C     
      RETURN
      END
