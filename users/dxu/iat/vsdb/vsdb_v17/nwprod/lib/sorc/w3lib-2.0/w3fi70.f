       SUBROUTINE W3FI70(PDS,CNST,IER)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: W3FI70         COMPUTES SCALING CONSTANTS USED BY GRDPRT
C   AUTHOR: STACKPOLE,J.     ORG: W342       DATE: 93-10-16
C   AUTHOR: JONES,R.E.
C
C ABSTRACT: COMPUTES THE FOUR SCALING CONSTANTS USED BY GRDPRT, W3FP03,
C   OR W3FP05 FROM THE 28 BYTE (PDS) PRODUCT DEFINITION SECTION OF 
C   GRIB EDITION ONE.
C
C PROGRAM HISTORY LOG:
C   91-10-26  R.E.JONES
C   93-03-29  R.E.JONES   ADD SAVE STATEMENT
C   93-08-08  R.E.JONES   ADD 156 (CIN), 158 (TKE) TO TABLES
C   93-10-16  R.E.JONES   CHANGES FOR O.N. 388  VER. OCT. 8,1993
C
C USAGE:  CALL W3FI70(PDS,CNST,IER)
C
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     PDS    ARG LIST  28 BYTE (PDS) GRIB PRODUCT DEFINITION SECTION
C
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     CNST   ARG LIST  4 CONSTANT'S USED BY GRDPRT,W3FP05, OR W3FP03
C     IER    ARG LIST  0 = NORMAL RETURN
C                      1 = 
C
C   SUBPROGRAMS CALLED:
C     NAMES                                                   LIBRARY
C     ------------------------------------------------------- --------
C     W3FI69                                                  W3LIB
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO, Indy
C
C$$$
C
C     SET DEFAULT VALUES FOR NMC FIELDS  GRID  PRINTING
C
      REAL           CNST(4)
C
      INTEGER        ID(25)
      INTEGER        Q
C
      CHARACTER * 1  PDS(28) 
C
      SAVE
C
C     UNPACK 28 BYTE (PDS) INTO 25 INTEGER WORDS
C
      CALL W3FI69(PDS,ID)
C
      IER    = 0
C     
C     INDICATOR OF PARAMETER AND UNITS
C
      Q      = ID(8)
C     
C     INDICATOR OF LEVEL OR LAYERS
C
      ITYPES = ID(9)
      I9     = ID(9)
C     
C     HEIGHTS, PRESSURE, ETC. OF THE LEVEL OR LAYER
C
      IF ((I9.GE.1.AND.I9.LE.100).OR.I9.EQ.102.OR.
     &     I9.EQ.103.OR.I9.EQ.105.OR.I9.EQ.107.OR.
     &     I9.EQ.109.OR.I9.EQ.111.OR.I9.EQ.113.OR.
     &     I9.EQ.125.OR.I9.EQ.160.OR.I9.EQ.200.OR.
     &     I9.EQ.201) THEN
        ILVL = ID(11)
      ELSE 
        ILVL = ID(10)
      END IF

      IF (Q.EQ.1.OR.Q.EQ.2.OR.Q.EQ.26) THEN
C
C***  PRESSURE, PRESSURE REDUCED TO MSL, PRESSURE ANOMALY (Pa) 
C
        CNST(1) =   0.0
        CNST(2) =   0.01
        CNST(3) =   4.0
        CNST(4) =   0.0
C***    IF SFC, TROPOPAUSE PRESSURE, SIGMA ..
        IF (ITYPES.EQ.1.OR.ITYPES.EQ.6.OR.ITYPES.EQ.7)CNST(3)=25.0
        IF (ITYPES.EQ.107) CNST(3) = 25.0
C
      ELSE IF (Q.EQ.3) THEN
C
C***  PRESSURE TENDENCY    (Pa/s)
C
        CNST(1) = 0.0
        CNST(2) = 1.0
        CNST(3) = 4.0
        CNST(4) = 0.0
C
      ELSE IF (Q.EQ.6) THEN
C
C***  GEOPOTENTIAL          (m**2/s**2)
C
        CNST(1) = 0.0
        CNST(2) = 1.0
        CNST(3) = 4.0
        CNST(4) = 0.0
C
      ELSE IF (Q.EQ.7.OR.Q.EQ.8.OR.Q.EQ.27.OR.Q.EQ.222) THEN
C
C***  GEOPOTENTIAL, GEOPOTENTIAL HEIGHT, ANOMALY
C***  5-WAVE GEOPOTENTIAL HEIGHT   ............
C
        CNST(3) = 60.
        IF (ILVL.LT.500) CNST(3) = 120.
C***    IF SFC OR TROPOPAUSE PRESSURE ..
        IF ((ITYPES.EQ.1) .OR. (ITYPES.EQ.7)) CNST(3) = 500.0
        IF (ITYPES.EQ.107) CNST(3) = 500.0

        CNST(1) = 0.0
        CNST(2) = 1.0
        CNST(4) = 0.0
        IF (CNST(3) .EQ. 500.) CNST(4) = 2.0
C
      ELSE IF (Q.EQ.11.OR.Q.EQ.12.OR.Q.EQ.13.OR.Q.EQ.14.OR.
     &         Q.EQ.15.OR.Q.EQ.16.OR.Q.EQ.17.OR.Q.EQ.18.OR.
     &         Q.EQ.25.OR.Q.EQ.85) THEN
C

C*** TEMPERATURES                            (deg. K)
C*** VIRTUAL TEMPERATURE                     (deg. K)
C*** POTENTIAL TEMPERATURE                   (deg. K)
C*** PSEUDO-ADIABATIC POTENTIAL TEMPERATURE  (deg. K)
C*** MAXIMUN TEMPERATURE                     (deg. K)
C*** MINUMUN TEMPERATURE                     (deg. K)
C*** DEW POINT TEMPERATURE                   (deg. K)
C*** DEW POINT DEPRESSION (OR DEFICIT)       (deg. K)
C
C*** TEMP (DEG K) CONVERT TO DEG C, EXCEPT POTENTIAL TEMPERATURE
C
C       CNST(1) = -273.15
        CNST(1) = 0.0
        CNST(2) = 1.0
        CNST(3) = 5.0
        CNST(4) = 0.0
        IF (Q.EQ.13) CNST(1) = 0.0
C
      ELSE IF (Q.EQ.19) THEN
C
C***  LAPSE RATE, deg. K/m ...............
C
        CNST(1) = 0.0
        CNST(2) = 1.0
        CNST(3) = 4.0
        CNST(4) = 0.0
C
      ELSE IF (Q.EQ.21.OR.Q.EQ.22.OR.Q.EQ.23) THEN
C
C***  RADAR SPECTRA (1), (2), (3) ...............
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.28.OR.Q.EQ.29.OR.Q.EQ.30) THEN
C
C***  WAVE SPECTRA (1), (2), (3) ...............
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.31) THEN
C
C***   WIND DIRECTION  (deg. true) 
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0 
C
      ELSE IF (Q.EQ.32.OR.Q.EQ.33.OR.Q.EQ.34) THEN
C
C***   WIND SPEED, U-COMPONENT OF WIND,
C***   V-COMPONENT OF WIND   m/s -------------------
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        IF (ITYPES.EQ.1.AND.ILVL.EQ.0) CNST(3) = 3.0
        IF (ITYPES.EQ.107) CNST(3) = 3.0
        CNST(4) =  0.0 
C
      ELSE IF (Q.EQ.35.OR.Q.EQ.36) THEN
C
C***  STREAM FUNCTION, VELOCITY POTENTIAL   (m**2/s) 
C***  STREAM FUNCTION OR VELOCITY POTENTIAL (m**2/s) CONVERTED TO M.
C***  CONVERT TO METERS.    (M*M/SEC  * FOG)
C
        CNST(1) = 0.
        CNST(2) = 1.03125E-4 / 9.8
        CNST(3) = 60.
        CNST(4) = 0.
C
      ELSE IF (Q.EQ.37) THEN
C
C***  MONTGOMERY STREAM FUNCTION   (m**2/s**2)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) =  2.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.38) THEN
C
C***  SIGMA COORD. VERTICAL VELOCITY (/s) TO MICROBARS/SEC
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) =  2.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.39) THEN
C
C***  VERTICAL VELOCITY (Pa/s) TO MICROBARS/SEC
C***  SIGN CHANGED SUCH THAT POSITIVE VALUES INDICATE UPWARD MOTION.
C
        CNST(1) =   0.0
        CNST(2) =  -1.E1
        CNST(3) =   2.0
        CNST(4) =   0.0
C
      ELSE IF (Q.EQ.40) THEN
C
C***  GEOMETRIC VERTICAL VELOCITY  -DZDT-  (m/s)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.41.OR.Q.EQ.42.OR.Q.EQ.43.OR.Q.EQ.44.OR.
     &         Q.EQ.45.OR.Q.EQ.46) THEN
C
C***  ABSOLUTE VORTICITY          -ABS-V (/s)
C***  ABSOLUTE DIVERGENCE         -ABS-V (/s)
C***  RELATIVE VORTICITY          -REL-V (/s)
C***  RELATIVE DIVERGENCE         -REL-D (/s)
C***  VERTICAL U-COMPONENT SHEAR  -VUCSH (/s)
C***  VERTICAL V-COMPONENT SHEAR  -VVCSH (/s)
C
        CNST(1) =  0.0
        CNST(2) =  1.0E+6
        CNST(3) =  40.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.47) THEN
C
C***  DIRECTION OF CURRENT  -DIR-C (deg. true)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.48.OR.Q.EQ.49.OR.Q.EQ.50) THEN
C
C***  SPEED OF CURRENT                (m/s)
C***  U AND V COMPONENTS OF CURRENT   (m/s)
C
        CNST(1) = 0.
        CNST(2) = 1.
        CNST(3) = 2.
        CNST(4) = 0.
C
      ELSE IF (Q.EQ.51.OR.Q.EQ.53) THEN
C
C***  SPECIFIC HUMIDITY        SPF H (kg/kg)
C***  HUMIDITY MIXING RATIO    MIXR  (kg/kg)
C
        CNST(1) =  0.0
        CNST(2) =  1.E+3
        CNST(3) =  2.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.52) THEN
C
C***  RELATIVE HUMIDITY  R H  (%)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 20.0
        CNST(4) =  0.0
C 
      ELSE IF (Q.EQ.54.OR.Q.EQ.57.OR.Q.EQ.58) THEN
C
C***  PRECIPITABLE WATER (kg/m**2) OR .1 GRAM/CM*CM OR MILLIMETERS/CM*CM
C***  CHANGE TO CENTI-INCHES/CM*CM
C***  EVAPERATION
C***  CLOUD ICE  (kg/m**2)
C
        CNST(1) =  0.0
        CNST(2) =  3.937
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.55.OR.Q.EQ.56) THEN
C
C***  VAPOR PRESSURE  VAPP, SATURATION DEFICIT SAT D   (Pa)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.59) THEN
C
C***  PRECIPITATION RATE   (kg/m**2/s)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 20.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.60) THEN
C
C***  THUNDERSTORM PROBABILITY   (%)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 20.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.61.OR.Q.EQ.62.OR.Q.EQ.63.OR.Q.EQ.64.OR.
     &         Q.EQ.65) THEN
C
C***  TOTAL PRECIPITATION                A PCP  (kg/m**2)
C***  LARGE SCALE PRECIPITATION          NCPCP  (kg/m**2)
C***  CONVECTIVE PRECIPITATION           ACPCP  (kg/m**2)
C***  SNOWFALL RATE WATER EQUIVALENT     SRWEQ  (kg/m**2/s)
C***  WATER EQUIV. OF ACCUM. SNOW DEPTH  WEASD  (kg/m**2)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) =  2.0
        CNST(4) =  0.0

      ELSE IF (Q.EQ.66) THEN
C
C***  SNOW DEPTH (METERS)   (1 or 0) for snow or no snow
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) =  1.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.67.OR.Q.EQ.68.OR.Q.EQ.69.OR.Q.EQ.70) THEN
C
C***  MIXING LAYER DEPTH          MIXHT (m) 
C***  TRANSIENT THEMOCLINE DEPTH  TTHDP (m) 
C***  MAIN THERMOCLINE DEPTH      MTHCD (m) 
C***  MAIN THERMOCLINE ANOMALY    MTHCA (m) 
C
        CNST(1) =  0.0
        CNST(2) = 39.37
        CNST(3) = 06.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.120.OR.Q.EQ.121) THEN
C
C***  WAVE COMPONENT OF GEOPOTENTIAL   (GEOP M)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.71.OR.Q.EQ.72.OR.Q.EQ.73.OR.Q.EQ.74.OR.
     &         Q.EQ.75) THEN
C
C***  TOTAL CLOUD COVER       T CDC (%)
C***  CONVECTIVE CLOUD COVER  CDCON (%)
C***  LOW CLOUD COVER         L CDC (%)
C***  MEDIUM CLOUD COVER      M CDC (%)
C***  HIGH CLOUD COVER        H CDC (%)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.76) THEN
C
C***  CLOUD WATER  -C-WAT (kg/m**2)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.78) THEN
C
C*** CONVECTIVE SNOW   -C-SNO (kg/m**2)
C
        CNST(1) =   0.0
        CNST(2) =   1.0 
        CNST(3) =  10.0
        CNST(4) =   0.0
C
      ELSE IF (Q.EQ.79) THEN
C
C*** LARGE SCALE SNOW  -LSSNO (kg/m**2)
C
        CNST(1) =   0.0
        CNST(2) =   0.1 
        CNST(3) = 500.0
        CNST(4) =   0.0
C
      ELSE IF (Q.EQ.80) THEN
C
C***  WATER TEMPERAUTER  -WTMP-  (deg. K)
C
        CNST(1) = 0.0
        CNST(2) = 1.0
        CNST(3) = 2.0
        CNST(4) = 0.0
C
      ELSE IF (Q.EQ.81) THEN
C
C***  LAND/SEA   (1=LAND; 0=SEA)
C***  ICE CONCENTRATION (ICE=1; NO ICE=0)
C
        CNST(1) = 0.0
        CNST(2) = 1.0
        CNST(3) = 1.0
        CNST(4) = 0.5
C
      ELSE IF (Q.EQ.82.OR.Q.EQ.83.OR.Q.EQ.92.OR.Q.EQ.97) THEN
C
C***  DEVIATION OF SEA LEVEL FROM MEAN  (m)
C***  SUFACE ROUGHNESS                  (m)
C***  ICE THICKNESS                     (m)
C***  ICE GROWTH                        (m)    
C
        CNST(1) = 0.0
        CNST(2) = 1.0
        CNST(3) = 2.0
        CNST(4) = 0.0
C
      ELSE IF (Q.EQ.84) THEN
C
C***  ALBEDO  (%)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.86) THEN
C
C***  SOIL MOISTURE CONTENT  (kg/m**2) -SOILM
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.87) THEN
C
C***  VEGETATION   -VEG-  (%)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.88) THEN
C
C***  SALINITY  -SALTY-   (kg/kg)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.89) THEN
C
C***  DENSITY  -DEN--     (kg/m**3)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.90) THEN
C
C***  WATER RUNOFF -WAT-R  (kg/m**2)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.93) THEN
C
C***  DIRECTION OF ICE DRIFT  -DICED (deg. true)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.94.OR.Q.EQ.95.OR.Q.EQ.96) THEN
C
C***  SPEED OF ICE DRIFT       -SICED  (m/s)
C***  U-COMPONENT OF ICE DRIFT -U-ICE  (m/s)
C***  V-COMPONENT OF ICE DRIFT -V-ICE  (m/s)
C
        CNST(1) = 0.0
        CNST(2) = 1.0
        CNST(3) = 2.0
        CNST(4) = 0.0
C
      ELSE IF (Q.EQ.98) THEN
C
C***  ICE DIVERGENCE    -ICE D (/s)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.99) THEN
C
C***  SNO MELT          -SNO- M (kg/m**2)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.100.OR.Q.EQ.102.OR.Q.EQ.105) THEN
C
C***  HEIGHT OF WIND DRIVEN OCEAN WAVES, SEA SWELLS, OR COMBINATION
C***  (m)
C
        CNST(1) = 0.0
        CNST(2) = 1.0
        CNST(3) = 1.0
        CNST(4) = 0.0
C
      ELSE IF (Q.EQ.101.OR.Q.EQ.104.OR.Q.EQ.107.OR.Q.EQ.109) THEN
C
C***  DIRECTION OF WIND WAVES, SWELLS WAVES, PRIMARY WAVE, SECONDARY
C***  WAVE (deg. true)  -------------------- 
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 20.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.103.OR.Q.EQ.106.OR.Q.EQ.108.OR.Q.EQ.110) THEN
C
C***  MEAN PERIOD OF WIND WAVES, SWELLS WAVES, PRIMARY WAVE, SECONDARY
C***  WAVE (s)  -------------------- 
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) =  2.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.111.OR.Q.EQ.112.OR.Q.EQ.113.OR.Q.EQ.114.OR.
     &         Q.EQ.115.OR.Q.EQ.116.OR.Q.EQ.117.OR.Q.EQ.121.OR.
     &         Q.EQ.122.OR.Q.EQ.123) THEN
C
C***  NET SHORTWAVE RADITION (SURFACE)        -NSWRS  w/m **2
C***  NET LONGWAVE  RADITION (SURFACE)        -SHTFL  w/m**2
C***  NET SHORTWAVE RADITION (TOP OF ATOMS.)  -NSWRT  w/m**2
C***  NET LONGWAVE  RADITION (TOP OF ATOMS.)  -NLWRT  w/m**2
C***  LONG  WAVE RADITION                     -LWAVR  w/m**2
C***  SHORT WAVE RADITION                     -SWAVE  w/m**2
C***  GLOBAL RADITION                         -G-RAD  w/m**2
C***  LATENT HEAT FLUX                        -LHTFL  w/m**2
C***  SENSIBLE HEAT FLUX                      -SHTFL  w/m**2
C***  BOUNDARY LAYER DISSIPATION              -BLYDP  w/m**2
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) =  5.0
        IF (Q.EQ.114) CNST(3) = 20.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.127) THEN
C
C     IMAGE DATA  -IMG-D  
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.128) THEN
C
C     Mean Sea Level Pressure  -MSLSA  (Pa)
C     (Standard Atmosphere Reduction)
C
        CNST(1) =  0.0
        CNST(2) =  0.01
        CNST(3) =  4.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.129) THEN
C
C     Mean Sea Level Pressure  -MSLMA  (Pa)
C     (Maps System Reduction)
C
        CNST(1) =  0.0
        CNST(2) =  0.01
        CNST(3) =  4.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.130) THEN
C
C     Mean Sea Level Pressure -MSLET  (Pa)
C     (ETA Model Reduction)
C
        CNST(1) =  0.0
        CNST(2) =  0.01
        CNST(3) =  4.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.131.OR.Q.EQ.132.OR.Q.EQ.133.OR.Q.EQ.134) THEN
C
C***  SURFACE LIFTED INDEX         ..(DEG K)    
C***  BEST (4 LAYER) LIFTED INDEX  ..(DEG K)     
C***  K INDEX                      ..(DEG K)   TO DEG C.
C***  SWEAT INDEX                  ..(DEG K)   TO DEG C.
C
        IF (Q.EQ.131.OR.Q.EQ.132) THEN
          CNST(1) = 0.0
        ELSE
          CNST(1) = -273.15
        END IF
        CNST(2) =  1.0
        CNST(3) =  4.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.135) THEN
C
C***  HORIZONTIAL MOISTURE DIVERGENCE  (KG/KG/S)  -MCONV
C
        CNST(1) = 0.0
        CNST(2) = 1.E+8
        CNST(3) = 10.0
        CNST(4) = 0.0
C
      ELSE IF (Q.EQ.136) THEN
C
C***  VERTICAL SPEED SHEAR (1/SEC)...  TO BE CONVERTED TO KNOTS/1000 FT
C
        CNST(1) =   0.0
        CNST(2) = 592.086
        CNST(3) =   2.0
        CNST(4) =   0.0
C
      ELSE IF (Q.EQ.137) THEN
C
C***  3-hr pressure tendency (TSLSA)  (Pa/s)
C
        CNST(1) =    0.0
        CNST(2) = 1000.0
        CNST(3) =   10.0
        CNST(4) =    0.0
C
      ELSE IF (Q.EQ.156) THEN
C
C***  CONVECTIVE INHIBITION  -CIN--  (J/kg)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.157) THEN
C
C***  CONVECTIVE AVAILABLE POTENTIAL ENERGY -CAPE- (J/kg)
C
        CNST(1) =   0.0
        CNST(2) =   1.0
        CNST(3) = 500.0
        CNST(4) =   0.0
C
      ELSE IF (Q.EQ.158) THEN
C
C***  TURBULENT KINETIC ENERGY -TKE--   (J/kg)
C
        CNST(1) = 0.0
        CNST(2) = 1.0
        CNST(3) = 100.0
        CNST(4) = 0.0
C
      ELSE IF (Q.EQ.175) THEN
C
C***  MODEL LAYER NUMBER (FROM BOTTOM UP) -SGLYR  (non-dim)
C
        CNST(1) = 0.0
        CNST(2) = 1.0
        CNST(3) = 1.0
        CNST(4) = 0.0
C
      ELSE IF (Q.EQ.176) THEN
C
C***  LATITUDE (-90 TO +90) -NLAT- (deg)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.177) THEN
C
C***  EAST LATITUDE (0-360) -ELON- (deg)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.201) THEN
C
C***  ICE-FREE WATER SURFACE -ICWAT  (%)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.204) THEN
C
C***  DOWNWARD SHORT WAVE RAD. FLUX  -DSWRF  (W/m**2)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.205) THEN
C
C***  DOWNWARD LONG WAVE RAD. FLUX  -DLWRF  (W/m**2)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.207) THEN
C
C***  MOISTURE AVAILABILITY  -MSTAV (%)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.208) THEN
C
C***  EXCHANGE COEFFICIENT -SFEXC (kg/m**3)(m/s)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
CC
      ELSE IF (Q.EQ.209) THEN
C
C***  NO. OF MIXED LAYERS NEXT TO SURFACE -MIXLY (integer)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.211) THEN
C
C***  UPWARD SHORT WAVE RAD. FLUX  -USWRF  (W/m**2)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.212) THEN
C
C***  UPWARD LONG WAVE RAD. FLUX  -ULWRF  (W/m**2)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.213) THEN
C
C***  AMOUNT OF NON-CONVECTIVE CLOUD  -CDLYR (%)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.216) THEN
C
C***  TEMPERATURE TENDENCY BY ALL RADIATION  -TTRAD (Deg. K/s)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.218) THEN
C
C***  PRECIP. INDEX (0.0-1.00) -PREIX  (note will look like %)
C
        CNST(1) =  0.0
        CNST(2) = 100.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
      ELSE IF (Q.EQ.220) THEN
C
C***  NATURAL LOG OF SURFACE PRESSURE -NLGSP ln(kPa)
C
        CNST(1) =  0.0
        CNST(2) =  1.0
        CNST(3) = 10.0
        CNST(4) =  0.0
C
C*** NONE OF THE ABOVE ....
C
      ELSE 
C
C     SET DEFAULT VALUES
C
        CNST(1) = 0.0
        CNST(2) = 1.0
        CNST(3) = 5.0
        CNST(4) = 0.0 
        IER = 1
      END IF
C
      RETURN
      END
