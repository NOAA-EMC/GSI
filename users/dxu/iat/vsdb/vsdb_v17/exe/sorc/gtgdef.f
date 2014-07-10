      SUBROUTINE gtgdef(igrid,istat,imax,jmax,alat1,elon1,dxx,dyy,elonv,
     +            alatan,latlong,lambert,polarstereo)
C                .      .    .                                       .
C SUBPROGRAM:    GTGDEF      RETRIEVE grid definition parameters
C   PRGMMR: Geoff  DiMego    ORG: W/NP22     DATE: 97-12-29
C
C ABSTRACT: RETRIEVES grid definition parameters 
C
C PROGRAM HISTORY LOG:
C   98-01-04  Geoff DiMego   Brand new code
C   2006-10-10 Binbin Z.     Add grid#255 for HYSPLIT MODEL
C

C USAGE:    CALL GTGDEF(IGRID,ISTAT,IMAX,JMAX,ALAT1,ELON1,
C    1    DXX,DYY,ELONV,ALATAN,LATLONG,LAMBERT,POLARSTEREO)
C
C   INPUT ARGUMENT LIST:
C     IGRID    - INTEGER NUMBER OF desired grid
C
C   OUTPUT ARGUMENT LIST:
C     ISTAT    - INTEGER =0  MEANS SUCCESSFUL COMPLETION
C                        = 1 MEANS GRID NOT currently supported
C     IMAX,JMAX- DIMENSIONS OF GRID
C     ALAT1,ELON1 LOCATION OF ORIGIN PT(1,1)
C     DXX,DYY  - MESH LENGTHS
C     ELONV    - VERTICAL LONGITUDE
C     ALATAN   - REFERENCE LATITUDE (FOR LAMBERT)
C     LATLONG,LAMBERT,POLARSTEREO  PROJECTION-TYPE SWITCHES
C            WHERE LATLONG,LAMBERT,POLARSTEREO are type LOGICAL
C
C REMARKS:

C ATTRIBUTES:
C   LANGUAGE: FORTRAN-77
C   MACHINE:  CRAY C-90
C$$$
      LOGICAL latlong, lambert, polarstereo

      INTEGER kgds(91)
C
      istat = 0
C     
C     USE W3FI71 WITH THE INPUT GRID NUMBER TO GET THE PDS
C     FROM WHICH TO EXTRACT THE GRID DEFINITION PARAMETERS
C
      if(igrid.ne.255) then     
       CALL w3fi71(igrid,kgds,istat)
       IF (istat.ne.0) RETURN
      else
         kgds(1+2)=0
         kgds(2+2)=801
         kgds(3+2)=534
         kgds(4+2)=0
         kgds(5+2)=-175000
         kgds(6+2)=0
         kgds(7+2)=79950
         kgds(8+2)=-55000
         kgds(9+2)=65534
         kgds(10+2)=65534
         kgds(11+2)=64
         kgds(12+2)=0
         kgds(13+2)=0
         kgds(14+2)=0
         kgds(15+2)=167772159
         kgds(16+2)=-268415528
         kgds(17+2)=0
         kgds(18+2)=0
         kgds(19+2)=1
         kgds(20+2)=255
         kgds(21+2)=0
         kgds(22+2)=0
         kgds(23+2)=0
         kgds(24+2)=0
         kgds(25+2)=0
       end if
C     
C     FILL IN GRIDEF COMMON BLOCK
C     W3FI71 RETURNS 2 EXTRA VALUES AT BEGINNING OF ARRAY KGDS
C     THE FOLLOWING DEFINED REGARDLESS OF GRID PROJECTION
C     
      imax = kgds(2+2)
      jmax = kgds(3+2)
C     
C     USE KGDS(1+2) TO DETERMINE GRID PROJECTION
C     
C     KGDS(1+2) = 0 ----> LATITUDE/LONGITUDE
C     KGDS(1+2) = 1 ----> MERCATOR (NOT YET USED)
C     KGDS(1+2) = 3 ----> LAMBERT CONFORMAL
C     KGDS(1+2) = 5 ----> POLAR STEREOGRAPHIC
C     
      IF (kgds(1+2).eq.0) THEN
        latlong = .true.
        lambert = .false.
        polarstereo = .false.
      ELSE IF (kgds(1+2).eq.3) THEN
        latlong = .false.
        lambert = .true.
        polarstereo = .false.
      ELSE IF (kgds(1+2).eq.5) THEN
        latlong = .false.
        lambert = .false.
        polarstereo = .true.
      ELSE
        iret = 99
        WRITE (6,*) ' KGDS(1+2) = ', kgds(1+2)
        WRITE (6,*) ' GRID CAN NOT BE USED IN THIS CODE IRET= ', iret
        istat = 1
        RETURN
      END IF
C     
C     SET THE REST OF THE GRID PARAMETERS BASED ON PROJECTION TYPE
C     
C     Change has been made for LATLON definition --- Yuejian Zhu
C     Changed back with checking -- K. Brill
C     
      IF (latlong) THEN
        alat1 = kgds(4+2) * 0.001
        elon1 = kgds(5+2) * 0.001
	IF ( elon1 .lt. 0.0 ) elon1 = elon1 + 360
        elonv = 0.0
        alatan = 0.0
        dyy = kgds(9+2) * 0.001
        dxx = kgds(10+2) * 0.001
      END IF
C     
      IF (lambert) THEN
        alat1 = kgds(4+2) * 0.001
        elon1 = kgds(5+2) * 0.001
	IF ( elon1 .lt. 0.0 ) elon1 = elon1 + 360
        elonv = kgds(7+2) * 0.001
	IF ( elonv .lt. 0.0 ) elonv = elonv + 360
        alatan = kgds(12+3) * 0.001
        dxx = kgds(8+2) * 0.001
        dyy = kgds(9+2) * 0.001
      END IF
C     
      IF (polarstereo) THEN
        alat1 = kgds(4+2) * 0.001
        elon1 = kgds(5+2) * 0.001
	IF ( elon1 .lt. 0.0 ) elon1 = elon1 + 360
        elonv = kgds(7+2) * 0.001
	IF ( elonv .lt. 0.0 ) elonv = elonv + 360
        alatan = 0.0
        dxx = kgds(8+2) * 0.001
        dyy = kgds(9+2) * 0.001
      END IF
C     
      PRINT *, 'gridspecs ', lambert, alat1, elon1, elonv, alatan, dxx,
     +            dyy
C     
      WRITE (6,*) ' GREETINGS FROM THE GRID-DEFINITION CODE! '
      WRITE (6,*) ' THE GRID YOU HAVE CHOSEN IS NUMBER ', igrid
      IF (latlong) THEN
        WRITE (6,*) ' A LAT/LON GRID WITH RES= ', dxx, ' BY ', dyy, 
     +              ' DEG'
      ELSE IF (polarstereo) THEN
        WRITE (6,*) ' A POLAR STEREO GRID CENTERED AT ', elonv, ' DEG E'
        WRITE (6,*) ' AND A HORIZONTAL RESOLUTION OF ', dxx, ' KM'
      ELSE IF (lambert) THEN
        WRITE (6,*) ' A LAMBERT CONFORMAL GRID CENTERED AT ', elonv, 
     +              ' DEG E'
        WRITE (6,*) ' AND A HORIZONTAL RESOLUTION OF ', dxx, ' KM'
      END IF
      WRITE (6,*) ' HORIZONTAL DIMENSIONS ARE ', imax, ' X', jmax

      RETURN
      END
