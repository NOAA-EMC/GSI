      SUBROUTINE gtgdef(igrid,istat)
C                .      .    .                                       .
C SUBPROGRAM:    GTGDEF      RETRIEVE grid definition parameters
C   PRGMMR: Geoff  DiMego    ORG: W/NP22     DATE: 97-12-29
C
C ABSTRACT: RETRIEVES grid definition parameters via common/GRIDEF/
C
C PROGRAM HISTORY LOG:
C   97-12-29  Geoff DiMego   Brand new code
C
C USAGE:    CALL GTGDEF(IGRID,ISTAT)
C
C   INPUT ARGUMENT LIST:
C     IGRID    - INTEGER NUMBER OF desired grid
C
C   OUTPUT ARGUMENT LIST:
C     ISTAT    - INTEGER =0  MEANS SUCCESSFUL COMPLETION
C                        = 1 MEANS GRID NOT currently supported
C   OUTPUT VIA COMMON BLOCK /GRIDEF/
C     COMMON /GRIDEF/IMAX,JMAX,KMAX,ALAT1,ELON1,DXX,DYY,ELONV,
C    1    ALATAN,LATLONG,LAMBERT,POLARSTEREO
C
C            WHERE LATLONG,LAMBERT,POLARSTEREO are type LOGICAL
C
C REMARKS:

C ATTRIBUTES:
C   LANGUAGE: FORTRAN-77
C   MACHINE:  CRAY C-90
C$$$
      LOGICAL latlong, lambert, polarstereo
      COMMON /gridef/ imax, jmax, kmax, alat1, elon1, dxx, dyy, elonv, 
     +            alatan, latlong, lambert, polarstereo

      INTEGER kgds(91)
C
      istat = 0
C     
C     USE W3FI71 WITH THE INPUT GRID NUMBER TO GET THE PDS
C     FROM WHICH TO EXTRACT THE GRID DEFINITION PARAMETERS
C     
      CALL w3fi71(igrid,kgds,istat)
      print*,'istat=',istat
      IF (istat.ne.0) RETURN
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
C     KGDS(1+2) = 1 ----> MERCATOR (NOT YET UESD)
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

c     do i=1,25
c      print*,'i,kgds(i)=',i,kgds(i)
c     enddo
      IF (latlong) THEN
        alat1 = kgds(4+2) * 0.001
        elon1 = kgds(5+2) * 0.001
c       print*,'alat1,elon1=',alat1,elon1
        IF (elon1.lt.0.) elon1 = elon1 + 360.0
        elonv = 0.0
        alatan = 0.0
        dyy = kgds(9+2) * 0.001
        dxx = kgds(10+2) * 0.001
        alatend=kgds(7+2) * 0.001
c       print*,'alat1,alatend=',alat1,alatend
c       if(alat1.ge.alatend) then
c        alat1 = alat1 - dyy * (jmax-1)
c        alat1 = max(-90.,alat1)
c        jj1 = jmax
c        jjinc = -1
c       endif
c       IF (igrid.eq.2.or.igrid.eq.3.or.igrid.eq.4.or.igrid.eq.181
c    *      .or.igrid.eq.182) alat1 = alat1 - (
c    +              jmax-1) * dyy
       IF (igrid.eq.2.or.igrid.eq.3.or.igrid.eq.4 )  alat1 = alat1 - (
     +              jmax-1) * dyy
c       if(alat1.ge.alatend) then
c        alat1 = alat1 - dyy * (jmax-1)
c        alat1 = max(-90.,alat1)
c        jj1 = jmax
c        jjinc = -1
c       endif
        if (igrid.eq.181.or.igrid.eq.182) alat1 = alat1 - (
     +              jmax-1) * dyy

      END IF
C     
      IF (lambert) THEN
        alat1 = kgds(4+2) * 0.001
        elon1 = kgds(5+2) * 0.001 + 360.0
        elonv = kgds(7+2) * 0.001 + 360.0
        alatan = kgds(12+3) * 0.001
        dxx = kgds(8+2) * 0.001
        dyy = kgds(9+2) * 0.001
      END IF
C     
      IF (polarstereo) THEN
        alat1 = kgds(4+2) * 0.001
        elon1 = kgds(5+2) * 0.001 + 360.0
        elonv = kgds(7+2) * 0.001 + 360.0
        alatan = 0.0
        dxx = kgds(8+2) * 0.001
        dyy = kgds(9+2) * 0.001
      END IF
C     
C     print *,'gridspecs ',LAMBERT,ALAT1,ELON1,ELONV,ALATAN,DXX,DYY
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
      WRITE (6,*) ' HORIZONTAL DIMENSIONS OF ', imax, ' X', jmax

      RETURN
      END
