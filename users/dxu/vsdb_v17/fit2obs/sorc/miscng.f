        SUBROUTINE NSFLIP(DATAI,DUMMY,IDIM,JDIM,YINT,FLAT1,PLATS)
C
        DIMENSION DATAI(IDIM,JDIM),DUMMY(IDIM,JDIM)
C
        do 1 j=1,jdim
        do 1 i=1,idim
        dummy(i,j)=0.
  1     continue
c
        DIFF=ABS(PLATS-FLAT1)/YINT
        JOFF=NINT(DIFF)+1
        JEND=JDIM-JOFF
C
        IF(FLAT1.GT.PLATS)  THEN
        DO 12 I=1,IDIM
        DO 11 J=1,JDIM
        IF(J.LE.JOFF) THEN
        JJ=JOFF+1-J
        ELSE
        JJ=JDIM+1+JOFF-J
        ENDIF
        DUMMY(I,JJ)=DATAI(I,J)
  11    CONTINUE
  12    CONTINUE
        ENDIF
C
        IF(FLAT1.LT.PLATS)  THEN
        DO 14 I=1,IDIM
        DO 13 J=1,JDIM
        IF(J.LT.JOFF) THEN
        JJ=JEND+1+J
        ELSE
        JJ=J+1-JOFF
        ENDIF
        DUMMY(I,JJ)=DATAI(I,J)
  13    CONTINUE
  14    CONTINUE
        ENDIF
C
        DO 15 J=1,JDIM
        DO 15 I=1,IDIM
        DATAI(I,J)=DUMMY(I,J)
  15    CONTINUE
C
        RETURN
        END
        SUBROUTINE EWFLIP(DATAI,DUMMY,IDIM,JDIM,
     *  XINT,FLON1,PLONW)
C
        DIMENSION DATAI(IDIM,JDIM),DUMMY(IDIM,JDIM)
C
        do 1 j=1,jdim
        do 1 i=1,idim
        dummy(i,j)=0.
  1     continue
c
        DIFF=ABS(PLONW-FLON1)/XINT
        IOFF=NINT(DIFF)
        IEND=IDIM-IOFF
C
        IF(PLONW.GT.FLON1) THEN
        DO 12 J=1,JDIM
        DO 11 I=1,IDIM
        IF(I.LE.IEND) THEN
        II=IOFF+I
        ELSE
        II=I-IEND
        ENDIF
        DUMMY(I,J)=DATAI(II,J)
  11    CONTINUE
  12    CONTINUE
        ENDIF
C
        IF(PLONW.LT.FLON1) THEN
        DO 14 J=1,JDIM
        DO 13 I=1,IDIM
        IF(I.LE.IOFF) THEN
        II=IEND+I
        ELSE
        II=I-IOFF
        ENDIF
        DUMMY(I,J)=DATAI(II,J)
  13    CONTINUE
  14    CONTINUE
        ENDIF
C
        DO 15 J=1,JDIM
        DO 15 I=1,IDIM
        DATAI(I,J)=DUMMY(I,J)
  15    CONTINUE
C
        RETURN
        END
       INTEGER FUNCTION nfill(C)
       CHARACTER*(*) C
       NFILL=LEN(C)
       DO 1 J=1,NFILL
       IF(C(J:J).EQ.' ') THEN
       NFILL=J-1
       RETURN
       ENDIF
 1     CONTINUE
       RETURN
       END
      SUBROUTINE LL2LL(REGIN ,IMXIN ,JMXIN , DLOIN, DLAIN, RLON, RLAT,
     1                 REGOUT,IMXOUT,JMXOUT)
C
C  INTERPOLATION FROM LAT/LON OR GAUSSIAN GRID TO OTHER LAT/LON GRID
C
      DIMENSION REGIN (IMXIN ,JMXIN )
      DIMENSION REGOUT(IMXOUT,JMXOUT)
C
      DIMENSION GAULI (500)
      DIMENSION RINLAT(500),OUTLAT(500)
      DIMENSION RINLON(1000)
C
      DIMENSION IINDX1(1000)
      DIMENSION IINDX2(1000)
      DIMENSION JINDX1(500)
      DIMENSION JINDX2(500)
C
      DIMENSION DDX(1000)
      DIMENSION DDY(500)
C
      DATA IFPI,JFPI,IFPO,JFPO,RFP1,RFP2/4*0,2*0./
C
      IF(IMXIN.EQ.1.OR.JMXIN.EQ.1) THEN
      DO 1 J=1,JMXOUT
      DO 1 I=1,IMXOUT
      REGOUT(I,J)=0.0
    1 CONTINUE
      RETURN
      ENDIF
C
      IF(DLOIN.EQ.0.0.OR.DLAIN.EQ.0.0) THEN
        PRINT *,'DLOIN OR DLAIN IS ZERO .... CHECK DATA CARDS'
        CALL ABORT
      ENDIF
C
      IF(IFPI.EQ.IMXIN .AND.JFPI.EQ.JMXIN .AND.
     1   IFPO.EQ.IMXOUT.AND.JFPO.EQ.JMXOUT.AND.
     2   RFP1.EQ.RLON.AND.RFP2.EQ.RLAT) GO TO 111
C
      IFPI=IMXIN
      JFPI=JMXIN
      IFPO=IMXOUT
      JFPO=JMXOUT
      RFP1=RLON
      RFP2=RLAT
C
C     PRINT *,'DLOIN=',DLOIN
C     PRINT *,'DLAIN=',DLAIN
C     PRINT *,'RLON=',RLON
C     PRINT *,'RLAT=',RLAT
C
      DO 5 J=1,JMXIN
      IF(RLAT.GT.0.0) THEN
        RINLAT(J)=RLAT-FLOAT(J-1)*DLAIN
      ELSE
        RINLAT(J)=RLAT+FLOAT(J-1)*DLAIN
      ENDIF
    5 CONTINUE
C
C     PRINT *,'RINLAT='
C     PRINT *,(RINLAT(J),J=1,JMXIN)
C
      DLAOUT=180./FLOAT(JMXOUT-1)
      DO 7 J=1,JMXOUT
      OUTLAT(J)=90.-FLOAT(J-1)*DLAOUT
    7 CONTINUE
C
C     PRINT *,'OUTLAT='
C     PRINT *,(OUTLAT(J),J=1,JMXOUT)
C
      DO 15 I=1,IMXIN
      RINLON(I)=RLON+FLOAT(I-1)*DLOIN
   15 CONTINUE
C
C     PRINT *,'RINLON='
C     PRINT *,(RINLON(I),I=1,IMXIN)
C
C  FIND I-INDEX FOR INTERPLATION
C
      DO 30 I=1,IMXOUT
      ALAMD=FLOAT(I-1)*360.0/FLOAT(IMXOUT)
      IF(RLON.LT.0.0) THEN
        IF(ALAMD.GT.180.0) ALAMD=ALAMD-360.0
      ENDIF
      DO 35 II=1,IMXIN
      IF(ALAMD.GT.RINLON(II)) GO TO 35
      IX=II
      GO TO 32
   35 CONTINUE
      I1=360.0/DLOIN+0.50
      IF(I1.GT.IMXIN) I1=IMXIN
      I2=1
      GO TO 34
   32 CONTINUE
      IF(IX.GE.2) GO TO 33
      I1=360.0/DLOIN+0.50
      IF(I1.GT.IMXIN) I1=IMXIN
      I2=1
      GO TO 34
   33 CONTINUE
      I2=IX
      I1=I2-1
   34 CONTINUE
      IINDX1(I)=I1
      IINDX2(I)=I2
      DENOM=RINLON(I2)-RINLON(I1)
      IF(DENOM.LT.0.0) DENOM=DENOM+360.0
      RNUME=ALAMD-RINLON(I1)
      IF(RNUME.LT.0.0) RNUME=RNUME+360.0
      DDX(I)=RNUME/DENOM
   30 CONTINUE
C
C  FIND J-INDEX FOR INTERPLATION
C
      JQ=1
      DO 40 J=1,JMXOUT
      APHI=OUTLAT(J)
      DO 50 JJ=1,JMXIN
      JX=JJ
      IF(RLAT.LT.0.0) JX=JMXIN-JJ+1
      IF(APHI.LT.RINLAT(JX)) GO TO 50
      JQ=JX
      GO TO 42
   50 CONTINUE
      IF(RLAT.GT.0.0) THEN
        J1=JMXIN
        J2=JMXIN
      ELSE
        J1=1
        J2=1
      ENDIF
      GO TO 44
   42 CONTINUE
      IF(RLAT.GT.0.0) THEN
        IF(JQ.GE.2) GO TO 43
        J1=1
        J2=1
      ELSE
        IF(JQ.LT.JMXIN) GO TO 43
        J1=JMXIN
        J2=JMXIN
      ENDIF
      GO TO 44
   43 CONTINUE
      IF(RLAT.GT.0.0) THEN
        J2=JQ
        J1=JQ-1
      ELSE
        J1=JQ
        J2=JQ+1
      ENDIF
   44 CONTINUE
      JINDX1(J)=J1
      JINDX2(J)=J2
      IF(J2.NE.J1) THEN
         DDY(J)=(APHI-RINLAT(J1))/(RINLAT(J2)-RINLAT(J1))
      ELSE
        IF(J1.EQ.1.AND.RLAT.GT.0.0.OR.J1.EQ.JMXIN.AND.RLAT.LT.0.0) THEN
          IF(ABS(90.0-RINLAT(J1)).GT.0.0010) THEN
            DDY(J)=(APHI-RINLAT(J1))/(90.0-RINLAT(J1))
          ELSE
            DDY(J)=0.00
          ENDIF
        ENDIF
        IF(J1.EQ.1.AND.RLAT.LT.0.0.OR.J1.EQ.JMXIN.AND.RLAT.GT.0.0) THEN
          IF(ABS(-90.0-RINLAT(J1)).GT.0.0010) THEN
            DDY(J)=(APHI-RINLAT(J1))/(-90.0-RINLAT(J1))
          ELSE
            DDY(J)=0.00
          ENDIF
        ENDIF
      ENDIF
   40 CONTINUE
C
C     PRINT *,'IINDX1'
C     PRINT *,(IINDX1(N),N=1,IMXOUT)
C     PRINT *,'IINDX2'
C     PRINT *,(IINDX2(N),N=1,IMXOUT)
C     PRINT *,'JINDX1'
C     PRINT *,(JINDX1(N),N=1,JMXOUT)
C     PRINT *,'JINDX2'
C     PRINT *,(JINDX2(N),N=1,JMXOUT)
C     PRINT *,'DDY'
C     PRINT *,(DDY(N),N=1,JMXOUT)
C     PRINT *,'DDX'
C     PRINT *,(DDX(N),N=1,JMXOUT)
C
  111 CONTINUE
C
      SUM1=0.0
      SUM2=0.0
      DO 80 I=1,IMXIN
      SUM1=SUM1+REGIN(I,1)
      SUM2=SUM2+REGIN(I,JMXIN)
   80 CONTINUE
      IF(RLAT.GT.0.0) THEN
        SUMN=SUM1/FLOAT(IMXIN)
        SUMS=SUM2/FLOAT(IMXIN)
      ELSE
        SUMS=SUM1/FLOAT(IMXIN)
        SUMN=SUM2/FLOAT(IMXIN)
      ENDIF
C
C  QUASI-BILINEAR INTERPOLATION
C
      DO 70 J=1,JMXOUT
      Y=DDY(J)
      J1=JINDX1(J)
      J2=JINDX2(J)
      DO 70 I=1,IMXOUT
      X=DDX(I)
      I1=IINDX1(I)
      I2=IINDX2(I)
C
      WI1J1=(1.0-X)*(1.0-Y)
      WI2J1=     X *(1.0-Y)
      WI1J2=(1.0-X)*      Y
      WI2J2=     X *      Y
C
      WSUM  =WI1J1+WI2J1+WI1J2+WI2J2
      WSUMIV=1./WSUM
C
      IF(J1.NE.J2) THEN
            REGOUT(I,J)=(WI1J1*REGIN(I1,J1)+WI2J1*REGIN(I2,J1)+
     1                   WI1J2*REGIN(I1,J2)+WI2J2*REGIN(I2,J2))*WSUMIV
      ELSE
        IF(J1.EQ.1.AND.RLAT.GT.0..OR.J1.EQ.JMXIN.AND.RLAT.LT.0.) THEN
            REGOUT(I,J)=(WI1J1*SUMN        +WI2J1*SUMN        +
     1                   WI1J2*REGIN(I1,J2)+WI2J2*REGIN(I2,J2))*WSUMIV
        ENDIF
        IF(J1.EQ.1.AND.RLAT.LT.0..OR.J1.EQ.JMXIN.AND.RLAT.GT.0.) THEN
            REGOUT(I,J)=(WI1J1*REGIN(I1,J1)+WI2J1*REGIN(I2,J1)+
     1                   WI1J2*SUMS        +WI2J2*SUMS        )*WSUMIV
        ENDIF
      ENDIF
C
   70 CONTINUE
C
      RETURN
      END
      subroutine grange(n,ld,d,dmin,dmax)
      logical ld
      dimension ld(n),d(n)
      dmin=1.e20
      dmax=-1.e20
      do i=1,n
        if(ld(i)) then
          dmin=min(dmin,d(i))
          dmax=max(dmax,d(i))
        endif
      enddo
      return
      end
      SUBROUTINE gridav(z,lon,lat,deg,gridv)
      dimension z(lon,lat),deg(lat)
c
c	grid averaging algorithm: spherical integration
c	not fancy but has a pole correction
c
c	algorithm:
c
c	dtheta = (deg(i+1)-deg(i)/2 + (deg(i) - deg(i-1))/2
c		= (deg(i+1) - deg(i-1))/2
c
c	integral = sum for all point: point_value * dtheta * cos(theta)
c		/ sum for all points: dtheta * cos(theta)
c
c	however there is a problem with the poles,
c	point_value*dos(theta) goes to zero
c
c	one solution: 
c	let dtheta = (90 degrees - (deg(1)+deg(2))/2)
c	and assume that f*cos(theta) is linear
c
c	and then integrate the over the interval
c	this yields an effective dtheta of
c
c	dth = 0.5 * (90.- (deg(1)+deg(2))/2)**2 / (90.- deg(1))
c	note:
c	grids must from north and work down
c
      gridv=0.
      PI=4.*ATAN(1.)
      X=0.
      W=0.
      DO 11 J=1,LAT
      COSL=COS(DEG(J)*PI/180.)
c	wne: pole problem fix
	if (j.eq.1) then
	   dth = 0.5*(90.- (deg(1)+deg(2))/2.)**2 * PI/180.
	else if (j.eq.lat) then
	   dth = 0.5*((deg(lat)+deg(lat-1))/2.+90.)**2 * PI/180.
	else
	   dth = 0.5 * (deg(j-1) - deg(j+1)) * COSL
	endif
      DO 10 I=1,LON
      X=X+Z(I,J)*DTH
      W=W+DTH
10    CONTINUE
11    CONTINUE
      GRIDV=X/W
      RETURN
      END
      subroutine gridavg(z,lon,lat,wt,gridv)
c
c.. computes accurate gaussian grid averages...
         real*4 z(lon,lat),wt(lat)
c
         gridv=0.
         do j=1,lat
         xx=0.
         do i=1,lon
         xx=xx+z(i,j)
         enddo
         xx=xx/float(lon)
         gridv=gridv+xx*wt(j)/2.
         enddo

         return
         end

      SUBROUTINE gridavm(z,lon,lat,deg,bmiss,gridv)
      dimension z(lon,lat),deg(lat)
c
c	grid averaging algorithm: spherical integration
c	not fancy but has a pole correction
c
c	algorithm:
c
c	dtheta = (deg(i+1)-deg(i)/2 + (deg(i) - deg(i-1))/2
c		= (deg(i+1) - deg(i-1))/2
c
c	integral = sum for all point: point_value * dtheta * cos(theta)
c		/ sum for all points: dtheta * cos(theta)
c
c	however there is a problem with the poles,
c	point_value*dos(theta) goes to zero
c
c	one solution: 
c	let dtheta = (90 degrees - (deg(1)+deg(2))/2)
c	and assume that f*cos(theta) is linear
c
c	and then integrate the over the interval
c	this yields an effective dtheta of
c
c	dth = 0.5 * (90.- (deg(1)+deg(2))/2)**2 / (90.- deg(1))
c	note:
c	grids must from north and work down
c
      gridv=0.
      PI=4.*ATAN(1.)
      X=0.
      W=0.
      DO 11 J=1,LAT
      COSL=COS(DEG(J)*PI/180.)
c	wne: pole problem fix
	if (j.eq.1) then
	   dth = 0.5*(90.- (deg(1)+deg(2))/2.)**2 * PI/180.
	else if (j.eq.lat) then
	   dth = 0.5*((deg(lat)+deg(lat-1))/2.+90.)**2 * PI/180.
	else
	   dth = 0.5 * (deg(j-1) - deg(j+1)) * COSL
	endif
      DO 10 I=1,LON
      if(z(i,j).ne.bmiss) then
      X=X+Z(I,J)*DTH
      W=W+DTH
      endif
10    CONTINUE
11    CONTINUE
      GRIDV=X/W
      RETURN
      END
      SUBROUTINE sectav(z,lon,lat,flatn,flats,dlat,flon1,flon2,dlon,
     *deg,gridv)
      dimension z(lon,lat),deg(lat)
c
      gridv=0.
      latn=nint(((90.-flatn)/dlat)+1.)
      lats=nint(((90.-flats)/dlat)+1.)
      nlat=lats-latn+1
      if(flon1.ge.0.) then
      lon1=nint((flon1/dlon)+1.)
      else
      lon1=nint((flon1/dlon)+float(lon+1))
      endif
      if(flon2.ge.0.) then
      lon2=nint((flon2/dlon)+1.)
      else
      lon2=nint((flon2/dlon)+float(lon+1))
      endif
c
      PI=4.*ATAN(1.)
      X=0.
      W=0.
      DO 11 J=LATN,LATS
      COSL=COS(DEG(J)*PI/180.)
c	wne: pole problem fix
	if (j.eq.1) then
	   dth = 0.5*(90.- (deg(1)+deg(2))/2.)**2 * PI/180.
	else if (j.eq.lat) then
	   dth = 0.5*((deg(lat)+deg(lat-1))/2.+90.)**2 * PI/180.
	else
	   dth = 0.5 * (deg(j-1) - deg(j+1)) * COSL
	endif
      DO 10 I=LON1,LON2
      X=X+Z(I,J)*DTH
      W=W+DTH
10    CONTINUE
11    CONTINUE
      GRIDV=X/W
      RETURN
      END
      SUBROUTINE ZDTOUV(Z,D,U,V,MEND1,IROMB,ER,MWVD2,MUWVD2)
C
C  THIS ROUTINE PERFORMS CONVERSION OF TRUE SCALAR RELATIVE
C  VORTICITY (Z), DIVERGENCE (D) TO PSEUDO SCALAR U AND V
C  IN SPECTRAL SPACE.
C
C  CALL ZDTOUV(Z,D,U,V,MEND1,IROMB,ER,MWVD2,MUWVD2)
C
C  Z      ..... RELATIVE VORTICITY
C  D      ..... DIVERGENCE
C  U      ..... OUTPUT PSEUDO SCALAR ZONAL WIND (=UCOS(PHI))
C  V      ..... OUTPUT PSEUDO SCALAR MERID WIND (=VCOS(PHI))
C  MEND1  ..... ZONAL WAVENUMBER + 1
C  IROMB  ..... TRUNCATION INDICATOR (=1 RHOMBOIDAL, =0 TRIANGULAR)
C  ER     ..... RADIUS OF EARTH
C  MWVD2  ..... DIMENSION OF Z AND D
C  MUWVD2 ..... DIMENSION OF U AND V
C
C  HUA-LU PAN   27 FEBRUARY 1989
C
      DIMENSION U(MUWVD2), V(MUWVD2), Z(MUWVD2), D(MUWVD2)
      EPS(N,M) = SQRT(FLOAT(N*N-M*M)/FLOAT(4*N*N-1))
      U(1)=0.
      U(2)=0.
      V(1)=0.
      V(2)=0.
      U(MUWVD2-1) = 0.
      U(MUWVD2  ) = 0.
      V(MUWVD2-1) = 0.
      V(MUWVD2  ) = 0.
C
C  HANDLE M = 0 SEPERATELY
C
      NM = 0
      NMU = 0
      MM = 1
      M = 0
      NEND1 = MEND1
      IF(IROMB.EQ.1) NEND1 = MM + MEND1 - 1
      D2 = 0.
      DO 100 NN = MM, NEND1+1
      NM = NM + 1
      NMU = NMU + 1
      NMT2 = NM * 2
      NMUT2 = NMU * 2
      N = NN - 1
      D1 = D2
      D2 = EPS(N+1,M)
      FAC1 = ER * D2 / FLOAT(N+1)
      IF(N.GT.0) FAC2 = ER * D1 / FLOAT(N)
      IF(N.EQ.M) THEN
        U(NMUT2-1) =  Z(NMT2+1) * FAC1
        U(NMUT2  ) =  Z(NMT2+2) * FAC1
        V(NMUT2-1) = -D(NMT2+1) * FAC1
        V(NMUT2  ) = -D(NMT2+2) * FAC1
      ELSEIF(N.GT.M.AND.N.LT.NEND1-1) THEN
        U(NMUT2-1) =  Z(NMT2+1) * FAC1 - Z(NMT2-3) * FAC2
        U(NMUT2  ) =  Z(NMT2+2) * FAC1 - Z(NMT2-2) * FAC2
        V(NMUT2-1) = -D(NMT2+1) * FAC1 + D(NMT2-3) * FAC2
        V(NMUT2  ) = -D(NMT2+2) * FAC1 + D(NMT2-2) * FAC2
      ELSE
        U(NMUT2-1) = -Z(NMT2-3) * FAC2
        U(NMUT2  ) = -Z(NMT2-2) * FAC2
        V(NMUT2-1) =  D(NMT2-3) * FAC2
        V(NMUT2  ) =  D(NMT2-2) * FAC2
      ENDIF
 100  CONTINUE
      NM = NM - 1
C
C  REST OF ZONAL WAVENUMBERS
C
      DO 300 MM = 2, MEND1
      M = MM - 1
      NEND1 = MEND1
      IF(IROMB.EQ.1) NEND1 = MM + MEND1 - 1
      D2 = 0.
      DO 200 NN = MM, NEND1+1
      NM = NM + 1
      NMU = NMU + 1
      NMT2 = NM * 2
      NMUT2 = NMU * 2
      N = NN - 1
      D1 = D2
      D2 = EPS(N+1,M)
      FAC1 = ER * D2 / FLOAT(N+1)
      FAC2 = ER * D1 / FLOAT(N)
      FAC3 = ER * FLOAT(M) / FLOAT(N * (N+1))
      IF(N.EQ.M.AND.N.LT.NEND1-1) THEN
        U(NMUT2-1) =  Z(NMT2+1) * FAC1 + D(NMT2  ) * FAC3
        U(NMUT2  ) =  Z(NMT2+2) * FAC1 - D(NMT2-1) * FAC3
        V(NMUT2-1) = -D(NMT2+1) * FAC1 + Z(NMT2  ) * FAC3
        V(NMUT2  ) = -D(NMT2+2) * FAC1 - Z(NMT2-1) * FAC3
      ELSEIF(N.GT.M.AND.N.LT.NEND1-1) THEN
        U(NMUT2-1) =  Z(NMT2+1) * FAC1 - Z(NMT2-3) * FAC2
     &              + D(NMT2  ) * FAC3
        U(NMUT2  ) =  Z(NMT2+2) * FAC1 - Z(NMT2-2) * FAC2
     &              - D(NMT2-1) * FAC3
        V(NMUT2-1) = -D(NMT2+1) * FAC1 + D(NMT2-3) * FAC2
     &              + Z(NMT2  ) * FAC3
        V(NMUT2  ) = -D(NMT2+2) * FAC1 + D(NMT2-2) * FAC2
     &              - Z(NMT2-1) * FAC3
      ELSEIF(N.EQ.NEND1-1) THEN
        U(NMUT2-1) = -Z(NMT2-3) * FAC2 + D(NMT2  ) * FAC3
        U(NMUT2  ) = -Z(NMT2-2) * FAC2 - D(NMT2-1) * FAC3
        V(NMUT2-1) =  D(NMT2-3) * FAC2 + Z(NMT2  ) * FAC3
        V(NMUT2  ) =  D(NMT2-2) * FAC2 - Z(NMT2-1) * FAC3
      ELSE
        U(NMUT2-1) = -Z(NMT2-3) * FAC2
        U(NMUT2  ) = -Z(NMT2-2) * FAC2
        V(NMUT2-1) =  D(NMT2-3) * FAC2
        V(NMUT2  ) =  D(NMT2-2) * FAC2
      ENDIF
 200  CONTINUE
      NM = NM - 1
 300  CONTINUE
      RETURN
      END
      SUBROUTINE UVTOZD(U,V,Z,D,MEND1,IROMB,ER,MWVD2,MUWVD2)
C
C  THIS ROUTINE PERFORMS CONVERSION OF PSEUDO-SCALAR U, V TO
C  TRUE SCALAR VORTICITY (Z) AND DIVERGENCE (D) IN SPECTRAL
C  SPACE.
C
C  CALL UVTOZD(U,V,Z,D,MEND1,IROMB,ER,MWVD2,MUWVD2)
C
C  CALLING ARGUEMENTS:
C
C  U      ..... PSEUDO SCALAR ZONAL WIND (= UCOS(PHI))
C  V      .....   ..    ..    MERID WIND (= VCOS(PHI))
C  Z      ..... OUTPUT OF RELATIVE VORTICITY
C  D      .....   ..   .. DIVERGENCE
C  MEND1  ..... ZONAL WAVENUMBER + 1
C  ER     ..... RADIUS OF EARTH
C  MWVD2  ..... DIMENSION OF Z AND D
C  MUWVD2 ..... DIMENSION OF U AND V
C
C  HUA-LU PAN   17 JANUARY 1989
C
      DIMENSION U(MUWVD2), V(MUWVD2), Z(MWVD2), D(MWVD2)
      COMPLEX AI, B(2,1000)
      REAL SUB(2,1000), DIAG(2,1000), SUP(2,1000)
      EPS(N,M) = SQRT(FLOAT(N*N-M*M)/FLOAT(4*N*N-1))
      AI = CMPLX(0.,1.)
      IF(MEND1.GT.1000) THEN
      WRITE(6,*) ' DIMENSION STATEMENTS IN UVTOZD NEED ADJUSTMENT'
      WRITE(6,*) ' ARRAYS DIMENSIONED 1000 MUST BECOME ',MEND1
      STOP 'ABEND 1000'
      ENDIF
C
C HANDLE M = 0 SEPERATELY
C
      NM = 0
      NMU = 0
      MM = 1
      M = 0
      NEND1 = MEND1
      IF(IROMB.EQ.1) NEND1 = MM + MEND1 - 1
      D2 = 0.
      DO 100 NN = MM, NEND1
      NM = NM + 1
      NMU = NMU + 1
      NMT2 = NM * 2
      NMUT2 = NMU * 2
      N = NN - 1
      D1 = D2
      IF(N.GT.0) THEN
      D2 = EPS(N,M)
      FAC1 = N / (ER * D2)
      ENDIF
      IF(N.GT.1) FAC2 = (N * D1) / ((N-1) * D2)
      IF(N.EQ.0) THEN
      Z(NMT2-1) = 0.
      Z(NMT2)   = 0.
      D(NMT2-1) = 0.
      D(NMT2)   = 0.
      ELSEIF(N.EQ.1) THEN
      Z(NMT2-1) =  U(NMUT2-3) * FAC1
      Z(NMT2)   =  U(NMUT2-2) * FAC1
      D(NMT2-1) = -V(NMUT2-3) * FAC1
      D(NMT2)   = -V(NMUT2-2) * FAC1
      ELSEIF(N.GT.1) THEN
      Z(NMT2-1) =  U(NMUT2-3) * FAC1 + Z(NMT2-5) * FAC2
      Z(NMT2)   =  U(NMUT2-2) * FAC1 + Z(NMT2-4) * FAC2
      D(NMT2-1) = -V(NMUT2-3) * FAC1 + D(NMT2-5) * FAC2
      D(NMT2)   = -V(NMUT2-2) * FAC1 + D(NMT2-4) * FAC2
      ENDIF
 100  CONTINUE
C
C  INCREMENT NMU BY ONE BECAUSE U, V HAVE AN EXTRA WAVENUMBER COMP.
C
      NMU = NMU + 1
C
C  SOLVE LINKED TRIDIAGONAL MATRICES FOR M GT 1
C  NOTE THAT THE INDEXING IS NOW BASED ON U AND V INSTEAD OF
C  Z AND D AS IN THE PREVIOUS LOOP
C
      DO 300 MM = 2, MEND1
      M = MM - 1
      NEND1 = MEND1
      IF(IROMB.EQ.1) NEND1 = MM + MEND1 - 1
      NDIM = NEND1 - MM + 1
      D2 = 0.
      NR = 0
      NMOLD = NM
      NMUOLD = NMU
      DO 200 NN = MM, NEND1
      NR = NR + 1
      N = NN - 1
      NM = NM + 1
      NMU = NMU + 1
      NMT2 = NM * 2
      NMUT2 = NMU * 2
      D1 = D2
      D2 = EPS(N+1,M)
      IF(N.EQ.M) THEN
        SUB(1,NR) = 0.
        SUB(2,NR) = 0.
      ELSE
        SUB(1,NR) = -D1 / N
        SUB(2,NR) = -SUB(1,NR)
      ENDIF
      B(1,NR) = CMPLX(U(NMUT2-1),U(NMUT2)) / ER
      B(2,NR) = CMPLX(V(NMUT2-1),V(NMUT2)) / ER
      DIAG(1,NR) = -FLOAT(M) / FLOAT(N * (N+1))
      DIAG(2,NR) = DIAG(1,NR)
      IF(NN.EQ.NEND1) THEN
        SUP(1,NR) = 0.
        SUP(2,NR) = 0.
      ELSE
        SUP(1,NR) = D2 / (N+1)
        SUP(2,NR) = -SUP(1,NR)
      ENDIF
 200  CONTINUE
      IF(NDIM.GT.1) THEN
        DO 210 I = 2, NDIM
        SUB(1,I) = SUB(1,I) / DIAG(2,I-1)
        SUB(2,I) = SUB(2,I) / DIAG(1,I-1)
        DIAG(1,I) = DIAG(1,I) + SUB(1,I) * SUP(2,I-1)
        DIAG(2,I) = DIAG(2,I) + SUB(2,I) * SUP(1,I-1)
        B(1,I) = B(1,I) + AI * SUB(1,I) * B(2,I-1)
        B(2,I) = B(2,I) + AI * SUB(2,I) * B(1,I-1)
 210    CONTINUE
      ENDIF
      B(1,NDIM) = -AI * B(1,NDIM) / DIAG(1,NDIM)
      B(2,NDIM) = -AI * B(2,NDIM) / DIAG(2,NDIM)
      IF(NDIM.GT.1) THEN
        DO 220 I = NDIM-1, 1, -1
        B(1,I) = -AI * (B(1,I) - SUP(1,I) * B(2,I+1)) / DIAG(1,I)
        B(2,I) = -AI * (B(2,I) - SUP(2,I) * B(1,I+1)) / DIAG(2,I)
 220    CONTINUE
      ENDIF
      NM = NMOLD
      NMU = NMUOLD
      NR = 0
      DO 230 NN = MM, NEND1
      N = NN - 1
      NM = NM + 1
      NMU = NMU + 1
      NR = NR + 1
      NMT2 = NM * 2
      NMUT2 = NMU * 2
      Z(NMT2-1) =  REAL(B(2,NR))
      Z(NMT2)   = AIMAG(B(2,NR))
      D(NMT2-1) =  REAL(B(1,NR))
      D(NMT2)   = AIMAG(B(1,NR))
 230  CONTINUE
      NMU = NMU + 1
 300  CONTINUE
C
C  MAKING SURE THAT THE IMAGINARY PART OF THE ZONAL (M=0) COMPONENT
C  IS ZERO AS WELL AS THE REAL PART OF N=M=0 IS ZERO
C
      Z(1) = 0.
      D(1) = 0.
      DO 400 N = 1, MEND1
      N2 = N * 2
      Z(N2) = 0.
      D(N2) = 0.
 400  CONTINUE
      RETURN
      END
        SUBROUTINE MAXMIN1(A,M,FMIN,IMIN,FMAX,IMAX)
C
        DIMENSION A(M)
C
        FMIN=A(1)
        FMAX=A(1)
        DO 1 I=1,M
        IF(A(I).GT.FMAX) THEN
        FMAX=A(I)
        IMAX=I
        ENDIF
        IF(A(I).LE.FMIN) THEN
        FMIN=A(I)
        IMIN=I
        ENDIF
 1      CONTINUE
C
        RETURN
        END
        SUBROUTINE MAXMIN2(A,M,N,FMIN,IMIN,JMIN,FMAX,IMAX,JMAX)
C
        DIMENSION A(M,N)
C
        FMIN=A(1,1)
        FMAX=A(1,1)
        DO 1 J=1,N
        DO 1 I=1,M
        IF(A(I,J).GT.FMAX) THEN
        FMAX=A(I,J)
        IMAX=I
        JMAX=J
        ENDIF
        IF(A(I,J).LE.FMIN) THEN
        FMIN=A(I,J)
        IMIN=I
        JMIN=J
        ENDIF
 1      CONTINUE
C
        RETURN
        END
       SUBROUTINE STRIP(GRID,IDIML,JDIML,COSL)
       DIMENSION GRID(IDIML,JDIML),COSL(JDIML)
C
       DO 1 J=2,JDIML-1
       DO 1 I=1,IDIML
       GRID(I,J)=GRID(I,J)*COSL(J)
 1     CONTINUE
C
       RETURN
       END
       SUBROUTINE INDALL(NRECP,NCHKP,KPDS5P,KPDS6P,KPDS7P,ICHKP,LEV,
     * LEVEL,ICLM1P,ICLM2P,ICLM3P,ICLM4P)
C
       DIMENSION KPDS5P(NRECP),KPDS6P(NRECP),KPDS7P(NRECP),ICHKP(NRECP)
       DIMENSION LEV(NRECP),LEVEL(16)
       DIMENSION ICLM1P(NCHKP),ICLM2P(NCHKP),ICLM3P(NCHKP),ICLM4P(NCHKP)
C
       NREC=0
C..... GEOPOTENTIAL HEIGHT...(1-12)
       DO 1 NL=1,12
       NREC=NREC+1
       KPDS5P(NREC)=7
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       LEV(NREC)=NL
       ICHKP(NREC)=1
       ICLM1P(NREC)=151
       ICLM2P(NREC)=157
       ICLM3P(NREC)=163
       ICLM4P(NREC)=169
  1    CONTINUE
C
C..... U-WIND......(13-24)
       DO 2 NL=1,12
       NREC=NREC+1
       KPDS5P(NREC)=33
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       LEV(NREC)=NL
       ICHKP(NREC)=1
       ICLM1P(NREC)=152
       ICLM2P(NREC)=158
       ICLM3P(NREC)=164
       ICLM4P(NREC)=170
  2    CONTINUE
C
C..... V-WIND......(25-36)
       DO 3 NL=1,12
       NREC=NREC+1
       KPDS5P(NREC)=34
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       LEV(NREC)=NL
       ICHKP(NREC)=1
       ICLM1P(NREC)=153
       ICLM2P(NREC)=159
       ICLM3P(NREC)=165
       ICLM4P(NREC)=171
  3    CONTINUE
C
C..... THERMODYNAMIC TEMPERATURE.....(37-48)
       DO 4 NL=1,12
       NREC=NREC+1
       KPDS5P(NREC)=11
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       LEV(NREC)=NL
       ICHKP(NREC)=1
       ICLM1P(NREC)=155
       ICLM2P(NREC)=161
       ICLM3P(NREC)=167
       ICLM4P(NREC)=173
  4    CONTINUE
C
C..... SPECIFIC HUMIDITY.....(49-54)
       DO 5 NL=1,6
       NREC=NREC+1
       KPDS5P(NREC)=52
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       LEV(NREC)=NL
       ICHKP(NREC)=1
       ICLM1P(NREC)=154
       ICLM2P(NREC)=160
       ICLM3P(NREC)=166
       ICLM4P(NREC)=172
  5    CONTINUE
C
C..... GEOPOTENTIAL HEIGHT...(55-58)
       DO 6 NL=13,16
       NREC=NREC+1
       KPDS5P(NREC)=7
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       LEV(NREC)=NL
       ICHKP(NREC)=0
  6    CONTINUE
C
C..... U-WIND......(59-62)
       DO 7 NL=13,16
       NREC=NREC+1
       KPDS5P(NREC)=33
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       LEV(NREC)=NL
       ICHKP(NREC)=0
  7    CONTINUE
C
C..... V-WIND......(63-66)
       DO 8 NL=13,16
       NREC=NREC+1
       KPDS5P(NREC)=34
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       LEV(NREC)=NL
       ICHKP(NREC)=0
  8    CONTINUE
C
C..... THERMODYNAMIC TEMPERATURE.....(67-70)
       DO 9 NL=13,16
       NREC=NREC+1
       KPDS5P(NREC)=11
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       LEV(NREC)=NL
       ICHKP(NREC)=0
  9    CONTINUE
C
C..... RELATIVE HUMIDITY......(71)
       NREC=NREC+1
       KPDS5P(NREC)=52
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(13)
       LEV(NREC)=13
       ICHKP(NREC)=0
C
C..... VERTICAL VELOCITY....(72-82)
       DO 10 NL=1,11
       NLL=NL
       NREC=NREC+1
       KPDS5P(NREC)=39
       KPDS6P(NREC)=100
       IF(NL.EQ.2) NLL=13
       IF(NL.GT.2) NLL=NL-1
       KPDS7P(NREC)=LEVEL(NLL)
       LEV(NREC)=NLL
       ICHKP(NREC)=0
 10    CONTINUE
C
C..... (RHMEAN) RELATIVE HUMIDITY MEAN.....(83)
       NREC=NREC+1
       KPDS5P(NREC)=52
       KPDS6P(NREC)=108
       KPDS7P(NREC)=10340
       LEV(NREC)=0
       ICHKP(NREC)=0
C
C..... (RHSRFC) SURFACE RELATIVE HUMIDITY.....(84)
       NREC=NREC+1
       KPDS5P(NREC)=52
       KPDS6P(NREC)=108
       KPDS7P(NREC)=16989
       LEV(NREC)=0
       ICHKP(NREC)=0
C
C..... (RHLOW) LOW RELATIVE HUMIDITY.....(85)
       NREC=NREC+1
       KPDS5P(NREC)=52
       KPDS6P(NREC)=108
       KPDS7P(NREC)=10306
       LEV(NREC)=0
       ICHKP(NREC)=0
C
C....  (PBLU)  U-WIND AT SIGMA LEVEL 1 .....(86)
       NREC=NREC+1
       KPDS5P(NREC)=33
       KPDS6P(NREC)=107
       KPDS7P(NREC)=9949
       LEV(NREC)=0
       ICHKP(NREC)=0
C
C..... (PBLV) V-WIND AT SIGMA LEVEL 1 .....(87)
       NREC=NREC+1
       KPDS5P(NREC)=34
       KPDS6P(NREC)=107
       KPDS7P(NREC)=9949
       LEV(NREC)=0
       ICHKP(NREC)=0
C
C..... (PBLT) POTENTIAL TEMPERATURE AT SIGMA LEVEL 1....(88)
       NREC=NREC+1
       KPDS5P(NREC)=13
       KPDS6P(NREC)=107
       KPDS7P(NREC)=9949
       LEV(NREC)=0
       ICHKP(NREC)=0
C
C.....(PBLW) PRESSURE VERTICAL VELOCITY AT SURFACE....(89)
       NREC=NREC+1
       KPDS5P(NREC)=39
       KPDS6P(NREC)=1
       KPDS7P(NREC)=0
       LEV(NREC)=0
       ICHKP(NREC)=0
C
C.....(PBLRH) RELATIVE HUMIDITY AT SURFACE....(90)
       NREC=NREC+1
       KPDS5P(NREC)=52
       KPDS6P(NREC)=1
       KPDS7P(NREC)=0
       LEV(NREC)=0
       ICHKP(NREC)=0
C
C.....(TROPU) U-WIND AT TROPOPAUSE LEVEL .....(91)
       NREC=NREC+1
       KPDS5P(NREC)=33
       KPDS6P(NREC)=7
       KPDS7P(NREC)=0
       LEV(NREC)=0
       ICHKP(NREC)=0
C
C.....(TROPV) V-WIND AT TROPOPAUSE LEVEL .....(92)
       NREC=NREC+1
       KPDS5P(NREC)=34
       KPDS6P(NREC)=7
       KPDS7P(NREC)=0
       LEV(NREC)=0
       ICHKP(NREC)=0
C
C.....(TROPT) THERMODYNAMIC TEMPERATURE AT TROPOPAUSE LEVEL.....(93)
       NREC=NREC+1
       KPDS5P(NREC)=11
       KPDS6P(NREC)=7
       KPDS7P(NREC)=0
       LEV(NREC)=0
       ICHKP(NREC)=0
C
C..... (TROPP) PRESSURE AT TROPOPAUSE LEVEL.....(94)
       NREC=NREC+1
       KPDS5P(NREC)=1
       KPDS6P(NREC)=7
       KPDS7P(NREC)=0
       LEV(NREC)=0
       ICHKP(NREC)=0
C
C.....(TROPWND) HORIZONTAL WIND SPEED SHEAR AT TROPOPAUSE LEVEL .....(95)
       NREC=NREC+1
       KPDS5P(NREC)=136
       KPDS6P(NREC)=7
       KPDS7P(NREC)=0
       LEV(NREC)=0
       ICHKP(NREC)=0
C
C..... (SRFCPP) SURFACE PRESSURE ...(96)
       NREC=NREC+1
       KPDS5P(NREC)=1
       KPDS6P(NREC)=1
       KPDS7P(NREC)=0
       LEV(NREC)=0
       ICHKP(NREC)=0
C
C..... (PRCPW) PRECIPITABLE WATER....(97)
       NREC=NREC+1
       KPDS5P(NREC)=54
       KPDS6P(NREC)=108
       KPDS7P(NREC)=100
       LEV(NREC)=0
       ICHKP(NREC)=0
C
C.....(SRFCTP) THERMODYNAMIC TEMPERATURE AT SURFACE....(98)
       NREC=NREC+1
       KPDS5P(NREC)=11
       KPDS6P(NREC)=1
       KPDS7P(NREC)=0
       LEV(NREC)=0
       ICHKP(NREC)=0
C
C.....(UMAX) U-WIND AT MAXIMUM WIND LEVEL....(99)
       NREC=NREC+1
       KPDS5P(NREC)=33
       KPDS6P(NREC)=6
       KPDS7P(NREC)=0
       LEV(NREC)=0
       ICHKP(NREC)=0
C
C.....(VMAX) V-WIND AT MAXIMUM WIND LEVEL...(100)
       NREC=NREC+1
       KPDS5P(NREC)=34
       KPDS6P(NREC)=6
       KPDS7P(NREC)=0
       LEV(NREC)=0
       ICHKP(NREC)=0
C
C.....(PMAX) PRESSURE AT MAXIMUM WIND LEVEL...(101)
       NREC=NREC+1
       KPDS5P(NREC)=1
       KPDS6P(NREC)=6
       KPDS7P(NREC)=0
       LEV(NREC)=0
       ICHKP(NREC)=0
C
c      do 1234 nr=1,nrecp
c      if(nr.le.nchkp) then
c      print *,kpds5p(nr),kpds6p(nr),kpds7p(nr),lev(nr),ichkp(nr),
c    * iclm1p(nr),iclm2p(nr),iclm3p(nr),iclm4p(nr),
c    * imlm1p(nr),imlm2p(nr),imlm3p(nr),imlm4p(nr),
c    * imlm5p(nr),imlm6p(nr)
c      else
c      print *,kpds5p(nr),kpds6p(nr),kpds7p(nr),lev(nr),ichkp(nr)
c      endif
c1234  continue
c
       RETURN
       END
       SUBROUTINE INDEXPF(NRECP,KPDS5P,KPDS6P,KPDS7P,LEV,
     * LEVEL,ICLM1P,ICLM2P,ICLM3P,ICLM4P)
C
       DIMENSION KPDS5P(NRECP),KPDS6P(NRECP),KPDS7P(NRECP)
       DIMENSION LEV(NRECP),LEVEL(12)
       DIMENSION ICLM1P(NRECP),ICLM2P(NRECP),ICLM3P(NRECP),ICLM4P(NRECP)
C
       NREC=0
C..... GEOPOTENTIAL HEIGHT...(1-12)
       DO 1 NL=1,12
       NREC=NREC+1
       KPDS5P(NREC)=7
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       LEV(NREC)=NL
       ICLM1P(NREC)=kpds5p(nrec)
       ICLM2P(NREC)=185
       ICLM3P(NREC)=200
       ICLM4P(NREC)=169
  1    CONTINUE
C
C..... U-WIND......(13-24)
       DO 2 NL=1,12
       NREC=NREC+1
       KPDS5P(NREC)=33
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       LEV(NREC)=NL
       ICLM1P(NREC)=kpds5p(nrec)
       ICLM2P(NREC)=186
       ICLM3P(NREC)=202
       ICLM4P(NREC)=170
  2    CONTINUE
C
C..... V-WIND......(25-36)
       DO 3 NL=1,12
       NREC=NREC+1
       KPDS5P(NREC)=34
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       LEV(NREC)=NL
       ICLM1P(NREC)=kpds5p(nrec)
       ICLM2P(NREC)=187
       ICLM3P(NREC)=203
       ICLM4P(NREC)=171
  3    CONTINUE
C
C..... THERMODYNAMIC TEMPERATURE.....(37-48)
       DO 4 NL=1,12
       NREC=NREC+1
       KPDS5P(NREC)=11
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       LEV(NREC)=NL
       ICLM1P(NREC)=kpds5p(nrec)
       ICLM2P(NREC)=189
       ICLM3P(NREC)=210
       ICLM4P(NREC)=173
  4    CONTINUE
C
C..... SPECIFIC HUMIDITY.....(49-54)
       DO 5 NL=1,6
       NREC=NREC+1
c..  going to read in relative humidity from pgb file
       KPDS5P(NREC)=52
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       LEV(NREC)=NL
c..  going to read in specific humidity from climatology file
       ICLM1P(NREC)=51
       ICLM2P(NREC)=188
       ICLM3P(NREC)=206
       ICLM4P(NREC)=172
  5    CONTINUE
C
       RETURN
       END
       SUBROUTINE INDEXPS(NRECP,KPDS5P,KPDS6P,KPDS7P,
     * LEVEL,ICLM1P,ICLM2P,ICLM3P,ICLM4P,
     * IMLM1P,IMLM2P,IMLM3P,IMLM4P,IMLM5P,IMLM6P)
C
       DIMENSION KPDS5P(NRECP),KPDS6P(NRECP),KPDS7P(NRECP)
       DIMENSION ICLM1P(NRECP),ICLM2P(NRECP),ICLM3P(NRECP),ICLM4P(NRECP)
       DIMENSION IMLM1P(NRECP),IMLM2P(NRECP),IMLM3P(NRECP),IMLM4P(NRECP)
       DIMENSION IMLM5P(NRECP),IMLM6P(NRECP)
       DIMENSION LEVEL(12)
C
       NREC=0
C..... GEOPOTENTIAL HEIGHT...(1-12)
       DO 1 NL=1,12
       NREC=NREC+1
       KPDS5P(NREC)=7
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       ICLM1P(NREC)=151
       ICLM2P(NREC)=156
       ICLM3P(NREC)=161
       ICLM4P(NREC)=166
       IMLM1P(NREC)=171
       IMLM2P(NREC)=176
       IMLM3P(NREC)=181
       IMLM4P(NREC)=186
       IMLM5P(NREC)=191
       IMLM6P(NREC)=196
  1    CONTINUE
C
C..... U-WIND......(13-24)
       DO 2 NL=1,12
       NREC=NREC+1
       KPDS5P(NREC)=33
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       ICLM1P(NREC)=152
       ICLM2P(NREC)=157
       ICLM3P(NREC)=162
       ICLM4P(NREC)=167
       IMLM1P(NREC)=172
       IMLM2P(NREC)=177
       IMLM3P(NREC)=182
       IMLM4P(NREC)=187
       IMLM5P(NREC)=192
       IMLM6P(NREC)=197
  2    CONTINUE
C
C..... V-WIND......(25-36)
       DO 3 NL=1,12
       NREC=NREC+1
       KPDS5P(NREC)=34
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       ICLM1P(NREC)=153
       ICLM2P(NREC)=158
       ICLM3P(NREC)=163
       ICLM4P(NREC)=168
       IMLM1P(NREC)=173
       IMLM2P(NREC)=178
       IMLM3P(NREC)=183
       IMLM4P(NREC)=188
       IMLM5P(NREC)=193
       IMLM6P(NREC)=198
  3    CONTINUE
C
C..... RELATIVE HUMIDITY......(37,42)
       DO 4 NL=1,6
       NREC=NREC+1
       KPDS5P(NREC)=52
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       ICLM1P(NREC)=154
       ICLM2P(NREC)=159
       ICLM3P(NREC)=164
       ICLM4P(NREC)=169
       IMLM1P(NREC)=174
       IMLM2P(NREC)=179
       IMLM3P(NREC)=184
       IMLM4P(NREC)=189
       IMLM5P(NREC)=194
       IMLM6P(NREC)=199
  4    CONTINUE
C
C..... THERMODYNAMIC TEMPERATURE.....(43-54)
       DO 5 NL=1,12
       NREC=NREC+1
       KPDS5P(NREC)=11
       KPDS6P(NREC)=100
       KPDS7P(NREC)=LEVEL(NL)
       ICLM1P(NREC)=155
       ICLM2P(NREC)=160
       ICLM3P(NREC)=165
       ICLM4P(NREC)=170
       IMLM1P(NREC)=175
       IMLM2P(NREC)=180
       IMLM3P(NREC)=185
       IMLM4P(NREC)=190
       IMLM5P(NREC)=195
       IMLM6P(NREC)=200
  5    CONTINUE
C
       RETURN
       END
       SUBROUTINE RLIM(IR,IMTH,VQOUT,VQTEN,VQGAN,NCHK)
C
       DIMENSION XQOUT(12),XQTEN(12),XQGAN(12)
       DIMENSION VQOUT(NCHK),VQTEN(NCHK),VQGAN(NCHK)
C
       REWIND IR
C
       DO 1 IREC=1,NCHK
C
       IF(IREC.EQ.37) THEN
       DO 2 NN=1,6
       READ(IR)
       READ(IR)
 2     CONTINUE
       ENDIF
       READ(IR)
       READ(IR) XQOUT,XQTEN,XQGAN
       VQOUT(IREC)=XQOUT(IMTH)
       VQTEN(IREC)=XQTEN(IMTH)
       VQGAN(IREC)=XQGAN(IMTH)
C
  1    CONTINUE
C
       RETURN
       END
       SUBROUTINE INDEXF(NRECF,KPDS5F,KPDS6F,KPDS7F,ICHKF,
     * CHKMF)
C
       Dimension KPDS5F(NRECF),KPDS6F(NRECF),KPDS7F(NRECF),
     * ICHKF(NRECF),CHKMF(NRECF)
C
C...........................FLUX FILE
       do 988 nrr=1,nrecf
       kpds5f(nrr)=0
       kpds6f(nrr)=0
       kpds7f(nrr)=0
 988   continue
c
       NREC=0
C
C.....(USTRES) SURFACE ZONAL WIND STRESS .....(1)
       NREC=NREC+1
       KPDS5F(NREC)=170
       KPDS6F(NREC)=1
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(VSRES) SURFACE MERIDIONAL WIND STRESS .....(2)
       NREC=NREC+1
       KPDS5F(NREC)=171
       KPDS6F(NREC)=1
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(SENSBH) SURFACE SENSIBLE HEAT FLUX .....(3)
       NREC=NREC+1
       KPDS5F(NREC)=122
       KPDS6F(NREC)=1
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(LATENTH) LATENT HEAT FLUX .....(4)
       NREC=NREC+1
       KPDS5F(NREC)=121
       KPDS6F(NREC)=1
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(SRFCTF) SURFACE THERMODYNAMIC TEMPERATURE .....(5)
       NREC=NREC+1
       KPDS5F(NREC)=11
       KPDS6F(NREC)=1
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(SOILM) SOIL MOISTURE CONTENT .....(6)
       NREC=NREC+1
       KPDS5F(NREC)=86
       KPDS6F(NREC)=1
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(SNOWD) SNOW DEPTH.................(7)
       NREC=NREC+1
       KPDS5F(NREC)=65
       KPDS6F(NREC)=1
       KPDS7F(NREC)=0
       ICHKF(NREC)=0
       CHKMF(NREC)=1.0
C
C.....(DLWHSFC) DOWNWARD LONGWAVE RADIATIVE HEAT FLUX AT SURFACE........(8)
       NREC=NREC+1
       KPDS5F(NREC)=205
       KPDS6F(NREC)=1
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(ULWHSFC) UPWARD LONGWAVE RADIATIVE HEAT FLUX AT SURFACE...........(9)
       NREC=NREC+1
       KPDS5F(NREC)=212
       KPDS6F(NREC)=1
       KPDS7F(NREC)=0
       ICHKF(NREC)=0
       CHKMF(NREC)=1.0
C
C.....(ULWHTOP) UPWARD LONGWAVE RADIATIVE HEAT FLUX AT THE TOP.........(10)
       NREC=NREC+1
       KPDS5F(NREC)=212
       KPDS6F(NREC)=107
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(USWHTOP) UPWARD SHORTWAVE RADIATIVE HEAT FLUX AT THE TOP......(11)
       NREC=NREC+1
       KPDS5F(NREC)=211
       KPDS6F(NREC)=107
       KPDS7F(NREC)=0
       ICHKF(NREC)=0
       CHKMF(NREC)=1.0
C
C.....(USWHSFC) UPWARD SHORTWAVE RADIATIVE HEAT FLUX AT THE SURFACE....(12)
       NREC=NREC+1
       KPDS5F(NREC)=211
       KPDS6F(NREC)=1
       KPDS7F(NREC)=0
       ICHKF(NREC)=0
       CHKMF(NREC)=1.0
C
C.....(DSWHSFC) DOWNWARD SHORTWAVE RADIATIVE HEAT FLUX AT THE SURFACE...(13)
       NREC=NREC+1
       KPDS5F(NREC)=204
       KPDS6F(NREC)=1
       KPDS7F(NREC)=0
       ICHKF(NREC)=0
       CHKMF(NREC)=1.0
C
C.....(HICLFR) HIGH CLOUD FRACTION.......(14)
       NREC=NREC+1
       KPDS5F(NREC)=71
       KPDS6F(NREC)=34
       KPDS7F(NREC)=0
       ICHKF(NREC)=0
       CHKMF(NREC)=1.0
C
C.....(HICLTOP) HIGH CLOUD TOP.......(15)
       NREC=NREC+1
       KPDS5F(NREC)=175
       KPDS6F(NREC)=33
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(HICLBOT) HIGH CLOUD BOTTOM.......(16)
       NREC=NREC+1
       KPDS5F(NREC)=175
       KPDS6F(NREC)=32
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(HICLTOPT) HIGH CLOUD TOP TEMPERATURE.......(17)
       NREC=NREC+1
       KPDS5F(NREC)=11
       KPDS6F(NREC)=33
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(MICLFR) MIDDLE CLOUD FRACTION.......(18)
       NREC=NREC+1
       KPDS5F(NREC)=71
       KPDS6F(NREC)=24
       KPDS7F(NREC)=0
       ICHKF(NREC)=0
       CHKMF(NREC)=1.0
C
C.....(MICLTOP) MIDDLE CLOUD TOP.......(19)
       NREC=NREC+1
       KPDS5F(NREC)=175
       KPDS6F(NREC)=23
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(MICLBOT) MIDDLE CLOUD BOTTOM.......(20)
       NREC=NREC+1
       KPDS5F(NREC)=175
       KPDS6F(NREC)=22
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(MICLTOPT) MIDDLE CLOUD TOP TEMPERATURE.......(21)
       NREC=NREC+1
       KPDS5F(NREC)=11
       KPDS6F(NREC)=23
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(LOCLFR) LOW CLOUD FRACTION.......(22)
       NREC=NREC+1
       KPDS5F(NREC)=71
       KPDS6F(NREC)=14
       KPDS7F(NREC)=0
       ICHKF(NREC)=0
       CHKMF(NREC)=1.0
C
C.....(LOCLTOP) LOW CLOUD TOP.......(23)
       NREC=NREC+1
       KPDS5F(NREC)=175
       KPDS6F(NREC)=13
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(LOCLBOT) LOW CLOUD BOTTOM.......(24)
       NREC=NREC+1
       KPDS5F(NREC)=175
       KPDS6F(NREC)=12
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(LOCLTOPT) LOW CLOUD TOP TEMPERATURE.......(25)
       NREC=NREC+1
       KPDS5F(NREC)=11
       KPDS6F(NREC)=13
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(TPRECIP) TOTAL PRECIPITATION.......(26)
       NREC=NREC+1
       KPDS5F(NREC)=61
       KPDS6F(NREC)=1
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(CPRECIP) CONVECTIVE PRECIPITATION.......(27)
       NREC=NREC+1
       KPDS5F(NREC)=63
       KPDS6F(NREC)=1
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(GROUNDH) GROUND HEAT FLUX.......(28)
       NREC=NREC+1
       KPDS5F(NREC)=155
       KPDS6F(NREC)=1
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(LSIMASK) LAND-SEA-ICE MASK......(29)
       NREC=NREC+1
       KPDS5F(NREC)=81
       KPDS6F(NREC)=1
       KPDS7F(NREC)=0
       ICHKF(NREC)=0
       CHKMF(NREC)=1.0
C
C.....(U10MWIND) U-WIND AT 10 METERS........(30)
       NREC=NREC+1
       KPDS5F(NREC)=33
       KPDS6F(NREC)=105
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(V10MWIND) V-WIND AT 10 METERS........(31)
       NREC=NREC+1
       KPDS5F(NREC)=34
       KPDS6F(NREC)=105
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(SIG1T) SIGMA LEVEL 1 TEMPERATURE........(32)
       NREC=NREC+1
       KPDS5F(NREC)=11
       KPDS6F(NREC)=105
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=1.0
C
C.....(SIG1Q) SIGMA LEVEL 1 SPECIFIC HUMIDITY........(33)
       NREC=NREC+1
       KPDS5F(NREC)=51
       KPDS6F(NREC)=105
       KPDS7F(NREC)=0
       ICHKF(NREC)=0
       CHKMF(NREC)=1.0
C
C.....(SRFCPR) SURFACE PRESSURE........(34)
       NREC=NREC+1
       KPDS5F(NREC)=1
       KPDS6F(NREC)=1
       KPDS7F(NREC)=0
       ICHKF(NREC)=1
       CHKMF(NREC)=0.01
C
       return
       end
c*********************************************************************
c   ipds values for regular lat/lon grid of 500 mb geopotential height 
c*********************************************************************
c      ipds(1)=28
c      ipds(2)=1
c      ipds(3)=7
c      ipds(4)=80
c      ipds(5)=255..............(grid identification)
c      ipds(6)=1
c      ipds(7)=0................(1 if bitmap is included)
c      ipds(8)=7................(parameter)
c      ipds(9)=100
c      ipds(10)=0
c      ipds(11)=500.............(level in mb)
c      ipds(12)=85..............(year)
c      ipds(13)=7...............(month)
c      ipds(14)=1...............(day)
c      ipds(15)=0...............(cycle)
c      ipds(16)=0
c      ipds(17)=1
c      ipds(18)=0
c      ipds(19)=0
c      ipds(20)=10
c      ipds(21)=0
c      ipds(22)=0
c      ipds(23)=20
c      ipds(24)=0
c      ipds(25)=0
c*********************************************************************
c   igds values for regular lat/lon grid of 500 mb geopotential height 
c*********************************************************************
c      igds(1)=0
c      igds(2)=255
c      igds(3)=0
c      igds(4)=144.............number of longitudes
c      igds(5)=73..............number of latitudes
c      igds(6)=90000
c      igds(7)=0
c      igds(8)=128
c      igds(9)=-90000
c      igds(10)=-2500
c      igds(11)=2500
c      igds(12)=2500
c      igds(13)=0
c      igds(14:18)=0
c*********************************************************************
c   ibds values for regular lat/lon grid of 500 mb geopotential height 
c*********************************************************************
c      ibds(1:9)=0
C*********************************************************************
C        DOCUMENTATION FOR W3FI63
C*********************************************************************
C USAGE:    CALL W3FI63(MSGA,KPDS,KGDS,KBMS,DATA,KPTR,KRET)
C   INPUT ARGUMENT LIST:
C     MSGA     - GRIB FIELD - "GRIB" THRU "7777"   CHAR*1
C
C   OUTPUT ARGUMENT LIST:
C     DATA     - ARRAY CONTAINING DATA ELEMENTS
C     KPDS     - ARRAY CONTAINING PDS ELEMENTS.  (VERSION 1)
C          (1)   - ID OF CENTER
C          (2)   - GENERATING PROCESS ID NUMBER
C          (3)   - GRID DEFINITION
C          (4)   - GDS/BMS FLAG
C          (5)   - INDICATOR OF PARAMETER
C          (6)   - TYPE OF LEVEL
C          (7)   - HEIGHT/PRESSURE , ETC OF LEVEL
C          (8)   - YEAR INCLUDING CENTURY
C          (9)   - MONTH OF YEAR
C          (10)  - DAY OF MONTH
C          (11)  - HOUR OF DAY
C          (12)  - MINUTE OF HOUR
C          (13)  - INDICATOR OF FORECAST TIME UNIT
C          (14)  - TIME RANGE 1
C          (15)  - TIME RANGE 2
C          (16)  - TIME RANGE FLAG
C          (17)  - NUMBER INCLUDED IN AVERAGE
C          (18)  - VERSION NR OF GRIB SPECIFICATION
C          (19)  - VERSION NR OF PARAMETER TABLE
C          (20)  - NR MISSING FROM AVERAGE/ACCUMULATION
C          (21)  - CENTURY OF REFERENCE TIME OF DATA
C          (22)  - UNITS DECIMAL SCALE FACTOR
C       (23-25)  - RESERVED FOR FUTURE USE
C     KGDS     - ARRAY CONTAINING GDS ELEMENTS.
C          (1)   - DATA REPRESENTATION TYPE
C          (19)  - NUMBER OF VERTICAL COORDINATE PARAMETERS
C          (20)  - OCTET NUMBER OF THE LIST OF VERTICAL COORDINATE
C                  PARAMETERS
C                  OR
C                  OCTET NUMBER OF THE LIST OF NUMBERS OF POINTS
C                  IN EACH ROW
C                  OR
C                  255 IF NEITHER ARE PRESENT
C       LATITUDE/LONGITUDE GRIDS
C          (2)   - N(I) NR POINTS ON LATITUDE CIRCLE
C          (3)   - N(J) NR POINTS ON LONGITUDE MERIDIAN
C          (4)   - LA(1) LATITUDE OF ORIGIN
C          (5)   - LO(1) LONGITUDE OF ORIGIN
C          (6)   - RESOLUTION FLAG
C          (7)   - LA(2) LATITUDE OF EXTREME POINT
C          (8)   - LO(2) LONGITUDE OF EXTREME POINT
C          (9)   - DI LATITUDINAL DIRECTION OF INCREMENT
C          (10)  - REGULAR LAT/LON GRID
C          (11)  - SCANNING MODE FLAG
C          (12)  - NV - NR OF VERT COORD PARAMETERS
C          (13)  - PV - OCTET NR OF LIST OF VERT COORD PARAMETERS
C                             OR
C                  PL - LOCATION OF THE LIST OF NUMBERS OF POINTS IN
C                       EACH ROW (IF NO VERT COORD PARAMETERS
C                       ARE PRESENT
C                             OR
C                  255 IF NEITHER ARE PRESENT
C     KBMS       - BITMAP DESCRIBING LOCATION OF OUTPUT ELEMENTS.
C     KPTR       - ARRAY CONTAINING STORAGE FOR FOLLOWING PARAMETERS
C          (1)   - TOTAL LENGTH OF GRIB MESSAGE
C          (2)   - LENGTH OF INDICATOR (SECTION  0)
C          (3)   - LENGTH OF PDS       (SECTION  1)
C          (4)   - LENGTH OF GDS       (SECTION  2)
C          (5)   - LENGTH OF BMS       (SECTION  3)
C          (6)   - LENGTH OF BDS       (SECTION  4)
C          (7)   - VALUE OF CURRENT BYTE
C          (8)   - BIT POINTER
C          (9)   - GRIB START BIT NR
C         (10)   - GRIB/GRID ELEMENT COUNT
C         (11)   - NR UNUSED BITS AT END OF SECTION 3
C         (12)   - BIT MAP FLAG
C         (13)   - NR UNUSED BITS AT END OF SECTION 2
C         (14)   - BDS FLAGS
C         (15)   - NR UNUSED BITS AT END OF SECTION 4
C     KRET       - FLAG INDICATING QUALITY OF COMPLETION
C          = 0 - NORMAL RETURN, NO ERRORS
C          = 1 - 'GRIB' NOT FOUND IN FIRST 100 CHARS
C          = 2 - '7777' NOT IN CORRECT LOCATION
C          = 3 - UNPACKED FIELD IS LARGER THAN 65160
C          = 4 - GDS/ GRID NOT ONE OF CURRENTLY ACCEPTED VALUES
C          = 5 - GRID NOT CURRENTLY AVAIL FOR CENTER INDICATED
C          = 8 - TEMP GDS INDICATED, BUT GDS FLAG IS OFF
C          = 9 - GDS INDICATES SIZE MISMATCH WITH STD GRID
C          =10 - INCORRECT CENTER INDICATOR
C          =11 - BINARY DATA SECTION (BDS) NOT COMPLETELY PROCESSED.
C                PROGRAM IS NOT SET TO PROCESS FLAG COMBINATIONS
C                SHOWN IN OCTETS 4 AND 14.
C          =12 - BINARY DATA SECTION (BDS) NOT COMPLETELY PROCESSED.
C                PROGRAM IS NOT SET TO PROCESS FLAG COMBINATIONS
C*********************************************************************
C        DOCUMENTATION FOR W3FI72
C*********************************************************************
C USAGE:  CALL W3FI72(ITYPE,FLD,IFLD,IBITL,
C        &            IPFLAG,ID,PDS,
C        &            IGFLAG,IGRID,IGDS,ICOMP,
C        &            IBFLAG,IBMAP,IBLEN,IBDSFL,
C        &            IBDSFL,
C        &            NPTS,KBUF,ITOT,JERR)
C
C   INPUT ARGUMENT LIST:
C     ITYPE    - 0 = FLOATING POINT DATA SUPPLIED IN ARRAY 'FLD'
C                1 = INTEGER DATA SUPPLIED IN ARRAY 'IFLD'
C     FLD      - REAL ARRAY OF DATA (AT PROPER GRIDPOINTS) TO BE
C                CONVERTED TO GRIB FORMAT IF ITYPE=0.
C     IFLD     - INTEGER ARRAY OF DATA (AT PROPER GRIDPOINTS) TO BE
C                CONVERTED TO GRIB FORMAT IF ITYPE=1.
C     IBITL    - 0 = COMPUTER COMPUTES LENGTH FOR PACKING DATA FROM
C                    POWER OF 2 (NUMBER OF BITS) BEST FIT OF DATA
C                    USING 'VARIABLE' BIT PACKER W3FI58.
C                8, 12, ETC. COMPUTER RESCALES DATA TO FIT INTO THAT
C                    'FIXED' NUMBER OF BITS USING W3FI59.
C     IPFLAG   - 0 = MAKE PDS FROM USER SUPPLIED ARRAY (ID)
C                1 = USER SUPPLYING PDS
C     ID       - INTEGER ARRAY OF 25 VALUES THAT W3FI68 WILL USE
C                TO MAKE AN EDITION 1 PDS IF IPFLAG=0.
C         ID(1) =NUMBER OF BYTES IN PRODUCT DEFINITION SECTION
C         ID(2) =PARAMETER TABLE VERSION NUMBER
C         ID(3) =IDENTIFICATION OF ORIGINATING CENTER
C         ID(4) =MODEL IDENTIFICATION
C         ID(5) =GRID IDENTIFICATION
C         ID(6) =0 IF NO GDS SECTION, 1 IF GDS SECTION IS INCLUDED
C         ID(7) =0 IF NO BMS SECTION, 1 IF BMS SECTION IS INCLUDED
C         ID(8) =INDICATOR OF PARAMETER AND UNITS (TABLE 2)
C         ID(9) =INDICATOR OF TYPE OF LEVEL       (TABLE 3)
C         ID(10)=VALUE 1 OF LEVEL (0 FOR 1-100,102,103,105,107,111,160)
C         ID(11)=VALUE 2 OF LEVEL
C         ID(12)=YEAR OF CENTURY
C         ID(13)=MONTH OF YEAR
C         ID(14)=DAY OF MONTH
C         ID(15)=HOUR OF DAY
C         ID(16)=MINUTE OF HOUR (IN MOST CASES SET TO 0)
C         ID(17)=FCST TIME UNIT
C         ID(18)=P1 PERIOD OF TIME
C         ID(19)=P2 PERIOD OF TIME
C         ID(20)=TIME RANGE INDICATOR
C         ID(21)=NUMBER INCLUDED IN AVERAGE
C         ID(22)=NUMBER MISSING FROM AVERAGES
C         ID(23)=CENTURY  (20, CHANGE TO 21 ON JAN. 1, 2001)
C         ID(24)=RESERVED - SET TO 0
C         ID(25)=SCALING POWER OF 10
C     PDS      - CHARACTER ARRAY OF 28 VALUES (VALID PDS SUPPLIED
C                BY USER) IF IPFLAG=1.
C     IGFLAG   - 0 = MAKE GDS BASED ON 'IGRID' VALUE.
C                1 = MAKE GDS FROM USER SUPPLIED INFO IN 'IGDS'
C                    AND 'IGRID' VALUE.
C     IGRID    - #   = GRID IDENTIFICATION (TABLE B)
C                255 = IF USER DEFINED GRID; IGDS MUST BE SUPPLIED
C                      AND IGFLAG MUST =1.
C     IGDS     - INTEGER ARRAY CONTAINING USER GDS INFO IF IGFLAG=1.
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
C           IGDS(18) =        IGDS   
C     ICOMP    - RESOLUTION AND COMPONENT FLAG FOR BIT 5 OF GDS(17)
C                0 = EARTH ORIENTED WINDS
C                1 = GRID ORIENTED WINDS
C     IBFLAG   - 0 = MAKE BIT MAP FROM USER SUPPLIED DATA
C                # = BIT MAP PREDEFINED BY CENTER
C     IBMAP    - INTEGER ARRAY CONTAINING BIT MAP
C     IBLEN    - LENGTH OF BIT MAP WILL BE USED TO VERIFY LENGTH
C                OF FIELD (ERROR IF IT DOESN'T MATCH).
C     IBDSFL   - INTEGER ARRAY CONTAINING TABLE 11 FLAG INFO
C                BDS OCTET 4:
C                (1) 0 = GRID POINT DATA
C                    1 = SPHERICAL HARMONIC COEFFICIENTS
C                (2) 0 = SIMPLE PACKING
C                    1 = SECOND ORDER PACKING
C                (3) ... SAME VALUE AS 'ITYPE'
C                    0 = ORIGINAL DATA WERE FLOATING POINT VALUES
C                    1 = ORIGINAL DATA WERE INTEGER VALUES
C                (4) 0 = NO ADDITIONAL FLAGS AT OCTET 14
C                    1 = OCTET 14 CONTAINS FLAG BITS 5-12
C       *** ***  BYTES 5-9 NOT IMPLENTED YET... SET TO '0' FOR NOW
C                (5) 0 = RESERVED - ALWAYS SET TO 0
C                (6) 0 = SINGLE DATUM AT EACH GRID POINT
C                    1 = MATRIX OF VALUES AT EACH GRID POINT
C                (7) 0 = NO SECONDARY BIT MAPS
C                    1 = SECONDARY BIT MAPS PRESENT
C                (8) 0 = SECOND ORDER VALUES HAVE CONSTANT WIDTH
C                    1 = SECOND ORDER VALUES HAVE DIFFERENT WIDTHS
C                (9) 0 = LIST ENCODED DATA
C                    1 = RUN LENGTH ENCODED
C
C   OUTPUT ARGUMENT LIST:
C     NPTS     - NUMBER OF GRIDPOINTS IN ARRAY FLD OR IFLD
C     KBUF     - ENTIRE GRIB MESSAGE ('GRIB' TO '7777')
C     ITOT     - TOTAL LENGTH OF GRIB MESSAGE IN BYTES
C     JERR     - = 0, COMPLETED MAKING GRIB FIELD WITHOUT ERROR
C                  1, IPFLAG NOT 0 OR 1
C                  2, IGFLAG NOT 0 OR 1
C                  3, ERROR CONVERTING IEEE F.P. NUMBER TO IBM370 F.P.
C                  4, W3FI71 ERROR/IGRID NOT DEFINED
C                  5, W3FK74 ERROR/GRID REPRESENTATION TYPE NOT VALID
C                  6, GRID TOO LARGE FOR PACKER DIMENSION ARRAYS
C                     SEE AUTOMATION DIVISION FOR REVISION!
C                  7, LENGTH OF BIT MAP NOT EQUAL TO SIZE OF FLD/IFLD
C                  8, W3FI73 ERROR, ALL VALUES IN IBMAP ARE ZERO
      SUBROUTINE GAULAT(GAUL,K)                                         
C                                                                       
      DIMENSION A(500)                                                  
      DIMENSION GAUL(1)                                                 
C                                                                       
      ESP=1.E-14                                                        
      C=(1.E0-(2.E0/3.14159265358979E0)**2)*0.25E0                      
      FK=K                                                              
      KK=K/2                                                            
      CALL BSSLZ1(A,KK)                                                 
      DO 30 IS=1,KK                                                     
      XZ=COS(A(IS)/SQRT((FK+0.5E0)**2+C))                               
      ITER=0                                                            
   10 PKM2=1.E0                                                         
      PKM1=XZ                                                           
      ITER=ITER+1                                                       
      IF(ITER.GT.10) GO TO 70                                           
      DO 20 N=2,K                                                       
      FN=N                                                              
      PK=((2.E0*FN-1.E0)*XZ*PKM1-(FN-1.E0)*PKM2)/FN                     
      PKM2=PKM1                                                         
   20 PKM1=PK                                                           
      PKM1=PKM2                                                         
      PKMRK=(FK*(PKM1-XZ*PK))/(1.E0-XZ**2)                              
      SP=PK/PKMRK                                                       
      XZ=XZ-SP                                                          
      AVSP=ABS(SP)                                                      
      IF(AVSP.GT.ESP) GO TO 10                                          
      A(IS)=XZ                                                          
   30 CONTINUE                                                          
      IF(K.EQ.KK*2) GO TO 50                                            
      A(KK+1)=0.E0                                                      
      PK=2.E0/FK**2                                                     
      DO 40 N=2,K,2                                                     
      FN=N                                                              
   40 PK=PK*FN**2/(FN-1.E0)**2                                          
   50 CONTINUE                                                          
      DO 60 N=1,KK                                                      
      L=K+1-N                                                           
      A(L)=-A(N)                                                        
   60 CONTINUE                                                          
C                                                                       
      RADI=180./(4.*ATAN(1.))                                           
      DO 211 N=1,K                                                      
      GAUL(N)=90.-ACOS(A(N))*RADI                                           
  211 CONTINUE                                                          
C
C     PRINT *,'GAUSSIAN LAT (DEG) FOR JMAX=',K 
C     PRINT *,(GAUL(N),N=1,K)                                           
C                                                                       
      RETURN                                                            
   70 WRITE(6,6000)                                                     
 6000 FORMAT(//5X,14HERROR IN GAUAW//)                                  
      STOP                                                              
      END                                                               
      SUBROUTINE BSSLZ1(BES,N)                                          
C                                                                       
      DIMENSION BES(N)                                                  
      DIMENSION BZ(50)                                                  
C                                                                       
      DATA PI/3.14159265358979E0/                                       
      DATA BZ         / 2.4048255577E0, 5.5200781103E0,                 
     $  8.6537279129E0,11.7915344391E0,14.9309177086E0,18.0710639679E0, 
     $ 21.2116366299E0,24.3524715308E0,27.4934791320E0,30.6346064684E0, 
     $ 33.7758202136E0,36.9170983537E0,40.0584257646E0,43.1997917132E0, 
     $ 46.3411883717E0,49.4826098974E0,52.6240518411E0,55.7655107550E0, 
     $ 58.9069839261E0,62.0484691902E0,65.1899648002E0,68.3314693299E0, 
     $ 71.4729816036E0,74.6145006437E0,77.7560256304E0,80.8975558711E0, 
     $ 84.0390907769E0,87.1806298436E0,90.3221726372E0,93.4637187819E0, 
     $ 96.6052679510E0,99.7468198587E0,102.888374254E0,106.029930916E0, 
     $ 109.171489649E0,112.313050280E0,115.454612653E0,118.596176630E0, 
     $ 121.737742088E0,124.879308913E0,128.020877005E0,131.162446275E0, 
     $ 134.304016638E0,137.445588020E0,140.587160352E0,143.728733573E0, 
     $ 146.870307625E0,150.011882457E0,153.153458019E0,156.295034268E0/ 
      NN=N                                                              
      IF(N.LE.50) GO TO 12                                              
      BES(50)=BZ(50)                                                    
      DO 5 J=51,N                                                       
    5 BES(J)=BES(J-1)+PI                                                
      NN=49                                                             
   12 DO 15 J=1,NN                                                      
   15 BES(J)=BZ(J)                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE GAU2LL(GAUIN,IMXIN,JMXIN,REGOUT,IMXOUT,JMXOUT)
C
      SAVE
C                                                                       
C  INTERPOLATION FROM LAT/LON GRID TO OTHER LAT/LON GRID                
C                                                                       
      DIMENSION GAUIN (IMXIN,JMXIN)                                   
C                                                                       
      DIMENSION REGOUT(IMXOUT,JMXOUT)                                     
      DIMENSION GAUL(500),REGL(500)                                     
      DIMENSION IINDX1(1000)                                            
      DIMENSION IINDX2(1000)                                            
      DIMENSION JINDX1(500)                                             
      DIMENSION JINDX2(500)                                             
      DIMENSION DDX(1000)                                               
      DIMENSION DDY(500)                                                
C                                                                       
      DATA IFP/0/                                                       
C                                                                       
      IF(IFP.NE.0) GO TO 111                                            
      IFP=1                                                             
C                                                                       
      CALL GAULAT(GAUL,JMXIN)                                           
C                                                                       
      DPHI=180./FLOAT(JMXOUT-1)                                         
      DO 20 J=1,JMXOUT                                                  
      REGL(J)=90.-FLOAT(J-1)*DPHI                                       
   20 CONTINUE                                                          
C                                                                       
      DXIN =360./FLOAT(IMXIN )                                          
      DXOUT=360./FLOAT(IMXOUT)                                          
C                                                                       
      DO 30 I=1,IMXOUT                                                  
      ALAMD=FLOAT(I-1)*DXOUT                                            
      I1=ALAMD/DXIN+1.001                                               
      IINDX1(I)=I1                                                      
      I2=I1+1                                                           
      IF(I2.GT.IMXIN) I2=1                                              
      IINDX2(I)=I2                                                      
      DDX(I)=(ALAMD-FLOAT(I1-1)*DXIN)/DXIN                              
   30 CONTINUE                                                          
C                                                                       
      J2=1                                                              
      DO 40 J=1,JMXOUT                                                  
      APHI=REGL(J)                                                      
      DO 50 JJ=1,JMXIN                                                  
      IF(APHI.LT.GAUL(JJ)) GO TO 50                                     
      J2=JJ                                                             
      GO TO 42                                                          
   50 CONTINUE                                                          
   42 CONTINUE                                                          
      IF(J2.GT.2) GO TO 43                                              
      J1=1                                                              
      J2=2                                                              
      GO TO 44                                                          
   43 CONTINUE                                                          
      IF(J2.LE.JMXIN) GO TO 45                                          
      J1=JMXIN-1                                                        
      J2=JMXIN                                                          
      GO TO 44                                                          
   45 CONTINUE                                                          
      J1=J2-1                                                           
   44 CONTINUE                                                          
      JINDX1(J)=J1                                                      
      JINDX2(J)=J2                                                      
      DDY(J)=(APHI-GAUL(J1))/(GAUL(J2)-GAUL(J1))                        
   40 CONTINUE                                                          
C                                                                       
  111 CONTINUE                                                          
C                                                                       
C     WRITE(LUPTR,*) 'IINDX1'                                                  
C     WRITE(LUPTR,*) (IINDX1(N),N=1,IMXOUT)                                    
C     WRITE(LUPTR,*) 'IINDX2'                                                  
C     WRITE(LUPTR,*) (IINDX2(N),N=1,IMXOUT)                                    
C     WRITE(LUPTR,*) 'JINDX1'                                                  
C     WRITE(LUPTR,*) (JINDX1(N),N=1,JMXOUT)                                    
C     WRITE(LUPTR,*) 'JINDX2'                                                  
C     WRITE(LUPTR,*) (JINDX2(N),N=1,JMXOUT)                                    
C     WRITE(LUPTR,*) 'DDY'                                                     
C     WRITE(LUPTR,*) (DDY(N),N=1,JMXOUT)                                       
C     WRITE(LUPTR,*) 'DDX'                                                     
C     WRITE(LUPTR,*) (DDX(N),N=1,JMXOUT)                                       
C                                                                       
      DO 60 J=1,JMXOUT                                                  
      Y=DDY(J)                                                          
      J1=JINDX1(J)                                                      
      J2=JINDX2(J)                                                      
      DO 60 I=1,IMXOUT                                                  
      X=DDX(I)                                                          
      I1=IINDX1(I)                                                      
      I2=IINDX2(I)                                                      
      REGOUT(I,J)=(1.-X)*(1.-Y)*GAUIN(I1,J1)+(1.-Y)*X*GAUIN(I2,J1)+     
     1           (1.-X)*Y*GAUIN(I1,J2)+X*Y*GAUIN(I2,J2)                 
   60 CONTINUE                                                          
C                                                                       
      SUM1=0.                                                           
      SUM2=0.                                                           
      DO 70 I=1,IMXIN                                                   
      SUM1=SUM1+GAUIN(I,1)                                              
      SUM2=SUM2+GAUIN(I,JMXIN)                                          
   70 CONTINUE                                                          
      SUM1=SUM1/FLOAT(IMXIN)                                            
      SUM2=SUM2/FLOAT(IMXIN)                                            
C                                                                       
      DO 80 I=1,IMXOUT                                                  
      REGOUT(I,     1)=SUM1                                             
      REGOUT(I,JMXOUT)=SUM2                                             
   80 CONTINUE                                                          
C                                                                       
      RETURN                                                            
      END                                                               
       SUBROUTINE QFROMTRH(GRIDT,GRIDRH,GRIDQ,P,IDIM,JDIM,DEG,
     * LABDATE,LABVAR,IDBUG)
c
       CHARACTER*8  LABVAR
       CHARACTER*10 LABDATE
       DIMENSION GRIDT(IDIM,JDIM)
       DIMENSION GRIDRH(IDIM,JDIM)
       DIMENSION GRIDQ(IDIM,JDIM)
       DIMENSION DEG(JDIM)
c
c... get specific humidity...
c
	   do j=1,jdim
	   do i=1,idim
c
           gridq(i,j)=0.
           T=gridt(i,j)
           RH=gridrh(i,j)/100.
               if(RH .gt. 0.) then
               Q=TPRH2Q(T,P,RH)
               gridq(i,j)=Q
               endif
c
	   enddo
	   enddo
c
       if(idbug.eq.1) then
       call gridav(gridq,idim,jdim,deg,avgq)
       PRINT 1111, LABDATE,LABVAR,avgq
 1111  format(A10,1X,A8,2X,f12.7)
       endif
c
       return
       end
       SUBROUTINE CHKCLIM(IREC,SP2,IDATE,
     * IDBUG,IDIML,JDIML,JS,JE,TVAL,XVAL,FVAL,FGRID,CMEAN,CSDMN,
     * GRID2,XQDAT,VQDAT,NQDAT,JQDAT,NBOX,LABDATE,LABVAR,
     * IWEST,IEAST,INORT,ISOUT,IWTPR,IWBPR)
C
       CHARACTER*8 LABVAR
       CHARACTER*10 LABDATE
       DIMENSION SP2(IDIML,JDIML)
       DIMENSION GRID2(IDIML,JDIML)
       DIMENSION CMEAN(IDIML,JDIML)
       DIMENSION CSDMN(IDIML,JDIML)
       DIMENSION NQDAT(NBOX)
       DIMENSION IWEST(NBOX),IEAST(NBOX),INORT(NBOX),ISOUT(NBOX)
C
       DO 879 NB=1,NBOX
       NQDAT(NB)=0
 879   CONTINUE
C
       IQDAT=0
       XQDAT=0
       JQDAT=0
C
       DO 111 J=JS,JE
       DO 111 I=1,IDIML
C
       QDAT=0.
       IBAD=0
C
       IF(CSDMN(I,J).EQ.0.) GO TO 111
C
       DIFF = GRID2(I,J) - CMEAN(I,J)
       QDAT = DIFF / CSDMN(I,J)
       AQDAT=ABS(QDAT)
C
       IF(SP2(I,J).EQ.0.) THEN
       IF(AQDAT.GT.TVAL) IQDAT=IQDAT+1
       IF(AQDAT.GT.XVAL) IBAD=1
       ELSE
       IF(AQDAT.GT.FVAL) IBAD=1
       ENDIF
C
       IF(IBAD.EQ.1) THEN
       DO 256 NB=1,NBOX
       IW=IWEST(NB)
       IE=IEAST(NB)
       IN=INORT(NB)
       IS=ISOUT(NB)
       IF((I.GE.IW).AND.(I.LE.IE).AND.(J.GE.IN).AND.(J.LE.IS)) THEN
       NQDAT(NB)=NQDAT(NB)+1
       IF(IDBUG.EQ.1) THEN
       IF(JQDAT.LE.5) PRINT *,' CVAL ',I,J,GRID2(I,J),CMEAN(I,J),DIFF,
     * CSDMN(I,J),AQDAT
       ENDIF
       GO TO 257
       ENDIF
 256   CONTINUE
 257   CONTINUE
       JQDAT=JQDAT+1
       ENDIF
C
 111   CONTINUE
C
       XQDAT=FLOAT(IQDAT)*FGRID*100.
       IF(XQDAT.GT.VQDAT) THEN
       INDXX=1
       WRITE(IWTPR,2111) LABDATE,LABVAR,IREC,VQDAT,XQDAT,INDXX
       PRINT 2111, LABDATE,LABVAR,IREC,VQDAT,XQDAT,INDXX
 2111  FORMAT(A10,1X,A8,1X,'REC ',I3,1X,'LCLM ',F5.1,1X,'VCLM ',F5.1,
     * 1X,'INDEX ',I1)
       ENDIF
C
       RETURN
       END
       SUBROUTINE CHKTEND(IREC,SP1,SP2,IDATE,
     * IDBUG,IDIML,JDIML,JS,JE,TVAL,XVAL,FGRID,CSDTN,GRID1,
     * GRID2,XQDAT,VQDAT,NQDAT,JQDAT,NBOX,LABDATE,LABVAR,
     * IWEST,IEAST,INORT,ISOUT,IWTPR,IWBPR)
C
       CHARACTER*8 LABVAR
       CHARACTER*10 LABDATE
       DIMENSION SP1(IDIML,JDIML)
       DIMENSION SP2(IDIML,JDIML)
       DIMENSION GRID1(IDIML,JDIML)
       DIMENSION GRID2(IDIML,JDIML)
       DIMENSION CSDTN(IDIML,JDIML)
       DIMENSION NQDAT(NBOX)
       DIMENSION IWEST(NBOX),IEAST(NBOX),INORT(NBOX),ISOUT(NBOX)
C
       DO 879 NB=1,NBOX
       NQDAT(NB)=0
 879   CONTINUE
C
       IQDAT=0
       XQDAT=0
       JQDAT=0
C
       DO 111 J=JS,JE
       DO 111 I=1,IDIML
C
       QDAT=0.
C
       IF(CSDTN(I,J).EQ.0.) GO TO 111
       IF((SP1(I,J).EQ.1.).OR.(SP2(I,J).EQ.1.)) GO TO 111
       TEND = GRID2(I,J) - GRID1(I,J)
       QDAT = TEND / CSDTN(I,J)
C
       AQDAT=ABS(QDAT)
       IF(AQDAT.GT.TVAL) IQDAT=IQDAT+1
C
       IF(AQDAT.GT.XVAL) THEN
       DO 260 NB=1,NBOX
       IW=IWEST(NB)
       IE=IEAST(NB)
       IN=INORT(NB)
       IS=ISOUT(NB)
       IF((I.GE.IW).AND.(I.LE.IE).AND.(J.GE.IN).AND.(J.LE.IS)) THEN
       NQDAT(NB)=NQDAT(NB)+1
       IF(IDBUG.EQ.1) THEN
       IF(JQDAT.LE.5) PRINT *,' TVAL ',I,J,GRID2(I,J),GRID1(I,J),TEND,
     * CSDTN(I,J),AQDAT
       ENDIF
       GO TO 261
       ENDIF
 260   CONTINUE
 261   CONTINUE
       JQDAT=JQDAT+1
       ENDIF
C
 111   CONTINUE
C
       XQDAT=FLOAT(IQDAT)*FGRID*100.
       IF(XQDAT.GT.VQDAT) THEN
       INDXX=2
       WRITE(IWTPR,2111) LABDATE,LABVAR,IREC,VQDAT,XQDAT,INDXX
       PRINT 2111, LABDATE,LABVAR,IREC,VQDAT,XQDAT,INDXX
 2111  FORMAT(A10,1X,A8,1X,'REC ',I3,1X,'LTEN ',F5.1,1X,'VTEN ',F5.1,
     * 1X,'INDEX ',I1)
       ENDIF
C
       RETURN
       END
       SUBROUTINE CHKINTP(IREC,SP1,SP2,SP3,IDATE,
     * IDBUG,IDIML,JDIML,JS,JE,TVAL,XVAL,FGRID,CSDGN,GRID1,
     * GRID2,GRID3,XQDAT,VQDAT,NQDAT,JQDAT,NBOX,LABDATE,LABVAR,
     * IWEST,IEAST,INORT,ISOUT,IWTPR,IWBPR)
C
       CHARACTER*8 LABVAR
       CHARACTER*10 LABDATE
       DIMENSION SP1(IDIML,JDIML)
       DIMENSION SP2(IDIML,JDIML)
       DIMENSION SP3(IDIML,JDIML)
       DIMENSION GRID1(IDIML,JDIML)
       DIMENSION GRID2(IDIML,JDIML)
       DIMENSION GRID3(IDIML,JDIML)
       DIMENSION CSDGN(IDIML,JDIML)
       DIMENSION NQDAT(NBOX)
       DIMENSION IWEST(NBOX),IEAST(NBOX),INORT(NBOX),ISOUT(NBOX)
C
       DO 879 NB=1,NBOX
       NQDAT(NB)=0
 879   CONTINUE
C
       IQDAT=0
       XQDAT=0
       JQDAT=0
C
       DO 111 J=JS,JE
       DO 111 I=1,IDIML
C
       QDAT=0.
C
       IF(CSDGN(I,J).EQ.0.) GO TO 111
       IF((SP1(I,J).EQ.1.).OR.(SP2(I,J).EQ.1.).OR.
     * (SP3(I,J).EQ.1.)) GO TO 111
C
       GAND=GRID2(I,J)-((GRID1(I,J)+GRID3(I,J))/2.)
       QDAT=GAND / CSDGN(I,J)
C
       AQDAT=ABS(QDAT)
       IF(AQDAT.GT.TVAL) IQDAT=IQDAT+1
C
       IF(AQDAT.GT.XVAL) THEN
       DO 260 NB=1,NBOX
       IW=IWEST(NB)
       IE=IEAST(NB)
       IN=INORT(NB)
       IS=ISOUT(NB)
       IF((I.GE.IW).AND.(I.LE.IE).AND.(J.GE.IN).AND.(J.LE.IS)) THEN
       NQDAT(NB)=NQDAT(NB)+1
       IF(IDBUG.EQ.1) THEN
       IF(JQDAT.LE.5) PRINT *,' GVAL ',I,J,GRID1(I,J),GRID2(I,J),
     * GRID3(I,J),GAND,CSDGN(I,J),AQDAT
       ENDIF
       GO TO 261
       ENDIF
 260   CONTINUE
 261   CONTINUE
       JQDAT=JQDAT+1
       ENDIF
C
 111   CONTINUE
C
       XQDAT=FLOAT(IQDAT)*FGRID*100.
       IF(XQDAT.GT.VQDAT) THEN
       INDXX=3
       WRITE(IWTPR,2111) LABDATE,LABVAR,IREC,VQDAT,XQDAT,INDXX
       PRINT 2111, LABDATE,LABVAR,IREC,VQDAT,XQDAT,INDXX
 2111  FORMAT(A10,1X,A8,1X,'REC ',I3,1X,'LGAN ',F5.1,1X,'VGAN ',F5.1,
     * 1X,'INDEX ',I1)
       ENDIF
C
       RETURN
       END
       SUBROUTINE INP(CMEANx,CMEAN,CSDMNx,CSDMN,CSDTNx,CSDTN,
     * CSDGNx,CSDGN,INPOL,IDIML,JDIML,NCHK,IREC,IDAY,DEG,
     * IDBUG,LABDATE,LABVAR)
C
       CHARACTER*10 LABDATE
       CHARACTER*8 LABVAR
C
       DIMENSION CMEANx(IDIML,JDIML,NCHK,INPOL)
       DIMENSION CSDMNx(IDIML,JDIML,NCHK,INPOL)
       DIMENSION CSDTNx(IDIML,JDIML,NCHK,INPOL)
       DIMENSION CSDGNx(IDIML,JDIML,NCHK,INPOL)
C
       DIMENSION CMEAN(IDIML,JDIML)
       DIMENSION CSDMN(IDIML,JDIML)
       DIMENSION CSDTN(IDIML,JDIML)
       DIMENSION CSDGN(IDIML,JDIML)
C
       DIMENSION DEG(JDIML)
C
C   NOW DO THE INTERPOLATION...
C
       TEMP=(FLOAT(IDAY)-15.)/30.
       PERC1 = 1.0-ABS(TEMP)
       PERC2 = 1.0-PERC1
c
       DO J = 1,JDIML
       DO I = 1,IDIML
       CMEAN(I,J) = CMEANx(I,J,IREC,1)*PERC1+CMEANx(I,J,IREC,2)*PERC2
       CSDMN(I,J) = CSDMNx(I,J,IREC,1)*PERC1+CSDMNx(I,J,IREC,2)*PERC2
       CSDTN(I,J) = CSDTNx(I,J,IREC,1)*PERC1+CSDTNx(I,J,IREC,2)*PERC2
       CSDGN(I,J) = CSDGNx(I,J,IREC,1)*PERC1+CSDGNx(I,J,IREC,2)*PERC2
       ENDDO
       ENDDO
c
       if(idbug.eq.1) then
c
       call gridav(cmeanx(1,1,irec,1),idiml,jdiml,deg,cmean1)
       call gridav(cmeanx(1,1,irec,2),idiml,jdiml,deg,cmean2)
       call gridav(cmean,idiml,jdiml,deg,cmeanm)
       PRINT *,' REC ',irec,' CMEAN1 ',CMEAN1,' CMEAN2 ',CMEAN2,
     * ' CMEAN ',CMEANM
c
       call gridav(csdmnx(1,1,irec,1),idiml,jdiml,deg,csdmn1)
       call gridav(csdmnx(1,1,irec,2),idiml,jdiml,deg,csdmn2)
       call gridav(csdmn,idiml,jdiml,deg,csdmnm)
       PRINT *,' REC ',irec,' CSDMN1 ',CSDMN1,' CSDMN2 ',CSDMN2,
     * ' CSDMN ',CSDMNM
c
       call gridav(csdtnx(1,1,irec,1),idiml,jdiml,deg,csdtn1)
       call gridav(csdtnx(1,1,irec,2),idiml,jdiml,deg,csdtn2)
       call gridav(csdtn,idiml,jdiml,deg,csdtnm)
       PRINT *,' REC ',irec,' CSDTN1 ',CSDTN1,' CSDTN2 ',CSDTN2,
     * ' CSDTN ',CSDTNM
c
       call gridav(csdgnx(1,1,irec,1),idiml,jdiml,deg,csdgn1)
       call gridav(csdgnx(1,1,irec,2),idiml,jdiml,deg,csdgn2)
       call gridav(csdgn,idiml,jdiml,deg,csdgnm)
       PRINT *,' REC ',irec,' CSDGN1 ',CSDGN1,' CSDGN2 ',CSDGN2,
     * ' CSDGN ',CSDGNM
c
       endif
c
       RETURN
       END
c     SUBROUTINE GLAT(JH,SLAT,CLAT,WLAT)
c
c... Mark's program expects only one hemisphere
c... this program fills in the whole globe
c
      SUBROUTINE GLAT(JF,SLAT,CLAT,WLAT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    GLAT        COMPUTE GAUSSIAN LATITUDE FUNCTIONS
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: COMPUTES SINES OF GAUSSIAN LATITUDE BY ITERATION.
C           THE COSINES OF GAUSSIAN LATITUDE AND GAUSSIAN WEIGHTS
C           ARE ALSO COMPUTED.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL GLAT(JH,SLAT,CLAT,WLAT)
C
C   INPUT ARGUMENT LIST:
C     JH       - INTEGER NUMBER OF GAUSSIAN LATITUDES IN A HEMISPHERE
C
C   OUTPUT ARGUMENT LIST:
C     SLAT     - REAL (JH) SINES OF (POSITIVE) GAUSSIAN LATITUDE
C     CLAT     - REAL (JH) COSINES OF GAUSSIAN LATITUDE
C     WLAT     - REAL (JH) GAUSSIAN WEIGHTS FOR THE NH
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
c     DIMENSION SLAT(JH),CLAT(JH),WLAT(JH)
      DIMENSION SLAT(JF),CLAT(JF),WLAT(JF)
      PARAMETER(JBZ=50)
      PARAMETER(PI=3.14159265358979,C=(1.-(2./PI)**2)*0.25,EPS=1.E-14)
      DIMENSION PK(JF),PKM1(JF),BZ(JBZ)
      DATA BZ        / 2.4048255577,  5.5200781103,
     $  8.6537279129, 11.7915344391, 14.9309177086, 18.0710639679,
     $ 21.2116366299, 24.3524715308, 27.4934791320, 30.6346064684,
     $ 33.7758202136, 36.9170983537, 40.0584257646, 43.1997917132,
     $ 46.3411883717, 49.4826098974, 52.6240518411, 55.7655107550,
     $ 58.9069839261, 62.0484691902, 65.1899648002, 68.3314693299,
     $ 71.4729816036, 74.6145006437, 77.7560256304, 80.8975558711,
     $ 84.0390907769, 87.1806298436, 90.3221726372, 93.4637187819,
     $ 96.6052679510, 99.7468198587, 102.888374254, 106.029930916,
     $ 109.171489649, 112.313050280, 115.454612653, 118.596176630,
     $ 121.737742088, 124.879308913, 128.020877005, 131.162446275,
     $ 134.304016638, 137.445588020, 140.587160352, 143.728733573,
     $ 146.870307625, 150.011882457, 153.153458019, 156.295034268 /
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        JH=(JF+1)/2
C  ESTIMATE LATITUDES USING BESSEL FUNCTION
      R=1./SQRT((2*JH+0.5)**2+C)
      DO J=1,MIN(JH,JBZ)
        SLAT(J)=COS(BZ(J)*R)
      ENDDO
      DO J=JBZ+1,JH
        SLAT(J)=COS((BZ(JBZ)+(J-JBZ)*PI)*R)
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CONVERGE UNTIL ALL SINES OF GAUSSIAN LATITUDE ARE WITHIN EPS
      SPMAX=1.
      DO WHILE(SPMAX.GT.EPS)
        SPMAX=0.
        PK=0.
        PKM1=0.
        DO J=1,JH
          PKM1(J)=1.
          PK(J)=SLAT(J)
        ENDDO
        DO N=2,2*JH
          DO J=1,JH
            PKM2=PKM1(J)
            PKM1(J)=PK(J)
            PK(J)=((2*N-1)*SLAT(J)*PKM1(J)-(N-1)*PKM2)/N
          ENDDO
        ENDDO
        DO J=1,JH
          SP=PK(J)*(1.-SLAT(J)**2)/(2*JH*(PKM1(J)-SLAT(J)*PK(J)))
          SLAT(J)=SLAT(J)-SP
          SPMAX=MAX(SPMAX,ABS(SP))
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE COSINES AND GAUSSIAN WEIGHTS
      DO J=1,JH
        CLAT(J)=SQRT(1.-SLAT(J)**2)
        WLAT(J)=2.*(1.-SLAT(J)**2)/(2*JH*PKM1(J))**2
      ENDDO
C  COMPUTE GLOBAL VALUES.. REFLECT NH INTO SH
C
      DO J=1,JH
      JS=JF+1-J
        SLAT(JS)=-SLAT(J)
        CLAT(JS)=CLAT(J)
        WLAT(JS)=WLAT(J)
      ENDDO
C
      RETURN
      END
C-----------------------------------------------------------------------
      subroutine laplac
     1(idir,wave,waved2,maxwv,iromb)
c
c  takes laplacian or inverse laplacian in spectral space
c
c  idir.gt.0  take laplacian:  field waved2 output
c  idir.lt.0  take inverse laplacian:  field wave output
c
c  wave are the 2*nmmax spectral coefficients of the scalar field
c  waved2 are the 2*nmmax spectral coefficients of its laplacian
c  note that wave and waved2 may be the same field (laplace in place)
c  also note that the 0,0 spectral component is not changed
c
c  waved2 = del2(wave)
c
c  iromb.eq.1 for rhomboidal truncation:  nmmax=(maxwv+1)*(maxwv+1)
c  iromb.eq.0 for triangular truncation:  nmmax=(maxwv+1)*(maxwv+2)/2
c
      dimension wave(2,1), waved2(2,1)
      data rerth/6.3712e6/
c***********************************************************************
c  take laplacian
      if (idir.gt.0)  then
        nm = 0
        do 10 m=0,maxwv
        do 10 n=m,maxwv+iromb*m
          rnn1a2 = -n*(n+1)/rerth**2
          nm = nm + 1
          if (n.gt.0)  then
            waved2(1,nm) = wave(1,nm)*rnn1a2
            waved2(2,nm) = wave(2,nm)*rnn1a2
          endif
   10   continue
c***********************************************************************
c  take inverse laplacian
      else if (idir.lt.0)  then
        nm = 0
        do 20 m=0,maxwv
        do 20 n=m,maxwv+iromb*m
          rnn1a2 = -n*(n+1)/rerth**2
          nm = nm + 1
          if (n.gt.0)  then
            wave(1,nm) = waved2(1,nm)/rnn1a2
            wave(2,nm) = waved2(2,nm)/rnn1a2
          endif
   20   continue
      endif
c***********************************************************************
      return
      end
c***********************************************************************
       SUBROUTINE UVPSICHI(U,V,Z,D,PSI,CHI,MEND1,IROMB,ER,MWVD2,MUWVD2)
C
C  THIS ROUTINE PERFORMS CONVERSION OF PSEUDO-SCALAR U, V TO
C  TRUE SCALAR VORTICITY (Z) AND DIVERGENCE (D) AND
C  TRUE SCALAR STREAMFUNCTION (PSI) AND VELOCITY POTENTIAL (CHI)
C  IN SPECTRAL SPACE.
C
C  CALL UVPSICHI(U,V,PSI,CHI,MEND1,IROMB,ER,MWVD2,MUWVD2)
C
C  CALLING ARGUEMENTS:
C
C  U      ..... PSEUDO SCALAR ZONAL WIND (= UCOS(PHI))
C  V      .....   ..    ..    MERID WIND (= VCOS(PHI))
C  Z      ..... OUTPUT OF RELATIVE VORTICITY
C  D      .....   ..   .. DIVERGENCE
C  PSI    ..... OUTPUT OF STREAM FUNCTIO
C  CHI    .....   ..   .. VELOCITY POTENTIAL
C  MEND1  ..... ZONAL WAVENUMBER + 1
C  ER     ..... RADIUS OF EARTH
C  MWVD2  ..... DIMENSION OF Z AND D
C  MUWVD2 ..... DIMENSION OF U AND V
C
C  SURANJANA SAHA DECEMBER 1994
C
       COMPLEX PSI(MWVD2)
       COMPLEX CHI(MWVD2)
       COMPLEX Z(MWVD2)
       COMPLEX D(MWVD2)
       COMPLEX U(MUWVD2)
       COMPLEX V(MUWVD2)
C
       CALL UVTOZD(U,V,Z,D,MEND1,IROMB,ER,MWVD2,MUWVD2)
C
       CALL LAPLAC(-1,Z,PSI,MWVD2,IROMB)
       CALL LAPLAC(-1,D,CHI,MWVD2,IROMB)
C
       RETURN
       END
      FUNCTION QFRMTP(T,P)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    QFRMTP      CALCULATES SPECIFIC HUMIDITY
C   PRGMMR: DIMEGO           ORG: W/NMC22    DATE: 86-03-31
C
C ABSTRACT: CALCULATES SPECIFIC HUMIDITY FROM VALUES FOR T & P
C
C PROGRAM HISTORY LOG:
C   86-03-31  G DIMEGO
C   88-09-01  B SCHMIDT ADDED THE DOCBLOCK
C
C USAGE:    CALL QFRMTP (T, P)
C   INPUT ARGUMENT LIST:
C     T        - TEMPERATURE
C     P        - PRESSURE
C
C ATTRIBUTES:
C   LANGUAGE: STANDARD FORTRAN
C   MACHINE:
C
C$$$
C
      A = T - 273.16
      ES = 6.1078 * EXP((17.269*A)/(A+237.3))
      QFRMTP = 0.622 * ES/(P - 0.378 * ES)
      RETURN
      END
	  FUNCTION TVPRH2T(TV,P,RH)
	  n=1
	  t=tv
	  qs=qfrmtp(tv,p)
	  q = .61*qs*rh
	  tx = tv/(1. + q)
	  do while (abs(tx-t).gt. 1e-6.and.n .lt .10)     
		  t=tx
		  qs=qfrmtp(tx,p)
		  q = .61*qs*rh
		  tx = tv/(1. + q)
		  n=n+1
	  enddo
	  tvprh2t=tx
	  return
	  end
	  FUNCTION TVPRH2Q(TV,P,RH)
	  n=1
	  t=tv
	  qs=qfrmtp(tv,p)
	  q = .61*qs*rh
	  tx = tv/(1. + q)
	  do while (abs(tx-t).gt. 1e-6.and.n .lt .10)     
		  t=tx
		  qs=qfrmtp(tx,p)
		  q = .61*qs*rh
		  tx = tv/(1. + q)
		  n=n+1
	  enddo
	  tvprh2q=q
	  return
	  end
	  FUNCTION TPRH2Q(T,P,RH)
	  qs=qfrmtp(t,p)
	  q = qs*rh
	  tprh2q=q
	  return
	  end
      SUBROUTINE LA2GA(REGIN ,IMXIN ,JMXIN , DLOIN, DLAIN, RLON, RLAT,
     1                 IGRIN,GAUOUT,IMXOUT,JMXOUT,SLMASK,INMASK,FSMASK,
     2                 NVARN)
C
C  INTERPOLATION FROM LAT/LON OR GAUSSIAN GRID TO OTHER LAT/LON GRID
C
C   INMASK=0   WILL NOT USE LSMASK
C   INMASK=1   WILL USE LSMASK (USING VALUES ON LAND ONLY)
C   INMASK=2   WILL USE LSMASK-1 (USING VALUES ON OCEAN ONLY)
C
      IMPLICIT REAL (A-H,O-Z)
      SAVE
      DIMENSION REGIN (IMXIN ,JMXIN )
      DIMENSION GAUOUT(IMXOUT,JMXOUT)
C
      DIMENSION SLMASK(IMXOUT,JMXOUT)
C
      DIMENSION GAULO (500)
      DIMENSION GAULI (500)
      DIMENSION RINLAT(500),OUTLAT(500)
      DIMENSION RINLON(1000)
C
      DIMENSION IINDX1(1000)
      DIMENSION IINDX2(1000)
      DIMENSION JINDX1(500)
      DIMENSION JINDX2(500)
C
      DIMENSION DDX(1000)
      DIMENSION DDY(500)
C
      CHARACTER*6 NVARN
C
      DATA IFPI,JFPI,IFPO,JFPO,RFP1,RFP2/4*0,2*0./
C
      IF(IMXIN.EQ.1.OR.JMXIN.EQ.1) THEN
      DO 1 J=1,JMXOUT
      DO 1 I=1,IMXOUT
      GAUOUT(I,J)=0.
    1 CONTINUE
      RETURN
      ENDIF
C
      IF(IGRIN.NE.1.AND.(DLOIN.EQ.0..OR.DLAIN.EQ.0.)) THEN
      PRINT *,'DLOIN OR DLAIN IS ZERO .... CHECK DATA CARDS'
      ENDIF
C
      IF(IFPI.EQ.IMXIN .AND.JFPI.EQ.JMXIN .AND.
     1   IFPO.EQ.IMXOUT.AND.JFPO.EQ.JMXOUT.AND.
     2   RFP1.EQ.RLON.AND.RFP2.EQ.RLAT) GO TO 111
C
      IFPI=IMXIN
      JFPI=JMXIN
      IFPO=IMXOUT
      JFPO=JMXOUT
      RFP1=RLON
      RFP2=RLAT
C
      PRINT *,'nvarn=',nvarn
      PRINT *,'fsmask=',fsmask
      PRINT *,'inmask=',inmask
      PRINT *,'igrin=',igrin
      PRINT *,'imxin=',imxin
      PRINT *,'jmxin=',jmxin
      PRINT *,'imxout=',imxout
      PRINT *,'jmxout=',jmxout
      PRINT *,'DLOIN=',DLOIN
      PRINT *,'DLAIN=',DLAIN
      PRINT *,'RLON=',RLON
      PRINT *,'RLAT=',RLAT
C
      IF(IGRIN.EQ.1) THEN
      CALL GAULAT(GAULI,JMXIN)
      DO 6 J=1,JMXIN
      RINLAT(J)=90.-GAULI(J)
    6 CONTINUE
      ELSE
      DO 5 J=1,JMXIN
      IF(RLAT.GT.0.) THEN
      RINLAT(J)=RLAT-FLOAT(J-1)*DLAIN
      ELSE
      RINLAT(J)=RLAT+FLOAT(J-1)*DLAIN
      ENDIF
    5 CONTINUE
      ENDIF
C
      PRINT *,'RINLAT='
      PRINT *,(RINLAT(J),J=1,JMXIN)
C
C    COMPUTE GAUSSIAN LATITUDE FOR OUTPUT GRID
C
      CALL GAULAT(GAULO,JMXOUT)
C
      DO 7 J=1,JMXOUT
      OUTLAT(J)=90.-GAULO(J)
    7 CONTINUE
C
      PRINT *,'OUTLAT='
      PRINT *,(OUTLAT(J),J=1,JMXOUT)
C
      IF(IGRIN.EQ.1) THEN
      DLOIN=360./FLOAT(IMXIN)
      RLON=0.
      ENDIF
      DO 15 I=1,IMXIN
      RINLON(I)=RLON+FLOAT(I-1)*DLOIN
   15 CONTINUE
C
      PRINT *,'RINLON='
      PRINT *,(RINLON(I),I=1,IMXIN)
C
C  FIND I-INDEX FOR INTERPLATION
C
      DO 30 I=1,IMXOUT
      ALAMD=FLOAT(I-1)*360./FLOAT(IMXOUT)
      IF(RLON.LT.0.) THEN
      IF(ALAMD.GT.180.) ALAMD=ALAMD-360.
      ENDIF
      DO 35 II=1,IMXIN
      IF(ALAMD.GT.RINLON(II)) GO TO 35
      IX=II
      GO TO 32
   35 CONTINUE
      I1=360./DLOIN+0.5
      I2=1
      GO TO 34
   32 CONTINUE
      IF(IX.GE.2) GO TO 33
      I1=360./DLOIN+0.5
      I2=1
      GO TO 34
   33 CONTINUE
      I2=IX
      I1=I2-1
   34 CONTINUE
      IINDX1(I)=I1
      IINDX2(I)=I2
      DENOM=RINLON(I2)-RINLON(I1)
      IF(DENOM.LT.0.) DENOM=DENOM+360.
      RNUME=ALAMD-RINLON(I1)
      IF(RNUME.LT.0.) RNUME=RNUME+360.
      DDX(I)=RNUME/DENOM
   30 CONTINUE
C
C  FIND J-INDEX FOR INTERPLATION
C
      JQ=1
      DO 40 J=1,JMXOUT
      APHI=OUTLAT(J)
      DO 50 JJ=1,JMXIN
      JX=JJ
      IF(RLAT.LT.0.) JX=JMXIN-JJ+1
      IF(APHI.LT.RINLAT(JX)) GO TO 50
      JQ=JX
      GO TO 42
   50 CONTINUE
      IF(RLAT.GT.0.) THEN
        J1=JMXIN
        J2=JMXIN
      ELSE
        J1=1
        J2=1
      ENDIF
      GO TO 44
   42 CONTINUE
      IF(RLAT.GT.0.) THEN
         IF(JQ.GE.2) GO TO 43
         J1=1
         J2=1
      ELSE
         IF(JQ.LT.JMXIN) GO TO 43
         J1=JMXIN
         J2=JMXIN
      ENDIF
      GO TO 44
   43 CONTINUE
      IF(RLAT.GT.0.) THEN
      J2=JQ
      J1=JQ-1
      ELSE
      J1=JQ
      J2=JQ+1
      ENDIF
   44 CONTINUE
      JINDX1(J)=J1
      JINDX2(J)=J2
      IF(J2.NE.J1) THEN
         DDY(J)=(APHI-RINLAT(J1))/(RINLAT(J2)-RINLAT(J1))
      ELSE
      IF(J1.EQ.1.AND.RLAT.GT.0..OR.J1.EQ.JMXIN.AND.RLAT.LT.0.) THEN
            IF(ABS(90.-RINLAT(J1)).GT.0.001) THEN
               DDY(J)=(APHI-RINLAT(J1))/(90.-RINLAT(J1))
            ELSE
               DDY(J)=0.0
            ENDIF
      ENDIF
      IF(J1.EQ.1.AND.RLAT.LT.0..OR.J1.EQ.JMXIN.AND.RLAT.GT.0.) THEN
            IF(ABS(-90.-RINLAT(J1)).GT.0.001) THEN
               DDY(J)=(APHI-RINLAT(J1))/(-90.-RINLAT(J1))
            ELSE
               DDY(J)=0.0
            ENDIF
         ENDIF
      ENDIF
   40 CONTINUE
C
C     PRINT *,'IINDX1'
C     PRINT *,(IINDX1(N),N=1,IMXOUT)
C     PRINT *,'IINDX2'
C     PRINT *,(IINDX2(N),N=1,IMXOUT)
C     PRINT *,'JINDX1'
C     PRINT *,(JINDX1(N),N=1,JMXOUT)
C     PRINT *,'JINDX2'
C     PRINT *,(JINDX2(N),N=1,JMXOUT)
C     PRINT *,'DDY'
C     PRINT *,(DDY(N),N=1,JMXOUT)
C     PRINT *,'DDX'
C     PRINT *,(DDX(N),N=1,JMXOUT)
C
  111 CONTINUE
C
      SUM1=0.
      SUM2=0.
      DO 80 I=1,IMXIN
      SUM1=SUM1+REGIN(I,1)
      SUM2=SUM2+REGIN(I,JMXIN)
   80 CONTINUE
      IF(RLAT.GT.0.) THEN
      SUMN=SUM1/FLOAT(IMXIN)
      SUMS=SUM2/FLOAT(IMXIN)
      ELSE
      SUMS=SUM1/FLOAT(IMXIN)
      SUMN=SUM2/FLOAT(IMXIN)
      ENDIF
C
C  QUASI-BILINEAR INTERPOLATION
C
      DO 70 J=1,JMXOUT
      Y=DDY(J)
      J1=JINDX1(J)
      J2=JINDX2(J)
      DO 70 I=1,IMXOUT
      X=DDX(I)
      I1=IINDX1(I)
      I2=IINDX2(I)
C
      WI1J1=(1.-X)*(1.-Y)
      WI2J1=    X *(1.-Y)
      WI1J2=(1.-X)*      Y
      WI2J2=    X *      Y
C
C  USE MASK
C
C     IF(INMASK.EQ.1) THEN
C     WI1J1=WI1J1*SLMASK(I1,J1)
C     WI2J1=WI2J1*SLMASK(I2,J1)
C     WI1J2=WI1J2*SLMASK(I1,J2)
C     WI2J2=WI2J2*SLMASK(I2,J2)
C     ENDIF
C     IF(INMASK.EQ.2) THEN
C     WI1J1=WI1J1*(1.-SLMASK(I1,J1))
C     WI2J1=WI2J1*(1.-SLMASK(I2,J1))
C     WI1J2=WI1J2*(1.-SLMASK(I1,J2))
C     WI2J2=WI2J2*(1.-SLMASK(I2,J2))
C     ENDIF
C
      WSUM  =WI1J1+WI2J1+WI1J2+WI2J2
      IF(WSUM.NE.0.) THEN
      WSUMIV = 1./WSUM
C
      IF(J1.NE.J2) THEN
          GAUOUT(I,J)=(WI1J1*REGIN(I1,J1)+WI2J1*REGIN(I2,J1)+
     1                 WI1J2*REGIN(I1,J2)+WI2J2*REGIN(I2,J2))*WSUMIV
      ELSE
         IF(J1.EQ.1.AND.RLAT.GT.0..OR.J1.EQ.JMXIN.AND.RLAT.LT.0.) THEN
          GAUOUT(I,J)=(WI1J1*SUMN        +WI2J1*SUMN        +
     1                 WI1J2*REGIN(I1,J2)+WI2J2*REGIN(I2,J2))*WSUMIV
         ENDIF
         IF(J1.EQ.1.AND.RLAT.LT.0..OR.J1.EQ.JMXIN.AND.RLAT.GT.0.) THEN
          GAUOUT(I,J)=(WI1J1*REGIN(I1,J1)+WI2J1*REGIN(I2,J1)+
     1                 WI1J2*SUMS        +WI2J2*SUMS        )*WSUMIV
         ENDIF
      ENDIF
      ELSE
       GAUOUT(I,J)=FSMASK
      ENDIF
C
C  MASK POINTS ON THE EDGE OF LAND OR SEA
C
C     IF(INMASK.EQ.1.AND.SLMASK(I,J).NE.1.0) GAUOUT(I,J) = FMASK
C     IF(INMASK.EQ.2.AND.SLMASK(I,J).NE.0.0) GAUOUT(I,J) = FMASK
C
   70 CONTINUE
C
      CALL MAXMIN(GAUOUT,IMXOUT,JMXOUT,1,NVARN)
C
      RETURN
      END
      SUBROUTINE MAXMIN(F,IMAX,JMAX,KMAX,TITLE)
C
      IMPLICIT REAL (A-H,O-Z)
C
      DIMENSION F(IMAX,JMAX,KMAX)
      CHARACTER*(*) TITLE
C
      WRITE(6,99) TITLE
   99 FORMAT(1H0,2X,'TITLE=',A)
C
      DO 10 K=1,KMAX
C
      FMAX=F(1,1,K)
      FMIN=F(1,1,K)
C
      DO 20 J=1,JMAX
      DO 20 I=1,IMAX
      IF(FMAX.LE.F(I,J,K)) THEN
      FMAX=F(I,J,K)
      IIMAX=I
      JJMAX=J
      ENDIF
      IF(FMIN.GE.F(I,J,K)) THEN
      FMIN=F(I,J,K)
      IIMIN=I
      JJMIN=J
      ENDIF
   20 CONTINUE
C
      WRITE(6,100) K,FMAX,IIMAX,JJMAX,FMIN,IIMIN,JJMIN
  100 FORMAT(2X,'LEVEL=',I3,' MAX=',E12.4,' AT I=',I5,' J=',I5,
     1                      ' MIN=',E12.4,' AT I=',I5,' J=',I5)
C
   10 CONTINUE
C
      RETURN
      END
      subroutine minmaxl(n,d,dmin,dmax,lbms)
      dimension d(n)
      logical*1 lbms(n)
      dmin=d(1)
      dmax=d(1)
      do i=1,n
          if(lbms(i)) then
          dmin=min(dmin,d(i))
          dmax=max(dmax,d(i))
          endif
      enddo
      return
      end
