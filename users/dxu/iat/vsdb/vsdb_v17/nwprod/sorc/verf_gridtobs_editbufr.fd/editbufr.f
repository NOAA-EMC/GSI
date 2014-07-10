C-----------------------------------------------------------------------
C  EDITBUFR WILL RETAIN DATA LOCATED IN RETENTION GRID DEFINED
C  FROM STANDARD INPUT. THE FORMAT OF THE PARAMETER
C  LIST IS AS FOLLOWS:
C
C  FIRST WE READ THE GRID NUMBER THAT DEFINES THE RETENTION AREA
C
C  GRID#     - IS THE NUMBER OF THE RETENTION GRID
C
C  THEN WE READ THE DATE INFORMATION
C
C  YYMMDD    - IS THE (TWO-DIGIT) YEAR, MONTH, AND DAY TO BE KEPT
C              IF YYMMDD IS ZERO, THEN NO DATE CHECK IS PERFORMED
C              IF YYMMDD IS LESS THAN ZERO, THEN ITS MAGNITUDE IS
C              USED AS A LIMIT FOR THE MAGNITUDE OF THE OBSERVATION'S
C              DHR - WHICH IS THE TIME DIFFERENCE IN HUNDREDTH'S OF
C              AN HOUR BETWEEN THE OB TIME AND THE ANALYSIS TIME
C
C  THEN WE READ UP TO 30 DATATYPEs
C
C  DATATYPE  - IS THE OI REPORT TYPE (I.E. 120 IS A RAWINSONDE).
C              IT IS POSSIBLE TO SPECIFY A RANGE OF DATA TYPES
C              BY GIVING AN ABBREVIATED FORM. THAT IS, TO SPECIFY
C              TYPES 120 THROUGH 129, THE PARAMETER IS '12'. TYPES
C              100 THROUGH 199 WOULD BE SPECIFIED BY '1'. AND SO ON.
C
C
C  THE INPUT  PREPBUFR FILE IS ASSIGNED TO UNIT 20
C  THE OUTPUT PREPBUFR FILE IS ASSIGNED TO UNIT 50
C
C* Log:
C* K. Brill/EMC		11/98	Use INT for grid pt location & change
C*				check.  Use INT for TYP/TFAC.
C-----------------------------------------------------------------------
      PROGRAM editbufr
 
      PARAMETER (mxrt = 30,mxts = 8,mxtb = 2000000)
 
      CHARACTER*8 subset, sslast
      DIMENSION rtyp(mxrt), tfac(mxrt)
      REAL*8 tab(mxts,mxtb) 
      DIMENSION ikep(mxtb)
      DIMENSION xob(mxtb), yob(mxtb), typ(mxtb), dhr(mxtb)
      DIMENSION nret(mxrt)
      LOGICAL keep, within(mxtb)
      LOGICAL latlong, lambert, polarstereo
      COMMON /gridef/ imax, jmax, kmax, alat1, elon1, dxx, dyy, elonv, 
     +            alatan, latlong, lambert, polarstereo
 
      DATA lubfi /20/
      DATA lubfj /50/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL DATELEN(10)
 
C  READ THE RETENTION GRID NUMBER
C  ------------------------------
c
c  igrid=1:  The input is a grid number
c  igrid=0:  The input are the lat/lon boundaries of the domain 
c
 
      READ (5,'(A80)')
      READ (5,*,END=10) iretgrid
      igrid=1
      if (iretgrid.eq.0) then
       igrid=0
c      read(5,'(4(f6.2,1x))') xlon1,xlon2,xlat1,xlat2
       read(5,*) xlon1,xlon2,xlat1,xlat2
       if(xlon1.lt.0.0) xlon1=xlon1+360.0
       if(xlon2.lt.0.0) xlon2=xlon2+360.0
c      print*,'xlon1,xlon2=',xlon1,xlon2
c      print*,'xlat1,xlat2=',xlat1,xlat2
      else
      CALL gtgdef(iretgrid,istat)
      IF (istat.ne.0) THEN
        PRINT *, 'GTGDEF RETURNED NON-ZERO ISTAT=', istat
        STOP 'BAD RETENTION GRID'
      END IF
      endif
 
C     READ THE RETENTION DATE
C     -----------------------
 
      READ (5,'(A80)')
      READ (5,*,END=10) jdate
      IF (jdate.lt.0) THEN
        jwndo = -jdate
        jdate = 0
      ELSE
        jwndo = 300
      END IF
      PRINT *, 'TIME-WINDOW=', jwndo, ' HUNDRETHS OF AN HOUR'
 
C     READ THE RETENTION OBTYPES
C     --------------------------
 
      READ (5,'(A80)')
 
      nobtyp = 0
 
      DO n = 1, mxrt
        READ (5,*,END=10) rtyp(n)
        print*,'n,rtyp(n)=',n,rtyp(n)
        nobtyp = n
      END DO
 
   10 IF (nobtyp.eq.0) THEN
        STOP 'EMPTY RETENTION LIST'
      ELSE
        PRINT *
        DO n = 1, nobtyp
          tfac(n) = 0
          IF (rtyp(n).lt.1000) tfac(n) = 1
          IF (rtyp(n).lt.100) tfac(n) = 10
          IF (rtyp(n).lt.10) tfac(n) = 100
          IF (rtyp(n).eq.0) tfac(n) = 1000
c         IF (tfac(n).eq.0) CALL abort('BAD OBTYPE PARAMETER INPUT')
          IF (tfac(n).eq.0) then
              print*,'BAD OBTYPE PARAMETER INPUT'
              CALL abort
          endif
          PRINT 1000, n, rtyp(n), iretgrid, jdate, jwndo
 1000     FORMAT ('RETENTION OBTYP ',I2,F6.0,3I9)
        END DO
      END IF
 
C     MAKE A TABLE OUT OF THE OBSERVATION TYPS, LATS, AND LONS
C     --------------------------------------------------------
 
      CALL datebf(lubfi,iy,im,id,ih,idate)
      idatew=idate
      print*,'idate=',idate
      PRINT *, 'DATE OF INPUT BUFR FILE ', idate
      if(rtyp(1).eq.256.0) then
      call ufbtab(lubfi,tab,mxts,mxtb,ntab,
     *     'CLONH CLATH YEAR MNTH DAYS HOUR MINU SAID') 
      PRINT *, 'TABULATED ', ntab, ' REPORTS FROM INPUT BUFR FILE'
      print*,'tab(8,n)=',tab(8,n)
      DO n = 1, ntab
        xob(n) = tab(1,n)
        yob(n) = tab(2,n)
c       typ(n) = tab(3,n)
c       dhr(n) = tab(4,n)
        if(tab(6,n).eq.2.0) then
        print*,'tab(5,n),tab(6,n),tab(7,n)=',tab(5,n),tab(6,n),tab(7,n)
        endif
        ihr=0
        if(tab(7,n).gt.30) ihr=60
        dhr(n) = abs(ihr-tab(7,n))/100.
c       if(n.eq.10000) then
c       if(n.ge.1.and.n.le.500.and.typ(n).eq.120.0) then
c       if(typ(n).eq.120.0) then
c         print*,'xob,yob,typ,dhr=',n,xob(n),yob(n),typ(n),dhr(n)
c       endif
      END DO
      else
      CALL ufbtab(lubfi,tab,mxts,mxtb,ntab,'XOB YOB TYP DHR')
      PRINT *, 'TABULATED ', ntab, ' REPORTS FROM INPUT BUFR FILE'

      DO n = 1, ntab
        xob(n) = tab(1,n)
        yob(n) = tab(2,n)
        typ(n) = tab(3,n)
        dhr(n) = tab(4,n)
c       if(n.eq.10000) then
c       if(n.ge.1.and.n.le.500.and.typ(n).eq.120.0) then
c       if(typ(n).eq.120.0) then
c         print*,'xob,yob,typ,dhr=',n,xob(n),yob(n),typ(n),dhr(n)
c       endif
      END DO

      endif
 
C     MARK THE OBSERVATIONS LOCATED WITHIN BOTH GRID & TIME-WINDOW
C     ------------------------------------------------------------
 
      ikep = 0
      nwith = 0
      nwndo = 0
c     N = 1 
c     print *,N,ALAT1,ELON1,DXX,ELONV,ALATAN
c     print *,N,IMAX,JMAX,KMAX
      DO n = 1, ntab
 
C       Depending on the grid type,
C       CALCULATE Grid coordinates of obs lat,long 
C       ------------------------------------------
c       print*,'latlong=',latlong
      if(igrid.eq.1) then
        IF (latlong) THEN
C         Latitiude - Longitude grid  - NOT global
c         print*,'elon1,alat1=',elon1,alat1
c         print*,'xob(n)-elon1=',xob(n)-elon1
c         print*,'yob(n)-alat1=',yob(n)-alat1
          if(xob(n).lt.0) xob(n)=xob(n)+360.0
          xi = (xob(n)-elon1) / dxx + 1.0
          yj = (yob(n)-alat1) / dyy + 1.0
c         print*,'xob(n),yob(n),xi,yj=',xob(n),yob(n),xi,yj
        END IF
        IF (polarstereo) THEN
C         Polar Stereographic grid
C         W3FB06 wants grid spacing in meters
          dxm = dxx * 1000.
          CALL w3fb06(yob(n),xob(n),alat1,elon1,dxm,elonv,xi,yj)
        END IF
        IF (lambert) THEN
C         Lambert Conic Conformal grid
C         W3FB11 wants grid spacing in meters
          dxm = dxx * 1000.
          CALL w3fb11(yob(n),xob(n),alat1,elon1,dxm,elonv,alatan,xi,yj)
        END IF
        kxi = INT(xi)
        kyj = INT(yj)
c       print*,'xob(n),yob(n),xi,yj=',xob(n),yob(n),kxi,kyj
c       IF(N.EQ.1.OR.MOD(N,500).EQ.0) THEN
c       print *,N,YOB(N),XOB(N),TYP(N),DHR(N)
c       print *,N,KXI,KYJ,IMAX,JMAX
c       ENDIF

C       CHECK IF OB IS WITHIN DOMAIN
C       ----------------------------
        IF ( kxi .lt. 1 .or. kxi .gt. imax .or.
     +	     kyj .lt. 1 .or. kyj .gt. jmax ) THEN
c       if(xob(n).le.255.0.or.xob(n).ge.274.0.or.
c    *        yob(n).le.26.0.or.yob(n).ge.43.0) then
          within(n) = .false.
c         print*,'ob not in domain'
c         print*,'kxi,kyj=',kxi,kyj
c         print*,'n,xob(n),yob(n)=',n,xob(n),yob(n)
        ELSE
          within(n) = .true.
          nwith = nwith + 1
c         NOW CHECK IF OB IS ALSO WITHIN TIME WINDOW
c         ------------------------------------------
          IF (nint(abs(dhr(n)*100.)).gt.jwndo) THEN
            nwndo = nwndo + 1
            within(n) = .false.
          END IF
        END IF
      else
         if(xob(n).le.xlon1.or.xob(n).ge.xlon2.or.
     *        yob(n).le.xlat1.or.yob(n).ge.xlat2) then
          within(n) = .false.
c         print*,'ob not in domain'
c         print*,'n,xob(n),yob(n)=',n,xob(n),yob(n)
c         print*,'xlon1,xlon2,xlat1,xlat2=',
c    *      xlon1,xlon2,xlat1,xlat2
         else
          within(n) = .true.
          nwith = nwith + 1
c         NOW CHECK IF OB IS ALSO WITHIN TIME WINDOW
c         ------------------------------------------
          IF (nint(abs(dhr(n)*100.)).gt.jwndo) THEN
            nwndo = nwndo + 1
            within(n) = .false.
          END IF
         endif
      endif  !  if igrid.eq.1
      END DO
      PRINT *, 'GRID-AREA INCLUSIONS =', nwith
      PRINT *, 'TIME-WINDOW EXCLUSIONS =', nwndo
C     NOW CHECK OB TYPE
C     -----------------
      print*,'ntab,nobtyp=',ntab,nobtyp
      DO n = 1, ntab
        DO nr = 1, nobtyp
          keep = nint(rtyp(nr)) .eq. int(typ(n)/tfac(nr)) .or. 
     +                rtyp(nr) .eq. 0
c         if (keep.eqv..false.) then
c           print*,'rtyp(nr),typ(n),tfac(nr),n,nr=',
c    *        rtyp(nr),typ(n),tfac(nr),n,nr
c         endif
          if(rtyp(1).eq.256.0) then
          keep = .true.
          endif
          IF (within(n).and.keep) ikep(n) = nr
        END DO
      END DO
 
C     OPEN THE OUTPUT FILE (POSITION MOD IF THE FILE EXISTS)
C     ------------------------------------------------------
C     OPEN THE INPUT FILE TO READ THROUGH
C     -----------------------------------
 
      CALL openbf(lubfi,'IN ',lubfi)
      CALL datebf(lubfj,iy,im,id,ih,idate)
      IF (idate.ge.0) CALL openbf(lubfj,'APN',lubfi)
      IF (idate.lt.0) CALL openbf(lubfj,'OUT',lubfi)

C     WRITE OUT ALL DATA MARKED FOR RETENTION
C     ---------------------------------------
 
      isub = 0
      nrep = 0
      nrej = 0
      nret = 0
 
      DO WHILE (ireadmg(lubfi,subset,idate).eq.0)
        nsub = nmsub(lubfi)
        IF (isub.eq.0) THEN
          sslast = subset
          PRINT *, 'SUBSET,ISUB,IDATE ', subset, isub, idate
        END IF
        nrep = nrep + nsub
        adate = idate
c       print*,'nsub=',nsub
        DO n = 1, nsub
          isub = isub + 1
c         print*,'isub=',isub
          iobtyp = ikep(isub)
c         print*,'adate,dhr(isub)=',adate,dhr(isub)
c         print*,'Before call to raddate'
          if(abs(dhr(isub)).gt.0.1E+20) goto 99
          CALL raddate(adate,dhr(isub),bdate)
c         print*,'After call to raddate'
          kdate = bdate * .01
c         print*,'iobtyp=',iobtyp
          IF (iobtyp.gt.0.and.(kdate.eq.jdate.or.jdate.eq.0)) THEN
            IF (subset.ne.sslast) THEN
              PRINT *, 'SUBSET,ISUB,IDATE ', subset, isub, idate
              CALL closmg(lubfj)
              sslast = subset
            END IF
            CALL openmb(lubfj,subset,idatew)
            CALL copysb(lubfi,lubfj,iret)
            nret(iobtyp) = nret(iobtyp) + 1
          ELSE
c           print*,'In ELSE block of IF iobtyp block'
c           print*,'lubfi=',lubfi
            CALL copysb(lubfi,0,iret)
c           print*,'After call to copysb, iret=',iret
            nrej = nrej + 1
99        END IF
        END DO
      END DO
 
      CALL closbf(lubfi)
      CALL closbf(lubfj)
 
C     GENERATE REPORT AND EXIT
C     ------------------------
 
      PRINT *
      DO n = 1, nobtyp
        PRINT *, 'RETAINED REPORTS OF TYPE#', n, rtyp(n), '=', nret(n)
      END DO
      mret = nrep - nrej
      PRINT *
      PRINT *, 'TOTAL INCLUDED REPORTS=', mret
      PRINT *
      PRINT *, 'TOTAL EXCLUDED REPORTS=', nrej
      PRINT *
      PRINT *, 'EDITBUFR PROCESSED ', nrep, ' REPORTS '
      PRINT *
 
      STOP
      END
