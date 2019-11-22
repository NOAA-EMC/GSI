program adjustps    
!
!ifort -I${NEMSIO_INC} adjustps.f90 ${NEMSIO_LIB} ${W3NCO_LIB4} ${BACIO_LIB4}
!
!$$$  main program documentation block
!
! program:  adjustps
!
! prgmmr: whitaker         org: esrl/psd               date: 2017-11-02
!
! abstract:  change orography in file 1 to match file 2, adjust ps
!            to new orography, interpolate 3d fields to new pressures, 
!            write out updated file.
!
! program history log:
!   2017-11-02  Initial version.
!
! usage: adjustps.x <file_1> <file_2> <fileout> <nlevt>
! nlevt is optional - sets level index for Benjamin and Miller temperature
! that is used in pressure adjustment.
!
! attributes:
!   language: f95
!
!$$$

  use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close,nemsio_charkind
  use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead,nemsio_readrec,&
       nemsio_writerec,nemsio_readrecv,nemsio_writerecv,nemsio_getrechead

  implicit none

  real,parameter:: zero=0.0_4, one=1.0_4

  character*500 filename_1,filename_2,filename_o
  character*3 charnlev
  integer iret,latb,lonb,nlevs,npts,k,n,nlevt,idsl
  integer nrec,nrec2,latb2,lonb2,nlevs2,npts2
  integer krecdp,ndpres,krect,krecq,krecu,krecv,ntrac,kq,kt,krecoz,kreccwmr,krecicmr
  real,allocatable,dimension(:,:,:) :: vcoord
  real,allocatable,dimension(:,:) ::&
  rwork_1,rwork_2,pressi,pressl,pressi_new,pressl_new
  real,allocatable,dimension(:) :: delz,delps,ak,bk,t0
  character(len=nemsio_charkind),allocatable,dimension(:) :: recnam
  character(len=nemsio_charkind) field
  real tpress,tv,kap1,kapr,rd,cp,grav,rlapse,alpha,ps,preduced,zob,zmodel,rv,fv
  type(nemsio_gfile) :: gfile_1,gfile_2,gfile_o
  logical ldpres 

! constants.
  grav = 9.8066
  rlapse = 0.0065
  rd = 287.05
  rv = 461.5
  fv = rv/(rd-one)
  cp = 1004.
  kap1 = (rd/cp)+1.0
  kapr = (cp/rd)
  alpha = rd*rlapse/grav

  call w3tagb('ADJUSTPS',2011,0319,0055,'NP25')

! read data from this file
  call getarg(1,filename_1)

! subtract this mean
  call getarg(2,filename_2)

! then add to this mean
  call getarg(3,filename_o)

! model level to use for Benjamin and Miller pressure adjustment
  if (iargc() > 3) then
    call getarg(4,charnlev)
    read(charnlev,'(i3)') nlevt
  else
    nlevt = 1 ! default value
  endif

  write(6,*)'ADJUSTPS:'
  write(6,*)'filename_1=',trim(filename_1)
  write(6,*)'filename_2=',trim(filename_2)
  write(6,*)'filename_o=',trim(filename_o)
  write(6,*)'nlevt=',nlevt

  call nemsio_open(gfile_1,trim(filename_1),'READ',iret=iret)
  if (iret == 0 ) then
      write(6,*)'Read nemsio ',trim(filename_1),' iret=',iret
      call nemsio_getfilehead(gfile_1, nrec=nrec, dimx=lonb, dimy=latb, dimz=nlevs, idsl=idsl,iret=iret)
      write(6,*)' lonb=',lonb,' latb=',latb,' levs=',nlevs,' nrec=',nrec
  else
      write(6,*)'***ERROR*** ',trim(filename_1),' contains unrecognized format.  ABORT'
  endif
  ! is dpres in the file?
  allocate(recnam(nrec))
  call nemsio_getfilehead(gfile_1,recname=recnam,iret=iret)
  ldpres = .false.
  ndpres = 0
  field = 'dpres'
  do n=1,nrec
     !print *,n,trim(field),' ',trim(recnam(n))
     if (trim(field) == trim(recnam(n))) then
        ldpres = .true.
        ndpres = 1
        exit
     endif
  enddo
  print *,'ldpres = ',ldpres

  call nemsio_open(gfile_2,trim(filename_2),'READ',iret=iret)
  if (iret /= 0) then
     print *,'Error opening ',trim(filename_2)
     stop
  endif
  call nemsio_getfilehead(gfile_2, nrec=nrec2, dimx=lonb2, dimy=latb2, dimz=nlevs2, iret=iret)

  npts=lonb*latb
  npts2=lonb2*latb2
  ! assumes ps,zs are first two records, then u,v,t,optionally dpres,q,oz,cwmr and optionally icmr
  if (nrec > 2 + (7+ndpres)*nlevs) then
     print *,'cannot handle nrec > ',2 + 7*nlevs
     stop
  endif
  ! q, oz, then microphys tracers are last (after u,v,T,dpres)
  krecq    = 2 + (3+ndpres)*nlevs + 1
  ntrac = (nrec-(krecq-1))/nlevs
  print *,'ntrac,nrec,idsl',ntrac,nrec,idsl
  if (npts .ne. npts2 .or. nlevs .ne. nlevs2) then
     print *,'grid size in file not what is expected, aborting..'
     stop
  endif
  allocate(rwork_1(npts,nrec))
  allocate(rwork_2(npts,nrec))
  allocate(delz(npts))
  allocate(delps(npts))
  allocate(t0(npts))
  allocate(pressi(npts,nlevs+1))
  allocate(pressi_new(npts,nlevs+1))
  allocate(pressl(npts,nlevs))
  allocate(pressl_new(npts,nlevs))
  allocate(ak(nlevs+1))
  allocate(bk(nlevs+1))
  rwork_1 = zero; rwork_2 = zero
  allocate(vcoord(nlevs+1,3,2))
  call nemsio_getfilehead(gfile_1,vcoord=vcoord,iret=iret)
  if (iret /= 0) then
     print *,'Error reading vcoord from ',trim(filename_1)
     stop
  endif
  ak = vcoord(:,1,1); bk = vcoord(:,2,1)
  deallocate(vcoord)

  ! read ps,zs from filename_2
  call nemsio_readrecv(gfile_2,'pres','sfc',1,rwork_2(:,1),iret=iret)
  if (iret /= 0) then
     print *,'Error reading ps from ',trim(filename_2)
     stop
  endif
  call nemsio_readrecv(gfile_2,'hgt','sfc',1,rwork_2(:,2),iret=iret)
  if (iret /= 0) then
     print *,'Error reading zs from ',trim(filename_1)
     stop
  endif
  ! read all fields from filename_1
  call nemsio_readrecv(gfile_1,'pres','sfc',1,rwork_1(:,1),iret=iret)
  if (iret /= 0) then
     print *,'Error reading ps from ',trim(filename_1)
     stop
  endif
  call nemsio_readrecv(gfile_1,'hgt','sfc',1,rwork_1(:,2),iret=iret)
  if (iret /= 0) then
     print *,'Error reading zs from ',trim(filename_1)
     stop
  endif
  print *,minval(rwork_1(:,1)),maxval(rwork_1(:,1))
  print *,minval(rwork_2(:,1)),maxval(rwork_2(:,1))
  print *,minval(rwork_1(:,2)),maxval(rwork_1(:,2))
  print *,minval(rwork_2(:,2)),maxval(rwork_2(:,2))
  do k = 1,nlevs
      krecu    = 2 + 0*nlevs + k
      krecv    = 2 + 1*nlevs + k
      krect    = 2 + 2*nlevs + k
      krecdp   = 2 + 3*nlevs + k
      krecq    = 2 + (3+ndpres)*nlevs + k
      krecoz   = 2 + (4+ndpres)*nlevs + k
      kreccwmr = 2 + (5+ndpres)*nlevs + k
      if (nrec > 2 + (6+ndpres)*nlevs) then
         krecicmr = 2 + (6+ndpres)*nlevs + k
      endif
      call nemsio_readrecv(gfile_1,'ugrd', 'mid layer',k,rwork_1(:,krecu),   iret=iret)
      if (iret /= 0) then
         print *,'Error reading u from ',trim(filename_1),k
         stop
      endif
      call nemsio_readrecv(gfile_1,'vgrd', 'mid layer',k,rwork_1(:,krecv),   iret=iret)
      if (iret /= 0) then
         print *,'Error reading v from ',trim(filename_1),k
         stop
      endif
      call nemsio_readrecv(gfile_1,'tmp',  'mid layer',k,rwork_1(:,krect),   iret=iret)
      if (iret /= 0) then
         print *,'Error reading t from ',trim(filename_1),k
         stop
      endif
      if (ldpres) then
         call nemsio_readrecv(gfile_1,'dpres',  'mid layer',k,rwork_1(:,krecdp),   iret=iret)
         if (iret /= 0) then
            print *,'Error reading dpres from ',trim(filename_1),k
            stop
         endif
         !print *,k,minval(rwork_1(:,krecdp)),maxval(rwork_1(:,krecdp))
      endif
      call nemsio_readrecv(gfile_1,'spfh', 'mid layer',k,rwork_1(:,krecq),   iret=iret)
      if (iret /= 0) then
         print *,'Error reading q from ',trim(filename_1),k
         stop
      endif
      call nemsio_readrecv(gfile_1,'o3mr', 'mid layer',k,rwork_1(:,krecoz),  iret=iret)
      if (iret /= 0) then
         print *,'Error reading o3 from ',trim(filename_1),k
         stop
      endif
      call nemsio_readrecv(gfile_1,'clwmr','mid layer',k,rwork_1(:,kreccwmr),iret=iret)
      if (iret /= 0) then
         print *,'Error reading cwmr from ',trim(filename_1),k
         stop
      endif
      if (nrec > 2 + 6*nlevs) then
      call nemsio_readrecv(gfile_1,'icmr','mid layer',k,rwork_1(:,krecicmr),iret=iret)
      if (iret /= 0) then
         print *,'Error reading icmr from ',trim(filename_1),k
         stop
      endif
      endif
  enddo

  delz = rwork_1(:,2) - rwork_2(:,2)
  delps = rwork_1(:,1) - rwork_2(:,1)
  print *,'min/max delz = ',minval(delz),maxval(delz)
  print *,'min/max delps = ',minval(delps),maxval(delps)
  if (iret /= 0) then
     print *,'Error closing ',trim(filename_1)
     stop
  endif

  !==> pressure at layers and interfaces.
  do k=1,nlevs+1
     pressi(:,k)=ak(k)+bk(k)*rwork_1(:,1) 
  enddo
  if (idsl == 2) then
! IDSL: TYPE OF SIGMA STRUCTURE (1 FOR PHILLIPS OR 2 FOR MEAN)
     do k=1,nlevs
        pressl(:,k)=0.5*(pressi(:,k)+pressi(:,k+1))
     end do
  else
     do k=1,nlevs
        ! "phillips" vertical interpolation
        pressl(:,k)=((pressi(:,k)**kap1-pressi(:,k+1)**kap1)/&
                     (kap1*(pressi(:,k)-pressi(:,k+1))))**kapr
     end do
  endif

  ! adjust surface pressure.
  ! update first two fields in output (rwork_2)
  do n=1,npts
! compute MAPS pressure reduction from model to station elevation
! See Benjamin and Miller (1990, MWR, p. 2100)
! uses 'effective' surface temperature extrapolated
! from virtual temp (tv) at pressure tpress
! using standard atmosphere lapse rate.
! ps - surface pressure to reduce.
! t - virtual temp. at pressure tpress.
! zmodel - model orographic height.
! zob - station height
     kt    = 2 + 2*nlevs + nlevt
     kq    = 2 + (3+ndpres)*nlevs + nlevt
     tv = (1.+fv*rwork_1(n,kq))*rwork_1(n,kt)
     tpress = pressl(n,nlevt); ps = rwork_1(n,1)
     zmodel = rwork_2(n,2); zob = rwork_1(n,2)
     t0(n) = tv*(ps/tpress)**alpha ! eqn 4 from B&M
     preduced = ps*((t0(n) + rlapse*(zob-zmodel))/t0(n))**(1./alpha) ! eqn 1 from B&M
     delps(n) = ps-preduced 
     rwork_1(n,1) = rwork_2(n,1) ! save old ps 
     rwork_2(n,1) = preduced     ! new surface pressure adjusted to new orography
  enddo
  print *,'min/max effective surface t',minval(t0),maxval(t0)
  print *,'min/max ps adjustment',minval(delps),maxval(delps)
  delps = rwork_1(:,1) - rwork_2(:,1)
  print *,'min/max delps after adjustment = ',minval(delps),maxval(delps)
  !==> new pressure at layers and interfaces.
  do k=1,nlevs+1
     pressi_new(:,k)=ak(k)+bk(k)*rwork_2(:,1)  ! updated ps
  enddo
  if (idsl == 2) then
     do k=1,nlevs
        pressl_new(:,k)=0.5*(pressi_new(:,k)+pressi_new(:,k+1))
     end do
  else
     do k=1,nlevs
        pressl_new(:,k)=((pressi_new(:,k)**kap1-pressi_new(:,k+1)**kap1)/&
                        (kap1*(pressi_new(:,k)-pressi_new(:,k+1))))**kapr
     end do
  endif
  gfile_o=gfile_1
  call nemsio_open(gfile_o,trim(filename_o),'WRITE',iret=iret)
  if (iret /= 0) then
     print *,'Error opening ',trim(filename_o)
     stop
  endif
! interpolate fields to new pressures (update rest of rwork_2).
  krecu    = 2 + 0*nlevs + 1
  krecv    = 2 + 1*nlevs + 1
  krect    = 2 + 2*nlevs + 1
  krecq    = 2 + (3+ndpres)*nlevs + 1
  print *,'min/max pressi diff',minval(pressi-pressi_new),maxval(pressi-pressi_new)
  print *,'min/max pressl diff',minval(pressl-pressl_new),maxval(pressl-pressl_new)
  call vintg(npts,npts,nlevs,nlevs,ntrac,pressl,&
             rwork_1(:,krecu:krecu+nlevs-1),rwork_1(:,krecv:krecv+nlevs-1),&
             rwork_1(:,krect:krect+nlevs-1),rwork_1(:,krecq:nrec),&
             pressl_new,&
             rwork_2(:,krecu:krecu+nlevs-1),rwork_2(:,krecv:krecv+nlevs-1),&
             rwork_2(:,krect:krect+nlevs-1),rwork_2(:,krecq:nrec))
  print *,'min/max u diff',minval(rwork_1(:,krecu:krecu+nlevs-1)-rwork_2(:,krecu:krecu+nlevs-1)),&
                           maxval(rwork_1(:,krecu:krecu+nlevs-1)-rwork_2(:,krecu:krecu+nlevs-1))
  print *,'min/max v diff',minval(rwork_1(:,krecv:krecv+nlevs-1)-rwork_2(:,krecv:krecv+nlevs-1)),&
                           maxval(rwork_1(:,krecv:krecv+nlevs-1)-rwork_2(:,krecv:krecv+nlevs-1))
  print *,'min/max t diff',minval(rwork_1(:,krect:krect+nlevs-1)-rwork_2(:,krect:krect+nlevs-1)),&
                           maxval(rwork_1(:,krect:krect+nlevs-1)-rwork_2(:,krect:krect+nlevs-1))
  print *,'min/max q diff',minval(rwork_1(:,krecq:krecq+nlevs-1)-rwork_2(:,krecq:krecq+nlevs-1)),&
                           maxval(rwork_1(:,krecq:krecq+nlevs-1)-rwork_2(:,krecq:krecq+nlevs-1))
  print *,'min/max tracer diff',minval(rwork_1(:,krecq+nlevs:nrec)-rwork_2(:,krecq+nlevs:nrec)),&
                           maxval(rwork_1(:,krecq+nlevs:nrec)-rwork_2(:,krecq+nlevs:nlevs))
  ! write out all fields to filename_o
  call nemsio_writerecv(gfile_o,'pres','sfc',1,rwork_2(:,1),iret=iret)
  if (iret /= 0) then
     print *,'Error writing ps to ',trim(filename_o)
     stop
  else
     print *,'wrote ps ',k,minval(rwork_2(:,1)),maxval(rwork_2(:,1))
  endif
  call nemsio_writerecv(gfile_o,'hgt','sfc',1,rwork_2(:,2),iret=iret)
  if (iret /= 0) then
     print *,'Error writing zs to ',trim(filename_o)
     stop
  else
     print *,'wrote zs ',k,minval(rwork_2(:,2)),maxval(rwork_2(:,2))
  endif
  do k = 1,nlevs
      krecu    = 2 + 0*nlevs + k
      krecv    = 2 + 1*nlevs + k
      krect    = 2 + 2*nlevs + k
      if (ldpres) then
         ndpres = 1
         krecdp = 2 + 3*nlevs + k
      else
         ndpres = 0
      endif
      krecq    = 2 + (3+ndpres)*nlevs + k
      krecoz   = 2 + (4+ndpres)*nlevs + k
      kreccwmr = 2 + (5+ndpres)*nlevs + k
      if (nrec > 2 + (6+ndpres)*nlevs) then
         krecicmr = 2 + (6+ndpres)*nlevs + k
      endif
      call nemsio_writerecv(gfile_o,'ugrd', 'mid layer',k,rwork_2(:,krecu),   iret=iret)
      if (iret /= 0) then
         print *,'Error writing u to ',trim(filename_o),k
         stop
      else
         print *,'wrote u level ',k,minval(rwork_2(:,krecu)),maxval(rwork_2(:,krecu))
      endif
      call nemsio_writerecv(gfile_o,'vgrd', 'mid layer',k,rwork_2(:,krecv),   iret=iret)
      if (iret /= 0) then
         print *,'Error writing v to ',trim(filename_o),k
         stop
      else
         print *,'wrote v level ',k,minval(rwork_2(:,krecv)),maxval(rwork_2(:,krecv))
      endif
      call nemsio_writerecv(gfile_o,'tmp',  'mid layer',k,rwork_2(:,krect),   iret=iret)
      if (iret /= 0) then
         print *,'Error writing t to ',trim(filename_o),k
         stop
      else
         print *,'wrote t level ',k,minval(rwork_2(:,krect)),maxval(rwork_2(:,krect))
      endif
      if (ldpres) then
         rwork_2(:,krecdp) = pressi_new(:,k)-pressi_new(:,k+1)
         call nemsio_writerecv(gfile_o,'dpres',  'mid layer',k,rwork_2(:,krecdp),   iret=iret)
         if (iret /= 0) then
            print *,'Error writing dpres to ',trim(filename_o),k
            stop
         else
            print *,'wrote dpres level ',k,minval(rwork_2(:,krecdp)),maxval(rwork_2(:,krecdp))
         endif
      endif
      call nemsio_writerecv(gfile_o,'spfh', 'mid layer',k,rwork_2(:,krecq),   iret=iret)
      if (iret /= 0) then
         print *,'Error writing q to ',trim(filename_o),k
         stop
      else
         print *,'wrote q level ',k,minval(rwork_2(:,krecq)),maxval(rwork_2(:,krecq))
      endif
      call nemsio_writerecv(gfile_o,'o3mr', 'mid layer',k,rwork_2(:,krecoz),  iret=iret)
      if (iret /= 0) then
         print *,'Error writing o3 to ',trim(filename_o),k
         stop
      else
         print *,'wrote o3 level ',k,minval(rwork_2(:,krecoz)),maxval(rwork_2(:,krecoz))
      endif
      call nemsio_writerecv(gfile_o,'clwmr','mid layer',k,rwork_2(:,kreccwmr),iret=iret)
      if (iret /= 0) then
         print *,'Error writing cwmr to ',trim(filename_o),k
         stop
      else
         print *,'wrote cwmr level ',k,minval(rwork_2(:,kreccwmr)),maxval(rwork_2(:,kreccwmr))
      endif
      if (nrec > 2 + 6*nlevs) then
      call nemsio_writerecv(gfile_o,'icmr','mid layer',k,rwork_2(:,krecicmr),iret=iret)
      if (iret /= 0) then
         print *,'Error writing icmr to ',trim(filename_o),k
         stop
      else
         print *,'wrote icmr level ',k,minval(rwork_2(:,krecicmr)),maxval(rwork_2(:,krecicmr))
      endif
      endif
  enddo
  deallocate(delps,delz,t0)
  deallocate(rwork_1,rwork_2)
  deallocate(ak,bk,pressi,pressl,pressi_new,pressl_new)
  call nemsio_close(gfile_o,iret=iret)
  if (iret /= 0) then
     print *,'Error closing ',trim(filename_o)
     stop
  endif
  call nemsio_close(gfile_1,iret=iret)
  if (iret /= 0) then
     print *,'Error closing ',trim(filename_1)
     stop
  endif
  call nemsio_close(gfile_2,iret=iret)
  if (iret /= 0) then
     print *,'Error closing ',trim(filename_2)
     stop
  endif

  call w3tage('ADJUSTPS')

END program adjustps

! these routines copied from global_chgres with minor mods to VINTG

      SUBROUTINE VINTG(IM,IX,KM1,KM2,NT,P1,U1,V1,T1,Q1,P2, &
                       U2,V2,T2,Q2)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    VINTG       VERTICALLY INTERPOLATE UPPER-AIR FIELDS
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
!
! ABSTRACT: VERTICALLY INTERPOLATE UPPER-AIR FIELDS.
!   WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS ARE INTERPOLATED.
!   THE INTERPOLATION IS CUBIC LAGRANGIAN IN LOG PRESSURE
!   WITH A MONOTONIC CONSTRAINT IN THE CENTER OF THE DOMAIN.
!   IN THE OUTER INTERVALS IT IS LINEAR IN LOG PRESSURE.
!   OUTSIDE THE DOMAIN, FIELDS ARE GENERALLY HELD CONSTANT,
!   EXCEPT FOR TEMPERATURE AND HUMIDITY BELOW THE INPUT DOMAIN,
!   WHERE THE TEMPERATURE LAPSE RATE IS HELD FIXED AT -6.5 K/KM AND
!   THE RELATIVE HUMIDITY IS HELD CONSTANT.
!
! PROGRAM HISTORY LOG:
!   91-10-31  MARK IREDELL
!
! USAGE:    CALL VINTG(IM,IX,KM1,KM2,NT,P1,U1,V1,T1,Q1,P2,
!    &                 U2,V2,T2,Q2)
!   INPUT ARGUMENT LIST:
!     IM           INTEGER NUMBER OF POINTS TO COMPUTE
!     IX           INTEGER FIRST DIMENSION
!     KM1          INTEGER NUMBER OF INPUT LEVELS
!     KM2          INTEGER NUMBER OF OUTPUT LEVELS
!     NT           INTEGER NUMBER OF TRACERS
!     P1           REAL (IX,KM1) INPUT PRESSURES
!                  ORDERED FROM BOTTOM TO TOP OF ATMOSPHERE
!     U1           REAL (IX,KM1) INPUT ZONAL WIND
!     V1           REAL (IX,KM1) INPUT MERIDIONAL WIND
!     T1           REAL (IX,KM1) INPUT TEMPERATURE (K)
!     Q1           REAL (IX,KM1,NT) INPUT TRACERS (HUMIDITY FIRST)
!     P2           REAL (IX,KM2) OUTPUT PRESSURES
!   OUTPUT ARGUMENT LIST:
!     U2           REAL (IX,KM2) OUTPUT ZONAL WIND
!     V2           REAL (IX,KM2) OUTPUT MERIDIONAL WIND
!     T2           REAL (IX,KM2) OUTPUT TEMPERATURE (K)
!     Q2           REAL (IX,KM2,NT) OUTPUT TRACERS (HUMIDITY FIRST)
!
! SUBPROGRAMS CALLED:
!   TERP3        CUBICALLY INTERPOLATE IN ONE DIMENSION
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN
!
!C$$$
      INTEGER, INTENT(IN) :: IX,KM1,KM2,NT
      REAL, INTENT(IN) :: P1(IX,KM1),U1(IX,KM1),V1(IX,KM1),T1(IX,KM1),Q1(IX,NT*KM1)
!    &     ,W1(IX,KM1)
      REAL, INTENT(IN) :: P2(IX,KM2)
      REAL, INTENT(OUT) :: U2(IX,KM2),V2(IX,KM2),T2(IX,KM2),Q2(IX,NT*KM2)
!    &     ,W2(IX,KM2)
      REAL,  PARAMETER :: DLTDZ=-6.5E-3*287.05/9.80665
      REAL,  PARAMETER :: DLPVDRT=-2.5E6/461.50

      REAL,allocatable :: Z1(:,:),Z2(:,:)
      REAL,allocatable :: C1(:,:,:),C2(:,:,:),J2(:,:,:)
      real dz
      integer             :: im,k,i,n
!
      allocate (Z1(IM+1,KM1),Z2(IM+1,KM2))
      allocate (C1(IM+1,KM1,4+NT),C2(IM+1,KM2,4+NT),J2(IM+1,KM2,4+NT))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE LOG PRESSURE INTERPOLATING COORDINATE
!  AND COPY INPUT WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS
!!$OMP PARALLEL DO DEFAULT(SHARED)
!!$OMP+ PRIVATE(K,I)
      !print *,minval(u1),maxval(u1)
      !print *,minval(t1),maxval(t1)
      DO K=1,KM1
        DO I=1,IM
          Z1(I,K)   = -LOG(P1(I,K))
          C1(I,K,1) =  U1(I,K)
          C1(I,K,2) =  V1(I,K)
!         C1(I,K,3) =  W1(I,K)
          C1(I,K,4) =  T1(I,K)
          C1(I,K,5) =  Q1(I,K)
        ENDDO
      ENDDO
!!$OMP END PARALLEL DO
      DO N=2,NT
        DO K=1,KM1
          DO I=1,IM
            C1(I,K,4+N) = Q1(I,(N-1)*KM1+K)
          ENDDO
        ENDDO
      ENDDO
!      print *,' p2=',p2(1,:)
!      print *,' im=',im,' km2=',km2,' ix=',ix,'nt=',nt
!!$OMP PARALLEL DO DEFAULT(SHARED)
!!$OMP+ PRIVATE(K,I)
      DO K=1,KM2
        DO I=1,IM
          Z2(I,K) = -LOG(P2(I,K))
        ENDDO
      ENDDO
!!$OMP END PARALLEL DO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  PERFORM LAGRANGIAN ONE-DIMENSIONAL INTERPOLATION
!  THAT IS 4TH-ORDER IN INTERIOR, 2ND-ORDER IN OUTSIDE INTERVALS
!  AND 1ST-ORDER FOR EXTRAPOLATION.
      CALL TERP3(IM,1,1,1,1,4+NT,(IM+1)*KM1,(IM+1)*KM2,&
                 KM1,IM+1,IM+1,Z1,C1,KM2,IM+1,IM+1,Z2,C2,J2)
!      print *,' c2=',maxval(c2(1,:,:)),minval(c2(1,:,:))
!     print *,' j2:=',j2(1,1,:)
!     print *,' j2:=',j2(im,km2,:)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COPY OUTPUT WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS
!  EXCEPT BELOW THE INPUT DOMAIN, LET TEMPERATURE INCREASE WITH A FIXED
!  LAPSE RATE AND LET THE RELATIVE HUMIDITY REMAIN CONSTANT.
      DO K=1,KM2
        DO I=1,IM
          U2(I,K)=C2(I,K,1)
          V2(I,K)=C2(I,K,2)
!         W2(I,K)=C2(I,K,3)
          DZ=Z2(I,K)-Z1(I,1)
          IF(DZ.GE.0) THEN
            T2(I,K)=C2(I,K,4)
            Q2(I,K)=C2(I,K,5)
          ELSE
            T2(I,K)=T1(I,1)*EXP(DLTDZ*DZ)
            Q2(I,K)=Q1(I,1)*EXP(DLPVDRT*(1/T2(I,K)-1/T1(I,1))-DZ)
          ENDIF
        ENDDO
      ENDDO
      DO N=2,NT
        DO K=1,KM2
          DO I=1,IM
            Q2(I,(N-1)*KM2+K)=C2(I,K,4+N)
          ENDDO
        ENDDO
      ENDDO
      !print *,minval(u2),maxval(u2)
      !print *,minval(t2),maxval(t2)
      deallocate (Z1,Z2,C1,C2,J2)
      END

      SUBROUTINE TERP3(IM,IXZ1,IXQ1,IXZ2,IXQ2,NM,NXQ1,NXQ2, &
                       KM1,KXZ1,KXQ1,Z1,Q1,KM2,KXZ2,KXQ2,Z2,Q2,J2)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    TERP3       CUBICALLY INTERPOLATE IN ONE DIMENSION
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 98-05-01
!
! ABSTRACT: INTERPOLATE FIELD(S) IN ONE DIMENSION ALONG THE COLUMN(S).
!   THE INTERPOLATION IS CUBIC LAGRANGIAN WITH A MONOTONIC CONSTRAINT
!   IN THE CENTER OF THE DOMAIN.  IN THE OUTER INTERVALS IT IS LINEAR.
!   OUTSIDE THE DOMAIN, FIELDS ARE HELD CONSTANT.
!
! PROGRAM HISTORY LOG:
!   98-05-01  MARK IREDELL
! 1999-01-04  IREDELL  USE ESSL SEARCH
!
! USAGE:    CALL TERP3(IM,IXZ1,IXQ1,IXZ2,IXQ2,NM,NXQ1,NXQ2,
!    &                 KM1,KXZ1,KXQ1,Z1,Q1,KM2,KXZ2,KXQ2,Z2,Q2,J2)
!   INPUT ARGUMENT LIST:
!     IM           INTEGER NUMBER OF COLUMNS
!     IXZ1         INTEGER COLUMN SKIP NUMBER FOR Z1
!     IXQ1         INTEGER COLUMN SKIP NUMBER FOR Q1
!     IXZ2         INTEGER COLUMN SKIP NUMBER FOR Z2
!     IXQ2         INTEGER COLUMN SKIP NUMBER FOR Q2
!     NM           INTEGER NUMBER OF FIELDS PER COLUMN
!     NXQ1         INTEGER FIELD SKIP NUMBER FOR Q1
!     NXQ2         INTEGER FIELD SKIP NUMBER FOR Q2
!     KM1          INTEGER NUMBER OF INPUT POINTS
!     KXZ1         INTEGER POINT SKIP NUMBER FOR Z1
!     KXQ1         INTEGER POINT SKIP NUMBER FOR Q1
!     Z1           REAL (1+(IM-1)*IXZ1+(KM1-1)*KXZ1)
!                  INPUT COORDINATE VALUES IN WHICH TO INTERPOLATE
!                  (Z1 MUST BE STRICTLY MONOTONIC IN EITHER DIRECTION)
!     Q1           REAL (1+(IM-1)*IXQ1+(KM1-1)*KXQ1+(NM-1)*NXQ1)
!                  INPUT FIELDS TO INTERPOLATE
!     KM2          INTEGER NUMBER OF OUTPUT POINTS
!     KXZ2         INTEGER POINT SKIP NUMBER FOR Z2
!     KXQ2         INTEGER POINT SKIP NUMBER FOR Q2
!     Z2           REAL (1+(IM-1)*IXZ2+(KM2-1)*KXZ2)
!                  OUTPUT COORDINATE VALUES TO WHICH TO INTERPOLATE
!                  (Z2 NEED NOT BE MONOTONIC)
!     
!   OUTPUT ARGUMENT LIST:
!     Q2           REAL (1+(IM-1)*IXQ2+(KM2-1)*KXQ2+(NM-1)*NXQ2)
!                  OUTPUT INTERPOLATED FIELDS
!     J2           REAL (1+(IM-1)*IXQ2+(KM2-1)*KXQ2+(NM-1)*NXQ2)
!                  OUTPUT INTERPOLATED FIELDS CHANGE WRT Z2
!
! SUBPROGRAMS CALLED:
!   RSEARCH      SEARCH FOR A SURROUNDING REAL INTERVAL
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN
!
!C$$$
      IMPLICIT NONE
      INTEGER IM,IXZ1,IXQ1,IXZ2,IXQ2,NM,NXQ1,NXQ2
      INTEGER KM1,KXZ1,KXQ1,KM2,KXZ2,KXQ2
      INTEGER I,K1,K2,N
      REAL Z1(1+(IM-1)*IXZ1+(KM1-1)*KXZ1)
      REAL Q1(1+(IM-1)*IXQ1+(KM1-1)*KXQ1+(NM-1)*NXQ1)
      REAL Z2(1+(IM-1)*IXZ2+(KM2-1)*KXZ2)
      REAL Q2(1+(IM-1)*IXQ2+(KM2-1)*KXQ2+(NM-1)*NXQ2)
      REAL J2(1+(IM-1)*IXQ2+(KM2-1)*KXQ2+(NM-1)*NXQ2)
      REAL FFA(IM),FFB(IM),FFC(IM),FFD(IM)
      REAL GGA(IM),GGB(IM),GGC(IM),GGD(IM)
      INTEGER K1S(IM,KM2)
      REAL Z1A,Z1B,Z1C,Z1D,Q1A,Q1B,Q1C,Q1D,Z2S,Q2S,J2S
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  FIND THE SURROUNDING INPUT INTERVAL FOR EACH OUTPUT POINT.
      CALL RSEARCH(IM,KM1,IXZ1,KXZ1,Z1,KM2,IXZ2,KXZ2,Z2,1,IM,K1S)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GENERALLY INTERPOLATE CUBICALLY WITH MONOTONIC CONSTRAINT
!  FROM TWO NEAREST INPUT POINTS ON EITHER SIDE OF THE OUTPUT POINT,
!  BUT WITHIN THE TWO EDGE INTERVALS INTERPOLATE LINEARLY.
!  KEEP THE OUTPUT FIELDS CONSTANT OUTSIDE THE INPUT DOMAIN.

!!$OMP PARALLEL DO DEFAULT(PRIVATE) SHARED(IM,IXZ1,IXQ1,IXZ2)
!!$OMP+ SHARED(IXQ2,NM,NXQ1,NXQ2,KM1,KXZ1,KXQ1,Z1,Q1,KM2,KXZ2)
!!$OMP+ SHARED(KXQ2,Z2,Q2,J2,K1S)

      DO K2=1,KM2
        DO I=1,IM
          K1=K1S(I,K2)
          IF(K1.EQ.1.OR.K1.EQ.KM1-1) THEN
            Z2S=Z2(1+(I-1)*IXZ2+(K2-1)*KXZ2)
            Z1A=Z1(1+(I-1)*IXZ1+(K1-1)*KXZ1)
            Z1B=Z1(1+(I-1)*IXZ1+(K1+0)*KXZ1)
            FFA(I)=(Z2S-Z1B)/(Z1A-Z1B)
            FFB(I)=(Z2S-Z1A)/(Z1B-Z1A)
            GGA(I)=1/(Z1A-Z1B)
            GGB(I)=1/(Z1B-Z1A)
          ELSEIF(K1.GT.1.AND.K1.LT.KM1-1) THEN
            Z2S=Z2(1+(I-1)*IXZ2+(K2-1)*KXZ2)
            Z1A=Z1(1+(I-1)*IXZ1+(K1-2)*KXZ1)
            Z1B=Z1(1+(I-1)*IXZ1+(K1-1)*KXZ1)
            Z1C=Z1(1+(I-1)*IXZ1+(K1+0)*KXZ1)
            Z1D=Z1(1+(I-1)*IXZ1+(K1+1)*KXZ1)
            FFA(I)=(Z2S-Z1B)/(Z1A-Z1B)* &
                   (Z2S-Z1C)/(Z1A-Z1C)* &
                   (Z2S-Z1D)/(Z1A-Z1D)
            FFB(I)=(Z2S-Z1A)/(Z1B-Z1A)* &
                   (Z2S-Z1C)/(Z1B-Z1C)* &
                   (Z2S-Z1D)/(Z1B-Z1D)
            FFC(I)=(Z2S-Z1A)/(Z1C-Z1A)* &
                   (Z2S-Z1B)/(Z1C-Z1B)* &
                   (Z2S-Z1D)/(Z1C-Z1D)
            FFD(I)=(Z2S-Z1A)/(Z1D-Z1A)* &
                   (Z2S-Z1B)/(Z1D-Z1B)* &
                   (Z2S-Z1C)/(Z1D-Z1C)
            GGA(I)=        1/(Z1A-Z1B)* &
                   (Z2S-Z1C)/(Z1A-Z1C)* &
                   (Z2S-Z1D)/(Z1A-Z1D)+ &
                   (Z2S-Z1B)/(Z1A-Z1B)* &
                           1/(Z1A-Z1C)* &
                   (Z2S-Z1D)/(Z1A-Z1D)+ &
                   (Z2S-Z1B)/(Z1A-Z1B)* &
                   (Z2S-Z1C)/(Z1A-Z1C)* &
                           1/(Z1A-Z1D)   
            GGB(I)=        1/(Z1B-Z1A)* &
                   (Z2S-Z1C)/(Z1B-Z1C)* &
                   (Z2S-Z1D)/(Z1B-Z1D)+ &
                   (Z2S-Z1A)/(Z1B-Z1A)* &
                           1/(Z1B-Z1C)* &
                   (Z2S-Z1D)/(Z1B-Z1D)+ &
                   (Z2S-Z1A)/(Z1B-Z1A)* &
                   (Z2S-Z1C)/(Z1B-Z1C)* &
                           1/(Z1B-Z1D)  
            GGC(I)=        1/(Z1C-Z1A)* &
                   (Z2S-Z1B)/(Z1C-Z1B)* &
                   (Z2S-Z1D)/(Z1C-Z1D)+ &
                   (Z2S-Z1A)/(Z1C-Z1A)* &
                           1/(Z1C-Z1B)* &
                   (Z2S-Z1D)/(Z1C-Z1D)+ &
                   (Z2S-Z1A)/(Z1C-Z1A)* &
                   (Z2S-Z1B)/(Z1C-Z1B)* &
                           1/(Z1C-Z1D)
            GGD(I)=        1/(Z1D-Z1A)* &    
                   (Z2S-Z1B)/(Z1D-Z1B)* &
                   (Z2S-Z1C)/(Z1D-Z1C)+ &
                   (Z2S-Z1A)/(Z1D-Z1A)* &
                           1/(Z1D-Z1B)* &
                   (Z2S-Z1C)/(Z1D-Z1C)+ &
                   (Z2S-Z1A)/(Z1D-Z1A)* &
                   (Z2S-Z1B)/(Z1D-Z1B)* &
                           1/(Z1D-Z1C)
          ENDIF
        ENDDO
!  INTERPOLATE.
        DO N=1,NM
          DO I=1,IM
            K1=K1S(I,K2)
            IF(K1.EQ.0) THEN
              Q2S=Q1(1+(I-1)*IXQ1+(N-1)*NXQ1)
              J2S=0
            ELSEIF(K1.EQ.KM1) THEN
              Q2S=Q1(1+(I-1)*IXQ1+(KM1-1)*KXQ1+(N-1)*NXQ1)
              J2S=0
            ELSEIF(K1.EQ.1.OR.K1.EQ.KM1-1) THEN
              Q1A=Q1(1+(I-1)*IXQ1+(K1-1)*KXQ1+(N-1)*NXQ1)
              Q1B=Q1(1+(I-1)*IXQ1+(K1+0)*KXQ1+(N-1)*NXQ1)
              Q2S=FFA(I)*Q1A+FFB(I)*Q1B
              J2S=GGA(I)*Q1A+GGB(I)*Q1B
            ELSE
              Q1A=Q1(1+(I-1)*IXQ1+(K1-2)*KXQ1+(N-1)*NXQ1)
              Q1B=Q1(1+(I-1)*IXQ1+(K1-1)*KXQ1+(N-1)*NXQ1)
              Q1C=Q1(1+(I-1)*IXQ1+(K1+0)*KXQ1+(N-1)*NXQ1)
              Q1D=Q1(1+(I-1)*IXQ1+(K1+1)*KXQ1+(N-1)*NXQ1)
              Q2S=FFA(I)*Q1A+FFB(I)*Q1B+FFC(I)*Q1C+FFD(I)*Q1D
              J2S=GGA(I)*Q1A+GGB(I)*Q1B+GGC(I)*Q1C+GGD(I)*Q1D
              IF(Q2S.LT.MIN(Q1B,Q1C)) THEN
                Q2S=MIN(Q1B,Q1C)
                J2S=0
              ELSEIF(Q2S.GT.MAX(Q1B,Q1C)) THEN
                Q2S=MAX(Q1B,Q1C)
                J2S=0
              ENDIF
            ENDIF
            Q2(1+(I-1)*IXQ2+(K2-1)*KXQ2+(N-1)*NXQ2)=Q2S
            J2(1+(I-1)*IXQ2+(K2-1)*KXQ2+(N-1)*NXQ2)=J2S
          ENDDO
        ENDDO
      ENDDO
!!$OMP END PARALLEL DO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
!-----------------------------------------------------------------------
      SUBROUTINE RSEARCH(IM,KM1,IXZ1,KXZ1,Z1,KM2,IXZ2,KXZ2,Z2,IXL2,KXL2,&
                         L2)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    RSEARCH     SEARCH FOR A SURROUNDING REAL INTERVAL
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 98-05-01
!
! ABSTRACT: THIS SUBPROGRAM SEARCHES MONOTONIC SEQUENCES OF REAL NUMBERS
!   FOR INTERVALS THAT SURROUND A GIVEN SEARCH SET OF REAL NUMBERS.
!   THE SEQUENCES MAY BE MONOTONIC IN EITHER DIRECTION; THE REAL NUMBERS
!   MAY BE SINGLE OR DOUBLE PRECISION; THE INPUT SEQUENCES AND SETS
!   AND THE OUTPUT LOCATIONS MAY BE ARBITRARILY DIMENSIONED.
!
! PROGRAM HISTORY LOG:
! 1999-01-05  MARK IREDELL
!
! USAGE:    CALL RSEARCH(IM,KM1,IXZ1,KXZ1,Z1,KM2,IXZ2,KXZ2,Z2,IXL2,KXL2,
!    &                   L2)
!   INPUT ARGUMENT LIST:
!     IM           INTEGER NUMBER OF SEQUENCES TO SEARCH
!     KM1          INTEGER NUMBER OF POINTS IN EACH SEQUENCE
!     IXZ1         INTEGER SEQUENCE SKIP NUMBER FOR Z1
!     KXZ1         INTEGER POINT SKIP NUMBER FOR Z1
!     Z1           REAL (1+(IM-1)*IXZ1+(KM1-1)*KXZ1)
!                  SEQUENCE VALUES TO SEARCH
!                  (Z1 MUST BE MONOTONIC IN EITHER DIRECTION)
!     KM2          INTEGER NUMBER OF POINTS TO SEARCH FOR
!                  IN EACH RESPECTIVE SEQUENCE
!     IXZ2         INTEGER SEQUENCE SKIP NUMBER FOR Z2
!     KXZ2         INTEGER POINT SKIP NUMBER FOR Z2
!     Z2           REAL (1+(IM-1)*IXZ2+(KM2-1)*KXZ2)
!                  SET OF VALUES TO SEARCH FOR
!                  (Z2 NEED NOT BE MONOTONIC)
!     IXL2         INTEGER SEQUENCE SKIP NUMBER FOR L2
!     KXL2         INTEGER POINT SKIP NUMBER FOR L2
!     
!   OUTPUT ARGUMENT LIST:
!     L2           INTEGER (1+(IM-1)*IXL2+(KM2-1)*KXL2)
!                  INTERVAL LOCATIONS HAVING VALUES FROM 0 TO KM1
!                  (Z2 WILL BE BETWEEN Z1(L2) AND Z1(L2+1))
!
! SUBPROGRAMS CALLED:
!   SBSRCH       ESSL BINARY SEARCH
!   DBSRCH       ESSL BINARY SEARCH
!
! REMARKS:
!   IF THE ARRAY Z1 IS DIMENSIONED (IM,KM1), THEN THE SKIP NUMBERS ARE
!   IXZ1=1 AND KXZ1=IM; IF IT IS DIMENSIONED (KM1,IM), THEN THE SKIP
!   NUMBERS ARE IXZ1=KM1 AND KXZ1=1; IF IT IS DIMENSIONED (IM,JM,KM1),
!   THEN THE SKIP NUMBERS ARE IXZ1=1 AND KXZ1=IM*JM; ETCETERA.
!   SIMILAR EXAMPLES APPLY TO THE SKIP NUMBERS FOR Z2 AND L2.
!
!   RETURNED VALUES OF 0 OR KM1 INDICATE THAT THE GIVEN SEARCH VALUE
!   IS OUTSIDE THE RANGE OF THE SEQUENCE.
!
!   IF A SEARCH VALUE IS IDENTICAL TO ONE OF THE SEQUENCE VALUES
!   THEN THE LOCATION RETURNED POINTS TO THE IDENTICAL VALUE.
!   IF THE SEQUENCE IS NOT STRICTLY MONOTONIC AND A SEARCH VALUE IS
!   IDENTICAL TO MORE THAN ONE OF THE SEQUENCE VALUES, THEN THE
!   LOCATION RETURNED MAY POINT TO ANY OF THE IDENTICAL VALUES.
!
!   TO BE EXACT, FOR EACH I FROM 1 TO IM AND FOR EACH K FROM 1 TO KM2,
!   Z=Z2(1+(I-1)*IXZ2+(K-1)*KXZ2) IS THE SEARCH VALUE AND
!   L=L2(1+(I-1)*IXL2+(K-1)*KXL2) IS THE LOCATION RETURNED.
!   IF L=0, THEN Z IS LESS THAN THE START POINT Z1(1+(I-1)*IXZ1)
!   FOR ASCENDING SEQUENCES (OR GREATER THAN FOR DESCENDING SEQUENCES).
!   IF L=KM1, THEN Z IS GREATER THAN OR EQUAL TO THE END POINT
!   Z1(1+(I-1)*IXZ1+(KM1-1)*KXZ1) FOR ASCENDING SEQUENCES
!   (OR LESS THAN OR EQUAL TO FOR DESCENDING SEQUENCES).
!   OTHERWISE Z IS BETWEEN THE VALUES Z1(1+(I-1)*IXZ1+(L-1)*KXZ1) AND
!   Z1(1+(I-1)*IXZ1+(L-0)*KXZ1) AND MAY EQUAL THE FORMER.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN
!
!C$$$
!     IMPLICIT NONE
!     INTEGER,INTENT(IN):: IM,KM1,IXZ1,KXZ1,KM2,IXZ2,KXZ2,IXL2,KXL2
!     REAL,INTENT(IN):: Z1(1+(IM-1)*IXZ1+(KM1-1)*KXZ1)
!     REAL,INTENT(IN):: Z2(1+(IM-1)*IXZ2+(KM2-1)*KXZ2)
!     INTEGER,INTENT(OUT):: L2(1+(IM-1)*IXL2+(KM2-1)*KXL2)
!     INTEGER(4) INCX,N,INCY,M,INDX(KM2),RC(KM2),IOPT
!     INTEGER I,K2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  FIND THE SURROUNDING INPUT INTERVAL FOR EACH OUTPUT POINT.
!     DO I=1,IM
!       IF(Z1(1+(I-1)*IXZ1).LE.Z1(1+(I-1)*IXZ1+(KM1-1)*KXZ1)) THEN
!  INPUT COORDINATE IS MONOTONICALLY ASCENDING.
!         INCX=KXZ2
!         N=KM2
!         INCY=KXZ1
!         M=KM1
!         IOPT=1
!         IF(DIGITS(1.).LT.DIGITS(1._8)) THEN
!           CALL SBSRCH(Z2(1+(I-1)*IXZ2),INCX,N,
!    &                  Z1(1+(I-1)*IXZ1),INCY,M,INDX,RC,IOPT)
!         ELSE
!           CALL DBSRCH(Z2(1+(I-1)*IXZ2),INCX,N,
!    &                  Z1(1+(I-1)*IXZ1),INCY,M,INDX,RC,IOPT)
!         ENDIF
!         DO K2=1,KM2
!           L2(1+(I-1)*IXL2+(K2-1)*KXL2)=INDX(K2)-RC(K2)
!         ENDDO
!       ELSE
!  INPUT COORDINATE IS MONOTONICALLY DESCENDING.
!         INCX=KXZ2
!         N=KM2
!         INCY=-KXZ1
!         M=KM1
!         IOPT=0
!         IF(DIGITS(1.).LT.DIGITS(1._8)) THEN
!           CALL SBSRCH(Z2(1+(I-1)*IXZ2),INCX,N,
!    &                  Z1(1+(I-1)*IXZ1),INCY,M,INDX,RC,IOPT)
!         ELSE
!           CALL DBSRCH(Z2(1+(I-1)*IXZ2),INCX,N,
!    &                  Z1(1+(I-1)*IXZ1),INCY,M,INDX,RC,IOPT)
!         ENDIF
!         DO K2=1,KM2
!           L2(1+(I-1)*IXL2+(K2-1)*KXL2)=KM1+1-INDX(K2)
!         ENDDO
!       ENDIF
!     ENDDO
!
      IMPLICIT NONE
      INTEGER,INTENT(IN):: IM,KM1,IXZ1,KXZ1,KM2,IXZ2,KXZ2,IXL2,KXL2
      REAL,INTENT(IN):: Z1(1+(IM-1)*IXZ1+(KM1-1)*KXZ1)
      REAL,INTENT(IN):: Z2(1+(IM-1)*IXZ2+(KM2-1)*KXZ2)
      INTEGER,INTENT(OUT):: L2(1+(IM-1)*IXL2+(KM2-1)*KXL2)
      INTEGER I,K2,L
      REAL Z
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!C  FIND THE SURROUNDING INPUT INTERVAL FOR EACH OUTPUT POINT.
      DO I=1,IM
        IF(Z1(1+(I-1)*IXZ1).LE.Z1(1+(I-1)*IXZ1+(KM1-1)*KXZ1)) THEN
!C  INPUT COORDINATE IS MONOTONICALLY ASCENDING.
          DO K2=1,KM2
            Z=Z2(1+(I-1)*IXZ2+(K2-1)*KXZ2)
            L=0
            DO
              IF(Z.LT.Z1(1+(I-1)*IXZ1+L*KXZ1)) EXIT
              L=L+1
              IF(L.EQ.KM1) EXIT
            ENDDO
            L2(1+(I-1)*IXL2+(K2-1)*KXL2)=L
          ENDDO
        ELSE
!C  INPUT COORDINATE IS MONOTONICALLY DESCENDING.
          DO K2=1,KM2
            Z=Z2(1+(I-1)*IXZ2+(K2-1)*KXZ2)
            L=0
            DO
              IF(Z.GT.Z1(1+(I-1)*IXZ1+L*KXZ1)) EXIT
              L=L+1
              IF(L.EQ.KM1) EXIT
            ENDDO
            L2(1+(I-1)*IXL2+(K2-1)*KXL2)=L
          ENDDO
        ENDIF
      ENDDO

      END SUBROUTINE
