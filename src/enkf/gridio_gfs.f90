 module gridio
!$$$  module documentation block
!
! module: gridio                     subroutines for reading and writing
!                                    ensemble members files using
!                                    EnKF internal format.  A separate
!                                    program must be run before and
!                                    after the EnKF analysis to convert
!                                    to and from the native model format.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: I/O for ensemble member files.
!
! Public Functions:
!  readgriddata, writegriddata
!
! this version reads and writes NCEP GFS sigma files.
!
! Public Variables: None
!
! Modules Used: constants (must be pre-initialized).
!
! program history log:
!   2009-02-23  Initial version.
!   2015-06-29  Add ability to read/write multiple time levels
!   2016-05-02  Modification for reading state vector from table
!               (Anna Shlyaeva)
!
! attributes:
!   language: f95
!
!$$$
 use constants, only: zero,one,cp,fv,rd,grav,zero,max_varname_length
 use params, only: nlons,nlats,reducedgrid,nlevs,use_gfs_nemsio,pseudo_rh, &
                   cliptracers,nlons,nlats,datestring,datapath,&
                   nbackgrounds,fgfileprefixes,anlfileprefixes
 use kinds, only: i_kind,r_double,r_kind,r_single
 use gridinfo, only: ntrunc,npts,ptop  ! gridinfo must be called first!
 use specmod, only: sptezv_s, sptez_s, init_spec_vars, ndimspec => nc, &
                    isinitialized
 use reducedgrid_mod, only: regtoreduced, reducedtoreg
 use mpisetup, only: nproc
 use mpeu_util, only: getindex
 implicit none
 private
 public :: readgriddata, writegriddata
 contains

 subroutine readgriddata(nanal,svars3d,svars2d,ns3d,ns2d,grdin,qsat)
  use sigio_module, only: sigio_head, sigio_data, sigio_sclose, sigio_sropen, &
                          sigio_srohdc, sigio_sclose, sigio_aldata, sigio_axdata
  use nemsio_module, only: nemsio_gfile,nemsio_open,nemsio_close,&
                           nemsio_getheadvar,nemsio_realkind,&
                           nemsio_readrecv,nemsio_init,nemsio_setheadvar,nemsio_writerecv
  implicit none

  character(len=500) :: filename
  character(len=3) charnanal
  integer, intent(in) :: nanal
  integer, intent(in) :: ns2d,ns3d
  character(len=max_varname_length), dimension(ns2d), intent(in) :: svars2d
  character(len=max_varname_length), dimension(ns3d), intent(in) :: svars3d
  real(r_double), dimension(npts,nlevs,nbackgrounds), intent(out) :: qsat
  real(r_single), dimension(npts,ns3d*nlevs+ns2d,nbackgrounds), intent(out) :: grdin

  real(r_kind) kap,kapr,kap1,clip

  real(r_kind), allocatable, dimension(:,:) :: vmassdiv
  real(r_single), allocatable, dimension(:,:) :: pressi,pslg
  real(r_kind), dimension(nlons*nlats) :: ug,vg
  real(r_kind), dimension(ndimspec) :: vrtspec,divspec
  real(r_kind), allocatable, dimension(:) :: psg,pstend,ak,bk
  real(nemsio_realkind), dimension(nlons*nlats) :: nems_wrk,nems_wrk2
  type(sigio_head) sighead
  type(sigio_data) sigdata
  type(nemsio_gfile) :: gfile

  integer :: u_ind, v_ind, t_ind, q_ind, oz_ind, cw_ind
  integer :: ps_ind, pst_ind


  integer(i_kind) k,nt,iunitsig,iret,nb
  logical ice

  backgroundloop: do nb=1,nbackgrounds

  write(charnanal,'(i3.3)') nanal
  iunitsig = 77
  filename = trim(adjustl(datapath))//trim(adjustl(fgfileprefixes(nb)))//"mem"//charnanal
  if (use_gfs_nemsio) then
     call nemsio_init(iret=iret)
     if(iret/=0) then
        write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_init, iret=',iret
        call stop2(23)
     end if
     call nemsio_open(gfile,filename,'READ',iret=iret)
     if (iret/=0) then
        write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_open, iret=',iret
        call stop2(23)
     endif
  else
     call sigio_srohdc(iunitsig,trim(filename), &
                       sighead,sigdata,iret)
     if (iret /= 0) then
        print *,'error reading file in gridio ',trim(filename)
        call stop2(23)
     end if
  endif
  ice = .false. ! calculate qsat w/resp to ice?
  kap = rd/cp
  kapr = cp/rd
  kap1 = kap+one

  u_ind   = getindex(svars3d, 'u')   !< indices in the state var arrays
  v_ind   = getindex(svars3d, 'v')   ! U and V (3D)
  t_ind   = getindex(svars3d, 'tv')  ! Tv (3D)
  q_ind   = getindex(svars3d, 'q')   ! Q (3D)
  oz_ind  = getindex(svars3d, 'oz')  ! Oz (3D)
  cw_ind  = getindex(svars3d, 'cw')  ! CW (3D)

  ps_ind  = getindex(svars2d, 'ps')  ! Ps (2D)
  pst_ind = getindex(svars2d, 'pst') ! Ps tendency (2D)   // equivalent of
                                     ! old logical massbal_adjust, if non-zero

  if (nproc == 0) then
    print *, 'indices: '
    print *, 'u: ', u_ind, ', v: ', v_ind, ', t: ', t_ind
    print *, 'q: ', q_ind, ', oz: ', oz_ind, ', cw: ', cw_ind
    print *, 'ps: ', ps_ind, ', pst: ', pst_ind
  endif

  if (.not. isinitialized) call init_spec_vars(nlons,nlats,ntrunc,4)

  allocate(pressi(nlons*nlats,nlevs+1))
  allocate(pslg(npts,nlevs))
  allocate(psg(nlons*nlats),pstend(nlons*nlats))
  if (pst_ind > 0) allocate(vmassdiv(nlons*nlats,nlevs))

  if (use_gfs_nemsio) then
     call nemsio_readrecv(gfile,'pres','sfc',1,nems_wrk,iret=iret)
     if (iret/=0) then
         write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(ps), iret=',iret
         call stop2(23)
     endif
     psg = 0.01_r_kind*nems_wrk ! convert ps to millibars.
     !print *,'min/max ps',minval(psg),maxval(psg),' nanal=',nanal
     ! pressure at interfaces
     pressi(:,1) = psg
     do k=1,nlevs
        call nemsio_readrecv(gfile,'dpres','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
            write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(dpres), iret=',iret
            call stop2(23)
        endif
        pressi(:,k+1) = pressi(:,k) - 0.01_r_kind*nems_wrk
        !if (nanal .eq. 1) print *,'min/max pressi',k,minval(pressi(:,k+1)),maxval(pressi(:,k+1))
     enddo
     pressi(:,nlevs+1) = ptop
  else
     vrtspec = sigdata%ps
     call sptez_s(vrtspec,psg,1)
     !==> input psg is ln(ps) in centibars - convert to ps in millibars.
     psg = 10._r_kind*exp(psg)
     allocate(ak(nlevs+1),bk(nlevs+1))
     if (sighead%idvc .eq. 0) then ! sigma coordinate, old file format.
         ak = zero
         bk = sighead%si(1:nlevs+1)
     else if (sighead%idvc == 1) then ! sigma coordinate
         ak = zero
         bk = sighead%vcoord(1:nlevs+1,2)
     else if (sighead%idvc == 2 .or. sighead%idvc == 3) then ! hybrid coordinate
         bk = sighead%vcoord(1:nlevs+1,2) 
         ak = 0.01_r_kind*sighead%vcoord(1:nlevs+1,1)  ! convert to mb
     else
         print *,'unknown vertical coordinate type',sighead%idvc
         call stop2(23)
     end if
     !==> pressure at interfaces.
     if (nanal .eq. 1) then
        print *,'time level ',nb
        print *,'---------------'
     endif
     do k=1,nlevs+1
        pressi(:,k)=ak(k)+bk(k)*psg
        if (nanal .eq. 1) print *,'min/max pressi',k,minval(pressi(:,k)),maxval(pressi(:,k))
     enddo
     deallocate(ak,bk)
  endif

  !==> get U,V,temp,q,ps on gaussian grid.
  ! u is first nlevs, v is second, t is third, then tracers.
  if (use_gfs_nemsio) then
     clip = tiny(vg(1))
     do k=1,nlevs
        call nemsio_readrecv(gfile,'ugrd','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
            write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(ugrd), iret=',iret
            call stop2(23)
        endif
        ug = nems_wrk
        call nemsio_readrecv(gfile,'vgrd','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
            write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(vgrd), iret=',iret
            call stop2(23)
        endif
        vg = nems_wrk
        if (u_ind > 0)       call copytogrdin(ug,grdin(:,nlevs*(u_ind-1) + k,nb))
        if (v_ind > 0)       call copytogrdin(vg,grdin(:,nlevs*(v_ind-1) + k,nb))
        ! calculate vertical integral of mass flux div (ps tendency)
        ! this variable is analyzed in order to enforce mass balance in the analysis
        if (pst_ind > 0) then
           ug = ug*(pressi(:,k)-pressi(:,k+1))
           vg = vg*(pressi(:,k)-pressi(:,k+1))
           call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
           call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
        endif
        call nemsio_readrecv(gfile,'tmp','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(tmp), iret=',iret
           call stop2(23)
        endif
        call nemsio_readrecv(gfile,'spfh','mid layer',k,nems_wrk2,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(spfh), iret=',iret
           call stop2(23)
        endif
        if (cliptracers)  where (nems_wrk2 < clip) nems_wrk2 = clip
        nems_wrk = nems_wrk * ( 1.0 + fv*nems_wrk2 ) ! convert T to Tv
        ug = nems_wrk
        vg = nems_wrk2
        if (t_ind > 0)       call copytogrdin(ug,grdin(:,nlevs*(t_ind-1)+k,nb))
        if (q_ind > 0)       call copytogrdin(vg,grdin(:,nlevs*(q_ind-1)+k,nb))
        if (oz_ind > 0) then
           call nemsio_readrecv(gfile,'o3mr','mid layer',k,nems_wrk2,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(o3mr), iret=',iret
              call stop2(23)
           endif
           if (cliptracers)  where (nems_wrk2 < clip) nems_wrk2 = clip
           ug = nems_wrk2
           call copytogrdin(ug,grdin(:,nlevs*(oz_ind-1)+k,nb))
        endif
        if (cw_ind > 0) then
           call nemsio_readrecv(gfile,'clwmr','mid layer',k,nems_wrk2,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(clwmr), iret=',iret
              call stop2(23)
           endif
           if (cliptracers)  where (nems_wrk2 < clip) nems_wrk2 = clip
           ug = nems_wrk2
           call copytogrdin(ug,grdin(:,nlevs*(cw_ind-1)+k,nb))
        endif
     enddo
  else
!$omp parallel do private(k,nt,ug,vg,divspec,vrtspec)  shared(sigdata,pressi,vmassdiv,grdin)
     do k=1,nlevs
   
        vrtspec = sigdata%z(:,k); divspec = sigdata%d(:,k)
        call sptezv_s(divspec,vrtspec,ug,vg,1)
        if (u_ind > 0) then
           call copytogrdin(ug,grdin(:,nlevs*(u_ind-1)+k,nb))
        endif
        if (v_ind > 0) then
           call copytogrdin(vg,grdin(:,nlevs*(v_ind-1)+k,nb))
        endif

! calculate vertical integral of mass flux div (ps tendency)
! this variable is analyzed in order to enforce mass balance in the analysis
        if (pst_ind > 0) then
           ug = ug*(pressi(:,k)-pressi(:,k+1))
           vg = vg*(pressi(:,k)-pressi(:,k+1))
           call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
           call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
        endif

        if (t_ind > 0) then
          divspec = sigdata%t(:,k)
          call sptez_s(divspec,ug,1)
          call copytogrdin(ug,grdin(:,nlevs*(t_ind-1)+k,nb))
        endif

        if (q_ind > 0) then
           divspec = sigdata%q(:,k,1)
           call sptez_s(divspec,ug,1)
           call copytogrdin(ug,grdin(:,nlevs*(q_ind-1)+k,nb))
        endif

        if (oz_ind > 0) then
           divspec = sigdata%q(:,k,2)
           call sptez_s(divspec,ug,1)
           call copytogrdin(ug,grdin(:,nlevs*(oz_ind-1)+k,nb))
        endif

        if (cw_ind > 0) then
           divspec = sigdata%q(:,k,3)
           call sptez_s(divspec,ug,1)
           call copytogrdin(ug,grdin(:,nlevs*(cw_ind-1)+k,nb))
        endif

     enddo
!$omp end parallel do
  endif

  ! surface pressure
  if (ps_ind > 0) then
    call copytogrdin(psg,grdin(:,nlevs*ns3d + ps_ind,nb))
  endif
  if (.not. use_gfs_nemsio) call sigio_axdata(sigdata,iret)

  ! surface pressure tendency
  if (pst_ind > 0) then
     pstend = sum(vmassdiv,2)
     if (nanal .eq. 1) &
     print *,nanal,'min/max first-guess ps tend',minval(pstend),maxval(pstend)
     call copytogrdin(pstend,grdin(:,nlevs*ns3d + pst_ind,nb))
  endif

  ! compute saturation q.
  do k=1,nlevs
    ! layer pressure from phillips vertical interolation
    ug(:) = ((pressi(:,k)**kap1-pressi(:,k+1)**kap1)/&
            (kap1*(pressi(:,k)-pressi(:,k+1))))**kapr
    call copytogrdin(ug,pslg(:,k))
  end do
  if (pseudo_rh) then
     call genqsat1(grdin(:,(q_ind-1)*nlevs+1:q_ind*nlevs,nb),qsat(:,:,nb),pslg,grdin(:,(t_ind-1)*nlevs+1:t_ind*nlevs,nb),ice,npts,nlevs)
  else
     qsat(:,:,nb) = 1._r_double
  end if
  
  deallocate(pressi,pslg)
  deallocate(psg,pstend)
  if (pst_ind > 0) deallocate(vmassdiv)
  if (use_gfs_nemsio) call nemsio_close(gfile,iret=iret)

  end do backgroundloop ! loop over backgrounds to read in

 end subroutine readgriddata

 subroutine writegriddata(nanal,svars3d,svars2d,ns3d,ns2d,grdin)
  use sigio_module, only: sigio_head, sigio_data, sigio_sclose, sigio_sropen, &
                          sigio_srohdc, sigio_sclose, sigio_axdata, &
                          sigio_aldata, sigio_swohdc
  use nemsio_module, only: nemsio_gfile,nemsio_open,nemsio_close,&
                           nemsio_readrec,nemsio_writerec,nemsio_intkind,&
                           nemsio_getheadvar,nemsio_realkind,nemsio_getfilehead,&
                           nemsio_readrecv,nemsio_init,nemsio_setheadvar,nemsio_writerecv
  implicit none

  character(len=500):: filenamein, filenameout
  integer, intent(in) :: nanal
  integer, intent(in) :: ns2d,ns3d
  character(len=max_varname_length), dimension(ns2d), intent(in) :: svars2d
  character(len=max_varname_length), dimension(ns3d), intent(in) :: svars3d
  real(r_single), dimension(npts,ns3d*nlevs+ns2d,nbackgrounds), intent(inout) :: grdin
  real(r_kind), allocatable, dimension(:,:) :: vmassdiv,dpanl,dpfg,pressi
  real(r_kind), allocatable, dimension(:,:) :: vmassdivinc
  real(r_kind), allocatable, dimension(:,:) :: ugtmp,vgtmp
  real(r_kind), allocatable,dimension(:) :: psg,pstend1,pstend2,pstendfg,vmass
  real(r_kind), dimension(nlons*nlats) :: ug,vg,uginc,vginc,psfg
  real(r_kind), dimension(ndimspec) :: vrtspec,divspec
  integer iadate(4),idate(4),nfhour,idat(7),iret,nrecs,jdate(7)
  integer:: nfminute, nfsecondn, nfsecondd
  integer,dimension(8):: ida,jda
  real(r_double),dimension(5):: fha
  real(r_kind) fhour
  type(sigio_head) sighead
  type(sigio_data) sigdata_inc
  character(len=3) charnanal

  real(r_kind) kap,kapr,kap1,clip
  real(nemsio_realkind), dimension(nlons*nlats) :: nems_wrk,nems_wrk2
  real(r_kind), dimension(nlevs+1) :: ak,bk
  real(nemsio_realkind), dimension(nlevs+1,3,2) :: nems_vcoord
  integer(nemsio_intkind) :: nems_idvc
  type(sigio_data) sigdata
  type(nemsio_gfile) :: gfilein,gfileout

  integer :: u_ind, v_ind, t_ind, q_ind, oz_ind, cw_ind
  integer :: ps_ind, pst_ind

  integer k,nt,ierr,iunitsig,nb

  iunitsig = 78
  kapr = cp/rd
  kap = rd/cp
  kap1 = kap+one
  write(charnanal,'(i3.3)') nanal

  backgroundloop: do nb=1,nbackgrounds

  filenameout = trim(adjustl(datapath))//trim(adjustl(anlfileprefixes(nb)))//"mem"//charnanal
  filenamein = trim(adjustl(datapath))//trim(adjustl(fgfileprefixes(nb)))//"mem"//charnanal
  ! for nemsio, analysis file must be copied from first guess at scripting
  ! level.  This file is read in and modified.

  if (use_gfs_nemsio) then
     call nemsio_init(iret=iret)
     if(iret/=0) then
        write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_init, iret=',iret
        call stop2(23)
     end if
     call nemsio_open(gfilein,filenamein,'READ',iret=iret)
     if (iret/=0) then
        write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_open, iret=',iret
        call stop2(23)
     endif
     call nemsio_getfilehead(gfilein,iret=iret,idate=idat,nfhour=nfhour,&
                             nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd,&
                             nrec=nrecs,&
                             vcoord=nems_vcoord,idvc=nems_idvc)
     write(6,111) trim(filenamein),idat,nfhour,nfminute,nfsecondn,nfsecondd
111  format(a32,1x,'idat=',7(i4,1x),' nfh=',i5,' nfm=',i5,' nfsn=',i5,' nfsd=',i5)

     if (iret/=0) then
        write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_getfilehead, iret=',iret
        call stop2(23)
     endif
     if (nems_idvc == 1) then ! sigma coordinate
         ak = zero
         bk = nems_vcoord(1:nlevs+1,2,1)
     else if (nems_idvc == 2 .or. nems_idvc == 3) then ! hybrid coordinate
         bk = nems_vcoord(1:nlevs+1,2,1) 
         ak = 0.01_r_kind*nems_vcoord(1:nlevs+1,1,1)  ! convert to mb
     else
         print *,'unknown vertical coordinate type',nems_idvc
         call stop2(23)
     end if
  else
     ! read in first-guess data.
     call sigio_srohdc(iunitsig,trim(filenamein), &
                       sighead,sigdata,ierr)
  endif

  u_ind   = getindex(svars3d, 'u')   !< indices in the state var arrays
  v_ind   = getindex(svars3d, 'v')   ! U and V (3D)
  t_ind   = getindex(svars3d, 'tv')  ! Tv (3D)
  q_ind   = getindex(svars3d, 'q')   ! Q (3D)
  oz_ind  = getindex(svars3d, 'oz')  ! Oz (3D)
  cw_ind  = getindex(svars3d, 'cw')  ! CW (3D)

  ps_ind  = getindex(svars2d, 'ps')  ! Ps (2D)
  pst_ind = getindex(svars2d, 'pst') ! Ps tendency (2D)   // equivalent of
                                     ! old logical massbal_adjust, if non-zero

  if (nproc == 0) then
    print *, 'indices: '
    print *, 'u: ', u_ind, ', v: ', v_ind, ', t: ', t_ind
    print *, 'q: ', q_ind, ', oz: ', oz_ind, ', cw: ', cw_ind
    print *, 'ps: ', ps_ind, ', pst: ', pst_ind
  endif

  if (pst_ind > 0) then
     allocate(vmassdiv(nlons*nlats,nlevs))
     allocate(vmassdivinc(nlons*nlats,nlevs))
  endif
  allocate(psg(nlons*nlats),pstend1(nlons*nlats))
  allocate(pstend2(nlons*nlats),vmass(nlons*nlats))
  allocate(dpfg(nlons*nlats,nlevs))
  allocate(dpanl(nlons*nlats,nlevs))
  allocate(pressi(nlons*nlats,nlevs+1))
  allocate(pstendfg(nlons*nlats))

! Compute analysis time from guess date and forecast length.
  if (.not. use_gfs_nemsio) then
     idate = sighead%idate
     fhour = sighead%fhour
  else
     idate(3)=idat(3) 
     idate(2)=idat(2)
     idate(4)=idat(1) 
     idate(1)=idat(4)
     fhour = nfhour
  endif
  fha=zero; ida=0; jda=0
  fha(2)=fhour    ! relative time interval in hours
  ida(1)=idate(4) ! year
  ida(2)=idate(2) ! month
  ida(3)=idate(3) ! day
  ida(4)=0                ! time zone
  ida(5)=idate(1) ! hour
  call w3movdat(fha,ida,jda)
!
!   INPUT VARIABLES:
!     RINC       REAL (5) NCEP RELATIVE TIME INTERVAL
!                (DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS)
!     IDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!
!   OUTPUT VARIABLES:
!     JDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!                (JDAT IS LATER THAN IDAT IF TIME INTERVAL IS POSITIVE.)
  iadate(1)=jda(5) ! hour
  iadate(2)=jda(2) ! mon
  iadate(3)=jda(3) ! day
  iadate(4)=jda(1) ! year
  if (nproc .eq. 0) then
     print *,'idate = ',idate
     print *,'iadate = ',iadate
  end if

  if (.not. use_gfs_nemsio) then
     sighead%idate = iadate
     sighead%fhour = zero
     ! ensemble info
     ! http://www.emc.ncep.noaa.gov/gmb/ens/info/ens_grib.html#gribex
     sighead%iens(1) = 3 ! pos pert
     sighead%iens(2) = nanal ! ensemble member number
     sighead%icen2 = 2 ! sub-center, must be 2 or ens info not used
     if (.not. isinitialized) call init_spec_vars(nlons,nlats,sighead%jcap,4)
     ! allocate new sigdata structure for increments.
     call sigio_aldata(sighead,sigdata_inc,ierr) 
     ! convert to increment to spectral coefficients.
!$omp parallel do private(k,nt,ug,vg,divspec,vrtspec)  shared(grdin,sigdata_inc)
     do k=1,nlevs
        ug = 0.
        if (u_ind > 0 ) then
          call copyfromgrdin(grdin(:,nlevs*(u_ind-1) + k,nb),ug)
        endif
        vg = 0.
        if (v_ind > 0) then
          call copyfromgrdin(grdin(:,nlevs*(v_ind-1) + k,nb),vg)
        endif
        call sptezv_s(divspec,vrtspec,ug,vg,-1)
        sigdata_inc%d(:,k) = divspec
        sigdata_inc%z(:,k) = vrtspec

        ug = 0.
        if (t_ind > 0) then
          call copyfromgrdin(grdin(:,nlevs*(t_ind-1)+k,nb),ug)
        endif
        call sptez_s(divspec,ug,-1)
        sigdata_inc%t(:,k) = divspec

        ug = 0.
        if (q_ind > 0) then
          call copyfromgrdin(grdin(:,nlevs*(q_ind-1)+k,nb),ug)
        endif
        call sptez_s(divspec,ug,-1)
        sigdata_inc%q(:,k,1) = divspec

        ug = 0.
        if (oz_ind > 0) then
          call copyfromgrdin(grdin(:,nlevs*(oz_ind-1)+k,nb),ug)
        endif
        call sptez_s(divspec,ug,-1)
        sigdata_inc%q(:,k,2) = divspec

        ug = 0.
        if (cw_ind > 0) then
          call copyfromgrdin(grdin(:,nlevs*(cw_ind-1)+k,nb),ug)
        endif
        call sptez_s(divspec,ug,-1)
        sigdata_inc%q(:,k,3) = divspec
 
     enddo
!$omp end parallel do

     divspec = sigdata%ps
     call sptez_s(divspec,vg,1)
     ! increment (in hPa) to reg grid.
     ug = 0.
     if (ps_ind > 0) then
       call copyfromgrdin(grdin(:,nlevs*ns3d + ps_ind,nb),ug)
     endif
     psfg = 10._r_kind*exp(vg)
     vg = psfg + ug ! first guess + increment
     psg = vg
     vg = log(vg/10._r_kind) ! convert back to centibars.
     call sptez_s(divspec,vg,-1)
     sigdata%ps = divspec

     if (sighead%idvc .eq. 0) then ! sigma coordinate, old file format.
         ak = zero
         bk = sighead%si(1:nlevs+1)
     else if (sighead%idvc == 1) then ! sigma coordinate
         ak = zero
         bk = sighead%vcoord(1:nlevs+1,2)
     else if (sighead%idvc == 2 .or. sighead%idvc == 3) then ! hybrid coordinate
         bk = sighead%vcoord(1:nlevs+1,2) 
         ak = 0.01_r_kind*sighead%vcoord(1:nlevs+1,1)  ! convert to mb
     else
         print *,'unknown vertical coordinate type',sighead%idvc
         call stop2(23)
     end if
     !==> first guess pressure at interfaces.
     do k=1,nlevs+1
        pressi(:,k)=ak(k)+bk(k)*psfg
     enddo
     do k=1,nlevs
        dpfg(:,k) = pressi(:,k)-pressi(:,k+1)
     enddo
     !==> analysis pressure at interfaces.
     do k=1,nlevs+1
        pressi(:,k)=ak(k)+bk(k)*psg
     enddo
     do k=1,nlevs
        dpanl(:,k) = pressi(:,k)-pressi(:,k+1)
        !if (nanal .eq. 1) print *,'k,dpanl,dpfg',minval(dpanl(:,k)),&
        !maxval(dpanl(:,k)),minval(dpfg(:,k)),maxval(dpfg(:,k))
     enddo

  else
     gfileout = gfilein

     nfhour    = 0        !  new forecast hour, zero at analysis time
     nfminute  = 0
     nfsecondn = 0
     nfsecondd = 100      ! default for denominator

     !iadate = hh/mm/dd/yyyy
     !jdate = yyyy/mm/dd/hh/min/secn/secd

     jdate(1) = iadate(4)  ! analysis year
     jdate(2) = iadate(2)  ! analysis month
     jdate(3) = iadate(3)  ! analysis day
     jdate(4) = iadate(1)  ! analysis hour
     jdate(5) = nfminute   ! analysis minute
     jdate(6) = nfsecondn  ! analysis scaled seconds
     jdate(7) = nfsecondd  ! analysis seconds multiplier

     call nemsio_open(gfileout,filenameout,'WRITE',iret=iret,&
          idate=jdate, nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, &
          nfsecondd=nfsecondd)

     write(6,112) trim(filenameout),jdate,nfhour,nfminute,nfsecondn,nfsecondd
112 format(a32,1x,'jdate=',7(i4,1x),' nfh=',i5,' nfm=',i5,' nfsn=',i5,' nfsd=',i5)

     if (iret/=0) then
        write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_open for output, iret=',iret
        call stop2(23)
     end if

!    read/write orographay
     call nemsio_readrecv(gfilein,'hgt','sfc',1,nems_wrk,iret=iret)
     call nemsio_writerecv(gfileout,'hgt','sfc',1,nems_wrk,iret=iret)

     call nemsio_readrecv(gfilein,'pres','sfc',1,nems_wrk,iret=iret)
     psfg = 0.01*nems_wrk ! convert ps to millibars.
     ! increment (in hPa) to reg grid.
     ug = 0.
     if (ps_ind > 0) then
       call copyfromgrdin(grdin(:,nlevs*ns3d + ps_ind,nb),ug)
     endif
     !print *,'nanal,min/max psfg,min/max inc',nanal,minval(psfg),maxval(psfg),minval(ug),maxval(ug)
     psg = psfg + ug ! first guess + increment
     nems_wrk = 100.*psg
     call nemsio_writerecv(gfileout,'pres','sfc',1,nems_wrk,iret=iret)
     if (iret/=0) then
        write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(pres), iret=',iret
        call stop2(23)
     endif
     ! analysis pressure at interfaces.
     pressi(:,1) = psg
     do k=1,nlevs
        call nemsio_readrecv(gfilein,'dpres','mid layer',k,nems_wrk,iret=iret)
        dpfg(:,k) = 0.01_r_kind*nems_wrk
        pressi(:,k+1) = ak(k+1) + bk(k+1)*psg
        dpanl(:,k) = pressi(:,k) - pressi(:,k+1)
        nems_wrk2 = 100.*dpanl(:,k)
        call nemsio_writerecv(gfileout,'dpres','mid layer',k,nems_wrk2,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(pres), iret=',iret
           call stop2(23)
        endif
        !if (nanal .eq. 1) print *,'k,dpanl,dpfg',minval(dpanl(:,k)),&
        !maxval(dpanl(:,k)),minval(dpfg(:,k)),maxval(dpfg(:,k))
     enddo
     pressi(:,nlevs+1) = ptop
     ! model layer pressures.
     do k=1,nlevs
        ! layer pressure from Phillips vertical interpolation.
        nems_wrk    = ((pressi(:,k)**kap1-pressi(:,k+1)**kap1)/&
                      (kap1*(pressi(:,k)-pressi(:,k+1))))**kapr
        nems_wrk = 100.*nems_wrk ! hPa to Pa
        call nemsio_writerecv(gfileout,'pres','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(model layer pres), iret=',iret
           call stop2(23)
        endif
     enddo

  endif

  if (pst_ind > 0) then

!$omp parallel do private(k,nt,ug,vg,vrtspec,divspec) shared(sigdata,dpfg,vmassdiv)
     do k=1,nlevs
!       re-calculate vertical integral of mass flux div for first-guess
        if (use_gfs_nemsio) then
           call nemsio_readrecv(gfilein,'ugrd','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
               write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(ugrd), iret=',iret
               call stop2(23)
           endif
           ug = nems_wrk
           call nemsio_readrecv(gfilein,'vgrd','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
               write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(vgrd), iret=',iret
               call stop2(23)
           endif
           vg = nems_wrk
        else
           divspec = sigdata%d(:,k); vrtspec = sigdata%z(:,k)
           call sptezv_s(divspec,vrtspec,ug,vg,1)
        endif
        ug = ug*dpfg(:,k)
        vg = vg*dpfg(:,k)
        call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
        call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
     enddo
!$omp end parallel do

     ! analyzed ps tend increment
     call copyfromgrdin(grdin(:,nlevs*ns3d + pst_ind,nb),pstend2)
     pstendfg = sum(vmassdiv,2)
     vmassdivinc = vmassdiv
     if (nanal .eq. 1) then
     print *,'time level ',nb
     print *,'--------------------'
     print *,nanal,'min/max pstendfg',minval(pstendfg),maxval(pstendfg)
     print *,nanal,'min/max pstend inc',minval(pstend2),maxval(pstend2)
     endif
     pstend2 = pstend2 + pstendfg ! add to background ps tend

  endif ! if pst_ind > 0

  if (.not. use_gfs_nemsio) then
  ! add increment to first guess in spectral space.
!$omp parallel do private(k,nt,ug,vg,vrtspec,divspec)  shared(sigdata,sigdata_inc,vmassdiv,dpanl)
     do k=1,nlevs

! add increments in spectral space
        sigdata%z(:,k) = sigdata%z(:,k) + sigdata_inc%z(:,k)
        sigdata%d(:,k) = sigdata%d(:,k) + sigdata_inc%d(:,k)
        sigdata%t(:,k) = sigdata%t(:,k) + sigdata_inc%t(:,k)
        do nt=1,sighead%ntrac
           sigdata%q(:,k,nt) = sigdata%q(:,k,nt) + sigdata_inc%q(:,k,nt)
        enddo

        if (pst_ind > 0) then
!          calculate vertical integral of mass flux div for updated state
           divspec = sigdata%d(:,k); vrtspec = sigdata%z(:,k)
           call sptezv_s(divspec,vrtspec,ug,vg,1)
           ug = ug*dpanl(:,k)
           vg = vg*dpanl(:,k)
           call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
           call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
        endif

     enddo
!$omp end parallel do

     ! don't need sigdata_inc anymore.
     call sigio_axdata(sigdata_inc,ierr)
  else
     if (pst_ind > 0) then
        allocate(ugtmp(nlons*nlats,nlevs),vgtmp(nlons*nlats,nlevs))
     endif
     ! update u,v,Tv,q,oz,clwmr
     do k=1,nlevs
        call nemsio_readrecv(gfilein,'ugrd','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(ugrd), iret=',iret
           call stop2(23)
        endif
        ug = 0.
        if (u_ind > 0) then
          call copyfromgrdin(grdin(:,(u_ind-1)*nlevs + k,nb),ug)
        endif
        ug =  nems_wrk + ug
        if (pst_ind < 0) then
           nems_wrk = ug
           call nemsio_writerecv(gfileout,'ugrd','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(ugrd), iret=',iret
              call stop2(23)
           endif
        else
           ugtmp(:,k) = ug
        endif
     
        call nemsio_readrecv(gfilein,'vgrd','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(vgrd), iret=',iret
           call stop2(23)
        endif
        vg = 0.
        if (v_ind > 0) then
           call copyfromgrdin(grdin(:,(v_ind-1)*nlevs + k,nb),vg)
        endif
        vg =  nems_wrk + vg
        if (pst_ind < 0) then
           nems_wrk = vg
           call nemsio_writerecv(gfileout,'vgrd','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(vgrd), iret=',iret
              call stop2(23)
           endif
        else
           vgtmp(:,k) = vg
        endif

        if (pst_ind > 0) then
           ug = ug*dpanl(:,k)
           vg = vg*dpanl(:,k)
           call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
           call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
        end if

        call nemsio_readrecv(gfilein,'tmp','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(tmp), iret=',iret
           call stop2(23)
        endif
        call nemsio_readrecv(gfilein,'spfh','mid layer',k,nems_wrk2,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(spfh), iret=',iret
           call stop2(23)
        endif
        ug = 0.
        if (t_ind > 0) then
          call copyfromgrdin(grdin(:,nlevs*(t_ind-1)+k,nb),ug)
        endif
        vg = 0.
        if (q_ind > 0) then
          call copyfromgrdin(grdin(:,nlevs*(q_ind-1)+k,nb),vg)
        endif
        ! ug is Tv increment, nems_wrk is background T, nems_wrk2 is background spfh
        ug = ug + nems_wrk * ( 1.0 + fv*nems_wrk2 )
        vg = vg + nems_wrk2 
        if (cliptracers)  where (vg < clip) vg = clip
        ! convert Tv back to T
        nems_wrk = ug/(1. + fv*vg)
        call nemsio_writerecv(gfileout,'tmp','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(tmp), iret=',iret
           call stop2(23)
        endif
        nems_wrk2 = vg
        call nemsio_writerecv(gfileout,'spfh','mid layer',k,nems_wrk2,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(spfh), iret=',iret
           call stop2(23)
        endif

        call nemsio_readrecv(gfilein,'o3mr','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(o3mr), iret=',iret
           call stop2(23)
        endif

        ug = 0.
        if (oz_ind > 0) then
           call copyfromgrdin(grdin(:,nlevs*(oz_ind-1)+k,nb),ug)
        endif
        nems_wrk = nems_wrk + ug
        if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
        call nemsio_writerecv(gfileout,'o3mr','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(o3mr), iret=',iret
           call stop2(23)
        endif

        call nemsio_readrecv(gfilein,'clwmr','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(clwmr), iret=',iret
           call stop2(23)
        endif
        ug = 0.
        if (cw_ind > 0) then
           call copyfromgrdin(grdin(:,nlevs*(cw_ind-1)+k,nb),ug)
        endif
        nems_wrk = nems_wrk + ug
        if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
        call nemsio_writerecv(gfileout,'clwmr','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(clwmr), iret=',iret
           call stop2(23)
        endif

    enddo
  endif

  if (pst_ind > 0) then

     vmassdivinc = vmassdiv - vmassdivinc ! analyis - first guess VIMFD
     ! (VIMFD = vertically integrated mass flux divergence)
     pstend1 = sum(vmassdiv,2)
     if (nanal .eq. 1) then
     print *,nanal,'min/max analysis ps tend',minval(pstend1),maxval(pstend1)
     print *,nanal,'min/max analyzed ps tend',minval(pstend2),maxval(pstend2)
     endif
     ! vmass is vertical integral of dp**2
     vmass = 0.
     do k=1,nlevs
        ! case 2 (4.3.1.2) in GEOS DAS document.
        ! (adjustment proportional to mass in layer)
        vmass = vmass + dpanl(:,k)**2
        ! case 3 (4.3.1.3) in GEOS DAS document.
        ! (adjustment propotional to mass-flux div increment)
        !vmass = vmass + vmassdivinc(:,k)**2
     enddo
     ! adjust wind field in analysis so pstend is consistent with pstend2
     ! (analyzed pstend)
!$omp parallel do private(k,nt,ug,vg,uginc,vginc,vrtspec,divspec)  shared(sigdata,vmassdiv,vmassdivinc,dpanl)
     do k=1,nlevs
        ! case 2 
        ug = (pstend2 - pstend1)*dpanl(:,k)**2/vmass
        ! case 3 
        !ug = (pstend2 - pstend1)*vmassdivinc(:,k)**2/vmass
        call sptez_s(divspec,ug,-1) ! divgrd to divspec
        vrtspec = 0.
        call sptezv_s(divspec,vrtspec,uginc,vginc,1) ! div,vrt to u,v
        if (nanal .eq. 1) then
          print *,k,'min/max u inc (member 1)',&
          minval(uginc/dpanl(:,k)),maxval(uginc/dpanl(:,k))
        endif
        if (use_gfs_nemsio) then
           ugtmp(:,k) = (ugtmp(:,k)*dpanl(:,k) + uginc)/dpanl(:,k)
           vgtmp(:,k) = (vgtmp(:,k)*dpanl(:,k) + vginc)/dpanl(:,k)
           ug = ugtmp(:,k); vg = vgtmp(:,k)
        else
           ! adjust spectral div,vort 
           ! (vrtspec,divspec to u,v, add increment to u,v, then convert
           ! back go vrtspec,divspec)
           divspec = sigdata%d(:,k); vrtspec = sigdata%z(:,k)
           call sptezv_s(divspec,vrtspec,ug,vg,1)
           ug = (ug*dpanl(:,k) + uginc)/dpanl(:,k)
           vg = (vg*dpanl(:,k) + vginc)/dpanl(:,k)
           call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
           sigdata%d(:,k) = divspec; sigdata%z(:,k) = vrtspec
           ! recompute u,v
           divspec = sigdata%d(:,k); vrtspec = sigdata%z(:,k)
           call sptezv_s(divspec,vrtspec,ug,vg,1)
        endif
! check result..
        ug = ug*dpanl(:,k); vg = vg*dpanl(:,k)
        call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
        call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
     enddo
!$omp end parallel do

     ! should be same as analyzed ps tend 
     psfg = sum(vmassdiv,2)
     !if (nanal .eq. 1) then
     !   open(919,file='pstend.dat',form='unformatted',access='direct',recl=nlons*nlats)
     !   write(919,rec=1) pstendfg
     !   write(919,rec=2) pstend2
     !   write(919,rec=3) psfg
     !   write(919,rec=4) pstend1
     !   close(919)
     !endif
     if (nanal .eq. 1) then
     print *,nanal,'min/max adjusted ps tend',minval(psfg),maxval(psfg)
     print *,nanal,'min/max diff between adjusted and analyzed ps tend',&
             minval(pstend2-psfg),maxval(pstend2-psfg)
     endif

  endif ! if pst_ind > 0

  if (.not. use_gfs_nemsio) then
  ! clip tracers.
     if (cliptracers) then
        clip = tiny(vg(1))
!$omp parallel do private(k,nt,vg,divspec)  shared(sigdata,clip)
        do k=1,nlevs
           if (q_ind > 0) then
             divspec = sigdata%q(:,k,1)
             call sptez_s(divspec,vg,1)
             where (vg < clip) vg = clip
             call sptez_s(divspec,vg,-1)
             sigdata%q(:,k,1) = divspec
          endif
          if (oz_ind > 0) then
             divspec = sigdata%q(:,k,2)
             call sptez_s(divspec,vg,1)
             where (vg < clip) vg = clip
             call sptez_s(divspec,vg,-1)
             sigdata%q(:,k,2) = divspec
          endif
          if (cw_ind > 0) then
             divspec = sigdata%q(:,k,3)
             call sptez_s(divspec,vg,1)
             where (vg < clip) vg = clip
             call sptez_s(divspec,vg,-1)
             sigdata%q(:,k,3) = divspec
          endif
        enddo
!$omp end parallel do
     end if

     ! write out analysis.
     call sigio_swohdc(iunitsig,filenameout,sighead,sigdata,ierr)
     ! deallocate sigdata structure.
     call sigio_axdata(sigdata,ierr)
  else
     if (pst_ind > 0) then
        ! update u,v
        do k=1,nlevs
           nems_wrk = ugtmp(:,k)
           call nemsio_writerecv(gfileout,'ugrd','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(ugrd), iret=',iret
              call stop2(23)
           endif
           nems_wrk = vgtmp(:,k)
           call nemsio_writerecv(gfileout,'vgrd','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(vgrd), iret=',iret
              call stop2(23)
           endif
        enddo
        deallocate(ugtmp,vgtmp)
     endif
  endif

  if (use_gfs_nemsio) then
      call nemsio_close(gfilein,iret=iret)
      call nemsio_close(gfileout,iret=iret)
  endif

  deallocate(pressi,dpanl,dpfg)
  deallocate(psg,pstend1,pstend2,pstendfg,vmass)
  if (pst_ind > 0) then
     deallocate(vmassdiv)
     deallocate(vmassdivinc)
  endif

  end do backgroundloop ! loop over backgrounds to write out

 end subroutine writegriddata

! copying to grdin (calling regtoreduced if reduced grid)
 subroutine copytogrdin(field, grdin)
 implicit none
 
 real(r_kind), dimension(:), intent(in)      :: field
 real(r_single), dimension(:), intent(inout) :: grdin
 
 if (reducedgrid) then
   call regtoreduced(field, grdin)
 else
   grdin = field
 endif
 
 end subroutine copytogrdin
 
! copying to grdin (calling regtoreduced if reduced grid)
 subroutine copyfromgrdin(grdin, field)
 implicit none
 
 real(r_single), dimension(:), intent(in)      :: grdin
 real(r_kind), dimension(:), intent(inout) :: field
 
 if (reducedgrid) then
   call reducedtoreg(grdin, field)
 else
   field = grdin
 endif
 
 end subroutine copyfromgrdin

 end module gridio
