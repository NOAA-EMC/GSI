program getsfcnstensupdp
!$$$  main program documentation block
!
! program:  getsfcnstenupdp              update sfc and nst files for ensemble
!
! prgmmr: Xi Li         org: EMC               date: 2014-05-01
!
! abstract:  update sfc & nst file for ensemble 
!
! program history log:
!   2014-05-01  Initial version.
!
! usage:
!   input files:
!
!   output files:
!
! attributes:
!   language: f95
!
!$$$
!   nstio_data        nst file data fields
!     slmsk             Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       surface mask: 0 = water; 1 = land; 2 = ice
!     xt                Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       heat content in DTL
!                       (M*K)
!     xs                Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       salinity content in DTL
!                       (M*ppt)
!     xu                Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       u-current content in DTL
!                       (M*M/S)
!     xv                Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       v-current content in DTL
!                       (M*M/S)
!     xz                Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       DTL thickness                                        (M)
!     zm                Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       MXL thickness                                        (M)
!     xtts              Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       d(xt)/d(Ts)
!                       (1/M)
!     xzts              Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       d(xz)/d(Ts)
!                       (M/K)
!     dt_cool           Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       sea surface cooling amount by sub-layer cooling effect
!     z_c               Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       sea sub-layer depth in m
!     c_0               Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       coefficient to calculate d(Tz)/d(tr) in dimensionless
!     c_d               Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       coefficient to calculate d(Tz)/d(tr) in
!                       (1/M)
!     w_0               Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       coefficient to calculate d(Tz)/d(tr) in dimensionless
!     w_d               Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       coefficient to calculate d(Tz)/d(tr)
!                       (1/M)
!     d_conv            Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       FCL thickness
!                       (M)
!     ifd               Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       index of time integral started mode: 0 = not yet; 1 =
!                       started already
!     Tref              Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       reference temperature
!                       (K)
!     Qrain             Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       sensible heat flux due to rainfall
!                       (W*M^-2)

!   sfcio_data        Surface file data fields
!     tsea              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       surface temperature in K
!     smc               Real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       soil volumetric water content in fraction
!     sheleg            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       snow depth in m
!     stc               Real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       soil temperature in K
!     tg3               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       deep soil temperature in K
!     zorl              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       roughness in cm
!     cv                Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud cover in fraction
!     cvb               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud bottom in kpa
!     cvt               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud top in kpa
!     alvsf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for visible scattered in fraction
!     alvwf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for visible beam in fraction
!     alnsf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for near-IR scattered in fraction
!     alnwf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for near-IR beam in fraction
!     slmsk             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       sea-land-ice mask (0-sea, 1-land, 2-ice)
!     vfrac             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       vegetation fraction in fraction
!     canopy            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       canopy water in m
!     f10m              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       10-meter wind speed over lowest model wind speed
!     t2m               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       2-meter temperature in K
!     q2m               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       2-meter specific humidity in kg/kg
!     vtype             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       vegetation type in integer 1-13
!     stype             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       soil type in integer 1-9
!     facsf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in fraction
!     facwf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in fraction
!     uustar            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     ffmm              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     ffhh              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     hice              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     fice              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     tisfc             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     tprcp             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     srflag            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     snwdph            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     slc               Real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       xxx in xxx
!     shdmin            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     shdmax            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     slope             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       slope type
!     snoalb            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     orog              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       orography in m

! create ensemble mean NCEP GFS surface file.

  use sfcio_module
  use nstio_module
  implicit none
 
  integer, parameter :: houra = 0.0
  character(len=10) :: fname_dtsinc
  character(len=13) :: fname_nstges,fname_sfcges,fname_sfcgcy,fname_nstanl,fname_sfcanl
  character(len=3)  :: charnanal
  character(len=1)  :: charnst
  integer :: lun_inc,lun_nstges,lun_nstanl,lun_sfcges,lun_sfcgcy,lun_sfcanl,iret,ierr,nanals
  integer :: mype,mype1,npe,nproc
  integer :: latb,lonb,n_new_water,n_new_seaice
  integer :: i,j
  integer :: nst_gsi

  type(nstio_head):: head_nst
  type(nstio_data):: data_nst
  type(sfcio_head):: head_sfcanl,head_sfcges,head_sfcgcy
  type(sfcio_data):: data_sfcanl,data_sfcges,data_sfcgcy

  double precision, allocatable, dimension(:,:) :: tf_inc
  real :: dtw,dtc

! mpi definitions.
  include 'mpif.h'

! Initialize mpi
!  mype is process number, npe is total number of processes.
  call mpi_init(iret)
  call MPI_Comm_rank(MPI_COMM_WORLD,mype,iret)
  call MPI_Comm_size(MPI_COMM_WORLD,npe,iret)

  if (mype==0) call w3tagb('GETSFCNSTENSUPD',2014,0921,0055,'NP25')

  call getarg(1,charnanal)
  read(charnanal,'(i2)') nanals

  call getarg(2,charnst)
  read(charnst,'(i1)') nst_gsi

  if (mype==0) then
     write(6,*)' '
     write(6,*)'Command line input'
     write(6,*)' nanals= ',nanals
     write(6,*)' nst_gsi = ',nst_gsi
  endif

  if (npe < nanals) then
     write(6,*)'***ERROR***  npe too small.  npe=',npe,' < nanals=',nanals
     call MPI_Abort(MPI_COMM_WORLD,99,iret)
     stop
  end if

  lun_inc=11
  lun_nstges=21
  lun_sfcges=22
  lun_sfcgcy=23
  lun_nstanl=61
  lun_sfcanl=62

  mype1 = mype + 1
  if (mype1 > nanals) then
    write (6,*) 'no files to process for mpi task = ',mype
  else
    write(charnanal,'(i3.3)') mype1

    fname_dtsinc = 'dtsinc_ens'
    fname_nstges = 'nstf06_mem'//charnanal
    fname_nstanl = 'nstanl_mem'//charnanal
    fname_sfcges = 'sfcf06_mem'//charnanal
    fname_sfcgcy = 'sfcgcy_mem'//charnanal
    fname_sfcanl = 'sfcanl_mem'//charnanal
         
!
!   read nsst guess fields
!
    call nstio_srohdc(lun_nstges,fname_nstges,head_nst,data_nst,iret)
    write(6,*)'Read ',trim(fname_nstges),' iret=',iret
!
!   read sfc guess fields
!
    call sfcio_srohdc(lun_sfcges,fname_sfcges,head_sfcges,data_sfcges,iret)
    write(6,*)'Read ',trim(fname_sfcges),' iret=',iret
!
!   read sfc global_cycle fields
!
    call sfcio_srohdc(lun_sfcgcy,fname_sfcgcy,head_sfcgcy,data_sfcgcy,iret)
    write(6,*)'Read ',trim(fname_sfcgcy),' iret=',iret

    if ( head_nst%latb /= head_sfcgcy%latb .or. head_nst%lonb /= head_sfcgcy%lonb ) then
       write(6,*) 'Inconsistent dimension for sfc & nst files. head_nst%latb,head_nst%lonb : ',head_nst%latb,head_nst%lonb, &
                  'head_sfcgcy%latb,head_sfcgcy%lonb : ',head_sfcgcy%latb,head_sfcgcy%lonb
    endif
!
!  Assign sfcanl as sfcgcy
!
    head_sfcanl = head_sfcgcy
    data_sfcanl = data_sfcgcy

    latb=head_sfcanl%latb
    lonb=head_sfcanl%lonb

    allocate(tf_inc(lonb,latb))

    open(lun_inc,file=fname_dtsinc,form='unformatted')
    read(lun_inc) tf_inc
!
!   For the new open water (sea ice just melted) grids, reset the NSSTM
!   variables
!
!   set tref = tfrozen = 271.2_r_kind
!   note: data_sfcges%slmsk is the mask of the guess
!         data_sfcanl%slmsk is the mask of the analysis
!
    where ( (data_sfcanl%slmsk(:,:) == 0.0) .and. (data_sfcges%slmsk(:,:) == 2.0) )
          data_nst%xt(:,:)      = 0.0
          data_nst%xs(:,:)      = 0.0
          data_nst%xu(:,:)      = 0.0
          data_nst%xv(:,:)      = 0.0
          data_nst%xz(:,:)      = 30.0
          data_nst%zm(:,:)      = 0.0
          data_nst%xtts(:,:)    = 0.0
          data_nst%xzts(:,:)    = 0.0
          data_nst%dt_cool(:,:) = 0.0
          data_nst%z_c(:,:)     = 0.0
          data_nst%c_0(:,:)     = 0.0
          data_nst%c_d(:,:)     = 0.0
          data_nst%w_0(:,:)     = 0.0
          data_nst%w_d(:,:)     = 0.0
          data_nst%d_conv(:,:)  = 0.0
          data_nst%ifd(:,:)     = 0.0
          data_nst%tref(:,:)    = 271.2
          data_nst%qrain(:,:)   = 0.0
    end where
!
!   update analysis variable: Tref (foundation temperature) for nstanl file
!
    where ( data_sfcanl%slmsk(:,:) == 0.0 )
       data_nst%tref(:,:) = max(data_nst%tref(:,:) + tf_inc(:,:),271.2)
    elsewhere
       data_nst%tref(:,:) = data_sfcanl%tsea(:,:)
    end where

!  Update guess date/time to analysis date/time for nst file
   head_nst%fhour    = head_sfcanl%fhour           ! forecast hour
   head_nst%idate(1) = head_sfcanl%idate(1)        ! hour
   head_nst%idate(2) = head_sfcanl%idate(2)        ! month
   head_nst%idate(3) = head_sfcanl%idate(3)        ! day
   head_nst%idate(4) = head_sfcanl%idate(4)        ! year

!  Write updated information to nst analysis file
   call nstio_swohdc(lun_nstanl,fname_nstanl,head_nst,data_nst,iret)

   write(6,101) fname_nstanl,lonb,latb,head_nst%fhour,(head_nst%idate(i),i=1,4),iret
101    format(' getsfcnstupdp:  nst analysis written for ',&
              a13,1x,2i6,1x,f4.1,4(i4,1x),' with iret=',i2)

!
!  update SST: tsea for sfcanl file
!
   if ( nst_gsi == 3 ) then

     where ( data_sfcanl%slmsk(:,:) == 0.0 )
        data_sfcanl%tsea(:,:) = max(data_nst%tref(:,:) + 2.0*data_nst%xt(:,:)/data_nst%xz(:,:) - data_nst%dt_cool(:,:), 271.2)
     end where

!
!    write out the info on the new open water and new sea ice grids
!
     if ( mype == 0 ) then
       n_new_water = 0
       n_new_seaice = 0
       do j = 1, latb
         do i = 1, lonb

           if ( data_sfcanl%slmsk(i,j) == 0.0 .and. data_sfcges%slmsk(i,j) == 2.0 ) then
             n_new_water = n_new_water + 1
             dtw = 2.0*data_nst%xt(i,j)/data_nst%xz(i,j)
             dtc = data_nst%dt_cool(i,j)
             write(*,'(a,I7,1x,I4,1x,I4,16F8.2)') 'Info on new water ens_grids :',n_new_water,j,i, &
                       data_sfcges%fice(i,j), data_sfcgcy%fice(i,j), data_sfcanl%fice(i,j), &
                       data_sfcges%hice(i,j), data_sfcgcy%hice(i,j), data_sfcanl%hice(i,j), &
                       data_sfcges%tisfc(i,j),data_sfcgcy%tisfc(i,j),data_sfcanl%tisfc(i,j), &
                       data_sfcges%tsea(i,j), data_sfcgcy%tsea(i,j), data_sfcanl%tsea(i,j), &
                       data_nst%tref(i,j),tf_inc(i,j),dtw,dtc
           endif

           if ( data_sfcanl%slmsk(i,j) == 2.0 .and. data_sfcges%slmsk(i,j) == 0.0 ) then
             n_new_seaice = n_new_seaice + 1
             dtw = 2.0*data_nst%xt(i,j)/data_nst%xz(i,j)
             dtc = data_nst%dt_cool(i,j)
             write(*,'(a,I7,1x,I4,1x,I4,16F8.2)') 'Info on new seaice ens_grids :',n_new_seaice,j,i, &
                       data_sfcges%fice(i,j), data_sfcgcy%fice(i,j), data_sfcanl%fice(i,j), &
                       data_sfcges%hice(i,j), data_sfcgcy%hice(i,j), data_sfcanl%hice(i,j), &
                       data_sfcges%tisfc(i,j),data_sfcgcy%tisfc(i,j),data_sfcanl%tisfc(i,j), &
                       data_sfcges%tsea(i,j), data_sfcgcy%tsea(i,j), data_sfcanl%tsea(i,j), &
                       data_nst%tref(i,j),tf_inc(i,j),dtw,dtc
           endif

         end do
       end do
     endif          ! if ( mype == 0 ) then



!    Write updated information to surface analysis file
     call sfcio_swohdc(lun_sfcanl,fname_sfcanl,head_sfcanl,data_sfcanl,iret)

     write(6,102) fname_sfcanl,lonb,latb,head_sfcanl%fhour,(head_sfcanl%idate(i),i=1,4),iret
102  format(' getsfcnstupdp:  sfc analysis written for ',&
              a13,1x,2i6,1x,f4.1,4(i4,1x),' with iret=',i2)

   endif

   close (lun_nstges)
   close (lun_sfcges)
   close (lun_sfcgcy)
   close (lun_nstanl)
   close (lun_sfcanl)

  end if  ! end if mype1

  call MPI_Barrier(MPI_COMM_WORLD,ierr)

  if (nproc==0) call w3tage('GETSFCNSTENSUPDP')

  call MPI_Finalize(ierr)
  if (nproc .eq. 0 .and. ierr .ne. 0) then
     print *, 'MPI_Finalize error status = ',ierr
  end if


END program getsfcnstensupdp
