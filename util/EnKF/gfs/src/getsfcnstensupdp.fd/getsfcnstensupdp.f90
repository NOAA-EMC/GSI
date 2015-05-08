program getsfcnstensupdp
!$$$  main program documentation block
!
! program:  getsfcnstenupdp              update sfc and nst files for ensemble
!
! prgmmr: Xu Li         org: EMC               date: 2014-05-01
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

  use mpi
  use kinds, only: r_kind,i_kind,r_single
  use constants, only: two,half,zero,z_w_max,tfrozen,init_constants_derived,pi
  use sfcio_module, only: sfcio_srohdc,sfcio_head,sfcio_data,sfcio_swohdc
  use nstio_module, only: nstio_srohdc,nstio_head,nstio_data,nstio_swohdc

  implicit none
 
  real(r_kind),    parameter :: houra=zero
  integer(i_kind), parameter :: nprep=15
  integer(i_kind), parameter :: lun_dtfanl=11,lun_nstges=21,lun_sfcges=22, &
                                lun_sfcgcy=23,lun_nstanl=61,lun_sfcanl=62
  integer(i_kind), parameter :: idrt=4

  character(len=80) :: fname_dtfanl,fname_nstges,fname_sfcges,fname_sfcgcy,fname_nstanl,fname_sfcanl
  character(len=3)  :: charnanal
  character(len=8)  :: charbuf

  integer(i_kind) :: mype,mype1,npe,nproc,iret
  integer(i_kind) :: latb,lonb,n_new_water,n_new_seaice
  integer(i_kind) :: i,j,jmax
  integer(i_kind) :: nanals,nst_gsi,zsea1,zsea2
  integer(i_kind) :: nlon_anl,nlat_anl    ! the number of lon/lat of GSI analysis grids, including two extra polar lats
  integer(i_kind) :: nlon_ens,nlat_ens    ! the number of lon/lat of ensemble grids, including two extra polar lats
  integer(i_kind), allocatable, dimension(:,:) :: isli_anl,isli_epd,isli_gsi
  real(r_kind), allocatable, dimension(:)      :: wlatx,slatx,rlats_anl,rlons_anl,rlats_ens,rlons_ens
  real(r_kind), allocatable, dimension(:,:)    :: dtf_anl,dtf_epd,dtf_gsi,dtf_ens
  real(r_single), allocatable, dimension(:,:)    :: work
  real(r_single), allocatable, dimension(:,:)  :: dtzm
  real(r_kind) :: dlon
  real(r_single) :: r_zsea1,r_zsea2
  real(r_kind) sumn,sums

  type(nstio_head):: head_nst
  type(nstio_data):: data_nst
  type(sfcio_head):: head_sfcanl,head_sfcges,head_sfcgcy
  type(sfcio_data):: data_sfcanl,data_sfcges,data_sfcgcy

! Initialize mpi
!  mype is process number, npe is total number of processes.
  call mpi_init(iret)
  call MPI_Comm_rank(MPI_COMM_WORLD,mype,iret)
  call MPI_Comm_size(MPI_COMM_WORLD,npe,iret)

  call init_constants_derived

  if ( mype == 0 ) call w3tagb('GETSFCNSTENSUPD',2014,0921,0055,'NP25')

  call getarg(1,charbuf)
  read(charbuf,'(i3)') nanals

  call getarg(2,charbuf)
  read(charbuf,'(i1)') nst_gsi

  call getarg(3,charbuf)
  read(charbuf,'(i8)') zsea1
  r_zsea1 = 0.001_r_single * real(zsea1,r_single)

  call getarg(4,charbuf)
  read(charbuf,'(i8)') zsea2
  r_zsea2 = 0.001_r_single * real(zsea2,r_single)

  if (mype==0) then
     write(6,'(a)')' '
     write(6,'(a)')'Command line input'
     write(6,'(a,i5)')' nanals  = ',nanals
     write(6,'(a,i5)')' nst_gsi = ',nst_gsi
     write(6,'(a,i5)')' zsea1   = ',zsea1
     write(6,'(a,i5)')' zsea2   = ',zsea2
  endif

  if ( npe < nanals ) then
     write(6,'(2(a,i5))')'***ERROR***  npe too small. npe = ',npe,' < nanals = ',nanals
     call MPI_Abort(MPI_COMM_WORLD,99,iret)
     stop
  endif

  mype1 = mype + 1
  if ( mype1 > nanals ) then

    write (6,'(a,i5)') 'no files to process for mpi task = ',mype

  else

    write(charnanal,'(i3.3)') mype1

    fname_dtfanl = 'dtfanl'
    fname_nstges = 'nstf06_mem' // charnanal
    fname_nstanl = 'nstanl_mem' // charnanal
    fname_sfcges = 'sfcf06_mem' // charnanal
    fname_sfcgcy = 'sfcgcy_mem' // charnanal
    fname_sfcanl = 'sfcanl_mem' // charnanal
         
!
!   read Tf analysis increment at GSI analysis grids and its grid info and surface mask
!
    open(lun_dtfanl,file=trim(fname_dtfanl),form='unformatted')
    read(lun_dtfanl) nlon_anl,nlat_anl

    allocate(dtf_anl(nlat_anl,nlon_anl),isli_anl(nlat_anl,nlon_anl))
    allocate(dtf_epd(nlat_anl,nlon_anl),isli_epd(nlat_anl,nlon_anl))

    read(lun_dtfanl) dtf_anl
    read(lun_dtfanl) isli_anl

!
!   read nsst guess fields
!
    call nstio_srohdc(lun_nstges,trim(fname_nstges),head_nst,data_nst,iret)
    write(6,'(3a,i5)')'Read ',trim(fname_nstges),' iret = ',iret
!
!   read sfc guess fields
!
    call sfcio_srohdc(lun_sfcges,trim(fname_sfcges),head_sfcges,data_sfcges,iret)
    write(6,'(3a,i5)')'Read ',trim(fname_sfcges),' iret = ',iret
!
!   read sfc global_cycle fields
!
    call sfcio_srohdc(lun_sfcgcy,trim(fname_sfcgcy),head_sfcgcy,data_sfcgcy,iret)
    write(6,'(3a,i5)')'Read ',trim(fname_sfcgcy),' iret = ',iret

    if ( head_nst%latb /= head_sfcgcy%latb .or. head_nst%lonb /= head_sfcgcy%lonb ) then
       if ( mype == 0 ) then
          write(6,'(a)') 'Inconsistent dimension for sfc and nst files'
          write(6,'(2(a,i5))') 'head_nst%latb    = ',head_nst%latb,   ' head_nst%lonb    = ',head_nst%lonb
          write(6,'(2(a,i5))') 'head_sfcgcy%latb = ',head_sfcgcy%latb,' head_sfcgcy%lonb = ',head_sfcgcy%lonb
       endif
    endif
!
!  Assign sfcanl as sfcgcy
!
    head_sfcanl = head_sfcgcy
    data_sfcanl = data_sfcgcy

    lonb=head_sfcanl%lonb
    latb=head_sfcanl%latb

    nlon_ens = lonb
    nlat_ens = latb + 2

    allocate(dtf_gsi(nlat_ens,nlon_ens),isli_gsi(lonb,latb),work(nlat_ens,nlon_ens))
    allocate(dtf_ens(lonb,latb),dtzm(lonb,latb))

    if ( (nlat_ens /= nlat_anl) .or. (nlon_ens /= nlon_anl) ) then

       if ( mype == 0 ) &
          write(6,'(a,2(2a,2i5))')'getsfcnstensupdp: grid dimensions differ: ',&
          'nlon_anl,nlat_anl = ',nlon_anl,nlat_anl,' nlon_ens,nlat_ens = ',nlon_ens,nlat_ens
!
!      get lats and lons for GSI analysis grids
!
       jmax=nlat_anl-2
       allocate(slatx(jmax),wlatx(jmax))
       allocate(rlats_anl(nlat_anl),rlons_anl(nlon_anl))
       call splat(idrt,jmax,slatx,wlatx)
       dlon=two*pi / real(nlon_anl,r_kind)
       do i=1,nlon_anl
          rlons_anl(i)=real(i-1,r_kind)*dlon
       enddo
       do i=1,(nlat_anl-1)/2
          rlats_anl(i+1)=-asin(slatx(i))
          rlats_anl(nlat_anl-i)=asin(slatx(i))
       enddo
       rlats_anl(1)=-half*pi
       rlats_anl(nlat_anl)=half*pi
       deallocate(slatx,wlatx)
!
!      get lats and lons for ensemble grids
!
       jmax=nlat_ens-2
       allocate(slatx(jmax),wlatx(jmax))
       allocate(rlats_ens(nlat_ens),rlons_ens(nlon_ens))
       call splat(idrt,jmax,slatx,wlatx)
       dlon=two*pi / real(nlon_ens,r_kind)
       do i=1,nlon_ens
          rlons_ens(i)=real(i-1,r_kind)*dlon
       enddo
       do i=1,(nlat_ens-1)/2
          rlats_ens(i+1)=-asin(slatx(i))
          rlats_ens(nlat_ens-i)=asin(slatx(i))
       enddo
       rlats_ens(1)=-half*pi
       rlats_ens(nlat_ens)=half*pi
       deallocate(slatx,wlatx)
!
!      Get updated/analysis surface mask info from sfcgcy_ensmean  file
!
       sumn = zero
       sums = zero
       do i=1, lonb
          sumn = data_sfcgcy%slmsk(i,1)    + sumn
          sums = data_sfcgcy%slmsk(i,latb) + sums
       end do
       sumn = sumn/float(lonb)
       sums = sums/float(lonb)

!    Transfer from local work array to surface guess array
       do j = 1, lonb
          work(1,j)=sums
          do i=2, latb+1
             work(i,j) = data_sfcgcy%slmsk(j,latb+2-i)
          end do
          work(latb+2,j)=sumn
       end do

       do j=1, nlon_ens
          do i=1, nlat_ens
             isli_gsi(i,j) = nint(work(i,j))
          enddo
       enddo
!
!      Get the expanded values for a surface type (0 = water now) and the new mask
!
       call int2_msk_glb_prep(dtf_anl,isli_anl,dtf_epd,isli_epd,nlat_anl,nlon_anl,0,nprep)
!
!      Interpolate dtf_epd(nlat_anl,nlon_anl) to dtf_gsi(nlat_ens,nlon_ens) with surface mask accounted
!
       call int22_msk_glb(dtf_epd,isli_epd,rlats_anl,rlons_anl,nlat_anl,nlon_anl, &
                          dtf_gsi,isli_gsi,rlats_ens,rlons_ens,nlat_ens,nlon_ens,0)
!
!      transform the dtf_gsi(nlat_ens,nlon_ens) to dtf_ens(lonb,latb) for sfc
!      file format
!
       do j=1,latb
          do i=1,lonb
             dtf_ens(i,j) = dtf_gsi(latb+2-j,i)
          enddo
       enddo

    else       ! when the GSI analysis grid is identical to ensemble one and
               ! no surface mask change from ges to anl

!
!      transform the dtf_anl(nlat_anl,nlon_anl) to dtf_ens(lonb,latb) for sfc file
!      format when nlat == latb-2 & nlon = lonb
!
       if ( mype == 0 ) &
          write(6,'(a,2(a,2i5))')'getsfcnstensupdp: grid dimensions same: ',&
          'nlon_anl,nlat_anl = ',nlon_anl,nlat_anl,' lonb,latb = ',lonb,latb

       do j=1,latb
          do i=1,lonb
             dtf_ens(i,j)=dtf_anl(latb+1-j,i)
          enddo
       enddo

    endif ! if ( (nlat_ens /= nlat_anl) .or. (nlon_ens /= nlon_anl) ) then
!
!   For the new open water (sea ice just melted) grids, (1) set dtf_ens = zero (2) reset the NSSTM variables
!
!   set tref = tfrozen = 271.2_r_kind
!   note: data_sfcges%slmsk is the mask of the guess
!         data_sfcanl%slmsk is the mask of the analysis
!
    where ( (data_sfcanl%slmsk(:,:) == zero) .and. (data_sfcges%slmsk(:,:) == two) )

          dtf_ens(:,:) = zero

          data_nst%xt(:,:)      = zero
          data_nst%xs(:,:)      = zero
          data_nst%xu(:,:)      = zero
          data_nst%xv(:,:)      = zero
          data_nst%xz(:,:)      = z_w_max
          data_nst%zm(:,:)      = zero
          data_nst%xtts(:,:)    = zero
          data_nst%xzts(:,:)    = zero
          data_nst%dt_cool(:,:) = zero
          data_nst%z_c(:,:)     = zero
          data_nst%c_0(:,:)     = zero
          data_nst%c_d(:,:)     = zero
          data_nst%w_0(:,:)     = zero
          data_nst%w_d(:,:)     = zero
          data_nst%d_conv(:,:)  = zero
          data_nst%ifd(:,:)     = zero
          data_nst%tref(:,:)    = tfrozen
          data_nst%qrain(:,:)   = zero

    end where
!
!   update analysis variable: Tref (foundation temperature) for nstanl file
!
    where ( data_sfcanl%slmsk(:,:) == zero )
       data_nst%tref(:,:) = max(data_nst%tref(:,:) + dtf_ens(:,:),tfrozen)
    elsewhere
       data_nst%tref(:,:) = data_sfcanl%tsea(:,:)
    end where

!   Update guess date/time to analysis date/time for nst file
    head_nst%fhour    = head_sfcanl%fhour           ! forecast hour
    head_nst%idate(1) = head_sfcanl%idate(1)        ! hour
    head_nst%idate(2) = head_sfcanl%idate(2)        ! month
    head_nst%idate(3) = head_sfcanl%idate(3)        ! day
    head_nst%idate(4) = head_sfcanl%idate(4)        ! year

!   Write updated information to nst analysis file
    call nstio_swohdc(lun_nstanl,trim(fname_nstanl),head_nst,data_nst,iret)

    write(6,101) trim(fname_nstanl),lonb,latb,head_nst%fhour,(head_nst%idate(i),i=1,4),iret
101 format(' getsfcnstupdp: nst analysis written for ',&
             a,1x,2i6,1x,f4.1,4(i4,1x),' with iret = ',i5)

!
!   update SST: tsea for sfcanl file
!
    if ( nst_gsi == 3 ) then

      call dtzm_2d(data_nst%xt,data_nst%xz,data_nst%dt_cool,data_nst%z_c, &
                   data_sfcanl%slmsk,r_zsea1,r_zsea2,lonb,latb,dtzm)
      where ( data_sfcanl%slmsk(:,:) == zero )
         data_sfcanl%tsea(:,:) = max(data_nst%tref(:,:) + dtzm(:,:),tfrozen)
      end where
 
!     Write updated information to surface analysis file
      call sfcio_swohdc(lun_sfcanl,trim(fname_sfcanl),head_sfcanl,data_sfcanl,iret)

      write(6,102) trim(fname_sfcanl),lonb,latb,head_sfcanl%fhour,(head_sfcanl%idate(i),i=1,4),iret
102   format(' getsfcnstupdp: sfc analysis written for ',&
               a,1x,2i6,1x,f4.1,4(i4,1x),' with iret = ',i5)

    endif ! if ( nst_gsi == 3 ) then

!
!   write out the info on the new open water and new sea ice grids
!
    if ( mype == 0 ) then
       n_new_water = 0
       n_new_seaice = 0
       do j=1,latb
          do i=1,lonb
             if ( data_sfcanl%slmsk(i,j) == 0.0 .and. data_sfcges%slmsk(i,j) == 2.0 ) &
                n_new_water = n_new_water + 1
             if ( data_sfcanl%slmsk(i,j) == 2.0 .and. data_sfcges%slmsk(i,j) == 0.0 ) &
                n_new_seaice = n_new_seaice + 1
          enddo
       enddo
       write(6,'(a,I3,2(1x,I8))') 'getsfcnstens,nst_gsi,n_new_water,n_new_seaice:',nst_gsi,n_new_water,n_new_seaice
    endif

  endif ! if ( mype1 < nanals ) then

  call MPI_Barrier(MPI_COMM_WORLD,iret)

  if ( nproc == 0 ) call w3tage('GETSFCNSTENSUPDP')

  call MPI_Finalize(iret)
  if ( nproc == 0 .and. iret /= 0 ) &
     write(6,'(a,i5)'), 'MPI_Finalize error status, iret = ',iret

END program getsfcnstensupdp
