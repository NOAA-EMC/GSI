!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 601.1     !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  GEOS_PertMod --- Handles perturbations between GSI and pertGCM
!
! !INTERFACE:
!
      module geos_pertmod

! !USES:

!  ADM/TLM entries
!  ---------------
#ifdef GEOS_PERT
      use precision
      use mod_comm,    only : mp_init
      use mod_comm,    only : mp_gather4d
      use mod_comm,    only : mp_scatter4d
      use stepon,      only : ng_d, ng_s
      use stepon,      only : stepon_set
      use stepon,      only : nymd, nhms
      use stepon,      only : pdt         ! time step of AD/TL models
      use prognostics, only : imr         ! no. of grid points in the zonal direction
      use prognostics, only : jnp         ! no. of grid points in the meridional direction
      use prognostics, only : nl          ! no. of levels
      use prognostics, only : nc          ! no. of tracers
      use prognostics, only : jfirst      ! pointer for lat decomposition
      use prognostics, only : jlast       ! pointer for lat decomposition
      use prognostics, only : prognostics_initial
      use prognostics, only : prognostics_final
      use prognostics, only : prognostics_dotp
      use prognostics, only : prognostics_dup
      use m_iostate,   only : getstate_init
      use m_iostate,   only : getstate
      use m_iostate,   only : getstate_clean
      use m_trjphys,   only : physdrv1_get_init
      use m_trjphys,   only : physdrv1_get_all
      use m_trjphys,   only : physdrv1_get_clean
      use m_trajmng,   only : getpert
      use m_trajmng,   only : putpert
      use m_trajmng,   only : inqpert_dims
      use prognostics, only : dyn_prog    ! GCM perturbation vector

      use m_interpack,    only : interpack_terpv
      use m_interpack_ad, only : interpack_terpv_ad

      use stepon_tl,   only : stepon_g4tog5_tl

      use m_die,       only : die

!  the following will be cleared once I update the interface to putpert
!  ....................................................................
      use stepon,      only : ak          ! GEOS-5 ADM/TLM pressure levels
      use stepon,      only : bk          ! GEOS-5 ADM/TLM pressure levels
      use stepon,      only : ts
      use stepon,      only : oro
      use stepon,      only : job
      use stepon,      only : nstep
      use stepon,      only : fvpsasdt

#endif /* GEOS_PERT */

!  GSI entries
!  -----------
      use kinds,       only : r_kind,i_kind
      use mpimod,      only : mype,mpi_rtype,mpi_comm_world
      use gridmod,     only : strip
      use gridmod,     only : displs_s,ijn_s
      use gridmod,     only : nlat, nlon     ! no. lat/lon
      use gridmod,     only : lat1, lon1     ! no. lat/lon on subdomain (no buffer)
      use gridmod,     only : lat2, lon2     ! no. lat/lon on subdomain (buffer pnts on ends)
      use gridmod,     only : nsig           ! no. levels
      use gridmod,     only : iglobal        ! no. of horizontal points on global grid
      use gridmod,     only : ijn            ! no. of horiz. pnts for each subdomain (no buffer)
      use gridmod,     only : displs_g       ! comm. array, displacement for receive on global grid
      use gridmod,     only : itotsub        ! no. of horizontal points of all subdomains combined
      use gridmod,     only : bk5
      use gsi_io,      only : reorder21,reorder12
      use constants,   only : izero,ione,zero,one,r1000,r3600
      use state_vectors                      ! GSI state vector

      implicit none

! !PUBLIC MEMBER FUNCTIONS:

      PRIVATE

      PUBLIC parallel_init
      PUBLIC model_init
      PUBLIC model_clean
      PUBLIC ndtpert

#ifdef GEOS_PERT

      PUBLIC pgcm2gsi
      PUBLIC gsi2pgcm
 
      interface pgcm2gsi; module procedure &
                pgcm2gsi0_,&
                pgcm2gsi1_
      end interface
      interface gsi2pgcm; module procedure &
                gsi2pgcm0_,&
                gsi2pgcm1_
      end interface
      interface SwapIJK; module procedure &
                SwapIJK_
      end interface

#endif /* GEOS_PERT */

      interface model_init; module procedure &
                init_
      end interface
      interface model_clean; module procedure &
                clean_
      end interface
      interface parallel_init; module procedure &
                mpp_init_
      end interface
!
! !DESCRIPTION: Maps gsi increments on to gcm perturbations and vice-versa.
!
! !REMARKS:
!
!   1) This package assumes xpert to have A-grid winds in its u/v slot
!   2) This package assumes xpert to have TV in the its pt slot, unless when
!      perturbation vector in read in from file.
!
! !TO DO:
!
!   1) Need to work on O3 (ADM/TLM not ready for this)
!   2) This interface should ultimately become an ESMF-Grided Component
!      and therefore become non-specific, i.e., applicable to an GCM
!      TL and AD models. For now, this is specific to GEOS-5.  
!   3) Allow vectype_ to be reset from RC file specific to this code to
!      handle GEOS-4 perturbation vector.
!
! !REVISION HISTORY:
!
!  08May2007  Todling   Initial code.
!  01Apr2010  Treadon   move strip to gridmod
!
!EOP
!-------------------------------------------------------------------------

      character(len=*), parameter :: myname = 'geos_pertmod'
      character(len=*), parameter :: fnpert = 'fsens.eta.hdf'
      character(len=*), parameter :: fnxgsi = 'xxgsi.eta'

      integer(i_kind),  parameter :: ROOT = izero ! should really come from above

      integer(i_kind), save  :: ndtpert

      integer(i_kind), save :: mycount = izero
      real(r_kind), parameter :: PPMV2DU    = 1.657E-6_r_kind
      real(r_kind), parameter :: kPa_per_Pa = 0.001_r_kind
      real(r_kind), parameter :: Pa_per_kPa = r1000

      logical, parameter :: memtraj = .true.
      logical, parameter :: verbose = .false.

      logical, save :: traj_initzd_ = .false.
      logical, save :: initialized_ = .false.
      logical, save :: skiptraj_    = .false.
      logical, save :: bks_checked_ = .false.
 
      integer(i_kind), save :: vectype_     = 5_i_kind       ! default is GEOS-5 vector

      CONTAINS

#ifdef GEOS_PERT
!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 601.1     !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: pgcm2gsi0_:  Convert gcm-adm/tlm vector to gsi vector
!
! !INTERFACE:

      subroutine pgcm2gsi0_ ( xx, which, stat, &
                              filename, skiptraj, nymd_in, nhms_in )

! !USES:

      implicit none

! !INPUT PARAMETERS:

      character(len=*),           intent(in   ) :: which    ! adm or tlm
      character(len=*), optional, intent(in   ) :: filename ! name of file w/ perturbation
      logical,          optional, intent(in   ) :: skiptraj ! allows skip of trajectory/ics 
                                                                                                                           
! !OUTPUT PARAMETERS:

      type(state_vector)        , intent(inout) :: xx       ! GSI increment

      integer(i_kind)           , intent(  out) :: stat

      integer(i_kind),optional  , intent(  out) :: nymd_in
      integer(i_kind),optional  , intent(  out) :: nhms_in

! !DESCRIPTION: Convert GEOS-5 perturbation vector to GSI increment vector
!               (as pgcm2gsi1_, but reads GCM perturbation from file)
!
! !REMARKS:
!
!  31Oct2007  Todling   De-activated interpolation capability; not fully coded yet
!
! !REVISION HISTORY:
!
!  08May2007  Todling   Initial code.
!  17Jul2007  Todling   Add ability to read in perturbation at diff resolution
!  17Jul2008  Todling   Add filename as optional argument
!
!EOP
!-----------------------------------------------------------------------

     character(len=*), parameter :: myname_ = myname//'*pgcm2gsi0_'
     type(dyn_prog) xpert
     type(dyn_prog) ypert

     character(len=255) :: fname
     integer(i_kind) myimr,myjnp,mynl,mync
     integer(i_kind) ierr,nymdp,nhmsp
     real(r_kind)    dmodel,dgsi

     stat = izero
     xx   = zero

!    Initializes this package
!    ------------------------
     call init_ ( ierr, skiptraj=skiptraj )
     if(ierr/=izero) return

!    Set file to be read
!    -------------------
     fname = fnpert
     if(present(filename)) fname = trim(filename)

#ifdef GEOS_PERT 
!    Get dims of incoming vector from input file
!    -------------------------------------------
     call inqpert_dims ( trim(fname), myimr, myjnp, mynl, mync, stat=ierr )

!    Create GCM perturbation vector
!    ------------------------------
     if ( myimr/=imr .or. myjnp/=jnp .or. mynl/=nl .or. mync<nc ) then
        stat = 89_i_kind
        if (mype==ROOT) then
           print*, 'myimr,myjnp,mynl,mync ', myimr,myjnp,mynl,mync
           print*, '  imr,  jnp,  nl,  nc ',   imr,  jnp,  nl,  nc
           print*, trim(myname_), ': Cannot handle resolution inconsistency '
        endif
        return
     else
        call prognostics_initial ( xpert )
     endif

!    Read in perturbation
!    --------------------
     nymdp = izero; nhmsp = izero
     call getpert ( trim(fname), nymdp, nhmsp, xpert, pick=.false., stat=ierr, vectype=vectype_, forceflip=.true. )
     if(ierr/=izero)then
        stat = 90_i_kind
        if(mype==ROOT) print*, trim(myname_), ': Error retrieving perturbation'
        return
     endif
     dmodel = prognostics_dotp(xpert,xpert)

     if (present(nymd_in)) then
        nymd_in = nymdp
     endif
     if (present(nhms_in)) then
        nhms_in = nhmsp
     endif

!    Convert to GSI perturbation vector
!    ----------------------------------
     if (nlon/=imr .or. nlat/=jnp ) then
        call die ( myname_,': this option is not fully implemented yet' )  ! RT: I am de-activating this for now
        if(mype==ROOT) print*, trim(myname_), ': Interpolating perturbation vector to GSI resolution: '

        call prognostics_initial ( ypert, nlon, nlat, nl, nc )

!       Interpolate input perturbation to internal resolution ...
!       ---------------------------------------------------------
        call pgcm2pgcm_ ( myimr,myjnp,mynl,xpert,  ypert, ierr )

!       ... convert GEOS-4 to GEOS-5 like perturbation and ...
!       ------------------------------------------------------
        if(vectype_==4_i_kind) call stepon_g4tog5_tl ( nymdp, nhmsp, ypert )

!       ... then convert to GSI
!       -----------------------
        call pgcm2gsi1_ ( ypert, xx, which, stat, jgradf=.true. )

        call prognostics_final ( ypert)

     else

!       Simply convert
!       --------------
        call pgcm2gsi1_ ( xpert, xx, which, stat, jgradf=.true. )

     endif
     dgsi = dot_product(xx,xx)
     if(mype==ROOT) write(6,'(2a,1p,e24.18)') trim(myname_), ': magnitude of input vector in model    space ', dmodel
     if(mype==ROOT) write(6,'(2a,1p,e24.18)') trim(myname_), ': magnitude of input vector in analysis space ', dgsi

!    Release GCM perturbation vector
!    -------------------------------
     call prognostics_final ( xpert )
#endif /* GEOS_PERT */ 

     end subroutine pgcm2gsi0_

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 601.1     !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: pgcm2gsi1_:  Convert gcm-adm/tlm vector to gsi vector
!
! !INTERFACE:

      subroutine pgcm2gsi1_ ( xpert, xx, which, stat, jgradf )

! !USES:

      implicit none

! !INPUT PARAMETERS:

      type(dyn_prog)    , intent(in   ) :: xpert  ! GCM perturbation vector 
      character(len=*)  , intent(in   ) :: which  ! adm or tlm
      logical, optional , intent(in   ) :: jgradf ! specify when input is forecast (error) gradient

! !OUTPUT PARAMETERS:

      type(state_vector), intent(inout) :: xx     ! GSI increment

      integer(i_kind)   , intent(  out) :: stat

! !DESCRIPTION: Convert GEOS-5 perturbation vector to GSI increment vector
!
! !REVISION HISTORY:
!
!  08May2007  Todling   Initial code.
!  21Sep2007  Todling   Handles for O3 and CW.
!  22Feb2008  Todling   Handle for forecast (error) gradient vector.
!
!EOP
!-----------------------------------------------------------------------

      character(len=*), parameter :: myname_ = myname//'*pgcm2gsi1_'

      real(r_kind),  allocatable, dimension(:,:,:) :: sub_u,sub_v,sub_delp,sub_q,sub_tv
      real(r_kind),  allocatable, dimension(:,:,:) :: sub_oz,sub_cw
      real(r_kind),  allocatable, dimension(:,:)   :: sub_ps

      integer(i_kind) i,j,k,ij,ijk
      integer(i_kind) ierr
      logical      scaleit

      scaleit = .true.  ! default: scale input vector as original var from G-5 GCM
      stat = izero
      if ( present(jgradf) ) then
         if(jgradf) scaleit = .false. ! input vector is a gradient, don't scale vars
      endif

!     Initializes this package
!     ------------------------
      call init_ ( ierr )
      if(ierr/=izero) return
      call check_bks()

      allocate (sub_tv  (lat2,lon2,nsig), sub_u (lat2,lon2,nsig),&
                sub_v   (lat2,lon2,nsig), sub_q (lat2,lon2,nsig),&
                sub_delp(lat2,lon2,nsig), sub_ps(lat2,lon2), stat=ierr )
      if ( ierr/=izero ) then
         stat = 91_i_kind
         if(mype==ROOT) print*, trim(myname_), ': Alloc(sub_)'
         return
      end if
      if ( nc>ione ) then
         allocate (sub_oz  (lat2,lon2,nsig), stat=ierr )
         if ( ierr/=izero ) then
            stat = 91_i_kind
            if(mype==ROOT) print*, trim(myname_), ': Alloc(sub_oz)'
            return
         end if
      endif
      if ( nc>2_i_kind ) then
         allocate (sub_cw  (lat2,lon2,nsig), stat=ierr )
         if ( ierr/=izero ) then
            stat = 91_i_kind
            if(mype==ROOT) print*, trim(myname_), ': Alloc(sub_cw)'
            return
         end if
      endif

!     Gather from GCM/Scatter to GSI subdomains
!     -----------------------------------------
      call pert2gsi_ ( xpert%u,          sub_u   , ng_d, ng_s, ierr )
      call pert2gsi_ ( xpert%v,          sub_v   , ng_s, ng_d, ierr )
      call pert2gsi_ ( xpert%pt,         sub_tv  , ng_d, ng_d, ierr )
      call pert2gsi_ ( xpert%delp,       sub_delp,izero,izero, ierr )
      call pert2gsi_ ( xpert%q(:,:,:,1), sub_q   , ng_d, ng_d, ierr )
      if (nc>ione)   call pert2gsi_ ( xpert%q(:,:,:,2), sub_oz  , ng_d, ng_d, ierr )
      if (nc>2_kind) call pert2gsi_ ( xpert%q(:,:,:,3), sub_cw  , ng_d, ng_d, ierr )
      if ( ierr/=izero ) then
         stat = 99_i_kind
         if(mype==ROOT) print*, trim(myname_), ': unfinished convertion ...'
         return
      end if

!     Calculate perturbation ps for GSI
!     ---------------------------------
      if (which == 'adm') then
         if ( scaleit ) then
            call ps2delp_ad_ ( Pa_per_kPa ) 
         else
            call delp2ps_    ( one ) 
         endif
      else if (which == 'tlm') then
         call delp2ps_    ( kPa_per_Pa ) 
      else
         call die ( myname_,': invalid option' )
      endif

!     Calculate all other perturbation for GSI
!     ----------------------------------------
      ijk=izero
      do k=1,nsig
         do j=1,lon2
            do i=1,lat2
               ijk=ijk+ione
               xx%u(ijk) = sub_u (i,j,k)
               xx%v(ijk) = sub_v (i,j,k)
               xx%t(ijk) = sub_tv(i,j,k)
               xx%q(ijk) = sub_q (i,j,k)
            enddo
         enddo
      enddo
      if ( nc>ione ) then
         ijk=izero
         do k=1,nsig
            do j=1,lon2
               do i=1,lat2
                  ijk=ijk+ione
                  xx%oz(ijk) = sub_oz (i,j,k)
               enddo
            enddo
         enddo
         if(scaleit) xx%oz(:)  = xx%oz(:) * PPMV2DU
      endif
      if ( nc>2_i_kind ) then
         ijk=izero
         do k=1,nsig
            do j=1,lon2
               do i=1,lat2
                  ijk=ijk+ione
                  xx%cw(ijk) = sub_cw (i,j,k)
               enddo
            enddo
         enddo
         xx%cw(:)  = xx%cw(:)
      endif

!     The following will be left untouched
!     ------------------------------------
!     xx%sst(:) = xx%sst(:)

      deallocate (sub_tv, sub_u, sub_v, sub_q, sub_delp, sub_ps, stat=ierr )
      if ( ierr/=izero ) then
         stat = 99_i_kind
         if(mype==ROOT) print*, trim(myname_), ': Dealloc(sub_)'
         return
      end if
      if ( nc>ione ) then
         deallocate (sub_oz, stat=ierr )
         if ( ierr/=izero ) then
            stat = 99_i_kind
            if(mype==ROOT) print*, trim(myname_), ': Dealloc(sub_oz)'
            return
         end if
      endif
      if ( nc>2_i_kind ) then
         deallocate (sub_cw, stat=ierr )
         if ( ierr/=izero ) then
            stat = 99_i_kind
            if(mype==ROOT) print*, trim(myname_), ': Dealloc(sub_cw)'
            return
         end if
      endif

      CONTAINS

      subroutine delp2ps_ ( alpha )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    delp2ps_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    alpha
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

! inverse
      implicit none

      real(r_kind), intent(in   ) :: alpha

      xx%p = zero
      do k=1,nsig
         ij=izero
         do j=1,lon2
            do i=1,lat2
               ij=ij+ione
               xx%p(ij) = xx%p(ij) + alpha * sub_delp(i,j,k)
            end do
         end do
      end do
      end subroutine delp2ps_

      subroutine ps2delp_ad_ ( alpha )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ps2delp_ad_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    alpha
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

! adm-only
      implicit none

      real(r_kind), intent(in   ) :: alpha

      real(r_kind) bkweight

      xx%p=zero
      do k=1,nsig
         bkweight = alpha * ( bk5(k) - bk5(k+ione) ) / ( bk5(1) - bk5(nsig+ione) )
         ij=izero
         do j=1,lon2
            do i=1,lat2
               ij=ij+ione
               xx%p(ij) = xx%p(ij) + bkweight * sub_delp(i,j,k)
            enddo
         enddo
      enddo
      end subroutine ps2delp_ad_

      subroutine pert2gsi_ ( fld, sub, ngd, ngs, stat_ )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    pert2gsi_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ngd,ngs
!    fld
!
!   output argument list:
!    sub
!    stat_
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
      implicit none

      integer(i_kind), intent(in   ) :: ngd, ngs
      real(r_kind)   , intent(in   ) :: fld(:,:,:)
      real(r_kind)   , intent(  out) :: sub(:,:,:)
      integer(i_kind), intent(  out) :: stat_

      character(len=*), parameter :: myname_ = myname//'*pert2gsi_'

      real(r_kind), allocatable :: work4d(:,:,:,:)   ! auxliar 4d array
      real(r_kind), allocatable :: work3d(:,:,:)     ! auxliar 3d array
      real(r_kind), allocatable :: work2d(:,:)       ! auxliar 2d array
      real(r_kind), allocatable :: work(:)
      integer(i_kind) mm1

      mm1 = mype+ione
      stat_ = izero

      allocate ( work3d(nlon,nlat,nsig), stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 91_i_kind
         if(mype==ROOT) print*, trim(myname_), ': Alloc(work3d)'
         return
      end if
      allocate ( work4d(imr,jnp,nl,1), stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 91_i_kind
         if(mype==ROOT) print*, trim(myname_), ': Alloc(work4d)'
         return
      end if
                                                                                                                           
!     Gather GCM perturbations to root processor
!     ------------------------------------------
      call mp_gather4d(fld, work4d, imr, jnp, nl, ione, jfirst, jlast, ione, nl, ngd, ngs, root)

!     Flip horizontal and vertical
!     ----------------------------
      if ( mype==ROOT ) then
         if (imr/=nlon .or. jnp/=nlat ) then
            if (which=='adm') then
               work3d = zero
               call interpack_terpv_ad ( nlon,nlat,nsig,work3d,work3d, imr,jnp,nl,work4d(:,:,:,1), ierr )
            else if (which=='tlm') then
               work3d = zero
               call interpack_terpv    ( imr,jnp,nl,work4d(:,:,:,1),  nlon,nlat,nsig,work3d, ierr )
            else
               call die ( myname_,': invalid option' )
            endif
         else
            work3d(:,:,:) = work4d(:,:,:,1)
         endif
         call SwapV_ ( work3d )
      endif

!     Swap work memory
!     ----------------
      deallocate ( work4d, stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 99_i_kind
         if(mype==ROOT) print*, trim(myname_), ': Dealloc(work4d)'
         return
      end if
      allocate ( work(itotsub), stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 91_i_kind
         if(mype==ROOT) print*, trim(myname_), ': Alloc(work)'
         return
      end if
      allocate ( work2d(lat2,lon2), stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 91_i_kind
         if(mype==ROOT) print*, trim(myname_), ': Alloc(work4d)'
         return
      end if

!     Scatter to GSI subdomains
!     -------------------------
      do k=1,nsig
         if (mype==ROOT) then
            call reorder21(work3d(:,:,k),work)
         endif
         call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
              work2d,ijn_s(mm1),mpi_rtype,root,mpi_comm_world,ierr)
         do j=1,lon2
            do i=1,lat2
               sub(i,j,k) = work2d(i,j)
            end do
         end do
      end do

!     Release work memory
!     -------------------
      deallocate ( work2d, stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 99_i_kind
         if(mype==ROOT) print*, trim(myname_), ': Dealloc(work4d)'
         return
      end if
      deallocate ( work, stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 99_i_kind
         if(mype==ROOT) print*, trim(myname_), ': delloc(work)'
         return
      end if
      deallocate ( work3d, stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 99_i_kind
         if(mype==ROOT) print*, trim(myname_), ': delloc(work3d)'
         return
      end if

      end subroutine pert2gsi_

      end subroutine pgcm2gsi1_

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 601.1     !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: gsi2pgcm0_:  Convert GSI increments to GCM perturbations
!
! !INTERFACE:

      subroutine gsi2pgcm0_ ( nymd, nhms, xx, which, stat, &
                              xp, filename )  ! optionals

! !USES:

      implicit none

! !INPUT PARAMETERS:
                                                                                                                           
      integer(i_kind)          , intent(in   ) :: nymd   ! date as in YYYYMMDD
      integer(i_kind)          , intent(in   ) :: nhms   ! time as in HHMMSS
      type(state_vector)       , intent(inout) :: xx     ! GSI increment
      character(len=*)         , intent(in   ) :: which  ! adm or tlm

      character(len=*),optional, intent(in   ) :: filename ! output filename

! !OUTPUT PARAMETERS:

      type(dyn_prog)  ,optional, intent(  out) :: xp
      integer(i_kind)          , intent(  out) :: stat

! !DESCRIPTION: Convert GSI increment vector to GEOS-5 perturbation vector
!               (as gsi2pgcm1_, but output GCM perturbation to file)
!
! !REVISION HISTORY:
!
!  08May2007  Todling   Initial code.
!  30Sep2007  Todling   Updated interface to putpert.
!
!EOP
!-----------------------------------------------------------------------

     character(len=*), parameter :: myname_ = myname//'*gsi2pgcm0_'
     type(dyn_prog) xpert

     character(len=255) fname
     integer(i_kind) ierr
     integer(i_kind) idim,jdim,kdim,i,j,k
     real(r_kind), allocatable :: ps(:,:)

     stat = izero

!    Initializes this package
!    ------------------------
     call init_ ( ierr )
     if(ierr/=izero) return
     call check_bks()

!    Create GCM perturbation vector
!    ------------------------------
     call prognostics_initial ( xpert )

!    Convert to GSI perturbation vector
!    ----------------------------------
     call gsi2pgcm1_ ( xx, xpert, which, stat )

!    Build surface pressure perturbation - output purposes
!    -----------------------------------------------------
     idim = size(xpert%delp,1)
     jdim = size(xpert%delp,2)
     kdim = size(xpert%delp,3)
     allocate ( ps(idim,jdim) )
     ps = zero
     do k=1,kdim
        do j=1,jdim
           do i=1,idim
              ps(i,j) = ps(i,j) + xpert%delp(i,j,k)
           end do
        end do
     end do

!    Write out perturbation
!    ----------------------
     mycount = mycount + ione
     if (present(filename)) then
        write(fname,'(3a)')      trim(job), '.', trim(filename)
     else
        write(fname,'(4a,i3.3)') trim(job), '.', trim(fnxgsi), '_', mycount
     endif
     call putpert ( job, nymd, nhms, xpert, fvpsasdt, nstep, &
                    ak, bk, Ts, oro, ps, fname, vectype=vectype_ )
     if(ierr/=izero)then
        stat = 90_i_kind
        if(mype==ROOT) print*, trim(myname_), ': Error retrieving perturbation'
        return
     endif

     if (present(xp)) then
        call prognostics_dup ( xpert, xp )
     endif

!    Release GCM perturbation vector
!    -------------------------------
     deallocate ( ps )
     call prognostics_final ( xpert )

     end subroutine gsi2pgcm0_

!------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 601.1    !
!------------------------------------------------------------------------
!BOP
!
! !ROUTINE: gsi2pgcm1_:  Convert GSI increments to GCM perturbations
!
! !INTERFACE:

      subroutine gsi2pgcm1_ ( xx, xpert, which, stat )

! !USES:

      implicit none

! !INPUT PARAMETERS:

      type(state_vector), intent(inout) :: xx    ! GSI increment vector
      character(len=*)  , intent(in   ) :: which ! adm or tlm

! !OUTPUT PARAMETERS:

      type(dyn_prog)    , intent(  out) :: xpert ! GCM perturbation vector
      integer(i_kind)   , intent(  out) :: stat  ! return error code

! !DESCRIPTION: Converts GSI increments in to ADM/TLM perturbations.
!
! !REVISION HISTORY:
!
!  08May2007  Todling   Initial code.
!  21Sep2007  Todling   Handles for O3 and CW.
!
!EOP
!-----------------------------------------------------------------------

      character(len=*), parameter :: myname_ = myname//'*gsi2pgcm_'

      real(r_kind), allocatable, dimension(:,:,:) :: sub_tv,sub_u,sub_v,sub_q,sub_delp
      real(r_kind), allocatable, dimension(:,:,:) :: sub_oz,sub_cw
      real(r_kind), allocatable, dimension(:,:)   :: sub_ps
      real(r_kind) factor

      integer  i,j,k,ijk,ij
      integer  ierr

      stat = izero

!     Initializes this package
!     ------------------------
      call init_ ( ierr )
      if(ierr/=izero) return

      allocate (sub_tv  (lat2,lon2,nsig), sub_u (lat2,lon2,nsig),&
                sub_v   (lat2,lon2,nsig), sub_q (lat2,lon2,nsig),&
                sub_delp(lat2,lon2,nsig), sub_ps(lat2,lon2), stat=ierr )
      if ( ierr/=izero ) then
         stat = 91_i_kind
         if(mype==ROOT) print*, trim(myname_), ': Alloc(sub_)'
         return
      end if
      if ( nc>ione ) then
         allocate (sub_oz  (lat2,lon2,nsig), stat=ierr )
         if ( ierr/=izero ) then
            stat = 91_i_kind
            if(mype==ROOT) print*, trim(myname_), ': Alloc(sub_oz)'
            return
         end if
      endif
      if ( nc>2_i_kind ) then
         allocate (sub_cw  (lat2,lon2,nsig), stat=ierr )
         if ( ierr/=izero ) then
            stat = 91_i_kind
            if(mype==ROOT) print*, trim(myname_), ': Alloc(sub_cw)'
            return
         end if
      endif

!     Fill in subdomain arrays
!     ------------------------
      ijk=izero
      do k=1,nsig
         do j=1,lon2
            do i=1,lat2
               ijk=ijk+ione
               sub_u (i,j,k) = xx%u(ijk)
               sub_v (i,j,k) = xx%v(ijk)
               sub_tv(i,j,k) = xx%t(ijk)
               sub_q (i,j,k) = xx%q(ijk)
            enddo
         enddo
      enddo
      if ( nc>ione ) then
         ijk=izero
         do k=1,nsig
            do j=1,lon2
               do i=1,lat2
                  ijk=ijk+ione
                  sub_oz(i,j,k) = xx%oz(ijk)
               enddo
            enddo
         enddo
         sub_oz = sub_oz / PPMV2DU
      endif
      if ( nc>2_i_kind ) then
         ijk=izero
         do k=1,nsig
            do j=1,lon2
               do i=1,lat2
                  ijk=ijk+ione
                  sub_cw(i,j,k) = xx%cw(ijk)
               enddo
            enddo
         enddo
      endif
      ij=izero
      do j=1,lon2
         do i=1,lat2
            sub_ps(i,j) = zero
         enddo
      enddo

!     Calculate perturbation delp
!     ---------------------------
      if (which == 'adm') then
         call delp2ps_ad_ ( kPa_per_Pa ) 
      else if (which == 'tlm') then
         call ps2delp_    ( Pa_per_kPa ) 
      else
         call die ( myname_, ': invalid option' )
      endif

!     Gather from GSI subdomains/Scatter to GCM
!     -----------------------------------------
      call gsi2pert_ ( sub_u,    xpert%u,          ng_d, ng_s, ierr )
      call gsi2pert_ ( sub_v,    xpert%v,          ng_s, ng_d, ierr )
      call gsi2pert_ ( sub_tv,   xpert%pt,         ng_d, ng_d, ierr )
      call gsi2pert_ ( sub_delp, xpert%delp,      izero,izero, ierr )
      call gsi2pert_ ( sub_q ,   xpert%q(:,:,:,1), ng_d, ng_d, ierr )
      if(nc>ione)     call gsi2pert_ ( sub_oz,   xpert%q(:,:,:,2), ng_d, ng_d, ierr )
      if(nc>2_i_kind) call gsi2pert_ ( sub_cw,   xpert%q(:,:,:,3), ng_d, ng_d, ierr )
      if ( ierr/=izero ) then
         stat = 98_i_kind
         if(mype==ROOT) print*, trim(myname_), ': unfinished convertion ...'
         return
      end if

      deallocate (sub_tv, sub_u, sub_v, sub_q, sub_delp, sub_ps, stat=ierr )
      if ( ierr/=izero ) then
         stat = 99_i_kind
         if(mype==ROOT) print*, trim(myname_), ': Dealloc(sub_)'
         return
      end if
      if ( nc>ione ) then
         deallocate (sub_oz, stat=ierr )
         if ( ierr/=izero ) then
            stat = 99_i_kind
            if(mype==ROOT) print*, trim(myname_), ': Dealloc(sub_oz)'
            return
         end if
      endif
      if ( nc>2_i_kind ) then
         deallocate (sub_cw, stat=ierr )
         if ( ierr/=izero ) then
            stat = 99_i_kind
            if(mype==ROOT) print*, trim(myname_), ': Dealloc(sub_cw)'
            return
         end if
      endif


      CONTAINS

      subroutine ps2delp_ ( alpha )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ps2delp_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    alpha
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
 
! tlm-only
      implicit none

      real(r_kind), intent(in   ) :: alpha

      real(r_kind) bkweight

      do k=1,nsig
         bkweight = alpha * ( bk5(k) - bk5(k+ione) ) / ( bk5(1) - bk5(nsig) )
         ij=izero
         do j=1,lon2
            do i=1,lat2
               ij=ij+ione
               sub_delp(i,j,k) = bkweight * xx%p(ij)
            enddo
         enddo
      enddo
      end subroutine ps2delp_

      subroutine delp2ps_ad_ ( alpha )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    delp2ps_ad_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    alpha
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

! inverse-adm
      implicit none

      real(r_kind), intent(in   ) :: alpha

      do k=1,nsig
         ij=izero
         do j=1,lon2
            do i=1,lat2
               ij=ij+ione
               sub_delp(i,j,k) = alpha * xx%p(ij)
            enddo
         enddo
      enddo
      end subroutine delp2ps_ad_

      subroutine gsi2pert_ ( sub, fld, ngd, ngs, stat_ )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsi2pert_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ngd,ngs
!    sub
!
!   output argument list:
!    fld
!    stat_
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
      implicit none

      integer(i_kind), intent(in   ) :: ngd, ngs
      real(r_kind)   , intent(in   ) :: sub(:,:,:)
      real(r_kind)   , intent(  out) :: fld(:,:,:)
      integer(i_kind), intent(  out) :: stat_

      character(len=*), parameter :: myname_ = myname//'*gsi2pert_'

      real(r_kind), allocatable :: fldsm(:,:)
      real(r_kind), allocatable :: work4d(:,:,:,:)   ! auxliar 4d array
      real(r_kind), allocatable :: work3d(:,:,:)     ! auxliar 3d array
      real(r_kind), allocatable :: work(:)

      integer(i_kind) mm1

      mm1 = mype+ione
      stat_ = izero

      allocate ( work3d(nlon,nlat,nsig), stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 91_i_kind
         if(mype==ROOT) print*, trim(myname_), ': Alloc(work3d)'
         return
      end if
      allocate ( work(max(iglobal,itotsub)), stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 91_i_kind
         if(mype==ROOT) print*, trim(myname_), ': Alloc(work)'
         return
      end if
      allocate ( fldsm(lat1*lon1,nsig), stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 91_i_kind
         if(mype==ROOT) print*, trim(myname_), ': Alloc(fldsm)'
         return
      end if

!     Strip off boundary points from subdomains
!     -----------------------------------------
      call strip(sub,fldsm,nsig)

!     Gather GSI perturbations to root processor
!     ------------------------------------------
      do k=1,nsig
         call mpi_gatherv(fldsm(1,k),ijn(mm1),mpi_rtype,&
              work,ijn,displs_g,mpi_rtype,&
              ROOT,mpi_comm_world,ierr)
         if (mype==ROOT) then
            call reorder12(work,work3d(:,:,k))
         endif
      end do

      deallocate ( fldsm, stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 99_i_kind
         if(mype==ROOT) print*, trim(myname_), ': delloc(fldsm)'
         return
      end if
      deallocate ( work, stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 99_i_kind
         if(mype==ROOT) print*, trim(myname_), ': delloc(work)'
         return
      end if

      allocate ( work4d(imr,jnp,nl,1), stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 91_i_kind
         if(mype==ROOT) print*, trim(myname_), ': Alloc(work4d)'
         return
      end if

!     Flip horizontal and vertical
!     ----------------------------
      if ( mype==ROOT ) then
         if (imr/=nlon .or. jnp/=nlat ) then
            if (which=='adm') then
               work4d = zero
               call interpack_terpv_ad ( imr,jnp,nl,work4d(:,:,:,1),work4d(:,:,:,1), nlon,nlat,nsig,work3d, ierr )
            else if (which=='tlm') then
               work4d = zero
               call interpack_terpv    ( nlon,nlat,nsig,work3d, imr,jnp,nl,work4d(:,:,:,1), ierr )
            else
               call die ( myname_,': invalid option' )
            endif
         else
            work4d(:,:,:,1) = work3d(:,:,:)
         endif
         call SwapV_ ( work4d(:,:,:,1) )
      endif

!     Scatter perturbations to GCM decomposition
!     ------------------------------------------
      call mp_scatter4d ( work4d, fld, imr, jnp, nl, ione, jfirst, jlast, ione, nl, ngd, ngs, root )

!     Swap work memory
!     ----------------
      deallocate ( work4d, stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 99_i_kind
         if(mype==ROOT) print*, trim(myname_), ': Dealloc(work4d)'
         return
      end if

      deallocate ( work3d, stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 99_i_kind
         if(mype==ROOT) print*, trim(myname_), ': delloc(work3d)'
         return
      end if

      end subroutine gsi2pert_

      end subroutine gsi2pgcm1_

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 601.1     !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: pgcm2pgcm_:  Convert gcm-adm/tlm vectors between diff resolutions
!
! !INTERFACE:

      subroutine pgcm2pgcm_ ( myimr,myjnp,mynl,ypert,  xpert, stat )

! !USES:

      implicit none

! !INPUT PARAMETERS:

      integer(i_kind), intent(in   ) :: myimr,myjnp,mynl
      type(dyn_prog) , intent(in   ) :: ypert  ! incoming GCM perturbation vector

! !OUTPUT PARAMETERS:

      type(dyn_prog) , intent(  out) :: xpert  ! interpolated GCM perturbation vector

      integer(i_kind), intent(  out) :: stat

! !DESCRIPTION: Interpolate and convert GEOS-5 perturbation vector 
!               into internal GEOS-5 perturbation vector.
!
! !REVISION HISTORY:
!
!  17Jul2007  Todling   Initial code.
!
!EOP
!-----------------------------------------------------------------------

      integer(i_kind) n,ierr

      stat = izero

      call pert2pert_ ( ng_d, ng_s, myimr,myjnp,mynl,ypert%u,           xpert%u,          ierr )
      call pert2pert_ ( ng_s, ng_d, myimr,myjnp,mynl,ypert%v,           xpert%v,          ierr )
      call pert2pert_ ( ng_d, ng_d, myimr,myjnp,mynl,ypert%pt,          xpert%pt,         ierr )
      call pert2pert_ (izero,izero, myimr,myjnp,mynl,ypert%delp,        xpert%delp,       ierr )
      do n = 1, nc
         call pert2pert_ ( ng_d, ng_d, myimr,myjnp,mynl,ypert%q(:,:,:,n),  xpert%q(:,:,:,n), ierr )
      enddo

      stat = ierr

      CONTAINS

      subroutine pert2pert_ ( ngd,ngs, myimr,myjnp,mynl,fldi,  fldo, stat_ )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    pert2pert_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ngd,ngs
!    myimr,myjnp,mynl
!    fldi
!
!   output argument list:
!    fldo
!    stat_
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
      implicit none

      integer(i_kind), intent(in   ) :: ngd,ngs
      integer(i_kind), intent(in   ) :: myimr,myjnp,mynl
      real(r_kind)   , intent(in   ) :: fldi(:,:,:)
      real(r_kind)   , intent(  out) :: fldo(:,:,:)
      integer(i_kind), intent(  out) :: stat_

      character(len=*), parameter :: myname_ = myname//'*pert2pert_'

      real(r_kind), allocatable :: work4di(:,:,:,:)   ! auxliar 4d array
      real(r_kind), allocatable :: work4do(:,:,:,:)   ! auxliar 4d array

      stat_ = izero

      allocate ( work4do(nlon,nlat,nl,1), stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 91_i_kind
         if(mype==ROOT) print*, trim(myname_), ': Alloc(work3d)'
         return
      end if
      allocate ( work4di(myimr,myjnp,mynl,1), stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 91_i_kind
         if(mype==ROOT) print*, trim(myname_), ': Alloc(work4d)'
         return
      end if
                                                                                                                           
!     Gather GCM perturbations to root processor
!     ------------------------------------------
      call mp_gather4d(fldi, work4di, myimr, myjnp, nl, ione, jfirst, jlast, ione, nl, ngd, ngs, root)

!     Interpolate perturbation to internal resolution
!     -----------------------------------------------
      if ( mype==ROOT ) then
         work4do = zero
         call interpack_terpv ( myimr,myjnp,mynl,work4di(:,:,:,1),  nlon,nlat,nl,work4do(:,:,:,1), ierr )
      endif

!     Scatter interpolated perturbations to internal vector
!     -----------------------------------------------------
      call mp_scatter4d ( work4do, fldo, nlon, nlat, nl, nc, jfirst, jlast, ione, nl, ngd, ngs, root )

!     Release work memory
!     -------------------
      deallocate ( work4di, stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 99_i_kind
         if(mype==ROOT) print*, trim(myname_), ': Dealloc(work4di)'
         return
      end if
      deallocate ( work4do, stat=ierr )
      if ( ierr/=izero ) then
         stat_ = 99_i_kind
         if(mype==ROOT) print*, trim(myname_), ': delloc(work4do)'
         return
      end if

      end subroutine pert2pert_

      end subroutine pgcm2pgcm_

#endif /* GEOS_PERT */

!------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 601.1    !
!------------------------------------------------------------------------
!BOP
!
! !ROUTINE: init_:  Initializes GEOS-5 TLM/ADM
!
! !INTERFACE:

      subroutine init_ ( stat, skiptraj )

      implicit none

! !INPUT PARAMETERS:

!     integer(i_kind)  , intent(in   ) :: nymd
!     integer(i_kind)  , intent(in   ) :: nhms
      logical, optional, intent(in   ) :: skiptraj     ! when .t., trajectory not read in
      
! !OUTPUT PARAMETERS:
  
      integer(i_kind)  , intent(  out) :: stat

! !DESCRIPTION: Initializes GEOS-5 TLM and ADM
!
! !REVISION HISTORY:
!
!  08May2007  Todling   Initial code.
!  19Nov2008  Todling   Allow upto 3 tracers.
!
!EOP
!-----------------------------------------------------------------------

      character(len=*), parameter :: myname_ = myname//'*init_'
#ifdef GEOS_PERT
      type(dyn_prog) :: prog
      integer(i_kind) m,n
#endif /* GEOS_PERT */

      stat = izero

#ifdef GEOS_PERT

!     If already initialized, there is nothing to do
!     ----------------------------------------------
      if(initialized_) return
      if(present(skiptraj)) skiptraj_ = skiptraj

!     Consistency checking between ADM/TLM and GSI dims
!     -------------------------------------------------
      if ( nc>3_i_kind ) then
         stat = 90_i_kind
         if(mype==ROOT) print*, trim(myname_), ': unacceptable number of tracers'
         return
      endif
      if ( nsig/=nl ) then
         stat = 91_i_kind
         if(mype==ROOT) print*, trim(myname_), ': inconsistent number of levels, nsig,nl: ', nsig,nl
         return
      endif

      call setfunc ( n, m, prog )

      call stepon_set ( prog, rstskip=skiptraj )

      call prognostics_final ( prog )

!     Set public time step of TL and AD models
!     ----------------------------------------
      ndtpert = pdt

      if ( .not. skiptraj_ ) then

!        Initialize dynamics trajectory handle
!        -------------------------------------
         call getstate_init ( nymd, nhms, memtrj=memtraj, verbose=verbose )
         call getstate      ( nymd, nhms )

!        Initialize physics trajectory handle
!        -------------------------------------
         call physdrv1_get_init ( nymd, nhms, memphys=memtraj, verbose=verbose )
         call physdrv1_get_all  ( nymd, nhms )

         traj_initzd_ = .true.

      endif
#else /* GEOS_PERT */

!     Set public DUMMY time step of TL and AD models
!     ----------------------------------------------
      ndtpert = r3600

#endif /* GEOS_PERT */

      initialized_ = .true.

      end subroutine init_

!------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 601.1    !
!------------------------------------------------------------------------
!BOP
!
! !ROUTINE: clean_:  Clean GEOS-5 TLM/ADM
!
! !INTERFACE:

      subroutine clean_ ( )

      implicit none

! !INPUT PARAMETERS:
      
! !OUTPUT PARAMETERS:
  

! !DESCRIPTION: Initializes GEOS-5 TLM and ADM
!
! !REVISION HISTORY:
!
!  08May2007  Todling   Initial code.
!
!EOP
!-----------------------------------------------------------------------

#ifdef GEOS_PERT
      type(dyn_prog) :: prog
#endif /* GEOS_PERT */

!     If not initialized, there is nothing to do
!     ------------------------------------------
      if(.not.initialized_) return

#ifdef GEOS_PERT

!_RT  call postfunc ( prog )

!_RT  call prognostics_final ( prog )

      if ( traj_initzd_ ) then

!        Initialize dynamics trajectory handle
!        -------------------------------------
         call getstate_clean ( )

!        Initialize physics trajectory handle
!        -------------------------------------
         call physdrv1_get_clean ( )
         traj_initzd_ = .false.

      endif

#endif /* GEOS_PERT */

      initialized_ = .false.

      end subroutine clean_

      subroutine mpp_init_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mpp_init_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
      implicit none
      integer(i_kind) ierror
      logical already_init_mpi
#ifdef GEOS_PERT
      call mp_init
#else
      call mpi_initialized(already_init_mpi,ierror)
      if(.not.already_init_mpi) call mpi_init(ierror)
#endif
      end subroutine mpp_init_

    subroutine check_bks
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    check_bks
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none
#ifdef GEOS_PERT
    integer(i_kind) k,kk 
    if(bks_checked_) return
    do k = 1, nsig+ione
       kk = nsig-k+2_i_kind
       if(abs(bk(k)-bk5(kk))>0.00001_r_kind)then
          if(mype==ROOT)then
             print*,'bk5',bk5
             print*,'bk',bk
          endif
          write(6,*)'check_bks: troubled vertical coord system'
          call stop2(126)
       endif
    enddo
#endif /* GEOS_PERT */
    bks_checked_ = .true.
    end subroutine check_bks


!-------------------------------------------------------------------------
   subroutine SwapV_(fld)
!-------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    SwapV_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    fld
!
!   output argument list:
!    fld
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

   implicit none

   real(r_kind), intent(inout) ::  fld(:,:,:)

   real(r_kind),allocatable   :: work(:,:,:)
   integer(i_kind) im, jm, km
   im   = size(fld,1)
   jm   = size(fld,2)
   km   = size(fld,3)
   allocate (work(im,jm,km))
   work = fld
   fld(:,:,km:1:-1) = work(:,:,1:km:+1)
   deallocate (work)
   end subroutine SwapV_
!-------------------------------------------------------------------------
   subroutine SwapIJK_(aij,aji)
!-------------------------------------------------------------------------
! transpose IJK-ordered array to JIK-ordered array
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    SwapIJK_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    aij
!    aji
!
!   output argument list:
!    aji
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

   implicit none

   real(r_kind),dimension(:,:,:), intent(in   ) :: aij
   real(r_kind),dimension(:,:,:), intent(inout) :: aji

   integer(i_kind) :: i,k,isz,jsz,ksz,kk
!
   isz=size(aij,1)
   jsz=size(aij,2)
   ksz=size(aij,3)
   kk=1
   do k=1,ksz
      do i=1,isz
         aji(1:jsz,i,kk)=aij(i,1:jsz,k)
      end do
      kk=kk+ione
   end do
   call SwapV_(aji)
   end subroutine SwapIJK_
  
   end module geos_pertmod
