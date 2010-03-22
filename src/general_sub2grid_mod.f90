module general_sub2grid_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    generic_sub2grid_mod  generalized sub2grid and grid2sub style routines
!   prgmmr: parrish          org: np22                date: 2010-02-05
!
! abstract: This module contains generalized sub2grid and grid2sub like routines
!             which are largely independent of other gsi routines/modules.
!             This has been created first to allow easier introduction of dual
!             resolution capability for the hybrid ensemble option.  But
!             it may be used eventually to replace most of the specialized
!             sub2grid/grid2sub routines.
!             NOTE: Since this will initially be used only for ensemble space
!             where it is not necessary to have haloes on subdomains, the
!             routine general_grid2sub_ strips off the haloes from the output,
!             while still including them internally.  They are included internally
!             since this version still uses the same computation of mpi_alltoallv
!             arguments used for existing grid2sub routine.  Also, the input for
!             general_sub2grid_ has no halo.
!             The initial use for this module will be to manipulate fields of ensemble
!             perturbations as required when running GSI with the hybrid ensemble mode
!             turned on.  In this case, the haloes are not required.
!             To make this a more useful generalized code, later add option of halo size,
!             from 0 to any value.
!
! program history log:
!   2010-02-05  parrish, initial documentation
!   2010-03-02  parrish - restore halo to size 1 and duplicate what is in current sub2grid/grid2sub.
!                         also, make sure that periodic and periodic_s are properly specified.
!                         there is a bug in existing use of periodic when running in regional mode.
!                         periodic should always be false when regional=.true.  this has been
!                         corrected in this version.
!   2010-03-11  parrish - add parameter kend_alloc to type sub2grid_info.  this is to fix problem
!                           of allocating arrays when kend_loc = kbegin_loc-1, which happens for
!                           processors not involved with sub2grid/grid2sub.
!                           to fix this, use kend_alloc = max(kend_loc,kbegin_loc) for allocation.
!
! subroutines included:
!   sub general_sub2grid_r_single  - convert from subdomains to grid for real single precision (4 byte)
!   sub general_grid2sub_r_single  - convert from grid to subdomains for real single precision (4 byte)

! Variable Definitions:
!   def sub2grid_info              - contains all information needed for general_sub2grid and general_grid2sub
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

   use kinds, only: r_kind,i_kind

   implicit none

! set default to private
   private
! set subroutines to public
   public :: general_sub2grid_r_single
   public :: general_grid2sub_r_single
   public :: general_sub2grid_r_double
   public :: general_grid2sub_r_double
   public :: general_sub2grid_create_info
   public :: general_sube2suba_r_double
   public :: general_sube2suba_r_double_ad
   public :: general_suba2sube_r_double
! set passed variables to public
   public :: sub2grid_info

   type sub2grid_info

      integer(i_kind) inner_vars      ! number of inner-most loop variables
      integer(i_kind) lat1            ! no. of lats on subdomain (no buffer)
      integer(i_kind) lon1            ! no. of lons on subdomain (no buffer)
      integer(i_kind) lat2            ! no. of lats on subdomain (buffer)
      integer(i_kind) lon2            ! no. of lons on subdomain (buffer)
      integer(i_kind) latlon11        ! no. of points on subdomain (including buffer)
      integer(i_kind) latlon1n        ! latlon11*nsig
      integer(i_kind) nlat            ! no. of latitudes
      integer(i_kind) nlon            ! no. of longitudes
      integer(i_kind) nsig            ! no. of vertical levels
      integer(i_kind) num_fields      ! total number of fields/levels
      integer(i_kind) iglobal         ! number of horizontal points on global grid
      integer(i_kind) itotsub         ! number of horizontal points of all subdomains combined
      integer(i_kind) kbegin_loc      ! starting slab index for local processor
      integer(i_kind) kend_loc        ! ending slab index for local processor
      integer(i_kind) kend_alloc      ! kend_loc can = kbegin_loc - 1, for a processor not involved.
                                      !  this causes problems with array allocation:
                                      !  to correct this, use kend_alloc=max(kend_loc,kbegin_loc)
      integer(i_kind) npe             ! total number of processors
      integer(i_kind) mype            ! local processor
      logical periodic                ! logical flag for periodic e/w domains
      logical,pointer :: periodic_s(:) => NULL()    ! logical flag for periodic e/w subdomain (all tasks)
      logical,pointer :: vector(:)     => NULL()    ! logical flag, true for vector variables
      integer(i_kind),pointer :: ilat1(:)       => NULL()    !  no. of lats for each subdomain (no buffer)
      integer(i_kind),pointer :: jlon1(:)       => NULL()    !  no. of lons for each subdomain (no buffer)
      integer(i_kind),pointer :: istart(:)      => NULL()    !  start lat of the whole array on each pe
      integer(i_kind),pointer :: jstart(:)      => NULL()    !  start lon of the whole array on each pe
      integer(i_kind),pointer :: recvcounts(:)  => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer ::  displs_g(:)   => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: rdispls(:)     => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: sendcounts(:)  => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: sdispls(:)     => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: ijn(:)         => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: ltosj(:)       => NULL()    !  lat index for reordering slab
      integer(i_kind),pointer :: ltosi(:)       => NULL()    !  lon index for reordering slab
      integer(i_kind),pointer :: recvcounts_s(:)=> NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer ::     irc_s(:)   => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer ::     ird_s(:)   => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer ::  displs_s(:)   => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: rdispls_s(:)   => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: sendcounts_s(:)=> NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: sdispls_s(:)   => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: ijn_s(:)       => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: ltosj_s(:)     => NULL()    !  lat index for reordering slab
      integer(i_kind),pointer :: ltosi_s(:)     => NULL()    !  lon index for reordering slab
      integer(i_kind),pointer :: kbegin(:)      => NULL()    !  starting slab index for each processor
      integer(i_kind),pointer :: kend(:)        => NULL()    !  ending slab index for each processor
      logical:: lallocated = .false.
    

   end type sub2grid_info

!  other declarations  ...

   contains

   subroutine general_sub2grid_create_info(s,inner_vars,nlat,nlon,nsig,num_fields,regional,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sub2grid_create_info populate info variable s
!   prgmmr: parrish          org: np22                date: 2010-02-12
!
! abstract: given dimensions of horizontal domain and various other 
!              information, obtain all required constants to allow
!              use of general_sub2grid_ and general_grid2sub_ and store them
!              in structure variable s.
!
! program history log:
!   2010-02-11  parrish, initial documentation
!   2010-03-02  parrish - add regional flag to input.  if regional=.true., 
!                           then periodic, periodic_s=.false. always.  this corrects a bug
!                           in existing code.  (never a problem, except when npe=1).
!
!   input argument list:
!     s          - structure variable, waiting for all necessary information for
!                    use with general_sub2grid and general_grid2sub.
!     inner_vars - inner index, reserved for eventually putting all ensemble members 
!                    on 1st (most rapidly varying) array index.
!     nlat       - number of horizontal grid points in "latitude" direction
!     nlon       - number of horizontal grid points in "longitude"
!     nsig       - number of vertical levels for 1 3d variable.
!     num_fields - total number of 2d fields to be processed.
!     vector     - optional logical array of length num_fields, set to true for
!                    each field which will be a vector component.
!                  if not present, s%vector = .false.
!     regional   - if true, then no periodicity in "longitude" direction
!
!   output argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use kinds, only: r_single
      use constants, only: izero,ione
      use mpimod, only: mpi_comm_world
      implicit none

      type(sub2grid_info),intent(inout) :: s
      integer(i_kind),    intent(in   ) :: inner_vars,nlat,nlon,nsig,num_fields
      logical,            intent(in   ) :: regional 
      logical,optional,   intent(in   ) :: vector(num_fields)

      integer(i_kind) i,ierror,j,k,num_loc_groups,nextra,mm1,n,ns
      integer(i_kind),allocatable:: isc_g(:),isd_g(:)

      call mpi_comm_size(mpi_comm_world,s%npe,ierror)
      call mpi_comm_rank(mpi_comm_world,s%mype,ierror)
      s%inner_vars=inner_vars
      s%nlat=nlat
      s%nlon=nlon
      s%iglobal=nlat*nlon
      s%nsig=nsig
      s%num_fields=num_fields
      if(s%lallocated) then
         deallocate(s%periodic_s,s%ilat1,s%istart,s%jlon1,s%jstart,s%kbegin,s%kend,s%ijn)
         deallocate(s%sendcounts,s%sdispls,s%recvcounts,s%rdispls)
         deallocate(s%sendcounts_s,s%sdispls_s,s%recvcounts_s,s%rdispls_s)
         deallocate(s%ltosi,s%ltosj,s%ltosi_s,s%ltosj_s)
         deallocate(s%displs_s,s%irc_s,s%ird_s)
         deallocate(s%displs_g)
         deallocate(s%vector)
         s%lallocated=.false.
      end if
      allocate(s%periodic_s(s%npe),s%jstart(s%npe),s%istart(s%npe),s%ilat1(s%npe),s%jlon1(s%npe))
      allocate(s%ijn(s%npe),s%ijn_s(s%npe))
      allocate(s%vector(num_fields))
      if(present(vector)) then
         s%vector=vector
      else
         s%vector=.false.
      end if

!      first determine subdomains
      call general_deter_subdomain_nolayout(s%npe,s%mype,s%nlat,s%nlon,regional, &
            s%periodic,s%periodic_s,s%lon1,s%lon2,s%lat1,s%lat2,s%ilat1,s%istart,s%jlon1,s%jstart)
      s%latlon11=s%lat2*s%lon2
      s%latlon1n=s%latlon11*s%nsig

      allocate(isc_g(s%npe),isd_g(s%npe),s%displs_g(s%npe),s%displs_s(s%npe),s%ird_s(s%npe),s%irc_s(s%npe))
 
      s%ijn=s%ilat1*s%jlon1
      s%ijn_s=(s%ilat1+2_i_kind)*(s%jlon1+2_i_kind)
      mm1=s%mype+ione
      do i=1,s%npe
         s%irc_s(i)=s%ijn_s(mm1)
         isc_g(i)=s%ijn(mm1)
      end do

!        obtain ltosi,ltosj
      allocate(s%ltosi(s%nlat*s%nlon),s%ltosj(s%nlat*s%nlon))
      do i=1,s%nlat*s%nlon
         s%ltosi(i)=izero
         s%ltosj(i)=izero
      end do
!                       load arrays dealing with global grids
      isd_g(1)=izero
      s%displs_g(1)=izero
      do n=1,s%npe
         if(n/=ione) then
            isd_g(n)=isd_g(n-ione)+isc_g(n-ione)
            s%displs_g(n)=s%displs_g(n-ione)+s%ijn(n-ione)
         end if
         do j=1,s%jlon1(n)
            ns=s%displs_g(n)+(j-ione)*s%ilat1(n)
            do i=1,s%ilat1(n)
               ns=ns+ione
               s%ltosi(ns)=s%istart(n)+i-ione
               s%ltosj(ns)=s%jstart(n)+j-ione
            end do
         end do
      end do

! Load arrays dealing with subdomain grids
      s%ird_s(1)=izero
      s%displs_s(1)=izero
      do n=1,s%npe
         if(n/=ione) then
            s%ird_s(n)=s%ird_s(n-ione)+s%irc_s(n-ione)
            s%displs_s(n)=s%displs_s(n-ione)+s%ijn_s(n-ione)
         end if
      end do
! set total number of points from all subdomain grids
      s%itotsub=s%displs_s(s%npe)+s%ijn_s(s%npe)

!        obtain ltosi_s,ltosj_s
      allocate(s%ltosi_s(s%itotsub),s%ltosj_s(s%itotsub))
      do i=1,s%itotsub
         s%ltosi_s(i)=izero
         s%ltosj_s(i)=izero
      end do

      if(regional)then

         do n=1,s%npe
            do j=1,s%jlon1(n)+2_i_kind
               ns=s%displs_s(n)+(j-ione)*(s%ilat1(n)+2_i_kind)
               do i=1,s%ilat1(n)+2_i_kind
                  ns=ns+ione
                  s%ltosi_s(ns)=s%istart(n)+i-2_i_kind
                  s%ltosj_s(ns)=s%jstart(n)+j-2_i_kind
                  if(s%ltosi_s(ns)==izero) s%ltosi_s(ns)=ione
                  if(s%ltosi_s(ns)==nlat+ione) s%ltosi_s(ns)=s%nlat
                  if(s%ltosj_s(ns)==izero) s%ltosj_s(ns)=ione
                  if(s%ltosj_s(ns)==nlon+ione) s%ltosj_s(ns)=s%nlon
               end do
            end do
         end do  ! end do over npe
      else
         do n=1,s%npe
            do j=1,s%jlon1(n)+2_i_kind
               ns=s%displs_s(n)+(j-ione)*(s%ilat1(n)+2_i_kind)
               do i=1,s%ilat1(n)+2_i_kind
                  ns=ns+ione
                  s%ltosi_s(ns)=s%istart(n)+i-2_i_kind
                  s%ltosj_s(ns)=s%jstart(n)+j-2_i_kind
                  if(s%ltosi_s(ns)==izero) s%ltosi_s(ns)=ione
                  if(s%ltosi_s(ns)==nlat+ione) s%ltosi_s(ns)=nlat
                  if(s%ltosj_s(ns)==izero) s%ltosj_s(ns)=nlon
                  if(s%ltosj_s(ns)==nlon+ione) s%ltosj_s(ns)=ione
               end do
            end do
         end do  ! end do over npe
      endif

      deallocate(isc_g,isd_g)

!      next, determine vertical layout:
      allocate(s%kbegin(0:s%npe),s%kend(0:s%npe-ione))
      num_loc_groups=s%num_fields/s%npe
      nextra=s%num_fields-num_loc_groups*s%npe
      s%kbegin(0)=ione
      if(nextra > izero) then
         do k=1,nextra
            s%kbegin(k)=s%kbegin(k-ione)+ione+num_loc_groups
         end do
      end if
      do k=nextra+ione,s%npe
         s%kbegin(k)=s%kbegin(k-ione)+num_loc_groups
      end do
      do k=0,s%npe-ione
         s%kend(k)=s%kbegin(k+ione)-ione
      end do
      if(s%mype == izero) then
         write(6,*)' in general_sub2grid_create_info, kbegin=',s%kbegin
         write(6,*)' in general_sub2grid_create_info, kend= ',s%kend
      end if
      s%kbegin_loc=s%kbegin(s%mype)
      s%kend_loc=s%kend(s%mype)
      s%kend_alloc=max(s%kend_loc,s%kbegin_loc)

!         get alltoallv indices for sub2grid
      allocate(s%sendcounts(0:s%npe-ione),s%sdispls(0:s%npe))
      allocate(s%recvcounts(0:s%npe-ione),s%rdispls(0:s%npe))
      s%sdispls(0)=izero
      do k=0,s%npe-ione
         s%sendcounts(k)=s%ijn(k+ione)*(s%kend_loc-s%kbegin_loc+ione)
         s%sdispls(k+ione)=s%sdispls(k)+s%sendcounts(k)
      end do
      s%rdispls(0)=izero
      do k=0,s%npe-ione
         s%recvcounts(k)=s%ijn(s%mype+ione)*(s%kend(k)-s%kbegin(k)+ione)
         s%rdispls(k+ione)=s%rdispls(k)+s%recvcounts(k)
      end do

!         get alltoallv indices for grid2sub
      allocate(s%sendcounts_s(0:s%npe-ione),s%sdispls_s(0:s%npe))
      allocate(s%recvcounts_s(0:s%npe-ione),s%rdispls_s(0:s%npe))
      s%sdispls_s(0)=izero
      do k=0,s%npe-ione
         s%sendcounts_s(k)=s%ijn_s(k+ione)*(s%kend_loc-s%kbegin_loc+ione)
         s%sdispls_s(k+ione)=s%sdispls_s(k)+s%sendcounts_s(k)
      end do
      s%rdispls_s(0)=izero
      do k=0,s%npe-ione
         s%recvcounts_s(k)=s%ijn_s(s%mype+ione)*(s%kend(k)-s%kbegin(k)+ione)
         s%rdispls_s(k+ione)=s%rdispls_s(k)+s%recvcounts_s(k)
      end do

      s%lallocated=.true.

   end subroutine general_sub2grid_create_info

   subroutine general_deter_subdomain_nolayout(npe,mype,nlat,nlon,regional, &
                    periodic,periodic_s,lon1,lon2,lat1,lat2,ilat1,istart,jlon1,jstart)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_deter_subdomain_nolayout   perform domain decomposition
!   prgmmr: weiyu yang       org: np20                date: 1998-05-14
!
! abstract: Given an array of the observation computation load and
!           the number of available mpi tasks (npe), this routine 
!           decomposes the total analysis grid into npe subdomains
!
! program history log:
!   1998-05-14  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-01  treadon - simplify algorithm
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2005-10-17  derber - rewrite routine using simpler algorithm
!   2005-10-26  treadon - correct error in 100 format text
!   2008-06-04  safford - rm unused vars
!   2008-09-05  lueken - merged ed's changes into q1fy09 code
!   2010-02-12  parrish - make copy for use in general_sub2grid_mod
!   2010-03-02  parrish - add regional flag to input.  if regional=.true., 
!                           then periodic, periodic_s=.false. always.  this corrects a bug
!                           in existing code.  (never a problem, except when npe=1).
!
!   input argument list:
!     mype      - mpi task number
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use kinds, only: r_kind,i_kind
      use constants, only: izero,ione
      implicit none

!     Declare passed variables
      integer(i_kind),intent(in   ) :: npe,mype,nlat,nlon
      logical        ,intent(in   ) :: regional
      logical        ,intent(  out) :: periodic,periodic_s(npe)
      integer(i_kind),intent(  out) :: lon1,lon2,lat1,lat2
      integer(i_kind),intent(  out) :: ilat1(npe),istart(npe),jlon1(npe),jstart(npe)

!     Declare local variables
      integer(i_kind) npts,nrnc,iinum,iileft,jrows,jleft,k,i,jjnum
      integer(i_kind) j,mm1,iicnt,ipts,jjleft
      integer(i_kind),dimension(npe+ione):: iiend,jjend,iistart
      real(r_kind):: anperpe

!************************************************************************
      periodic=.false.
      periodic_s=.false.
!     Compute number of points on full grid and target number of
!     point per mpi task (pe)
      npts=nlat*nlon
      anperpe=float(npts)/float(npe)

!     Start with square subdomains
      nrnc=sqrt(anperpe)
      iinum=nlon/nrnc
      if(iinum==izero) iinum=ione
      iicnt=nlon/iinum
      iileft=nlon-iicnt*iinum
      jrows=npe/iinum
      jleft=npe-jrows*iinum

!     Adjust subdomain boundaries
      k=izero
      istart=ione
      jstart=ione
      iistart(1)=ione
      do i=1,iinum
         ipts = iicnt
         if(i <= iileft)ipts=ipts+ione
         iiend(i)=iistart(i)+ipts-ione
         iistart(i+ione)=iiend(i)+ione
         jjnum=jrows
         if(i <= jleft)jjnum=jrows+ione
         do j=1,jjnum
            k=k+ione
            jlon1(k)=ipts
            jstart(k)= iistart(i)
            ilat1(k)=nlat/jjnum
            jjleft=nlat-ilat1(k)*jjnum
            if(j <= jjleft)ilat1(k)=ilat1(k)+ione
            if(j > ione)istart(k)=jjend(j-1)+ione
            jjend(j)=istart(k)+ilat1(k)-ione

            if (jlon1(k)==nlon.and..not.regional) then
               periodic=.true.
               periodic_s(k)=.true.
            endif
            if(mype == izero) &
                 write(6,100) k-ione,istart(k),jstart(k),ilat1(k),jlon1(k)
         end do
      end do
    100 format('general_DETER_SUBDOMAIN:  task,istart,jstart,ilat1,jlon1=',6(i6,1x))


! Set number of latitude and longitude for given subdomain
      mm1=mype+ione
      lat1=ilat1(mm1)
      lon1=jlon1(mm1)
      lat2=lat1+2_i_kind
      lon2=lon1+2_i_kind
  
      return

   end subroutine general_deter_subdomain_nolayout

   subroutine general_sub2grid_r_single(s,sub_vars,grid_vars)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sub2grid_r_single  convert from subdomains to full horizontal grid
!   prgmmr: parrish          org: np22                date: 2010-02-11
!
! abstract: generalized version of sub2grid--uses only gsi module kinds.
!              All information needed is contained in the structure variable
!              "s", instead of various modules.  This allows
!              for easy adaptation for any collection/ordering of variables
!              defined on subdomains, which need to be made available on
!              full horizontal grid for horizontal operations.
!              The structure variable is specified by subroutine general_sub2grid_setup.
!              This version works with single precision (4-byte) real variables.
!              Input sub_vars, the desired arrays on horizontal subdomains, has one
!              halo row, for now, which is filled with zero, since for ensemble use,
!              there is no need for a halo, but is easiest for now to keep it.
!              A later version will have variable number of halo rows, filled with proper values.
!
! program history log:
!   2010-02-11  parrish, initial documentation
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     sub_vars   - input grid values in vertical subdomain mode (contains one halo row)
!
!   output argument list:
!     grid_vars  - output grid values in horizontal slab mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use kinds, only: r_single,i_kind,i_long
      use constants, only: izero,ione
      use mpimod, only: mpi_comm_world,mpi_real4
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_single),     intent(in   ) :: sub_vars(s%inner_vars,s%lat2,s%lon2,s%num_fields)
      real(r_single),    intent(  out)  :: grid_vars(s%inner_vars,s%nlat,s%nlon,s%kbegin_loc:s%kend_alloc)

      real(r_single) :: sub_vars0(s%inner_vars,s%lat1,s%lon1,s%num_fields)
      real(r_single) :: work(s%inner_vars,s%itotsub*(s%kend_alloc-s%kbegin_loc+ione)) 
      integer(i_kind) iloc,iskip,i,i0,ii,j,j0,k,n,k_in,ilat,jlon,ierror
      integer(i_long) mpi_string

!    remove halo row
      do k=1,s%num_fields
         do j=2,s%lon2-1
            j0=j-1
            do i=2,s%lat2-1
               i0=i-1
               do ii=1,s%inner_vars
                  sub_vars0(ii,i0,j0,k)=sub_vars(ii,i,j,k)
               end do
            end do
         end do
      end do
      call mpi_type_contiguous(s%inner_vars,mpi_real4,mpi_string,ierror)
      call mpi_type_commit(mpi_string,ierror)

      call mpi_alltoallv(sub_vars0,s%recvcounts,s%rdispls,mpi_string, &
                        work,s%sendcounts,s%sdispls,mpi_string,mpi_comm_world,ierror)

      call mpi_type_free(mpi_string,ierror)

      k_in=s%kend_loc-s%kbegin_loc+ione


! Load temp array in desired order
      do k=s%kbegin_loc,s%kend_loc
         iskip=izero
         iloc=izero
         do n=1,s%npe
            if (n/=ione) then
               iskip=iskip+s%ijn(n-ione)*k_in
            end if
            do i=1,s%ijn(n)
               iloc=iloc+ione
               ilat=s%ltosi(iloc)
               jlon=s%ltosj(iloc)
               do ii=1,s%inner_vars
                  grid_vars(ii,ilat,jlon,k)=work(ii,i + iskip + (k-s%kbegin_loc)*s%ijn(n))
               end do
            end do
         end do
      end do

   end subroutine general_sub2grid_r_single

   subroutine general_grid2sub_r_single(s,grid_vars,sub_vars)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sub2grid  convert from subdomains to full horizontal grid
!   prgmmr: parrish          org: np22                date: 2010-02-11
!
! abstract: generalized version of grid2sub--uses only gsi module kinds.
!              All information needed is contained in the structure variable
!              "s", instead of various modules.  This allows
!              for easy adaptation for any collection/ordering of variables
!              defined on subdomains, which need to be made available on
!              full horizontal grid for horizontal operations.
!              The structure variable is specified by subroutine general_sub2grid_setup.
!              This version works with single precision (4-byte) real variables.
!              Output sub_vars, the desired arrays on horizontal subdomains, has one 
!              halo row, for now, which is filled with zero, since for ensemble use,
!              there is no need for a halo, but is easiest for now to keep it.
!              A later version will have variable number of halo rows, filled with proper values.
!
! program history log:
!   2010-02-11  parrish, initial documentation
!   2010-03-02  parrish - remove setting halo to zero in output
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     grid_vars  - input grid values in horizontal slab mode.
!
!   output argument list:
!     sub_vars   - output grid values in vertical subdomain mode
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use kinds, only: r_single,i_kind,i_long
      use constants, only: izero,ione,zero
      use mpimod, only: mpi_comm_world,mpi_real4
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_single), intent(in   )     :: grid_vars(s%inner_vars,s%nlat,s%nlon,s%kbegin_loc:s%kend_alloc)
      real(r_single),     intent(  out) :: sub_vars(s%inner_vars,s%lat2,s%lon2,s%num_fields)

      real(r_single),allocatable :: temp(:,:),work(:,:,:)
      integer(i_kind) iloc,iskip,i,ii,j,k,n,k_in,ilat,jlon,ierror
      integer(i_long) mpi_string

      allocate(temp(s%inner_vars,s%itotsub*(s%kend_alloc-s%kbegin_loc+ione)))
      allocate(work(s%inner_vars,s%itotsub,s%kbegin_loc:s%kend_alloc))
!     reorganize for eventual distribution to local domains
      work=zero    !????????????not needed??
      do k=s%kbegin_loc,s%kend_loc
         do i=1,s%itotsub
            ilat=s%ltosi_s(i)
            jlon=s%ltosj_s(i)
            do ii=1,s%inner_vars
               work(ii,i,k)=grid_vars(ii,ilat,jlon,k)
            end do
         end do
      end do

!     load temp array in order of subdomains
      temp=zero
      iloc=izero
      iskip=izero
      do n=1,s%npe
         if (n/=ione) then
            iskip=iskip+s%ijn_s(n-ione)
         end if

         do k=s%kbegin_loc,s%kend_loc
            do i=1,s%ijn_s(n)
               iloc=iloc+ione
               do ii=1,s%inner_vars
                  temp(ii,iloc)=work(ii,iskip+i,k)
               end do
            end do
         end do
      end do

!     Now load the temp array back into work
      iloc=izero
      do k=s%kbegin_loc,s%kend_loc
         do i=1,s%itotsub
            iloc=iloc+ione
            do ii=1,s%inner_vars
               work(ii,i,k)=temp(ii,iloc)
            end do
         end do
      end do
      deallocate(temp)

      call mpi_type_contiguous(s%inner_vars,mpi_real4,mpi_string,ierror)
      call mpi_type_commit(mpi_string,ierror)

      call mpi_alltoallv(work,s%sendcounts_s,s%sdispls_s,mpi_string, &
                        sub_vars,s%recvcounts_s,s%rdispls_s,mpi_string,mpi_comm_world,ierror)
      deallocate(work)
      call mpi_type_free(mpi_string,ierror)

   end subroutine general_grid2sub_r_single

   subroutine general_sub2grid_r_double(s,sub_vars,grid_vars)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sub2grid_r_double  convert from subdomains to full horizontal grid
!   prgmmr: parrish          org: np22                date: 2010-02-11
!
! abstract: generalized version of sub2grid--uses only gsi module kinds.
!              All information needed is contained in the structure variable
!              "s", instead of various modules.  This allows
!              for easy adaptation for any collection/ordering of variables
!              defined on subdomains, which need to be made available on
!              full horizontal grid for horizontal operations.
!              The structure variable is specified by subroutine general_sub2grid_setup.
!              This version works with double precision (8-byte) real variables.
!              Input sub_vars, the desired arrays on horizontal subdomains, has one
!              halo row, for now, which is filled with zero, since for ensemble use,
!              there is no need for a halo, but is easiest for now to keep it.
!              A later version will have variable number of halo rows, filled with proper values.
!
! program history log:
!   2010-02-11  parrish, initial documentation
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     sub_vars   - input grid values in vertical subdomain mode
!
!   output argument list:
!     grid_vars  - output grid values in horizontal slab mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use kinds, only: r_double,i_kind,i_long
      use constants, only: izero,ione
      use mpimod, only: mpi_comm_world,mpi_real8
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_double),     intent(in   ) :: sub_vars(s%inner_vars,s%lat2,s%lon2,s%num_fields)
      real(r_double),    intent(  out)  :: grid_vars(s%inner_vars,s%nlat,s%nlon,s%kbegin_loc:s%kend_alloc)

      real(r_double) :: sub_vars0(s%inner_vars,s%lat1,s%lon1,s%num_fields)
      real(r_double) :: work(s%inner_vars,s%itotsub*(s%kend_alloc-s%kbegin_loc+ione)) 
      integer(i_kind) iloc,iskip,i,i0,ii,j,j0,k,n,k_in,ilat,jlon,ierror
      integer(i_long) mpi_string

!    remove halo row
      do k=1,s%num_fields
         do j=2,s%lon2-1
            j0=j-1
            do i=2,s%lat2-1
               i0=i-1
               do ii=1,s%inner_vars
                  sub_vars0(ii,i0,j0,k)=sub_vars(ii,i,j,k)
               end do
            end do
         end do
      end do
      call mpi_type_contiguous(s%inner_vars,mpi_real8,mpi_string,ierror)
      call mpi_type_commit(mpi_string,ierror)

      call mpi_alltoallv(sub_vars0,s%recvcounts,s%rdispls,mpi_string, &
                        work,s%sendcounts,s%sdispls,mpi_string,mpi_comm_world,ierror)

      call mpi_type_free(mpi_string,ierror)

      k_in=s%kend_loc-s%kbegin_loc+ione


! Load temp array in desired order
      do k=s%kbegin_loc,s%kend_loc
         iskip=izero
         iloc=izero
         do n=1,s%npe
            if (n/=ione) then
               iskip=iskip+s%ijn(n-ione)*k_in
            end if
            do i=1,s%ijn(n)
               iloc=iloc+ione
               ilat=s%ltosi(iloc)
               jlon=s%ltosj(iloc)
               do ii=1,s%inner_vars
               grid_vars(ii,ilat,jlon,k)=work(ii,i + iskip + (k-s%kbegin_loc)*s%ijn(n))
               end do
            end do
         end do
      end do

   end subroutine general_sub2grid_r_double

   subroutine general_grid2sub_r_double(s,grid_vars,sub_vars)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sub2grid  convert from subdomains to full horizontal grid
!   prgmmr: parrish          org: np22                date: 2010-02-11
!
! abstract: generalized version of grid2sub--uses only gsi module kinds.
!              All information needed is contained in the structure variable
!              "s", instead of various modules.  This allows
!              for easy adaptation for any collection/ordering of variables
!              defined on subdomains, which need to be made available on
!              full horizontal grid for horizontal operations.
!              The structure variable is specified by subroutine general_sub2grid_setup.
!              This version works with double precision (8-byte) real variables.
!              Output sub_vars, the desired arrays on horizontal subdomains, has one 
!              halo row, for now, which is filled with zero, since for ensemble use,
!              there is no need for a halo, but is easiest for now to keep it.
!              A later version will have variable number of halo rows, filled with proper values.
!
! program history log:
!   2010-02-11  parrish, initial documentation
!   2010-03-02  parrish - remove setting halo to zero in output
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     grid_vars  - input grid values in horizontal slab mode.
!
!   output argument list:
!     sub_vars   - output grid values in vertical subdomain mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use kinds, only: r_double,i_kind,i_long
      use constants, only: izero,ione,zero
      use mpimod, only: mpi_comm_world,mpi_real8
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_double), intent(in   )     :: grid_vars(s%inner_vars,s%nlat,s%nlon,s%kbegin_loc:s%kend_alloc)
      real(r_double),     intent(  out) :: sub_vars(s%inner_vars,s%lat2,s%lon2,s%num_fields)

      real(r_double),allocatable :: temp(:,:),work(:,:,:)
      integer(i_kind) iloc,iskip,i,ii,j,k,n,k_in,ilat,jlon,ierror
      integer(i_long) mpi_string

      allocate(temp(s%inner_vars,s%itotsub*(s%kend_alloc-s%kbegin_loc+ione)))
      allocate(work(s%inner_vars,s%itotsub,s%kbegin_loc:s%kend_alloc))
!     reorganize for eventual distribution to local domains
      work=zero
      do k=s%kbegin_loc,s%kend_loc
         do i=1,s%itotsub
            ilat=s%ltosi_s(i)
            jlon=s%ltosj_s(i)
            do ii=1,s%inner_vars
               work(ii,i,k)=grid_vars(ii,ilat,jlon,k)
            end do
         end do
      end do

!     load temp array in order of subdomains
      temp=zero
      iloc=izero
      iskip=izero
      do n=1,s%npe
         if (n/=ione) then
            iskip=iskip+s%ijn_s(n-ione)
         end if

         do k=s%kbegin_loc,s%kend_loc
            do i=1,s%ijn_s(n)
               iloc=iloc+ione
               do ii=1,s%inner_vars
                  temp(ii,iloc)=work(ii,iskip+i,k)
               end do
            end do
         end do
      end do

!     Now load the temp array back into work
      iloc=izero
      do k=s%kbegin_loc,s%kend_loc
         do i=1,s%itotsub
            iloc=iloc+ione
            do ii=1,s%inner_vars
               work(ii,i,k)=temp(ii,iloc)
            end do
         end do
      end do
      deallocate(temp)

      call mpi_type_contiguous(s%inner_vars,mpi_real8,mpi_string,ierror)
      call mpi_type_commit(mpi_string,ierror)

      call mpi_alltoallv(work,s%sendcounts_s,s%sdispls_s,mpi_string, &
                        sub_vars,s%recvcounts_s,s%rdispls_s,mpi_string,mpi_comm_world,ierror)
      deallocate(work)
      call mpi_type_free(mpi_string,ierror)

   end subroutine general_grid2sub_r_double

   subroutine general_sube2suba_r_double(se,sa,p_e2a,sube_vars,suba_vars,regional)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sube2suba_r_double  interpolate ens grid to anl grid
!   prgmmr: parrish          org: np22                date: 2010-02-27
!
! abstract: interpolate ensemble grid variables to analysis grid variables,
!              where input and output are in the respective subdomains as defined
!              by the structure variables se and sa.
!
! program history log:
!   2010-02-27  parrish, initial documentation
!
!   input argument list:
!     se         - ensemble grid structure variable
!     sa         - analysis grid structure variable
!     p_e2a      - interpolation from ensemble to grid to analysis grid structure variable
!     sube_vars  - input ensemble grid values in ensemble subdomain mode (as defined by se)
!     regional   - true for regional grids--this code currently works only with global grids
!                     need to fix this.
!
!   output argument list:
!     suba_vars  - output analysis grid values in analysis subdomain mode (as defined by sa)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use kinds, only: r_double,i_kind,i_long
      use constants, only: izero,ione
      use egrid2agrid_mod, only: g_egrid2agrid,egrid2agrid_parm
      use mpimod, only: mpi_comm_world,mpi_real8
      implicit none

      type(sub2grid_info),   intent(in   ) :: se,sa
      type(egrid2agrid_parm),intent(in   ) :: p_e2a
      real(r_double),        intent(in   ) :: sube_vars(se%inner_vars,se%lat2,se%lon2,se%num_fields)
      real(r_double),        intent(  out) :: suba_vars(sa%inner_vars,sa%lat2,sa%lon2,sa%num_fields)
      logical,               intent(in   ) :: regional

      real(r_double),allocatable:: gride_vars(:,:),grida_vars(:,:)
      integer(i_kind) k

      allocate(gride_vars(se%inner_vars*se%nlat*se%nlon,se%kbegin_loc:se%kend_alloc))
      call general_sub2grid_r_double(se,sube_vars,gride_vars)
      allocate(grida_vars(sa%inner_vars*sa%nlat*sa%nlon,sa%kbegin_loc:sa%kend_alloc))
      if(regional) then
         write(6,*)' not ready for regional dual_res yet'
         call mpi_finalize(k)
         stop
      else
         do k=se%kbegin_loc,se%kend_loc
            call g_egrid2agrid(p_e2a,gride_vars(:,k),grida_vars(:,k),se%vector(k))
         end do
      end if
      deallocate(gride_vars)
      call general_grid2sub_r_double(sa,grida_vars,suba_vars)
      deallocate(grida_vars)

   end subroutine general_sube2suba_r_double

   subroutine general_sube2suba_r_double_ad(se,sa,p_e2a,sube_vars,suba_vars,regional)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sube2suba_r_double_ad  adjoint of interpolate ens grid to anl grid
!   prgmmr: parrish          org: np22                date: 2010-02-28
!
! abstract: adjoint of general_sube2suba_r_double.
!
! program history log:
!   2010-02-28  parrish, initial documentation
!
!   input argument list:
!     se         - ensemble grid structure variable
!     sa         - analysis grid structure variable
!     p_e2a      - interpolation from ensemble to grid to analysis grid structure variable
!     sube_vars  - input ensemble grid values in ensemble subdomain mode (as defined by se)
!     regional   - true for regional grids--this code currently works only with global grids
!                     need to fix this.
!
!   output argument list:
!     suba_vars  - output analysis grid values in analysis subdomain mode (as defined by sa)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use kinds, only: r_double,i_kind,i_long
      use constants, only: izero,ione
      use egrid2agrid_mod, only: g_egrid2agrid_ad,egrid2agrid_parm
      use mpimod, only: mpi_comm_world,mpi_real8
      implicit none

      type(sub2grid_info),   intent(in   ) :: se,sa
      type(egrid2agrid_parm),intent(in   ) :: p_e2a
      real(r_double),        intent(  out) :: sube_vars(se%inner_vars,se%lat2,se%lon2,se%num_fields)
      real(r_double),        intent(in   ) :: suba_vars(sa%inner_vars,sa%lat2,sa%lon2,sa%num_fields)
      logical,               intent(in   ) :: regional

      real(r_double),allocatable:: gride_vars(:,:),grida_vars(:,:)
      integer(i_kind) k

      allocate(grida_vars(sa%inner_vars*sa%nlat*sa%nlon,sa%kbegin_loc:sa%kend_alloc))
      call general_sub2grid_r_double(sa,suba_vars,grida_vars)
      allocate(gride_vars(se%inner_vars*se%nlat*se%nlon,se%kbegin_loc:se%kend_alloc))
      if(regional) then
         write(6,*)' not ready for regional dual_res yet'
         call mpi_finalize(k)
         stop
      else
         do k=se%kbegin_loc,se%kend_loc
            call g_egrid2agrid_ad(p_e2a,gride_vars(:,k),grida_vars(:,k),se%vector(k))
         end do
      end if
      deallocate(grida_vars)
      call general_grid2sub_r_double(se,gride_vars,sube_vars)
      deallocate(gride_vars)

   end subroutine general_sube2suba_r_double_ad

   subroutine general_suba2sube_r_double(sa,se,p_e2a,suba_vars,sube_vars,regional)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_suba2sube_r_double  smoothing interpolate anl grid to ens grid
!   prgmmr: parrish          org: np22                date: 2010-03-01
!
! abstract: smoothing interpolation from analysis grid to ensemble grid (analysis subdomain
!            input, ensemble subdomain output).
!
! program history log:
!   2010-03-01  parrish, initial documentation
!
!   input argument list:
!     sa         - ensemble grid structure variable
!     se         - analysis grid structure variable
!     p_e2a      - interpolation from ensemble to grid to analysis grid structure variable
!     suba_vars  - input analysis grid values in analysis subdomain mode (as defined by sa)
!     regional   - true for regional grids--this code currently works only with global grids
!                     need to fix this.
!
!   output argument list:
!     sube_vars  - output ensemble grid values in ensemble subdomain mode (as defined by se)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use kinds, only: r_double,i_kind,i_long
      use constants, only: izero,ione
      use egrid2agrid_mod, only: g_agrid2egrid,egrid2agrid_parm
      use mpimod, only: mpi_comm_world,mpi_real8
      implicit none

      type(sub2grid_info),   intent(in   ) :: sa,se
      type(egrid2agrid_parm),intent(in   ) :: p_e2a
      real(r_double),        intent(in   ) :: suba_vars(sa%inner_vars,sa%lat2,sa%lon2,sa%num_fields)
      real(r_double),        intent(  out) :: sube_vars(se%inner_vars,se%lat2,se%lon2,se%num_fields)
      logical,               intent(in   ) :: regional

      real(r_double),allocatable:: gride_vars(:,:),grida_vars(:,:)
      integer(i_kind) k

      allocate(grida_vars(sa%inner_vars*sa%nlat*sa%nlon,sa%kbegin_loc:sa%kend_alloc))
      call general_sub2grid_r_double(sa,suba_vars,grida_vars)
      allocate(gride_vars(se%inner_vars*se%nlat*se%nlon,se%kbegin_loc:se%kend_alloc))
      if(regional) then
         write(6,*)' not ready for regional dual_res yet'
         call mpi_finalize(k)
         stop
      else
         do k=se%kbegin_loc,se%kend_loc
            call g_agrid2egrid(p_e2a,grida_vars(:,k),gride_vars(:,k),se%vector(k))
         end do
      end if
      deallocate(grida_vars)
      call general_grid2sub_r_double(se,gride_vars,sube_vars)
      deallocate(gride_vars)

   end subroutine general_suba2sube_r_double

end module general_sub2grid_mod
