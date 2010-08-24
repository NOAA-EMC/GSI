!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  GSI_BundleMod --- GSI Bundle
!
! !INTERFACE:

module GSI_BundleMod
   
! !USES:

   use kinds, only: i_kind, r_kind,r_quad
   use constants, only: zero,zero_quad
   use m_rerank, only: rerank

   implicit none
   private

!
! !PUBLIC MEMBER FUNCTIONS:
!
   public GSI_1D
   public GSI_2D
   public GSI_3D
   public GSI_Bundle           ! Bundle
   public GSI_BundleCreate     ! Create a Bundle
   public GSI_BundleDPlevs     ! dot product w/ possible "halo"
   public GSI_BundleSum        ! dot product w/ possible "halo"
   public GSI_BundleSet        ! Set Bundle
   public GSI_BundleInquire    ! Inquire about Bundle contents
   public GSI_BundleMerge      ! Merge two Bundles
   public GSI_BundlePrint      ! Print contents of Bundle
   public GSI_BundleGetPointer ! Get pointer to variable
   public GSI_BundleGetVar     ! Get contents of variable
   public GSI_BundlePutVar     ! Put contents in variable
   public GSI_BundleUnset      ! Unset Bundle
   public GSI_BundleDestroy    ! Destroy Bundle
   public assignment(=)        ! Assign to Bundle contents
   public self_add             ! Add contents of bundles
   public self_mul             ! Add contents of bundles
   public gsi_bundlehadamard   ! Hadamard product of contents of two bundles

!  These should be moved out of the bundle soon (gridmod?)
   public GSI_Grid             ! Grid (not yet general)
   public GSI_GridCreate       ! Create a grid

! !METHOD OVERLOADING:

   interface GSI_BundleCreate        ! create bundle from ...
          module procedure create1_  !   scratch
          module procedure create2_  !   existing bundle
          module procedure create3_  !   merging two bundles
   end interface
   interface GSI_BundleSet           ! set pointer to bundle contents
          module procedure set0_
          module procedure set1_
   end interface
   interface GSI_BundleInquire            ! inquire about bundle ...
          module procedure inquire_char_  !   character contents
   end interface
   interface GSI_BundleMerge         ! merge bundles
          module procedure merge_
   end interface
   interface GSI_BundlePrint         ! print summary of bundle contents
          module procedure print_
   end interface
   interface GSI_BundleGetVar        ! get fiedl(s) from bundle
          module procedure getvar1d_ !   rank-1 field
          module procedure getvar2d_ !   rank-2 field
          module procedure getvar3d_ !   rank-3 field
   end interface
   interface GSI_BundlePutVar        ! put field(s) in bundle ...
          module procedure putvar0d_ !  assign field to constant
          module procedure putvar1d_ !  write to rank-1 content
          module procedure putvar2d_ !  write to rank-2 content
          module procedure putvar3d_ !  write to rank-3 content
   end interface
   interface GSI_BundleGetPointer    ! get pointer to field(s) in bundle
          module procedure get1_     !   single-field 
          module procedure get2_     !   many-field
          module procedure get31_    !   rank-1 explict pointer
          module procedure get32_    !   rank-2 explict pointer
          module procedure get33_    !   rank-3 explict pointer
   end interface
   interface GSI_BundleUnSet         ! nullify pointers in bundle
          module procedure unset_
   end interface
   interface GSI_BundleDestroy       ! deallocate contents of bundle
          module procedure destroy_
   end interface
   interface assignment (=)
          module procedure copy_
          module procedure assign_const_
   end interface

   interface self_add  ! What we really want here is ASSIGNMENT (+=)
          module procedure self_add_st, self_add_scal
   end interface
   interface gsi_bundlehadamard
          module procedure hadamard_upd_
   end interface
   interface gsi_bundledplevs  ! needs to be generalized to operate on bundle
          module procedure dplevs2d_
          module procedure dplevs3d_
   end interface
   interface gsi_bundlesum  ! needs to be generalized to operate on bundle
          module procedure sum2d_
          module procedure sum3d_
   end interface


! !PRIVATE TYPES:

   integer(i_kind), parameter :: MAXSTR=256

   type GSI_Grid                ! simple regular grid for now
      integer(i_kind) :: im=-1  ! dim of 1st rank
      integer(i_kind) :: jm=-1  ! dim of 2nd rank
      integer(i_kind) :: km=-1  ! dim of 3nd rank
                                          ! A more general grid would include
!!    integer(i_kind) :: ihalo  ! halo of 1st dim
!!    integer(i_kind) :: jhalo  ! halo of 2nd dim
!!    integer(i_kind) :: khalo  ! halo of 3nd dim (usually not needed)
!!    real(r_kind), pointer :: lat(:,:)   ! field of latitudes
!!    real(r_kind), pointer :: lon(:,:)   ! field of longitudes
!!    real(r_kind), pointer :: pm (:,:,:) ! field of mid-layer pressures 
!!    real(r_kind), pointer :: pe (:,:,:) ! field of edge pressures
   end type GSI_Grid

   type GSI_1D
      character(len=MAXSTR) :: shortname          ! name, e.g., 'ps'
      character(len=MAXSTR) :: longname           ! longname, e.g., 'Surface Pressure'
      character(len=MAXSTR) :: units              ! units, e.g. 'hPa'
      real(r_kind), pointer :: q(:) => null()     ! rank-1 field
   end type GSI_1D

   type GSI_2D
      character(len=MAXSTR) :: shortname
      character(len=MAXSTR) :: longname
      character(len=MAXSTR) :: units
      real(r_kind), pointer :: q(:,:) => null()   ! rank-2 field
   end type GSI_2D

   type GSI_3D
      character(len=MAXSTR) :: shortname
      character(len=MAXSTR) :: longname
      character(len=MAXSTR) :: units
      logical               :: edge               ! edge field: 3rd dim is km+1
      real(r_kind), pointer :: q(:,:,:) => null() ! rank-3 field
   end type GSI_3D

! !PUBLIC TYPES:
!
   type GSI_Bundle
      character(len=MAXSTR) :: name
!!#ifdef HAVE_ESMF
!!      type(ESMF_FieldBundle), pointer :: Bundle ! Associated ESMF bundle
!!      type(ESMF_Grid) :: grid                   ! Associated ESMF grid
!!#endif /* HAVE_ESMF */
      integer(i_kind) :: n1d=-1     ! number of 1-d variables
      integer(i_kind) :: n2d=-1     ! number of 2-d variables
      integer(i_kind) :: n3d=-1     ! number of 3-d variables
      integer(i_kind) :: NumVars=-1 ! total number of variables (n1d+n2d+n3d)
      integer(i_kind) :: ndim=-1    ! size of pointer values
      type(GSI_Grid)  :: grid 
      type(GSI_1D),    pointer :: r1(:) => null()
      type(GSI_2D),    pointer :: r2(:) => null()
      type(GSI_3D),    pointer :: r3(:) => null()
      integer(i_kind), pointer :: ival1(:)  => null()
      integer(i_kind), pointer :: ival2(:)  => null()
      integer(i_kind), pointer :: ival3(:)  => null()
      real(r_kind),    pointer :: values(:) => null()
   end type GSI_Bundle

   interface init_     ! internal procedure only - not to become public
          module procedure init1d_
          module procedure init2d_
          module procedure init3d_
   end interface
   interface copy_item_ ! internal procedure only - not to become public
          module procedure copy_item1d_
          module procedure copy_item2d_
          module procedure copy_item3d_
   end interface
   interface clean_    ! internal procedure only - not to become public
          module procedure clean1d_
          module procedure clean2d_
          module procedure clean3d_
   end interface

!
! !DESCRIPTION: This module implements the bundle structure for GSI. 
!  It is meant to be general enough to allow its use in GSI within 
!  both the control and the state vectors. Ultimately, the guess-vector of 
!  GSI could also aim at using the GSI\_Bundle as a general approach to 
!  gathering various fields needed to define the guess.
!
!  A first example of the use of GSI\_Bundle is used in the module 
!  gsi\_chemtracer\_mod.F90 that allows adding an arbitrary number of 
!  chemical constituents and species into GSI  --- with a note that 
!  only CO and CO2 are currently known by the internal GSI guess module.
!
!  The GSI\_Bundle is a collection of fields defined on a grid. By definition,
!  the bundle can only keep fields on the same grid. The concept of a GSI\_Bundle
!  is similar to that of an ESMF Bundle.
!
!  This version of GSI\_Bundle is MPI-free. This modules knows nothing about
!  the distribution of the grid. Indeed, it does not need to know. The only
!  procedure that could have an MPI support here is GSI\_BundlePrint, but for
!  now it is simple and ignorant of distributed calling codes.
!
!
! !REVISION HISTORY:
!
!  22Apr2010 Todling - initial code, based on discussion w/ Arlindo da Silva 
!                      and his f90/ESMF's SimpleBundle.
!  18Aug2010      Hu - declared GSI_1D, GSI_2D, and GSI_3D as public.
!
! !SEE ALSO:  
!           gsi_chemtracer_mod.F90
!
! !REMARKS: 
!
!  1. This module should never depend on more than the following GSI modules:
!      kinds
!      constants
!      m_rerank
!
!  2. Currently the Bundle uses a very simple (im,jm,km) grid. The grid could 
!     be generalized. In doing so, it should not be a part of the Bundle, but
!     rather an outside entity that can them be used by the Bundle; instead
!     of passing im,jm,km the routines would pass the Grid type.
!
!  3. Bundle does not accept redundancy in variable names.
!
!  4. Routines and interfaces are only written if they are needed and can
!     be tested in GSI. There is no need to create code that is not being 
!     used.
!
!  5. Not all prologues will show in "protex -s" since I have purposefully
!     placed BOC and EOC strategically to eliminate interfaces that users
!     have no need to be concerned with.
!
!  6. This module uses the following conventions:
!
!       6.a) all public procedures are named with the prefix GSI_Bundle
!
!       6.b) all public procedures must be declared via an interface
!            declaration.
! 
!       6.c) name of internal procedures end with an underscore; the 
!            corresponding public names are created via an interface with a 
!            similar name without the underscore, e.g., internal procedure 
!            print_ is made public with the name GSI_BundlePrint
!
!  7. For the time being the GSI stop2 routine is being used to kill 
!     certain error conditions. Ultimately, this module should never
!     call stop2 or be killed. All procedures should return an error 
!     code. It is up to the calling program to check on the error code
!     and abort in case of error.
!
!  8. An error messaging system has not yet been developed. Ideally,
!     this module should have its own error codes and not depend on the
!     GSI error codes.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

   character(len=*), parameter :: myname='GSI_BundleMod'
   logical, parameter :: VERBOSE_=.true.

CONTAINS

!noEOC
!............................................................................................
!_BOP
!  
! !IROUTINE:  Init1d_ --- Initialze rank-1 meta-data
!
! !INTERFACE:

 subroutine init1d_(flds,nd,names,istatus,longnames,units)

! !INPUT PARAMETERS:

 integer(i_kind), intent(in):: nd
 character(len=*),intent(in):: names(nd)
 character(len=*),OPTIONAL,intent(in):: longnames(nd)
 character(len=*),OPTIONAL,intent(in):: units(nd)

! !INPUT/OUTPUT PARAMETERS:

 type(GSI_1D),    intent(inout):: flds(nd)

! !OUTPUT PARAMETERS:

 integer(i_kind), intent(out):: istatus

! !DESCRIPTION: Initialize rank-1 meta-data
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!_EOP
!-------------------------------------------------------------------------
!noBOC
 
 integer(i_kind) i
 
 do i=1,nd
    flds(i)%shortname = trim(names(i))
    if (present(longnames)) then
        flds(i)%longname  = trim(longnames(i))
    endif
    if (present(units)) then
        flds(i)%units     = trim(units(i))
    endif
 enddo
 istatus=0

 end subroutine init1d_

 subroutine clean1d_(flds,nd,istatus)
 integer(i_kind),intent(in)   :: nd
 type(GSI_1D),   intent(inout):: flds(nd)
 integer(i_kind),intent(out)  :: istatus
 
 integer(i_kind) i
    
 do i=1,nd
    flds(i)%shortname = ""
    flds(i)%longname  = ""
    flds(i)%units     = ""
 enddo
 istatus=0

 end subroutine clean1d_
!............................................................................................
 subroutine init2d_(flds,nd,names,istatus,longnames,units)
 integer(i_kind), intent(in) :: nd
 type(GSI_2D),    intent(inout):: flds(nd)
 character(len=*),intent(in):: names(nd)
 integer(i_kind), intent(out):: istatus
 character(len=*),OPTIONAL,intent(in):: longnames(nd)
 character(len=*),OPTIONAL,intent(in):: units(nd)
 
 integer(i_kind) i
   
 do i=1,nd
    flds(i)%shortname = trim(names(i))
    if (present(longnames)) then
        flds(i)%longname  = trim(longnames(i))
    endif
    if (present(units)) then
        flds(i)%units     = trim(units(i))
    endif
 enddo
 istatus=0

 end subroutine init2d_

 subroutine clean2d_(flds,nd,istatus)
 integer(i_kind),intent(in) :: nd
 type(GSI_2D),   intent(inout):: flds(nd)
 integer(i_kind),intent(out):: istatus
 
 integer(i_kind) i
    
 do i=1,nd
    flds(i)%shortname = ""
    flds(i)%longname  = ""
    flds(i)%units     = ""
 enddo
 istatus=0

 end subroutine clean2d_
!............................................................................................
 subroutine init3d_(flds,nd,names,istatus,longnames,units)
 integer(i_kind), intent(in) :: nd
 type(GSI_3D),    intent(inout):: flds(nd)
 character(len=*),intent(in):: names(nd)
 integer(i_kind), intent(out):: istatus
 character(len=*),OPTIONAL,intent(in):: longnames(nd)
 character(len=*),OPTIONAL,intent(in):: units(nd)
 
 integer(i_kind) i
    
 do i=1,nd
    flds(i)%shortname = trim(names(i))
    if (present(longnames)) then
        flds(i)%longname  = trim(longnames(i))
    endif
    if (present(units)) then
        flds(i)%units     = trim(units(i))
    endif
 enddo
 istatus=0

 end subroutine init3d_

 subroutine clean3d_(flds,nd,istatus)
 integer(i_kind),intent(in) :: nd
 type(GSI_3D),   intent(inout):: flds(nd)
 integer(i_kind),intent(out):: istatus
 
 integer(i_kind) i
    
 do i=1,nd
    flds(i)%shortname = ""
    flds(i)%longname  = ""
    flds(i)%units     = ""
 enddo
 istatus=0

 end subroutine clean3d_

!............................................................................................
 subroutine copy_item1d_ (i1,i2,fld1,fld2,istatus)
 integer(i_kind),intent(in) :: i1(:),i2(:)
 type(GSI_1D),   intent(in) :: fld1(:)
 type(GSI_1D),   intent(inout):: fld2(:)
 integer(i_kind),intent(out) :: istatus
 integer(i_kind) i
 istatus=0
 if(size(i1)>size(i2)) then
   istatus=1
   return
 endif
 do i = 1, size(i1)
    fld2(i2(i))%shortname = fld1(i1(i))%shortname
    fld2(i2(i))%longname  = fld1(i1(i))%longname
    fld2(i2(i))%units     = fld1(i1(i))%units
    fld2(i2(i))%q         = fld1(i1(i))%q
 enddo
 end subroutine copy_item1d_
 subroutine copy_item2d_ (i1,i2,fld1,fld2,istatus)
 integer(i_kind),intent(in) :: i1(:),i2(:)
 type(GSI_2D),   intent(in) :: fld1(:)
 type(GSI_2D),   intent(inout):: fld2(:)
 integer(i_kind),intent(out) :: istatus
 integer(i_kind) i
 istatus=0
 if(size(i1)>size(i2)) then
   istatus=1
   return
 endif
 do i = 1, size(i1)
    fld2(i2(i))%shortname = fld1(i1(i))%shortname
    fld2(i2(i))%longname  = fld1(i1(i))%longname
    fld2(i2(i))%units     = fld1(i1(i))%units
    fld2(i2(i))%q         = fld1(i1(i))%q
 enddo
 end subroutine copy_item2d_
 subroutine copy_item3d_ (i1,i2,fld1,fld2,istatus)
 integer(i_kind),intent(in) :: i1(:),i2(:)
 type(GSI_3D),   intent(in) :: fld1(:)
 type(GSI_3D),   intent(inout):: fld2(:)
 integer(i_kind),intent(out) :: istatus
 integer(i_kind) i
 istatus=0
 if(size(i1)>size(i2)) then
   istatus=1
   return
 endif
 do i = 1, size(i1)
    fld2(i2(i))%shortname = fld1(i1(i))%shortname
    fld2(i2(i))%longname  = fld1(i1(i))%longname
    fld2(i2(i))%units     = fld1(i1(i))%units
    fld2(i2(i))%q         = fld1(i1(i))%q
 enddo
 end subroutine copy_item3d_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  Set0_ --- Set pointers to bundle; all vars interface
!
! !INTERFACE:
!

 subroutine set0_ ( Bundle, grid, name, istatus, &
                    names1d, names2d, names3d, edges )

! !INPUT PARAMETERS:

    type(GSI_Grid),  intent(in) :: grid
    character(len=*),intent(in) :: name  ! define name of this bundle
    character(len=*),OPTIONAL,intent(in) :: names1d(:) ! 1-d variable names
    character(len=*),OPTIONAL,intent(in) :: names2d(:) ! 2-d variable names
    character(len=*),OPTIONAL,intent(in) :: names3d(:) ! 3-d variable names
    logical,         OPTIONAL,intent(in) :: edges(:)   ! array of size(names3d)
                                                       ! indicating which are
                                                       ! edge(.t.) fields

! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle) :: Bundle         ! The Bundle

! !OUTPUT PARAMETERS:

    integer(i_kind), intent(out) :: istatus  ! return error code

! !DESCRIPTION: Set pointers to bundle (all-variable interface).
! 
! 
! !SEE ALSO: 
!           set1_
!
! !REMARKS: 
!  1. This does not allocate dimension for vectors (only set pointers).
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!  12May2010 Todling  Add handle for edges.
!  16May2010 Todling  Pass the grid instead of im,jm,km.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    character(len=*),parameter :: myname_=myname//'*set0_'

    integer(i_kind) :: im,jm,km,i,ii,nd,n1d,n2d,n3d,ndim1d,ndim2d,ndim3d,ntotal
    integer(i_kind) :: mold2(2,2), mold3(2,2,2),ndim3d1,km1,ndim3de

    n1d = -1
    n2d = -1
    n3d = -1
    Bundle%name = name
! ... need grid create for more general grids
!   copy external grid ...
    im=grid%im
    jm=grid%jm
    km=grid%km
!   ... to internal grid
    Bundle%grid%im=im
    Bundle%grid%jm=jm
    Bundle%grid%km=km
    ndim1d=im
    ndim2d=ndim1d*jm
    ndim3d=ndim2d*km
    ndim3de=ndim2d*(km+1)

!   First count vector size for ...
!   1-d arrays ...
    ntotal=0
    if (present(names1d)) then
        nd=size(names1d)
        if (nd>0) then
            n1d=nd
            allocate(Bundle%r1(n1d), stat=istatus) 
            call init_ (Bundle%r1(:),n1d,names1d,istatus)
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating bundle(init1), ', istatus
               call stop2(999)
            endif
            ntotal=ntotal+n1d*ndim1d
        endif
    endif
!   2-d arrays ...
    if (present(names2d)) then
        nd=size(names2d)
        if (nd>0) then
            n2d=nd
            allocate(Bundle%r2(n2d), stat=istatus) 
            call init_ (Bundle%r2(:),n2d,names2d,istatus)
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating bundle(init2), ', istatus
               call stop2(999)
            endif
            ntotal=ntotal+n2d*ndim2d
        endif
    endif
!   and 3-d
    if (present(names3d)) then
        nd=size(names3d)
        if (nd>0) then
            n3d=nd
            allocate(Bundle%r3(n3d), stat=istatus) 
            call init_ (Bundle%r3(:),n3d,names3d,istatus)
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating bundle(init3), ', istatus
               call stop2(999)
            endif
            if (present(edges)) then
               do i=1,n3d
                  Bundle%r3(i)%edge = edges(i)
                  if(edges(i)) then
                     ntotal=ntotal+ndim3de
                  else
                     ntotal=ntotal+ndim3d
                  endif
               enddo
            else
               do i=1,n3d
                  Bundle%r3(i)%edge = .false.
               enddo
               ntotal=ntotal+n3d*ndim3d
            endif
        endif
    endif

    if(n1d>0) allocate(Bundle%ival1(n1d),stat=istatus)
    if(n2d>0) allocate(Bundle%ival2(n2d),stat=istatus)
    if(n3d>0) allocate(Bundle%ival3(n3d),stat=istatus)
    if(istatus/=0) then
       write(6,*) myname_, ':trouble allocating bundle ivals, ', istatus
       call stop2(999)
    endif

    ii=0
    if (n3d>0) then
        do i = 1, n3d
           km1=km; ndim3d1=ndim3d
           if(Bundle%r3(i)%edge) then
              km1=km1+1
              ndim3d1=ndim3de
           endif
           Bundle%r3(i)%q => rerank(Bundle%values(ii+1:ii+ndim3d1),mold3,(/im,jm,km1/))
           Bundle%ival3(i) =  ii+1
           ii=ii+ndim3d1
        enddo
    endif
    if (n2d>0) then
        do i = 1, n2d
           Bundle%r2(i)%q => rerank(Bundle%values(ii+1:ii+ndim2d),mold2,(/im,jm/))
           Bundle%ival2(i) =  ii+1
           ii=ii+ndim2d
        enddo
    endif
    if (n1d>0) then
        do i = 1, n1d
           Bundle%r1(i)%q => Bundle%values(ii+1:ii+ndim1d)
           Bundle%ival1(i) =  ii+1
           ii=ii+ndim1d
        enddo
    endif
    if(ii==ntotal) then
       Bundle%ndim = ntotal
    else
       istatus=1
       write(6,*) myname_, ':trouble allocating bundle ivals, ', istatus
       call stop2(999)
    endif

    Bundle%NumVars=max(0,n1d)+max(0,n2d)+max(0,n3d)
    Bundle%n1d=n1d
    Bundle%n2d=n2d
    Bundle%n3d=n3d

 end subroutine set0_
!noEOC
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Set1_ --- Set pointers to bundle; 1-d interface
!
! !INTERFACE:
!
 subroutine set1_ ( Bundle, im, name, istatus, &
                    names1d )

! !INPUT PARAMETERS:

    integer(i_kind), intent(in) :: im    ! first  dimension of grid
    character(len=*),intent(in) :: name  ! define name of this bundle
    character(len=*),OPTIONAL,intent(in) :: names1d(:) ! 1-d variable names

! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle) :: Bundle         ! The Bundle

! !OUTPUT PARAMETERS:

    integer(i_kind), intent(out) :: istatus  ! return error code

! !DESCRIPTION: Set pointers to bundle (1d-variable interface).
!
!
! !SEE ALSO: 
!           set0_
!
! !REMARKS:
!  1. This does not allocate dimension for vectors (only set pointers).
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    character(len=*),parameter :: myname_=myname//'*set2_'

    integer(i_kind) :: i,ii,nd,n1d,n2d,n3d,ndim1d,ndim2d,ndim3d,ntotal
    integer(i_kind) :: mold2(2,2), mold3(2,2,2)

    n1d = -1
    Bundle%name = name
! ... need grid create for more general grids
    Bundle%grid%im=im
    ndim1d=im

!   First count vector size for ...
!   1-d arrays ...
    ntotal=0
    if (present(names1d)) then
        nd=size(names1d)
        if (nd>0) then
            n1d=nd
            allocate(Bundle%r1(n1d), stat=istatus) 
            call init_ (Bundle%r1(:),n1d,names1d,istatus)
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating bundle(init1), ', istatus
               call stop2(999)
            endif
            ntotal=ntotal+n1d*ndim1d
        endif
    endif
    Bundle%ndim = ntotal

    if(n1d>0) allocate(Bundle%ival1(n1d),stat=istatus)
    if(istatus/=0) then
       write(6,*) myname_, ':trouble allocating bundle ivals, ', istatus
       call stop2(999)
    endif

    ii=0
    if (n1d>0) then
        do i = 1, n1d
           Bundle%r1(i)%q => Bundle%values(ii+1:ii+ndim1d)
           Bundle%ival1(i) =  ii+1
           ii=ii+ndim1d
        enddo
    endif
    if(ii==ntotal) then
       Bundle%ndim = ntotal
    else
       istatus=1
       write(6,*) myname_, ':trouble allocating bundle ivals, ', istatus
       call stop2(999)
    endif

    Bundle%NumVars=max(0,n1d)
    Bundle%n1d=n1d

 end subroutine set1_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  Create1_ --- Create generel bundle from grid specification and var names
!
! !INTERFACE:
!
 subroutine create1_ ( Bundle, grid, name, istatus, &
                       names1d, names2d, names3d, edges )

    implicit none

! !INPUT PARAMETERS:

    type(GSI_Grid),  intent(in) :: grid  ! GSI grid
    character(len=*),intent(in) :: name  ! define name of this bundle
    character(len=*),OPTIONAL,intent(in) :: names1d(:) ! 1-d variable names
    character(len=*),OPTIONAL,intent(in) :: names2d(:) ! 2-d variable names
    character(len=*),OPTIONAL,intent(in) :: names3d(:) ! 3-d variable names
    logical,         OPTIONAL,intent(in) :: edges(:)   ! arrays of size(names3d)
                                                       ! indicating whose edge(.t.)


! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle) :: Bundle         ! The Bundle

! !OUTPUT PARAMETERS:

    integer(i_kind), intent(out) :: istatus  ! return error code

! !DESCRIPTION: 
!               Create bundle from grid specification and user-defined
!               variable names; allocation of memory performed here.
!
!
! !SEE ALSO: 
!           create2_, create3_
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!  03May2010 Treadon  Add (:) to Bundle%r1, %r2, %r3 when calling init_.
!  10May2010 Todling  Add handling for edge-like fields.
!  16May2010 Todling  Pass the grid instead of im,jm,km.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    character(len=*),parameter :: myname_=myname//'*create1_'

    integer(i_kind) :: i,ii,nd,n1d,n2d,n3d,ndim1d,ndim2d,ndim3d,ndim3de,ntotal
    integer(i_kind) :: im,jm,km,km1,ndim3d1
    integer(i_kind) :: mold2(2,2), mold3(2,2,2)

    n1d = -1
    n2d = -1
    n3d = -1
    Bundle%name = name
! ... need grid create for more general grids
    im=grid%im
    jm=grid%jm
    km=grid%km
    Bundle%grid%im=im
    Bundle%grid%jm=jm
    Bundle%grid%km=km
    ndim1d=im
    ndim2d=ndim1d*jm
    ndim3d=ndim2d*km
    ndim3de=ndim2d*(km+1)

!   First count vector size for ...
!   1-d arrays ...
    ntotal=0
    if (present(names1d)) then
        nd=size(names1d)
        if (nd>0) then
            n1d=nd
            allocate(Bundle%r1(n1d), stat=istatus) 
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating bundle(init1), ', istatus
               call stop2(999)
            endif
            ntotal=ntotal+n1d*ndim1d
        endif
    endif
!   2-d arrays ...
    if (present(names2d)) then
        nd=size(names2d)
        if (nd>0) then
            n2d=nd
            allocate(Bundle%r2(n2d), stat=istatus) 
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating bundle(init2), ', istatus
               call stop2(999)
            endif
            ntotal=ntotal+n2d*ndim2d
        endif
    endif
!   and 3-d
    if (present(names3d)) then
        nd=size(names3d)
        if (nd>0) then
            n3d=nd
            allocate(Bundle%r3(n3d), stat=istatus) 
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating bundle(init3), ', istatus
               call stop2(999)
            endif
            if (present(edges)) then
               do i=1,n3d
                  Bundle%r3(i)%edge = edges(i)
                  if(edges(i)) then
                     ntotal=ntotal+ndim3de
                  else
                     ntotal=ntotal+ndim3d
                  endif
               enddo
            else
               do i=1,n3d
                  Bundle%r3(i)%edge = .false.
               enddo
               ntotal=ntotal+n3d*ndim3d
            endif
        endif
    endif
    
!   Now allocate long vector
    allocate(Bundle%values(ntotal),stat=istatus)
    if(istatus/=0) then
       write(6,*) myname_, ':trouble allocating bundle values, ', istatus
       call stop2(999)
    endif

    if(n1d>0) allocate(Bundle%ival1(n1d),stat=istatus)
    if(n2d>0) allocate(Bundle%ival2(n2d),stat=istatus)
    if(n3d>0) allocate(Bundle%ival3(n3d),stat=istatus)
    if(istatus/=0) then
       write(6,*) myname_, ':trouble allocating bundle ivals, ', istatus
       call stop2(999)
    endif

    ii=0
    if (n3d>0) then
        do i = 1, n3d
           km1=km; ndim3d1=ndim3d
           if(Bundle%r3(i)%edge) then
              km1=km1+1
              ndim3d1=ndim3de
           endif
!          Bundle%r3(i)%q(1:im,1:jm,1:km) => Bundle%values(ii+1:ii+ndim3d)
           Bundle%r3(i)%q => rerank(Bundle%values(ii+1:ii+ndim3d1),mold3,(/im,jm,km1/))
           Bundle%ival3(i) =  ii+1
           ii=ii+ndim3d1
        enddo
    endif
    if (n2d>0) then
        do i = 1, n2d
!          Bundle%r2(i)%q => Bundle%values(ii+1:ii+ndim2d)
           Bundle%r2(i)%q => rerank(Bundle%values(ii+1:ii+ndim2d),mold2,(/im,jm/))
           Bundle%ival2(i) =  ii+1
           ii=ii+ndim2d
        enddo
    endif
    if (n1d>0) then
        do i = 1, n1d
           Bundle%r1(i)%q => Bundle%values(ii+1:ii+ndim1d)
           Bundle%ival1(i) =  ii+1
           ii=ii+ndim1d
        enddo
    endif
    if(ii==ntotal) then
       Bundle%ndim = ntotal
    else
       istatus=1
       write(6,*) myname_, ':trouble allocating bundle ivals, ', istatus
       call stop2(999)
    endif

    if (present(names3d)) then
        if (n3d>0) then
            call init_ (Bundle%r3(:),n3d,names3d,istatus)
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating bundle(init3), ', istatus
               call stop2(999)
            endif
        endif
    endif
    if (present(names2d)) then
        if (n2d>0) then
            call init_ (Bundle%r2(:),n2d,names2d,istatus)
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating bundle(init2), ', istatus
               call stop2(999)
            endif
        endif
    endif
    if (present(names1d)) then
        if (n1d>0) then
            call init_ (Bundle%r1(:),n1d,names1d,istatus)
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating bundle(init1), ', istatus
               call stop2(999)
            endif
        endif
    endif

    Bundle%NumVars=max(0,n1d)+max(0,n2d)+max(0,n3d)
    Bundle%n1d=n1d
    Bundle%n2d=n2d
    Bundle%n3d=n3d

    if ( redundant_(Bundle) ) then
        write(6,*) myname_, ': bundle has redundant names, aborting ...'
        call stop2(999)
    endif

  end subroutine create1_
!noEOC

!BOP
!
! !IROUTINE:  Create2_ ---  Create new bundle from an existing bundle
!
! !INTERFACE:
!
  subroutine create2_ ( NewBundle, Bundle, name, istatus )

! !INPUT PARAMETERS:

    character(len=*),intent(in)  :: name
    type(GSI_Bundle),intent(in)  :: Bundle

! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle)             :: NewBundle

! !OUTPUT PARAMETERS:

    integer,intent(out)::istatus

! !DESCRIPTION: Create new bundle from another existing bundle.
!
!
! !SEE ALSO: 
!           create1_, create3_
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!  10May2010 Todling  Update to handle edges.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

    character(len=*),parameter::myname_=myname//'*create2_'
    integer(i_kind) ::  i,k,n1d,n2d,n3d
    character(len=MAXSTR),allocatable::names1d(:),names2d(:),names3d(:)
    logical,allocatable::edges(:)

    n1d = max(0,Bundle%n1d)
    n2d = max(0,Bundle%n2d)
    n3d = max(0,Bundle%n3d)
    allocate(names1d(n1d))
    allocate(names2d(n2d))
    allocate(names3d(n3d))

    do k=1,n1d
       names1d(k)=trim(Bundle%r1(k)%shortname)
    enddo

    do k=1,n2d
       names2d(k)=trim(Bundle%r2(k)%shortname)
    enddo

    allocate(edges(n3d))
    do k=1,n3d
       names3d(k)=trim(Bundle%r3(k)%shortname)
       edges(k)=Bundle%r3(k)%edge
    enddo

    call create1_ ( NewBundle, Bundle%grid, trim(name), istatus, &
                    names1d=names1d,names2d=names2d,names3d=names3d, &
                    edges=edges )

    deallocate(edges)
    deallocate(names3d)
    deallocate(names2d)
    deallocate(names1d)

  end subroutine create2_
!noEOC
!BOP
!
! !IROUTINE:  Create3_ ---  Create new bundle from merge of two existing bundles
!
! !INTERFACE:
!
  subroutine create3_ ( MergeBundle, Bundle1, Bundle2, Name, istatus )

! !INPUT PARAMETERS:

    character(len=*),intent(in) :: name  ! define name of new bundle

! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle) :: Bundle1     ! 1st existing bunlde (must be inout)
    type(GSI_Bundle) :: Bundle2     ! 2nd existing bunlde (must be inout)
    type(GSI_Bundle) :: MergeBundle ! newly created merged bundle

! !OUTPUT PARAMETERS:

    integer(i_kind), intent(out) :: istatus  ! return error code

! !DESCRIPTION: Create new bundle from merge of two previously existing
!               bundles.
!
!
! !SEE ALSO: 
!           create1_, create2_
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!  10May2010 Todling  Update to handle edges.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

    character(len=*),parameter::myname_=myname//'*create3_'
    integer(i_kind) ::  i,k,n1d,n2d,n3d,im,jm,km
    character(len=MAXSTR),allocatable::names1d(:),names2d(:),names3d(:)
    logical,allocatable::edges(:)
    type(GSI_Grid) :: grid

    istatus=0

!   Defining the grid the following way is dangerous ...
    im = max(Bundle1%grid%im,Bundle2%grid%im) 
    jm = max(Bundle1%grid%jm,Bundle2%grid%jm)
    km = max(Bundle1%grid%km,Bundle2%grid%km)
    call GSI_GridCreate(grid,im,jm,km)

    n1d = max(0,Bundle1%n1d)+max(0,Bundle2%n1d)
    n2d = max(0,Bundle1%n2d)+max(0,Bundle2%n2d)
    n3d = max(0,Bundle1%n3d)+max(0,Bundle2%n3d)
    allocate(names1d(n1d))
    allocate(names2d(n2d))
    allocate(names3d(n3d))
    allocate(edges(n3d))

    i=0
    do k=1,Bundle1%n1d
       i=i+1
       names1d(i)=trim(Bundle1%r1(k)%shortname)
    enddo
    do k=1,Bundle2%n1d
       i=i+1
       names1d(i)=trim(Bundle2%r1(k)%shortname)
    enddo


    i=0
    do k=1,Bundle1%n2d
       i=i+1
       names2d(i)=trim(Bundle1%r2(k)%shortname)
    enddo
    do k=1,Bundle2%n2d
       i=i+1
       names2d(i)=trim(Bundle2%r2(k)%shortname)
    enddo


    i=0
    do k=1,Bundle1%n3d
       i=i+1
       names3d(i)=trim(Bundle1%r3(k)%shortname)
       edges  (i)=Bundle1%r3(k)%edge
    enddo
    do k=1,Bundle2%n3d
       i=i+1
       names3d(i)=trim(Bundle2%r3(k)%shortname)
       edges  (i)=Bundle2%r3(k)%edge
    enddo

    call create1_ ( MergeBundle, grid, name, istatus, &
                    names1d=names1d, names2d=names2d, names3d=names3d, &
                    edges=edges )

    if ( redundant_(MergeBundle) ) then
        print*, MergeBundle%n1d
        print*, MergeBundle%n2d
        print*, MergeBundle%n3d
        print*, MergeBundle%ndim
        print*, MergeBundle%numvars
        write(6,*) myname_, ': merge bundle has redundant names, aborting ...'
        call stop2(999)
    endif

    deallocate(edges)
    deallocate(names3d)
    deallocate(names2d)
    deallocate(names1d)

  end subroutine create3_
!noEOC

!............................................................................................
!BOP
!
! !IROUTINE:  Get0_ ---  Get pointer for a field in bundle
!
! !INTERFACE:
!
  subroutine get0_ ( Bundle, fldname, ipnt, istatus, irank, ival )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),intent(in) :: Bundle
    character(len=*),intent(in) :: fldname        ! required field name

! !OUTPUT PARAMETERS:

    integer(i_kind),intent(out) :: ipnt           ! actual pointer to individual field
    integer(i_kind),intent(out) :: istatus        ! status error code
    integer(i_kind),intent(out) :: irank          ! field rank (e.g., 1, or 2, or 3)
    integer(i_kind),intent(out) :: ival           ! optional pointer to long vector form

! !DESCRIPTION: Retrieve pointer for required field
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!  07Jul2010 Todling  Fixed interface (no optionals, per Guo's suggestion) to
!                     avoid problem found Kokron of referencing undef variables
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: i, n1d, n2d, n3d, irank_

    istatus=0
    n1d = Bundle%n1d
    n2d = Bundle%n2d
    n3d = Bundle%n3d
    ipnt=-1; irank=-1; ival=-1
    do i=1,n1d
       if (trim(fldname).eq.trim(Bundle%r1(i)%shortname)) then
          ipnt=i
          irank=1
          ival=Bundle%ival1(i)
          return
       endif
    enddo
    do i=1,n2d
       if (trim(fldname).eq.trim(Bundle%r2(i)%shortname)) then
          ipnt=i
          irank=2
          ival=Bundle%ival2(i)
          return
       endif
    enddo
    do i=1,n3d
       if (trim(fldname).eq.trim(Bundle%r3(i)%shortname)) then
          ipnt=i
          irank=3
          ival=Bundle%ival3(i)
          return
       endif
    enddo

    if(ipnt<0) istatus=1
  end subroutine get0_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  Get1_ ---  Get pointer for a field in bundle
!
! !INTERFACE:
!
  subroutine get1_ ( Bundle, fldname, ipnt, istatus, irank, ival )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),intent(in) :: Bundle
    character(len=*),intent(in) :: fldname        ! required field name

! !OUTPUT PARAMETERS:

    integer(i_kind),intent(out) :: ipnt           ! actual pointer to individual field
    integer(i_kind),intent(out) :: istatus        ! status error code
    integer(i_kind),OPTIONAL,intent(out) :: irank ! field rank (e.g., 1, or 2, or 3)
    integer(i_kind),OPTIONAL,intent(out) :: ival  ! optional pointer to long vector form

! !DESCRIPTION: Retrieve pointer for required field
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!  07Jul2010 Todling  Use call to fixed-interface get0_
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: i,nflds
    integer(i_kind) :: irank_
    integer(i_kind) :: ival_

    istatus=0
    call get0_ ( Bundle, fldname, ipnt, istatus, irank_, ival_ )
    if(present(irank)) then
       irank=irank_
    endif
    if(present(ival)) then
       ival=ival_
    endif

  end subroutine get1_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  Get2_ ---  Get pointers for require fields in bundle
!
! !INTERFACE:
  subroutine get2_ ( Bundle, fldnames, ipnts, istatus, iranks, ivals )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),intent(in) :: Bundle   ! the Bundle
    character(len=*),intent(in) :: fldnames(:)    ! list with field names

! !OUTPUT PARAMETERS:

    integer(i_kind),intent(out) :: ipnts(:)          ! actual pointer to individual field
    integer(i_kind),intent(out) :: istatus           ! status error code
    integer(i_kind),OPTIONAL,intent(out) :: iranks(:)! fields rank (e.g., 1, or 2, or 3)
    integer(i_kind),OPTIONAL,intent(out) :: ivals(:) ! optional pointers to long vector form

! !DESCRIPTION: Retrieve pointers for required fields.
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!  07Jul2010 Todling  Use call to fixed-interface get0_
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: i,nflds
    integer(i_kind),allocatable,dimension(:) :: iranks_
    integer(i_kind),allocatable,dimension(:) :: ivals_

    istatus=0
    nflds = size(fldnames)
    allocate(iranks_(nflds),ivals_(nflds))
    do i=1,nflds
       call get0_ ( Bundle, fldnames(i), ipnts(i), istatus, iranks_(i), ivals_(i) )
    enddo
    if(present(iranks)) then
       iranks=iranks_
    endif
    if(present(ivals)) then
       ivals=ivals_
    endif
    deallocate(iranks_,ivals_)

  end subroutine get2_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  Get31_ ---  Get pointer to rank-1 field
!
! !INTERFACE:
  subroutine get31_ ( Bundle, fldname, pntr, istatus )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),target,intent(in) :: Bundle   ! the Bundle
    character(len=*),intent(in) :: fldname  ! name of field

! !OUTPUT PARAMETERS:

    real(r_kind),pointer,intent(out) :: pntr(:)  ! actual pointer to individual field
    integer(i_kind),     intent(out) :: istatus  ! status error code

! !DESCRIPTION: Retrieve pointer to specific rank-1 field.
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!  13May2010 Todling  Also return rank-N into rank-1
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: i,irank,ipnt,ival,nsz

    istatus=0
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank, ival=ival )
    if (istatus==0) then
        select case (irank)
          case(1)
             pntr => Bundle%r1(ipnt)%q
          case(2)
!            pntr => rerank(Bundle%r2(ipnt)%q)
             nsz=size(Bundle%r2(ipnt)%q)
             pntr => Bundle%values(ival:ival+nsz)
          case(3)
!            pntr => rerank(Bundle%r3(ipnt)%q)
             nsz=size(Bundle%r3(ipnt)%q)
             pntr => Bundle%values(ival:ival+nsz)
          case default
             istatus=1
          end select
    else 
        istatus=1
    endif

  end subroutine get31_
!noEOC
!BOP
!
! !IROUTINE:  Get32_ ---  Get pointer to rank-2 field
!
!
! !INTERFACE:
  subroutine get32_ ( Bundle, fldname, pntr, istatus )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),intent(in) :: Bundle   ! the Bundle
    character(len=*),intent(in) :: fldname  ! name of field

! !OUTPUT PARAMETERS:

    real(r_kind),pointer,intent(out) :: pntr(:,:)  ! actual pointer to individual field
    integer(i_kind),intent(out) :: istatus  ! status error code

! !DESCRIPTION: Retrieve pointer to specific rank-2 field.
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: i,irank,ipnt

    istatus=0
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if (istatus==0.and.irank==2) then
        pntr => Bundle%r2(ipnt)%q
    else
        istatus=1
    endif

  end subroutine get32_
!noEOC
!BOP
!
! !IROUTINE:  Get33_ ---  Get pointer to rank-3 field
!
! !INTERFACE:
  subroutine get33_ ( Bundle, fldname, pntr, istatus )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),intent(in) :: Bundle   ! the Bundle
    character(len=*),intent(in) :: fldname  ! name of field

! !OUTPUT PARAMETERS:

    real(r_kind),pointer,intent(out) :: pntr(:,:,:)  ! actual pointer to individual field
    integer(i_kind),intent(out) :: istatus  ! status error code

! !DESCRIPTION: Retrieve pointer to specific rank-3 field.
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: i,irank,ipnt

    istatus=0
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if (istatus==0.and.irank==3) then
        pntr => Bundle%r3(ipnt)%q
    else
        istatus=1
    endif

  end subroutine get33_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  PutVar0_ ---  Set request field to a constant value
!
! !INTERFACE:

  subroutine putvar0d_ ( Bundle, fldname, cnst, istatus )
    
! !INPUT PARAMETERS:

    character(len=*),intent(in) :: fldname          ! name of field
    real(r_kind),    intent(in) :: cnst             ! constant value

! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle),intent(inout) :: Bundle  ! the Bundle

! !OUTPUT PARAMETERS:

    integer(i_kind),intent(out) :: istatus          ! status error code

! !DESCRIPTION: Set user-specified field in bundle to a constant value. 
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: n,irank,ipnt,im,jm,km

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if(istatus/=0) return

!   retrieve variable
    if( irank==1 ) then
        Bundle%r1(ipnt)%q = cnst
    endif
    if( irank==2 ) then
        Bundle%r2(ipnt)%q = cnst
    endif
    if( irank==3 ) then
        Bundle%r3(ipnt)%q = cnst
    endif

  end subroutine putvar0d_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  PutVar1d_ ---  Set request field to given input field values; 1d to Nd
!
! !INTERFACE:

  subroutine putvar1d_ ( Bundle, fldname, fld, istatus )


! !INPUT PARAMETERS:

    character(len=*),intent(in) :: fldname          ! field name
    real(r_kind),    intent(in) :: fld(:)           ! input field values

! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle),intent(inout) :: Bundle  ! the Bundle

! !OUTPUT PARAMETERS:

    integer(i_kind),intent(out) :: istatus          ! status error code
     
! !DESCRIPTION: Set user-specified field in bundle the given input field. 
!               rank-1 input to rank-N output.
!
!
! !REMARKS: 
!   1. This routine also allows overwritting a 2d-/3d-array in bundle
!      with user-specified field.
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    integer(i_kind) :: n,irank,ipnt,im,jm,km

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank )
    if(istatus/=0) return

    im=Bundle%grid%im
    jm=Bundle%grid%jm
    km=Bundle%grid%km

!   retrieve variable
    if( irank==1 ) then
        Bundle%r1(ipnt)%q = fld
    endif
    if( irank==2 ) then
        Bundle%r2(ipnt)%q = reshape(fld,(/im,jm/))
    endif
    if( irank==3 ) then
        Bundle%r3(ipnt)%q = reshape(fld,(/im,jm,km/))
    endif

  end subroutine putvar1d_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  PutVar2d_ ---  Set request field to given input field values; 2d to 2d
!
! !INTERFACE:
  subroutine putvar2d_ ( Bundle, fldname, fld, istatus )
    
! !INPUT PARAMETERS:
    character(len=*),intent(in) :: fldname
    real(r_kind),    intent(in) :: fld(:,:)

! !INPUT/OUTPUT PARAMETERS:
    type(GSI_Bundle),intent(inout) :: Bundle

! !OUTPUT PARAMETERS:
    integer(i_kind),intent(out) :: istatus

! !DESCRIPTION: Set user-specified field in bundle the given input field. 
!               2d-input to 2d output.
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: n,irank,ipnt,im,jm,km

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if(istatus/=0) return

!   retrieve variable
    if( irank==2 ) then
        Bundle%r2(ipnt)%q = fld
    endif

  end subroutine putvar2d_
  subroutine putvar3d_ ( Bundle, fldname, fld, istatus )
! This routine also allows putting a 1d-array into a 2d-/3d-array
    
    type(GSI_Bundle),intent(inout) :: Bundle
    character(len=*),intent(in) :: fldname
    real(r_kind),    intent(in) :: fld(:,:,:)
    integer(i_kind),intent(out) :: istatus
    
    integer(i_kind) :: n,irank,ipnt

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if(istatus/=0) return

!   retrieve variable
    if( irank==3 ) then
        Bundle%r3(ipnt)%q = fld
    endif

  end subroutine putvar3d_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  GetVar2d_ ---  Retrieve request field from bundle; Nd to 1d
!
! !INTERFACE:

  subroutine getvar1d_ ( Bundle, fldname, fld, istatus )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),intent(in) :: Bundle   ! the Bundle
    character(len=*),intent(in) :: fldname        ! request field name

! !INPUT/OUTPUT PARAMETERS:
    real(r_kind),    intent(inout) :: fld(:)      ! request field values

! !OUTPUT PARAMETERS:
    integer(i_kind),intent(out) :: istatus        ! status error code

! !DESCRIPTION: Retrieve request field from bundle and return as 1d-array.
!               Nd-input to 1d output.
!
!
! !REMARKS: 
!   1. This routine also allows retrieving a 2d-/3d-array in bundle
!      into the 1d output array.
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: n,irank,ipnt

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if(istatus/=0) return

!   retrieve variable
    select case (irank)
      case(1)
        fld = Bundle%r1(ipnt)%q
      case(2)
        n=size(Bundle%r2(ipnt)%q)
        fld = reshape(Bundle%r2(ipnt)%q,(/n/))
      case(3)
        n=size(Bundle%r3(ipnt)%q)
        fld = reshape(Bundle%r3(ipnt)%q,(/n/))
      case default
        istatus=1
    end select

  end subroutine getvar1d_
  subroutine getvar2d_ ( Bundle, fldname, fld, istatus )
    
    type(GSI_Bundle),intent(in) :: Bundle
    character(len=*),intent(in) :: fldname
    real(r_kind),    intent(inout) :: fld(:,:)
    integer(i_kind),intent(out) :: istatus
    
    integer(i_kind) :: irank,ipnt

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if(istatus/=0) return

!   retrieve variable
    fld = Bundle%r2(ipnt)%q

  end subroutine getvar2d_
  subroutine getvar3d_ ( Bundle, fldname, fld, istatus )
    
    type(GSI_Bundle),intent(in) :: Bundle
    character(len=*),intent(in) :: fldname
    real(r_kind),    intent(inout) :: fld(:,:,:)
    integer(i_kind),intent(out) :: istatus
   
    integer(i_kind) :: irank,ipnt

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank )
    if(istatus/=0) return

!   retrieve variable
    fld = Bundle%r3(ipnt)%q

  end subroutine getvar3d_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE: Inquire_Char_ ---  Inquire about character-type meta-data
!
! !INTERFACE:

  subroutine inquire_char_ ( Bundle, what, vars, istatus )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),intent(in)  :: Bundle    ! the Bundle
    character(len=*),intent(in)  :: what      ! type of inquire (shortname,longname,etc)

! !OUTPUT PARAMETERS:

    character(len=*),intent(inout) :: vars(:) ! variable names/units
    integer(i_kind), intent(out)   :: istatus
    
! !DESCRIPTION: Inquire about (character-type) meta-data-like variables
!               in bundle.
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

    integer(i_kind) :: i,ii

    if(size(vars)<Bundle%NumVars) then
       istatus=1
       return
    endif
    istatus=1
    if(trim(what)=='shortnames') then
       ii=0
       do i=1,Bundle%n1d
          ii=ii+1
          vars(ii)=Bundle%r1(i)%shortname
       enddo
       do i=1,Bundle%n2d
          ii=ii+1
          vars(ii)=Bundle%r2(i)%shortname
       enddo
       do i=1,Bundle%n3d
          ii=ii+1
          vars(ii)=Bundle%r3(i)%shortname
       enddo
       istatus=0
    endif
    if(trim(what)=='longnames') then
       ii=0
       do i=1,Bundle%n1d
          ii=ii+1
          vars(ii)=Bundle%r1(i)%longname
       enddo
       do i=1,Bundle%n2d
          ii=ii+1
          vars(ii)=Bundle%r2(i)%longname
       enddo
       do i=1,Bundle%n3d
          ii=ii+1
          vars(ii)=Bundle%r3(i)%longname
       enddo
       istatus=0
    endif
    if(trim(what)=='units') then
       ii=0
       do i=1,Bundle%n1d
          ii=ii+1
          vars(ii)=Bundle%r1(i)%units
       enddo
       do i=1,Bundle%n2d
          ii=ii+1
          vars(ii)=Bundle%r2(i)%units
       enddo
       do i=1,Bundle%n3d
          ii=ii+1
          vars(ii)=Bundle%r3(i)%units
       enddo
       istatus=0
    endif

  end subroutine inquire_char_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE: Merge_ ---  Merge two existing Bundles into new Bundle
!
! !INTERFACE:

  subroutine merge_ ( MergeBundle, Bundle1, Bundle2, NewName, istatus )

! !INPUT PARAMETERS:

    character(len=*),intent(in) :: NewName

! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle),intent(inout) :: Bundle1
    type(GSI_Bundle),intent(inout) :: Bundle2
    type(GSI_Bundle) :: MergeBundle

! !OUTPUT PARAMETERS:

    integer(i_kind),intent(out) :: istatus

! !DESCRIPTION: Merge two existing Bundles into new Bundle.
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

    integer(i_kind) :: ie, is, i, n1d, n2d, n3d
    integer(i_kind),allocatable:: idi(:),ido(:)

!   Check for redundancy in bundles
!   to be done ....

    call create3_ ( MergeBundle, Bundle1, Bundle2, Newname, istatus )
              if(istatus/=0)return

    n1d=MergeBundle%n1d
    n2d=MergeBundle%n2d
    n3d=MergeBundle%n3d
    istatus=0

!   Handle 1d-part of bundles
    if(n1d>0) then
       is=1
       ie=max(0,Bundle1%n1d)
       if (ie>0) then
          allocate(idi(Bundle1%n1d),ido(Bundle1%n1d))
          idi=(/(i,i=1,Bundle1%n1d)/)
          ido=(/(i,i=is,ie)/)
          call copy_item_ (idi,ido,Bundle1%r1(:),MergeBundle%r1(:),istatus)
              if(istatus/=0)return
          deallocate(idi,ido)
       endif
       is=ie+1
       ie=n1d
       if (ie>=is) then
          allocate(idi(Bundle2%n1d),ido(Bundle2%n1d))
          idi=(/(i,i=1,Bundle2%n1d)/)
          ido=(/(i,i=is,ie)/)
          call copy_item_ (idi,ido,Bundle2%r1(:),MergeBundle%r1(:),istatus)
              if(istatus/=0)return
          deallocate(idi,ido)
       endif
    endif

!   Handle 2d-part of bundles
    if(n2d>0) then
       is=1
       ie=max(0,Bundle1%n2d)
       if (ie>0) then
          allocate(idi(Bundle1%n2d),ido(Bundle1%n2d))
          idi=(/(i,i=1,Bundle1%n2d)/)
          ido=(/(i,i=is,ie)/)
          call copy_item_ (idi,ido,Bundle1%r2(:),MergeBundle%r2(:),istatus)
              if(istatus/=0)return
          deallocate(idi,ido)
       endif
       is=ie+1
       ie=n2d
       if (ie>=is) then
          allocate(idi(Bundle2%n2d),ido(Bundle2%n2d))
          idi=(/(i,i=1,Bundle2%n2d)/)
          ido=(/(i,i=is,ie)/)
          call copy_item_ (idi,ido,Bundle2%r2(:),MergeBundle%r2(:),istatus)
              if(istatus/=0)return
          deallocate(idi,ido)
       endif
    endif

!   Handle 3d-part of bundles
    if(n3d>0) then
       is=1
       ie=max(0,Bundle1%n3d)
       if (ie>0) then
          allocate(idi(Bundle1%n3d),ido(Bundle1%n3d))
          idi=(/(i,i=1,Bundle1%n3d)/)
          ido=(/(i,i=is,ie)/)
          call copy_item_ (idi,ido,Bundle1%r3(:),MergeBundle%r3(:),istatus)
              if(istatus/=0)return
          deallocate(idi,ido)
       endif
       is=ie+1
       ie=n3d
       if (ie>=is) then
          allocate(idi(Bundle2%n3d),ido(Bundle2%n3d))
          idi=(/(i,i=1,Bundle2%n3d)/)
          ido=(/(i,i=is,ie)/)
          call copy_item_ (idi,ido,Bundle2%r3(:),MergeBundle%r3(:),istatus)
              if(istatus/=0)return
          deallocate(idi,ido)
       endif
    endif
    if(verbose_) print*, 'complete merge'

  end subroutine merge_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE: Copy_ --- Copy one Bundle into another 
!
! !INTERFACE:

  subroutine copy_(Bundo,Bundi)

! !USES:

  implicit none

! !INPUT PARAMETERS:

  type(GSI_Bundle), intent(in   ) :: bundi

! !INPUT/OUTPUT PARAMETERS:

  type(GSI_Bundle), intent(inout) :: bundo

! !DESCRIPTION: Copy contents of one bundle into another.
!
!
! !REVISION HISTORY:
!
!       2007 Tremolet Initial code.
!  27Apr2010 Todling  Adapted from control-vector.
!  14May2010 Treadon  Bug fix in copying names to new bundle
!
!EOP
!-------------------------------------------------------------------------
!noBOC

  character(len=*),parameter::myname_='copy_'
  integer(i_kind) :: ii,istatus
  logical :: samedim

  samedim = bundo%ndim==bundi%ndim.and.&
            bundo%n1d ==bundi%n1d .and.&
            bundo%n2d ==bundi%n2d .and.&
            bundo%n3d ==bundi%n3d
  if (.not.samedim) then
     write(6,*)trim(myname_),': error length',bundi%ndim,bundo%ndim,&
                                              bundi%n1d,bundo%n1d,&
                                              bundi%n2d,bundo%n2d,&
                                              bundi%n3d,bundo%n3d
     call stop2(999)
  end if

  if(bundi%n1d>0) then
     bundo%r1%shortname=bundi%r1%shortname
     bundo%r1%longname =bundi%r1%longname
     bundo%r1%units    =bundi%r1%units
  endif
  if(bundi%n2d>0) then
     bundo%r2%shortname=bundi%r2%shortname
     bundo%r2%longname =bundi%r2%longname
     bundo%r2%units    =bundi%r2%units
  endif
  if(bundi%n3d>0) then
     bundo%r3%shortname=bundi%r3%shortname
     bundo%r3%longname =bundi%r3%longname
     bundo%r3%units    =bundi%r3%units
  endif

!$omp parallel do
  do ii=1,bundo%ndim
     bundo%values(ii)=bundi%values(ii)
  enddo
!$omp end parallel do

  do ii=1,bundo%n1d
     bundo%ival1(ii)=bundi%ival1(ii)
  enddo
  do ii=1,bundo%n2d
     bundo%ival2(ii)=bundi%ival2(ii)
  enddo
  do ii=1,bundo%n3d
     bundo%ival3(ii)=bundi%ival3(ii)
  enddo

  return
  end subroutine copy_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE: Assign_Const_ --- Assign values of bundle to a give constant
!
! !INTERFACE:

  subroutine assign_const_(Bundo,cnst)

! !USES:

  implicit none

! !INPUT PARAMETERS:

  real(r_kind),     intent(in   ) :: cnst

! !INPUT/OUTPUT PARAMETERS:

  type(GSI_Bundle), intent(inout) :: bundo

! !DESCRIPTION: Assign values of bundle to a constant
!
!
! !REVISION HISTORY:
!
!       2007 Tremolet Initial code.
!  27Apr2010 Todling  Adapted from control-vector.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

  character(len=*),parameter::myname_='assign_const_'
  integer(i_kind) :: ii,istatus

!$omp parallel do
  do ii=1,bundo%ndim
     bundo%values(ii)=cnst
  enddo
!$omp end parallel do

  return
  end subroutine assign_const_
!noEOC
subroutine hadamard_upd_(zst,yst,xst)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    hadamard_upd
!   prgmmr: todling
!
! abstract: calculate element-by-element product of two state vector
!           and update input vector accordingly.
!
! program history log:
!   2009-08-12  lueken - added subprogram doc block
!   2010-05-15  todling - update to use gsi_bundle
!
!   input argument list:
!    yst
!    xst
!
!   output argument list:
!    zst
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(gsi_bundle), intent(inout) :: zst
  type(gsi_bundle), intent(in   ) :: yst
  type(gsi_bundle), intent(in   ) :: xst
  integer(i_kind) :: ii

  if(yst%ndim/=xst%ndim.or.yst%ndim/=zst%ndim) then
     write(6,*)'hadamard_upd_st: error length'
     call stop2(313)
  endif

  DO ii=1,zst%ndim
     zst%values(ii)=zst%values(ii) + xst%values(ii)*yst%values(ii)
  ENDDO

  return
end subroutine hadamard_upd_
! ----------------------------------------------------------------------
subroutine self_add_st(yst,xst)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    self_add_st
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-12  lueken - added subprogram doc block
!   2010-05-15  todling - update to use gsi_bundle
!
!   input argument list:
!    yst
!    xst
!
!   output argument list:
!    yst
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(gsi_bundle), intent(inout) :: yst
  type(gsi_bundle), intent(in   ) :: xst
  integer(i_kind) :: ii

  if(yst%ndim/=xst%ndim) then
     write(6,*)'self_add_st: error length'
     call stop2(313)
  endif

  DO ii=1,yst%ndim
     yst%values(ii)=yst%values(ii)+xst%values(ii)
  ENDDO

  return
end subroutine self_add_st
! ----------------------------------------------------------------------
subroutine self_add_scal(yst,pa,xst)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    self_add_scal
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-12  lueken - added subprogram doc block
!   2010-05-15  todling - update to use gsi_bundle
!
!   input argument list:
!    yst
!    pa
!    xst
!
!   output argument list:
!    yst
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(gsi_bundle), intent(inout) :: yst
  real(r_kind),     intent(in   ) :: pa
  type(gsi_bundle), intent(in   ) :: xst
  integer(i_kind) :: ii

  if(yst%ndim/=xst%ndim) then
     write(6,*)'self_add_scal: error length'
     call stop2(313)
  endif

  DO ii=1,yst%ndim
     yst%values(ii)=yst%values(ii)+pa*xst%values(ii)
  ENDDO

  return
end subroutine self_add_scal
! ----------------------------------------------------------------------
subroutine self_mul(yst,pa)
!   2009-08-12  lueken - added subprogram doc block
!   2010-05-15  todling - update to use gsi_bundle
  implicit none
  type(gsi_bundle), intent(inout) :: yst
  real(r_kind),     intent(in   ) :: pa
  integer(i_kind) :: ii

  DO ii=1,yst%ndim
     yst%values(ii)=pa*yst%values(ii)
  ENDDO

  return
end subroutine self_mul

real(r_quad) function dplevs2d_(dx,dy,ihalo)
!   2009-08-04  lueken - added subprogram doc block

  implicit none
  real(r_kind)   ,intent(in) :: dx(:,:),dy(:,:)
  integer(i_kind),optional,intent(in) :: ihalo

  real(r_quad) dplevs
  integer(i_kind) :: im,jm,km,ii,jj,kk,ihalo_

  im=size(dx,1)
  jm=size(dx,2)
  ihalo_=0
  if(present(ihalo)) then
     ihalo_=ihalo
  endif

  dplevs=zero_quad
  do jj=1+ihalo_,jm-ihalo_
     do ii=1+ihalo_,im-ihalo_
        dplevs=dplevs+dx(ii,jj)*dy(ii,jj)
     end do
  end do
  dplevs2d_=dplevs

return
end function dplevs2d_
real(r_quad) function dplevs3d_(dx,dy,ihalo)
!   2009-08-04  lueken - added subprogram doc block

  implicit none
  real(r_kind)   ,intent(in) :: dx(:,:,:),dy(:,:,:)
  integer(i_kind),optional,intent(in) :: ihalo

  real(r_quad) dplevs
  integer(i_kind) :: im,jm,km,ii,jj,kk,ihalo_

  im=size(dx,1)
  jm=size(dx,2)
  km=size(dx,3)
  ihalo_=0
  if(present(ihalo)) then
     ihalo_=ihalo
  endif

  dplevs=zero_quad
  do kk=1,km
     do jj=1+ihalo_,jm-ihalo_
        do ii=1+ihalo_,im-ihalo_
           dplevs=dplevs+dx(ii,jj,kk)*dy(ii,jj,kk)
        end do
     end do
  end do
  dplevs3d_=dplevs

return
end function dplevs3d_

real(r_kind) function sum2d_(field,ihalo)
!   2009-08-12  lueken - added subprogram doc block
  implicit none
  real(r_kind),dimension(:,:),intent(in) :: field
  integer(i_kind),optional   ,intent(in) :: ihalo

! local variables
  real(r_kind) :: sum_mask
  integer(i_kind) :: im,jm,i,j,ihalo_
 
  im=size(field,1)
  jm=size(field,2)
  ihalo_=0
  if(present(ihalo)) then
     ihalo_=ihalo
  endif

  sum_mask=zero
  do j=1+ihalo_,jm-ihalo_
     do i=1+ihalo_,im-ihalo_
        sum_mask=sum_mask+field(i,j)
     end do
  end do
  sum2d_=sum_mask
  return
end function sum2d_
real(r_kind) function sum3d_(field,ihalo)
!   2009-08-12  lueken - added subprogram doc block
  implicit none
  real(r_kind),dimension(:,:,:),intent(in) :: field
  integer(i_kind),optional     ,intent(in) :: ihalo

! local variables
  real(r_kind) :: sum_mask
  integer(i_kind) :: im,jm,km,i,j,k,ihalo_
 
  im=size(field,1)
  jm=size(field,2)
  km=size(field,3)
  ihalo_=0
  if(present(ihalo)) then
     ihalo_=ihalo
  endif

  sum_mask=zero
  do k=1,km
     do j=1+ihalo_,jm-ihalo_
        do i=1+ihalo_,im-ihalo_
           sum_mask=sum_mask+field(i,j,k)
        end do
     end do
  end do
  sum3d_=sum_mask
  return
end function sum3d_
!............................................................................................
!BOP
!
! !IROUTINE: Unset_ --- Unset pointers within Bundle
!
! !INTERFACE:
  subroutine unset_ ( Bundle, istatus )
    
    type(GSI_Bundle),intent(inout) :: Bundle
    integer(i_kind),intent(out) :: istatus

! !DESCRIPTION: Nullify bundle pointers.
!
! !REVISION HISTORY:
!
!  27Apr2010 Todling  Adapted from control-vector.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

    integer(i_kind) :: i, is, n1d, n2d, n3d

    n1d = Bundle%n1d
    n2d = Bundle%n2d
    n3d = Bundle%n3d

    is=0
    istatus=0
    if(n1d>0) then
       call clean_(Bundle%r1,n1d,is)
       istatus=istatus+is
       do i = 1, n1d
          nullify(Bundle%r1(i)%q)
       enddo
       deallocate(Bundle%r1,stat=is)
       istatus=istatus+is
       deallocate(Bundle%ival1,stat=is)
    endif
    istatus=istatus+is
    if(n2d>0) then
       call clean_(Bundle%r2,n2d,is)
       istatus=istatus+is
       do i = 1, n2d
          nullify(Bundle%r2(i)%q)
       enddo
       deallocate(Bundle%r2,stat=is)
       istatus=istatus+is
       deallocate(Bundle%ival2,stat=is)
    endif
    istatus=istatus+is
    if(n3d>0) then
       call clean_(Bundle%r3,n3d,is)
       istatus=istatus+is
       do i = 1, n3d
          nullify(Bundle%r3(i)%q)
       enddo
       deallocate(Bundle%r3,stat=is)
       istatus=istatus+is
       deallocate(Bundle%ival3,stat=is)
    endif
    istatus=istatus+is

!  .... need grid clean for more general grids
    Bundle%grid%im=-1
    Bundle%grid%jm=-1
    Bundle%grid%km=-1

    Bundle%n1d=-1
    Bundle%n2d=-1
    Bundle%n3d=-1
    Bundle%ndim=-1

  end subroutine unset_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE: Destroy_ --- Deallocate contents of Bundle
!
! !INTERFACE:

  subroutine destroy_ ( Bundle, istatus )
    
! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle),intent(inout) :: Bundle

! !OUTPUT PARAMETERS:

    integer(i_kind),intent(out) :: istatus

! !DESCRIPTION: Deallocate contents of bundle.
!
!
! !REVISION HISTORY:
!
!  27Apr2010 Todling  Adapted from control-vector.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

    integer(i_kind) :: i, is, n1d, n2d, n3d

    n1d = Bundle%n1d
    n2d = Bundle%n2d
    n3d = Bundle%n3d
    is=0

!   In opposite order of creation
    if(n3d>0) deallocate(Bundle%ival3,stat=istatus)
    is=istatus+is
    if(n2d>0) deallocate(Bundle%ival2,stat=istatus)
    is=istatus+is
    if(n1d>0) deallocate(Bundle%ival1,stat=istatus)
    is=istatus+is

    deallocate(Bundle%values,stat=istatus)
    is=istatus+is

    if(n1d>0) then
       call clean_(Bundle%r1,n1d,istatus)
       is=istatus+is
       do i = 1, n1d
          nullify(Bundle%r1(i)%q)
       enddo
       deallocate(Bundle%r1,stat=istatus)
    endif
    is=istatus+is
    if(n2d>0) then
       call clean_(Bundle%r2,n2d,istatus)
       is=istatus+is
       do i = 1, n2d
          nullify(Bundle%r2(i)%q)
       enddo
       deallocate(Bundle%r2,stat=istatus)
    endif
    is=istatus+is
    if(n3d>0) then
       call clean_(Bundle%r3,n3d,istatus)
       is=istatus+is
       do i = 1, n3d
          nullify(Bundle%r3(i)%q)
       enddo
       deallocate(Bundle%r3,stat=istatus)
    endif
    is=istatus+is

!  .... need grid clean for more general grids
    Bundle%grid%im=-1
    Bundle%grid%jm=-1
    Bundle%grid%km=-1

    Bundle%n1d=-1
    Bundle%n2d=-1
    Bundle%n3d=-1
    Bundle%NumVars=-1
    Bundle%ndim=-1

  end subroutine destroy_
!............................................................................................
!BOP
!
! !IROUTINE: Print_ --- Print max/min of bundle contents
!
! !INTERFACE:

  subroutine print_ ( Bundle )

! !INPUT PARAMETERS:

    type(GSI_Bundle) :: Bundle
    
! !DESCRIPTION: Summarize contents of bundle by echoing max/min values.
!
!
! !REMARKS:
!   1. As the rest of the Bundle, this routine is MPI-free, so user
!      should be cautions when calling it each process will write its own.
!
! !REVISION HISTORY:
!
!       2010 da Silva  Initial code
!  27Apr2010 Todling   Adapt to GSI_Bundle
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    integer(i_kind) :: i 
    print *
    print *, 'Bundle: ', trim(Bundle%name)
    do i = 1, Bundle%n1d
       write(*,'(a20,2x,1p,e11.4,2x,e11.4)') '  [1d] '//Bundle%r1(i)%shortname, &
                       minval(Bundle%r1(i)%q), &
                       maxval(Bundle%r1(i)%q)
    end do
    do i = 1, Bundle%n2d
       write(*,'(a20,2x,1p,e11.4,2x,e11.4)') '  [2d] '//Bundle%r2(i)%shortname, &
                       minval(Bundle%r2(i)%q), &
                       maxval(Bundle%r2(i)%q)
    end do
    do i = 1, Bundle%n3d
       write(*,'(a20,2x,1p,e11.4,2x,e11.4)') '  [3d] '//Bundle%r3(i)%shortname, &
                       minval(Bundle%r3(i)%q), &
                       maxval(Bundle%r3(i)%q)
    end do
                             
  end subroutine print_
!noEOC

  logical function redundant_ ( Bundle )
  type(gsi_bundle),intent(in) :: Bundle
  integer(i_kind) i,ii,j,ic,n1d,n2d,n3d,nvars,istatus
  character(len=MAXSTR),allocatable::fnames(:)

  redundant_=.false.

  n1d=Bundle%n1d
  n2d=Bundle%n2d
  n3d=Bundle%n3d
  nvars=Bundle%Numvars
  allocate(fnames(nvars))
  call inquire_char_ ( Bundle, 'shortnames', fnames, istatus )
  if (istatus/=0) then
     redundant_=.true. ! what else to do?
     deallocate(fnames)
     return
  endif
  if (nvars>0) then
      do j = 1,nvars
         ic=0
         do i = j,nvars
            if(trim(fnames(i))==trim(fnames(j))) ic=ic+1 ! this is stupid
         enddo
         if(ic/=1) then 
            redundant_=.true.
            deallocate(fnames)
            return
         endif
      enddo
  endif
  deallocate(fnames)
  end function redundant_

  subroutine GSI_GridCreate ( grid, im, jm, km )
! this does not belong to the bundle ... gridmod?
  implicit none
  integer,        intent(in)  :: im, jm, km
  type(GSI_Grid), intent(out) :: grid
  grid%im=im
  grid%jm=jm
  grid%km=km
  end subroutine GSI_GridCreate

end module GSI_BundleMod
!EOC
