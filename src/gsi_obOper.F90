module gsi_obOper
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module gsi_obOper
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2018-06-26
!
! abstract: GSI observation operator, bunlding obs_diags and obsLList lists
!
! program history log:
!   2018-06-26  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! module interface:

  use m_obsNodeTypeManager, only: obsNode_typeIndex
  use m_obsNodeTypeManager, only: obsNode_typeIndex
  use m_obsdiags   , only: obsdiags
  use m_obsdiags   , only: obsLLists
  use m_obsdiagNode, only: obs_diags
  use m_obsLList   , only: obsLList

  use kinds, only: i_kind
  implicit none
  private	! except
  public :: obOper		! data structure
  public :: len_obstype
  public :: len_isis

        integer(i_kind),parameter:: len_obstype=10
        integer(i_kind),parameter:: len_isis   =20

        ! obOper is a bundle of observation operator arrays (or lists), such as
        ! linked-lists of obs_diag (obs_diags) and obsNode (obsLList), plus type
        ! specific parameters.
        !
        ! In this implementation, an obOper, with pointers _associated_ to
        ! rank-1 arrays of obs_diags and obsLList, where both targets are
        ! instantiated separately with own fixed dimensions in nobs_type and
        ! nobs_bins.
        !
        ! It is planned in the future, to implement an obOper _contains_ dynamic
        ! components of these rank-1 arrays.

  type,abstract:: obOper
    !private
        ! In the first obOper implementation, %obsLL(:) and odiagLL(:) are
        ! treated as aliases to the instances of m_obsdiags::obsdiags(:,:) and
        ! m_obsdiagss::obsLLists(:,:).  Both linked-lists are dimensioned for
        ! 1:nobs_type in the current implementation, and accesssed once per type
        ! and per bin, in intjo() and stpjo().
        !
        ! On the other hand, in the current setuprhsall() implementation, obOper
        ! objects are accessed for 1:ndat, or once per obs-stream, where each
        ! type is in general accessed in zero or multiple times.

    type(obs_diags),pointer,dimension(:):: odiagLL    ! (1:nobs_bins)
    type(obsLList ),pointer,dimension(:)::   obsLL    ! (1:nobs_bins)

  contains
    procedure(mytype),deferred,nopass:: mytype  ! type information

    procedure, non_overridable:: init  => init_         ! initialize
    procedure, non_overridable:: slice => slice_        ! create a slice, 
    procedure, non_overridable:: clean => clean_        ! finalize

    generic:: setup => setup_
      procedure(setup_ ),deferred:: setup_       ! incremental object initialization
    generic:: intjo => intjo_, intjo1_
      procedure,  non_overridable:: intjo_      ! interface supporting intjo()
      procedure(intjo1_),deferred:: intjo1_     ! interface supporting intjo()
    generic:: stpjo => stpjo_, stpjo1_
      procedure,  non_overridable:: stpjo_      ! interface supporting stpjo()
      procedure(stpjo1_),deferred:: stpjo1_     ! interface supporting stpjo()

  end type obOper

  public:: obOper_create
  public:: obOper_destroy
  public:: obOper_slice
        interface obOper_create ; module procedure  create_  ; end interface
        interface obOper_destroy; module procedure destroy_  ; end interface
        interface obOper_slice  ; module procedure &
                slice_, &
                slices_ ; end interface

        ! In setuprhsall(),
        !
        !   | use gsi_obOperTypeManager, only: obOper_typeMold
        !   | use obsmod, only: ndat,dtype
        !
        ! then in a loop of obs-streams
        !
        !   | do is=1,ndat
        !   |   isop => obOper_create(obOper_typeMold(dtype(is)))
        !   |   call isop%setup(...)
        !   |   call obOper_destroy(iop)
        !   | enddo
        !

        ! In intjo() or stpjo(),
        !
        !   | use gsi_obOperTypeManager, only: lbound_obOper
        !   | use gsi_obOperTypeManager, only: ubound_obOper
        !   | use gsi_obOperTypeManager, only: obOper_typeMold
        ! 
        ! then in a loop of obOper
        !
        !   | do it=lbound_obOper,ubound_obOper
        !   |   iop => obOper_create(obOper_typeMold(it))
        !   |   call iop%intjo(...)
        !   |   call obOper_destroy(iop)
        !   | enddo

!--- Design Considerations ---
! (1) Fully objectize obOper, meaning, capable of being instantiated where and
!     when it is needed.
!
!
! (2) For continuity, its instantiation is a type-indexed array of polymorphic
!     class(obOper), containing rank-1 pointers aliased to obsLList(1:nobs_bins)
!     and diagLList(1:nobs_bins).  This means its current instantiation is
!     declared based on a type-wrapper-array structure,
!
!       type,abstract:: obOper; ...
!       type:: obOper_element; class(obOper),pointer:: ptr; ...
!       type(obOper_element),dimension(nobs_type):: obopers
!
!     defined in a type-loop, (m_obsdiags?)
!
!       allocate(obopers(it)%ptr,mold=obOper_typeMold(it))
!
!       | oboper_typeMold(it) result(mold)
!       |   select case(it)
!       |   case(iobType_rad); mold => radOper_mold()
!       |   case ...
!
!     followed by
!
!       associate(i_op => obopers(...)%ptr)
!         call i_op%init(...)   # type-bound init(), with a line of
!                               # self%nodetype=obOper%mytype(nodetype=.true.)
!       end associate
!
!
! (3) In future implementations, one might want to define obOper on a per-stream
!     base.  Then it would be instantiated in a stream-loop,
!
!       allocate(obopers(is)%ptr,mold=obOper_typeMold(dtype(is)))
!
!       | oboper_typeMold(dtype) result(mold)
!       |  select case(dtype)
!       |  case("rad","amsua",...); mold => radOper_mold()
!       |  case ...
!  
! (4) So types of obOpers are now one-to-one mapped to obsNode types.  This means
!     that each obOper type must be hardwired to a known obsNode type, while
!     dtype(:) to obOpers(:) types are not.
!     

!--------- interfaces
abstract interface
  function mytype(nodetype)
        ! %mytype() for self's typename
        ! %mytype(nodetype=.true.) for self's corresponding node type name
    implicit none
    character(len=:), allocatable:: mytype
    logical, optional, intent(in):: nodetype      ! if .true., return %mytype() of its obsNode

    ! logical:: nodetype_
    ! nodetype_=.false.
    ! if(present(nodetype)) nodetype_=nodetype
    ! if(nodetype_) then
    !   mytype="rad"
    ! else
    !   mytype="[radOper]"
    ! endif

  end function mytype
end interface

abstract interface
  subroutine setup_(self, lunin, mype, is, nobs, init_pass,last_pass)
    use kinds, only: i_kind
    import:: obOper
    implicit none
    class(obOper  ), intent(inout):: self
    integer(i_kind), intent(in):: lunin
    integer(i_kind), intent(in):: mype
    integer(i_kind), intent(in):: is
    integer(i_kind), intent(in):: nobs
    logical        , intent(in):: init_pass     ! supporting multi-pass setup()
    logical        , intent(in):: last_pass     ! with incremental backgrounds.

    ! An example in radOper%setup(),
    !
    !   if(nobs == 0) return
    !
    !   read(lunin,iostat=ier) obstype,isis,nreal,nchanl
    !   if(ier/=0) call die(myname_,'read(), iostat =',ier)
    !   nele=nreal+nchanl
    !
    !   call setuprad(self%obsLL(:),self%odiagLL(:), lunin, mype,   &
    !     aivals,stats,nchanl,nreal,nobs,obstype,isis,is,rad_diagsave,init_pass,last_pass)

  end subroutine setup_
end interface

#ifdef _TO_BE_REMOVED_
abstract interface
  subroutine intjo_(self, rval, sval, qpred, sbias)
    use gsi_bundlemod  , only: gsi_bundle
    use bias_predictors, only: predictors
    use kinds          , only: r_quad
    import:: obOper
    implicit none
    class(obOper   ), intent(inout):: self
    type(gsi_bundle), dimension(  :),intent(inout):: rval
    type(gsi_bundle), dimension(  :),intent(in   ):: sval
    real(r_quad    ), dimension(:,:),intent(inout):: qpred ! a buffer of rbias
    type(predictors)                ,intent(in   ):: sbias

        ! This implementation can be used both to an obOper instance with
        ! multiple bins, or a "slice" of obOper instance with a single bin,
        ! where the slice of self contains arrays (ibin:ibin) of components.

    ! call self%intjo(rval(1:nbins),sval(1:nbins),qpred(:,1:nbins),sbias)
  end subroutine intjo_
end interface
#endif

abstract interface
  subroutine intjo1_(self, ibin, rval, sval, qpred, sbias)
    use gsi_bundlemod  , only: gsi_bundle
    use bias_predictors, only: predictors
    use kinds          , only: i_kind, r_quad
    import:: obOper
    implicit none
    class(obOper   ), intent(in   ):: self
    integer(i_kind ), intent(in   ):: ibin
    type(gsi_bundle), intent(inout):: rval
    type(gsi_bundle), intent(in   ):: sval
    real(r_quad    ), target, dimension(:),intent(inout):: qpred ! a buffer of rbias
    type(predictors), target,              intent(in   ):: sbias

        ! This implementation can be used both to an obOper instance with
        ! multiple bins, or a "slice" of obOper instance with a single bin,
        ! where the slice of self contains arrays (ibin:ibin) of components.

    !do ibin=lbound(self%obsLL,1),ubound(self%obsLL,1)
    !  call self%intjo(ibin, rval(ibin),sval(ibin), qpred(:,ibin),sbias)
    !enddo
  end subroutine intjo1_
end interface

#ifdef _TO_BE_REMOVED_
abstract interface
  ! in stpjo()
  !
  !   do mm=1,stpcnt
  !     ll=ll_jo(mm)    ! itype
  !     ib=ib_jo(mm)
  !
  !     llop => obOper_create(obOper_typeMold(ll))
  !     ibop => obOper_slice(llop,ib)
  !
  !     call ibop%stpjo(dval(ib:ib),dbias,xval(ib:ib),xbias,pbcjo(:,ll,ib:ib),sges,nstep)
  !
  !     call obOper_destroy(ibop)
  !     call obOper_destroy(itop)
  !   enddo

  subroutine stpjo_(self, dval,xval,pbcjo,sges,nstep,dbias,xbias)
    use gsi_bundlemod  , only: gsi_bundle
    use bias_predictors, only: predictors
    use kinds          , only: r_quad,r_kind,i_kind
    import:: obOper
    implicit none
    class(obOper), intent(in):: self

    type(gsi_bundle),dimension(  :),intent(in   ):: dval  ! (1:nbins)
    type(gsi_bundle),dimension(  :),intent(in   ):: xval  ! (1:nbins)
    real(r_quad    ),dimension(:,:),intent(inout):: pbcjo ! (1:4,:)
    real(r_kind    ),dimension(:  ),intent(in   ):: sges
    integer(i_kind ),               intent(in   ):: nstep
    type(predictors),intent(in):: dbias
    type(predictors),intent(in):: xbias

    !integer(i_kind):: ibin
    !real(r_quad),pointer,dimension(:):: pbc_ibin
    !
    !do ibin=lbound(self%obsLL,1),ubound(self%obsLL,1)
    !  pbc_ibin => pbcjo(1:4,ibin)
    !  call stprad(self%obsLL(ibin), dval(ibin),xval(ibin),dbias%predr,xbias%predr,pbc_ibin,sges,nstep)
    !  pbc_ibin => null()
    !enddo
  end subroutine stpjo_
end interface
#endif

abstract interface
  !>> call self%stpjo(ib,dval(ib),xval(ib),pbcjo(:,it,ib),sges,nstep,dbias,xbias)

  subroutine stpjo1_(self, ibin,dval,xval,pbcjo,sges,nstep,dbias,xbias)
    use gsi_bundlemod  , only: gsi_bundle
    use bias_predictors, only: predictors
    use kinds          , only: r_quad,r_kind,i_kind
    import:: obOper
    implicit none
    class(obOper  ),intent(in):: self
    integer(i_kind),intent(in):: ibin

    type(gsi_bundle),intent(in   ):: dval
    type(gsi_bundle),intent(in   ):: xval
    real(r_quad    ),dimension(:),intent(inout):: pbcjo ! (1:4)
    real(r_kind    ),dimension(:),intent(in   ):: sges
    integer(i_kind ),             intent(in   ):: nstep
    type(predictors), target, intent(in):: dbias
    type(predictors), target, intent(in):: xbias

  end subroutine stpjo1_
end interface

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  character(len=*),parameter:: myname="gsi_obOper"

contains
#include "myassert.H"

function create_(mold)
  implicit none
  class(obOper),pointer:: create_
  class(obOper),target,intent(in):: mold
  create_ => mold
  if(associated(create_)) then
    allocate(create_,mold=mold)
    call create_%init()
  endif
end function create_

function slice_(self,i) result(slice)
!-- slice is a copy of self, except its linked-list pointers, which are
!-- referenced only to a user specified "slice" of the original linked-
!-- list pointers of "self".

  implicit none
  class(obOper  ), pointer   :: slice
  class(obOper  ), intent(in):: self
  integer(i_kind), intent(in):: i       ! bin index
  allocate(slice,source=self)           ! slice is first a full copy of self,
  slice%odiagLL(i:) => self%odiagLL(i:i)  ! then a slice of %odiagLL(:), and
  slice%  obsLL(i:) => self%  obsLL(i:i)  ! a slice of %obsLL(:)
end function slice_

function slices_(self,lbin,ubin) result(slice)
!-- slices is a copy of self, except its linked-list pointers, which are
!-- referenced only to a user specified "slice" of the original linked-
!-- list pointers of "self".

  implicit none
  class(obOper  ), pointer   :: slice
  class(obOper  ), intent(in):: self
  integer(i_kind), intent(in):: lbin    ! lower bin index
  integer(i_kind), intent(in):: ubin    ! upper bin index
  allocate(slice,source=self)           ! slice is first a full copy of self,
  slice%odiagLL(lbin:) => self%odiagLL(lbin:ubin)  ! then a slice of %odiagLL(:), and
  slice%  obsLL(lbin:) => self%  obsLL(lbin:ubin)  ! a slice of % obsLL(:)
end function slices_

subroutine destroy_(self)
  implicit none
  class(obOper),pointer,intent(inout):: self
  if(associated(self)) then
    call self%clean()
    deallocate(self)
  endif
end subroutine destroy_

subroutine init_(self)
  implicit none
  class(obOper),intent(inout):: self

        ! Through its assigned node type, map an obOper type to its
        ! corresponding obsNode index.

  call initbyindex_(self,obsNode_typeIndex(self%mytype(nodetype=.true.)))
end subroutine init_

subroutine initbyvname_(self,vname)
  implicit none
  class(obOper   ),intent(inout):: self
  character(len=*),intent(in   ):: vname

  call initbyindex_(self,obsNode_typeIndex(vname))
end subroutine initbyvname_

subroutine initbyindex_(self,itype)
  use m_obsdiags, only: obsdiags
  use m_obsdiags, only: obsLLists
  use m_obsNodeTypeManager, only: obsNode_typeIndex
  implicit none
  class(obOper   ),intent(inout):: self
  integer(i_kind ),intent(in   ):: itype

        ASSERT(itype>=lbound(obsLLists,1))
        ASSERT(itype<=ubound(obsLLists,1))
        ASSERT(itype>=lbound(obsdiags ,1))
        ASSERT(itype<=ubound(obsdiags ,1))

  self%odiagLL => obsdiags (itype,:)
  self%  obsLL => obsLLists(itype,:)
end subroutine initbyindex_

subroutine clean_(self)
  implicit none
  class(obOper),intent(inout):: self
  self%odiagLL => null()
  self%  obsLL => null()
end subroutine clean_

subroutine intjo_(self, rval,sval,qpred,sbias)
  use gsi_bundlemod  , only: gsi_bundle
  use bias_predictors, only: predictors
  use kinds, only: i_kind, r_quad
  implicit none
  class(obOper   ), intent(in):: self
  type(gsi_bundle), dimension(:  ),intent(inout):: rval
  type(gsi_bundle), dimension(:  ),intent(in   ):: sval
  real(r_quad    ), dimension(:,:),intent(inout):: qpred
  type(predictors)                ,intent(in   ):: sbias

        ! nb=nobs_bins
        ! do ityp=1,nobs_type
        !   iop => obOper_create(mold=obOper_typemold(ityp))
        !   call iop%intjo(rval(:nb),sval(:nb), qpred(:,:nb),sbias)
        !   iop => null()
        ! enddo
        !
        ! This implementation can be used both to an obOper instance with
        ! multiple bins, or a "slice" of obOper instance with a single bin,
        ! where the slice of self contains arrays (ibin:ibin) of components.

  character(len=*),parameter:: myname_=myname//"::intjo_"
  integer(i_kind):: lbnd,ubnd,ibin

  lbnd = lbound(self%obsLL,1)
  ubnd = ubound(self%obsLL,1)
  ASSERT(lbnd == lbound( rval,1) .and. ubnd == ubound( rval,1))
  ASSERT(lbnd == lbound( sval,1) .and. ubnd == ubound( sval,1))
  ASSERT(lbnd == lbound(qpred,2) .and. ubnd == ubound(qpred,2))

  do ibin=lbnd,ubnd
    call self%intjo(ibin,rval(ibin),sval(ibin),qpred(:,ibin),sbias)
  enddo
end subroutine intjo_

subroutine stpjo_(self, dval,xval, pbcjo,sges,nstep, dbias,xbias)
  use kinds, only: r_quad,r_kind,i_kind
  use gsi_bundlemod, only: gsi_bundle
  use bias_predictors, only: predictors
  implicit none
  class(obOper   ),intent(in):: self
  type(gsi_bundle),dimension(  :),intent(in   ):: dval
  type(gsi_bundle),dimension(  :),intent(in   ):: xval
  real(r_quad    ),dimension(:,:),intent(inout):: pbcjo ! (1:4,1:nbin)
  real(r_kind    ),dimension(:  ),intent(in   ):: sges
  integer(i_kind),intent(in):: nstep

  type(predictors), intent(in):: dbias
  type(predictors), intent(in):: xbias

  integer(i_kind):: lbnd,ubnd,ibin

  lbnd = lbound(self%obsLL,1)
  ubnd = ubound(self%obsLL,1)
  ASSERT(lbnd == lbound( dval,1) .and. ubnd == ubound( dval,1))
  ASSERT(lbnd == lbound( xval,1) .and. ubnd == ubound( xval,1))
  ASSERT(lbnd == lbound(pbcjo,2) .and. ubnd == ubound(pbcjo,2))

  do ibin=lbnd,ubnd
    call self%stpjo(ibin,dval(ibin),xval(ibin),pbcjo(:,ibin),sges,nstep,dbias,xbias)
  enddo
end subroutine stpjo_
end module gsi_obOper
!.
