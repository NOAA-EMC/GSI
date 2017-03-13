module m_obsdiagNode
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_obsdiagNode
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2016-05-18
!
! abstract: module of node type obs_diag and linked-list type obs_diags.
!
! program history log:
!   2016-05-18  j guo   - added this document block for the initial implementation.
!   2016-06-24  j.guo   - Added support of using m_latlonRange to find a cluster
!                         latlonRange from (elat,elon) values of observations.
!                       . cleaned out some components from obsdiagNode, which
!                         were put in for debugging purposes. (%dlat,%dlon).
!                       . removed some earlier routines for debuggings and
!                         testings.  e.g. lmock_() and obsnode_mock_().
!                       . use a fixed miter size for both write_() and read_(),
!                         for a simpler control in the future.
!                       . renamed lsize_() to lcount_().  Then reimplemented a
!                         new lsize_() to separate different functionalities.
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
  use kinds , only: i_kind,r_kind
  use obsmod, only: obs_diag
  use obsmod, only: obs_diags
  use mpeu_util, only: assert_,tell,warn,perr,die
#define _obsNode_   obs_diag
#define _obsLList_  obs_diags
  implicit none

  private
        ! Primery behaviors:
  public:: obsdiagLList_reset   ! destructor + initializer
  public:: obsdiagLList_read    ! reader, for input
  public:: obsdiagLList_write   ! writer, for otuput
  public:: obsdiagLList_lsize   ! size inquiry
  public:: obsdiagLList_lcount  ! size inquiry with recount
  public:: obsdiagLList_lsort   ! sort nodes according to their keys
  public:: obsdiagLList_checksum! size consistency checking
  public:: obsdiagLList_summary ! status report

        interface obsdiagLList_reset; module procedure lreset_; end interface
        interface obsdiagLList_read ; module procedure  lread_; end interface
        interface obsdiagLList_checksum; module procedure &
          lchecksum_  , &
          lchecksum1_ , &
          lchecksum2_ ; end interface
        interface obsdiagLList_lsize  ; module procedure lsize_   ; end interface
        interface obsdiagLList_lcount ; module procedure lcount_  ; end interface
        interface obsdiagLList_lsort  ; module procedure lsort_   ; end interface
        interface obsdiagLList_write  ; module procedure lwrite_  ; end interface
        interface obsdiagLList_summary; module procedure lsummary_; end interface

        ! Node lookup, secondary function with its searching component
  public:: obsdiagLookup_build  ! setup, its searching component
  public:: obsdiagLookup_locate ! node lookup, with the searching component
  public:: obsdiagLookup_clean  ! clean, its searching component

        interface obsdiagLookup_build ; module procedure lbuild_; end interface
        interface obsdiagLookup_locate; module procedure locate_; end interface
        interface obsdiagLookup_clean ; module procedure lclean_; end interface

  public:: obsdiagLList_dump
        interface obsdiagLList_dump; module procedure ldump_; end interface

  public:: obsdiagNode_append
        interface obsdiagNode_append; module procedure obsNode_append_; end interface
  public:: obsdiagNode_first
        interface obsdiagNode_first ; module procedure  obsNode_first_; end interface
  public:: obsdiagNode_next
        interface obsdiagNode_next  ; module procedure   obsNode_next_; end interface

  !public:: fptr_obsdiagNode

  type fptr_obsdiagNode
    type(obs_diag),pointer:: node
  end type fptr_obsdiagNode

!#define NDEBUG
!#define DEBUG_TRACE
!#define DEBUG_VERBOSE

#include "myassert.H"
#include "mytrace.H"

  character(len=*),parameter:: myname="m_obsdiagNode"

contains
subroutine lwrite_(diagLL,iunit,luseonly,jiter,miter,jj_type,ii_bin,luseRange)
  use m_latlonRange  , only: latlonRange
  use m_latlonRange  , only: latlonRange_enclose
  use mpeu_util, only: stdout
  use mpeu_util, only: stdout_lead
  implicit none
  type(_obsLList_)    ,intent(inout):: diagLL   ! the linked list of data
  integer(kind=i_kind),intent(in   ):: iunit    ! the output unit
  logical             ,intent(in   ):: luseonly ! write only if(luse)
  integer(kind=i_kind),intent(in   ):: jiter    ! diag width for the IO (or this iter)
  integer(kind=i_kind),intent(in   ):: miter    ! diag width of the memory
  integer(kind=i_kind),intent(in   ):: jj_type, ii_bin
  type(latlonRange),optional,intent(inout):: luseRange

  character(len=*),parameter:: myname_=myname//"::lwrite_"
  integer(kind=i_kind):: iobs,kobs,lobs,mobs
  integer(kind=i_kind):: istat
  type(_obsNode_), pointer:: iNode
  logical:: isluse_
_ENTRY_(myname_)
!_TIMER_ON_(myname_)

    lobs=obsdiagLList_lcount(diagLL,luseonly=luseonly)
    mobs=lobs
    if(.not.luseonly) mobs=obsdiagLList_lsize(diagLL)

    call obsHeader_write_(iunit,ii_bin,jj_type,lobs,jiter,miter,istat)
                if(istat/=0) then
                  call perr(myname_,'obsHeader_write_(), istat =',istat)
                  call perr(myname_,'                    iunit =',iunit)
                  call perr(myname_,'                   ii_bin =',ii_bin)
                  call perr(myname_,'                    jtype =',jj_type)
                  call perr(myname_,'                    jiter =',jiter)
                  call perr(myname_,'                    miter =',miter)
                  call perr(myname_,'    total-luse-node, lobs =',lobs)
                  call perr(myname_,'     total-all-node, mobs =',mobs)
                  call perr(myname_,'                 luseonly =',luseonly)
                  call  die(myname_)
                endif

    _TRACE_(myname_,'looping through obshead pointers')

    if(lobs<=0) then
      !_TIMER_OFF_(myname_)
      _EXIT_(myname_)
      return
    endif

    iobs=0
    kobs=0
    iNode => obsNode_first_(diagLL)
    do while(associated(iNode))
      iobs=iobs+1
      isluse_=obsNode_isluse_(iNode)
      if(isluse_ .or. .not.luseonly) then

                ! Update luseRange with a luse observation, for the lat-lon-
                ! range on the current PE.

        if(isluse_ .and. present(luseRange)) &
                call latlonRange_enclose(luseRange,iNode%elat,iNode%elon)

                ! Count it, then write the node out.  Use of miter suggests a
                ! fixed output size.
        kobs=kobs+1
        call obsNode_write_(iNode,iunit,miter,istat)
                if(istat/=0) then
                  call perr(myname_,'obsNode_write_(), istat =',istat)
                  call perr(myname_,'                  iunit =',iunit)
                  call perr(myname_,'                  jiter =',jiter)
                  call perr(myname_,'                  miter =',miter)
                  call perr(myname_,'                 ii_bin =',ii_bin)
                  call perr(myname_,'                  jtype =',jj_type)
                  call perr(myname_,'current-luse-node, kobs =',kobs)
                  call perr(myname_,' current-all-node, iobs =',iobs)
                  call perr(myname_,'  total-luse-node, lobs =',lobs)
                  call perr(myname_,'   total-all-node, mobs =',mobs)
                  call perr(myname_,'               luseonly =',luseonly)
                  call  die(myname_)
                endif
      endif
      iNode => obsNode_next_(diagLL)
    enddo

    ASSERT(kobs==lobs)
    ASSERT(iobs==mobs)

!_TIMER_OFF_(myname_)
_EXIT_(myname_)
return
end subroutine lwrite_

subroutine ldump_(diagLL,jiter)
  use mpeu_util, only: stdout
  implicit none
  type(_obsLList_),         intent(inout):: diagLL  ! the list to dump
  integer(i_kind ),optional,intent(in   ):: jiter   ! jiter of diagLL

  character(len=*),parameter:: myname_=myname//"::ldump_"
  integer(kind=i_kind):: iobs,lobs,mobs
  integer(kind=i_kind):: jiter_
  type(_obsNode_), pointer:: iNode
  logical:: isluse_,ismuse_
_ENTRY_(myname_)
!_TIMER_ON_(myname_)
    jiter_=0
    if(present(jiter)) jiter_=jiter

    call lbuild_(diagLL)        ! create a pointer array %lookup, sorted by (idv,iob,ich)

    lobs=0
    mobs=0
    do iobs=1,size(diagLL%lookup(:))
      iNode => diagLL%lookup(iobs)%ptr

      isluse_=obsNode_isluse_(iNode)
      if(isluse_) lobs=lobs+1

      ismuse_=jiter_>=1.and.jiter_<=size(iNode%muse)
      if(ismuse_) ismuse_=iNode%muse(jiter_)
      if(ismuse_) mobs=mobs+1

      write(stdout,'(2x,2l1,3i8,2x,2f12.4)') isluse_,ismuse_, &
        iNode%idv,iNode%iob,iNode%ich, iNode%elat,iNode%elon
    enddo
    write(stdout,'(2x,a,4i8)') '***',jiter_,size(diagLL%lookup(:)),lobs,mobs
    call lclean_(diagLL)        ! destroy the pointer array %lookup.

!_TIMER_OFF_(myname_)
_EXIT_(myname_)
return
end subroutine ldump_

subroutine lread_(diagLL,iunit,redistr,jiter,miter,jj_type,ii_bin,jread,leadNode,ignore_iter)
!_DEBUG_USE_
!_TIMER_USE_
  use obsmod, only: lobserver
  use obs_sensitivity, only: lobsensfc, lsensrecompute
  implicit none
  type(_obsLList_),intent(inout):: diagLL
  integer(kind=i_kind),intent(in   ):: iunit
  logical        ,intent(in   ):: redistr
  integer(kind=i_kind),intent(in   ):: jiter
  integer(kind=i_kind),intent(in   ):: miter
  integer(kind=i_kind),intent(in   ):: jj_type, ii_bin
  integer(kind=i_kind),intent(  out):: jread
  type(_obsNode_), pointer, intent(out):: leadNode
  logical    ,optional,intent(in   ):: ignore_iter

  character(len=*),parameter:: myname_=myname//"::lread_"
  integer(kind=i_kind):: ki,kj,kobs,kiter,miter_read
  integer(kind=i_kind):: kk,istat
  type(_obsNode_), pointer:: aNode
  logical:: ignore_iter_
_ENTRY_(myname_)
!_TIMER_ON_(myname_)
!call timer_ini(myname_)
      ignore_iter_=.false.
      if(present(ignore_iter)) ignore_iter_=ignore_iter

      call obsHeader_read_(iunit,ki,kj,kobs,kiter,miter_read,istat)
                if(istat/=0) then
                  call perr(myname_,'obsHeader_read_(), istat =',istat)
                  call perr(myname_,'                   iunit =',iunit)
                  call  die(myname_)
                endif

      if(ki/=ii_bin .or. kj/=jj_type .or. miter/=miter_read) then
        call perr(myname_,'obsHeader_read_(), unexpected header values (ii,jj,miter)')
        call perr(myname_,'  expecting miter =',miter)
        call perr(myname_,'     actual miter =',miter_read)
        call perr(myname_,'     expecting ii =',ii_bin)
        call perr(myname_,'        actual ii =',ki)
        call perr(myname_,'     expecting jj =',jj_type)
        call perr(myname_,'        actual jj =',kj)
        call  die(myname_)
#ifdef DEBUG_VERBOSE
      else
        call tell(myname_,'obsHeader_read_(), current jiter =',jiter)
        call tell(myname_,'                 expecting miter =',miter)
        call tell(myname_,'                    expecting ii =',ii_bin)
        call tell(myname_,'                    expecting jj =',jj_type)
        call tell(myname_,'                    actual miter =',miter_read)
        call tell(myname_,'                    actual jiter =',kiter)
        call tell(myname_,'                       actual ii =',ki)
        call tell(myname_,'                       actual jj =',kj)
        call tell(myname_,'                       lobsensfc =',lobsensfc)
        call tell(myname_,'                  lsensrecompute =',lsensrecompute)
        call tell(myname_,'                       lobserver =',lobserver)
#endif
      endif

    if(.not.ignore_iter_) then
      if (lobsensfc.and..not.lsensrecompute) then       ! a backward iter
        if(kiter/=miter) then
          call perr(myname_,'obsHeader_read_(), unexpected header value, kiter =',kiter)
          call perr(myname_,'                                  expecting miter =',miter)
          call perr(myname_,'                                        lobsensfc =',lobsensfc)
          call perr(myname_,'                                   lsensrecompute =',lsensrecompute)
          call  die(myname_)
        endif

      else if (lobserver) then  ! a forward iter
        if(kiter/=jiter-1) then
          call perr(myname_,'obsHeader_read_(), unexpected header value, kiter =',kiter)
          call perr(myname_,'                                expecting jiter-1 =',jiter-1)
          call perr(myname_,'                                        lobserver =',lobserver)
          call  die(myname_)
        endif

      else
        if(kiter/=jiter) then   ! the same iter
          call perr(myname_,'obsHeader_read_(), unexpected header value, kiter =',kiter)
          call perr(myname_,'                                  expecting jiter =',jiter)
          call perr(myname_,'                                        lobserver =',lobserver)
          call  die(myname_)
        endif
      endif
    endif
    jread=kiter


      !-- construct an an_obsNode
      leadNode => null()
      aNode => obsNode_alloc_(miter)
      do kk=1,kobs
        !-- initialize an_obsNode from a file (iunit).  Use of miter suggests a
        !-- fixed input size.
        call obsNode_read_(aNode,iunit,miter,istat,redistr=redistr)
                if(istat<0) then
                  call perr(myname_,'obsNode_read_(), istat =',istat)
                  call perr(myname_,'               redistr =',redistr)
                  call  die(myname_)
                endif

                ! istat <0:     a failed read(aNode)
                !      ==0:     passed, thus an incomplete aNode
                !       >0:     a good aNode to keep
        if(istat==0) cycle
        if(redistr) call obsNode_setluse_(aNode)

                ! keep this obsNode in its linked-list, diagLL := obsdiags(jj,ii)
        call obsNode_append_(diagLL,aNode)
                !-- mark the beginning of this linked-list segment
        if(.not.associated(leadNode)) leadNode => aNode

                !-- drop this aNode, to construct a new.  This _alloc_
                !   ensures an aNode is not in anyway referencible to
                !   the one that has been appended to the linked-list.
                !   Then, a deep-deallocation of aNode is alwasy safe.
        aNode => obsNode_alloc_(miter)
      enddo  ! < kobs >
      call obsNode_dealloc_(aNode,deep=.true.)  ! Clean up the malloc of aNode

! ----------------------------------------------------------
!call timer_fnl(myname_)
!_TIMER_OFF_(myname_)
_EXIT_(myname_)
return
end subroutine lread_

subroutine lreset_(diagLL)
  implicit none
  type(_obsLList_), intent(inout):: diagLL

  character(len=*),parameter:: myname_=myname//"::lreset_"
  type(_obsNode_),pointer:: l_obsNode
  type(_obsNode_),pointer:: n_obsNode
  integer(kind=i_kind):: ip
_ENTRY_(myname_)

  l_obsNode => obsNode_first_(diagLL)
  ip=0
  do while(associated(l_obsNode))
    ip=ip+1
    !_TRACEV_(myname_,'deallocating at ip =',ip)
    !call obsNode_check_(myname_,l_obsNode)
        ! Steps of forward resetting,
        !   (1) hold the %next node,
        !   (2) clean (leaving the %next node untouched,
        !   (3) deallocate the current node,
        !   (4) point the starting point to the %next node.
    n_obsNode => obsNode_next_(diagLL)
    call obsNode_dealloc_(l_obsNode,deep=.true.)
    l_obsNode => n_obsNode
  enddo
  !n_obsNode   => null()
  !l_obsNode   => null()

  diagLL%n_alloc = 0
  diagLL%head => null()
  diagLL%tail => null()
  if(allocated(diagLL%lookup)) deallocate(diagLL%lookup)

_EXIT_(myname_)
return
end subroutine lreset_

subroutine lchecksum_(diagLL,leadNode,itype,ibin,sorted)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lchecksum_
!   prgmmr:      J. Guo
!
! abstract: check the size values against a known counts.
!
! program history log:
!   2015-06-26  guo     - 
!
!   input argument list: (see Fortran declarations below)
!
!   output argument list: (see Fortran declarations below)
!
! attributes:
!   language: f90/f95/f2003/f2008
!   machine:
!
!$$$ end documentation block
  use mpeu_util, only: stdout
  use mpeu_util, only: stdout_lead
  implicit none
  type(_obsLList_), intent(in):: diagLL
  type(_obsNode_ ), pointer, optional, intent(in):: leadNode
  integer(kind=i_kind),optional,intent(in ):: itype
  integer(kind=i_kind),optional,intent(in ):: ibin
  logical             ,optional,intent(out):: sorted

  character(len=*),parameter:: myname_=MYNAME//"::lchecksum_"
  integer(kind=i_kind):: jtype,jbin
  integer(kind=i_kind):: mcount
  integer(kind=i_kind):: nuse,nooo,ndup
  integer(kind=i_kind),dimension(3):: ksum
!jtest
!  logical:: lasso,lhead

_ENTRY_(myname_)
!jtest
!  ASSERT(present(leadNode))
!  lasso=associated(leadNode)
!  lhead=associated(diagLL%head,leadNode)

  mcount=lcount_(diagLL,recount=.true.,nuse=nuse,nooo=nooo,ndup=ndup,ksum=ksum,leadNode=leadNode)
  if(present(sorted)) sorted = nooo==0.and.ndup==0

!jtest
!  if(mcount/=diagLL%n_alloc) then
!    call perr(myname_,'checksum failed, mcount =',mcount)
!    call perr(myname_,'      diagLList%n_alloc =',diagLL%n_alloc)
!    if(present(itype)) &
!    call perr(myname_,'                  itype =',itype)
!    if(present(ibin)) &
!    call perr(myname_,'                   ibin =',ibin)
!    call  die(myname_)
!  endif

   if(present(itype)) jtype=itype
   if(present(ibin))  jbin =ibin
#ifdef DEBUG_TRACE
  if(diagLL%n_alloc>0) then
    jtype=0
    jbin =0
    if(present(itype)) jtype=itype
    if(present(ibin )) jbin =ibin

    if(nooo/=0.or.ndup/=0) then
      write(stdout,'(a,2x,2i4,5i6,3i10,a)') stdout_lead(myname_,''),jtype,jbin,diagLL%n_alloc,mcount,nuse,nooo,ndup,ksum,' >>> WARNING <<<'
    else
      write(stdout,'(a,2x,2i4,5i6,3i10  )') stdout_lead(myname_,''),jtype,jbin,diagLL%n_alloc,mcount,nuse,nooo,ndup,ksum
    endif
  endif
#endif /* DEBUG_TRACE */
_EXIT_(myname_)
return
end subroutine lchecksum_
subroutine lchecksum1_(diagLL,leadNode,itype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lchecksum_
!   prgmmr:      J. Guo
!
! abstract: check the size values against a known counts.
!
! program history log:
!   2015-06-26  guo     - 
!
!   input argument list: (see Fortran declarations below)
!
!   output argument list: (see Fortran declarations below)
!
! attributes:
!   language: f90/f95/f2003/f2008
!   machine:
!
!$$$ end documentation block
  implicit none
  type(_obsLList_), dimension(:),intent(in):: diagLL
  integer(kind=i_kind),optional,intent(in):: itype
  type(fptr_obsdiagNode),optional,dimension(:),intent(in):: leadNode

  character(len=*),parameter:: myname_=MYNAME//"::lchecksum1_"
  integer(kind=i_kind):: i
_ENTRY_(myname_)
  if(present(leadNode)) then
    ASSERT(size(diagLL)==size(leadNode))
    do i=1,size(diagLL)
      call lchecksum_(diagLL(i),itype=itype,ibin=i,leadNode=leadNode(i)%node)
    enddo
  else
    do i=1,size(diagLL)
      call lchecksum_(diagLL(i),itype=itype,ibin=i)
    enddo
  endif
_EXIT_(myname_)
return
end subroutine lchecksum1_
subroutine lchecksum2_(diagLL)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lchecksum_
!   prgmmr:      J. Guo
!
! abstract: check the size values against a known counts.
!
! program history log:
!   2015-06-26  guo     - 
!
!   input argument list: (see Fortran declarations below)
!
!   output argument list: (see Fortran declarations below)
!
! attributes:
!   language: f90/f95/f2003/f2008
!   machine:
!
!$$$ end documentation block
  implicit none
  type(_obsLList_), dimension(:,:),intent(in):: diagLL

  character(len=*),parameter:: myname_=MYNAME//"::lchecksum2_"
  integer(kind=i_kind):: it,ib
_ENTRY_(myname_)
  do it=1,size(diagLL,1)
  do ib=1,size(diagLL,2)
    call lchecksum_(diagLL(it,ib),itype=it,ibin=ib)
  enddo
  enddo
_EXIT_(myname_)
return
end subroutine lchecksum2_

subroutine lsummary_(diagLL,verbose)
  implicit none
  type(_obsLList_), intent(in):: diagLL
  logical,optional, intent(in):: verbose

  character(len=*),parameter:: myname_=MYNAME//"::lsummary_"
  type(_obsNode_ ), pointer:: iNode
  type(_obsLList_), target :: tempLL
  integer(kind=i_kind):: iobs_
  logical:: verbose_
  verbose_=.false.
  if(present(verbose)) verbose_=verbose
_ENTRY_(myname_)
#ifdef DEBUG_TRACE
  _TRACEV_(myname_,' depth-of(diagLL) =',diagLL%n_alloc)
  if(allocated(diagLL%lookup)) &
  _TRACEV_(myname_,'    size(%lookup) =',size(diagLL%lookup))
#endif

  if(verbose_) then
    tempLL = diagLL
    iobs_ = 0
    iNode => obsNode_first_(tempLL)
    do while(associated(iNode))
      iobs_=iobs_+1
      call obsNode_show_(iNode,iobs_)
      iNode => obsNode_next_(tempLL)
    enddo
  endif
_EXIT_(myname_)
return
end subroutine lsummary_

function lsize_(diagLL) result(lobs_)
  implicit none
  integer(kind=i_kind):: lobs_
  type(_obsLList_),   target, intent(in):: diagLL
  lobs_=diagLL%n_alloc
end function lsize_

function lcount_(diagLL,luseonly,recount,nuse,nooo,ndup,ksum,leadNode) result(lobs_)
  implicit none
  integer(kind=i_kind):: lobs_
  type(_obsLList_),   target, intent(in):: diagLL
  logical         , optional, intent(in):: luseonly
  logical         , optional, intent(in):: recount
  integer(kind=i_kind),optional,intent(out):: nuse      ! no. of luse
  integer(kind=i_kind),optional,intent(out):: nooo      ! no. out-of-orders
  integer(kind=i_kind),optional,intent(out):: ndup      ! no. duplicates
  integer(kind=i_kind),optional,dimension(:),intent(out):: ksum ! key value sum
  type(_obsNode_ ), pointer, optional, intent(in):: leadNode

  character(len=*),parameter:: myname_=myname//"::lcount_"
  type(_obsNode_ ), pointer:: iNode
  type(_obsLList_), target :: tempLL
  integer(kind=i_kind):: nuse_
  integer(kind=i_kind):: k
  integer(kind=i_kind),dimension(3) :: kprev
  logical:: luseonly_,recount_,checksum_
_ENTRY_(myname_)

  luseonly_=.false.
  if(present(luseonly)) luseonly_=luseonly
  recount_ =.false.
  if(present(recount )) recount_ =recount
  if(present(leadNode)) recount_ =.true.

  checksum_= present(nuse).or.present(nooo).or.present(ndup).or.present(ksum)
  recount_ = recount_ .or. checksum_
  !if(.not.recount_) recount_ = checksum_

  if(present(ksum)) then
    ALWAYS_ASSERT(size(ksum)==size(kprev))
  endif

  if(.not.(luseonly_.or.recount_)) then
    lobs_=diagLL%n_alloc

  else  ! recount through the list
    tempLL = diagLL	! A copy of diagLL, such that diagLL can remain intent(in)

    lobs_ = 0
    nuse_ = 0

    if(checksum_) call checksum_init_(kprev,nooo=nooo,ndup=ndup,ksum=ksum)

    iNode => obsNode_first_(tempLL,atNode=leadNode)
    do while(associated(iNode))
      if(obsNode_isluse_(iNode)) nuse_=nuse_+1
      if(.not.luseonly_ .or. obsNode_isluse_(iNode)) lobs_=lobs_+1

      if(checksum_) call checksum_add_(kprev, &
        (/iNode%idv,iNode%iob,iNode%ich/),nooo=nooo,ndup=ndup,ksum=ksum)

      iNode => obsNode_next_(tempLL)
    enddo
    if(present(nuse)) nuse=nuse_
  endif

_EXIT_(myname_)
return
contains
subroutine checksum_init_(kprev,nooo,ndup,ksum)
  implicit none
  integer(kind=i_kind),dimension(:),intent(out):: kprev
  integer(kind=i_kind),optional,intent(out):: nooo
  integer(kind=i_kind),optional,intent(out):: ndup
  integer(kind=i_kind),optional,dimension(:),intent(out):: ksum

  kprev(:)= 0
  if(present(nooo)) nooo=0
  if(present(ndup)) ndup=0
  if(present(ksum)) ksum(:)=0
end subroutine checksum_init_
subroutine checksum_add_(kprev,knext,nooo,ndup,ksum)
  implicit none
  integer(kind=i_kind),dimension(:),intent(inout):: kprev
  integer(kind=i_kind),dimension(:),intent(in   ):: knext
  integer(kind=i_kind),optional,intent(inout):: nooo
  integer(kind=i_kind),optional,intent(inout):: ndup
  integer(kind=i_kind),optional,dimension(:),intent(inout):: ksum

  k=compare_(kprev,knext)
  if(present(nooo).and.k> 0) nooo=nooo+1
  if(present(ndup).and.k==0) ndup=ndup+1
  if(present(ksum)) ksum(:)=ksum(:)+knext(:)
  kprev(:)=knext(:)
end subroutine checksum_add_
end function lcount_

function obsNode_first_(diagLL,atNode) result(here_)
  implicit none
  type(_obsNode_ ), pointer	:: here_
  type(_obsLList_), target, intent(inout):: diagLL
  type(_obsNode_ ), optional, pointer,intent(in):: atNode

  character(len=*),parameter:: myname_=myname//"::obsNode_first_"
_ENTRY_(myname_)
  !_TRACEV_(myname_,'%n_alloc =',diagLL%n_alloc)
  !_TRACEV_(myname_,'associated(%head) =',associated(diagLL%head))
  here_ => diagLL%head
  if(present(atNode)) here_=>atNode
  diagLL%tail => here_  ! update the tail-node

  if(associated(here_)) call obsNode_check_(myname_,here_)
_EXIT_(myname_)
return
end function obsNode_first_

function obsNode_next_(diagLL,atNode) result(next_)
  implicit none
  type(_obsNode_ ), pointer      :: next_
  type(_obsLList_), target, intent(inout):: diagLL
  type(_obsNode_ ), optional, pointer,intent(in):: atNode

  character(len=*),parameter:: myname_=myname//"::obsNode_next_"
_ENTRY_(myname_)
  next_ => diagLL%tail%next
  if(present(atNode)) next_=>atNode%next
  diagLL%tail => next_  ! update the tail-node
_EXIT_(myname_)
return
end function obsNode_next_

subroutine obsNode_append_(diagLL,targetNode)
        ! Link the next node of the list to the given targetNode.  The return
        ! result is a pointer associated to the same targetNode.
  use jfunc, only: miter
  implicit none
  type(_obsLList_), intent(inout):: diagLL
  type(_obsNode_ ), target, intent(in):: targetNode

  character(len=*),parameter:: myname_=myname//"::obsNode_append_"
  type(_obsNode_ ),pointer:: aNode
_ENTRY_(myname_)
  if(.not.associated(diagLL%head)) then
                ! this is a fresh starting -node- for this linked-list ...
    diagLL%n_alloc = 1
    diagLL%head => targetNode
    diagLL%tail => diagLL%head

  else
                ! this is for a new next -node- from here ...
    diagLL%n_alloc = diagLL%n_alloc +1
    diagLL%tail%next => targetNode
    diagLL%tail      => diagLL%tail%next

    !diagLL%tail%append(next_)
    !    append(t,next_)
    !            t%next => next_
    !            t      => t%next
  endif
  if(associated(diagLL%tail)) diagLL%tail%next => null()

  aNode => diagLL%tail
  ASSERT(lbound(aNode%muse    ,1)==1.and.ubound(aNode%muse    ,1)==miter+1)
  ASSERT(lbound(aNode%nldepart,1)==1.and.ubound(aNode%nldepart,1)==miter+1)
  ASSERT(lbound(aNode%tldepart,1)==1.and.ubound(aNode%tldepart,1)==miter  )
  ASSERT(lbound(aNode%obssen  ,1)==1.and.ubound(aNode%obssen  ,1)==miter  )
  aNode => null()

_EXIT_(myname_)
return
end subroutine obsNode_append_

subroutine lsort_(diagLL,itype,ibin)
!       lsort_: node-sort diagLL, to line-up nodes according to their keys
!_DEBUG_USE_
!_TIMER_USE_
!  use timermod , only: timer_ini,timer_fnl
  !use mpeu_util, only: IndexSet
  !use mpeu_util, only: IndexSort
  !use mpeu_util, only: die
  implicit none
  type(_obsLList_) , intent(inout):: diagLL
  integer(kind=i_kind),optional,intent(in):: itype,ibin

  character(len=*),parameter:: myname_=myname//'::lsort_'
  integer(kind=i_kind):: i,nobs,mobs
  logical:: sorted
_ENTRY_(myname_)
!_TIMER_ON_(myname_)
!  call timer_ini(myname_)

  call lchecksum_(diagLL,itype=itype,ibin=ibin,sorted=sorted)
        if(sorted) then
          _EXIT_(myname_)
          return
        endif

        ! created a sorted table
  call lbuild_(diagLL)

  nobs = diagLL%n_alloc
  mobs = size(diagLL%lookup(:))
        ASSERT(nobs==mobs)

        ! rebuild the linked-list
  diagLL%n_alloc=0
  diagLL%head => null()
  diagLL%tail => null()

        ! rebuild the list according to the sorted table
  do i=1,mobs
    call obsNode_append_(diagLL,diagLL%lookup(i)%ptr)
  enddo
        ASSERT(nobs==diagLL%n_alloc)
        if(associated(diagLL%tail)) then
          ASSERT(.not.associated(diagLL%tail%next))
        endif

        ! discard the sorted table
  call lclean_(diagLL)

  call lchecksum_(diagLL,itype=itype,ibin=ibin,sorted=sorted)
        if(.not.sorted) then
          call perr(myname_,'failed post-sorting lchecksum_(diagLL), sorted =',sorted)
          if(present(itype)) &
          call perr(myname_,'                                         itype =',itype)
          if(present(ibin )) &
          call perr(myname_,'                                          ibin =',ibin )
          call  die(myname_)
        endif

!  call timer_fnl(myname_)
!_TIMER_OFF_(myname_)
_EXIT_(myname_)
return
end subroutine lsort_

subroutine lbuild_(diagLL,leadNode,jiter)
!_DEBUG_USE_
!_TIMER_USE_
!  use timermod , only: timer_ini,timer_fnl
  use mpeu_util, only: IndexSet
  use mpeu_util, only: IndexSort
  !use mpeu_util, only: die
  implicit none
  type(_obsLList_), intent(inout):: diagLL
  type(_obsNode_ ), pointer, optional, intent(in):: leadNode
  integer(i_kind) , optional, intent(in):: jiter

  character(len=*),parameter:: myname_=myname//'::lbuild_'
  type(_obsNode_),pointer:: iNode,pNode
  integer(kind=i_kind),allocatable,dimension(:):: indx,idv_,iob_,ich_
  integer(kind=i_kind):: i,m,n
  integer(kind=i_kind):: idum
  logical:: good
_ENTRY_(myname_)
!_TIMER_ON_(myname_)
!  call timer_ini(myname_)
  if(present(jiter)) idum=jiter

        ! Mark the leading node
  iNode => null()
  if(present(leadNode)) iNode => leadNode
  if(.not.associated(iNode)) iNode => diagLL%head

  m=diagLL%n_alloc
  if(m<0) call die(myname_,'unexpected diagLL, %n_alloc =',m)

        ! Count, starting from the leading node
  n=0
  pNode => iNode
  do while(associated(pNode))
    n=n+1
    pNode => pNode%next
  enddo

  if(n>diagLL%n_alloc) then
    call perr(myname_,'unexpected diagLL, %n_alloc =',m)
    call  die(myname_,'               actual count =',n)
  endif

  allocate(diagLL%lookup(n))
  allocate(indx(n),idv_(n),iob_(n),ich_(n))

  associate(lookup => diagLL%lookup(:))
        ! Loop over the linked-list, to get keys.
    i=0
    pNode => iNode
    do while(associated(pNode))
      i=i+1
      if(i<=n) then
        lookup(i)%ptr => pNode
          idv_(i)     =  pNode%idv
          iob_(i)     =  pNode%iob
          ich_(i)     =  pNode%ich
        !call obsNode_get(idv=idv_(i),iob=iob_(i),ich=ich_(i))
      endif
      pNode => pNode%next
    enddo
  end associate

        ! sort %lookup(1:n), by its (idv,iob,ich) values
  call IndexSet (indx)
  call IndexSort(indx,ich_)
  call IndexSort(indx,iob_)
  call IndexSort(indx,idv_)

  associate(lookup => diagLL%lookup(:))
    lookup(1:n) = lookup(indx(1:n))
  end associate

!#ifdef DEBUG_VERIFY
    idv_(1:n) = idv_(indx(1:n))
    iob_(1:n) = iob_(indx(1:n))
    ich_(1:n) = ich_(indx(1:n))

    associate(lookup => diagLL%lookup(:))
      good = .true.
      do i=1,n
        good = lookup(i)%ptr%idv==idv_(i) .and. &
               lookup(i)%ptr%iob==iob_(i) .and. &
               lookup(i)%ptr%ich==ich_(i)
        if(.not.good) exit
      enddo

      if(.not.good) then
        call perr(myname_,'verification failed at %lookup(i)%ptr,  i =',i)
        call perr(myname_,'                                 %ptr%idv =',lookup(i)%ptr%idv)
        call perr(myname_,'                                      idv_=',idv_(i))
        call perr(myname_,'                                 %ptr%iob =',lookup(i)%ptr%iob)
        call perr(myname_,'                                      iob_=',iob_(i))
        call perr(myname_,'                                 %ptr%ich =',lookup(i)%ptr%ich)
        call perr(myname_,'                                      ich_=',ich_(i))
        call die(myname_)
      endif
    end associate
!#endif

  deallocate(indx,idv_,iob_,ich_)

!  call timer_fnl(myname_)
!_TIMER_OFF_(myname_)
_EXIT_(myname_)
return
end subroutine lbuild_

subroutine lclean_(diagLL)
  implicit none
  type(_obsLList_), intent(inout):: diagLL

  character(len=*),parameter:: myname_=myname//'::lclean_'
  integer(kind=i_kind):: ier,i
_ENTRY_(myname_)
  associate(lookup => diagLL%lookup(:))
    do i=1,size(lookup)
      lookup(i)%ptr => null()
    end do
  end associate
  deallocate(diagLL%lookup,stat=ier)
        if(ier/=0) call die(myname_,'deallocate(diagLL%lookup), stat =',ier)
_EXIT_(myname_)
return
end subroutine lclean_

function locate_(diagLL,idv,iob,ich) result(here_)
  use timermod , only: timer_ini,timer_fnl
  implicit none
  type(_obsNode_ ), pointer:: here_
  type(_obsLList_), intent(in):: diagLL
  integer(kind=i_kind), intent(in):: idv,iob,ich

  character(len=*),parameter:: myname_=myname//"::locate_"
  type(_obsNode_ ),pointer:: idiag
  integer(kind=i_kind):: m,i,lb,ub
  logical:: done
_ENTRY_(myname_)
  call timer_ini(myname_)

  here_ => null()     ! return null() if the key is not located.

  associate(lookup => diagLL%lookup(:))
    lb=lbound(lookup,1)
    ub=ubound(lookup,1)
    done=.false.
    do while(.not.done)
      i=(lb+ub)/2
      idiag => lookup(i)%ptr

      m=compare_((/idiag%idv,idiag%iob,idiag%ich/),(/idv,iob,ich/))
      done = m==0
      if(done) exit

        ! We are searching for EQUAL, so skip the i-th point if not equal.
      if(m<0) then
        ! if idiag%(idv,iob,ich) < (/idv,iob,ich/), move the lower range (lb) up
        ! to continue the search above i
        lb=i+1
      else
        ! if idiag%(idv,iob,ich) > (/idv,iob,ich/), move the upper range (ub) down
        ! to continue the search below i.
        ub=i-1
      endif

      if(ub<lb) exit      ! termionate the search
    enddo
  end associate

  if(done) then
    here_ => idiag
  endif

  call timer_fnl(myname_)
_EXIT_(myname_)
return
end function locate_

function compare_(key1,key2) result (m)
  implicit none
  integer(kind=i_kind):: m
  integer(kind=i_kind),dimension(:),intent(in):: key1,key2

  integer(kind=i_kind):: n,i
  m=0
  n=min(size(key1),size(key2))
  do i=1,n
    if    (key1(i)<key2(i)) then
      m=-1; exit
    elseif(key1(i)>key2(i)) then
      m=+1; exit
    endif
  enddo
end function compare_

!-------------------
function obsNode_islocal_(aNode) result(islocal_)
  use mpimod, only: myPE
  use m_cvgridLookup, only: cvgridLookup_islocal
  implicit none
  logical:: islocal_
  type(_obsNode_),intent(in):: aNode

  character(len=*),parameter:: myname_=myname//"::obsNode_islocal_"
_ENTRY_(myname_)
  islocal_=cvgridLookup_islocal(aNode%elat,aNode%elon,myPE)
_EXIT_(myname_)
return
end function obsNode_islocal_

function obsNode_isluse_(aNode) result(isluse_)
  implicit none
  logical:: isluse_
  type(_obsNode_),intent(in):: aNode

  character(len=*),parameter:: myname_=myname//"::obsNode_isluse_"
_ENTRY_(myname_)
  isluse_=aNode%luse
_EXIT_(myname_)
return
end function obsNode_isluse_

subroutine obsNode_setluse_(aNode)
  use mpimod, only: myPE
  use m_cvgridLookup, only: cvgridLookup_isluse
  implicit none
  type(_obsNode_),intent(inout):: aNode

  character(len=*),parameter:: myname_=myname//"::obsNode_setluse_"
_ENTRY_(myname_)
  aNode%luse=cvgridLookup_isluse(aNode%elat, aNode%elon, myPE)
  !    call obstype_setluse(aNode%luse, aNode%elat, aNode%elon, myPE)
_EXIT_(myname_)
return
end subroutine obsNode_setluse_

subroutine obsHeader_read_(iunit,ii_bin,jj_type,lobs,jiter,miter,istat)
  implicit none
  integer(kind=i_kind),intent(in ):: iunit
  integer(kind=i_kind),intent(out):: ii_bin,jj_type,lobs,jiter,miter
  integer(kind=i_kind),intent(out):: istat

  character(len=*),parameter:: myname_=myname//"::obsHeader_read_"
_ENTRY_(myname_)
  read(iunit,iostat=istat) ii_bin,jj_type,lobs,jiter,miter
_EXIT_(myname_)
return
end subroutine obsHeader_read_

subroutine obsHeader_write_(iunit,ii_bin,jj_type,lobs,jiter,miter,istat)
  implicit none
  integer(kind=i_kind),intent(in ):: iunit
  integer(kind=i_kind),intent(in ):: ii_bin,jj_type,lobs,jiter,miter
  integer(kind=i_kind),intent(out):: istat

  character(len=*),parameter:: myname_=myname//"::obsHeader_write_"
_ENTRY_(myname_)
  write(iunit,iostat=istat) ii_bin,jj_type,lobs,jiter,miter
_EXIT_(myname_)
return
end subroutine obsHeader_write_

subroutine obsNode_check_(who,aNode)
  use jfunc, only: miter        ! for debugging
  implicit none
  character(len=*),intent(in):: who
  type(_obsNode_),intent(in):: aNode

  logical:: equival
  character(len=256)::mywho

  mywho=who
    !_TRACEV_(who,'associated(aNode%muse    ) =',associated(aNode%muse    ))
    !_TRACEV_(who,'associated(aNode%nldepart) =',associated(aNode%nldepart))
    !_TRACEV_(who,'associated(aNode%tldepart) =',associated(aNode%tldepart))
    !_TRACEV_(who,'associated(aNode%obssen  ) =',associated(aNode%obssen  ))

  equival = associated(aNode%nldepart) .eqv. associated(aNode%muse    )
  if(equival) equival = associated(aNode%tldepart) .eqv. associated(aNode%nldepart)
  if(equival) equival = associated(aNode%obssen  ) .eqv. associated(aNode%tldepart)
  if(equival) equival = associated(aNode%muse)

  ASSERT(equival)

  ASSERT(lbound(aNode%muse    ,1)==1.and.ubound(aNode%muse    ,1)==miter+1)
  ASSERT(lbound(aNode%nldepart,1)==1.and.ubound(aNode%nldepart,1)==miter+1)
  ASSERT(lbound(aNode%tldepart,1)==1.and.ubound(aNode%tldepart,1)==miter  )
  ASSERT(lbound(aNode%obssen  ,1)==1.and.ubound(aNode%obssen  ,1)==miter  )

return
end subroutine obsNode_check_

function obsNode_alloc_(miter) result(aNode_)
  implicit none
  type(_obsNode_), pointer   :: aNode_
  integer(kind=i_kind), intent(in):: miter

  character(len=*),parameter:: myname_=myname//"::obsNode_alloc_"
_ENTRY_(myname_)
  allocate(aNode_)
  aNode_%next => null()

  allocate(aNode_%muse    (miter+1), &
           aNode_%nldepart(miter+1), &
           aNode_%tldepart(miter  ), &
           aNode_%obssen  (miter  )  )

  aNode_%luse = .false.
  aNode_%elat = 0._r_kind
  aNode_%elon = 0._r_kind
  aNode_%idv  =-1
  aNode_%iob  =-1
  aNode_%ich  =-1

  aNode_%indxglb    =-99999
  aNode_%nchnperobs =-99999
  aNode_%muse    (:)= .false.
  aNode_%nldepart(:)=-huge(0._r_kind)
  aNode_%tldepart(:)= 0._r_kind
  aNode_%wgtjo      =-huge(0._r_kind)
  aNode_%obssen  (:)= 0._r_kind

  call obsNode_check_(myname_,aNode_)
_EXIT_(myname_)
return
end function obsNode_alloc_

subroutine obsNode_read_(aNode,iunit,kiter,istat,redistr)
  implicit none
  type(_obsNode_), intent(inout):: aNode
  integer(kind=i_kind), intent(in   ):: iunit
  integer(kind=i_kind), intent(in   ):: kiter   ! input size
  integer(kind=i_kind), intent(out  ):: istat
  logical        , intent(in   ):: redistr

  character(len=*),parameter:: myname_=myname//'::obsNode_read_'
  integer(kind=i_kind):: ier
  !real(kind=r_kind),dimension(1:kiter):: zobssen
_ENTRY_(myname_)

  istat=0
  read(iunit,iostat=ier) aNode%luse,aNode%elat,aNode%elon, &
                         aNode%idv ,aNode%iob ,aNode%ich
        if(ier/=0) then
          call perr(myname_,'read(%luse,%elat,%elon,...), iostat =',ier)
          istat=-1
          _EXIT_(myname_)
          return
        endif

  istat=1
  if(redistr) then
    istat=0
    if(aNode%luse) then
      if(obsNode_islocal_(aNode)) istat=1
    endif
  endif

  if(istat==0) then
    read(iunit,iostat=ier)
        if(ier/=0) then
          call perr(myname_,'skipping read(%indxglb,%nchnperobs,%muse,...), iostat =',ier)
          istat=-2
          _EXIT_(myname_)
          return
        endif

  else
    read(iunit,iostat=ier)       &
        aNode%indxglb,           &    ! = kindx
        aNode%nchnperobs,        &    ! = mchanl
        aNode%muse    (1:kiter+1), &    ! = lmuse(1:kiter)
        aNode%nldepart(1:kiter+1), &    ! = znldepart(1:kiter)
        aNode%tldepart(1:kiter), &    ! = ztldepart(1:kiter)
        aNode%wgtjo,             &    ! = zwgtjo
        aNode%obssen  (1:kiter)       ! = zobssen(1:kiter)
        if(ier/=0) then
          call perr(myname_,'read(%indxglb,%nchnperobs,%muse,...), iostat =',ier)
          istat=-3
          _EXIT_(myname_)
          return
        endif

!        if    (lobsensfc.and..not.lsensrecompute) then
!          aNode%obssen(jiter+1:miter  )=zobssen(jiter+1:miter  )
!        elseif(lobserver) then
!          aNode%obssen(      1:jiter-1)=zobssen(      1:jiter-1)
!        else
!          aNode%obssen(      1:miter  )=zobssen(      1:miter  )
!        endif
  endif

  call obsNode_check_(myname_,aNode)
_EXIT_(myname_)
return
end subroutine obsNode_read_

subroutine obsNode_write_(aNode,iunit,jiter,istat)
  implicit none
  type(_obsNode_), intent(in   ):: aNode
  integer(kind=i_kind), intent(in   ):: iunit
  integer(kind=i_kind), intent(in   ):: jiter   ! the output size
  integer(kind=i_kind), intent(inout):: istat

  character(len=*),parameter:: myname_=myname//'::obsNode_write_'
_ENTRY_(myname_)

  write(iunit,iostat=istat) aNode%luse,aNode%elat,aNode%elon, &
                            aNode%idv,aNode%iob,aNode%ich
        if(istat/=0) then
          call perr(myname_,'write(%luse,%elat,%elon,...), iostat =',istat)
          _EXIT_(myname_)
          return
        endif

  write(iunit,iostat=istat)     &
        aNode%indxglb,          &
        aNode%nchnperobs,       &
        aNode%muse    (1:jiter+1),&
        aNode%nldepart(1:jiter+1),&
        aNode%tldepart(1:jiter),&
        aNode%wgtjo,            &
        aNode%obssen(1:jiter)

        if(istat/=0) then
          call perr(myname_,'write(%indxglb,%nchnperobs,%muse,...), iostat =',istat)
          _EXIT_(myname_)
          return
        endif
  call obsNode_check_(myname_,aNode)
_EXIT_(myname_)
return
end subroutine obsNode_write_

subroutine obsNode_dealloc_(aNode,deep)
  implicit none
  type(_obsNode_),pointer,intent(inout):: aNode
  logical,optional,intent(in):: deep

  character(len=*),parameter:: myname_=myname//'::obsNode_dealloc_'
  logical:: deep_
_ENTRY_(myname_)
  call obsNode_check_(myname_,aNode)

  deep_=.false.
  if(present(deep)) deep_=deep
        ASSERT(associated(aNode))

!  _TRACEV_(myname_,'if(deep_), deep_ =',deep_)
  if(deep_) then
!    _TRACEV_(myname_,'associated(aNode%nldepart) =',associated(aNode%nldepart))
    if(associated(aNode%nldepart)) deallocate(aNode%nldepart)
!    _TRACEV_(myname_,'associated(aNode%tldepart) =',associated(aNode%tldepart))
    if(associated(aNode%tldepart)) deallocate(aNode%tldepart)
!    _TRACEV_(myname_,'associated(aNode%obssen  ) =',associated(aNode%obssen  ))
    if(associated(aNode%obssen  )) deallocate(aNode%obssen  )
!    _TRACEV_(myname_,'associated(aNode%muse    ) =',associated(aNode%muse    ))
    if(associated(aNode%muse    )) deallocate(aNode%muse    )
  endif
    ! This is NOT a recursive dealloc_().  Therefore, the associated target of
    ! %next is not deallocated, but only %next itself is nullified.
!  _TRACEV_(myname_,'associated(%next) =',associated(aNode%next))
  aNode%next => null()
!  _TRACEV_(myname_,'associated(%next) =',associated(aNode%next))
  deallocate(aNode)
!  _TRACEV_(myname_,'associated(aNode) =',associated(aNode))
_EXIT_(myname_)
return
end subroutine obsNode_dealloc_

subroutine obsNode_show_(aNode,iob)
  use mpeu_util, only: stdout
  implicit none
  type(_obsNode_),intent(in):: aNode
  integer(kind=i_kind),intent(in):: iob

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_show_'
_ENTRY_(myname_)
  write(stdout,'(2a,5i4,l4,2f8.2)') myname,":: iob,ity,%(idv,iob,ich,luse,elat,elon) =", &
        iob,0,aNode%idv,aNode%iob,aNode%ich,aNode%luse,aNode%elat,aNode%elon
  call obsNode_check_(myname_,aNode)
_EXIT_(myname_)
return
end subroutine obsNode_show_

end module m_obsdiagNode
