module m_obsdiags
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module m_obsdiags
!   prgmmr:      j guo <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:      2015-02-04
!
! abstract: a bundle of GSI multiple obstypes and the obsdiags linked-lists
!
! program history log:
!   2015-02-04  j guo   - Re-implemented read_obsdiags() and write_obsdiags(),
!                         to support reconfigurable observation operators.  This
!                         implemenstation uses an obsLList template to support,
!                         in ceterian degree, static polymoprhism for different
!                         observation types.
!   2015-10-09  j guo   - By using Fortran 2003 dynamic polymorphism, this
!                         version has removed many ugly type dispatching SELECT
!                         CASE constructs, by using an obsLList, a linked-list
!                         of dynamic polymorphic observation type (obsNode),
!                         which replaced the earlier obsLList template.
!   2016-06-22  j guo   - Added latlonRange for selected file readings, to let
!                         []_mread() to skip unnecessary read() of some files
!                         containing no relevant observations.
!                       . Added obsdiags_alwaysLocal, as a user controlable
!                         switch to allow or to bypass selected file readings.
!                       . Added CHECK_SIZES_ outputs to allow size checkings.
!                       . Added #define SHOW_LLRANGE, for text-dumping of latlonRanges.
!                       . Added #define DEBUG_obsdiags, for text-dumping
!                         specific sections of obsdiags(:,:).
!                       . Locally renamed MPI_comm_world to gsi_comm_world.
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

  use kinds, only: i_kind
  use mpeu_util, only: tell,warn,perr,die
  use mpeu_util, only: assert_
  use mpeu_util, only: stdout_open,stdout_close,stdout
  use mpeu_mpif, only: gsi_comm_world => MPI_comm_world

  use m_obsLList, only: obsLList

  use m_psNode   , only:    psNode !  1
  use m_tNode    , only:     tNode !  2
  use m_wNode    , only:     wNode !  3
  use m_qNode    , only:     qNode !  4
  use m_spdNode  , only:   spdNode !  5
  use m_srwNode  , only:   srwNode !  6
  use m_rwNode   , only:    rwNode !  7
  use m_dwNode   , only:    dwNode !  8
  use m_sstNode  , only:   sstNode !  9
  use m_pwNode   , only:    pwNode ! 10
  use m_pcpNode  , only:   pcpNode ! 11
  use m_ozNode   , only:    ozNode ! 12
  use m_o3lNode  , only:   o3lNode ! 13
  use m_gpsNode  , only:   gpsNode ! 14
  use m_radNode  , only:   radNode ! 15
  use m_tcpNode  , only:   tcpNode ! 16
  use m_lagNode  , only:   lagNode ! 17
  use m_colvkNode, only: colvkNode ! 18
  use m_aeroNode , only:  aeroNode ! 19
  use m_aerolNode, only: aerolNode ! 20
  use m_pm2_5Node, only: pm2_5Node ! 21
  use m_gustNode , only:  gustNode ! 22
  use m_visNode  , only:   visNode ! 23
  use m_pblhNode , only:  pblhNode ! 24

  use m_wspd10mNode , only:  wspd10mNode ! 25
  use m_td2mNode , only:  td2mNode ! 26
  use m_mxtmNode , only:  mxtmNode ! 27
  use m_mitmNode , only:  mitmNode ! 28
  use m_pmslNode , only:  pmslNode ! 29
  use m_howvNode , only:  howvNode ! 30
  use m_tcamtNode, only: tcamtNode ! 31
  use m_lcbasNode, only: lcbasNode ! 32

  use m_pm10Node , only:  pm10Node ! 33
  use m_cldchNode, only:  cldchNode ! 34

  use m_obsNodeTypeManager, only: nobs_type
  use gsi_4dvar           , only: nobs_bins

  use obsmod, only: obsdiags     ! (nobs_type,nobs_bins)
  implicit none
  private       ! except

  public:: obsdiags_reset
  public:: obsdiags_write
  public:: obsdiags_read
  public:: obsdiags_sort

        interface obsdiags_reset; module procedure reset_; end interface
        interface obsdiags_write; module procedure write_; end interface
        interface obsdiags_read ; module procedure mread_; end interface
        interface obsdiags_sort ; module procedure lsort_; end interface

  public:: obsdiags_create
  public:: obsdiags_destroy
        interface obsdiags_create ; module procedure  create_obsmod_vars; end interface
        interface obsdiags_destroy; module procedure destroy_obsmod_vars; end interface

  public:: obsdiags_summary

        interface obsdiags_summary ; module procedure summary_ ; end interface

  public:: obsdiags_alwaysLocal
        logical,save:: obsdiags_alwaysLocal = .false.

! Note: User configurables
!
!   (1) obsdiags_mread(..,mPEs,..) via /SETUP/:mPEs_observer:
!
!       mPEs==0, for reading "my own data";
!       mPEs=>0, reading "all data", from PE 0 to mPEs-1, but only up to the
!                highest count of the actually accessible files.
!
!       This value is configured through gsimod namelist/SETUP/:mPEs_observer,
!       with the default value set to 0, to behave as it was ("my own data").
!       Otherwise, a simple usage is to let mPEs_observer=1024, or other large
!       enough value, such that the solver-mode will try to determine how many
!       files created by the observer-mode are actually there to read.
!
!   (2) obsdiags_alwaysLocal via /SETUP/:alwaysLocal:
!       
!       obsdiags_alwaysLocal sets an alternative default value of the optional
!       argument of obsdiags_mread(..,alwaysLocal).
!
!       obsdiags_alwaysLocal==.false., its default value.
!               It let obsdiags_mread() to check the locality of a file first,
!               using latlonRange_islocal(iPE), to avoid unnecessary opening+
!               reading some files.
!       obsdiags_alwaysLocal==.true., override latlonRange_islocal(iPE).
!               It let obsdiags_mread() to always open+read all file.

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname='m_obsdiags'
  logical,save:: lobsdiags_allocated_ = .false.
  logical,save:: lobstypes_allocated_ = .false.

  logical,parameter:: All_PEs =.false.  ! report status on all PEs or root only
  !logical,parameter:: All_PEs =.true.  ! report status on all PEs or root only
  logical,parameter:: DO_SUMMARY =.false.  ! report status on all PEs or root only
  !logical,parameter:: DO_SUMMARY =.true.  ! report status on all PEs or root only

        ! SYNCH_MESSAGES is a flag to invoke MPI_Barrier() calls before some
        ! status messages.  These calls are otherwise not necessary for the
        ! functionalities, but used here to ensure those messages mean what they
        ! intent to mean, in case that only the root PE is used to report some
        ! all PE status.

  !logical,parameter:: SYNCH_MESSAGES = .true.          ! turn synch on
  !logical,parameter:: SYNCH_MESSAGES = .not. All_PEs   ! conditionally turn on
  logical,parameter:: SYNCH_MESSAGES = .false.          ! turn synch off

  public :: obsdiags
  public :: obsLLists

  public ::    pshead
  public ::   tcphead
  public ::     thead
  public ::     whead
  public ::     qhead
  public ::   spdhead
  public ::   srwhead
  public ::    rwhead
  public ::    dwhead
  public ::   ssthead
  public ::   pcphead
  public ::    pwhead
  public ::    ozhead
  public ::   o3lhead
  public ::  aerohead
  public :: aerolhead
  public :: pm2_5head
  public ::   gpshead
  public ::   radhead
  public ::   laghead
  public :: colvkhead
  public :: gusthead
  public :: vishead
  public :: pblhhead

  public :: wspd10mhead
  public :: td2mhead
  public :: mxtmhead
  public :: mitmhead
  public :: pmslhead
  public :: howvhead
  public :: tcamthead
  public :: lcbashead

  public :: pm10head
  public :: cldchhead

  type(obsLList),dimension(:),pointer :: pshead => null()
  type(obsLList),dimension(:),pointer :: tcphead => null()
  type(obsLList),dimension(:),pointer :: thead => null()
  type(obsLList),dimension(:),pointer :: whead => null()
  type(obsLList),dimension(:),pointer :: qhead => null()
  type(obsLList),dimension(:),pointer :: spdhead => null()
  type(obsLList),dimension(:),pointer :: srwhead => null()
  type(obsLList),dimension(:),pointer :: rwhead => null()
  type(obsLList),dimension(:),pointer :: dwhead => null()
  type(obsLList),dimension(:),pointer :: ssthead => null()
  type(obsLList),dimension(:),pointer :: pcphead => null()
  type(obsLList),dimension(:),pointer :: pwhead => null()
  type(obsLList),dimension(:),pointer :: ozhead => null()
  type(obsLList),dimension(:),pointer :: o3lhead => null()
  type(obsLList),dimension(:),pointer :: aerohead => null()
  type(obsLList),dimension(:),pointer :: aerolhead => null()
  type(obsLList),dimension(:),pointer :: pm2_5head => null()
  type(obsLList),dimension(:),pointer ::   gpshead => null()
  type(obsLList),dimension(:),pointer ::   radhead => null()
  type(obsLList),dimension(:),pointer ::   laghead => null()
  type(obsLList),dimension(:),pointer :: colvkhead => null()
  type(obsLList),dimension(:),pointer ::  gusthead => null()
  type(obsLList),dimension(:),pointer ::   vishead => null()
  type(obsLList),dimension(:),pointer ::  pblhhead => null()

  type(obsLList),dimension(:),pointer :: wspd10mhead => null()
  type(obsLList),dimension(:),pointer :: td2mhead => null()
  type(obsLList),dimension(:),pointer :: mxtmhead => null()
  type(obsLList),dimension(:),pointer :: mitmhead => null()
  type(obsLList),dimension(:),pointer :: pmslhead => null()
  type(obsLList),dimension(:),pointer :: howvhead => null()
  type(obsLList),dimension(:),pointer :: tcamthead => null()
  type(obsLList),dimension(:),pointer :: lcbashead => null()

  type(obsLList),dimension(:),pointer :: pm10head => null()
  type(obsLList),dimension(:),pointer :: cldchhead => null()

  type(obsLList),dimension(:,:),pointer :: obsLLists => null()

  !type(obs_diags),dimension(:,:),pointer :: obsdiags => null()  ! (nobs_type,nobs_bins)


!#define DEBUG_TRACE
#include "mytrace.H"
#include "myassert.H"

#define _TIMER_ON_
#ifdef  _TIMER_ON_
#undef  _TIMER_ON_
#undef  _TIMER_OFF_
#undef  _TIMER_USE_
#define _TIMER_ON_(id)  call timer_ini(id)
#define _TIMER_OFF_(id) call timer_fnl(id)
#define _TIMER_USE_     use timermod, only: timer_ini,timer_fnl
#else
#define _TIMER_ON_(id)
#define _TIMER_OFF_(id)
#define _TIMER_USE_
#endif

  logical,parameter:: CHECK_SIZES_=.false.
  !logical,parameter:: CHECK_SIZES_=.true.

  !-- if(CHECK_SIZES_) then 
  !--   these size counters,

  integer(i_kind),allocatable,dimension(:),save:: lsize_type    !  luse counts of ob_type
  integer(i_kind),allocatable,dimension(:),save:: nsize_type    ! total counts of ob_type
  integer(i_kind),allocatable,dimension(:),save:: lsize_diag    !  luse counts of obs_diags
  integer(i_kind),allocatable,dimension(:),save:: msize_diag    !  muse counts of obs_diags
  integer(i_kind),allocatable,dimension(:),save:: nsize_diag    ! total counts of obs_diags

  !--   will be used to generate extra log-information, reporting different
  !--   size-counts of linked-lists, of all j-type, i-bin, on all PEs.  Search
  !--   "CHECK_SIZES_" here for details.
  !-- endif

contains
subroutine lobsdiags_statusCheck_(who,allocated)
!-- check the allocation status of basic obsdiags components.
  use obsmod, only: luse_obsdiag
  implicit none
  character(len=*),intent(in):: who
  logical,intent(in):: allocated

  if(.not.luse_obsdiag) return
  if(allocated) then
    if( .not.lobsdiags_allocated_ .or. &
        .not.lobstypes_allocated_ ) then
      if(.not.lobsdiags_allocated_) call perr(who,'.not.lobsdiags_allocated_')
      if(.not.lobstypes_allocated_) call perr(who,'.not.lobstypes_allocated_')
      call die(who)
    endif

  else
    if( lobsdiags_allocated_ .or. &
        lobstypes_allocated_ ) then
      if(lobsdiags_allocated_) call perr(who,'lobsdiags_allocated_ already')
      if(lobstypes_allocated_) call perr(who,'lobstypes_allocated_ already')
      call die(who)
    endif
  endif
end subroutine lobsdiags_statusCheck_

subroutine mread_(cdfile,mPEs,force,ignore_iter,alwaysLocal)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    m_obdiags::mread_
!   prgmmr:      tremolet
!
! abstract: Read obsdiags data structure from file.
!
! program history log:
!   2007-07-05  tremolet
!   2007-08-04  todling  - using get_lun to determine file unit number
!   2007-10-03  todling  - expanded to account for full observer 
!   2009-01-08  todling  - remove reference to ozohead
!   2009-01-23  todling  - add read_gpshead
!   2009-04-02  meunier  - add read_laghead
!   2010-04-27  tangborn - addded read_colvkhead
!   2010-05-26  treadon  - add read_tcphead
!   2011-05-18  todling  - aero, aerol, and pm2_5
!   2011-09-20  hclin    - 1d wij for aero
!   2015-02-04  j guo   - Re-implemented to support re-configurable observation
!                         operators.  read_() is split to read_() for a single
!                         file, and mread_() for one file only or all files for
!                         redistribution
!   2015-10-09  j guo   - Now it uses Fortran 2003 dynamic polymorphism.
!
!   input argument list:
!     cdfile - filename to read data from
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use mpeu_util, only: tell,perr,die,stdout_open,stdout_close,stdout
  _TIMER_USE_
  use kinds, only: r_kind,i_kind

  use obsmod, only: lobserver
  use mpimod, only: myPE
  use m_latlonRange, only: latlonRange
  use m_latlonRange, only: latlonRange_reset
  use m_latlonRange, only: latlonRange_islocal
  use m_latlonRange, only: latlonRange_readBcast
  use m_latlonRange, only: latlonRange_allDump

  use m_obsdiagNode, only: obsdiagLList_dump
  implicit none
  character(len=*), intent(in) :: cdfile          ! prefix, "obsdiags.<miter>"
  integer(i_kind),optional,intent(in):: mPEs      ! number of files, from 0 to mPEs-1
  logical        ,optional,intent(in):: force     ! force to read ob_types, regardless l4dvar etc.
  logical        ,optional,intent(in):: ignore_iter ! ignore iter checking
  logical        ,optional,intent(in):: alwaysLocal ! read all files

! ----------------------------------------------------------
  character(len=*),parameter:: myname_=myname//"::mread_"
  logical:: redistr,exist_
  integer(i_kind):: lPE,uPE,iPE,ier
  integer(i_kind):: jtyp,jread
  logical:: force_read
  logical:: alwaysLocal_
  logical:: fileislocal
  type(latlonRange),allocatable,dimension(:):: allRanges
_ENTRY_(myname_)
_TIMER_ON_(myname_)
!call stdout_open("obsdiags_mread")
  force_read=.false.
  if(present(force)) force_read=force
  alwaysLocal_=obsdiags_alwaysLocal
  if(present(alwaysLocal)) alwaysLocal_=alwaysLocal

  call lobsdiags_statusCheck_(myname_,allocated=.true.)

        ! Determine the configuration, either read-my-own-data-only, or
        ! try-to-read-all-data-available.

  lPE=myPE
  uPE=lPE
  redistr=.false.
  if(present(mPEs)) then
    if(mPEs>0) then
      redistr=.true.
      lPE=0
      uPE=-1
      do iPE=lPE,mPEs-1
        inquire(file=trim(filename_(cdfile,iPE)), exist=exist_)
        if(exist_) uPE=iPE
      enddo
    endif
  endif

        ! Reset components of obsdiags, for their re-construction from files
  call reset_()

  if(CHECK_SIZES_) then
    allocate(lsize_type(nobs_type))
    allocate(nsize_type(nobs_type))
    allocate(lsize_diag(nobs_type))
    allocate(nsize_diag(nobs_type))
    allocate(msize_diag(nobs_type))

    lsize_type(:)=0
    nsize_type(:)=0
    lsize_diag(:)=0
    nsize_diag(:)=0
    msize_diag(:)=0
  endif

        ! MPI_Barrier() calls are not necessary.  They are used here to ensure
        ! the log-messages mean what they really mean, if only the root is used to
        ! report the all-PE status.

  if(SYNCH_MESSAGES) call MPI_Barrier(gsi_comm_world,ier)

  if(redistr) then
    if(mype==0) then
      call tell(myname_,'Reading obsdiags files for redistribution, nPEs =',uPE-lPE+1)
      call tell(myname_,'                    prefix of the files, cdfile =',trim(cdfile))
      call tell(myname_,'                                            lPE =',lPE)
      call tell(myname_,'                                            uPE =',uPE)
      call tell(myname_,'                                    alwaysLocal =',alwaysLocal_)
    endif

    allocate(allRanges(0:uPE))
    call latlonRange_reset(allRanges)
    call latlonRange_readBcast(hdfilename_(cdfile),allRanges,root=0,comm=gsi_comm_world)

!#define SHOW_LLRANGE
#ifdef SHOW_LLRANGE
    call latlonRange_alldump(allRanges,"obsLLRange")
#endif


    jread=-1    ! checker of the input jiter values
    do iPE=lPE,uPE
      fileislocal=latlonRange_islocal(allRanges(iPE))
      if(alwaysLocal_.or.fileislocal) then
        call read_(cdfile,iPE,redistr,fileislocal=fileislocal, &
                force=force, &
                ignore_iter=ignore_iter, &
                verbose=.not.alwaysLocal_.or.myPE==0, &
                jread=jread)
      endif
    enddo

!#define DEBUG_obsdiags
#ifdef DEBUG_obsdiags
        ! This is an example of dumping information for debugging, on selected
        ! PEs, for specific jtyp and ibin.
        !
        ! This example is on PE #1, for (jtype==3 .and. ibin==3).

    if(myPE==1) then
      call tell(myname_)
      call tell(myname_,'dumping obsdiags(), jtyp =',3)
      call tell(myname_,'                    ibin =',3)
      call tell(myname_,'                   jread =',jread)
      call obsdiagLList_dump(obsdiags(3,3),jiter=jread)
    endif
#endif

        ! Sort to ensure the ordering is unique.
    call lsort_()

    call latlonRange_reset(allRanges)
    deallocate(allRanges)

  else  ! of if(redistr)
    call read_(cdfile,myPE,redistr,fileislocal=.true., &
        force=force, &
        ignore_iter=ignore_iter, &
        verbose=.true.)

  endif ! of if(redistr)

  if(myPE==0) then
    call tell(myname_,'Finished reading of all obsdiags files, nPEs =',uPE-lPE+1)
  endif
  
  if(CHECK_SIZES_) then
    do jtyp=lbound(lsize_type,1),ubound(lsize_type,1)
      if( msize_diag(jtyp)>0.or.lsize_diag(jtyp)>0.or.nsize_diag(jtyp)>0 .or. &
                                lsize_type(jtyp)>0.or.nsize_type(jtyp)>0 ) then
        write(stdout,'(i5.3,i5,7x,5i8,2x,l1)')   myPE,jtyp ,lsize_type(jtyp),nsize_type(jtyp), &
                                           msize_diag(jtyp),lsize_diag(jtyp),nsize_diag(jtyp)
      endif
    enddo

    call iMPI_reduceSUM_(lsize_type,root=0,comm=gsi_comm_world)
    call iMPI_reduceSUM_(nsize_type,root=0,comm=gsi_comm_world)
    call iMPI_reduceSUM_(lsize_diag,root=0,comm=gsi_comm_world)
    call iMPI_reduceSUM_(nsize_diag,root=0,comm=gsi_comm_world)
    call iMPI_reduceSUM_(msize_diag,root=0,comm=gsi_comm_world)

    if(myPE==0) then
      do jtyp=lbound(lsize_type,1),ubound(lsize_type,1)
        if( msize_diag(jtyp)>0.or.lsize_diag(jtyp)>0.or.nsize_diag(jtyp)>0 .or. &
                                  lsize_type(jtyp)>0.or.nsize_type(jtyp)>0 ) then
          write(stdout,'(2x,a,i5,7x,5i8,2x,l1)')    '***',jtyp ,lsize_type(jtyp),nsize_type(jtyp), &
                                               msize_diag(jtyp),lsize_diag(jtyp),nsize_diag(jtyp)
        endif
      enddo
    endif

    deallocate(lsize_type)
    deallocate(nsize_type)
    deallocate(lsize_diag)
    deallocate(nsize_diag)
    deallocate(msize_diag)
  endif

  if(SYNCH_MESSAGES) call MPI_Barrier(gsi_comm_world,ier)
  if(DO_SUMMARY) call summary_(myname_)

  if(lobserver) then
    if(.not.force_read) then
      !call destroyobs(   skipit=.true.)
      call reset_(obsdiags_keep=.true.)
    endif
  endif
!call stdout_close()
_TIMER_OFF_(myname_)
_EXIT_(myname_)
return
end subroutine mread_

subroutine reset_(obsdiags_keep)
  use obsmod, only: obsdiags
  use obsmod, only: luse_obsdiag
  use obsmod, only: lobsdiag_allocated

  use m_obsdiagNode, only: obsdiagLList_reset
  use m_obsLList, only: obsLList_reset
  use m_obsNode , only: obsNode
  use m_obsNodeTypeManager, only: obsNode_typeMold

! use m_wspd10mNode, only: obsLList_reset
! use    m_td2mNode, only: obsLList_reset
! use    m_mxtmNode, only: obsLList_reset
! use    m_mitmNode, only: obsLList_reset
! use    m_pmslNode, only: obsLList_reset
! use    m_howvNode, only: obsLList_reset
! use   m_tcamtNode, only: obsLList_reset
! use   m_lcbasNode, only: obsLList_reset
! use    m_pm10Node, only: obsLList_reset
! use   m_cldchNode, only: obsLList_reset

  _TIMER_USE_
  implicit none
  logical,optional,intent(in):: obsdiags_keep
  character(len=*),parameter:: myname_=myname//'::reset_'
  integer(i_kind):: ii,jj
  logical:: obsdiags_keep_
  integer(i_kind):: ier
  class(obsNode),pointer:: mNode_
_ENTRY_(myname_)
_TIMER_ON_(myname_)

_TRACEV_(myname_,'lobsdiag_allocated   =',lobsdiag_allocated)

_TRACEV_(myname_,'lobsdiags_allocated_ =',lobsdiags_allocated_)
  if(luse_obsdiag) then
    if(.not.lobsdiags_allocated_) then
      lobsdiags_allocated_=.true.
      if(.not.associated(obsdiags)) then; call die(myname_,'associated(obsdiags)=',associated(obsdiags)); endif
      !allocate( obsdiags(nobs_type,nobs_bins))
    endif

    ASSERT(all(shape(obsdiags)==shape(obsLLists)))
    ASSERT(size(obsdiags,1)==size(obsLLists,1))
    ASSERT(size(obsdiags,2)==size(obsLLists,2))

  endif

  if(.not.lobstypes_allocated_) then
    lobstypes_allocated_=.true.
    if(.not.associated(obsLLists)) call die(myname_,'.not.associated(obsLLists)')
  endif

  obsdiags_keep_=.false.
  if(present(obsdiags_keep)) obsdiags_keep_=obsdiags_keep

  do ii=1,size(obsLLists,2)      ! nobs_bins
    do jj=1,size(obsLLists,1)    ! nobs_type
      if(luse_obsdiag) then
        if(.not.obsdiags_keep_) then
          call obsdiagLList_reset(obsdiags(jj,ii))
          lobsdiag_allocated=.false.
        endif
      endif

      mNode_ => obsNode_typeMold(jj)        ! get the ob_type of jj
      call obsLList_reset(obsLLists(jj,ii),mold=mNode_, stat=ier)
        if(ier/=0) then
          call perr(myname_,'call obsLList_reset(), stat =',ier)
          call perr(myname_,'                       ibin =',ii)
          call perr(myname_,'                      jtype =',jj)
          call perr(myname_,'              mold%mytype() =',mNode_%mytype())
          call  die(myname_)
        endif
      mNode_ => null()
    enddo
  enddo
_TIMER_OFF_(myname_)
_EXIT_(myname_)
return
end subroutine reset_

function ptr_obsbins_(oll,vname) result(ptr_)
!-- returns a pointer to the list of vname, from array oll.
  use m_obsNodeTypeManager, only: obsNode_typeIndex
  implicit none
  type(obsLList),pointer,dimension(:):: ptr_
  type(obsLList),dimension(:,:),target, intent(in):: oll
  character(len=*)                    , intent(in):: vname

  character(len=*),parameter:: myname_=myname//"::ptr_obsbins_"
  integer(i_kind):: itype
_ENTRY_(myname_)
  itype = obsNode_typeIndex(vname)  ! e.g. vname="ps"

  ptr_ => null()                ! e.g. if(.not.associated(ptr_)) ...

        ASSERT(itype>0)
        ASSERT(itype<=size(oll,1))

  ptr_ => oll(itype,:)

_EXIT_(myname_)
return
end function ptr_obsbins_

subroutine aliasesCreate_()
   implicit none

   character(len=*),parameter:: myname_=myname//"aliasesCreate_"
_ENTRY_(myname_)

   !! too much to declare, if use enumerated index value directly
   !    pshead => obsllists(i_ps_ob_type,:)            ! too much to declare
   !    pshead => ptr_obsLList(obsllists,i_ps_ob_type) ! too much to declare
   !    pshead => ptr_obsLList(obsllists,iobstype_ps)  ! too much to declare
   !! use a vname may be a balance
   !    pshead => ptr_obsLList(obsllists,'ps')  ! name is less explicit
   !    pshead => ptr_psbins(obsllists)         ! too many to implement
   !    pshead => ptr_obsbins(obsllists,'ps')   ! explicit and minimal-ism
   !! an example of m_obsNodeTypeManager implementation
   !            use m_obsNodeTypeManager, only: obsType_mold
   !                    ! index_mold(index=i_ps_ob_type)
   !                    ! vname_mold(vname="ps"|"[psNode]")
   !            use m_obsNodeTypeManager, only: obsType_index
   !                    ! vname_index(vname='ps'|'[psNode]')
   !                    ! vmold_index(vmold=psNode())
   !! to implement ptr_obsbins()
   !    function ptr_obsbins(obsllists,vname) result(ptr_)
   !      use m_obsNodeTypeManager, only: obsType_index
   !      iobstype = obsType_index(vname) ! e.g. vname="ps"
   !      ptr_ => obsllists(iobstype,:)
   
      pshead => ptr_obsbins_(obsllists,'ps')
       thead => ptr_obsbins_(obsllists,'t')
       whead => ptr_obsbins_(obsllists,'w')
       qhead => ptr_obsbins_(obsllists,'q')
     spdhead => ptr_obsbins_(obsllists,'spd')
     srwhead => ptr_obsbins_(obsllists,'srw')
      rwhead => ptr_obsbins_(obsllists,'rw')
      dwhead => ptr_obsbins_(obsllists,'dw')
     ssthead => ptr_obsbins_(obsllists,'sst')
      pwhead => ptr_obsbins_(obsllists,'pw')
      ozhead => ptr_obsbins_(obsllists,'oz')
     o3lhead => ptr_obsbins_(obsllists,'o3l')
     pcphead => ptr_obsbins_(obsllists,'pcp')
     gpshead => ptr_obsbins_(obsllists,'gps')
     radhead => ptr_obsbins_(obsllists,'rad')
     tcphead => ptr_obsbins_(obsllists,'tcp')
     laghead => ptr_obsbins_(obsllists,'lag')
   colvkhead => ptr_obsbins_(obsllists,'colvk')
    aerohead => ptr_obsbins_(obsllists,'aero')
   aerolhead => ptr_obsbins_(obsllists,'aerol')
   pm2_5head => ptr_obsbins_(obsllists,'pm2_5')
     vishead => ptr_obsbins_(obsllists,'vis')
    gusthead => ptr_obsbins_(obsllists,'gust')
    pblhhead => ptr_obsbins_(obsllists,'pblh')

 wspd10mhead => ptr_obsbins_(obsllists,'wspd10m')
    td2mhead => ptr_obsbins_(obsllists,'td2m')
    mxtmhead => ptr_obsbins_(obsllists,'mxtm')
    mitmhead => ptr_obsbins_(obsllists,'mitm')
    pmslhead => ptr_obsbins_(obsllists,'pmsl')
    howvhead => ptr_obsbins_(obsllists,'howv')
   tcamthead => ptr_obsbins_(obsllists,'tcamt')
   lcbashead => ptr_obsbins_(obsllists,'lcbas')

    pm10head => ptr_obsbins_(obsllists,'pm10')
   cldchhead => ptr_obsbins_(obsllists,'cldch')

_EXIT_(myname_)
return
end subroutine aliasesCreate_

subroutine aliasesDestroy_()
   implicit none

   character(len=*),parameter:: myname_=myname//"aliasesDestroy_"
_ENTRY_(myname_)

      pshead => null()
       thead => null()
       whead => null()
       qhead => null()
     spdhead => null()
     srwhead => null()
      rwhead => null()
      dwhead => null()
     ssthead => null()
      pwhead => null()
      ozhead => null()
     o3lhead => null()
     pcphead => null()
     gpshead => null()
     radhead => null()
     tcphead => null()
     laghead => null()
   colvkhead => null()
    aerohead => null()
   aerolhead => null()
   pm2_5head => null()
     vishead => null()
    gusthead => null()
    pblhhead => null()

 wspd10mhead => null()
    td2mhead => null()
    mxtmhead => null()
    mitmhead => null()
    pmslhead => null()
    howvhead => null()
   tcamthead => null()
   lcbashead => null()

    pm10head => null()
   cldchhead => null()

_EXIT_(myname_)
return
end subroutine aliasesDestroy_

subroutine lsort_()
!$$$  subprogram documentation block
!
! abstract: sort entries of obsdiags(:,:) and obsLLists(:,:)
!
! program history log:
!
!   input argument list:
!
!$$$

  use gsi_unformatted, only: unformatted_open
  use obsmod, only: luse_obsdiag

  use m_obsLList, only: obsLList_lsort
  use m_obsdiagNode, only: obsdiagLList_lsize
  use m_obsdiagNode, only: obsdiagLList_lsort

  _TIMER_USE_
  implicit none

  character(len=*), parameter :: myname_=myname//"::lsort_"

  integer(i_kind) :: ii,jj !,iobs,lobs,ierr
_ENTRY_(myname_)
_TIMER_ON_(myname_)
! ----------------------------------------------------------
  call lobsdiags_statusCheck_(myname_,allocated=.true.)

  if (luse_obsdiag) then

     ASSERT(all(shape(obsdiags)==shape(obsLLists)))
     ASSERT(size(obsdiags,1)==size(obsLLists,1))
     ASSERT(size(obsdiags,2)==size(obsLLists,2))

  endif

  do jj=1,size(obsdiags,1)
     do ii=1,size(obsdiags,2)
        call obsdiagLList_lsort(obsdiags(jj,ii),itype=jj,ibin=ii)
     enddo
  enddo

  do jj=1,size(obsLLists,1)
     do ii=1,size(obsLLists,2)
       call obsLList_lsort(obsLLists(jj,ii),itype=jj,ibin=ii)
     enddo
  enddo

! ----------------------------------------------------------
_TIMER_OFF_(myname_)
_EXIT_(myname_)
return
end subroutine lsort_

subroutine write_(cdfile,luseonly,force)
!$$$  subprogram documentation block
!
! abstract: Write obsdiags data structure to file.
!
! program history log:
!   2007-07-05  tremolet
!   2007-10-03  todling - expanded to account for full observer
!   2007-10-24  todling - add parameter nchnperobs to obsdiag
!   2009-01-08  todling - remove reference to ozohead
!   2009-01-27  todling - add gps write
!   2010-05-26  treadon - add write_tcphead
!   2010-06-03  todling - add write_colvkhead
!   2011-05-18  todling - aero, aerol, and pm2_5
!   2015-02-04  j guo   - Re-implemented to support re-configurable observation
!                         operators.
!   2015-10-09  j guo   - Now it uses Fortran 2003 dynamic polymorphism.
!
!   input argument list:
!     cdfile - filename to write data
!
!$$$

use mpeu_util, only: tell,die,perr,stdout_open,stdout_close
_TIMER_USE_

  use gsi_unformatted, only: unformatted_open
  use mpimod, only: mype
  use gsi_4dvar, only: nobs_bins,l4dvar
  use  jfunc, only: jiter, miter

  use m_obsLList, only: obsLList_write
  use m_obsdiagNode, only: obsdiagLList_lsize
  use m_obsdiagNode, only: obsdiagLList_write

  use m_latlonRange, only: latlonRange
  use m_latlonRange, only: latlonRange_reset
  use m_latlonRange, only: latlonRange_gatherWrite
  use m_latlonRange, only: latlonRange_gatherDump

  implicit none
  character(len=*), intent(in) :: cdfile        ! := "obsdiags.<miter>"
  logical,optional, intent(in) :: luseonly      ! output only if(%luse)
  logical,optional, intent(in) :: force         ! write all out regardlessly

  character(len=*), parameter :: myname_=myname//"::write_"

integer(i_kind) :: iunit,istat
integer(i_kind) :: ii,jj,ier
logical :: luseonly_
logical :: force_write
type(latlonRange):: luseRange
! ----------------------------------------------------------
_ENTRY_(myname_)
_TIMER_ON_(myname_)
!call stdout_open("obsdiags_write")
  force_write=.false.
  if(present(force)) force_write=force
  call lobsdiags_statusCheck_(myname_,allocated=.true.)

        ASSERT(all(shape(obsdiags)==shape(obsLLists)))
        ASSERT(size(obsdiags,1)==size(obsLLists,1))
        ASSERT(size(obsdiags,2)==size(obsLLists,2))

  luseonly_=.false.
  if(present(luseonly)) luseonly_=luseonly
 
  call unformatted_open( unit=iunit, &
        file=trim(filename_(cdfile,myPE)), &
        class='.obsdiags.', &
        action='write', &
        status='unknown', &
        newunit=.true., &       ! with newunit=.true., unit returns a value assigned by Fortran.
        iostat=istat,silent=.true.)
                if(istat/=0) then
                  call perr(myname_,'unformatted_open(), file =',filename_(cdfile,myPE))
                  call perr(myname_,'                 newunit =',iunit)
                  call perr(myname_,'                  iostat =',istat)
                  call  die(myname_)
                endif

  if(DO_SUMMARY) call summary_(myname_)

  do ii=1,nobs_bins
    do jj=1,nobs_type
      call obsdiagLList_write(obsdiags(jj,ii),iunit,luseonly_,jiter,miter,jj,ii,luseRange=luseRange)

      if (force_write .or. l4dvar) then
        call obsLList_write(obsLLists(jj,ii),iunit,luseonly_,jj,luseRange=luseRange)
      endif

      write(iunit)ii,jj   ! a jj_obstype-block trailer
    enddo
  enddo

  close(iunit)

        ! latlonRange_gatherWrite() implies a mpi_barrier() action.
  call latlonRange_gatherWrite(luseRange,hdfilename_(cdfile),root=0,comm=gsi_comm_world)

#ifdef SHOW_LLRANGE
        ! Text-dump to diagnose the values
  call latlonRange_gatherDump(          "cvgLLRange",root=0,comm=gsi_comm_world)
  call latlonRange_gatherDump(luseRange,"obsLLRange",root=0,comm=gsi_comm_world)
#endif

  call latlonRange_reset(luseRange)

  if(SYNCH_MESSAGES) call MPI_Barrier(gsi_comm_world,ier)
  if (mype==0) call tell(myname_,'Finish writing obsdiags to file ',filename_(cdfile,myPE))

! ----------------------------------------------------------
!call stdout_close()
_TIMER_OFF_(myname_)
_EXIT_(myname_)
return
end subroutine write_

subroutine read_(cdfile,iPE,redistr,fileislocal,force,ignore_iter,verbose,jread)
  use mpeu_util, only: tell,perr,die
  use mpeu_util, only: stdout
  use mpimod, only: mype
  use gsi_4dvar, only: l4dvar
  use gsi_unformatted, only: unformatted_open
  use  jfunc, only: jiter,miter
  _TIMER_USE_

  use obsmod, only: lobserver
  use obsmod, only: obs_diag

  use m_obsLList, only: obsLList_read
  use m_obsLList, only: obsLList_lsize
  use m_obsLList, only: obsLList_lcount

  use m_obsdiagNode, only: obsdiagLList_read
  use m_obsdiagNode, only: obsdiagLList_lsize
  use m_obsdiagNode, only: obsdiagLList_lcount
  use m_obsdiagNode, only: obsdiagLookup_build
  use m_obsdiagNode, only: obsdiagLookup_clean

  implicit none
  character(len=*), intent(in ):: cdfile        ! prefix of the input file
  integer(i_kind ), intent(in ):: iPE           ! iPE of the input file
  logical         , intent(in ):: redistr       ! data redistribution is expected
  logical         , intent(in ):: fileislocal   ! the file to read, is known local

  logical,optional, intent(in ):: force         ! (force to read ob_type data
  logical,optional, intent(in ):: ignore_iter   ! ignore checking of iter
  logical,optional, intent(in ):: verbose       ! report each reading
  integer(i_kind ), optional, intent(inout):: jread     ! jiter read from the input

  character(len=*),parameter:: myname_=myname//'::read_'
  character(len=*),parameter:: diag_timer_=myname_//'.obsdiagLList_read'
  character(len=*),parameter:: list_timer_=myname_//'.obsLList_read'
  integer(i_kind):: ii,jj
  integer(i_kind):: ki,kj
  integer(i_kind):: iunit,istat
  integer(i_kind):: jread_
  integer(i_kind):: lsize_type_,nsize_type_
  integer(i_kind):: lsize_diag_,nsize_diag_,msize_diag_
  type(obs_diag),pointer:: leadNode => NULL()
  logical:: force_read
  logical:: verbose_
_ENTRY_(myname_)
_TIMER_ON_(myname_)

  call lobsdiags_statusCheck_(myname_,allocated=.true.)
  force_read=.false.
  if(present(force)) force_read=force

  verbose_=.false.
  if(present(verbose)) verbose_=verbose

        ASSERT(all(shape(obsdiags)==shape(obsLLists)))
        ASSERT(size(obsdiags,1)==size(obsLLists,1))
        ASSERT(size(obsdiags,2)==size(obsLLists,2))
        if(CHECK_SIZES_) then
          ASSERT(size(obsdiags,1)==size(lsize_type ))
          ASSERT(size(obsdiags,1)==size(nsize_type ))
          ASSERT(size(obsdiags,1)==size(lsize_diag ))
          ASSERT(size(obsdiags,1)==size(nsize_diag ))
        endif

  call unformatted_open( unit=iunit, &
        file=trim(filename_(cdfile,iPE)), &
        class='.obsdiags.', &
        action='read', &
        status='old', &
        newunit=.true., &       ! with newunit=.true., unit returns a value assigned by Fortran.
        iostat=istat,silent=.true.)
                if(istat/=0) then
                  call perr(myname_,'unformatted_open(), file =',trim(filename_(cdfile,iPE)))
                  call perr(myname_,'                    myPE =',myPE)
                  call perr(myname_,'                     iPE =',iPE)
                  call perr(myname_,'                   miter =',miter)
                  call perr(myname_,'                 redistr =',redistr)
                  call perr(myname_,'                 newunit =',iunit)
                  call perr(myname_,'                  iostat =',istat)
                  call  die(myname_)
                endif

  if(verbose_) call tell(myname_,'Reading obsdiags, file =',trim(filename_(cdfile,iPE)))

  leadNode => null()
  do ii=1,size(obsdiags,2)
    do jj=1,size(obsdiags,1)
      if(CHECK_SIZES_) then
        lsize_type_= obsLList_lcount(obsLLists(jj,ii),luseonly=.true.,recount=.true.)
        nsize_type_= obsLList_lsize (obsLLists(jj,ii)                )

        lsize_diag_= obsdiagLList_lcount(obsdiags(jj,ii),luseonly=.true.,recount=.true.)
        !msize_diag_= obsdiagLList_lcount(obsdiags(jj,ii),museonly=.true.)
        nsize_diag_= obsdiagLList_lsize (obsdiags(jj,ii)                )
      endif

      call obsdiagLList_read(obsdiags(jj,ii),iunit,redistr,jiter,miter,jj,ii,jread_,leadNode=leadNode,ignore_iter=ignore_iter)
      if(present(jread)) then
        if(jread/=jread_) then
          if(jread>0) then
            call perr(myname_,'not the same iteration, jiter =',jiter)
            call perr(myname_,'                  saved jread =',jread)
            call perr(myname_,'                current jread =',jread_)
            call  die(myname_)
          endif
          jread=jread_
        endif
      endif

      call obsdiagLookup_build(obsdiags(jj,ii),leadNode=leadNode,jiter=jread)
          leadNode => null()     ! nullified after its use, to avoid leadNode dangling arround.

      if (force_read .or. l4dvar.and..not.(lobserver.and.jiter==1)) then
        call obsLList_read(obsLLists(jj,ii),iunit,redistr,obsdiags(jj,ii),jj)
      endif

      call obsdiagLookup_clean(obsdiags(jj,ii))

      read(iunit)ki,kj
      if(ki/=ii .or. kj/=jj) then
        call perr(myname_,'mismatched block id, file =',filename_(cdfile,iPE))
        if(kj/=jj) then
        call perr(myname_,'               reading kj =',kj)
        call perr(myname_,'             expecting jj =',jj)
        endif
        if(ki/=ii) then
        call perr(myname_,'               reading ki =',ki)
        call perr(myname_,'             expecting ii =',ii)
        endif
        call perr(myname_,'                     file =',filename_(cdfile,iPE))
        call perr(myname_,'                   cdfile =',cdfile)
        call perr(myname_,'                     myPE =',myPE)
        call perr(myname_,'                      iPE =',iPE)
        call perr(myname_,'                    miter =',miter)
        call perr(myname_,'                  redistr =',redistr)
        call perr(myname_,'                  newunit =',iunit)
        call perr(myname_,'                   iostat =',istat)
        call  die(myname_)
      endif

      ASSERT(1<=jj.and.jj<=nobs_type)

      if(CHECK_SIZES_) then
        lsize_type_= obsLList_lcount(obsLLists(jj,ii),luseonly=.true.)-lsize_type_
        nsize_type_= obsLList_lsize (obsLLists(jj,ii)                )-nsize_type_

        lsize_diag_= obsdiagLList_lcount(obsdiags(jj,ii),luseonly=.true.)-lsize_diag_
        !msize_diag_= obsdiagLList_lcount(obsdiags(jj,ii),museonly=.true.)-msize_diag_
        nsize_diag_= obsdiagLList_lsize (obsdiags(jj,ii)                )-nsize_diag_

        if( fileislocal  .or. lsize_type_>0.or.nsize_type_>0 .or. &
            msize_diag_>0.or. lsize_diag_>0.or.nsize_diag_>0 ) then
          write(stdout,'(i5.3,2i5,2x,5i6,2x,l1)') iPE,jj,ii,lsize_type_,nsize_type_, &
                                                msize_diag_,lsize_diag_,nsize_diag_,fileislocal
        endif
      
        lsize_type(jj)= lsize_type(jj) +lsize_type_
        nsize_type(jj)= nsize_type(jj) +nsize_type_

        lsize_diag(jj)= lsize_diag(jj) +lsize_diag_
        !msize_diag(jj)= msize_diag(jj) +msize_diag_
        nsize_diag(jj)= nsize_diag(jj) +nsize_diag_
      endif

    enddo       ! jj=1,nobs_type
  enddo         ! ii=1,nobs_bins

  close(iunit)
! ----------------------------------------------------------
_TIMER_OFF_(myname_)
_EXIT_(myname_)
return
end subroutine read_

function filename_(prefix,iPE)
  implicit none
  character(len=:),allocatable:: filename_
  character(len=*)    , intent(in ):: prefix
  integer(kind=i_kind), intent(in ):: iPE

  character(len=4):: chPE
  write(chPE,'(i4.4)') iPE
  filename_=trim(adjustl(prefix))//'.'//trim(chPE)
end function filename_

function hdfilename_(prefix)
  use kinds, only: i_kind
  implicit none
  character(len=:),allocatable:: hdfilename_
  character(len=*)    , intent(in ):: prefix
  hdfilename_=trim(adjustl(prefix))//'.headers'
end function hdfilename_

subroutine summary_(title)
!-- get a summary of obsdiags(:,:) and obsLLists(:,:)
use obsmod, only: luse_obsdiag
use mpeu_util, only: tell,die,perr,stdout_open,stdout_close
_TIMER_USE_

  use gsi_unformatted, only: unformatted_open
  use gsi_4dvar, only: nobs_bins

  use m_obsLList, only: obsLList_lsize => obsLList_lcount
  use m_obsdiagNode, only: obsdiagLList_lsize => obsdiagLList_lcount

  implicit none
  character(len=*), intent(in) :: title

  character(len=*), parameter :: myname_=myname//"::summary_"

  integer(i_kind) :: ii,jj
  integer(i_kind),dimension(nobs_type,nobs_bins):: ldiag,ndiag
  integer(i_kind),dimension(nobs_type,nobs_bins):: lobss,nobss
_ENTRY_(myname_)
_TIMER_ON_(myname_)
! ----------------------------------------------------------

  call lobsdiags_statusCheck_(myname_,allocated=.true.)

  if (luse_obsdiag) then
        ASSERT(all(shape(obsdiags)==shape(obsLLists)))
        ASSERT(size(obsdiags,1)==size(obsLLists,1))
        ASSERT(size(obsdiags,2)==size(obsLLists,2))
  endif

  do ii=1,size(obsdiags,2)
    do jj=1,size(obsdiags,1)
      ldiag(jj,ii) = obsdiagLList_lsize(obsdiags(jj,ii),luseonly=.true. ,recount=.true.)
      ndiag(jj,ii) = obsdiagLList_lsize(obsdiags(jj,ii),luseonly=.false.,recount=.true.)
    enddo
  enddo

  do ii=1,size(obsLLists,2)
    do jj=1,size(obsLLists,1)
      lobss(jj,ii) = obsLList_lsize(obsLLists(jj,ii),luseonly=.true. ,recount=.true.)
      nobss(jj,ii) = obsLList_lsize(obsLLists(jj,ii),luseonly=.false.,recount=.true.)
    enddo
  enddo

  call gather_write_(title,lobss,ldiag,nobss,ndiag,root=0,comm=gsi_comm_world)

! ----------------------------------------------------------
_TIMER_OFF_(myname_)
_EXIT_(myname_)
return
end subroutine summary_

subroutine gather_write_(title,lobss,ldiag,nobss,ndiag,root,comm)
  use mpimod   , only: mype,nPE
  use kinds    , only: i_kind
  use mpeu_mpif, only: MPI_ikind
  _TIMER_USE_
  implicit none
  character(len=*),intent(in):: title
  integer(kind=i_kind),dimension(:,:),intent(in):: lobss,ldiag
  integer(kind=i_kind),dimension(:,:),intent(in):: nobss,ndiag
  integer(kind=MPI_ikind),intent(in):: root
  integer(kind=MPI_ikind),intent(in):: comm

  character(len=*),parameter:: myname_=myname//'::gather_write_'
  integer(kind=i_kind):: jj,ii,iPE
  integer(kind=i_kind) :: mtyp,mbin,mPEs
  integer(kind=i_kind),allocatable,dimension(:,:,:):: ldiagm,ndiagm
  integer(kind=i_kind),allocatable,dimension(:,:,:):: lobssm,nobssm

_ENTRY_(myname_)
_TIMER_ON_(myname_)
  mtyp=size(lobss,1)
  mbin=size(lobss,2)
        ASSERT(mtyp==size(nobss,1))
        ASSERT(mbin==size(nobss,2))
        ASSERT(mtyp==size(ldiag,1))
        ASSERT(mbin==size(ldiag,2))
        ASSERT(mtyp==size(ndiag,1))
        ASSERT(mbin==size(ndiag,2))

  mPEs=0        ! its value is significant only on root
  if(myPE==root) mPEs=nPE

  allocate(lobssm(mtyp,mbin,0:mPEs-1))
  allocate(ldiagm(mtyp,mbin,0:mPEs-1))
  allocate(nobssm(mtyp,mbin,0:mPEs-1))
  allocate(ndiagm(mtyp,mbin,0:mPEs-1))

  call iMPI_gather_(lobss,lobssm,root,comm)
  call iMPI_gather_(nobss,nobssm,root,comm)
  call iMPI_gather_(ldiag,ldiagm,root,comm)
  call iMPI_gather_(ndiag,ndiagm,root,comm)

  if(myPE==root) then
    do iPE=0,nPE-1
      write(stdout,'(2a,i6)'     ) title,'(): local obs/diag counts, iPE =',iPE
      write(stdout,'(2a,9(1x,a))') title,'(): typ', ('|  -----lo -----ld  -----no -----nd',ii=1,mbin)
      do jj=1,mtyp
        write(stdout,'(2a,i3,9(1x,a,2(1x,2i8)))') &
                                   title,'(): ',jj , &
          ("|",lobssm(jj,ii,iPE),ldiagm(jj,ii,iPE), &
               nobssm(jj,ii,iPE),ndiagm(jj,ii,iPE), ii=1,mbin)
      enddo
    enddo
  endif

  deallocate(lobssm)
  deallocate(ldiagm)
  deallocate(nobssm)
  deallocate(ndiagm)
_TIMER_OFF_(myname_)
_EXIT_(myname_)
end subroutine gather_write_

subroutine iMPI_barrier_(comm)
  use mpeu_mpif, only: MPI_ikind
  use mpeu_util, only: die
  implicit none
  integer(kind=MPI_ikind),intent(in):: comm

  character(len=*),parameter:: myname_=myname//"::iMPI_barrier_"
  integer(kind=MPI_ikind):: ier

  call MPI_barrier(comm,ier)
        if(ier/=0) call die(myname_,'MPI_barrier() error, ierror =',ier)
end subroutine iMPI_barrier_

subroutine iMPI_gather_(isend,irecv,root,comm)
  use mpeu_mpif,only: MPI_ikind,MPI_type
  use mpeu_util, only: die
  use kinds, only: i_kind
  implicit none
  integer(kind=i_kind),dimension(:,:  ),intent(in ):: isend
  integer(kind=i_kind),dimension(:,:,:),intent(out):: irecv
  integer(kind=MPI_ikind),intent(in):: root
  integer(kind=MPI_ikind),intent(in):: comm

  character(len=*),parameter:: myname_=myname//"::iMPI_gather_"
  integer(kind=MPI_ikind):: itype,isize,ierr

  isize=size(isend)
  itype=MPI_type(isend)
  call MPI_gather(isend,isize,itype, &
                  irecv,isize,itype, root,comm,ierr)
        if(ierr/=0) call die(myname_,'MPI_gather() error, ierror =',ierr)
end subroutine iMPI_gather_

subroutine iMPI_reduceSUM_(iredu,root,comm)
  use mpeu_mpif,only: MPI_ikind,MPI_type,MPI_SUM
  use mpeu_util, only: die
  use kinds, only: i_kind
  implicit none
  integer(kind=i_kind),dimension(:),intent(inout):: iredu
  integer(kind=MPI_ikind),intent(in):: root
  integer(kind=MPI_ikind),intent(in):: comm

  character(len=*),parameter:: myname_=myname//"::iMPI_reduceSUM_"
  integer(kind=MPI_ikind):: itype,isize,ierr
  !integer(kind=kind(iredu)),dimension(size(iredu)):: irecv

  isize=size(iredu)
  itype=MPI_type(iredu)
  call MPI_reduce((iredu),iredu,isize,itype, MPI_SUM, root,comm,ierr)
        if(ierr/=0) call die(myname_,'MPI_reduce(MPI_SUM) error, ierror =',ierr)
  !iredu(:)=irecv(:)
end subroutine iMPI_reduceSUM_

subroutine create_obsmod_vars()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_obsmod_vars
!     prgmmr:    derber            org: np23           date: 2003-09-25
!
! abstract:  allocate arrays to hold observation related information
!
! program history log:
!   2003-09-25  derber
!   2004-05-13  treadon, documentation
!   2015-10-09  j guo   - moved here from MODULE OBSMOD with modifcations
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block
    use gsi_4dvar, only: nobs_bins
    implicit none
    !ALLOCATE(obsdiags(nobs_type,nobs_bins))
    allocate(obsllists(nobs_type,nobs_bins))
    call aliasesCreate_()
    return
end subroutine create_obsmod_vars

subroutine destroy_obsmod_vars()
!-- Created to pair with create_obsmod_vars().
  implicit none
  call aliasesDestroy_()
  deallocate(obsllists)
  !deallocate(obsdiags)
  return
end subroutine destroy_obsmod_vars
end module m_obsdiags
