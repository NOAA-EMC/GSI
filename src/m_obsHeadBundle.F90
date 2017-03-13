module m_obsHeadBundle
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module m_obsHeadBundle
!   prgmmr:      j guo <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:      2015-08-27
!
! abstract: obsHeadBundle replaces type::obs_handle and variable obsmod::yobs.
!
! program history log:
!   2015-08-27  j guo   - added this document block
!   2015-09-03  j guo   - moved "yobs", and its construction and destruction
!                         here, to use them when and where yobs is needed.
!                       . In particular, setupyobs.f90 is included here as a
!                         module procedure create_().  And a destroy_() has been
!                         added, to clean up after any use of create_().
!   2015-09-03  j guo   - changed create_() from a function to a subroutine.
!                       . removed internal dependency to nobs_bins.
!   2016-05-04  j guo   - added 9 new obs-types, to a total of 33 obs-types
!   2016-07-26  j guo   - merged in the earlier proram history log (setupyobs).
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

! Earlier program history log:
!
! subprogram:    setupyobs
!   prgmmr:      tremolet
!
! abstract:  Setup observation vectors (ie the "y" the in "H(x)-y" )
!            In 3D-Var, it contains all observations, in 4D-Var, each
!            y contains all the observations in a given time slot.
!
! program history log:
!   2007-04-17  tremolet - initial code
!   2009-01-08  todling  - remove reference to ozohead
!   2009-03-05  meunier  - add pointer to lagrangean data
!   2009-08-11  lueken   - updated documentation
!   2010-04-22  tangborn - updated reference to co
!   2010-07-10  todling  - add aerosols pointer
!   2010-10-15  pagowski  - add pm2_5 pointer
!   2011-02-19  zhu      - add gust,vis,pblh pointers
!   2014-03-19  pondeca  - add wspd10m
!   2014-04-10  pondeca  - add td2m,mxtm,mitm,pmsl
!   2014-05-07  pondeca  - add howv
!   2014-06-20  carley/zhu - add tcamt and lcbas pointers
!   2015-07-10  pondeca  - add cldch


! module interface:

  use m_obsNode  , only:   obsNode

  use m_psNode   , only:    psNode  !  1
  use m_tNode    , only:     tNode  !  2
  use m_wNode    , only:     wNode  !  3
  use m_qNode    , only:     qNode  !  4
  use m_spdNode  , only:   spdNode  !  5
  use m_srwNode  , only:   srwNode  !  6
  use m_rwNode   , only:    rwNode  !  7
  use m_dwNode   , only:    dwNode  !  8
  use m_sstNode  , only:   sstNode  !  9
  use m_pwNode   , only:    pwNode  ! 10
  use m_pcpNode  , only:   pcpNode  ! 11
  use m_ozNode   , only:    ozNode  ! 12
  use m_o3lNode  , only:   o3lNode  ! 13
  use m_gpsNode  , only:   gpsNode  ! 14
  use m_radNode  , only:   radNode  ! 15
  use m_tcpNode  , only:   tcpNode  ! 16
  use m_lagNode  , only:   lagNode  ! 17
  use m_colvkNode, only: colvkNode  ! 18
  use m_aeroNode , only:  aeroNode  ! 19
  use m_aerolNode, only: aerolNode  ! 20
  use m_pm2_5Node, only: pm2_5Node  ! 21
  use m_gustNode , only:  gustNode  ! 22
  use m_visNode  , only:   visNode  ! 23
  use m_pblhNode , only:  pblhNode  ! 24

  use m_wspd10mNode, only: wspd10mNode ! 25
  use m_td2mNode , only:  td2mNode  ! 26
  use m_mxtmNode , only:  mxtmNode  ! 27
  use m_mitmNode , only:  mitmNode  ! 28
  use m_pmslNode , only:  pmslNode  ! 29
  use m_howvNode , only:  howvNode  ! 30
  use m_tcamtNode, only: tcamtNode  ! 31
  use m_lcbasNode, only: lcbasNode  ! 32

  use m_pm10Node , only:  pm10Node  ! 33
  use m_cldchNode, only: cldchNode  ! 34

  use m_obsLList , only: obsLList_headNode

  implicit none
  private       ! except

  public :: obsHeadBundle       ! data structure

        ! Create()/Destroy() pair, for rank-1 pointers with alloc()/dealloc().
  public :: obsHeadBundle_create
  public :: obsHeadBundle_destroy

        interface obsHeadBundle_create ; module procedure  create_; end interface
        interface obsHeadBundle_destroy; module procedure destroy_; end interface

        ! init()/clean() pair, for allocated scalar objects.
  public :: obsHeadBundle_init
  public :: obsHeadBundle_clean

        interface obsHeadBundle_init ; module procedure  init_; end interface
        interface obsHeadBundle_clean; module procedure clean_; end interface

  type obsHeadBundle
        ! obsHeadBundle is a replacement of obs_handle.  It is implemented as a
        ! snap-shot projection of the actual objects managed by m_obsdiags, and
        ! to be used on demands, closed to where and when a such bundle is
        ! needed.
    !private
    class(obsNode),pointer::    ps => null()   !  1
    class(obsNode),pointer::     t => null()   !  2
    class(obsNode),pointer::     w => null()   !  3
    class(obsNode),pointer::     q => null()   !  4
    class(obsNode),pointer::   spd => null()   !  5
    class(obsNode),pointer::   srw => null()   !  6
    class(obsNode),pointer::    rw => null()   !  7
    class(obsNode),pointer::    dw => null()   !  8
    class(obsNode),pointer::   sst => null()   !  9
    class(obsNode),pointer::    pw => null()   ! 10
    class(obsNode),pointer::   pcp => null()   ! 11
    class(obsNode),pointer::    oz => null()   ! 12
    class(obsNode),pointer::   o3l => null()   ! 13
    class(obsNode),pointer::   gps => null()   ! 14
    class(obsNode),pointer::   rad => null()   ! 15
    class(obsNode),pointer::   tcp => null()   ! 16
    class(obsNode),pointer::   lag => null()   ! 17
    class(obsNode),pointer:: colvk => null()   ! 18
    class(obsNode),pointer::  aero => null()   ! 19
    class(obsNode),pointer:: aerol => null()   ! 20
    class(obsNode),pointer:: pm2_5 => null()   ! 21
    class(obsNode),pointer::  gust => null()   ! 22
    class(obsNode),pointer::   vis => null()   ! 23
    class(obsNode),pointer::  pblh => null()   ! 24
    class(obsNode),pointer:: wspd10m => null()   ! 25
    class(obsNode),pointer::  td2m => null()   ! 26
    class(obsNode),pointer::  mxtm => null()   ! 27
    class(obsNode),pointer::  mitm => null()   ! 28
    class(obsNode),pointer::  pmsl => null()   ! 29
    class(obsNode),pointer::  howv => null()   ! 30
    class(obsNode),pointer:: tcamt => null()   ! 31
    class(obsNode),pointer:: lcbas => null()   ! 32
    class(obsNode),pointer::  pm10 => null()   ! 33
    class(obsNode),pointer:: cldch => null()   ! 34

  end type obsHeadBundle

! Usecases:
!
! (1) yobs(1:nobs_bins) - an array of obsHeadBundle, as yobs(:) has been used
!     so far.
!
!       use gsi_4dvar, only: nobs_bins
!         ...
!       type(obsHeadBundle),pointer,dimension(:):: yobs ! declaration
!         ...
!       call obsHeadBundle_create(yobs,nobs_bins)
!         ...
!       call obsHeadBundle_destroy(yobs)                ! clean() then deallocation
!
! (2) yobs of a given bin - initialized where it is needed.
!
!       use gsi_4dvar, only: nobs_bins
!         ...
!       type(obsHeadBundle):: yobs_ibin                 ! declaration/instanciation
!         ...
!       do ibin=1,nobs_bins
!         call obsHeadBundle_init(yobs_ibin,ibin)       ! initialization
!           ...
!         call obsHeadBundle_clean(yobs_ibin)           ! cleaning
!       enddo
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname='m_obsHeadBundle'

#include "myassert.H"

contains
!---------------------------------------------
subroutine create_(yobs,nbins)
  use kinds, only: i_kind
  implicit none
  type(obsHeadBundle),pointer,dimension(:):: yobs
  integer(kind=i_kind),intent(in):: nbins

  integer(i_kind):: ibin
  allocate(yobs(nbins))
  do ibin=1,size(yobs)
    call init_(yobs(ibin),ibin)
  enddo
return
end subroutine create_

!---------------------------------------------
subroutine destroy_(yobs)
  use kinds, only: i_kind
  implicit none
  type(obsHeadBundle),pointer,dimension(:),intent(inout):: yobs

  integer(i_kind):: ibin
  do ibin=1,size(yobs)
    call clean_(yobs(ibin))
  enddo
  deallocate(yobs)
return
end subroutine destroy_

!---------------------------------------------
subroutine init_(yobs,ibin)
  !use m_obsdiags, only: ps_headNode
  !use m_obsdiags, only: obsllist_
  !use m_obsLList, only: obsLList_head
  !use m_psNode  , only: psNode_typecast ! = 1

  use m_obsdiags, only: pshead          ! = 1
  use m_obsdiags, only: thead           ! = 2
  use m_obsdiags, only: whead           ! = 3
  use m_obsdiags, only: qhead           ! = 4
  use m_obsdiags, only: spdhead         ! = 5
  use m_obsdiags, only: srwhead         ! = 6
  use m_obsdiags, only: rwhead          ! = 7
  use m_obsdiags, only: dwhead          ! = 8
  use m_obsdiags, only: ssthead         ! = 9
  use m_obsdiags, only: pwhead          ! =10
  use m_obsdiags, only: pcphead         ! =11
  use m_obsdiags, only: ozhead          ! =12
  use m_obsdiags, only: o3lhead         ! =13
  use m_obsdiags, only: gpshead         ! =14
  use m_obsdiags, only: radhead         ! =15
  use m_obsdiags, only: tcphead         ! =16
  use m_obsdiags, only: laghead         ! =17
  use m_obsdiags, only: colvkhead       ! =18
  use m_obsdiags, only: aerohead        ! =19
  use m_obsdiags, only: aerolhead       ! =20
  use m_obsdiags, only: pm2_5head       ! =21
  use m_obsdiags, only: gusthead        ! =22
  use m_obsdiags, only: vishead         ! =23
  use m_obsdiags, only: pblhhead        ! =24

  use m_obsdiags, only: wspd10mhead     ! =25
  use m_obsdiags, only:  td2mhead       ! =26
  use m_obsdiags, only:  mxtmhead       ! =27
  use m_obsdiags, only:  mitmhead       ! =28
  use m_obsdiags, only:  pmslhead       ! =29
  use m_obsdiags, only:  howvhead       ! =30
  use m_obsdiags, only: tcamthead       ! =31
  use m_obsdiags, only: lcbashead       ! =32
  use m_obsdiags, only:  pm10head       ! =33
  use m_obsdiags, only: cldchhead       ! =34

  use kinds, only: i_kind
  use mpeu_util, only: assert_
  implicit none
  type(obsHeadBundle),intent(out):: yobs
  integer(i_kind),intent(in ):: ibin

  ASSERT(1<=ibin)
  ASSERT(ibin<=size(   pshead))  ! = 1
  ASSERT(ibin<=size(    thead))  ! = 2
  ASSERT(ibin<=size(    whead))  ! = 3
  ASSERT(ibin<=size(    qhead))  ! = 4
  ASSERT(ibin<=size(  spdhead))  ! = 5
  ASSERT(ibin<=size(  srwhead))  ! = 6
  ASSERT(ibin<=size(   rwhead))  ! = 7
  ASSERT(ibin<=size(   dwhead))  ! = 8
  ASSERT(ibin<=size(  ssthead))  ! = 9
  ASSERT(ibin<=size(   pwhead))  ! =10
  ASSERT(ibin<=size(  pcphead))  ! =11
  ASSERT(ibin<=size(   ozhead))  ! =12
  ASSERT(ibin<=size(  o3lhead))  ! =13
  ASSERT(ibin<=size(  gpshead))  ! =14
  ASSERT(ibin<=size(  radhead))  ! =15
  ASSERT(ibin<=size(  tcphead))  ! =16
  ASSERT(ibin<=size(  laghead))  ! =17
  ASSERT(ibin<=size(colvkhead))  ! =18
  ASSERT(ibin<=size( aerohead))  ! =19
  ASSERT(ibin<=size(aerolhead))  ! =20
  ASSERT(ibin<=size(pm2_5head))  ! =21
  ASSERT(ibin<=size( gusthead))  ! =22
  ASSERT(ibin<=size(  vishead))  ! =23
  ASSERT(ibin<=size( pblhhead))  ! =24
  ASSERT(ibin<=size(wspd10mhead))! =25
  ASSERT(ibin<=size( td2mhead))  ! =26
  ASSERT(ibin<=size( mxtmhead))  ! =27
  ASSERT(ibin<=size( mitmhead))  ! =28
  ASSERT(ibin<=size( pmslhead))  ! =29
  ASSERT(ibin<=size( howvhead))  ! =30
  ASSERT(ibin<=size(tcamthead))  ! =31
  ASSERT(ibin<=size(lcbashead))  ! =32
  ASSERT(ibin<=size( pm10head))  ! =33
  ASSERT(ibin<=size(cldchhead))  ! =34

  yobs%ps    => obsLList_headNode(   pshead(ibin))    ! = 1
  yobs%t     => obsLList_headNode(    thead(ibin))    ! = 2
  yobs%w     => obsLList_headNode(    whead(ibin))    ! = 3
  yobs%q     => obsLList_headNode(    qhead(ibin))    ! = 4
  yobs%spd   => obsLList_headNode(  spdhead(ibin))    ! = 5
  yobs%srw   => obsLList_headNode(  srwhead(ibin))    ! = 6
  yobs%rw    => obsLList_headNode(   rwhead(ibin))    ! = 7
  yobs%dw    => obsLList_headNode(   dwhead(ibin))    ! = 8
  yobs%sst   => obsLList_headNode(  ssthead(ibin))    ! = 9
  yobs%pw    => obsLList_headNode(   pwhead(ibin))    ! =10
  yobs%pcp   => obsLList_headNode(  pcphead(ibin))    ! =11
  yobs%oz    => obsLList_headNode(   ozhead(ibin))    ! =12
  yobs%o3l   => obsLList_headNode(  o3lhead(ibin))    ! =13
  yobs%gps   => obsLList_headNode(  gpshead(ibin))    ! =14
  yobs%rad   => obsLList_headNode(  radhead(ibin))    ! =15
  yobs%tcp   => obsLList_headNode(  tcphead(ibin))    ! =16
  yobs%lag   => obsLList_headNode(  laghead(ibin))    ! =17
  yobs%colvk => obsLList_headNode(colvkhead(ibin))    ! =18
  yobs%aero  => obsLList_headNode( aerohead(ibin))    ! =19
  yobs%aerol => obsLList_headNode(aerolhead(ibin))    ! =20
  yobs%pm2_5 => obsLList_headNode(pm2_5head(ibin))    ! =21
  yobs%gust  => obsLList_headNode( gusthead(ibin))    ! =22
  yobs%vis   => obsLList_headNode(  vishead(ibin))    ! =23
  yobs%pblh  => obsLList_headNode( pblhhead(ibin))    ! =24

  yobs%wspd10m => obsLList_headNode(wspd10mhead(ibin))! =25
  yobs%td2m  => obsLList_headNode( td2mhead(ibin))    ! =26
  yobs%mxtm  => obsLList_headNode( mxtmhead(ibin))    ! =27
  yobs%mitm  => obsLList_headNode( mitmhead(ibin))    ! =28
  yobs%pmsl  => obsLList_headNode( pmslhead(ibin))    ! =29
  yobs%howv  => obsLList_headNode( howvhead(ibin))    ! =30
  yobs%tcamt => obsLList_headNode(tcamthead(ibin))    ! =31
  yobs%lcbas => obsLList_headNode(lcbashead(ibin))    ! =32

  yobs%pm10  => obsLList_headNode( pm10head(ibin))    ! =33
  yobs%cldch => obsLList_headNode(cldchhead(ibin))    ! =34
return
end subroutine init_

!---------------------------------------------
subroutine clean_(yobs)
  implicit none
  type(obsHeadBundle),intent(out):: yobs
  yobs=obsHeadBundle()
end subroutine clean_

end module m_obsHeadBundle
