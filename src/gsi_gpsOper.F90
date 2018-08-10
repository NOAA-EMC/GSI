module gsi_gpsOper
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module gsi_gpsOper
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2018-08-10
!
! abstract: an obOper extension for gpsNode type
!
! program history log:
!   2018-08-10  j guo   - added this document block
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

  use gsi_obOper, only: obOper
  implicit none
  public:: gpsOper      ! data stracture

  type,extends(obOper):: gpsOper
  contains
    procedure,nopass:: mytype
    procedure:: setup_
    procedure:: intjo1_
    procedure:: stpjo1_
  end type gpsOper

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname='gsi_gpsOper'

contains
  function mytype(nodetype)
    implicit none
    character(len=:),allocatable:: mytype
    logical,optional, intent(in):: nodetype
    mytype="[gpsOper]"
    if(present(nodetype)) then
      if(nodetype) mytype='gps'
    endif
  end function mytype

  subroutine setup_(self, lunin, mype, is, nobs, init_pass,last_pass)
    use gpsbend_setup, only: bend_setup => setup
    use  gpsref_setup, only:  ref_setup => setup
    use kinds, only: i_kind
    use gsi_obOper, only: len_obstype
    use gsi_obOper, only: len_isis

    use m_rhs  , only: awork => rhs_awork
    use m_rhs  , only: iwork => i_gps
    use m_rhs  , only: toss_gps_sub => rhs_toss_gps

    use obsmod  , only: write_diag
    use convinfo, only: diag_conv
    use jfunc   , only: jiter

    use mpeu_util, only: perr,die
    implicit none
    class(gpsOper ), intent(inout):: self
    integer(i_kind), intent(in):: lunin
    integer(i_kind), intent(in):: mype
    integer(i_kind), intent(in):: is
    integer(i_kind), intent(in):: nobs
    logical        , intent(in):: init_pass     ! supporting multi-pass setup()
    logical        , intent(in):: last_pass     ! with incremental backgrounds.

    !----------------------------------------
    character(len=*),parameter:: myname_=myname//"::setup_"

    character(len=len_obstype):: obstype
    character(len=len_isis   ):: isis
    integer(i_kind):: nreal,nchanl,ier,nele
    logical:: diagsave

    if(nobs == 0) return

    read(lunin,iostat=ier) obstype,isis,nreal,nchanl
    if(ier/=0) call die(myname_,'read(obstype,...), iostat =',ier)
    nele = nreal+nchanl

    diagsave  = write_diag(jiter) .and. diag_conv

    select case(obstype)
    case ("gps_ref")
      call ref_setup(self%obsLL(:), self%odiagLL(:), &
        lunin,mype,awork(:,iwork),nele,nobs,toss_gps_sub,is,init_pass,last_pass,diagsave)
    case ("gps_bnd")
      call bend_setup(self%obsLL(:), self%odiagLL(:), &
        lunin,mype,awork(:,iwork),nele,nobs,toss_gps_sub,is,init_pass,last_pass,diagsave)
    case default
      call perr(myname_,'unknown value, obstype =',obstype)
      call perr(myname_,'                  isis =',isis)
      call perr(myname_,'                    is =',is)
      call  die(myname_)
    end select

  end subroutine setup_

  subroutine intjo1_(self, ibin, rval,sval, qpred,sbias)
    use intgpsmod, only: intjo => intgps
    use gsi_bundlemod  , only: gsi_bundle
    use bias_predictors, only: predictors
    use m_obsNode , only: obsNode
    use m_obsLList, only: obsLList_headNode
    use kinds     , only: i_kind, r_quad
    implicit none
    class(gpsOper  ),intent(in   ):: self
    integer(i_kind ),intent(in   ):: ibin
    type(gsi_bundle),intent(inout):: rval   ! (ibin)
    type(gsi_bundle),intent(in   ):: sval   ! (ibin)
    real(r_quad    ),target,dimension(:),intent(inout):: qpred  ! (ibin)
    type(predictors),target,             intent(in   ):: sbias

    !----------------------------------------
    character(len=*),parameter:: myname_=myname//"::intjo1_"
    class(obsNode),pointer:: headNode

    headNode => obsLList_headNode(self%obsLL(ibin))
    call intjo(headNode, rval,sval)
    headNode => null()

  end subroutine intjo1_

  subroutine stpjo1_(self, ibin, dval,xval,pbcjo,sges,nstep,dbias,xbias)
    use stpgpsmod, only: stpjo => stpgps
    use gsi_bundlemod, only: gsi_bundle
    use bias_predictors, only: predictors
    use m_obsNode , only: obsNode
    use m_obsLList, only: obsLList_headNode
    use kinds, only: r_quad,r_kind,i_kind
    implicit none
    class(gpsOper  ),intent(in):: self
    integer(i_kind ),intent(in):: ibin
    type(gsi_bundle),intent(in):: dval
    type(gsi_bundle),intent(in):: xval
    real(r_quad    ),dimension(:),intent(inout):: pbcjo ! (1:4)
    real(r_kind    ),dimension(:),intent(in   ):: sges
    integer(i_kind),intent(in):: nstep

    type(predictors),target, intent(in):: dbias
    type(predictors),target, intent(in):: xbias

    !----------------------------------------
    character(len=*),parameter:: myname_=myname//"::stpjo1_"
    class(obsNode),pointer:: headNode

    headNode => obsLList_headNode(self%obsLL(ibin))
    call stpjo(headNode,dval,xval,pbcjo(:),sges,nstep)
    headNode => null()
  end subroutine stpjo1_

end module gsi_gpsOper
