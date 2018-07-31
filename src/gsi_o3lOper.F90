module gsi_o3lOper
! a template for conv.obs. without bias correction
  use gsi_obOper, only: obOper
  implicit none
  public:: o3lOper      ! data stracture

  type,extends(obOper):: o3lOper
  contains
    procedure,nopass:: mytype
    procedure:: setup_
    procedure:: intjo1_
    procedure:: stpjo1_
  end type o3lOper

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname='gsi_o3lOper'

contains
  function mytype(nodetype)
    implicit none
    character(len=:),allocatable:: mytype
    logical,optional, intent(in):: nodetype
    mytype="[o3lOper]"
    if(present(nodetype)) then
      if(nodetype) mytype='o3l'
    endif
  end function mytype

  subroutine setup_(self, lunin, mype, is, nobs, init_pass,last_pass)
    use o3l_setup, only: setup
    use kinds, only: i_kind
    use gsi_obOper, only: len_obstype
    use gsi_obOper, only: len_isis

    use m_rhs , only: stats => rhs_stats_oz
    use obsmod, only: write_diag
    use ozinfo, only: diag_ozone
    use jfunc , only: jiter

    use mpeu_util, only: die
    implicit none
    class(o3lOper ), intent(inout):: self
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
    integer(i_kind):: nreal,nchanl,ier
    logical:: diagsave

    if(nobs == 0) return

    read(lunin,iostat=ier) obstype,isis,nreal,nchanl
    if(ier/=0) call die(myname_,'read(obstype,...), iostat =',ier)

    diagsave  = write_diag(jiter) .and. diag_ozone

    call setup(self%obsLL(:), self%odiagLL(:), &
        lunin,mype,stats,nchanl,nreal,nobs,obstype,isis,is,diagsave,init_pass)

  end subroutine setup_

  subroutine intjo1_(self, ibin, rval,sval, qpred,sbias)
    use into3lmod, only: intjo => intozlev
    use gsi_bundlemod  , only: gsi_bundle
    use bias_predictors, only: predictors
    use m_obsNode , only: obsNode
    use m_obsLList, only: obsLList_headNode
    use kinds     , only: i_kind, r_quad
    implicit none
    class(o3lOper  ),intent(in   ):: self
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
    use stpo3lmod, only: stpjo => stpozlev
    use gsi_bundlemod, only: gsi_bundle
    use bias_predictors, only: predictors
    use m_obsNode , only: obsNode
    use m_obsLList, only: obsLList_headNode
    use kinds, only: r_quad,r_kind,i_kind
    implicit none
    class(o3lOper  ),intent(in):: self
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

end module gsi_o3lOper
