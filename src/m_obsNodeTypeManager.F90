module m_obsNodeTypeManager
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_obsNodeTypeManager
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2015-08-13
!
! abstract: obsNode type manager, as an enumerated type molder.
!
! program history log:
!   2015-08-13  j guo   - added this document block.
!   2016-05-18  j guo   - finished its initial polymorphic implementation,
!                         with total 33 obs-types.
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

  use obsmod, only: nobs_type

  use obsmod, only: iobsType_ps    =>    i_ps_ob_type
  use obsmod, only: iobsType_t     =>     i_t_ob_type
  use obsmod, only: iobsType_w     =>     i_w_ob_type
  use obsmod, only: iobsType_q     =>     i_q_ob_type
  use obsmod, only: iobsType_spd   =>   i_spd_ob_type
  use obsmod, only: iobsType_srw   =>   i_srw_ob_type
  use obsmod, only: iobsType_rw    =>    i_rw_ob_type
  use obsmod, only: iobsType_dw    =>    i_dw_ob_type
  use obsmod, only: iobsType_sst   =>   i_sst_ob_type
  use obsmod, only: iobsType_pw    =>    i_pw_ob_type
  use obsmod, only: iobsType_pcp   =>   i_pcp_ob_type
  use obsmod, only: iobsType_oz    =>    i_oz_ob_type
  use obsmod, only: iobsType_o3l   =>   i_o3l_ob_type
  use obsmod, only: iobsType_gps   =>   i_gps_ob_type
  use obsmod, only: iobsType_rad   =>   i_rad_ob_type
  use obsmod, only: iobsType_tcp   =>   i_tcp_ob_type
  use obsmod, only: iobsType_lag   =>   i_lag_ob_type
  use obsmod, only: iobsType_colvk => i_colvk_ob_type
  use obsmod, only: iobsType_aero  =>  i_aero_ob_type
  use obsmod, only: iobsType_aerol => i_aerol_ob_type
  use obsmod, only: iobsType_pm2_5 => i_pm2_5_ob_type
  use obsmod, only: iobsType_gust  =>  i_gust_ob_type
  use obsmod, only: iobsType_vis   =>   i_vis_ob_type
  use obsmod, only: iobsType_pblh  =>  i_pblh_ob_type

  use obsmod, only: iobsType_wspd10m => i_wspd10m_ob_type
  use obsmod, only: iobsType_td2m  =>  i_td2m_ob_type
  use obsmod, only: iobsType_mxtm  =>  i_mxtm_ob_type
  use obsmod, only: iobsType_mitm  =>  i_mitm_ob_type
  use obsmod, only: iobsType_pmsl  =>  i_pmsl_ob_type
  use obsmod, only: iobsType_howv  =>  i_howv_ob_type
  use obsmod, only: iobsType_tcamt => i_tcamt_ob_type
  use obsmod, only: iobsType_lcbas => i_lcbas_ob_type

  use obsmod, only: iobsType_pm10  =>  i_pm10_ob_type
  use obsmod, only: iobsType_cldch => i_cldch_ob_type

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
  use m_cldchNode, only: cldchNode ! 34

  use kinds, only: i_kind
  use m_obsNode, only: obsNode
  use mpeu_util, only: perr,die

  implicit none
  private	! except
  public :: nobs_type
  public :: obsNode_typeMold
  public :: obsNode_typeIndex

        interface obsNode_typeMold; module procedure &
                index2vmold_, &
                vname2vmold_
        end interface
        interface obsNode_typeIndex; module procedure &
                vmold2index_, &
                vname2index_
        end interface

  type(psNode   ), target, save::    ps_mold !  1
  type(tNode    ), target, save::     t_mold !  2
  type(wNode    ), target, save::     w_mold !  3
  type(qNode    ), target, save::     q_mold !  4
  type(spdNode  ), target, save::   spd_mold !  5
  type(srwNode  ), target, save::   srw_mold !  6
  type(rwNode   ), target, save::    rw_mold !  7
  type(dwNode   ), target, save::    dw_mold !  8
  type(sstNode  ), target, save::   sst_mold !  9
  type(pwNode   ), target, save::    pw_mold ! 10
  type(pcpNode  ), target, save::   pcp_mold ! 11
  type(ozNode   ), target, save::    oz_mold ! 12
  type(o3lNode  ), target, save::   o3l_mold ! 13
  type(gpsNode  ), target, save::   gps_mold ! 14
  type(radNode  ), target, save::   rad_mold ! 15
  type(tcpNode  ), target, save::   tcp_mold ! 16
  type(lagNode  ), target, save::   lag_mold ! 17
  type(colvkNode), target, save:: colvk_mold ! 18
  type(aeroNode ), target, save::  aero_mold ! 19
  type(aerolNode), target, save:: aerol_mold ! 20
  type(pm2_5Node), target, save:: pm2_5_mold ! 21
  type(gustNode ), target, save::  gust_mold ! 22
  type(visNode  ), target, save::   vis_mold ! 23
  type(pblhNode ), target, save::  pblh_mold ! 24

  type(wspd10mNode), target, save:: wspd10m_mold ! 25
  type(   td2mNode), target, save::    td2m_mold ! 26
  type(   mxtmNode), target, save::    mxtm_mold ! 27
  type(   mitmNode), target, save::    mitm_mold ! 28
  type(   pmslNode), target, save::    pmsl_mold ! 29
  type(   howvNode), target, save::    howv_mold ! 30
  type(  tcamtNode), target, save::   tcamt_mold ! 31
  type(  lcbasNode), target, save::   lcbas_mold ! 32

  type(   pm10Node), target, save::    pm10_mold ! 33
  type(  cldchNode), target, save::   cldch_mold ! 34
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname='m_obsNodeTypeManager'

! UseCase 1: configuration of a single mold
!
!   use m_obsNodeTypeManager, only: obsNode_typeMold
!   use m_psNode, only: i_psNode
!   ...
!   allocate(psLList%mold, source=obsNode_typeMold(i_psNode))
! or, for Fortran 2008 ALLOCATE() with MOLD= specifier
!   allocate(psLList%mold,   mold=obsNode_typeMold(i_psNode))
!
! UseCase 2: configuration of molds in an array
!
!   use m_obsLList, only: obsLList_moldConfig
!   use m_obsNodeTypeManager, only: obsNode_typeMold
!   ...
!   do jtype=lbound(obsdiags,2),ubound(obsdiags,2)
!     do ibin=lbound(obsdiags,1),ubound(obsdiags,1)
!       call obsLList_moldConfig(obsdiags(ibin,jtype),mold=obsNode_typeMold(jtype))
!     enddo
!   enddo
!

contains
function vname2index_(vname) result(index_)
  use mpeu_util, only: lowercase
  implicit none
  integer(i_kind):: index_
  character(len=*),intent(in):: vname
  character(len=len(vname)):: vname_
  vname_=lowercase(vname)

  index_=0
  select case(vname_)
  case("ps"   ,   "[psnode]"); index_ = iobsType_ps
  case("t"    ,    "[tnode]"); index_ = iobsType_t
  case("w"    ,    "[wnode]"); index_ = iobsType_w
  case("q"    ,    "[qnode]"); index_ = iobsType_q
  case("spd"  ,  "[spdnode]"); index_ = iobsType_spd
  case("srw"  ,  "[srwnode]"); index_ = iobsType_srw
  case("rw"   ,   "[rwnode]"); index_ = iobsType_rw
  case("dw"   ,   "[dwnode]"); index_ = iobsType_dw
  case("sst"  ,  "[sstnode]"); index_ = iobsType_sst
  case("pw"   ,   "[pwnode]"); index_ = iobsType_pw
  case("pcp"  ,  "[pcpnode]"); index_ = iobsType_pcp
  case("oz"   ,   "[oznode]"); index_ = iobsType_oz
  case("o3l"  ,  "[o3lnode]"); index_ = iobsType_o3l
  case("gps"  ,  "[gpsnode]"); index_ = iobsType_gps
  case("rad"  ,  "[radnode]"); index_ = iobsType_rad
  case("tcp"  ,  "[tcpnode]"); index_ = iobsType_tcp
  case("lag"  ,  "[lagnode]"); index_ = iobsType_lag
  case("colvk","[colvknode]"); index_ = iobsType_colvk
  case("aero" , "[aeronode]"); index_ = iobsType_aero
  case("aerol","[aerolnode]"); index_ = iobsType_aerol
  case("pm2_5","[pm2_5node]"); index_ = iobsType_pm2_5
  case("gust" , "[gustnode]"); index_ = iobsType_gust
  case("vis"  ,  "[visnode]"); index_ = iobsType_vis
  case("pblh" , "[pblhnode]"); index_ = iobsType_pblh

  case("wspd10m", &
             "[wspd10mnode]"); index_ = iobsType_wspd10m
  case("td2m" , "[td2mnode]"); index_ = iobsType_td2m
  case("mxtm" , "[mxtmnode]"); index_ = iobsType_mxtm
  case("mitm" , "[mitmnode]"); index_ = iobsType_mitm
  case("pmsl" , "[pmslnode]"); index_ = iobsType_pmsl
  case("howv" , "[howvnode]"); index_ = iobsType_howv
  case("tcamt","[tcamtnode]"); index_ = iobsType_tcamt
  case("lcbas","[lcbasnode]"); index_ = iobsType_lcbas

  case("pm10" , "[pm10node]"); index_ = iobsType_pm10
  case("cldch","[cldchnode]"); index_ = iobsType_cldch

  end select
end function vname2index_

function vmold2index_(mold) result(index_)
  implicit none
  integer(i_kind):: index_
  class(obsNode),target,intent(in):: mold

  index_=vname2index_(mold%mytype())
end function vmold2index_

function vmold2index_select_(mold) result(index_)
  implicit none
  integer(i_kind):: index_
  class(obsNode),target,intent(in):: mold

  index_=0
  select type(mold)
  type is(   psNode); index_ = iobsType_ps
  type is(    tNode); index_ = iobsType_t
  type is(    wNode); index_ = iobstype_w
  type is(    qNode); index_ = iobstype_q
  type is(  spdNode); index_ = iobstype_spd
  type is(  srwNode); index_ = iobstype_srw
  type is(   rwNode); index_ = iobstype_rw
  type is(   dwNode); index_ = iobstype_dw
  type is(  sstNode); index_ = iobstype_sst
  type is(   pwNode); index_ = iobstype_pw
  type is(  pcpNode); index_ = iobstype_pcp
  type is(   ozNode); index_ = iobstype_oz
  type is(  o3lNode); index_ = iobstype_o3l
  type is(  gpsNode); index_ = iobstype_gps
  type is(  radNode); index_ = iobstype_rad
  type is(  tcpNode); index_ = iobstype_tcp
  type is(  lagNode); index_ = iobstype_lag
  type is(colvkNode); index_ = iobstype_colvk
  type is( aeroNode); index_ = iobstype_aero
  type is(aerolNode); index_ = iobstype_aerol
  type is(pm2_5Node); index_ = iobstype_pm2_5
  type is( gustNode); index_ = iobstype_gust
  type is(  visNode); index_ = iobstype_vis
  type is( pblhNode); index_ = iobstype_pblh

  type is(wspd10mNode); index_ = iobsType_wspd10m
  type is( td2mNode); index_ = iobsType_td2m
  type is( mxtmNode); index_ = iobsType_mxtm
  type is( mitmNode); index_ = iobsType_mitm
  type is( pmslNode); index_ = iobsType_pmsl
  type is( howvNode); index_ = iobsType_howv
  type is(tcamtNode); index_ = iobsType_tcamt
  type is(lcbasNode); index_ = iobsType_lcbas

  type is( pm10Node); index_ = iobsType_pm10
  type is(cldchNode); index_ = iobsType_cldch
  end select
end function vmold2index_select_

function index2vmold_(i_obType) result(obsmold_)
  implicit none
  class(obsNode),pointer:: obsmold_
  integer(kind=i_kind),intent(in):: i_obType

  character(len=*),parameter:: myname_=myname//"::index2vmold_"

  obsmold_ => null()
  select case(i_obType)
  case(iobsType_ps   ); obsmold_ =>    ps_mold
  case(iobsType_t    ); obsmold_ =>     t_mold
  case(iobsType_w    ); obsmold_ =>     w_mold
  case(iobsType_q    ); obsmold_ =>     q_mold
  case(iobsType_spd  ); obsmold_ =>   spd_mold
  case(iobsType_srw  ); obsmold_ =>   srw_mold
  case(iobsType_rw   ); obsmold_ =>    rw_mold
  case(iobsType_dw   ); obsmold_ =>    dw_mold
  case(iobsType_sst  ); obsmold_ =>   sst_mold
  case(iobsType_pw   ); obsmold_ =>    pw_mold
  case(iobsType_pcp  ); obsmold_ =>   pcp_mold
  case(iobsType_oz   ); obsmold_ =>    oz_mold
  case(iobsType_o3l  ); obsmold_ =>   o3l_mold
  case(iobsType_gps  ); obsmold_ =>   gps_mold
  case(iobsType_rad  ); obsmold_ =>   rad_mold
  case(iobsType_tcp  ); obsmold_ =>   tcp_mold
  case(iobsType_lag  ); obsmold_ =>   lag_mold
  case(iobsType_colvk); obsmold_ => colvk_mold
  case(iobsType_aero ); obsmold_ =>  aero_mold
  case(iobsType_aerol); obsmold_ => aerol_mold
  case(iobsType_pm2_5); obsmold_ => pm2_5_mold
  case(iobsType_gust ); obsmold_ =>  gust_mold
  case(iobsType_vis  ); obsmold_ =>   vis_mold
  case(iobsType_pblh ); obsmold_ =>  pblh_mold

  case(iobsType_wspd10m)
                        obsmold_ => wspd10m_mold
  case(iobsType_td2m ); obsmold_ =>    td2m_mold
  case(iobsType_mxtm ); obsmold_ =>    mxtm_mold
  case(iobsType_mitm ); obsmold_ =>    mitm_mold
  case(iobsType_pmsl ); obsmold_ =>    pmsl_mold
  case(iobsType_howv ); obsmold_ =>    howv_mold
  case(iobsType_tcamt); obsmold_ =>   tcamt_mold
  case(iobsType_lcbas); obsmold_ =>   lcbas_mold

  case(iobsType_pm10 ); obsmold_ =>    pm10_mold
  case(iobsType_cldch); obsmold_ =>   cldch_mold
  end select
end function index2vmold_

function vname2vmold_(vname) result(obsmold_)
  implicit none
  class(obsNode),pointer:: obsmold_
  character(len=*),intent(in):: vname

  character(len=*),parameter:: myname_=myname//"::vname2vmold_"
  integer(kind=i_kind):: i_obType

  i_obType=vname2index_(vname)
  obsmold_ => index2vmold_(i_obType)
end function vname2vmold_

end module m_obsNodeTypeManager
