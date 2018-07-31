module gsi_obOperTypeManager
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module gsi_obOperTypeManager
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2018-07-12
!
! abstract: GSI observation operator (obOper) type manager
!
! program history log:
!   2018-07-12  j guo   - added this document block
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

  use gsi_aeroOper    , only: aeroOper    
  use gsi_cldchOper   , only: cldchOper   
  use gsi_colvkOper   , only: colvkOper   
  use gsi_dwOper      , only: dwOper      
  use gsi_gpsOper     , only: gpsOper     
  use gsi_gustOper    , only: gustOper    
  use gsi_howvOper    , only: howvOper    
  use gsi_lcbasOper   , only: lcbasOper   
  use gsi_lwcpOper    , only: lwcpOper    
  use gsi_mitmOper    , only: mitmOper    
  use gsi_mxtmOper    , only: mxtmOper    
  use gsi_o3lOper     , only: o3lOper     
  use gsi_ozOper      , only: ozOper      
  use gsi_pblhOper    , only: pblhOper    
  use gsi_pcpOper     , only: pcpOper     
  use gsi_pm10Oper    , only: pm10Oper    
  use gsi_pm2_5Oper   , only: pm2_5Oper   
  use gsi_pmslOper    , only: pmslOper    
  use gsi_psOper      , only: psOper      
  use gsi_pwOper      , only: pwOper      
  use gsi_qOper       , only: qOper       
  use gsi_radOper     , only: radOper     
  use gsi_rwOper      , only: rwOper      
  use gsi_spdOper     , only: spdOper     
  use gsi_sstOper     , only: sstOper     
  use gsi_swcpOper    , only: swcpOper    
  use gsi_tcamtOper   , only: tcamtOper   
  use gsi_tcpOper     , only: tcpOper     
  use gsi_td2mOper    , only: td2mOper    
  use gsi_tOper       , only: tOper       
  use gsi_uwnd10mOper , only: uwnd10mOper 
  use gsi_visOper     , only: visOper     
  use gsi_vwnd10mOper , only: vwnd10mOper 
  use gsi_wOper       , only: wOper       
  use gsi_wspd10mOper , only: wspd10mOper 

  use kinds     , only: i_kind
  use mpeu_util , only: perr,die
  implicit none
  private	! except

  public:: obOper_typeMold
  public:: obOper_typeIndex
        interface obOper_typeMold; module procedure &
                dtype2vmold_,   &
                index2vmold_    ; end interface
        interface obOper_typeIndex; module procedure &
                vmold2index_,   &
                dtype2index_    ; end interface

  !public:: obOper_config
  !      interface obOper_config; module procedure config_; end interface

  public:: obOper_undef
  public:: obOper_lbound
  public:: obOper_ubound
  public:: obOper_size

  enum, bind(C)
    enumerator:: iobOper_zero_   = 0

    enumerator:: iobOper_ps
    enumerator:: iobOper_t
    enumerator:: iobOper_w
    enumerator:: iobOper_q
    enumerator:: iobOper_spd
    enumerator:: iobOper_rw
    enumerator:: iobOper_dw
    enumerator:: iobOper_sst
    enumerator:: iobOper_pw
    enumerator:: iobOper_pcp
    enumerator:: iobOper_oz
    enumerator:: iobOper_o3l
    enumerator:: iobOper_gps
    enumerator:: iobOper_rad
    enumerator:: iobOper_tcp
   !enumerator:: iobOper_lag
    enumerator:: iobOper_colvk
    enumerator:: iobOper_aero
   !enumerator:: iobOper_aerol
    enumerator:: iobOper_pm2_5
    enumerator:: iobOper_gust
    enumerator:: iobOper_vis
    enumerator:: iobOper_pblh
    enumerator:: iobOper_wspd10m
    enumerator:: iobOper_td2m
    enumerator:: iobOper_mxtm
    enumerator:: iobOper_mitm
    enumerator:: iobOper_pmsl
    enumerator:: iobOper_howv
    enumerator:: iobOper_tcamt
    enumerator:: iobOper_lcbas
    enumerator:: iobOper_pm10
    enumerator:: iobOper_cldch
    enumerator:: iobOper_uwnd10m
    enumerator:: iobOper_vwnd10m
    enumerator:: iobOper_swcp
    enumerator:: iobOper_lwcp

    enumerator:: iobOper_extra_
  end enum
  
  integer,parameter:: enum_kind = kind(iobOper_zero_)

  integer(enum_kind),parameter:: obOper_undef  = -1_enum_kind
  integer(enum_kind),parameter:: obOper_lbound = iobOper_zero_ +1
  integer(enum_kind),parameter:: obOper_ubound = iobOper_extra_-1
  integer(enum_kind),parameter:: obOper_size   = obOper_ubound-obOper_lbound+1

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname='gsi_obOperTypeManager'
  logical,save:: obOper_configured_ = .false.

  type(     psOper), target, save::       psOper_mold
  type(      tOper), target, save::        tOper_mold
  type(      wOper), target, save::        wOper_mold
  type(      qOper), target, save::        qOper_mold
  type(    spdOper), target, save::      spdOper_mold
  type(     rwOper), target, save::       rwOper_mold
  type(     dwOper), target, save::       dwOper_mold
  type(    sstOper), target, save::      sstOper_mold
  type(     pwOper), target, save::       pwOper_mold
  type(    pcpOper), target, save::      pcpOper_mold
  type(     ozOper), target, save::       ozOper_mold
  type(    o3lOper), target, save::      o3lOper_mold
  type(    gpsOper), target, save::      gpsOper_mold
  type(    radOper), target, save::      radOper_mold
  type(    tcpOper), target, save::      tcpOper_mold
 !type(    lagOper), target, save::      lagOper_mold
  type(  colvkOper), target, save::    colvkOper_mold
  type(   aeroOper), target, save::     aeroOper_mold
 !type(  aerolOper), target, save::    aerolOper_mold
  type(  pm2_5Oper), target, save::    pm2_5Oper_mold
  type(   gustOper), target, save::     gustOper_mold
  type(    visOper), target, save::      visOper_mold
  type(   pblhOper), target, save::     pblhOper_mold
  type(wspd10mOper), target, save::  wspd10mOper_mold
  type(   td2mOper), target, save::     td2mOper_mold
  type(   mxtmOper), target, save::     mxtmOper_mold
  type(   mitmOper), target, save::     mitmOper_mold
  type(   pmslOper), target, save::     pmslOper_mold
  type(   howvOper), target, save::     howvOper_mold
  type(  tcamtOper), target, save::    tcamtOper_mold
  type(  lcbasOper), target, save::    lcbasOper_mold
  type(   pm10Oper), target, save::     pm10Oper_mold
  type(  cldchOper), target, save::    cldchOper_mold
  type(uwnd10mOper), target, save::  uwnd10mOper_mold
  type(vwnd10mOper), target, save::  vwnd10mOper_mold
  type(   swcpOper), target, save::     swcpOper_mold
  type(   lwcpOper), target, save::     lwcpOper_mold

contains
function dtype2index_(dtype) result(index_)
  implicit none
  integer(i_kind):: index_
  character(len=*),intent(in):: dtype

  select case(dtype)
  case("ps"     ,"[psoper]"     ); index_= iobOper_ps
  case("t"      ,"[toper]"      ); index_= iobOper_t

  case("w"      ,"[woper]"      ); index_= iobOper_w
    case("uv"     ); index_= iobOper_w

  case("q"      ,"[qoper]"      ); index_= iobOper_q
  case("spd"    ,"[spdoper]"    ); index_= iobOper_spd
  case("rw"     ,"[rwoper]"     ); index_= iobOper_rw
  case("dw"     ,"[dwoper]"     ); index_= iobOper_dw
  case("sst"    ,"[sstoper]"    ); index_= iobOper_sst
  case("pw"     ,"[pwoper]"     ); index_= iobOper_pw
  case("pcp"    ,"[pcpoper]"    ); index_= iobOper_pcp

  case("oz"     ,"[ozoper]"     ); index_= iobOper_oz
    case("sbuv2"  ); index_= iobOper_oz
    case("omi"    ); index_= iobOper_oz
    case("gome"   ); index_= iobOper_oz

  case("o3l"    ,"[o3loper]"    ); index_= iobOper_o3l
    case("o3lev"  ); index_= iobOper_o3l
    case("mls20"  ); index_= iobOper_o3l
    case("mls22"  ); index_= iobOper_o3l
    case("mls30"  ); index_= iobOper_o3l
    case("mls55"  ); index_= iobOper_o3l
    case("omieff" ); index_= iobOper_o3l
    case("tomseff"); index_= iobOper_o3l

  case("gps"    ,"[gpsoper]"    ); index_= iobOper_gps
    case("gps_ref"); index_= iobOper_gps
    case("gps_bnd"); index_= iobOper_gps

  case("rad"    ,"[radoper]"    ); index_= iobOper_rad
        ! platid /= "aqua"
    case("amsua"  ); index_= iobOper_rad
    case("amsub"  ); index_= iobOper_rad
    case("msu"    ); index_= iobOper_rad
    case("mhs"    ); index_= iobOper_rad
    case("hirs2"  ); index_= iobOper_rad
    case("hirs3"  ); index_= iobOper_rad
    case("hirs4"  ); index_= iobOper_rad
    case("ssu"    ); index_= iobOper_rad
        !
    case("atms"   ); index_= iobOper_rad
    case("saphir" ); index_= iobOper_rad
        ! platid == "aqua"
   !case("amsua"  ); index_= iobOper_rad
    case("airs"   ); index_= iobOper_rad
    case("hsb"    ); index_= iobOper_rad
        !
    case("iasi"   ); index_= iobOper_rad
    case("cris"   ); index_= iobOper_rad
    case("cris-fsr"  ); index_= iobOper_rad
        !
    case("sndr"   ); index_= iobOper_rad
    case("sndrd1" ); index_= iobOper_rad
    case("sndrd2" ); index_= iobOper_rad
    case("sndrd3" ); index_= iobOper_rad
    case("sndrd4" ); index_= iobOper_rad
        !
    case("ssmi"   ); index_= iobOper_rad
        !
    case("amsre"  ); index_= iobOper_rad
    case("amsre_low"); index_= iobOper_rad
    case("amsre_mid"); index_= iobOper_rad
    case("amsre_hig"); index_= iobOper_rad
        !
    case("ssmis"  ); index_= iobOper_rad
    case("ssmis_las"); index_= iobOper_rad
    case("ssmis_uas"); index_= iobOper_rad
    case("ssmis_img"); index_= iobOper_rad
    case("ssmis_env"); index_= iobOper_rad
        !
    case("amsr2"  ); index_= iobOper_rad
    case("goes_img"); index_= iobOper_rad
    case("gmi"    ); index_= iobOper_rad
    case("seviri" ); index_= iobOper_rad
    case("ahi"    ); index_= iobOper_rad
        !
    case("avhrr_navy"); index_= iobOper_rad
    case("avhrr"  ); index_= iobOper_rad

  case("tcp"    ,"[tcpoper]"    ); index_= iobOper_tcp

 !case("lag"    ,"[lagoper]"    ); index_= iobOper_lag

  case("colvk"  ,"[colvkoper]"  ); index_= iobOper_colvk
    case("mopitt" ); index_= iobOper_colvk

  case("aero"   ,"[aerooper]"   ); index_= iobOper_aero
    case("aod"    ); index_= iobOper_aero
    case("modis_aod"); index_= iobOper_aero

 !case("aerol"  ,"[aeroloper]"  ); index_= iobOper_aerol

  case("pm2_5"  ,"[pm2_5oper]"  ); index_= iobOper_pm2_5
  case("gust"   ,"[gustoper]"   ); index_= iobOper_gust
  case("vis"    ,"[visoper]"    ); index_= iobOper_vis
  case("pblh"   ,"[pblhoper]"   ); index_= iobOper_pblh

  case("wspd10m","[wspd10moper]"); index_= iobOper_wspd10m
  case("uwnd10m","[uwnd10moper]"); index_= iobOper_uwnd10m
  case("vwnd10m","[vwnd10moper]"); index_= iobOper_vwnd10m

  case("td2m"   ,"[td2moper]"   ); index_= iobOper_td2m
  case("mxtm"   ,"[mxtmoper]"   ); index_= iobOper_mxtm
  case("mitm"   ,"[mitmoper]"   ); index_= iobOper_mitm
  case("pmsl"   ,"[pmsloper]"   ); index_= iobOper_pmsl
  case("howv"   ,"[howvoper]"   ); index_= iobOper_howv
  case("tcamt"  ,"[tcamtoper]"  ); index_= iobOper_tcamt
  case("lcbas"  ,"[lcbasoper]"  ); index_= iobOper_lcbas

  case("pm10"   ,"[pm10oper]"   ); index_= iobOper_pm10
  case("cldch"  ,"[cldchoper]"  ); index_= iobOper_cldch

  case("swcp"   ,"[swcpoper]"   ); index_= iobOper_swcp
  case("lwcp"   ,"[lwcpoper]"   ); index_= iobOper_lwcp

        ! Known dtype values, but no known obOper type defined
  case("mta_cld"); index_= obOper_undef
  case("gos_ctp"); index_= obOper_undef
  case("rad_ref"); index_= obOper_undef
  case("lghtn"  ); index_= obOper_undef
  case("larccld"); index_= obOper_undef
  case("larcglb"); index_= obOper_undef
        ! A catch all case
  case default   ; index_= obOper_undef
  end select
end function dtype2index_

function vmold2index_(mold) result(index_)
  implicit none
  integer(i_kind):: index_
  class(obOper),target,intent(in):: mold

  index_=dtype2index_(mold%mytype())

  ! An alternative implementation to cache a managed iobOper value inside each
  ! obOper class.  This implementation requires two new TBPs, %myinfo_get() and
  ! %myinfo_set().
  !
  ! call mold%myinfo_get(iobOper=index_)
  ! if(index_<obOper_lbound .or. index_>obOper_ubound) then
  !   index_=dtype2index_(mold%mytype())
  !   call mold%myinfo_set(iobOper_=index_)
  ! endif

end function vmold2index_

function dtype2vmold_(dtype) result(vmold_)
  implicit none
  class(obOper),pointer:: vmold_
  character(len=*),intent(in):: dtype

  integer(i_kind):: iobOper_
  iobOper_ = dtype2index_(dtype)
  vmold_ => index2vmold_(iobOper_)
end function dtype2vmold_

function index2vmold_(iobOper) result(vmold_)
  implicit none
  class(obOper),pointer:: vmold_
  integer(i_kind),intent(in):: iobOper
  select case(iobOper)

  case(iobOper_ps       ); vmold_ =>      psOper_mold
  case(iobOper_t        ); vmold_ =>       tOper_mold
  case(iobOper_w        ); vmold_ =>       wOper_mold
  case(iobOper_q        ); vmold_ =>       qOper_mold
  case(iobOper_spd      ); vmold_ =>     spdOper_mold
  case(iobOper_rw       ); vmold_ =>      rwOper_mold
  case(iobOper_dw       ); vmold_ =>      dwOper_mold
  case(iobOper_sst      ); vmold_ =>     sstOper_mold
  case(iobOper_pw       ); vmold_ =>      pwOper_mold
  case(iobOper_pcp      ); vmold_ =>     pcpOper_mold
  case(iobOper_oz       ); vmold_ =>      ozOper_mold
  case(iobOper_o3l      ); vmold_ =>     o3lOper_mold
  case(iobOper_gps      ); vmold_ =>     gpsOper_mold
  case(iobOper_rad      ); vmold_ =>     radOper_mold
  case(iobOper_tcp      ); vmold_ =>     tcpOper_mold
 !case(iobOper_lag      ); vmold_ =>     lagOper_mold
  case(iobOper_colvk    ); vmold_ =>   colvkOper_mold
  case(iobOper_aero     ); vmold_ =>    aeroOper_mold
 !case(iobOper_aerol    ); vmold_ =>   aerolOper_mold
  case(iobOper_pm2_5    ); vmold_ =>   pm2_5Oper_mold
  case(iobOper_gust     ); vmold_ =>    gustOper_mold
  case(iobOper_vis      ); vmold_ =>     visOper_mold
  case(iobOper_pblh     ); vmold_ =>    pblhOper_mold
  case(iobOper_wspd10m  ); vmold_ => wspd10mOper_mold
  case(iobOper_td2m     ); vmold_ =>    td2mOper_mold
  case(iobOper_mxtm     ); vmold_ =>    mxtmOper_mold
  case(iobOper_mitm     ); vmold_ =>    mitmOper_mold
  case(iobOper_pmsl     ); vmold_ =>    pmslOper_mold
  case(iobOper_howv     ); vmold_ =>    howvOper_mold
  case(iobOper_tcamt    ); vmold_ =>   tcamtOper_mold
  case(iobOper_lcbas    ); vmold_ =>   lcbasOper_mold
  case(iobOper_pm10     ); vmold_ =>    pm10Oper_mold
  case(iobOper_cldch    ); vmold_ =>   cldchOper_mold
  case(iobOper_uwnd10m  ); vmold_ => uwnd10mOper_mold
  case(iobOper_vwnd10m  ); vmold_ => vwnd10mOper_mold
  case(iobOper_swcp     ); vmold_ =>    swcpOper_mold
  case(iobOper_lwcp     ); vmold_ =>    lwcpOper_mold

  case( obOper_undef    ); vmold_ => null()
  case default           ; vmold_ => null()
  end select
end function index2vmold_

subroutine config_()
  implicit none
  character(len=*),parameter:: myname_=myname//"::config_"
  class(obOper),pointer:: vmold_
  integer(i_kind):: iset_,iget_
  logical:: good_

  good_=.true.
  do iset_ = obOper_lbound, obOper_ubound
    vmold_ => index2vmold_(iset_)
        if(.not.associated(vmold_)) then
          call perr(myname_,'unexpected index, iset_ =',iset_)
          call perr(myname_,'          obOper_lbound =',obOper_lbound)
          call perr(myname_,'          obOper_ubound =',obOper_ubound)
          call  die(myname_)
        endif

    iget_=iset_         ! for additional test.
    !call vmold_%myinfo_set(iobOper=iset_)
    !call vmold_%myinfo_get(iobOper=iget_)
        if(iget_/=iset_) then
          call perr(myname_,'unexpected return, %myinfo_get(iobOper) =',iget_)
          call perr(myname_,'                   %myinfo_set(iobOper) =',iset_)
          call perr(myname_,'                              %mytype() =',vmold_%mytype())
          good_=.false.
        endif

    vmold_ => null()
  enddo
  if(.not.good_) call die(myname_)

  obOper_configured_ = .true.
end subroutine config_

end module gsi_obOperTypeManager
