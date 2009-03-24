!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !ROUTINE: make_satinfo - create a satinfo file for a given date-time
!
! !DESCRIPTION:
!
! !INTERFACE:
!#include "regime.H"
#include "assert.H"

    program make_satinfo
      use m_actvchan,only: actvchan_select,actvchan_clean,actvchan_show
      use m_satinst ,only:  satinst_select, satinst_clean, satinst_show
      use m_satops  ,only:   satops_select,  satops_clean,  satops_show

      use m_sitmpl    , only :  sitmpl_get,sitmpl_put,sitmpl_clean
      use satinfo_util, only : warn,perr,die,stdin,stdout
      use satinfo_util, only : alloc,realloc,dealloc
      !use satinfo_util
      implicit none

! !REVISION HISTORY:
! 	21Sep07	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- added "usable_channels" table for "passive" data.
!		- added user-definable values of INUSE flag. (setup.nml)
! 	24May07	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- redesigned with a more explicit database concept
!		- added additional tables to the existing database
!		- reengineered with modules
!		- removed dependency to GMAO libraries
!       04Jan07 - Jing Guo <guo@gmao.gsfc.nasa.gov>
!               - restructured the original &SETUP namelist to getjpch_
!		  to avoid crashing on every variable added to the GSI
!		  &SETUP namelist.
!		- modified exit messages to better inform users.
!	14Feb07 - R. Todling
!		- add usage routine
! 	before  - E. Liu
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='make_satinfo'
  integer,parameter :: PATHSIZE=512
  integer,parameter :: NAMESIZE=20
  integer,parameter :: DESCSIZE=80

  character(len=NAMESIZE),pointer,dimension(:) :: asat_nm => null()
  integer :: nasat=-1

  character(len=NAMESIZE),pointer,dimension(:) :: usbl_nm => null()
  character(len=NAMESIZE),pointer,dimension(:) :: usbl_sn => null()
  integer,pointer,dimension(:) :: usbl_ch => null()
  integer :: nusbl=-1

  character(len=NAMESIZE),pointer,dimension(:) :: actv_nm => null()
  character(len=NAMESIZE),pointer,dimension(:) :: actv_sn => null()
  integer,pointer,dimension(:) :: actv_ch => null()
  integer :: nactv=-1

  character(len=NAMESIZE),pointer,dimension(:) :: sens_nm => null()
  character(len=NAMESIZE),pointer,dimension(:) :: sens_sn => null()
  integer,pointer,dimension(:) :: sens_ch => null()
  integer :: nsens=-1

  character(len=NAMESIZE),pointer,dimension(:) :: tmpl_nusis => null()
  integer,pointer,dimension(:) :: tmpl_nusat => null()
  integer,pointer,dimension(:) :: tmpl_nuchn => null()
  integer,pointer,dimension(:) :: tmpl_inuse => null()
  real,pointer,dimension(:) :: tmpl_varch => null()
  real,pointer,dimension(:) :: tmpl_ermax => null()
  real,pointer,dimension(:) :: tmpl_brad => null()
  real,pointer,dimension(:) :: tmpl_pgrad => null()
  real,pointer,dimension(:) :: tmpl_polar => null()
  integer :: ntmpl=-1

    ! key parameters.  Must be defined explicitly.
  integer :: nymd
  integer :: nhms

    ! optional parameters.  Default values can be used.
  character(len=PATHSIZE) :: satinfo_tmpl = "satinfo.tmpl"
  character(len=PATHSIZE) :: satinfo_outf = "satinfo.txt"
  character(len=PATHSIZE) :: dbname        ='./sidb'
  character(len=PATHSIZE) ::   satops_tbl    ='satops.tbl'
  character(len=PATHSIZE) ::   satinst_tbl   ='satinst.tbl'
  character(len=PATHSIZE) ::   actvchan_tbl  ='active_channels.tbl'
  character(len=PATHSIZE) ::   usblchan_tbl  ='usable_channels.tbl'
  logical :: verbose=.false.
  logical :: nowarn =.false.
  logical :: samever=.false.
  integer :: NOTUSED_FLAG=-2
  integer :: USABLE_FLAG = 0
  integer :: ACTIVE_FLAG =+1

  integer :: itmpl
  integer :: iver
  integer :: ios
  logical :: warning

  namelist/setup/ nymd,nhms,dbname,satinfo_tmpl,satinfo_outf, &
    satops_tbl,satinst_tbl,actvchan_tbl,usblchan_tbl,verbose,samever, &
    NOTUSED_FLAG,USABLE_FLAG,ACTIVE_FLAG,nowarn

!!$ Get required and optional control arguments:
!!$			
  nymd = -HUGE(nymd)
  nhms = -HUGE(nhms)

  read(stdin,setup,iostat=ios)
  	if(ios/=0) call die(myname,'read(/setup/), iostat',ios)

    !!$ Verify /setup/ input for required arguments:
    !!$		nymd: <yyyymmdd>, e.g. 20050101 for 01/01/05
    !!$		nhms:   <hhmmss>, e.g.   083000 for 08:30:00

      if( nymd == -HUGE(nymd) .or. nhms == -HUGE(nhms) ) then
	if( nymd==-HUGE(nymd) ) call perr(myname,'undefined nymd')
	if( nhms==-HUGE(nhms) ) call perr(myname,'undefined nhms')
  	call die(myname,'read(/setup/)')
      endif
      warning=.not.nowarn

!!$ Get an active_channel list

    ! get a list of active satilletes from all satops_[satclasses] at
    ! the given date:time: [asat_nm]

      ! [date time date time <n> satnames]  --> asat_nm(:) nasat
  call satops_select(asat_nm,nasat,where_dt=(/nymd,nhms/), &
      from=trim(satops_tbl),dbase=dbname)
  if(verbose) call satops_show(asat_nm,nasat)

    ! sort+merge satops_satnms(:) would be nice for efficiency, but not
    ! necessary at this time.

!!$ Get a full_channels list
    ! [satnm satid <n> sensors]  --> satnms(:) satids(:) sensors(:)

  call satinst_select(sens_nm,sens_sn,sens_ch,nsens, &
    where_nm=asat_nm(1:nasat),from=satinst_tbl,dbase=dbname)
  if(verbose) call satinst_show(sens_nm,sens_sn,sens_ch,nsens)
  if(satinfo_tmpl=='') then
    call satinst_show(sens_nm,sens_sn,sens_ch,nsens)
    stop
  endif

!!$ Mark tmpl-channels list according to the active-channels list.
!!$   for the same sisname-sischan, set rad_flag = yes

  call sitmpl_get(satinfo_tmpl,ntmpl, &
    tmpl_nusis,tmpl_nusat,tmpl_nuchn,tmpl_inuse, &
    tmpl_varch,tmpl_ermax,tmpl_brad ,tmpl_pgrad, tmpl_polar, vern=iver )

!!$ All channels in the template are turned off
  tmpl_inuse(1:ntmpl)=NOTUSED_FLAG

!!$ Read the all usable-channels list (as passive channels)
  call actvchan_select(usbl_nm,usbl_sn,usbl_ch,nusbl, &
    where_dt=(/nymd,nhms/),where_nm=asat_nm(1:nasat), &
    from=usblchan_tbl,dbase=dbname)
  if(verbose) call actvchan_show(usbl_nm,usbl_sn,usbl_ch,nusbl)

  call tmpl_xcheck(USABLE_FLAG,tmpl_nusis,tmpl_nuchn,tmpl_inuse,ntmpl, &
    usbl_nm,usbl_sn,usbl_ch,nusbl, &
    sens_nm,sens_sn,sens_ch,nsens, tag='usable')

  call actvchan_clean(usbl_nm,usbl_sn,usbl_ch,nusbl)

!!$ Read the all active channel list (as active channels)
  call actvchan_select(actv_nm,actv_sn,actv_ch,nactv, &
    where_dt=(/nymd,nhms/),where_nm=asat_nm(1:nasat), &
    from=actvchan_tbl,dbase=dbname)
  if(verbose) call actvchan_show(actv_nm,actv_sn,actv_ch,nactv)

  call tmpl_xcheck(ACTIVE_FLAG,tmpl_nusis,tmpl_nuchn,tmpl_inuse,ntmpl, &
    actv_nm,actv_sn,actv_ch,nactv, &
    sens_nm,sens_sn,sens_ch,nsens, tag='active')

  call actvchan_clean(actv_nm,actv_sn,actv_ch,nactv)

  call satops_clean(asat_nm,nasat)
  call satinst_clean(sens_nm,sens_sn,sens_ch,nsens)

!!$ Verify tmpl_inuse flag values as a checksum verification for the
!!$ algorithm implementation.

  do itmpl=1,ntmpl
    if(.not. ((tmpl_inuse(itmpl)==NOTUSED_FLAG) .or. &
              (tmpl_inuse(itmpl)==USABLE_FLAG)  .or. &
              (tmpl_inuse(itmpl)==ACTIVE_FLAG)) ) then 
       call perr(myname,'  entry #',itmpl)
       call perr(myname,'  nusis =',tmpl_nusis(itmpl))
       call perr(myname,'  nuchn =',tmpl_nuchn(itmpl))
       call die(myname,'unknown tmpl_inuse value',tmpl_inuse(itmpl))
    endif
  end do

!!$ write out the satinfo table.
  if(.not.samever) iver=2 ! use the later version by the default

  call sitmpl_put(satinfo_outf,ntmpl, &
    tmpl_nusis,tmpl_nusat,tmpl_nuchn,tmpl_inuse, &
    tmpl_varch,tmpl_ermax,tmpl_brad ,tmpl_pgrad, tmpl_polar, &
    vern=iver,nymd=nymd,nhms=nhms)

  call sitmpl_clean(ntmpl,tmpl_nusis,tmpl_nusat,tmpl_nuchn,tmpl_inuse, &
                          tmpl_varch,tmpl_ermax,tmpl_brad ,tmpl_pgrad,tmpl_polar)

contains
subroutine tmpl_xcheck(inuse,tmpl_nusis,tmpl_nuchn,tmpl_inuse,ntmpl, &
    actv_nm,actv_sn,actv_ch,nactv, &
    sens_nm,sens_sn,sens_ch,nsens,tag)

  use satinfo_util,only : assert_,warn,tell
  implicit none
  integer,intent(in) :: inuse	! channel in-use flag: -1 (passive)
				!                   or +1 (active)

  character(len=*),dimension(:),intent(in   ) :: tmpl_nusis
  integer         ,dimension(:),intent(in   ) :: tmpl_nuchn
  integer         ,dimension(:),intent(inout) :: tmpl_inuse
  integer,intent(in) :: ntmpl

  character(len=*),dimension(:),intent(in) :: actv_nm
  character(len=*),dimension(:),intent(in) :: actv_sn
  integer         ,dimension(:),intent(in) :: actv_ch
  integer,intent(in) :: nactv
  
  character(len=*),dimension(:),intent(in) :: sens_nm
  character(len=*),dimension(:),intent(in) :: sens_sn
  integer         ,dimension(:),intent(in) :: sens_ch
  integer,intent(in) :: nsens

  character(len=*),intent(in) :: tag
  
  character(len=len(tmpl_nusis)) :: actv_sis
  integer :: actv_chn

  logical :: actv_matched
  integer :: isens,iactv,itmpl

  do iactv=1,nactv

        ! With a given (nm,ch) entry in the active-channels list,
        ! search the full-channels list for a match, to find the
        ! corresponding (sis,chn) value.

    actv_matched=.false.
    do isens=1,nsens
      actv_matched = sens_nm(isens)==actv_nm(iactv) .and. &
                     sens_sn(isens)==actv_sn(iactv) .and. &
                     sens_ch(isens)==actv_ch(iactv)

      if( actv_matched ) then
        actv_sis=trim(sens_sn(isens))//'_'//trim(sens_nm(isens))
        actv_chn=sens_ch(isens)
        exit
      endif
    enddo ! isens

    if((.not.actv_matched) .and. warning) then
      call warn(myname,trim(tag)//'_nm =',actv_nm(iactv))
      call warn(myname,trim(tag)//'_sn =',actv_sn(iactv))
      call warn(myname,trim(tag)//'_ch =',actv_ch(iactv))
      call warn(myname,'['//trim(tag)//'_nm:sn:ch] entry not seen in [satinst] table')
      cycle ! skip the satinfo-template-list searching
    endif

        ! With the (si,ch) of the matching (nm,ch), locate the same
        ! entry in the satinfo-template list, and set its _inuse flag.

    actv_matched=.false.
    do itmpl=1,ntmpl
      actv_matched = actv_sis==tmpl_nusis(itmpl) .and. &
                     actv_chn==tmpl_nuchn(itmpl)

      if( actv_matched ) then

        tmpl_inuse(itmpl)=inuse ! mark this channel to [inuse]
        actv_matched=.true.

        exit
      endif
    enddo

    if((.not.actv_matched) .and. warning) then
      call warn(myname,trim(tag)//'_sis =',actv_sis)
      call warn(myname,trim(tag)//'_chn =',actv_chn)
      call warn(myname,'['//trim(tag)//'_sis:chn] entry not seen in the satinfo-template')
      cycle
    endif
  enddo

end subroutine tmpl_xcheck
end program make_satinfo
