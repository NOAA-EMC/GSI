!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_gsiCheck - Show GSI background fields
!
! !DESCRIPTION:
!
! !INTERFACE:
!#include "regime.H"

    module m_gsiCheck
      implicit none
      private	! except

#ifdef _GMAO_FVGSI_
      public :: gsiCheck_show
      public :: gsiCheck_surface_show

    interface gsiCheck_show; module procedure	&
    	showdv_,show_; end interface
    interface gsiCheck_surface_show; module procedure	&
    	surface_show_; end interface

! !REVISION HISTORY:
! 	19Nov04	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_gsiCheck'

#include "assert.H"
#include "mytrace.H"
contains
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: showdv_ - interpolate from fvGrid to the GSI _subdomains_.
!
! !DESCRIPTION:
!
! !REVISION HISTORY:
!
!   2006-04-12  treadon - replace sigi with bk5
!
! !INTERFACE:

    subroutine showdv_(where,				&
		zsfc,lnps,uwnd,vwnd,tv,q,cwmr,oz,sfct,	&
		vor ,div , comm)

      use m_gsiGuess,only : gsiREAL

      use gridmod,	only: istart,ilat1
      use gridmod,	only: jstart,jlon1
      use gridmod,	only: lon1, lat1
      use gridmod,      only: lon2, lat2
      use gridmod,	only: nsig, bk5, n1
      use gridmod,	only: nlon, nlat

      use m_checksums,only : checksums_show
      use m_mpif90,only : MP_comm_rank
      use m_mpout ,only : mpout,mpout_log,mpout_ison
      use m_die,only : assert_,MP_die,die
      implicit none

      character(len=*),intent(in) :: where

      real(gsiREAL),dimension(:,:),intent(in) :: zsfc	! elevation
      real(gsiREAL),dimension(:,:),intent(in) :: lnps	! log(ps) in kPa
      real(gsiREAL),dimension(:,:,:),intent(in) :: vor ! 2d-vor
      real(gsiREAL),dimension(:,:,:),intent(in) :: div ! 2d-div
      real(gsiREAL),dimension(:,:,:),intent(in) :: uwnd ! u-wind
      real(gsiREAL),dimension(:,:,:),intent(in) :: vwnd ! v-wind
      real(gsiREAL),dimension(:,:,:),intent(in) :: tv   ! virt. temp.
      real(gsiREAL),dimension(:,:,:),intent(in) :: q    ! virt. temp.
      real(gsiREAL),dimension(:,:,:),intent(in) :: cwmr ! cld.wat.m.r.
      real(gsiREAL),dimension(:,:,:),intent(in) :: oz   ! ozone
      real(gsiREAL),dimension(:,:),intent(in) :: sfct	  ! sfc. tv

      integer,intent(in) :: comm	! MPI communicator

! !REVISION HISTORY:
! 	19Nov04	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::showdv_'
  integer :: iGdim,iGloc,iGlen
  integer :: jGdim,jGloc,jGlen
  integer :: ier,myPE
  integer :: kb,ke,k
  integer :: itime
_ALLENTRY_
!________________________________________

  	call MP_comm_rank(comm,myPE,ier)
		if(ier/=0) call MP_die(myname_,'MP_comm_rank()',ier)

  iGdim=nlon
  jGdim=nlat
  iGloc=jstart(myPE+1)
  iGlen=jlon1 (myPE+1)
  jGloc=istart(myPE+1)
  jGlen=ilat1 (myPE+1)

    call mpout_log(myname_,'iGdim (gsi:nlon) =',iGdim)
    call mpout_log(myname_,'jGdim (gsi:nlat) =',jGdim)
    call mpout_log(myname_,'iGloc (gsi:jstart) =',iGloc)
    call mpout_log(myname_,'jGloc (gsi:istart) =',jGloc)
    call mpout_log(myname_,'iGlen (gsi:jlon1) =',iGlen)
    call mpout_log(myname_,'jGlen (gsi:ilat1) =',jGlen)
    call mpout_log(myname_,'gsi:lon2 =',lon2)
    call mpout_log(myname_,'gsi:lat2 =',lat2)

  	ASSERT(iGlen==lon1)
  	ASSERT(jGlen==lat1)

#ifdef _TORM_
	ASSERT(lon2==size(ggrid_g3,3))
	ASSERT(lat2==size(ggrid_g3,2))

	ASSERT(nsig==size(rcwm,3))
	ASSERT(lon2==size(rcwm,2))
	ASSERT(lat2==size(rcwm,1))

	ASSERT(lon2==size(sfct,2))
	ASSERT(lat2==size(sfct,1))

	ASSERT(nsig==size(rdivs,3))
	ASSERT(lon2==size(rdivs,2))
	ASSERT(lat2==size(rdivs,1))

	ASSERT(nsig==size(rqlon,3))
	ASSERT(lon2==size(rqlon,2))
	ASSERT(lat2==size(rqlat,1))

	ASSERT(nsig==size(rtlon,3))
	ASSERT(lon2==size(rtlon,2))
	ASSERT(lat2==size(rtlat,1))
!________________________________________

    call mpout_log(myname_,'gsi:nsig =',nsig)
    call mpout_log(myname_,'gsi:n1   =',n1  )
    if(mpout_ison()) then
      write(mpout,'(2a)') myname_,':: -- nsig+1 sigma levels --'
      do kb=1,nsig+1,8
        ke=min(kb+7,nsig+1)
        write(mpout,'(8(f10.6))') (bk5(k),k=kb,ke)
      end do
    endif
#endif
!________________________________________
call mpout_log(myname_,trim(where))

_ALLTRACE_("gsi:2-d variables")
  call checksums_show(    sfct(:,:) ,'UNKW','gsi:sfct')
  call checksums_show(    zsfc(:,:) ,'UNKW','gsi:z')

  call checksums_show(exp(lnps(:,:)),'UNKW','gsi:ps')
  call checksums_show(    lnps(:,:) ,'UNKW','gsi:log(ps)')

_ALLTRACE_("gsi:(u,v)")
  call checksums_show(    uwnd(:,:,:) ,'UNKW','gsi:u')
  call checksums_show(    vwnd(:,:,:) ,'UNKW','gsi:v')
_ALLTRACE_("gsi:tv")
  call checksums_show(    tv  (:,:,:) ,'UNKW','gsi:tv')
_ALLTRACE_("gsi:q")
  call checksums_show(    q   (:,:,:) ,'UNKW','gsi:q')

_ALLTRACE_("gsi:div")
  call checksums_show(    div (:,:,:) ,'UNKW','gsi:div')
_ALLTRACE_("gsi:vor")
  call checksums_show(    vor (:,:,:) ,'UNKW','gsi:vor')
_ALLTRACE_("gsi:oz")
  call checksums_show(    oz  (:,:,:) ,'UNKW','gsi:oz')
_ALLTRACE_("gsi:cwmr")
  call checksums_show(    cwmr(:,:,:) ,'UNKW','gsi:cwmr')
_ALLEXIT_
end subroutine showdv_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: show_ - interpolate from fvGrid to the GSI _subdomains_.
!
! !DESCRIPTION:
!
! !REVISION HISTORY:
!
!   2006-04-12  treadon - replace sigi with bk5
!
! !INTERFACE:

    subroutine show_(where,				&
		zsfc,lnps,uwnd,vwnd,tv,q,cwmr,oz,sfct,	&
		comm)

      use m_gsiGuess,only : gsiREAL

      use gridmod,	only: istart,ilat1
      use gridmod,	only: jstart,jlon1
      use gridmod,	only: lon1, lat1
      use gridmod,      only: lon2, lat2
      use gridmod,	only: nsig, bk5, n1
      use gridmod,	only: nlon, nlat

      use m_checksums,only : checksums_show
      use m_mpif90,only : MP_comm_rank
      use m_mpout ,only : mpout,mpout_log,mpout_ison
      use m_die,only : assert_,MP_die,die
      implicit none

      character(len=*),intent(in) :: where

      real(gsiREAL),dimension(:,:),intent(in) :: zsfc	! elevation
      real(gsiREAL),dimension(:,:),intent(in) :: lnps	! log(ps) in kPa
      real(gsiREAL),dimension(:,:,:),intent(in) :: uwnd ! u-wind
      real(gsiREAL),dimension(:,:,:),intent(in) :: vwnd ! v-wind
      real(gsiREAL),dimension(:,:,:),intent(in) :: tv   ! virt. temp.
      real(gsiREAL),dimension(:,:,:),intent(in) :: q    ! virt. temp.
      real(gsiREAL),dimension(:,:,:),intent(in) :: cwmr ! cld.wat.m.r.
      real(gsiREAL),dimension(:,:,:),intent(in) :: oz   ! ozone
      real(gsiREAL),dimension(:,:),intent(in) :: sfct	  ! sfc. tv

      integer,intent(in) :: comm	! MPI communicator

! !REVISION HISTORY:
! 	19Nov04	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::show_'
  integer :: iGdim,iGloc,iGlen
  integer :: jGdim,jGloc,jGlen
  integer :: ier,myPE
  integer :: kb,ke,k
  integer :: itime
_ALLENTRY_
!________________________________________

  	call MP_comm_rank(comm,myPE,ier)
		if(ier/=0) call MP_die(myname_,'MP_comm_rank()',ier)

  iGdim=nlon
  jGdim=nlat
  iGloc=jstart(myPE+1)
  iGlen=jlon1 (myPE+1)
  jGloc=istart(myPE+1)
  jGlen=ilat1 (myPE+1)

    call mpout_log(myname_,'iGdim (gsi:nlon) =',iGdim)
    call mpout_log(myname_,'jGdim (gsi:nlat) =',jGdim)
    call mpout_log(myname_,'iGloc (gsi:jstart) =',iGloc)
    call mpout_log(myname_,'jGloc (gsi:istart) =',jGloc)
    call mpout_log(myname_,'iGlen (gsi:jlon1) =',iGlen)
    call mpout_log(myname_,'jGlen (gsi:ilat1) =',jGlen)
    call mpout_log(myname_,'gsi:lon2 =',lon2)
    call mpout_log(myname_,'gsi:lat2 =',lat2)

  	ASSERT(iGlen==lon1)
  	ASSERT(jGlen==lat1)

#ifdef _TORM_
	ASSERT(lon2==size(ggrid_g3,3))
	ASSERT(lat2==size(ggrid_g3,2))

	ASSERT(nsig==size(rcwm,3))
	ASSERT(lon2==size(rcwm,2))
	ASSERT(lat2==size(rcwm,1))

	ASSERT(lon2==size(sfct,2))
	ASSERT(lat2==size(sfct,1))

	ASSERT(nsig==size(rdivs,3))
	ASSERT(lon2==size(rdivs,2))
	ASSERT(lat2==size(rdivs,1))

	ASSERT(nsig==size(rqlon,3))
	ASSERT(lon2==size(rqlon,2))
	ASSERT(lat2==size(rqlat,1))

	ASSERT(nsig==size(rtlon,3))
	ASSERT(lon2==size(rtlon,2))
	ASSERT(lat2==size(rtlat,1))
!________________________________________

    call mpout_log(myname_,'gsi:nsig =',nsig)
    call mpout_log(myname_,'gsi:n1   =',n1  )
    if(mpout_ison()) then
      write(mpout,'(2a)') myname_,':: -- nsig+1 sigma levels --'
      do kb=1,nsig+1,8
        ke=min(kb+7,nsig+1)
        write(mpout,'(8(f10.6))') (bk5(k),k=kb,ke)
      end do
    endif
#endif
!________________________________________
call mpout_log(myname_,trim(where))

_ALLTRACE_("gsi:2-d variables")
  call checksums_show(    sfct(:,:) ,'UNKW','gsi:sfct')
  call checksums_show(    zsfc(:,:) ,'UNKW','gsi:z')

  call checksums_show(exp(lnps(:,:)),'UNKW','gsi:ps')
  call checksums_show(    lnps(:,:) ,'UNKW','gsi:log(ps)')

_ALLTRACE_("gsi:(u,v)")
  call checksums_show(    uwnd(:,:,:) ,'UNKW','gsi:u')
  call checksums_show(    vwnd(:,:,:) ,'UNKW','gsi:v')
_ALLTRACE_("gsi:tv")
  call checksums_show(    tv  (:,:,:) ,'UNKW','gsi:tv')
_ALLTRACE_("gsi:q")
  call checksums_show(    q   (:,:,:) ,'UNKW','gsi:q')

_ALLTRACE_("gsi:oz")
  call checksums_show(    oz  (:,:,:) ,'UNKW','gsi:oz')
_ALLTRACE_("gsi:cwmr")
  call checksums_show(    cwmr(:,:,:) ,'UNKW','gsi:cwmr')
_ALLEXIT_
end subroutine show_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: surface_show_ - show surface data in GSI _subdomains_.
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine surface_show_(where,			&
		ws10,tskn,snow_dep,veg_type,veg_frac,	&
		soil_type,soil_temp,soil_mois,		&
		isli_mask,isli_glbl,	comm)

      use m_gsiGuess,only : gsiREAL
      use m_gsiGuess,only : gsiINTG

      use gridmod,	only: istart,ilat1
      use gridmod,	only: jstart,jlon1
      use gridmod,	only: lon1, lat1
      use gridmod,      only: lon2, lat2
      use gridmod,	only: nlon, nlat

      use m_checksums,only : checksums_show
      use m_mpif90,only : MP_comm_rank
      use m_mpout ,only : mpout,mpout_log,mpout_ison
      use m_die,only : assert_,MP_die,die
      implicit none

      character(len=*),intent(in) :: where

      real(gsiREAL),dimension(:,:),intent(in) :: ws10
      real(gsiREAL),dimension(:,:),intent(in) :: tskn
      real(gsiREAL),dimension(:,:),intent(in) :: snow_dep
      real(gsiREAL),dimension(:,:),intent(in) :: veg_type
      real(gsiREAL),dimension(:,:),intent(in) :: veg_frac
      real(gsiREAL),dimension(:,:),intent(in) :: soil_type
      real(gsiREAL),dimension(:,:),intent(in) :: soil_temp
      real(gsiREAL),dimension(:,:),intent(in) :: soil_mois
      integer(gsiINTG),dimension(:,:),intent(in) :: isli_mask
      integer(gsiINTG),dimension(:,:),intent(in) :: isli_glbl

      integer,intent(in) :: comm	! MPI communicator

! !REVISION HISTORY:
! 	19Nov04	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::surface_show_'
  integer :: iGdim,iGloc,iGlen
  integer :: jGdim,jGloc,jGlen
  integer :: ier,myPE
  integer :: kb,ke,k
  integer :: itime
_ALLENTRY_
!________________________________________

  	call MP_comm_rank(comm,myPE,ier)
		if(ier/=0) call MP_die(myname_,'MP_comm_rank()',ier)

  iGdim=nlon
  jGdim=nlat
  iGloc=jstart(myPE+1)
  iGlen=jlon1 (myPE+1)
  jGloc=istart(myPE+1)
  jGlen=ilat1 (myPE+1)

    call mpout_log(myname_,'iGdim (gsi:nlon) =',iGdim)
    call mpout_log(myname_,'jGdim (gsi:nlat) =',jGdim)
    call mpout_log(myname_,'iGloc (gsi:jstart) =',iGloc)
    call mpout_log(myname_,'jGloc (gsi:istart) =',jGloc)
    call mpout_log(myname_,'iGlen (gsi:jlon1) =',iGlen)
    call mpout_log(myname_,'jGlen (gsi:ilat1) =',jGlen)
    call mpout_log(myname_,'gsi:lon2 =',lon2)
    call mpout_log(myname_,'gsi:lat2 =',lat2)

  	ASSERT(iGlen==lon1)
  	ASSERT(jGlen==lat1)

	ASSERT(lon2==size(ws10,2))
	ASSERT(lat2==size(ws10,1))
	ASSERT(lon2==size(tskn,2))
	ASSERT(lat2==size(tskn,1))
	ASSERT(lon2==size(snow_dep ,2))
	ASSERT(lat2==size(snow_dep ,1))
	ASSERT(lon2==size( veg_type,2))
	ASSERT(lat2==size( veg_type,1))
	ASSERT(lon2==size( veg_frac,2))
	ASSERT(lat2==size( veg_frac,1))
	ASSERT(lon2==size(soil_type,2))
	ASSERT(lat2==size(soil_type,1))
	ASSERT(lon2==size(soil_temp,2))
	ASSERT(lat2==size(soil_temp,1))
	ASSERT(lon2==size(soil_mois,2))
	ASSERT(lat2==size(soil_mois,1))
	ASSERT(lon2==size(isli_mask,2))
	ASSERT(lat2==size(isli_mask,1))
	ASSERT(nlon==size(isli_glbl,2))
	ASSERT(nlat==size(isli_glbl,1))
!________________________________________
call mpout_log(myname_,trim(where))

_ALLTRACE_("gsi:surface variables")
  call checksums_show(     ws10(:,:) ,'UNKW','gsi:ws10')
  call checksums_show(     tskn(:,:) ,'UNKW','gsi:tskn')
  call checksums_show(snow_dep (:,:) ,'UNKW','gsi:snow_dep')
  call checksums_show( veg_type(:,:) ,'UNKW','gsi:veg_type')
  call checksums_show( veg_frac(:,:) ,'UNKW','gsi:veg_frac')
  call checksums_show(soil_type(:,:) ,'UNKW','gsi:soil_type')
  call checksums_show(soil_temp(:,:) ,'UNKW','gsi:soil_temp')
  call checksums_show(soil_mois(:,:) ,'UNKW','gsi:soil_mois')
  call checksums_show(isli_mask(:,:)*1._gsiREAL,'UNKW','gsi:isli_mask')
  call checksums_show(isli_glbl(:,:)*1._gsiREAL,'UNKW','gsi:isli_glbl')
_ALLEXIT_
end subroutine surface_show_
#endif
end module m_gsiCheck
