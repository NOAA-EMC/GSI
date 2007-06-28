!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_gsiGuess - An interface to the GSI guess_grids
!
! !DESCRIPTION:
!
! !INTERFACE:
!#include "regime.H"

    module m_gsiGuess

      use jfunc,	only: jiterstart
      use guess_grids,	only: nfldsig, ifilesig
      use guess_grids,	only: nfldsfc, ifilesfc
      use guess_grids,	only: ntguessig

      use kinds,  only : gsiSNGL => r_single
      use kinds,  only : gsiREAL => r_kind
      use kinds,  only : gsiINTG => i_kind

      implicit none
      private	! except

      public :: gsiREAL
      public :: gsiSNGL
      public :: gsiINTG

      public :: jiterstart	! the first outer loop iteration
      public :: nfldsig		! number of guess states
      public :: ifilesig	! INTEGER tags of all guess states
      public :: ntguessig	! the index to the analysis time.

#ifdef _GMAO_FVGSI_
	! This blocks off the rest of this module, since there is
	! nothing needed if m_fvAnaGrid is ifdefed too.

      public :: gsiGuessGrid_nmlread
		! input namelist/gsiGuess_opts/, to specify the user's
		! intention on the gsiGuess grid definition.

      public :: gsiGuessGrid_setup
      		! gsiGuessGrid_setup() defines the global grid
		! specifications of gsiGuess gridded data.  These global
		! grid specifications include the grid sizes and their
		! grid points reference values in each dimensions, and
		! distributed to all processors.  It sets the stage to
		! determine domain decompostion of the gridded guess
		! fields by other routines.

      public :: gsiGuess_intp
      		! gsiGuess_intp() interpolates a state on a distributed
		! FV _interleaved-levels_ grid to the GSI internal
		! distributed sigma-Gaussian _subdomains_ grid.

      public :: gsiGuess_surface_intp
      		! gsiGuess_surface_intp() interpolates surface states
		! on a distributed FV _interleaved-levels_ grid to the
		! GSI internal distributed sigma-Gaussian or whatnot
		! _subdomains_ grid.

      public :: gsiGuess_unintp
      		! gsiGuess_unintp() interpolates a state on the GSI
		! internal distributed sigma-Gaussian _subdomains_ grid
		! to a distributed FV _interleaved-levels_ grid.
		! gsiGuess_unintp() is an approximate inverse operation
		! of gsiGuess_intp().

      public :: aincout
      public :: aincfile
      public :: phisfile

      public :: init_bias_lnps
      public :: init_bias_ts
      public :: init_bias_tv
      public :: init_bias_wind
      public :: init_bias_cwmr
      public :: init_bias_q
      public :: init_bias_oz

    interface gsiGuessGrid_nmlread; module procedure	&
    	nmlread_; end interface
    interface gsiGuessGrid_setup; module procedure	&
    	gridsetup_; end interface

    interface gsiGuess_intp; module procedure	&
    	intp_; end interface
    interface gsiGuess_surface_intp; module procedure	&
	fvSurfintp_,	&
    	ncSurfintp_; end interface
    interface gsiGuess_unintp; module procedure	&
    	unintp_; end interface

! !REVISION HISTORY:
! 	19Nov04	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
! 	09Dec05	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- clean up with additional comments
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_gsiGuess'

  real(kind=gsiREAL),parameter :: kPa_per_Pa=.001_gsiREAL
  real(kind=gsiREAL),parameter :: Pa_per_kPa=1000._gsiREAL
  real(kind=gsiREAL),parameter :: hPa_per_kPa=10._gsiREAL
  real(kind=gsiREAL),parameter :: GG_per_PPMV=1.657e-6_gsiREAL

#ifdef _IGNORE_GRIDVERIFY_
  logical,parameter :: GRIDVERIFY_=.false.
#else
  logical,parameter :: GRIDVERIFY_=.true.
#endif

  logical,save :: LLGrid_=.false.	! Flag equal-interval latitude
  logical,save :: fvGrid_=.false.	! Flag using of FV vertical grid

		! By default, Gaussian grid is used for the GSI internal
		! guess grid.  However, if the input grid (FV) has the
		! same sizes in both longitude and latitude grids, this
		! program will assume the program is configured to run
		! on the same horizonal grid as the input FV data.

  integer,parameter :: hGrid_Auto = 0
  integer,parameter :: hGrid_EqualSpacing = 1
  integer,parameter :: hGrid_Gaussian = 2
  integer,parameter :: vGrid_Auto = 0
  integer,parameter :: vGrid_UserDefined = 1	! via. this namelist

  logical,save :: initialized_=.false.
  integer,save :: hGrid_type
  integer,save :: vGrid_type
  integer,save :: vGrid_size
  real(gsiREAL),save :: vGrid_unit
  real(gsiREAL),save,allocatable,dimension(:) :: vGrid_ak,vGrid_bk

  logical,save :: aincout = .true.
  character(len=256),save :: aincfile = '.none.' ! a-grid binary dump

  character(len=256),save :: phisfile = '.none.'

  logical,save :: init_bias_lnps = .false.
  logical,save :: init_bias_ts   = .false.
  logical,save :: init_bias_tv   = .false.
  logical,save :: init_bias_wind = .false. ! all wind variables.
  logical,save :: init_bias_cwmr = .false.
  logical,save :: init_bias_q    = .false.
  logical,save :: init_bias_oz   = .false.

#include "assert.H"
#include "mytrace.H"
contains
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: nmlread_ - Input NAMELIST/gsiGuess_opts/
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine nmlread_(lu)
      use m_mpout,only : mpout_log
      use m_die,only : die
      implicit none
      integer,intent(in) :: lu	! input unit with the NAMELIST in.

! !REVISION HISTORY:
!       27Jan06 - Jing Guo <guo@gmao.gsfc.nasa.gov>
!               - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::nmlread_'

  integer :: ier
  real,dimension(200) :: ak,bk
  real :: ak_unit

  namelist/gsiGuess_opts/ hGrid_type, &
	vGrid_type,vGrid_size,ak_unit,ak,bk,aincout, &
	init_bias_lnps,init_bias_ts,init_bias_tv,    &
	init_bias_wind,init_bias_cwmr,init_bias_q,   &
	init_bias_oz,phisfile,aincfile

  if(initialized_) call die(myname_,	&
  	'namelist already initialized, /gsiGuess_opts/')
  initialized_=.true.

	! set default values
  hGrid_type=hGrid_AUTO
  vGrid_type=vGrid_AUTO
  vGrid_size=-1
  ak_unit=100.	! default to hPa
  ak(1:0)=0./0.	! as Not-A-Number (NaN)
  bk(1:0)=0./0.	! NaN

  phisfile = '.none.'
  aincout=.false.
  aincfile = '.none.'	! file of a-grid analysis increments

  init_bias_lnps = .false.
  init_bias_ts   = .false.
  init_bias_tv   = .false.
  init_bias_wind = .false.	! including u, v, div, and vor.
  init_bias_cwmr = .false.
  init_bias_q    = .false.
  init_bias_oz   = .false.

	! update from the namalist input
  read(lu,gsiGuess_opts,iostat=ier)
    if(ier/=0) then
      call mpout_log(myname_,'use default /gsiGuess_opts/')
      hGrid_type=hGrid_AUTO
      vGrid_type=vGrid_AUTO
      vGrid_size=-1
      vGrid_unit=100.	! default to hPa
      return
    endif
    call mpout_log(myname_,'use user defined /gsiGuess_opts/')

	! store the values.
  vGrid_unit=ak_unit
  allocate(vGrid_ak(vGrid_size+1),vGrid_bk(vGrid_size+1))
  vGrid_ak(1:vGrid_size+1)=ak(1:vGrid_size+1)
  vGrid_bk(1:vGrid_size+1)=bk(1:vGrid_size+1)

end subroutine nmlread_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: nmlshow_ - show /gsiGuess_opts/ values
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine nmlshow_()
      use m_mpout,only : mpout,mpout_ison
      implicit none

! !REVISION HISTORY:
!       27Jan06 - Jing Guo <guo@gmao.gsfc.nasa.gov>
!               - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::nmlshow_'

  if(mpout_ison()) then
    write(mpout,'(a,2x,i5)') 'hGrid_type =',hGrid_type
    write(mpout,'(a,2x,i5)') 'vGrid_type =',vGrid_type
    write(mpout,'(a,2x,i5)') 'vGrid_size =',vGrid_size
    write(mpout,'(a,2x,f7.2)') 'vGrid_unit =',vGrid_unit
  endif
  call showakbk_(vGrid_ak ,vGrid_bk ,myname_,'GSI',unit=vGrid_unit)
end subroutine nmlshow_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: gridsetup_ - setup GSI grid sizes and references
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine gridsetup_(fvname,iyr,imo,idy,ihr,freq,comm,root)
      use gridmod,only : gmao_intfc
      use gridmod,only : ak5,bk5
      use gridmod,only : nlon,rlats,nlat,nsig
      use m_fvGrid,only : fvGrid,fvGrid_get,clean
      use m_fvGridHeader,only : fvGridHeader_allread
      use m_interleavedObject,only : interleavedObject
      use m_die,only : assert_,die,perr
      use m_mpout,only : mpout_log
      implicit none
      character(len=*),intent(in) :: fvname
      integer,intent(out) :: iyr,imo,idy,ihr	! for yyyy,mm,dd,hh
      integer,intent(out) :: freq	! time interval in hhmmss
      integer,intent(in) :: comm
      integer,intent(in) :: root

! !REVISION HISTORY:
! 	07Apr05	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::gridsetup_'
  type(fvGrid) :: h_fv
  type(interleavedObject) :: etaLev1,etaLevs
  integer :: nlon_,nlat_,nsig_,ier
!________________________________________
_ALLENTRY_
  ALWAYS_ASSERT(gmao_intfc)

  call fvGridHeader_allread(fvname,h_fv,comm,ROOT,	&
	gridverify=GRIDVERIFY_)

! Define grid references.  Background state grid dimensions (from file
! fvname) are compared to the pre-defined analysis-increment grid
! dimensions (namelist /gridopt/ in file gsi.rc), to determine the type
! of the analysis-increment grid.  On the other hand, the data-time
! data from file fvname are returned as arguments.

! For the horizontal grid, the type of latitude grid is either Gaussian
! or equal-interval.

	ASSERT(mod(nlon,8)==0)	! as required by the GSI polar cascate.

  call fvGrid_get(h_fv,im=nlon_,jm=nlat_)
  select case(hGrid_type)
  case(hGrid_AUTO)
    LLGrid_ = nlon==nlon_ .and. nlat==nlat_
  case(hGrid_EqualSpacing)
    LLGrid_ = .true.
  case(hGrid_Gaussian)
    LLGrid_ = .false.
  case default
    call die(myname_,'unknown hGrid_type (0,1,2)',hGrid_type)
  end select

  if(LLGrid_) then	! defined on equal-interval grid
    call genLLgrid_vars_()
  else			! defined on Gaussian grid
    call gengrid_vars()
  endif

! For the vertical grid, the type is either user-speicified (yet to be
! implemented) or the same as the background state.

  call fvGrid_get(h_fv,km=nsig_)
  select case(vGrid_type)
  case(vGrid_Auto)
    fvGrid_ = nsig==nsig_
  case(vGrid_UserDefined)
    fvGrid_ = .false.
  end select

  if(.not.fvGrid_) then		! defined from a user specified table

	if(.not.initialized_) call die(myname_,	&
		'namelist not initialized, /gsiGuess_opts/')
	if(nsig/=vGrid_size) then
	  call perr(myname_,'/gridopts/nsig=',nsig)
	  call perr(myname_,'/gsiGuess_opts/vGrid_size=',vGrid_size)
	  ASSERT(nsig==vGrid_size)
	endif

    ak5(1:nsig+1)=vGrid_ak(1:vGrid_size+1)*vGrid_unit	! in Pa
    bk5(1:nsig+1)=vGrid_bk(1:vGrid_size+1)		! in 1.
    
  else				! defined from background state
    call fvGrid_get(h_fv,ak=ak5,bk=bk5)
  endif

	! If the surface is not the first in the array, reorder (ak,bk)
	! according to GSI.  Note that at the output of fvGrid_get(),
	! ak is expected in Pa, while GSI expects ak (and ps) in kPa.

  if(	ak5(      1)+bk5(      1)*100000. <	&
	ak5(nsig_+1)+bk5(nsig_+1)*100000.	) then

    ak5(1:nsig+1:+1)=ak5(nsig+1:1:-1)
    bk5(1:nsig+1:+1)=bk5(nsig+1:1:-1)
  endif
  ak5(:)=ak5(:)*kPa_per_Pa		! from Pa to kPa

  call fvGrid_get(h_fv,year=iyr,month=imo,day=idy,hour=ihr,freq=freq)
	call clean(h_fv)
_ALLEXIT_
contains
subroutine genLLgrid_vars_()
  use gridmod  , only: nlon,rlons,sinlon,coslon
  use gridmod  , only: nlat,rlats,wgtlats,rbs2
  use constants, only: zero,half,one,pi
  implicit none

! Implemented based on GSI gengrid_vars() for the global spectral grid
! configuration.  The original document is kept below between two lines
! marked by "!$$$", for where credit is due.  However, the code has been
! modified by this developer, thus responsible for any mistake for the
! modification and its application.

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gengrid_vars
!   prgmmr: treadon          org: np23                date: 2003-11-24 
!
! abstract: initialize and define grid related variables
!
! program history log:
!   2003-11-24  treadon
!   2004-05-13  kleist, documentation and cleanup
!   2004-08-04  treadon - add only on use declarations; add intent in/out
!   2006-04-12  stassi - set wgtlats to TINY instead of zero (see comment)
!
!   input argument list:
!
!   output argument list:
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

! Declare local variables
  integer(kind(nlon)) :: i,j
  real   (kind(half)) :: dlon,dlat,pih

! Set grid longitude array.
    dlon=(pi+pi)/nlon	! in radiance
    do i=1,nlon			! from 0 to 2pi
      rlons (i)=(i-1)*dlon
      coslon(i)=cos(rlons(i))
      sinlon(i)=sin(rlons(i))
    end do

! Set grid latitude array.
    pih =half*pi
    dlat=pi/(nlat-1)
    do j=1,nlat			! from -pi/2 to +pi/2
      rlats(j)=(j-1)*dlat - pih
    end do

! Wgtlats might has been used by spectral code. The values are used as
! divisor in the compact_diffs::inisph() routine to a part of a table,
! which is not appeared to be used everywhere in the GSI.  To avoid
! invalid arithmatic computations, it is set to TINY instead of ZERO.
    wgtlats(:)=TINY(wgtlats)

! rbs2=1/cos^2(rlats)) is used in pcp.  polar points are set to zeroes.
    rbs2(1       )=zero
    rbs2(2:nlat-1)=cos(rlats(2:nlat-1))
    rbs2(2:nlat-1)=one/(rbs2(2:nlat-1)*rbs2(2:nlat-1))
    rbs2(  nlat  )=zero

end subroutine genLLgrid_vars_
end subroutine gridsetup_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: intp_ - interpolate from fvGrid to the GSI _subdomains_.
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine intp_(w_fv,		&	! fv-state, input
    	h_fv,etaLev1,etaLevs,		&	! grid of w_fv, input
    	ncep_phis,			&	! NCEP topography of GSI
	zsfc,lnps,uwnd,vwnd,tv,q,cwmr,oz,sfct,	&
	comm,vor,div)			! topography of w_gsi

      use m_fvGriddedState,only : fvGriddedState
      use m_fvGriddedState,only : ptr_ps
      use m_fvGriddedState,only : ptr_phis
      use m_fvGriddedState,only : ptr_ts

      use m_fvGriddedState,only : ptr_uw
      use m_fvGriddedState,only : ptr_vw
      use m_fvGriddedState,only : ptr_pt
      use m_fvGriddedState,only : ptr_cw
      use m_fvGriddedState,only : ptr_q
      use m_fvGriddedState,only : ptr_oz

      use m_fvGrid,only : fvGrid
      use m_fvGrid,only : fvGrid_get
      use m_interleavedObject,only : interleavedObject

      use m_fgInterp,only : MASS, THTA
      use m_fgInterp,only : VIRTUAL_TEMPERATURE
      use m_fgInterp,only : POTENTIAL_TEMPERATURE

      use m_fgInterp,only : fgInterp
      use m_fgInterp,only : fgInterp_l2ginit	! to Gaussian grid
      use m_fgInterp,only : fgInterp_l2linit	! to eq.int. latitudes
      use m_fgInterp,only : psInterp_ftog
      use m_fgInterp,only : fgInterp_ftog
      use m_fgInterp,only : fgInterp_dtog
      use m_fgInterp,only : clean

      use m_ggGradient,only : ggGradient
      use m_ggGradient,only : ggGradient_init,clean
      use m_ggGradient,only : ggDivo,ggGrad

      use m_ppInterp,only : ppInterp
      use m_ppInterp,only : clean

      use jfunc,	only: jiterstart

      use guess_grids,	only: ntguessig

      use gridmod,	only: istart,ilat1
      use gridmod,	only: jstart,jlon1
      use gridmod,	only: lon1, lat1
      use gridmod,      only: lon2, lat2
      use gridmod,	only: nsig, ak5, bk5
      use gridmod,	only: nlon, nlat, rlats

      use m_geosapi,only : getcon
      use m_mpif90,only : MP_comm_rank
      use m_mpif90,only : MP_comm_size
      use m_mpout ,only : mpout,mpout_log,mpout_ison
      use m_die,only : assert_,MP_die,die
      implicit none

      type(fvGriddedState),intent(in) :: w_fv

	type(fvGrid),intent(in) :: h_fv	! header of fv, grid defs.
	type(interleavedObject),intent(in) :: etaLev1	! of 2-d fields
	type(interleavedObject),intent(in) :: etaLevs	! of 3-d fields

      character(len=*),intent(in) :: ncep_phis	! source of NCEP_phis

      real(gsiREAL),dimension(:,:),intent(out) :: zsfc	! topography
      real(gsiREAL),dimension(:,:),intent(out) :: lnps	! log(ps) in kPa

      real(gsiREAL),dimension(:,:,:),intent(out) :: uwnd ! u-wind
      real(gsiREAL),dimension(:,:,:),intent(out) :: vwnd ! v-wind
      real(gsiREAL),dimension(:,:,:),intent(out) :: tv   ! virt. temp.
      real(gsiREAL),dimension(:,:,:),intent(out) :: q    ! virt. temp.
      real(gsiREAL),dimension(:,:,:),intent(out) :: cwmr ! cld.wat.m.r.
      real(gsiREAL),dimension(:,:,:),intent(out) :: oz   ! ozone

      real(gsiREAL),dimension(:,:),intent(out) :: sfct	  ! t_skin

      integer,intent(in) :: comm	! MPI communicator

      real(gsiREAL),optional,dimension(:,:,:),intent(out) :: vor
      real(gsiREAL),optional,dimension(:,:,:),intent(out) :: div

! !REVISION HISTORY:
! 	19Nov04	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::intp_'
  type(fgInterp) :: fgintp
  type(ppInterp) :: ppintp
  type(ggGradient) :: gr
  integer :: iAdim,jAdim
  integer :: iGdim,iGloc,iGlen
  integer :: jGdim,jGloc,jGlen
  integer :: kmfv
  integer :: ier,myPE,nPEs
  integer :: kb,ke,k
  logical :: divo_
  real(gsiREAL),allocatable,dimension(:) :: akfv,bkfv
  real(gsiREAL) :: GRAV
_ALLENTRY_
!________________________________________

  	call MP_comm_rank(comm,myPE,ier)
		if(ier/=0) call MP_die(myname_,'MP_comm_rank()',ier)
  	call MP_comm_size(comm,nPEs,ier)
		if(ier/=0) call MP_die(myname_,'MP_comm_size()',ier)

	! In this module, prefix i- is normally for longitude, and j-
	! is normally for latitude.  For an array v defined with
	! longitude as the faster running index than latitude, one may
	! see loop structures like this,
	!
	!   do j=1,..	! for all latitude grid points
	!   do i=1,..	! for all longitude grid points
	!	v(i,j,...) = ...
	!
	! This prefix rule is different from what is used in GSI for
	! variables in subdomains, where the prefix i- is for latitudes
	! and j- is for longitudes, such as in istart, ilat1, jstart,
	! and jlon1.  Also, a gsi field variable w is often defined as
	! an array with latitude as the faster running index than the
	! longitude running index.  Therefore, the final loop structure
	! may look identical, but with a total different meaning.
	!
	!   do j=1,..	! for all longitude grid points
	!   do i=1,..	! for all latitude grid points
	!	w(i,j,...) = ...
	!
	! Note that the definitions of prefixes i- and j- are swapped.

  call fvGrid_get(h_fv,im=iAdim,jm=jAdim,km=kmfv)

    call mpout_log(myname_,'iAdim (fv:nlon) =',iAdim)
    call mpout_log(myname_,'jAdim (fv:nlat) =',jAdim)

  iGdim=nlon
  jGdim=nlat
  iGloc=jstart(myPE+1)
  iGlen=jlon1 (myPE+1)
  jGloc=istart(myPE+1)
  jGlen=ilat1 (myPE+1)

    call showblocks_(jstart(1:nPEs),istart(1:nPEs),	&
		      jlon1(1:nPEs), ilat1(1:nPEs),	&
		      myname_)

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
!________________________________________
! Define the fgInterp operator by starting from defining its all
! communication operators between intermediate distributed grids.

  if(LLGrid_) then
	! configure for an equal-interval latitude grid
    call fgInterp_l2linit(fgintp,ncep_phis,		&
  	iAdim,jAdim,etaLev1,etaLevs,		&
	iGdim,jGdim,iGloc,iGlen,jGloc,jGlen, nsig ,comm)
  else
	! configure for a Gaussian grid
    call fgInterp_l2ginit(fgintp,ncep_phis,		&
  	iAdim,jAdim,etaLev1,etaLevs,		&
	iGdim,jGdim,iGloc,iGlen,jGloc,jGlen, nsig ,comm)
  endif
!________________________________________

  	allocate(akfv(kmfv+1),bkfv(kmfv+1))
  call fvGrid_get(h_fv,ak=akfv,bk=bkfv)
	akfv(:)=akfv(:)*kPa_per_Pa	! convert to kPa

	! show (ak,bk) in hPa
  call showakbk_(hPa_per_kPa*ak5 ,bk5 ,myname_,'GSI')
  call showakbk_(hPa_per_kPa*akfv,bkfv,myname_,' FV')
!________________________________________
! Interpolate the surface pressure, which is the basis for the
! definition of the target vertical GSI analysis grid.

	! ptr_ps(w_fv) is not converted before interpolation.  It is
	! already in Pa.  However, ak must be converted.
  call psInterp_ftog(fgintp,	ptr_ps(w_fv),			&
				ptr_phis(w_fv),			&
  				akfv(1:kmfv+1)*Pa_per_kPa,	&
				bkfv(1:kmfv+1),			&
				ptr_pt(w_fv),			&
		     ppintp,	lnps(:,:),			&
		     		zsfc(:,:),			&
		     		ak5(1:nsig+1)*Pa_per_kPa,	&
				bk5(1:nsig+1),			&
		     		comm)

	! Convert interpolated ps in Pa (~100,000) to ps in kPa (or
	! centi-bar) (~100) then to log(ps).

  lnps(:,:)=log(lnps(:,:)*kPa_per_Pa)

	deallocate(akfv,bkfv)

! Topography (in meters) is required to determine the surface pressure,
! a by-product of ps-interpolation.
!________________________________________

	! Convert interpolated zs in m^2/s^2 to m.
  	GRAV=getcon("GRAVITY")
  zsfc(:,:)=zsfc(:,:)/GRAV
!________________________________________
! 2-d variable: surface temperature

!*** the exact location of allocating sfct(:,:,:) must be verified ***

  call fgInterp_ftog(fgintp,ptr_ts  (w_fv), sfct(:,:), MASS,comm)
  						! SST/Tskin, K.
!________________________________________
! upper-air winds

	ASSERT(lat2==size(uwnd,1))
	ASSERT(lon2==size(uwnd,2))
	ASSERT(nsig==size(uwnd,3))

	ASSERT(lat2==size(vwnd,1))
	ASSERT(lon2==size(vwnd,2))
	ASSERT(nsig==size(vwnd,3))

  call fgInterp_dtog(fgintp,ppintp,	&
  	ptr_uw(w_fv),			&
	ptr_vw(w_fv),			&
	uwnd(:,:,1:nsig),	&	! u-wind, m/s
	vwnd(:,:,1:nsig),	&	! v-wind, m/s
	comm				)

! Derive grid level divorgence and vorticity if needed.

  divo_=.false.
  if(present(vor).and.present(div)) divo_=.true.
  if(divo_) then

	ASSERT(lat2==size(div,1))
	ASSERT(lon2==size(div,2))
	ASSERT(nsig==size(div,3))

	ASSERT(lat2==size(vor,1))
	ASSERT(lon2==size(vor,2))
	ASSERT(nsig==size(vor,3))

	ASSERT(jGdim==size(rlats))

    call ggGradient_init(gr,iGdim,rlats(1:nlat),	&
  	iGloc,iGlen,jGloc,jGlen,nsig, comm)

    call ggDivo(gr,			&
		uwnd(:,:,:),	&	! in: u
		vwnd(:,:,:),	&	! in: v
		div (:,:,:),	&	! div(u,v)
		vor (:,:,:),	&	! vor(u,v)
		comm)					! communicator

    call clean(gr)

  else
    if(present(vor).or.present(div))	&
	call die(myname_,'require both vor and div arguments')
  endif
!________________________________________
! cw- cloud water mixing ratio

  call fgInterp_ftog(fgintp,ppintp,	&
  	ptr_cw(w_fv),			&
	cwmr(:,:,:),			&	! c.w. mixing ratio, g/g
	MASS,comm)
!________________________________________
! q- specific humidity

  call fgInterp_ftog(fgintp,ppintp,	&
  	ptr_q(w_fv),			&
  	q(:,:,:),			&	! q, spec. humid., g/g
	MASS,comm)
!________________________________________
! oz- Ozone

  call fgInterp_ftog(fgintp,ppintp,	&
  	GG_per_PPMV*ptr_oz(w_fv),	& ! fv-ozone, in ppmv
  	oz(:,:,:),			& ! convert to gsi-ozone, in g/g
	MASS,comm)
!________________________________________
! thv- virtual potential temperature, to virtual temperature

  call fgInterp_ftog(fgintp,ppintp,	&
  	ptr_pt(w_fv),			&
  	tv(:,:,:),		&	! tv, K
	THTA,comm, TgtTemp=VIRTUAL_TEMPERATURE)
!________________________________________

  call clean(ppintp)
  call clean(fgintp)
_ALLEXIT_
end subroutine intp_
subroutine showblocks_(jstart,istart,jlon1,ilat1,where)
  use m_SortingTools,only : indexSet,indexSort
  use m_mpout,only : mpout_ison,mpout
  implicit none
  integer,dimension(:),intent(in) :: jstart,istart,jlon1,ilat1
  character(len=*),intent(in) :: where

  integer,dimension(size(jstart)) :: indx
  integer,parameter :: INC=10
  integer :: i,ii,n,m

  if(.not.mpout_ison()) return

  n=size(jstart)
		! a two key sorting through a stable algorithm
  call indexSet(indx)
  call indexSort(indx,jstart(1:n))
  call indexSort(indx,istart(1:n))

		! list the sorted processor ranks
  write(mpout,'(4a)') where,':: -- processors ranked by lat-lon --'
  write(mpout,'(3x,12i6)') (i,i=0,INC-1)
  do i=1,n,INC
    m=min(n,i+INC-1)
    write(mpout,'(i4,12i6)') i,(indx(ii)-1,ii=i,m)
  end do
  write(mpout,'(a)') '::'

		! list the sorted block set
  call showlist_(jstart(indx(1:n)),where,'jstart(:) ranked by lat-lon')
  call showlist_( jlon1(indx(1:n)),where,' jlon1(:) ranked by lat-lon')
  call showlist_(istart(indx(1:n)),where,'istart(:) ranked by lat-lon')
  call showlist_( ilat1(indx(1:n)),where,' ilat1(:) ranked by lat-lon')
end subroutine showblocks_
subroutine showlist_(list,where,tag)
  use m_mpout,only : mpout_ison,mpout
  implicit none
  integer,dimension(0:),intent(in) :: list
  character(len=*),intent(in) :: where,tag

  integer,parameter :: INC=10
  integer :: i,ii,n,m
  n=size(list)
  if(mpout_ison()) then
    write(mpout,'(4a)') where,':: -- ',trim(tag),' --'
    write(mpout,'(3x,12i6)') (i,i=0,INC-1)
    do i=0,n-1,INC
      m=min(n-1,i+INC-1)
      write(mpout,'(i4,12i6)') i,(list(ii),ii=i,m)
    end do
    write(mpout,'(a)') '::'
  endif
end subroutine showlist_

subroutine showakbk_(ak,bk,where,tag,unit)
  use m_mpout, only: mpout_ison	! control
  use m_mpout, only: mpout	! logical unit
  use m_mpout, only: mpout_log	! message
  use m_die,only : assert_
  implicit none
  real(gsiREAL),dimension(:),intent(in) :: ak,bk
  character(len=*),intent(in) :: where,tag
  real(gsiREAL),optional,intent(in) :: unit

  integer :: nsig,k,kb,ke
  real :: u
  u=100.
  if(present(unit)) u=unit
  u=u/100.

  nsig=size(ak)-1
  	ASSERT(nsig==size(bk)-1)

  call mpout_log(where,tag//':nsig =',nsig,flush=.true.)
  if(.not.mpout_ison()) return

  write(mpout,'(2a)') where,':: -- nsig+1 (a,1000*b) levels in hPa--'
  do kb=1,nsig+1,4
    ke=min(kb+3,nsig+1)
	! expecting in hPa ...
    write(mpout,'(f9.4,7(f10.4))') (u*ak(k),1000.*bk(k),k=kb,ke)
  end do
end subroutine showakbk_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: fvSurfintp_ - interpolate from fv surface to GSI grid
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine fvSurfintp_(w_fv, ws10,tskn,	&
    	snow_dep,soil_temp,soil_mois,isli_mask,isli_glbl, comm)

      use jfunc,	only: jiterstart
      use guess_grids,	only: ntguessig

      use gridmod,	only: istart,ilat1
      use gridmod,	only: jstart,jlon1
      use gridmod,	only: lon1, lat1
      use gridmod,      only: lon2, lat2
      use gridmod,	only: nsig, ak5, bk5
      use gridmod,	only: nlon, nlat, rlats

      use m_fvSurface,only : fvSurface
      use m_fvSurface,only : get
      use m_fvSurface,only : clean
      use m_fvSurface,only : ptr_u10m,ptr_v10m
      use m_fvSurface,only : ptr_tskn
      use m_fvSurface,only : ptr_snow_dep
      use m_fvSurface,only : ptr_soil_temp
      use m_fvSurface,only : ptr_soil_mois
      use m_fvSurface,only : ptr_isli_mask

      use m_interleavedObject,only : InterleavedObject
      use m_interleavedObject,only : clean

      use m_fgInterp,only : MASS

      use m_fgInterp,only : fgInterp
      use m_fgInterp,only : fgInterp_surface_l2ginit
      use m_fgInterp,only : fgInterp_surface_l2linit
      use m_fgInterp,only : fgInterp_lh2rh
      use m_fgInterp,only : clean
      use m_fgInterp,only : populate

      use m_undef  ,only : undef_ssi !!TORM ,like,unlike
      use m_geosapi,only : getcon
      use m_mpif90,only : MP_comm_rank
      use m_mpout ,only : mpout,mpout_log,mpout_ison
      use m_die,only : assert_,MP_die,die
      implicit none

      type(fvSurface),intent(in) :: w_fv

      real(gsiREAL),dimension(:,:),intent(out) :: ws10 	! 10m wind speed
      real(gsiREAL),dimension(:,:),intent(out) :: tskn	! t_skin
      real(gsiREAL),dimension(:,:),intent(out) :: snow_dep
      real(gsiREAL),dimension(:,:),intent(out) :: soil_temp
      real(gsiREAL),dimension(:,:),intent(out) :: soil_mois
      integer      ,dimension(:,:),intent(out) :: isli_mask
      integer      ,dimension(:,:),intent(out) :: isli_glbl

      integer,intent(in) :: comm	! MPI communicator

! !REVISION HISTORY:
! 	19Nov04	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::fvSurfintp_'
  type(fgInterp) :: fgintp
  type(interleavedObject) :: scatSfc
  integer :: iAdim,jAdim
  integer :: iGdim,iGloc,iGlen
  integer :: jGdim,jGloc,jGlen
  integer :: ier,myPE,n
  real(gsiREAL),allocatable,dimension(:,:) :: bufr
_ALLENTRY_
!________________________________________

  	call MP_comm_rank(comm,myPE,ier)
		if(ier/=0) call MP_die(myname_,'MP_comm_rank()',ier)

  call get(w_fv,idim=iAdim,jdim=jAdim)

    call mpout_log(myname_,'iAdim (fv:nlon) =',iAdim)
    call mpout_log(myname_,'jAdim (fv:nlat) =',jAdim)

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
!________________________________________
! Define the fgInterp operator by starting from defining its all
! communication operators between intermediate distributed grids.

	call get(w_fv,scat=scatSfc)

  if(LLGrid_) then
    call fgInterp_surface_l2linit(fgintp,iAdim,jAdim,scatSfc,	&
  	iGdim,jGdim,iGloc,iGlen,jGloc,jGlen, comm)
  else
    call fgInterp_surface_l2ginit(fgintp,iAdim,jAdim,scatSfc,	&
  	iGdim,jGdim,iGloc,iGlen,jGloc,jGlen, comm)
  endif

	call clean(scatSfc)
!________________________________________
	! Wind speed at 10m.
	! ws10 is used as a temporary storage for u10m.
	! Tskn is used as a temporary storage for v10m.

  call fgInterp_lh2rh(fgintp,ptr_u10m(w_fv),ptr_v10m(w_fv),&
  				 ws10,          tskn, comm)

  ws10(:,:)=sqrt(ws10(:,:)*ws10(:,:)+tskn(:,:)*tskn(:,:))

  call fgInterp_lh2rh(fgintp,ptr_snow_dep (w_fv),snow_dep ,MASS,comm)
  call fgInterp_lh2rh(fgintp,ptr_soil_temp(w_fv),soil_temp,MASS,comm)
  call fgInterp_lh2rh(fgintp,ptr_soil_mois(w_fv),soil_mois,MASS,comm)

	! Tskn is used as a temporary storage for isli_mask.

  call fgInterp_lh2rh(fgintp,ptr_isli_mask(w_fv),tskn     ,MASS,comm)

    isli_mask(:,:) = nint(tskn(:,:))
    isli_mask(:,:) = min(max(0,isli_mask(:,:)),2)

  	ASSERT(iGdim==size(isli_glbl,2))
  	ASSERT(jGdim==size(isli_glbl,1))

    	allocate(bufr(jGdim,iGdim))	! note the swapped dimensions
    call populate(fgintp,tskn,bufr,comm)

    isli_glbl(:,:) = nint(bufr(:,:))
    isli_glbl(:,:) = min(max(0,isli_glbl(:,:)),2)

    	deallocate(bufr)

	! Tskn is now used as itself.

  call fgInterp_lh2rh(fgintp,ptr_tskn     (w_fv),tskn     ,MASS,comm)
	call clean(fgIntp)

#ifdef _TORM_
  n=size(isli_mask)
if(n/=0) then
  print'(2a,i6)',myname_,': size(isli_mask) = ',n
  print'(2a,i6)',myname_,				&
    ': count( sea (isli_mask==0)) = ',count(isli_mask==0)
  print'(2a,i6)',myname_,				&
    ': count(land (isli_mask==1)) = ',count(isli_mask==1)
  print'(2a,i6)',myname_,				&
    ': count( ice (isli_mask==2)) = ',count(isli_mask==2)

  print'(2a,2i6)',myname_,				&
    ':  sea (0) points with/without soil_temp ',	&
    count(isli_mask==0 .and. unlike(soil_temp,undef_ssi())),	&
    count(isli_mask==0 .and.   like(soil_temp,undef_ssi()))
  print'(2a,2i6)',myname_,				&
    ': land (1) points with/without soil_temp ',	&
    count(isli_mask==1 .and. unlike(soil_temp,undef_ssi())),	&
    count(isli_mask==1 .and.   like(soil_temp,undef_ssi()))
  print'(2a,2i6)',myname_,				&
    ':  ice (2) points with/without soil_temp ',	&
    count(isli_mask==2 .and. unlike(soil_temp,undef_ssi())),	&
    count(isli_mask==2 .and.   like(soil_temp,undef_ssi()))

  print'(2a,2i6)',myname_,				&
    ':  sea (0) points with/without snow_dep ',	&
    count(isli_mask==0 .and. unlike(snow_dep,undef_ssi())),	&
    count(isli_mask==0 .and.   like(snow_dep,undef_ssi()))
  print'(2a,2i6)',myname_,				&
    ': land (1) points with/without snow_dep ',	&
    count(isli_mask==1 .and. unlike(snow_dep,undef_ssi())),	&
    count(isli_mask==1 .and.   like(snow_dep,undef_ssi()))
  print'(2a,2i6)',myname_,				&
    ':  ice (2) points with/without snow_dep ',	&
    count(isli_mask==2 .and. unlike(snow_dep,undef_ssi())),	&
    count(isli_mask==2 .and.   like(snow_dep,undef_ssi()))
endif
#endif
_ALLEXIT_
end subroutine fvSurfintp_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: ncSurfintp_ - interpolate from fv surface to GSI grid
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine ncSurfintp_(w_nc, veg_frac,veg_type,soil_type, comm)

      use jfunc,	only: jiterstart
      use guess_grids,	only: ntguessig
      use gridmod,	only: istart,ilat1
      use gridmod,	only: jstart,jlon1
      use gridmod,	only: lon1, lat1
      use gridmod,      only: lon2, lat2
      use gridmod,	only: nlon, nlat, rlats

      use m_ncSurface,only : ncSurface
      use m_ncSurface,only : get
      use m_ncSurface,only : ptr_veg_frac
      use m_ncSurface,only : ptr_veg_type
      use m_ncSurface,only : ptr_soil_type

      use m_fgInterp,only : MASS
      use m_fgInterp,only : fgInterp
      use m_fgInterp,only : fgInterp_surface_g2ginit
      use m_fgInterp,only : fgInterp_surface_g2linit
      use m_fgInterp,only : fgInterp_lh2rh
      use m_fgInterp,only : clean

      use m_InterleavedObject,only : InterleavedObject
      use m_InterleavedObject,only : clean

      use m_geosapi,only : getcon
      use m_mpif90,only : MP_comm_rank
      use m_mpout ,only : mpout,mpout_log,mpout_ison
      use m_die,only : assert_,MP_die,die
      implicit none

      type(ncSurface),intent(in) :: w_nc

      real(gsiREAL),dimension(:,:),intent(out) :: veg_frac
      real(gsiREAL),dimension(:,:),intent(out) :: veg_type
      real(gsiREAL),dimension(:,:),intent(out) :: soil_type

      integer,intent(in) :: comm	! MPI communicator

! !REVISION HISTORY:
! 	19Nov04	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::ncSurfintp_'
  type(fgInterp) :: fgintp
  type(InterleavedObject) :: scatSfc
  integer :: iAdim,jAdim
  integer :: iGdim,iGloc,iGlen
  integer :: jGdim,jGloc,jGlen
  integer :: ier,myPE
_ALLENTRY_
!________________________________________

  	call MP_comm_rank(comm,myPE,ier)
		if(ier/=0) call MP_die(myname_,'MP_comm_rank()',ier)

  call get(w_nc,idim=iAdim,jdim=jAdim)

    call mpout_log(myname_,'iAdim (nc:nlon) =',iAdim)
    call mpout_log(myname_,'jAdim (nc:nlat) =',jAdim)

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
!________________________________________
! Define the fgInterp operator by starting from defining its all
! communication operators between intermediate distributed grids.

  call get(w_nc,scat=scatSfc)
  if(LLGrid_) then
    call fgInterp_surface_g2linit(fgintp,iAdim,jAdim,scatSfc,	&
  	iGdim,jGdim,iGloc,iGlen,jGloc,jGlen, comm)
	call clean(scatSfc)
  else
    call fgInterp_surface_g2ginit(fgintp,iAdim,jAdim,scatSfc,	&
  	iGdim,jGdim,iGloc,iGlen,jGloc,jGlen, comm)
	call clean(scatSfc)
  endif

  call fgInterp_lh2rh(fgintp,ptr_veg_frac (w_nc), veg_frac,MASS,comm)
  call fgInterp_lh2rh(fgintp,ptr_veg_type (w_nc), veg_type,MASS,comm)
  call fgInterp_lh2rh(fgintp,ptr_soil_type(w_nc),soil_type,MASS,comm)

	call clean(fgintp)
_ALLEXIT_
end subroutine ncSurfintp_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: unintp_ - interpolate from fvGrid to the GSI _subdomains_.
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine unintp_(w_fv,		&	! fv-state, output
    	w_ref,h_fv, etaLev1,etaLevs,	&	! ref. w_fv and grid
	ncep_phis,			&	! topography of w_gsi
	zsfc,lnps,uwnd,vwnd,tv,q,cwmr,oz,sfct, comm)	! gsi fields

      use m_fvGriddedState,only : fvGriddedState
      use m_fvGriddedState,only : fvGriddedState_init
      use m_fvGriddedState,only : fvGriddedState_asis
      use m_fvGriddedState,only : ptr_ps
      use m_fvGriddedState,only : ptr_phis
      use m_fvGriddedState,only : ptr_ts

      use m_fvGriddedState,only : ptr_delp
      use m_fvGriddedState,only : ptr_uw
      use m_fvGriddedState,only : ptr_vw
      use m_fvGriddedState,only : ptr_pt
      use m_fvGriddedState,only : ptr_cw
      use m_fvGriddedState,only : ptr_q
      use m_fvGriddedState,only : ptr_oz

      use m_fvGrid,only : fvGrid
      use m_fvGrid,only : fvGrid_get
      use m_interleavedObject,only : interleavedObject

      use m_fgInterp,only : MASS, THTA
      use m_fgInterp,only : VIRTUAL_TEMPERATURE
      use m_fgInterp,only : POTENTIAL_TEMPERATURE

      use m_fgInterp,only : fgInterp
      use m_fgInterp,only : fgInterp_l2ginit	! to a Gaussian grid
      use m_fgInterp,only : fgInterp_l2linit	! to eq.int. latitude
      use m_fgInterp,only : psInterp_gtof
      use m_fgInterp,only : fgInterp_gtof
      use m_fgInterp,only : fgInterp_gtod
      use m_fgInterp,only : clean

      use m_ppInterp,only : ppInterp
      use m_ppInterp,only : clean
      use m_checksums,only : checksums_show

      use jfunc,	only: jiterstart

      use guess_grids,	only: ntguessig

      use gridmod,	only: istart,ilat1
      use gridmod,	only: jstart,jlon1
      use gridmod,	only: lon1, lat1
      use gridmod,	only: lon2, lat2
      use gridmod,	only: nsig, ak5, bk5
      use gridmod,	only: nlon, nlat

      use m_mpif90,only : MP_comm_rank
      use m_mpout ,only : mpout,mpout_log,mpout_ison
      use m_die,only : die,MP_die,assert_
      implicit none

      type(fvGriddedState),intent(out) :: w_fv	! the state

		! Domain, grid, and distribution specifications of
		! fvGriddedState

        type(fvGriddedState),intent(in) :: w_ref ! a reference state
	type(fvGrid),intent(in) :: h_fv		! header with grid defs.
	type(interleavedObject),intent(in) :: etaLev1	! 2-d distr.
	type(interleavedObject),intent(in) :: etaLevs	! 3-d distr.

      character(len=*),intent(in) :: ncep_phis	! source of NCEP_phis

      real(gsiREAL),dimension(:,:),intent(in) :: zsfc	! topography
      real(gsiREAL),dimension(:,:),intent(in) :: lnps	! log(ps) in kPa

      real(gsiREAL),dimension(:,:,:),intent(in) :: uwnd ! u-wind
      real(gsiREAL),dimension(:,:,:),intent(in) :: vwnd ! v-wind
      real(gsiREAL),dimension(:,:,:),intent(in) :: tv   ! virt. temp.
      real(gsiREAL),dimension(:,:,:),intent(in) :: q    ! virt. temp.
      real(gsiREAL),dimension(:,:,:),intent(in) :: cwmr ! cld.wat.m.r.
      real(gsiREAL),dimension(:,:,:),intent(in) :: oz   ! ozone

      real(gsiREAL),dimension(:,:),intent(in) :: sfct	  ! t_skin

      integer,intent(in) :: comm	! MPI communicator

! !REVISION HISTORY:
! 	19Nov04	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::unintp_'
  type(fgInterp) :: fgintp
  type(ppInterp) :: ppintp
  integer :: iAdim,jAdim
  integer :: iGdim,iGloc,iGlen
  integer :: jGdim,jGloc,jGlen

  real(gsiREAL),pointer,dimension(:,:,:) :: upd_ps
  real(gsiREAL),pointer,dimension(:,:,:) :: upd_ts

  real(gsiREAL),pointer,dimension(:,:,:) :: upd_delp
  real(gsiREAL),pointer,dimension(:,:,:) :: upd_uw
  real(gsiREAL),pointer,dimension(:,:,:) :: upd_vw
  real(gsiREAL),pointer,dimension(:,:,:) :: upd_pt
  real(gsiREAL),pointer,dimension(:,:,:) :: upd_cw
  real(gsiREAL),pointer,dimension(:,:,:) :: upd_q
  real(gsiREAL),pointer,dimension(:,:,:) :: upd_oz

  integer :: kmfv
  integer :: ier,myPE
  integer :: kb,ke,k
  real(gsiREAL),allocatable,dimension(:) :: akfv,bkfv

_ALLENTRY_
!________________________________________

  	call MP_comm_rank(comm,myPE,ier)
		if(ier/=0) call MP_die(myname_,'MP_comm_rank()',ier)

    call mpout_log(myname_,'rank =',myPE)

  call fvGrid_get(h_fv,im=iAdim,jm=jAdim,km=kmfv)

    call mpout_log(myname_,'iAdim (fv:nlon) =',iAdim)
    call mpout_log(myname_,'jAdim (fv:nlat) =',jAdim)

	! note that i-j are swapped for guess_grids

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

  	! allocate components of w_fv

  call fvGriddedState_init(w_fv,h_fv,etaLev1,etaLevs)

	! Copy everything we don't know, such as, %phis, %lwi,
	! %hs_stdv, and %q(3:).

  call fvGriddedState_asis(w_fv,w_ref)

	! Prepare for override everything else we do know

 	upd_ps	=> ptr_ps  (w_fv)
	upd_ts	=> ptr_ts  (w_fv)

	upd_delp=> ptr_delp(w_fv)
	upd_uw	=> ptr_uw  (w_fv)
	upd_vw	=> ptr_vw  (w_fv)
	upd_pt	=> ptr_pt  (w_fv)
	upd_cw	=> ptr_cw  (w_fv)
	upd_q	=> ptr_q   (w_fv)
	upd_oz	=> ptr_oz  (w_fv)
!________________________________________

! Define the fgInterp operator by starting from defining its all
! communication operators between intermediate distributed grids.

  if(LLGrid_) then
	! configure for an equal-interval latitude grid
    call fgInterp_l2linit(fgintp,ncep_phis,	&
  	iAdim,jAdim,etaLev1,etaLevs,		&
	iGdim,jGdim,iGloc,iGlen,jGloc,jGlen, nsig ,comm)
  else
	! configure for a Gaussian grid
    call fgInterp_l2ginit(fgintp,ncep_phis,	&
  	iAdim,jAdim,etaLev1,etaLevs,		&
	iGdim,jGdim,iGloc,iGlen,jGloc,jGlen, nsig ,comm)
  endif
!________________________________________

  	allocate(akfv(1:kmfv+1),bkfv(1:kmfv+1))
  call fvGrid_get(h_fv,ak=akfv,bk=bkfv)
	akfv(:)=akfv(:)*kPa_per_Pa	! convert to kPa

	! show (ak,bk) in hPa
  call showakbk_(hPa_per_kPa*akfv,bkfv,myname_,' FV')
  call showakbk_(hPa_per_kPa*ak5 ,bk5 ,myname_,'GSI')
!________________________________________
! Interpolate the surface pressure, which is the basis for the
! definition of the object sigma-Gaussian grid.  The object sigma-
! Gaussian grid is defined by ps and sigma

	! Convert log(ps) to ps as an argument of the subroutine.

			! lnps(:,:) is log(ps(:,:)) with
			! ps in centi-bar (kPa).  It is converted to
			! Pa before the interpolation.
			! tv(:,:,:) is tv(:,:,:)

#ifdef _DEBUG_CHECKSUMS
call checksums_show(exp(lnps(:,:))*Pa_per_kPa,'UNKW','gsi:ps')
call checksums_show(ptr_phis(w_fv),'UNKW','w:phis:0')
#endif

  call psInterp_gtof(fgintp,exp(lnps(:,:))*Pa_per_kPa,		&
  				ak5(1:nsig+1)*Pa_per_kPa,	&
				bk5(1:nsig+1),			&
				tv(:,:,:),			&
		     ppintp,	upd_ps, ptr_phis(w_fv),		&
				akfv*Pa_per_kPa,bkfv  , comm,	&
				delp=upd_delp)
#ifdef _DEBUG_CHECKSUMS
call checksums_show(ptr_phis(w_fv),'UNKW','w:phis:999')
call checksums_show(upd_ps,'UNKW','wfv:ps')
#endif
	! upd_ps unit is already in Pa, as expected by w_fv.

	deallocate(akfv,bkfv)
	nullify(upd_delp)
	nullify(upd_ps)
!________________________________________
! Other 2-d variables: surface temperature?

  call fgInterp_gtof(fgintp, sfct(:,:),upd_ts, MASS,comm)
	nullify(upd_ts)
!________________________________________
! upper-air winds

  call fgInterp_gtod(fgintp,ppintp,	&
	uwnd(:,:,:), vwnd(:,:,:),	&
  	upd_uw, upd_vw, comm)
	nullify(upd_uw)
	nullify(upd_vw)
!________________________________________
! cw- cloud water mixing ratio

  call fgInterp_gtof(fgintp,ppintp,	&
	cwmr(:,:,:), upd_cw, MASS,comm)
	nullify(upd_cw)
!________________________________________
! q- specific humidity

  call fgInterp_gtof(fgintp,ppintp,	&
  	q(:,:,:), upd_q, MASS,comm)
	nullify(upd_q )
!________________________________________
! oz- Ozone

  call fgInterp_gtof(fgintp,ppintp,	&
  	oz(:,:,:), upd_oz, MASS,comm)	! gsi-ozone, in g/g

	upd_oz=upd_oz/GG_per_PPMV	! convert to fv-ozone, in ppmv
	nullify(upd_oz)
!________________________________________
! thv- virtual potential temperatur, t_v to theta_v

  call fgInterp_gtof(fgintp,ppintp,	&
  	tv(:,:,:),upd_pt, THTA,comm, SrcTemp=VIRTUAL_TEMPERATURE)
	nullify(upd_pt)
!________________________________________

  call clean(ppintp)
  call clean(fgintp)
_ALLEXIT_
end subroutine unintp_
#endif
!	of _GMAO_FVGSI_
end module m_gsiGuess

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_gsiStubs - some GMAO library stubs
!
! !DESCRIPTION:
!
! !INTERFACE:
!#include "regime.H"

#ifdef _GMAO_FVGSI_

    module m_gsiStubs
      implicit none
      private	! except

! !REVISION HISTORY:
! 	20Jul05	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_gsiStubs'
end module m_gsiStubs

#else

module m_ggGradient
      use kinds,  only : gsiSNGL => r_single
      use kinds,  only : gsiREAL => r_kind
      implicit none
      private	! except

      public :: ggGradient		! data structure
      public :: ggGradient_init,init
      public :: clean
      public :: ggGrad		! (x,y) = grad(f)
      public :: ggDivo		! d=div(x,y) & z=curl(x,y)

    type ggGradient
      private
      integer :: i
    end type ggGradient

    interface ggGradient_init; module procedure	&
      gginit_,	&
      init_; end interface
    interface init ; module procedure	&
      gginit_,	&
      init_; end interface
    interface clean; module procedure clean_; end interface

    interface ggGrad; module procedure	&
      grad2_,	&
      grad3_; end interface

    interface ggDivo; module procedure	&
      divvor2_,	&
      divvor3_; end interface

contains

subroutine gginit_(ob,nGlon,nGlat,	&
    	lcLon,lnLon,lcLat,lnLat,nlevs,comm, radius)
      implicit none
      type(ggGradient),intent(out) :: ob
      integer,intent(in) :: nGlon,nGlat		! nglon, nglat
      integer,intent(in) :: lcLon,lnLon		! local lon.-subdomain
      integer,intent(in) :: lcLat,lnLat		! local lat.-subdomain
      integer,intent(in) :: nlevs		! size of 3-d grid
      integer,intent(in) :: comm		! communicator
      real(gsiREAL),optional,intent(in) :: radius	! default to Earth
  end subroutine gginit_

subroutine init_(ob,nGlon,rlats,	&
    	lcLon,lnLon,lcLat,lnLat,nlevs,comm, radius)
      implicit none
      type(ggGradient),intent(out) :: ob
      integer,intent(in) :: nGlon		! nglon
      real(gsiREAL),dimension(:),intent(in) :: rlats	! a given list of lat.
      integer,intent(in) :: lcLon,lnLon		! local lon.-subdomain
      integer,intent(in) :: lcLat,lnLat		! local lat.-subdomain
      integer,intent(in) :: nlevs		! size of 3-d grid
      integer,intent(in) :: comm		! communicator
      real(gsiREAL),optional,intent(in) :: radius	! default to Earth
  end subroutine init_

subroutine clean_(ob)
      implicit none
      type(ggGradient),intent(inout) :: ob
  end subroutine clean_

subroutine divvor2_(ob,u,v,div,vor,comm)
      implicit none
      type(ggGradient),intent(in) :: ob		! ggGradient operator
      real(gsiREAL),dimension(:,:),intent(in ) :: u  ,v	! a vector field
      real(gsiREAL),dimension(:,:),intent(out) :: vor,div ! curl(u,v) & div(u,v)
      integer,intent(in) :: comm	! communicator
  end subroutine divvor2_

subroutine divvor3_(ob,u,v,div,vor,comm)
      implicit none
      type(ggGradient),intent(in) :: ob		! ggGradient operator
      real(gsiREAL),dimension(:,:,:),intent(in ) :: u  ,v	! a vector field
      real(gsiREAL),dimension(:,:,:),intent(out) :: vor,div ! curl(u,v) & div(u,v)
      integer,intent(in) :: comm	! communicator
  end subroutine divvor3_

subroutine grad2_(ob,f,x,y,comm)
      implicit none
      type(ggGradient),intent(in) :: ob		! ggGradient operator
      real(gsiREAL),dimension(:,:),intent(in ) :: f	! a scalar field
      real(gsiREAL),dimension(:,:),intent(out) :: x,y	! (x,y)=grad(f)
      integer,intent(in) :: comm	! communicator
  end subroutine grad2_

subroutine grad3_(ob,f,x,y,comm)
      implicit none
      type(ggGradient),intent(in) :: ob		! ggGradient operator
      real(gsiREAL),dimension(:,:,:),intent(in ) :: f	! a scalar field
      real(gsiREAL),dimension(:,:,:),intent(out) :: x,y	! (x,y)=grad(f)
      integer,intent(in) :: comm	! communicator
  end subroutine grad3_

end module m_ggGradient

module m_die
  interface die; module procedure	&
  	die4_; end interface
contains

subroutine die4_(where,mesg1,ival1,mesg2,ival2)
      implicit none
      character(len=*),intent(in) :: where
      character(len=*),intent(in) :: mesg1
      integer,intent(in) :: ival1
      character(len=*),intent(in) :: mesg2
      integer,intent(in) :: ival2
  end subroutine die4_
end module m_die
#endif
