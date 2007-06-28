!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_fvAnaGrid - An analysis for GSI on a FV model grid.
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_fvAnaGrid
#ifdef _GMAO_FVGSI_
      use m_dyn,only : dyn_vect
#endif
      implicit none
      private	! except

      public :: fvAnaGrid_nmlread
		! Read a namelist if there is one, for a collection of
		! GMAO gsiGuess grid configuration data.

      public :: fvAnaGrid_setup
      		! Define analysis grid attributes

      public :: fvAnaGrid_allgetlist
      public :: fvAnaGrid_getlist
      		! Define a list of input file numbers.  This is a hack.
		! It needs to be improved once we make the module
		! working as expected.

      public :: fvAnaGrid_read
      		! Read, if not already in memory, background state on a
		! fv-eta grid as the guess state; then interpolate the
		! guess state on to a distributed sigma-Gaussian grid,
		! defined in module guess_grids.  Additional diagnostic
		! variables are derived from the basic guess_grids.

      public :: fvAnaGrid_surface_read
      		! Read surface variables from a fv-surface file and from
		! a ncep-surface file.

      public :: fvAnaGrid_surface_getvar
      		! Read a single surface variable from a fv-surface file
		! or a ncep-surface file.

      public :: fvAnaGrid_write
		! (un)interpolate data in guess_grids as the up-to-date
		! analysis state onto a fv-eta grid.  Compute analysis
		! increments and write out the analysis.

      interface fvAnaGrid_nmlread; module procedure	&
		nmlread_; end interface

      interface fvAnaGrid_setup; module procedure setup_; end interface
      interface fvAnaGrid_allgetlist ; module procedure		&
      		allgetlist_; end interface
      interface fvAnaGrid_getlist ; module procedure		&
      		getlist_; end interface

      interface fvAnaGrid_read ; module procedure  read_; end interface
      interface fvAnaGrid_surface_read ; module procedure	&
      		surface_read_; end interface
      interface fvAnaGrid_surface_getvar ; module procedure	&
      		surface_getvar_; end interface

      interface fvAnaGrid_write; module procedure write_; end interface

! !REVISION HISTORY:
! 	17Sep04	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!       08Jan06  -Banglin Zhang - updated for analysis of satellite rain rate data
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_fvAnaGrid'

! This module is a singleton object.

  			! File tag of FV surface variables
  character(len=*),parameter :: FV_SFC="fv-sfc"
  integer,parameter :: IFILE_FV=6
  			! File tag of NCEP surface variables
  character(len=*),parameter :: NC_SFC="nc-sfc"
  integer,parameter :: IFILE_NC=6

  			! File tag of the first guess in "dyn" format.

  character(len=*),parameter :: FV_DYN="fv-dyn"

  			! File tag of the analysis  in "dyn" format.

  character(len=*),parameter :: AN_DYN="an-dyn"

  			! "dyn" file tag of the analysis increments.

  character(len=*),parameter :: AI_DYN="ai-dyn"

#ifdef _IGNORE_GRIDVERIFY_
  logical,parameter :: GRIDVERIFY_=.false.
#else
  logical,parameter :: GRIDVERIFY_=.true.
#endif
#ifdef _GMAO_FVGSI_
#include "mytrace.H"
#endif
contains
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: nmlread_ - read NAMELIST/gsiGuess/
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine nmlread_(lu)
#ifdef _GMAO_FVGSI_
      use m_gsiGuess,only : gsiGuessGrid_nmlread
      use m_mpout,only : mpout_log
      use m_die,only : die
#endif
      implicit none
      integer,intent(in) :: lu	! input is opened on this unit

! !REVISION HISTORY:
! 	26Jan06	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::nmlread_'
  integer :: ier
#ifdef _GMAO_FVGSI_
_ALLENTRY_
	! This is a thin layer to pass the control to gsiGuess_nmlread()
	! to read user defined grid options.
  call gsiGuessGrid_nmlread(lu)
_ALLEXIT_
#endif
end subroutine nmlread_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: setup_ - configure the global grid
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine setup_(ifile,iyr,imo,idy,ihr,freq, comm)
#ifdef _GMAO_FVGSI_
      use m_gsiGuess,only : gsiGuessGrid_setup
      use m_mpout,only : mpout_log
#endif
      implicit none
      integer,intent(in) :: ifile
      integer,intent(out):: iyr,imo,idy,ihr
      integer,intent(out):: freq	! increment in hhmmss
      integer,intent(in) :: comm

! !REVISION HISTORY:
! 	07Apr05	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::setup_'
  character(len=len(FV_DYN)+4) :: fvdynName
  integer,parameter :: ROOT=0
  integer :: ier,myPE
!________________________________________
#ifdef _GMAO_FVGSI_
_ALLENTRY_
! read the header of the guess

  write(fvdynName,'(2a,i2.2)') trim(FV_DYN),'.',ifile
		call mpout_log(myname_,	&
			'reading from "'//trim(fvdynName)//'"')
  call gsiGuessGrid_setup(fvdynName,iyr,imo,idy,ihr,freq,comm,ROOT)
_ALLEXIT_
#else
	iyr=-1;imo=-1;idy=-1;ihr=-1;freq=-1
#endif
end subroutine setup_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: allgetlist_ - get a list of input files
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine allgetlist_(comm)
      use guess_grids,only : nfldsig,ntguessig,ifilesig,hrdifsig
      use guess_grids,only : nfldsfc,ntguessfc,ifilesfc,hrdifsfc
#ifdef _GMAO_FVGSI_
      use m_mpout,only : mpout_log,mpout,mpout_ison
      use m_mpif90,only : MP_comm_rank,MP_type
      use m_die   ,only : MP_die
#endif
      implicit none
      integer,intent(in) :: comm

! !REVISION HISTORY:
! 	02Aug05	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::allgetlist_'
  integer :: itime
  integer :: ier
  integer :: myPE
  integer,parameter :: ROOT=0
#ifdef _GMAO_FVGSI_
_ALLENTRY_

  call MP_comm_rank(comm,myPE,ier)
	if(ier/=0) call MP_die(myname_,'MP_comm_rank()',ier)

  if(myPE==ROOT) call getlist_()
  
  call mpi_bcast(ntguessig,1,MP_type(ntguessig),ROOT,comm,ier)
  	if(ier/=0) call MP_die(myname_,'MPI_bcast(ntguessig)',ier)
  call mpi_bcast(nfldsig,1,MP_type(nfldsig),ROOT,comm,ier)
  	if(ier/=0) call MP_die(myname_,'MPI_bcast(nfldsig)',ier)
  call mpi_bcast(ifilesig,size(ifilesig),MP_type(ifilesig),	&
  	ROOT,comm,ier)
  	if(ier/=0) call MP_die(myname_,'MPI_bcast(ifilesig)',ier)
  call mpi_bcast(hrdifsig,size(hrdifsig),MP_type(hrdifsig),	&
  	ROOT,comm,ier)
  	if(ier/=0) call MP_die(myname_,'MPI_bcast(hrdifsig)',ier)

  call mpi_bcast(ntguessfc,1,MP_type(ntguessfc),ROOT,comm,ier)
  	if(ier/=0) call MP_die(myname_,'MPI_bcast(ntguessfc)',ier)
  call mpi_bcast(nfldsfc,1,MP_type(nfldsfc),ROOT,comm,ier)
  	if(ier/=0) call MP_die(myname_,'MPI_bcast(nfldsfc)',ier)
  call mpi_bcast(ifilesfc,size(ifilesfc),MP_type(ifilesfc),	&
  	ROOT,comm,ier)
  	if(ier/=0) call MP_die(myname_,'MPI_bcast(ifilesfc)',ier)
  call mpi_bcast(hrdifsfc,size(hrdifsfc),MP_type(hrdifsfc),	&
  	ROOT,comm,ier)
  	if(ier/=0) call MP_die(myname_,'MPI_bcast(hrdifsfc)',ier)

  if(mpout_ison().or.(mod(myPE,10)==0)) then
    do itime=1,nfldsig
      write(mpout,'(1x,2a,i3.2,2(i4.2,a,f7.3,a))')		&
      		myname_,': state #',itime,			&
      		ifilesig(itime),'(',hrdifsig(itime),' hr)',	&
      		ifilesfc(itime),'(',hrdifsfc(itime),' hr)'
    end do
  endif

_ALLEXIT_
#endif
end subroutine allgetlist_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: getlist_ - get a list of input files
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine getlist_()
      use guess_grids,only : nfldsig,ntguessig,ifilesig,hrdifsig
      use guess_grids,only : nfldsfc,ntguessfc,ifilesfc,hrdifsfc
      use obsmod     ,only : iadate
      use gridmod    ,only : nhr_assimilation
#ifdef _GMAO_FVGSI_
      use m_fvGrid,only : fvGrid
      use m_fvGrid,only : fvGrid_get
      use m_fvGrid,only : clean
      use m_fvGridHeader,only : fvGridHeader_read

      use m_fvSurface,only : fvSurface
      use m_fvSurface,only : fvSurface_getheader
      use m_fvSurface,only : get, clean

      use m_ncSurface,only : ncSurface
      use m_ncSurface,only : ncSurface_getheader
      use m_ncSurface,only : get, clean

      use m_mpout ,only : mpout_log
      use m_die   ,only : perr,die
#endif
      implicit none

! !REVISION HISTORY:
! 	02Aug05	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!   16Aug05 - Russ Treadon <russ.treadon@noaa.gov>
!       - move h_fvG, h_fvS, h_ncS declarations inside "ifdef _GMAO_FVGSI_" construct
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::getlist_'
  integer :: ier,i,ifile
  integer :: iyr,imo,idy,ihr,inc
  logical :: fexist
  integer :: nmin,nmin_an,nmin_df,nmin_half
  character(len=max(len(FV_DYN),len(FV_SFC),len(NC_SFC))+4) :: fname
  character(len=8) :: cymd
  character(len=6) :: chms

#ifdef _GMAO_FVGSI_
  type(fvGrid)    :: h_fvG
  type(fvSurface) :: h_fvS
  type(ncSurface) :: h_ncS
_ENTRY_

  ifilesig(:)=-100
  hrdifsig(:)=   0
  nfldsig=1
  ntguessig=1
  ifilesig(1:nfldsig)=6
  hrdifsig(1:nfldsig)=0.

	! Set the list for surface data files by copying the list for
  	! upperair data files.

  nfldsfc=nfldsig
  ntguessfc=ntguessig
  ifilesfc(1:nfldsfc)=ifilesig(1:nfldsig)
  ifilesfc(nfldsfc+1:)=ifilesig(nfldsig+1:)
  hrdifsfc(1:nfldsfc)=hrdifsig(1:nfldsig)
  hrdifsfc(nfldsfc+1:)=hrdifsig(nfldsig+1:)

  	! Set the time baseline (analysis time)

  call w3fs21(iadate,nmin_an)
	! w3fs21(NCEP-w3) converts analysis time to minutes relative to
	! a fixed date.

  write(cymd,'(i4.4,i2.2,i2.2)') iadate(1),iadate(2),iadate(3)
  write(chms,'(i2.2,i2.2,i2.2)') iadate(4),0,0
  call mpout_log(myname_,'analysis yyyymmdd:hhmmss = '//cymd//':'//chms)
  call mpout_log(myname_,'analysis time in minutes =',nmin_an)

  	! Set the time range (half) in minutes

  nmin_half=(nhr_assimilation+1)/2*60
!________________________________________
    	! Check the list for fv-dyn files (GMAO GEOS/DAS)
    i=0
    ntguessig=-1
    do ifile=00,99
      write(fname,'(2a,i2.2)') trim(FV_DYN),'.',ifile
      inquire(file=fname,exist=fexist)
      if(fexist) then

        call fvGridHeader_read(fname,h_fvG,gridverify=GRIDVERIFY_)
        call fvGrid_get(h_fvG,year=iyr,month=imo,day=idy,	&
	  hour=ihr,freq=inc)
	call clean(h_fvG)

		! covert the data time to minutes
	call w3fs21((/iyr,imo,idy,ihr,0/),nmin)

	write(cymd,'(i4.4,i2.2,i2.2)') iyr,imo,idy
	write(chms,'(i2.2,i2.2,i2.2)') ihr,  0,  0
        call mpout_log(myname_,'"'//trim(fname)//	&
		'", yyyymmdd:hhmmss = '//cymd//':'//chms)

	nmin_df=nmin-nmin_an
	call mpout_log(myname_,'time in minutes =',nmin)
	call mpout_log(myname_,'diff in minutes =',nmin_df)

	if(abs(nmin_df) <= nmin_half) then
          i=i+1				! count it in
          ifilesig(i)=ifile		! file number
	  hrdifsig(i)=nmin_df/60.	! time offset in hours

	  if(nmin_df==0) then		! 1 min >> 1hr*.001, comparing
	  				! to r0_001 in read_files()
	    call mpout_log(myname_,	&
	    	'set ntguessig to file "'//trim(fname)//'",',i)

	    if(ntguessig/=-1) call die(myname_,	&
	    	'already defined, ntguessig',ntguessig)

	    ntguessig=i
	  endif
	endif
      endif
    end do
    nfldsig=i
    if(ntguessig==-1) call die(myname_,'undefined, ntguessig')
    if(nfldsig  == 0) call die(myname_,'no background file included')
!________________________________________
    	! Check the list for fv-sfc files (GMAO GEOS/DAS).  The list
	! should be all match, until I am sure what the rest of the
	! GSI is expecting.

    nfldsfc  =nfldsig
    ntguessfc=ntguessig
    ifilesfc(:)=ifilesig(:)
    hrdifsfc(:)=hrdifsig(:)

    ier=0
    do i=1,nfldsfc
      ifile=ifilesfc(i)

      write(fname,'(2a,i2.2)') trim(FV_SFC),'.',ifile
      inquire(file=fname,exist=fexist)

      if(.not.fexist) then
	ier=1
        call perr(myname_,'file not found, "'//trim(fname)//'"')

      else
        call fvSurface_getheader(fname,h_fvS)
        call get(h_fvS,year=iyr,month=imo,day=idy,hour=ihr,freq=inc)
	call clean(h_fvS)

		! covert the data time to minutes
	call w3fs21((/iyr,imo,idy,ihr,0/),nmin)

	write(cymd,'(i4.4,i2.2,i2.2)') iyr,imo,idy
	write(chms,'(i2.2,i2.2,i2.2)') ihr,  0,  0
        call mpout_log(myname_,'"'//trim(fname)//	&
		'", yyyymmdd:hhmmss = '//cymd//':'//chms)

	nmin_df=nmin-nmin_an
	call mpout_log(myname_,'time in minutes =',nmin)
	call mpout_log(myname_,'diff in minutes =',nmin_df)

	if(nint(hrdifsfc(i)*60)/=nmin_df) then
	  ier=1
	  call perr(myname_,'inconsistent hrdifsfc("'//trim(fname)// &
	  	'"), expecting',nint(hrdifsfc(i)*60),	&
		'get',nmin_df)
	endif
      endif
    end do

    if(ier/=0) call die(myname_,'inconsistent input files')
!________________________________________

    	! Check the list for nc-sfc files (NCEP)
    ier=-1
    do i=1,nfldsfc
      write(fname,'(2a,i2.2)') trim(NC_SFC),'.',ifilesfc(i)
      inquire(file=fname,exist=fexist)
      if(fexist) then
        call ncSurface_getheader(fname,h_ncS)
        call get(h_ncS,year=iyr,month=imo,day=idy,hour=ihr,freq=inc)
	call clean(h_ncS)

		! (iyr,imo,idy,ihr) += inc; inc=0
	call movdate_(iyr,imo,idy,ihr,inc)

		! convert the data time to minutes
	call w3fs21((/iyr,imo,idy,ihr,0/),nmin)

	write(cymd,'(i4.4,i2.2,i2.2)') iyr,imo,idy
	write(chms,'(i2.2,i2.2,i2.2)') ihr,  0,  0
        call mpout_log(myname_,'"'//trim(fname)//	&
		'", yyyymmdd:hhmmss = '//cymd//':'//chms)

	nmin_df=nmin-nmin_an
	call mpout_log(myname_,'time in minutes =',nmin)
	call mpout_log(myname_,'diff in minutes =',nmin_df)

	ier=ier+1
      endif
    end do

    if(ier<0) call die(myname_,'no ncSurface file')
    ier=0
_EXIT_
contains
subroutine movdate_(iyr,imo,idy,ihr,inc)
  use m_gsiGuess,only : gsiREAL,gsiINTG
  implicit none
  integer,intent(inout) :: iyr,imo,idy,ihr,inc	! y,m,d,h, hhmmss

  integer(gsiINTG),dimension(8) :: idate,odate
  real(gsiREAL),dimension(5) :: xtime

  idate(1)=iyr; idate(2)=imo		! year; month
  idate(3)=idy; idate(4)=0		! day ; time-zone
  idate(5)=ihr; idate(6)=0		! hour; min
  idate(7)=0  ; idate(8)=0		! sec ; milsec.

  xtime(1)=0  ; xtime(2)=inc/10000	! day; hour
  xtime(3)=mod(inc/100,100)		! min
  xtime(4)=0  ; xtime(5)=0		! sec; milsec

  call w3movdat(xtime,idate,odate)
  iyr=odate(1); imo=odate(2)		! year; month
  idy=odate(3); ihr=odate(5)		! day ; hour
  inc=0					! hhmmss
end subroutine movdate_
#endif
end subroutine getlist_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: read_ - interpolate fvGriddedState to GSI guess fields.
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine read_(itime,ifile,				&
	zsfc,lnps,vorx,divx,uwnd,vwnd,tv  ,q   ,cwmr,oz  ,	&
	sfct, comm)

      use m_gsiGuess,only : gsiREAL
      use m_gsiGuess,only : ntguessig

#ifdef _GMAO_FVGSI_
      use m_gsiGuess,only : gsiGuess_intp
      use m_gsiGuess,only : gsiGuess_unintp
      use m_gsiGuess,only : phisfile
      use m_gsiCheck,only : gsiCheck_show

      use m_fvGriddedState,only : fvGriddedState
      use m_fvGriddedState,only : fvGriddedState_read
      use m_fvGriddedState,only : fvGriddedState_write
      use m_fvGriddedState,only : ptr_phis
      use m_fvGriddedState,only : clean
      use m_checksums,only : checksums_show

      use m_interleavedObject,only : interleavedObject
      use m_interleavedObject,only : clean
      use m_fvGrid,only : fvGrid
      use m_fvGrid,only : clean

      use m_mpout,only : mpout_log
#endif
      implicit none

      integer,intent(in) :: itime	! state index
      integer,intent(in) :: ifile	! file index

      real(gsiREAL),dimension(:,:),intent(out) :: zsfc	! elevation
      real(gsiREAL),dimension(:,:),intent(out) :: lnps	! log(ps) in kPa
      real(gsiREAL),dimension(:,:,:),intent(out) :: vorx ! 2d-vor
      real(gsiREAL),dimension(:,:,:),intent(out) :: divx ! 2d-div
      real(gsiREAL),dimension(:,:,:),intent(out) :: uwnd ! u-wind
      real(gsiREAL),dimension(:,:,:),intent(out) :: vwnd ! v-wind
      real(gsiREAL),dimension(:,:,:),intent(out) :: tv   ! virt. temp.
      real(gsiREAL),dimension(:,:,:),intent(out) :: q    ! virt. temp.
      real(gsiREAL),dimension(:,:,:),intent(out) :: cwmr ! cld.wat.m.r.
      real(gsiREAL),dimension(:,:,:),intent(out) :: oz   ! ozone
      real(gsiREAL),dimension(:,:),intent(out) :: sfct	  ! sfc. tv

      integer,intent(in) :: comm	! communicator

! !REVISION HISTORY:
! 	17Sep04	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::read_'
  integer,parameter :: ROOT=0

  character(len=len(FV_DYN)+4) :: fvdynName
  character(len=len(AN_DYN)+4) :: andynName

  integer :: ier

#ifdef _GMAO_FVGSI_
  type(fvGriddedState) :: w_fv,w_sv
  type(fvGrid) :: h_fv
  type(interleavedObject) :: etaLev1,etaLevs
!________________________________________
 	! Read a file at a given time
_ALLENTRY_
      write(fvdynName,'(2a,i2.2)') trim(FV_DYN),'.',ifile
		call mpout_log(myname_,	&
			'reading from "'//trim(fvdynName)//'"')

      call fvGriddedState_read(fvdynName,			&
      		w_fv,h_fv,etaLev1,etaLevs,comm,root,		&
		gridverify=GRIDVERIFY_)

      	! derives background state variables (plus divergence and
	! vorticity) on the GSI guess grid by interpolation, from,
	! for example, the FV grid.
      call gsiGuess_intp(w_fv, h_fv,etaLev1,etaLevs,	&
		phisfile,				&
		zsfc,lnps,uwnd,vwnd,tv,q,cwmr,oz,sfct,	&
		comm,vor=vorx,div=divx)

#ifdef DEBUG_CHECKSUMS
	call gsiCheck_show(myname_,			&
		zsfc,lnps,uwnd,vwnd,tv,q,cwmr,oz,sfct,	&
		vorx,divx, comm)
#endif
	!________________________________________
      if(itime==ntguessig) then
	call mpout_log(myname_,'itime(ntguessig) =',itime)
	call mpout_log(myname_,'ifile(ntguessig) =',ifile)

      		! do an inverse-interpolation if the time is the
		! analysis time.

        call gsiGuess_unintp(w_sv, w_fv,h_fv,etaLev1,etaLevs,	&
		phisfile,				&
		zsfc,lnps,uwnd,vwnd,tv,q,cwmr,oz,sfct,	&
		comm)

		! write out the inversely-interpolated state for
		! future use.

        write(andynName,'(2a,i2.2)') trim(AN_DYN),'.',0
		call mpout_log(myname_,	&
			'writing to "'//trim(andynName)//'"')

	call fvGriddedState_write(andynName,		&
			w_sv,h_fv,etaLev1,etaLevs,comm,root )

	call clean(w_sv)
      endif
      call clean(w_fv)
	!________________________________________
      call clean(h_fv)
      call clean(etaLev1)
      call clean(etaLevs)

!*** It is not clear, thus yet to be implemented, the mechanism of ***
!*** bias correction.						   ***

_ALLEXIT_
#else
	zsfc=0.; lnps=0.; vorx=0.; divx=0.; uwnd=0.
	vwnd=0.; tv  =0.; q   =0.; cwmr=0.; oz  =0.
	sfct=0.
#endif
end subroutine read_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: surface_read_ - interpolate surface parameters to GSI grid
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine surface_read_(itime,ifile, ws10,tskn,		&
    	snow_dep,veg_type,veg_frac,soil_type,soil_temp,		&
	soil_mois,isli_mask,isli_glbl, comm )

      use m_gsiGuess,only : gsiREAL
      use m_gsiGuess,only : ntguessig

#ifdef _GMAO_FVGSI_
      use m_gsiGuess,only : gsiGuess_surface_intp

      use m_fvSurface,only : fvSurface
      use m_fvSurface,only : fvSurface_read
      use m_fvSurface,only : ptr_isli_mask
      use m_fvSurface,only : clean

      use m_ncSurface,only : ncSurface
      use m_ncSurface,only : ncSurface_read
      use m_ncSurface,only : clean

      use m_mpout,only : mpout_log
      use m_mpif90,only : MP_type,MP_comm_rank
      use m_die,only : MP_die

      use m_gsiCheck,only : gsiCheck_surface_show
#endif
      implicit none

      integer,intent(in) :: itime	! state index, not used
      integer,intent(in) :: ifile	! file index, not used

      real(gsiREAL),dimension(:,:),intent(out) :: ws10	! 10m wind speed
      real(gsiREAL),dimension(:,:),intent(out) :: tskn	! T skin
      real(gsiREAL),dimension(:,:),intent(out) :: snow_dep ! snow depth
      real(gsiREAL),dimension(:,:),intent(out) :: veg_type ! veg. type
      real(gsiREAL),dimension(:,:),intent(out) :: veg_frac ! veg. cover.
      real(gsiREAL),dimension(:,:),intent(out) :: soil_type ! soil type
      real(gsiREAL),dimension(:,:),intent(out) :: soil_temp ! soil temp.
      real(gsiREAL),dimension(:,:),intent(out) :: soil_mois ! soil mois.

      integer,dimension(:,:),intent(out) :: isli_mask ! island/land/ice
      integer,dimension(:,:),intent(out) :: isli_glbl ! island/land/ice

      	! isli_mask is in subdomains, while isli_glbl is global on
	! every PE.

      integer,intent(in) :: comm	! communicator

! !REVISION HISTORY:
! 	17Sep04	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::surface_read_'
  integer,parameter :: ROOT=0

  character(len=len(FV_SFC)+4) :: fvSfcName
  character(len=len(NC_SFC)+4) :: ncSfcName
  integer :: ier,myPE
  integer :: isize,jsize
#ifdef _GMAO_FVGSI_
  type(fvSurface) :: w_fv
  type(ncSurface) :: w_nc
  real(gsiREAL),pointer,dimension(:,:,:) :: ptr
!________________________________________
	! Read a file at a given time
_ALLENTRY_
	call MP_comm_rank(comm,myPE,ier)
		if(ier/=0) call MP_die(myname_,'MP_comm_ranl()',ier)

      write(fvSfcName,'(2a,i2.2)') trim(FV_SFC),'.',ifile
		call mpout_log(myname_,	&
			'reading from "'//trim(fvSfcName)//'"')
	!________________________________________
      	! Derive surface variables from a FV file.  First, read the
	! file and put the data into a distributed data structure.

      call fvSurface_read(fvsfcName,w_fv,comm,root)
      call gsiGuess_surface_intp(w_fv, ws10,tskn,snow_dep,	&
      	soil_temp,soil_mois,isli_mask,isli_glbl, comm)
	call clean(w_fv)
!_______________________________________________________________________
	! Read a file at a given time

      write(ncSfcName,'(2a,i2.2)') trim(NC_SFC),'.',IFILE_NC
		call mpout_log(myname_,	&
			'reading from "'//trim(ncSfcName)//'"')
	!________________________________________
      	! Derive surface variables from a NCEP sfcio file

      call ncSurface_read(ncSfcName,w_nc,comm,root)
      call gsiGuess_surface_intp(w_nc,veg_frac,veg_type,soil_type,comm)
	call clean(w_nc)

#ifdef DEBUG_CHECKSUMS
      call gsiCheck_surface_show(myname_, ws10,tskn,		&
    	snow_dep , veg_type, veg_frac,				&
	soil_type,soil_temp, soil_mois,isli_mask,isli_glbl, comm )
#endif
_ALLEXIT_
#endif
end subroutine surface_read_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: surface_getvar_ - get/interpolate a fv-surface variable
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine surface_getvar_(var, tag, ifile)
      use m_gsiGuess,only : gsiREAL
#ifdef _GMAO_FVGSI_
      use m_fvSurface,only : fvSurface_alloc_getvar
      use m_llInterp,only : llInterp
      use m_llInterp,only : llInterp_l2ginit,clean
      use m_llInterp,only : llInterp_lh2rh
      use m_swapij,only : swapij

      use m_mpout,only : mpout_log
      use m_die,only : die
#endif
      implicit none

      		! below is a selected global variable stored locally
      real(gsiREAL),dimension(:,:),intent(out) :: var
      character(len=*),intent(in) :: tag	! name of the variable.
      integer,optional,intent(in) :: ifile	! file index, not used

! !REVISION HISTORY:
! 	17Sep04	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!   16Aug05 - Russ Treadon <russ.treadon@noaa.gov>
!       - move "type(llInterp) :: llintp" inside "ifdef _GMAO_FVGSI_" construct
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::surface_getvar_'

  character(len=len(FV_SFC)+4) :: fvSfcName
  character(len=len(NC_SFC)+4) :: ncSfcName
  integer :: ier
  integer :: isize,jsize
  integer :: mlon,mlat
  integer :: nlon,nlat
  real(gsiREAL),pointer    ,dimension(:,:) :: pvar
  real(gsiREAL),allocatable,dimension(:,:) :: qvar
#ifdef _GMAO_FVGSI_
  type(llInterp) :: llintp
!________________________________________
	! Read a file at a given time
_ENTRY_
	!________________________________________
      	! Derive surface variables from a FV file.

      nlon=size(var,2)
      nlat=size(var,1)

      select case(tag)
      case('tsea','sheleg','slmsk')
if(present(ifile)) then
        write(fvSfcName,'(2a,i2.2)') trim(FV_SFC),'.',ifile
		call mpout_log(myname_,'reading from "'//	&
			trim(fvSfcName)//'" for "'//		&
			trim(tag)//'"' )
else
	! I have to hardwired this filename to a fixed number, not
	! from ifilesfc(:), since this called before getlist().

        write(fvSfcName,'(2a,i2.2)') trim(FV_SFC),'.',IFILE_FV
		call mpout_log(myname_,'reading from "'//	&
			trim(fvSfcName)//'" for "'//		&
			trim(tag)//'"' )
endif
        select case(tag)
	case('tsea')
          call fvSurface_alloc_getvar(fvsfcName,pvar,'TSKIN')
	case('sheleg')
          call fvSurface_alloc_getvar(fvsfcName,pvar,'SNOWDP')
	case('slmsk')
          call fvSurface_alloc_getvar(fvsfcName,pvar,'ORO')
	end select

	mlon=size(pvar,1)
	mlat=size(pvar,2)

	call llInterp_l2ginit(llintp,mlon,mlat,nlon,nlat)

				allocate(qvar(nlon,nlat))
	call llInterp_lh2rh(llintp,pvar, qvar)
			deallocate(pvar)

	call swapij(	   qvar,var)
		deallocate(qvar)

        call clean(llintp)
      case default
      	call die(myname_,'unsupported variable, "'//trim(tag)//'"')
      end select
!_______________________________________________________________________
_EXIT_
#endif
end subroutine surface_getvar_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: write_ - interpolate guess_grid as analysis and write out.
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine write_(itime,ifile,			&
	zsfc,lnps,uwnd,vwnd,tv  ,q   ,cwmr,oz  ,	&
	sfct, comm)

      use m_gsiGuess,only : gsiREAL
      use m_gsiGuess,only : ntguessig
#ifdef _GMAO_FVGSI_
      use m_gsiGuess,only : gsiGuess_unintp
      use m_gsiGuess,only : aincout
      use m_gsiGuess,only : aincfile
      use m_gsiGuess,only : phisfile
      use m_gsiCheck,only : gsiCheck_show

      use m_fvGriddedState,only : fvGriddedState
      use m_fvGriddedState,only : fvGriddedState_read
      use m_fvGriddedState,only : fvGriddedState_incr
      use m_fvGriddedState,only : fvGriddedState_write
      use m_fvGriddedState,only : fvGriddedState_bdump
      use m_fvGriddedState,only : ptr_phis
      use m_fvGriddedState,only : clean

      use m_interleavedObject,only : interleavedObject
      use m_interleavedObject,only : clean
      use m_fvGrid,only : fvGrid
      use m_fvGrid,only : clean

      use m_mpout,only : mpout_log
      use m_die,only : die
#endif
      implicit none

      integer,intent(in) :: itime	! state index
      integer,intent(in) :: ifile	! file index

      real(gsiREAL),dimension(:,:),intent(in) :: zsfc	! elevation
      real(gsiREAL),dimension(:,:),intent(in) :: lnps	! log(ps) in kPa
      real(gsiREAL),dimension(:,:,:),intent(in) :: uwnd ! u-wind
      real(gsiREAL),dimension(:,:,:),intent(in) :: vwnd ! v-wind
      real(gsiREAL),dimension(:,:,:),intent(in) :: tv   ! virt. temp.
      real(gsiREAL),dimension(:,:,:),intent(in) :: q    ! virt. temp.
      real(gsiREAL),dimension(:,:,:),intent(in) :: cwmr ! cld.wat.m.r.
      real(gsiREAL),dimension(:,:,:),intent(in) :: oz   ! ozone
      real(gsiREAL),dimension(:,:),intent(in) :: sfct	  ! sfc. tv

      integer,intent(in) :: comm	! communicator

! !REVISION HISTORY:
! 	17Sep04	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::write_'

  integer,parameter :: ROOT=0

  character(len=len(FV_DYN)+4) :: fvdynName
  character(len=len(AN_DYN)+4) :: andynName
  character(len=len(AN_DYN)+4) :: aidynName

#ifdef _GMAO_FVGSI_
  type(fvGriddedState) :: w_fv,w_sv,w_an
  type(fvGrid) :: h_fv,h_sv
  type(interleavedObject) :: etaLev1,etaLevs
  integer :: ier
!________________________________________
_ALLENTRY_

if(itime/=ntguessig)	&
	call die(myname_,'itime',itime,'ntguessig',ntguessig)

! read in the first guess

  write(fvdynName,'(2a,i2.2)') trim(FV_DYN),'.',ifile
		call mpout_log(myname_,	&
			'reading from "'//trim(fvdynName)//'"')

  call fvGriddedState_read(fvdynName,		&
  			w_fv,h_fv,etaLev1,etaLevs,comm,ROOT,	&
			gridverify=GRIDVERIFY_)

	call clean(etaLev1)
	call clean(etaLevs)

! read in the saved inversely-interpolated interpolatetd-first-guess

	call mpout_log(myname_,'itime(ntguessig) =',itime)
	call mpout_log(myname_,'ifile(ntguessig) =',ifile)

  write(andynName,'(2a,i2.2)') trim(AN_DYN),'.',0
		call mpout_log(myname_,	&
			'reading from "'//trim(andynName)//'"')

  		call fvGriddedState_read(andynName,		&
  			w_sv,h_sv,etaLev1,etaLevs,comm,ROOT,	&
			gridverify=GRIDVERIFY_)
		call clean(h_sv)	! use h_fv instead

! inversely interpolate the current analysis

#ifdef DEBUG_CHECKSUMS
	call gsiCheck_show(myname_,			&
		zsfc,lnps,uwnd,vwnd,tv,q,cwmr,oz,sfct,	&
		comm)
#endif

  call gsiGuess_unintp(w_an, w_fv,h_fv,etaLev1,etaLevs,	&
		phisfile,				&
		zsfc,lnps,uwnd,vwnd,tv,q,cwmr,oz,sfct,	&
		comm)

! write out new w_an for verification

  write(andynName,'(2a,i2.2)') trim(AN_DYN),'.',98
 	call mpout_log(myname_,	&
			'writing to "'//trim(andynName)//'"')
	call fvGriddedState_write(andynName,		&
			w_an,h_fv,etaLev1,etaLevs,comm,ROOT )

! update the first guess, w_fv += w_an-w_sv

  call fvGriddedState_incr(w_fv,w_an,w_sv)
	call clean(w_sv)

! write out new w_fv as analysis

  write(andynName,'(2a,i2.2)') trim(AN_DYN),'.',99
	 	call mpout_log(myname_,	&
			'writing to "'//trim(andynName)//'"')
   call fvGriddedState_write(andynName,		&
			w_fv,h_fv,etaLev1,etaLevs,comm,ROOT )

! write out w_an as analysis increment

  if(aincout) then
    write(aidynName,'(2a,i2.2)') trim(AI_DYN),'.',99
	 	call mpout_log(myname_,	&
			'writing to "'//trim(aidynName)//'"')
    call fvGriddedState_write(aidynName,		&
			w_an,h_fv,etaLev1,etaLevs,comm,ROOT )
  endif

  if(aincfile/='.none.') then
    call mpout_log(myname_,'write a-grid increments to "'// &
        trim(aincfile)//'"')
    call fvGriddedState_bdump(aincfile,w_an,h_fv,etaLev1,etalevs, &
        comm,ROOT)
  endif

	call clean(etaLev1)
	call clean(etaLevs)
	call clean(h_fv)
  	call clean(w_fv)
  	call clean(w_an)

_ALLEXIT_
#endif
end subroutine write_
end module m_fvAnaGrid
