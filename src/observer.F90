module observermod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    observermod   driver for gridpoint statistical 
!                                     interpolation
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: executes gridpoint spectral interpolation analysis.
!
! program history log:
!   2009-01-28  todling - split observer into init/set/run/finalize
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: i_kind
  use constants, only: izero,ione
  use mpimod, only: mype
  use jfunc, only: miter,jiter,jiterstart,destroy_jfunc,&
       set_pointer,&
       switch_on_derivatives,tendsflag,create_jfunc
  use guess_grids, only: create_ges_grids,create_sfc_grids,&
       destroy_sfct,destroy_ges_grids,destroy_gesfinfo
  use obsmod, only: write_diag,obs_setup,ndat,dirname,lobserver
  use gsi_4dvar, only: l4dvar
  use convinfo, only: convinfo_destroy
  use m_gsiBiases, only : create_bias_grids, destroy_bias_grids
  use control_vectors
  use m_berror_stats, only: berror_get_dims
  use timermod, only: timer_ini, timer_fnl
  use read_obsmod, only: read_obs
  use lag_fields, only: lag_guessini


  implicit none

  private
! public observer_guess_init     ! RT: doesn't really belong here
  public observer_init
  public observer_set
  public observer_run
  public observer_finalize
! public observer_guess_finalize ! RT: doesn't really belong here
  public ndata

  integer(i_kind),allocatable,dimension(:,:):: ndata

  interface observer_guess_init
     module procedure guess_init_
  end interface
  interface observer_init
     module procedure init_
  end interface
  interface observer_set
     module procedure set_
  end interface
  interface observer_run
     module procedure run_
  end interface
  interface observer_finalize
     module procedure final_
  end interface
  interface observer_guess_finalize
     module procedure guess_final_
  end interface

  logical,save:: iamset_         = .false.
  logical,save:: fg_initialized_ = .false.
  logical,save:: fg_finalized_   = .false.

contains

subroutine guess_init_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    observer_guess_init  driver for gridpoint statistical 
!                                     interpolation
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: executes gridpoint spectral interpolation analysis.
!
! program history log:
!   1990-10-06  parrish
!   2007-10-03  todling - created this file from slipt of glbsoi
!   2009-01-28  todling - split observer into init/set/run/finalize
!   2009-03-10  meunier - read in the original position of balloons
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare passed variables

! Declare local variables

  integer(i_kind):: msig,mlat

!*******************************************************************************************
!
  if( fg_initialized_ ) return

! Allocate arrays to hold surface and sigma guess fields.
  call create_ges_grids(switch_on_derivatives,tendsflag)
  call create_bias_grids()
  call create_sfc_grids

! Read model guess fields.
  call read_guess(mype)

! Set length of control vector and other control vector constants
  call set_pointer

! Allocate arrays used in minimization
  call berror_get_dims(msig,mlat)  ! _RT: observer should not depend on B
  call create_jfunc(mlat)

! Intialize lagrangian data assimilation and read in initial position of balloons
  call lag_guessini()
 
! Read output from previous min.
  if (l4dvar.and.jiterstart>ione) then
  else
  ! If requested and if available, read guess solution.
  endif

  fg_initialized_ = .true.

! End of routine
end subroutine guess_init_


subroutine init_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    observer_init      driver for gridpoint statistical 
!                                     interpolation
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: executes gridpoint spectral interpolation analysis.
!
! program history log:
!   1990-10-06  parrish
!   2007-10-03  todling - created this file from slipt of glbsoi
!   2009-01-28  todling - split observer into init/set/run/finalize
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare passed variables

! Declare local variables


!*******************************************************************************************
!
! Initialize timer for this procedure
  call timer_ini('observer')

! Initialize guess
  call guess_init_

!     ndata(*,1)- number of prefiles retained for further processing
!     ndata(*,2)- number of observations read
!     ndata(*,3)- number of observations keep after read
  allocate(ndata(ndat,3))


! End of routine
end subroutine init_

subroutine set_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    observer_set       driver for gridpoint statistical 
!                                     interpolation
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: executes gridpoint spectral interpolation analysis.
!
! program history log:
!   1990-10-06  parrish
!   2007-10-03  todling - created this file from slipt of glbsoi
!   2009-01-28  todling - split observer into init/set/run/finalize
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare passed variables

! Declare local variables

!*******************************************************************************************
 
  if ( iamset_ ) return

! Create file names for pe relative observation data.  obs_setup files are used
! in outer loop setup routines. 
   obs_setup = trim(dirname) // 'obs_setup'

! Read observations
  call read_obs(ndata,mype)
  
! Locate observations within each subdomain (not to call in 4dvar inner loop)
  if(lobserver.or.(.not.l4dvar)) call obs_para(ndata,mype)

  iamset_ = .true.

! End of routine
end subroutine set_

subroutine run_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    observer_run       driver for gridpoint statistical 
!                                     interpolation
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: executes gridpoint spectral interpolation analysis.
!
! program history log:
!   1990-10-06  parrish
!   2007-10-03  todling - created this file from slipt of glbsoi
!   2009-01-28  todling - split observer into init/set/run/finalize
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare passed variables

! Declare local variables

  integer(i_kind) jiterlast
  logical :: last
  character(len=12) :: clfile

!*******************************************************************************************
 
! Read observations and scatter
  call set_

  jiterlast=miter
  if (l4dvar) then
    jiterlast=jiterstart
  else
    write(6,*)'observer should only be called in 4dvar'
    call stop2(157)
  endif
  if (mype==izero) write(6,*)'OBSERVER: jiterstart,jiterlast=',jiterstart,jiterlast

! Main outer analysis loop
  do jiter=jiterstart,jiterlast

!   Set up right hand side of analysis equation
    call setuprhsall(ndata,mype)

    last  = jiter == miter+ione
    if (l4dvar.and.(.not.last)) then
       clfile='obsdiags.ZZZ'
       write(clfile(10:12),'(I3.3)') jiter
       call write_obsdiags(clfile)
    endif

! End of outer iteration loop
  end do

  if (.not.l4dvar) then
    jiter=miter+ione
!   If requested, write obs-anl information to output files
    if (write_diag(jiter)) call setuprhsall(ndata,mype)

  endif

! End of routine
end subroutine run_

subroutine final_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    observer_final     driver for gridpoint statistical 
!                                     interpolation
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: executes gridpoint spectral interpolation analysis.
!
! program history log:
!   1990-10-06  parrish
!   2007-10-03  todling - created this file from slipt of glbsoi
!   2009-01-28  todling - split observer into init/set/run/finalize
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  implicit none

! Declare passed variables

! Declare local variables

!*******************************************************************************************
 
  call guess_final_

! Deallocate arrays
  call convinfo_destroy

  deallocate(ndata)

! Finalize timer for this procedure
  call timer_fnl('observer')

! End of routine
end subroutine final_

subroutine guess_final_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    observer_final     driver for gridpoint statistical 
!                                     interpolation
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: executes gridpoint spectral interpolation analysis.
!
! program history log:
!   1990-10-06  parrish
!   2007-10-03  todling - created this file from slipt of glbsoi
!   2009-01-28  todling - split observer into init/set/run/finalize
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  implicit none

! Declare passed variables

! Declare local variables

!*******************************************************************************************
  if ( fg_finalized_ ) return
 
! Deallocate remaining arrays
  call destroy_sfct
  call destroy_ges_grids(switch_on_derivatives,tendsflag)
  call destroy_bias_grids()
  call destroy_jfunc
  call destroy_gesfinfo

  fg_finalized_ = .true.

! End of routine
end subroutine guess_final_

end module observermod
