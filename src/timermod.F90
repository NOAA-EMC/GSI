module timermod

!$$$ module documentation block
!           .      .    .                                       .
! module:   timermod
!  prgmmr: todling          org: gmao                date: 2007-10-01
!
! abstract: module providing interface to timing procedures
!
! program history log:
!   2007-10-01  todling
!   2009-02-26  todling - if-def from GMAO_FVGSI to GEOS_PERT
!   2009-08-13  lueken - update documentation
!
! subroutines included:
!   sub init_
!   sub final_
!   sub flush_
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only : i_kind
#ifdef GEOS_PERT
use m_zeit, only : zeit_ci
use m_zeit, only : zeit_co
use m_zeit, only : zeit_flush
#endif /* GEOS_PERT */

implicit none

private

public timer_ini
public timer_fnl
public timer_pri

interface timer_ini; module procedure &
          init_
end interface
interface timer_fnl; module procedure &
          final_
end interface
interface timer_pri; module procedure &
          flush_
end interface

contains

subroutine init_ (str)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_       initialize procedure timing
!
!   prgmmr: todling          org: gmao                date: 2007-10-01
!
! abstract: initializes timer
!
! program history log:
!   2007-10-01  todling
!
!   input argument list:
!     str - string designation for process to be timed
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none
character(len=*) str
#ifdef GEOS_PERT
call zeit_ci(str)
#endif /* GEOS_PERT */
end subroutine init_

subroutine final_ (str)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    final_       finalize procedure timing
!
!   prgmmr: todling          org: gmao                date: 2007-10-01
!
! abstract: finalize timer
!
! program history log:
!   2007-10-01  todling
!
!   input argument list:
!     str - string designation for process timed
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none
character(len=*) str
#ifdef GEOS_PERT
call zeit_co(str)
#endif /* GEOS_PERT */
end subroutine final_

subroutine flush_ (lu)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    flush_       summarizes timing results
!
!   prgmmr: todling          org: gmao                date: 2007-10-01
!
! abstract: summary of timing results
!
! program history log:
!   2007-10-01  todling
!
!   input argument list:
!     str - string designation for process timed
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none
integer(i_kind) :: lu
#ifdef GEOS_PERT
call zeit_flush(lu)
#endif /* GEOS_PERT */
end subroutine flush_

end module timermod
