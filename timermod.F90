module timermod

!$$$  subprogram documentation block
!
!   module: timermod
!   prgmmr: todling          org: gmao                date: 2007-10-01
!
! abstract: module providing interface to timing procedures
!
! program history log:
!   2007-10-01  todling
!
!$$$

use kinds, only : i_kind
#ifdef _GMAO_FVGSI_
use m_zeit, only : zeit_ci
use m_zeit, only : zeit_co
use m_zeit, only : zeit_flush
#endif /* _GMAO_FVGSI_ */

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
!
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
!$$$

implicit none
character(len=*) str
#ifdef _GMAO_FVGSI_
call zeit_ci(str)
#endif /* _GMAO_FVGSI_ */
end subroutine init_

subroutine final_ (str)
!$$$  subprogram documentation block
!
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
!$$$

implicit none
character(len=*) str
#ifdef _GMAO_FVGSI_
call zeit_co(str)
#endif /* _GMAO_FVGSI_ */
end subroutine final_

subroutine flush_ (lu)
!$$$  subprogram documentation block
!
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
!$$$

implicit none
integer(i_kind) :: lu
#ifdef _GMAO_FVGSI_
call zeit_flush(lu)
#endif /* _GMAO_FVGSI_ */
end subroutine flush_

end module timermod
