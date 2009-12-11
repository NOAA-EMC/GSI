subroutine abor1(error_msg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    abor1
!   prgmmr: tremolet
!
! abstract:  Print error message and abort execution of mpi code.
!
! program history log:
!   2007-04-13  tremolet - initial code
!
!   input argument list:
!     error_msg - Error message
!     ierror_code - Error code
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: i_kind
use mpimod, only: mpi_comm_world,ierror
implicit none

character(len=*), intent(in) :: error_msg

integer(i_kind) :: ierr

ierr=100_i_kind

write(6,*)'ABOR1 called: ',error_msg
write(0,*)'ABOR1 called: ',error_msg

call flush(6)
call flush(0)

call system("sleep 1")

call mpi_abort(mpi_comm_world,ierr,ierror)

stop
return
end subroutine abor1
