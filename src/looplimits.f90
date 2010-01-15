      subroutine looplimits(taskid, ntasks, lb, ub, i1, i2)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    looplimits
!
!   prgrmmr:
!
! abstract:     Block partition the loop bounds (lb...ub) -> (i1...i2).
!               The number of tasks is ntasks;  taskid = 0, 1, ..., ntasks-1.
!               The first nt1 tasks get a chunk one bigger than the rest.
!               The counts and displacements arrays range from 1 to ntasks.
!
! program history log:
!   2008-05-29  safford -- add subprogram doc block
!
!   input argument list:
!    taskid, ntasks, lb, ub
!
!   output argument list:
!    i1, i2
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

      use kinds, only: i_kind
      use constants, only: ione

      implicit none
 
      integer(i_kind),intent(in   ) :: taskid, ntasks, lb, ub
      integer(i_kind),intent(  out) :: i1, i2

      integer(i_kind) chunk, nwork, nt1, nt2
      integer(i_kind) itask, netdisp
      integer(i_kind) counts(ntasks), displacements(ntasks)

      nwork = ub - lb + ione
      chunk = nwork/ntasks
      nt1 = nwork - ntasks*chunk
      nt2 = ntasks - nt1

      netdisp = lb
      do itask = 1, nt1
         counts(itask) = chunk + ione
         displacements(itask) = netdisp  
         netdisp = min(ub,netdisp+chunk+ione)
      end do
      do itask = nt1 + ione , ntasks
         counts(itask) = chunk
         displacements(itask) = netdisp  
         netdisp = min(ub,netdisp+chunk)
      end do

      i1 = displacements(taskid+ione)
      i2 = min(ub,i1+counts(taskid+ione)-ione)

      return
      end subroutine looplimits
