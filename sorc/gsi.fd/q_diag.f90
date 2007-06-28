subroutine q_diag(mype)
!$$$  subroutine documentation block
!                .      .    .                                       .
! subprogram:    q_diag        get moisture diagnostics
!
!   prgmmr: kleist           org: np20                date: 2005-11-21
!
! abstract: compute statistics for negative and supersatured moisture points
!
! program history log:
!   2005-11-21  kleist
!
!   input argument list:
!     su       - mpi task id
!     guo/rt   - output count was local; changed to global
! 
!   output argument list
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use guess_grids, only: ges_q,ntguessig
  use jfunc, only: qsatg
  use mpimod, only: mpi_rtype,mpi_comm_world,mpi_sum
  use constants,only: zero,two
  use gridmod, only: lat2,lon2,nsig
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local variables
  real(r_kind):: qrms_neg,qrms_sat
  real(r_kind),dimension(2):: qrms,qcount,qrms0,qcount0
  integer(i_kind) it,i,j,k

  it=ntguessig

  qrms=zero
  qcount=zero

  do k=1,nsig
    do j=2,lon2-1
      do i=2,lat2-1
        if (ges_q(i,j,k,it).LT.zero) then
          qrms(1)=qrms(1) + ges_q(i,j,k,it)**two
          qcount(1)=qcount(1) + 1
        end if
        if (ges_q(i,j,k,it).GT.qsatg(i,j,k)) then
          qrms(2)=qrms(2) + (ges_q(i,j,k,it)-qsatg(i,j,k))**two
          qcount(2)=qcount(2) + 1
        end if
      end do
    end do
  end do

  call mpi_reduce(qrms,qrms0,2,mpi_rtype,mpi_sum,0,mpi_comm_world,i)
  call mpi_reduce(qcount,qcount0,2,mpi_rtype,mpi_sum,0,mpi_comm_world,i)

  if(mype.eq.0) then
     qrms_neg = zero
     qrms_sat = zero
     if(qcount0(1)>zero) qrms_neg=sqrt(qrms0(1)/qcount0(1))
     if(qcount0(2)>zero) qrms_sat=sqrt(qrms0(2)/qcount0(2))
     write(6,100) nint(qcount0(1)),qrms_neg,nint(qcount0(2)),qrms_sat
100  format(' Q_DIAG:  NEG Q COUNT,RMS=',i9,1x,g12.6,',   SUPERSAT Q COUNT,RMS=',i9,1x,g12.6)
  end if

  return
end subroutine q_diag
