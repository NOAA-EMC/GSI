module mpl_allreducemod
!$$$ module documentation block
!           .      .    .                                       .
! module:   mpl_allreduce    reproducible sums
!   prgmmr:
!
! abstract: module for reproducible sums
!
! program history log:
!   2008-12-09 todling 
!   2009-01-17 todling - add allgather (quad)
!
! subroutines included:
!   sub rmpl_allreduce
!   sub qmpl_allreduce1d
!   sub qmpl_allreduce2d
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
implicit none

PRIVATE
PUBLIC mpl_allreduce
PUBLIC mpl_allgather

INTERFACE mpl_allreduce
MODULE PROCEDURE rmpl_allreduce,qmpl_allreduce1d,qmpl_allreduce2d
END INTERFACE

INTERFACE mpl_allgather
MODULE PROCEDURE mpl_allgatherq
END INTERFACE

contains

subroutine rmpl_allreduce(klen,rpvals)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rmpl_allreduce
!   prgmmr:
!
! abstract: Reproducible all reduce
!
! program history log:
!   2007-04-13  tremolet - initial code
!
!   input argument list:
!    klen  - length of array pvals
!    pvals - array of values to be reduced (overwritten)
!
!   output argument list:
!    pvals - array of values to be reduced (overwritten)
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,npe
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: klen
  real(r_kind)   ,intent(inout) :: rpvals(klen)

! Declare local variables
  integer(i_kind) :: ii,jj
  real(r_kind)    :: zwork(klen,npe)

! ----------------------------------------------------------

  if (npe>1 .and. klen>0) then

!    Gather contributions
     call mpi_allgather(rpvals,klen,mpi_rtype, &
                      & zwork,klen,mpi_rtype, mpi_comm_world,ierror)

!    Sum (note this is NOT reproducible across different numbers of processors)
     do ii=1,klen
        rpvals(ii)=zwork(ii,1)
     end do
     do jj=2,npe
        do ii=1,klen
           rpvals(ii)=rpvals(ii)+zwork(ii,jj)
        end do
     end do

  endif

! ----------------------------------------------------------
return
end subroutine rmpl_allreduce
! ----------------------------------------------------------
subroutine qmpl_allreduce1d(klen,qpvals)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    qmpl_allreduce1d
!   prgmmr:
!
! abstract: Reproducible all reduce
!
! program history log:
!   2007-04-13  tremolet - initial code
!
!   input argument list:
!    klen  - length of array pvals
!    pvals - array of values to be reduced (overwritten)
!
!   output argument list:
!    pvals - array of values to be reduced (overwritten)
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind,r_quad
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,npe,mpi_real16
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: klen
  real(r_quad)   ,intent(inout) :: qpvals(klen)

! Declare local variables
  integer(i_kind) :: ii,jj
  real(r_quad)    ::qpval2(klen,npe)

! ----------------------------------------------------------

  if (npe>1 .and. klen>0) then

!    Gather contributions
     call mpi_allgather(qpvals,klen,mpi_real16, &
                      & qpval2,klen,mpi_real16, mpi_comm_world,ierror)

!    Reproducible sum (when truncated to real precision)
     do ii=1,klen
        qpvals(ii)=qpval2(ii,1)
     end do
     do jj=2,npe
        do ii=1,klen
           qpvals(ii)=qpvals(ii)+qpval2(ii,jj)
        end do
     end do

  endif

! ----------------------------------------------------------
return
end subroutine qmpl_allreduce1d
! ----------------------------------------------------------
subroutine qmpl_allreduce2d(ilen,klen,pvals,pvnew)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    qmpl_allreduce2d
!   prgmmr:
!
! abstract: Reproducible (across different pe's) all reduce
!
! program history log:
!   2008-12-09  todling - embed Derber's reproducible sum in subroutine
!
!   input argument list:
!    ilen  - first dimension of array pvals
!    klen  - second dimension of array pvals
!    pvals - array of values to be reduced (overwritten)
!
!   output argument list:
!    pvals - array of values to be reduced (overwritten)
!    pvnew
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind,r_quad
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,mype,npe,mpi_real16
  implicit none

! Declare passed variables
  integer(i_kind)      ,intent(in   ) :: ilen,klen
  real(r_quad)         ,intent(inout) :: pvals(ilen,klen)
  real(r_quad),optional,intent(  out) :: pvnew(ilen,klen)

! Declare local variables
  integer(i_kind) :: ii,kk,nn
  real(r_quad)    :: pval2(ilen,klen,npe)

! ----------------------------------------------------------

! Gather contributions
  call mpi_allgather(pvals,ilen*klen,mpi_real16, &
                   & pval2,ilen*klen,mpi_real16, mpi_comm_world,ierror)
  
    
  if (present(pvnew)) then

     do kk=1,klen
       do ii=1,ilen
         pvnew(ii,kk)=pval2(ii,kk,1)
       end do
     end do
     do nn=2,npe
        do kk=1,klen
           do ii=1,ilen
              pvnew(ii,kk)=pvnew(ii,kk)+pval2(ii,kk,nn)
           end do
        end do
     end do

  else

     do kk=1,klen
       do ii=1,ilen
         pvals(ii,kk)=pval2(ii,kk,1)
       end do
     end do
     do nn=2,npe
        do kk=1,klen
           do ii=1,ilen
              pvals(ii,kk)=pvals(ii,kk)+pval2(ii,kk,nn)
           end do
        end do
     end do

  endif 

! ----------------------------------------------------------
return
end subroutine qmpl_allreduce2d

subroutine mpl_allgatherq(idim,jdim,zloc,zall)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mpl_allgatherq
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-11-04  lueken - added subprogram doc block
!
!   input argument list:
!    idim
!    jdim
!    zloc
!
!   output argument list:
!    zall
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: i_kind,r_kind,r_quad
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,npe,mpi_real16
  implicit none

  integer(i_kind),intent(in   ) :: idim,jdim
  real(r_quad)   ,intent(in   ) :: zloc(idim)
  real(r_quad)   ,intent(  out) :: zall(idim,jdim)

  if(jdim/=npe) then
     write(6,*)'state_vectors: troubled jdim/npe',jdim,npe
     call stop2(153)
  end if

  call mpi_allgather(zloc,idim,mpi_real16, &
                     zall,idim,mpi_real16, mpi_comm_world,ierror)


end subroutine mpl_allgatherq
! ----------------------------------------------------------------------
end module mpl_allreducemod
