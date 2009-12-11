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
!   sub qmpl_allreduce0
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
MODULE PROCEDURE rmpl_allreduce,qmpl_allreduce0,qmpl_allreduce2d
END INTERFACE

INTERFACE mpl_allgather
MODULE PROCEDURE mpl_allgatherq
END INTERFACE

contains

subroutine rmpl_allreduce(klen,pvals)
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
  use constants, only: izero,ione
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,npe
  implicit none

! Declare passed variables
  integer(i_kind),intent(in) :: klen
  real(r_kind),intent(inout) :: pvals(klen)

! Declare local variables
  integer(i_kind) :: ii,jj
  real(r_kind)    :: zwork(klen,npe)

! ----------------------------------------------------------

  if (npe>ione .and. klen>izero) then

!   Gather contributions
    call mpi_allgather(pvals,klen,mpi_rtype, &
                     & zwork,klen,mpi_rtype, mpi_comm_world,ierror)

!   Reproducible sum
    DO ii=1,klen
      pvals(ii)=zwork(ii,1)
    ENDDO
    DO jj=2,npe
      DO ii=1,klen
        pvals(ii)=pvals(ii)+zwork(ii,jj)
      ENDDO
    ENDDO

  endif

! ----------------------------------------------------------
return
end subroutine rmpl_allreduce
! ----------------------------------------------------------
subroutine qmpl_allreduce0(klen,pvals)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    qmpl_allreduce0
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
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,npe
  use constants, only: izero,ione,zero,zero_quad
  implicit none

! Declare passed variables
  integer(i_kind),intent(in) :: klen
  real(r_quad),intent(inout) :: pvals(klen)

! Declare local variables
  integer(i_kind) :: ii,jj
  real(r_kind)    :: zwork1(klen,2),zwork2(klen,npe,2)

! ----------------------------------------------------------

  if (npe>ione .and. klen>izero) then

    zwork1=zero
    do ii=1,klen
      zwork1(ii,1)=pvals(ii)
      zwork1(ii,2)=pvals(ii)-zwork1(ii,1)
    end do

!   Gather contributions
    call mpi_allgather(zwork1(1,  1),klen,mpi_rtype, &
                     & zwork2(1,1,1),klen,mpi_rtype, mpi_comm_world,ierror)
    call mpi_allgather(zwork1(1,  2),klen,mpi_rtype, &
                     & zwork2(1,1,2),klen,mpi_rtype, mpi_comm_world,ierror)

!   Reproducible sum
    DO ii=1,klen
      pvals(ii)=zero_quad
    ENDDO
    DO jj=1,npe
      DO ii=1,klen
        pvals(ii)=pvals(ii)+zwork2(ii,jj,1)+zwork2(ii,jj,2)
      ENDDO
    ENDDO

  endif

! ----------------------------------------------------------
return
end subroutine qmpl_allreduce0
! ----------------------------------------------------------------------
subroutine qmpl_allreduce1d(klen,pvals)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    qmpl_allreduce1d
!   prgmmr:
!
! abstract: Reproducible (across different pe's) all reduce
!
! program history log:
!   2008-12-09  todling - embed Derber's reproducible sum in subroutine
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
  use constants, only: ione, zero_quad, zero
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,mype,npe,mpi_sum
  implicit none

! Declare passed variables
  integer(i_kind),intent(in) :: klen
  real(r_quad),intent(inout) :: pvals(klen)

! Declare local variables
  integer(i_kind) :: ii,jj,mp1
  real(r_kind)    :: zwork1(klen,npe,2),zwork2(klen,npe,2)

! ----------------------------------------------------------

!  Break quad precision number into two double precision numbers
!  on each processor for mpi_allreduce

  mp1=mype+ione
  zwork1=zero
  do ii=1,klen
    zwork1(ii,mp1,1)=pvals(ii)
    zwork1(ii,mp1,2)=pvals(ii)-zwork1(ii,mp1,1)
  end do

! Reduce now

  call mpi_allreduce(zwork1,zwork2,2*npe*klen,mpi_rtype,mpi_sum,&
       mpi_comm_world,ierror)
  
!  Combine double precision numbers together and sum into quad precision number
    
  pvals=zero_quad
  do jj=1,npe
    do ii=1,klen
      pvals(ii)=pvals(ii)+zwork2(ii,jj,1)+zwork2(ii,jj,2)
    end do
  end do

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
  use constants, only: ione, zero_quad, zero
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,mype,npe,mpi_sum
  implicit none

! Declare passed variables
  integer(i_kind),intent(in) :: ilen,klen
  real(r_quad),intent(inout) :: pvals(ilen,klen)
  real(r_quad),optional,intent(out) :: pvnew(ilen,klen)

! Declare local variables
  integer(i_kind) :: ii,kk,nn,mp1
  real(r_kind)    :: zwork1(ilen,klen,npe,2),zwork2(ilen,klen,npe,2)

! ----------------------------------------------------------

!  Break quad precision number into two double precision numbers
!  on each processor for mpi_allreduce

  mp1=mype+ione
  zwork1=zero
  do kk=1,klen
    do ii=1,ilen
      zwork1(ii,kk,mp1,1)=pvals(ii,kk)
      zwork1(ii,kk,mp1,2)=pvals(ii,kk)-zwork1(ii,kk,mp1,1)
    end do
  end do

! Reduce now

  call mpi_allreduce(zwork1,zwork2,2*npe*ilen*klen,mpi_rtype,mpi_sum,&
       mpi_comm_world,ierror)
  
!  Combine double precision numbers together and sum into quad precision number
    
  if (present(pvnew)) then

    pvnew=zero_quad
    do nn=1,npe
      do kk=1,klen
        do ii=1,ilen
          pvnew(ii,kk)=pvnew(ii,kk)+zwork2(ii,kk,nn,1)+zwork2(ii,kk,nn,2)
        end do
      end do
    end do

  else

    pvals=zero_quad
    do nn=1,npe
      do kk=1,klen
        do ii=1,ilen
          pvals(ii,kk)=pvals(ii,kk)+zwork2(ii,kk,nn,1)+zwork2(ii,kk,nn,2)
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
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,npe
  use constants, only: zero,zero_quad
  implicit none

  integer(i_kind),intent(in   ) :: idim,jdim
  real(r_quad)   ,intent(in   ) :: zloc(idim)
  real(r_quad)   ,intent(  out) :: zall(idim,jdim)

  integer(i_kind) :: i,j
  real(r_kind) :: z1(idim,2),z2(idim,jdim,2)

  if(jdim/=npe) then
    write(6,*)'state_vectors: troubled jdim/npe',jdim,npe
    call stop2(153)
  end if

! break up quad precision number into 2 double precision numbers
  z1=zero
  do i=1,idim
      z1(i,1)=zloc(i)
      z1(i,2)=zloc(i)-z1(i,1)
  end do

  call mpi_allgather(z1(1,1)  ,idim,mpi_rtype, &
                     z2(1,1,1),idim,mpi_rtype, mpi_comm_world,ierror)
  call mpi_allgather(z1(1,2)  ,idim,mpi_rtype, &
                     z2(1,1,2),idim,mpi_rtype, mpi_comm_world,ierror)

! reintegrate quad precision number
  do i=1,idim
      zall(i,1)=zero_quad
  end do
  do j=1,jdim
    do i=1,idim
        zall(i,j)=zall(i,j)+z2(i,j,1)+z2(i,j,2)
    end do
  end do

end subroutine mpl_allgatherq
! ----------------------------------------------------------------------
end module mpl_allreducemod
