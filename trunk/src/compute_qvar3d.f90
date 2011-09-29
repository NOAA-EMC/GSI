subroutine compute_qvar3d

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    compute_qvar3d
!   prgmmr: zhu               org: np22                date: 2010-03-15
!
! abstract: compute rhgues and qvar3d
!
! program history log:
! 2010-03-15 zhu - extracted out from compute_derived
! 2010-04-10 parrish - make rhgues local, since removed from jfunc by derber (no longer used)
! 2010-05-28  todling - obtain variable id's on the fly (add getindex)
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use berror, only: dssv
  use jfunc, only: qsatg,qgues,varq,qoption
  use control_vectors, only: cvars3d
  use gridmod, only: lat2,lon2,nsig
  use constants, only: izero,one,ione,fv
  use guess_grids, only: fact_tv,ges_q,ntguessig,nfldsig,ges_tsen,ges_prsl
  use mpeu_util, only: getindex

  implicit none

! Declare local variables
  logical ice
  integer(i_kind) :: i,j,k,it,n,np,iderivative,nrf3_q
  real(r_kind) d,dn1,dn2
  real(r_kind),allocatable,dimension(:,:,:):: rhgues

  nrf3_q=getindex(cvars3d,'q')

! Limit q to be >= 1.e-10_r_kind
  do it=1,nfldsig
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              ges_q(i,j,k,it)=max(ges_q(i,j,k,it),1.e-10_r_kind)
           end do
        end do
     end do
  end do

! Load guess q.  Initialize saturation array to guess.
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           qgues(i,j,k)=ges_q(i,j,k,ntguessig) ! q guess
           qsatg(i,j,k)=ges_q(i,j,k,ntguessig) ! q guess
           fact_tv(i,j,k)=one/(one+fv*qsatg(i,j,k))      ! factor for tv to tsen conversion
        end do
     end do
  end do

! Compute saturation specific humidity.  Set up normalization factor
! for limq routines (1/qs*2)
  iderivative = izero
  if(qoption == ione)then
      iderivative = ione
  else
      iderivative = 2
  end if

  ice=.true.
  call genqsat(qsatg,ges_tsen(1,1,1,ntguessig),ges_prsl(1,1,1,ntguessig),lat2,lon2,nsig,ice,iderivative)
  allocate(rhgues(lat2,lon2,nsig))

  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           rhgues(i,j,k)=qgues(i,j,k)/qsatg(i,j,k)
        end do
     end do
  end do

  if (qoption==2) then
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              d=20.0_r_kind*rhgues(i,j,k) + one
              n=int(d)
              np=n+ione
              dn2=d-float(n)
              dn1=one-dn2
              n=min0(max(ione,n),25_i_kind)
              np=min0(max(ione,np),25_i_kind)
              dssv(i,j,k,nrf3_q)=(varq(n,k)*dn1 + varq(np,k)*dn2)*dssv(i,j,k,nrf3_q)
           end do
        end do
     end do
  end if
  deallocate(rhgues)

end subroutine compute_qvar3d

