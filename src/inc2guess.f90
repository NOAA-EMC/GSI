subroutine inc2guess(sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inc2guess          replaces background with increment
!   prgmmr: todling          org: np22                date: 2007-07-05
!
! abstract:  This routine replaces the background fields with the
!            increments. Its main purpose is to be used in the 4d-var
!            case, though it could be considered for the 3d-var case 
!            as well.
!
!            As it is, this routine assumes a call to update_guess 
!            preceeds this and changes sval appropriately, including
!            change the scales of ozone and calculating vor and div.
!
! program history log:
!
!   input argument list:
!     sval     - analysis increment in grid space
!
!   output argument list:
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: mype
  use gridmod, only: lat2,lon2,nsig
  use guess_grids, only: ges_div,ges_vor,ges_ps,ges_cwmr,ges_tv,ges_q,&
       ges_oz,ges_u,ges_v,nfldsig,hrdifsig,&
       nfldsfc,sfct
  use gsi_4dvar, only: nobs_bins, hr_obsbin
  use xhat_vordivmod, only: xhat_vor,xhat_div
  use state_vectors

  implicit none

! Declare passed variables
  type(state_vector), intent(in) :: sval(nobs_bins)

! Declare local variables
  integer(i_kind) i,j,k,it,ij,ijk,ii
  real(r_kind) :: zt

!*******************************************************************************

! Overwrite guess fields by increments
  do it=1,nfldsig
     if (nobs_bins>1) then
       zt = hrdifsig(it)
       ii = NINT(zt/hr_obsbin)+1
     else
       ii = 1
     endif
     ijk=0
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              ijk=ijk+1
              ges_u(i,j,k,it)    = sval(ii)%u(ijk)
              ges_v(i,j,k,it)    = sval(ii)%v(ijk)
              ges_tv(i,j,k,it)   = sval(ii)%t(ijk)
              ges_q(i,j,k,it)    = sval(ii)%q(ijk)
              ges_oz(i,j,k,it)   = sval(ii)%oz(ijk)
              ges_cwmr(i,j,k,it) = sval(ii)%cw(ijk)
              ges_div(i,j,k,it)  = xhat_div(i,j,k,ii)
              ges_vor(i,j,k,it)  = xhat_vor(i,j,k,ii)
           end do
        end do
     end do
     ij=0
     do j=1,lon2
        do i=1,lat2
           ij=ij+1
           ges_ps(i,j,it) = sval(ii)%p(ij)
        end do
     end do
  end do

  do k=1,nfldsfc
     ij=0
     do j=1,lon2
        do i=1,lat2
           ij=ij+1
           sfct(i,j,k)= sval(ii)%sst(ij)
        end do
     end do
  end do
  if(mype==0) write(6,*) 'inc2guess: overwriting guess with increment'

  return
end subroutine inc2guess
