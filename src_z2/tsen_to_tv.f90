subroutine tsen_to_tv(tsen,q,tv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tsen_to_tv  tlm for sensible to virtual temp
!   prgmmr: eliu               org: np20                date: 2012-02-09
!
! abstract: get sensible temperature from virtual temperature
!
! program history log:
!   2012-02-09  eliu 
!
!   input argument list:
!      tsen   - sensible temperature
!      q      - specific humidity
!
!   output argument list:
!      tv     - virtual temperature
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig,twodvar_regional
  use constants, only: fv,one
  use guess_grids, only: ges_tsen,fact_tv,ntguessig
  use jfunc, only: tsensible,qgues

  implicit none

  real(r_kind),intent(in   ) :: tsen(lat2,lon2,nsig)
  real(r_kind),intent(in   ) ::    q(lat2,lon2,nsig)
  real(r_kind),intent(  out) ::   tv(lat2,lon2,nsig)

! local arrays
  integer(i_kind) i,j,k,it

  it=ntguessig

! Convert normalized tv to tsen
  if (twodvar_regional .and. tsensible) then
     tv=tsen
  else
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              tv(i,j,k) = tsen(i,j,k)*(one+fv*qgues(i,j,k))+ges_tsen(i,j,k,it)*fv*q(i,j,k) 
           end do
        end do
     end do
  end if

end subroutine tsen_to_tv

subroutine tsen_to_tv_ad(tsen,q,tv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tsen_to_tv_ad  adjoint of tsen_to_tv
!   prgmmr: eliu               org: np20                date: 2012-02-09
!
! abstract: adjoint of tsen_to_tv
!
! program history log:
!   2012-02-09  eliu 
!
!   input argument list:
!      tv     - virtual temperature
!      q      - specific humidity
!      tsen   - sensible temperature
!
!   output argument list:
!      tv     - virtual temperature
!      q      - specific humidity
!      tsen   - sensible temperature
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig,twodvar_regional
  use constants, only: fv,zero,one
  use guess_grids, only: ges_tsen,fact_tv,ntguessig
  use jfunc, only: tsensible,qgues

  implicit none

  real(r_kind),intent(inout) :: tv(lat2,lon2,nsig)
  real(r_kind),intent(inout) :: q (lat2,lon2,nsig)
  real(r_kind),intent(inout) :: tsen(lat2,lon2,nsig)

! local variables:
  integer(i_kind) i,j,k,it

  it=ntguessig

! Adjoint of convert tv to t sensible
  if (twodvar_regional .and. tsensible) then
     tsen=tsen+tv
  else
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
!!            tv(i,j,k)   = tsen(i,j,k)*(one+fv*qgues(i,j,k))+ges_tsen(i,j,k,it)*fv*q(i,j,k) ! TL 
              tsen(i,j,k) = tsen(i,j,k)+(one+fv*qgues(i,j,k))*tv(i,j,k)                      ! AD
              q(i,j,k)    = q(i,j,k)+ges_tsen(i,j,k,it)*fv*tv(i,j,k)                         ! AD
              tv(i,j,k)   = zero                                                             ! AD
           end do
        end do
     end do
  end if

end subroutine tsen_to_tv_ad
