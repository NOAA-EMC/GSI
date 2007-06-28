subroutine getprs(ps,ps_x,ps_y,prs,prs_x,prs_y)
! subprogram:    getprs       get 3d pressure or 3d pressure deriv
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: calculate 3d pressure and its horizontal derivatives
!
! program history log:
!   2005-09-29  kleist
!   2006-04-12  treadon - replace sigi with bk5
!   2006-07-31  kleist  - analysis variable change from ln(ps) to ps
!
! usage:
!   input argument list:
!     ps       - surface pressure
!     ps_x     - dps/dx
!     ps_y     - dps/dy
!
!   output argument list:
!     prs        - 3d pressure
!     prs_x      - dp/dx
!     prs_y      - dp/dy
!
!$$$
  use kinds,only: r_kind,i_kind
  use constants,only: zero,one_tenth
  use gridmod,only: nsig,lat2,lon2,ak5,bk5
  use gridmod,only: wrf_nmm_regional,eta1_ll,eta2_ll,pdtop_ll,pt_ll
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2),intent(in):: ps,ps_x,ps_y
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(out):: prs,prs_x,prs_y

! Declare local variables
  integer(i_kind) i,j,k

!   Declare local parameter
  real(r_kind),parameter:: ten = 10.0_r_kind

  prs=zero ; prs_x=zero ; prs_y=zero

  if(wrf_nmm_regional) then
    do k=1,nsig+1
      do j=1,lon2
        do i=1,lat2
          prs(i,j,k)=one_tenth* &
                           (eta1_ll(k)*pdtop_ll + &
                            eta2_ll(k)*(ten*ps(i,j)-pdtop_ll-pt_ll) + &
                            pt_ll)
          prs_x(i,j,k)=eta2_ll(k)*ps_x(i,j)
          prs_y(i,j,k)=eta2_ll(k)*ps_y(i,j)
        end do
      end do
    end do
  else
    do k=1,nsig+1
      do j=1,lon2
        do i=1,lat2
          prs(i,j,k)=ak5(k)+bk5(k)*ps(i,j)
          prs_x(i,j,k)=bk5(k)*ps_x(i,j)
          prs_y(i,j,k)=bk5(k)*ps_y(i,j)
        end do
      end do
    end do
  end if

  return
end subroutine getprs

subroutine getprs_tl(ps,ps_x,ps_y,prs,prs_x,prs_y)
!
! subprogram:    getprs_tl    TLM of getprs
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: TLM of routine that gets 3d pressure and its derivatives
!
! program history log:
!   2005-09-29  kleist
!   2006-04-12  treadon - replace sigi with bk5
!   2006-07-31  kleist  - analysis variable change from ln(ps) to ps
!
! usage:
!   input argument list:
!     ps       - surface pressure
!     ps_x     - dps/dx
!     ps_y     - dps/dy
!
!   output argument list:
!     prs        - 3d pressure
!     prs_x      - dp/dx
!     prs_y      - dp/dy
!
!
!$$$
  use kinds,only: r_kind,i_kind
  use constants,only: zero
  use gridmod,only: nsig,lat2,lon2,ak5,bk5
  use gridmod,only: wrf_nmm_regional,eta2_ll
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2),intent(in):: ps,ps_x,ps_y
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(out):: prs,prs_x,prs_y

! Declare local variables
  integer(i_kind) i,j,k

  prs=zero ; prs_x=zero ; prs_y=zero

  if(wrf_nmm_regional) then
    do k=1,nsig+1
      do j=1,lon2
        do i=1,lat2
          prs(i,j,k)=eta2_ll(k)*ps(i,j)
          prs_x(i,j,k)=eta2_ll(k)*ps_x(i,j)
          prs_y(i,j,k)=eta2_ll(k)*ps_y(i,j)
        end do
      end do
    end do
  else
    do k=1,nsig+1
      do j=1,lon2
        do i=1,lat2
          prs(i,j,k)=bk5(k)*ps(i,j)
          prs_x(i,j,k)=bk5(k)*ps_x(i,j)
          prs_y(i,j,k)=bk5(k)*ps_y(i,j)
        end do
      end do
    end do
  end if

  return
end subroutine getprs_tl

subroutine getprs_ad(ps,ps_x,ps_y,prs,prs_x,prs_y)
!
! subprogram:    getprs_ad    adjoint of getprs_tl
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: adjoint of linear routine that gets 3d pressure and derivs
!
! program history log:
!   2005-09-29  kleist
!   2006-04-12  treadon - replace sigi with bk5
!   2006-07-31  kleist  - analysis variable change from ln(ps) to ps
!
! usage:
!   input argument list:
!     ps       - surface pressure
!     ps_x     - dps/dx
!     ps_y     - dps/dy
!     prs        - 3d pressure
!     prs_x      - dp/dx
!     prs_y      - dp/dy
!
!   output argument list:
!     ps       - log surface pressure
!     ps_x     - d(ln(ps))/dx
!     ps_y     - d(ln(ps))/dy
!
!  notes:
!     Adjoint check performed and verified on 2005-08-29 by d. kleist
!
!$$$
  use kinds,only: r_kind,i_kind
  use gridmod,only: nsig,lat2,lon2,ak5,bk5
  use gridmod,only: wrf_nmm_regional,eta2_ll
  use constants,only: zero
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in):: prs,prs_x,prs_y
  real(r_kind),dimension(lat2,lon2),intent(inout):: ps,ps_x,ps_y

! Declare local variables
  integer(i_kind) i,j,k

  if(wrf_nmm_regional) then
    do k=1,nsig+1
      do j=1,lon2
        do i=1,lat2
          ps_y(i,j) = ps_y(i,j) + eta2_ll(k)*prs_y(i,j,k)
          ps_x(i,j) = ps_x(i,j) + eta2_ll(k)*prs_x(i,j,k)
          ps(i,j) = ps(i,j) + eta2_ll(k)*prs(i,j,k)
        end do
      end do
    end do
  else
    do k=1,nsig+1
      do j=1,lon2
        do i=1,lat2
          ps_y(i,j) = ps_y(i,j) + bk5(k)*prs_y(i,j,k)
          ps_x(i,j) = ps_x(i,j) + bk5(k)*prs_x(i,j,k)
          ps(i,j) = ps(i,j) + bk5(k)*prs(i,j,k)
        end do
      end do
    end do
  end if
 
  return
end subroutine getprs_ad
