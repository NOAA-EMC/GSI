module observer_enkf
use general_tll2xy_mod, only: llxy_cons

private
public init_observer_enkf, calc_linhx

type(llxy_cons) :: gt_data


contains

subroutine init_observer_enkf
  use kinds, only: r_kind, i_kind
  use params, only: nlons, nlats
  use gridinfo, only: latsgrd, lonsgrd
  use general_tll2xy_mod, only: general_create_llxy_transform
  implicit none

  integer(i_kind) :: i, j
  real(r_kind), dimension(nlats, nlons) :: lats, lons


  do i = 1,nlons
     do j = 1,nlats
        lats(j,i) = latsgrd((j-1)*nlons+i)
        lons(j,i) = lonsgrd((j-1)*nlons+i)
     enddo
  enddo
  call general_create_llxy_transform(lats, lons, nlats, nlons, gt_data)

end subroutine init_observer_enkf

subroutine calc_linhx(hx, dens, rlat, rlon, time, dhx_dx, hx_ens)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calc_linhx
!   prgmmr: shlyaeva         org: esrl/psd            date: 2016-11-29
!
! abstract: 
!
! program history log:
!   2016-11-29  shlyaeva
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f95
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use params, only: nstatefields, nlons, nlats, nlevs, nhr_state, fhr_assim
  use gridinfo, only: npts, latsgrd, lonsgrd
  use statevec, only: nsdim
  use constants, only: zero,one,pi
  use sparsearr, only: sparr
  use general_tll2xy_mod, only: general_tll2xy
  use mpisetup
  implicit none

! Declare passed variables
  real(r_single)                                   ,intent(in   ) :: hx           ! H(x_mean)
  real(r_single),dimension(npts,nsdim,nstatefields),intent(in   ) :: dens         ! x_ens - x_mean, state vector space
  real(r_single)                                   ,intent(in   ) :: rlat, rlon   ! observation lat and lon in radians
  real(r_single)                                   ,intent(in   ) :: time         ! observation time relative to middle of window
  type(sparr)                                      ,intent(in   ) :: dhx_dx       ! dH(x)/dx |x_mean profiles
  real(r_single)                                   ,intent(  out) :: hx_ens       ! H (x_ens)

! Declare local variables
  integer(i_kind) :: ix, iy, it, ixp, iyp, itp
  real(r_kind)    :: dx, dy
  integer(i_kind) :: i,j 
  real(r_kind)    :: delx, dely, delxp, delyp, delt, deltp
  logical :: outside

  call general_tll2xy(gt_data, real(rlon,r_kind), real(rlat,r_kind), dx, dy, outside)

  ix = max(1,min(int(dx),nlons))
  iy = max(1,min(int(dy),nlats))

  delx = max(zero, min(dx - float(ix), one))
  dely = max(zero, min(dy - float(iy), one))

  delxp = one - delx
  delyp = one - dely

  ixp = min(ix + 1, nlons)
  iyp = min(iy + 1, nlats)

  iy = iy - 1; iyp = iyp - 1

  it = 1
  do while (time + fhr_assim > nhr_state(it) .and. it < nstatefields)
    it = it + 1
  enddo
  itp = it
  it = max(1,itp-1)
  if (it /= itp) then
     delt = (time + fhr_assim - nhr_state(it)) / (nhr_state(itp) - nhr_state(it))
  else
     delt = one
  endif

  deltp = one - delt
  delxp = one - delx
  delyp = one - dely


  ! interpolate state horizontally and in time and do  dot product with dHx/dx profile
  ! saves from calculating interpolated x_ens for each state variable

  hx_ens = hx
  do i = 1, dhx_dx%nnz
     j = dhx_dx%ind(i)
     hx_ens = hx_ens + dhx_dx%val(i) *                              &
             (( dens( iy*nlons  + ix , j, it) *delyp*delxp          &
              + dens( iyp*nlons + ix , j, it) *dely *delxp          &
              + dens( iy*nlons  + ixp, j, it) *delyp*delx           &
              + dens( iyp*nlons + ixp, j, it) *dely *delx )*deltp   &
            + ( dens( iy*nlons  + ix , j, itp)*delyp*delxp          &
              + dens( iyp*nlons + ix , j, itp)*dely *delxp          &
              + dens( iy*nlons  + ixp, j, itp)*delyp*delx           &
              + dens( iyp*nlons + ixp, j, itp)*dely *delx )*delt)
  enddo

  return
end subroutine calc_linhx

end module observer_enkf
