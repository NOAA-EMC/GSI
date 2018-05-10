module observer_enkf

private
public init_observer_enkf, calc_linhx

contains

subroutine init_observer_enkf
   return
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
  use params, only: nstatefields, nlons, nlats, nhr_state, fhr_assim
  use gridinfo, only: npts, latsgrd, lonsgrd
  use statevec, only: nsdim
  use constants, only: zero,one,pi
  use sparsearr, only: sparr
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
  integer(i_kind) :: i,j 
  real(r_kind)    :: delx, dely, delxp, delyp, delt, deltp

  ! find interplation indices and deltas
  ix = 0
  do while (latsgrd(ix*nlons+1) >= rlat)
    ix = ix + 1
    if (ix == nlats-1) exit
  enddo
  ix  = min(ix,   nlats-1)
  ixp = max(ix-1, 0)

  if (ixp /= ix) then
     delx = (rlat - latsgrd(ix*nlons+1)) / (latsgrd(ixp*nlons + 1) - latsgrd(ix*nlons+1))
  else
     delx = one
  endif
  delx = max(zero,min(delx,one))

  iyp = 1
  do while (iyp <= nlons .and. lonsgrd(ix*nlons + iyp) <= rlon)
    iyp = iyp + 1
  enddo
  iy = iyp - 1
  if(iy < 1)     iy = iy + nlons
  if(iyp > nlons) iyp = iyp - nlons
  if(iy > nlons) iy = iy - nlons

  if (iy /= nlons) then
     dely = (rlon - lonsgrd(ix*nlons + iy)) / (lonsgrd(ix*nlons + iyp) - lonsgrd(ix*nlons + iy))
  else
     dely = (rlon - lonsgrd(ix*nlons + iy)) / (lonsgrd(ix*nlons + iyp) + 2*pi - lonsgrd(ix*nlons + iy))
  endif

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
             (( dens( ix*nlons  + iy , j, it) *delxp*delyp          &
              + dens( ixp*nlons + iy , j, it) *delx *delyp          &
              + dens( ix*nlons  + iyp, j, it) *delxp*dely           &
              + dens( ixp*nlons + iyp, j, it) *delx *dely )*deltp   &
            + ( dens( ix*nlons  + iy , j, itp)*delxp*delyp          &
              + dens( ixp*nlons + iy , j, itp)*delx *delyp          &
              + dens( ix*nlons  + iyp, j, itp)*delxp*dely           &
              + dens( ixp*nlons + iyp, j, itp)*delx *dely )*delt)
  enddo

  return
end subroutine calc_linhx

end module observer_enkf
