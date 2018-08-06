module observer_enkf
use statevec, only: nsdim, ns3d, ns2d, slevels
use params, only: nlevs, neigv

private
public init_observer_enkf, setup_linhx, calc_linhx, calc_linhx_modens,&
       destroy_observer_enkf
integer, allocatable, dimension(:) ::  kindx

contains

subroutine init_observer_enkf
   integer nn,n,k,nl
   allocate(kindx(nsdim))
   nn = 0
   do n=1,ns3d
     if (n .eq. 1) then
       nl = slevels(n)
     else
       nl = slevels(n)-slevels(n-1)
     endif
     !print *,'ns3d,levs',n,nl
     do k=1,nl
        nn = nn + 1
        kindx(nn) = k
        ! FIXME - deal with state variables with nlevs+1 levels (like prse)
        if (kindx(nn) > nlevs) kindx(nn)=nlevs
     enddo
   enddo
   do n=1,ns2d ! 2d fields are treated as surface fields.
     nn = nn + 1
     kindx(nn) = 1
   enddo
   return
end subroutine init_observer_enkf

subroutine destroy_observer_enkf
 if (allocated(kindx)) deallocate(kindx)
end subroutine destroy_observer_enkf

subroutine setup_linhx(rlat, rlon, time, ix, delx, ixp, delxp, iy, dely,  &
                       iyp, delyp, it, delt, itp, deltp)
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
  use mpisetup
  implicit none

! Declare passed variables
  real(r_single)                                   ,intent(in   ) :: rlat, rlon   ! observation lat and lon in radians
  real(r_single)                                   ,intent(in   ) :: time         ! observation time relative to middle of window
  integer(i_kind), intent(out) :: ix, iy, it, ixp, iyp, itp
  real(r_kind), intent(out) :: delx, dely, delxp, delyp, delt, deltp


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

end subroutine setup_linhx

subroutine calc_linhx(hx, dens, dhx_dx, hx_ens, &
                      ix, delx, ixp, delxp, iy, dely, iyp, delyp, &
                      it, delt, itp, deltp)
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
  use mpisetup
  implicit none

! Declare passed variables
  real(r_single)                                   ,intent(in   ) :: hx           ! H(x_mean)
  real(r_single),dimension(npts,nsdim,nstatefields),intent(in   ) :: dens         ! x_ens - x_mean, state vector space
  integer(i_kind), intent(in) :: ix, iy, it, ixp, iyp, itp
  real(r_kind), intent(in) :: delx, dely, delxp, delyp, delt, deltp
  type(sparr)                                      ,intent(in   ) :: dhx_dx       ! dH(x)/dx |x_mean profiles
  real(r_single)                                   ,intent(  out) :: hx_ens       ! H (x_ens)
  integer(i_kind) i,j,k

  ! interpolate state horizontally and in time and do  dot product with dHx/dx profile
  ! saves from calculating interpolated x_ens for each state variable
  hx_ens = hx
  do i = 1, dhx_dx%nnz
     j = dhx_dx%ind(i)
     k = kindx(j)
     hx_ens = hx_ens + dhx_dx%val(i) *                                        &
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

subroutine calc_linhx_modens(hx, dens, dhx_dx, hx_ens, &
                      ix, delx, ixp, delxp, iy, dely, iyp, delyp, &
                      it, delt, itp, deltp, vscale)
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
  use mpisetup
  implicit none

! Declare passed variables
  real(r_single)                                   ,intent(in   ) :: hx           ! H(x_mean)
  real(r_single),dimension(npts,nsdim,nstatefields),intent(in   ) :: dens         ! x_ens - x_mean, state vector space
  integer(i_kind), intent(in) :: ix, iy, it, ixp, iyp, itp
  real(r_kind), intent(in) :: delx, dely, delxp, delyp, delt, deltp
  type(sparr)                                      ,intent(in   ) :: dhx_dx       ! dH(x)/dx |x_mean profiles
  real(r_single)                                   ,intent(  out) :: hx_ens(neigv)! H (x_ens)
  real(r_double),dimension(neigv,nlevs+1)          ,intent(in   ) :: vscale       ! vertical scaling (for modulated ens)
  integer(i_kind) i,j,k

  ! interpolate state horizontally and in time and do  dot product with dHx/dx profile
  ! saves from calculating interpolated x_ens for each state variable
  hx_ens = hx
  do i = 1, dhx_dx%nnz
     j = dhx_dx%ind(i)
     k = kindx(j)
     hx_ens(:) = hx_ens(:) + dhx_dx%val(i) *                                    &
             (( dens( ix*nlons  + iy , j, it) *vscale(:,k)*delxp*delyp          &
              + dens( ixp*nlons + iy , j, it) *vscale(:,k)*delx *delyp          &
              + dens( ix*nlons  + iyp, j, it) *vscale(:,k)*delxp*dely           &
              + dens( ixp*nlons + iyp, j, it) *vscale(:,k)*delx *dely )*deltp   &
            + ( dens( ix*nlons  + iy , j, itp)*vscale(:,k)*delxp*delyp          &
              + dens( ixp*nlons + iy , j, itp)*vscale(:,k)*delx *delyp          &
              + dens( ix*nlons  + iyp, j, itp)*vscale(:,k)*delxp*dely           &
              + dens( ixp*nlons + iyp, j, itp)*vscale(:,k)*delx *dely )*delt)
  enddo

  return
end subroutine calc_linhx_modens

end module observer_enkf
