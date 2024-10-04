module loc_advection

use mpisetup
use gridinfo, only: latsgrd,lonsgrd,nlevs_pres,npts
use statevec_efsoi, only: id_u,id_v
use loadbal_efsoi, only: numptsperproc, indxproc, kdtree_grid
use scatter_chunks_efsoi
use kinds
use params
use constants
use kdtree2_module
implicit none

private
public loc_advection_efsoi, adloc_chunk
real(r_single),allocatable,dimension(:,:) :: adloc_chunk

contains

subroutine loc_advection_efsoi
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    loc_advection
!   prgmmr: ota
!
! abstract: computes analysis grid point location with simple horizontal
!   advection.
!
! program history log:
!   2011-09-16  ota    - created
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  real(r_kind),allocatable,dimension(:) :: coslat
  real(r_kind) :: adtime, halfpi, pi2, adlon, adlat
  integer(i_kind) :: npt, nn, nnu, nnv, nnpt
  allocate(adloc_chunk(3,numptsperproc(nproc+1)*nlevs_pres))
  allocate(coslat(numptsperproc(nproc+1)))
  adtime = adrate*real(eft,r_kind)*3600._r_kind/rearth
  halfpi = half * pi
  pi2 = 2._r_kind * pi
!$omp parallel private(npt)
!$omp do
  do npt=1,numptsperproc(nproc+1)
     coslat(npt) = 1._r_kind/cos(latsgrd(indxproc(nproc+1,npt)))
  end do
!$omp end do
!$omp end parallel
!$omp parallel private(nn,nnu,nnv,npt,nnpt,adlon,adlat)
!$omp do
  do nn=1,nlevs_pres
     if(nn == nlevs_pres) then
        nnu = id_u(1)
        nnv = id_v(1)
     else
        nnu = id_u(nn)
        nnv = id_v(nn)
     end if
     do npt=1,numptsperproc(nproc+1)
        nnpt = nlevs_pres * (npt-1) + nn
        adlon = lonsgrd(indxproc(nproc+1,npt)) &
             & - half * (ensmean_chunk(npt,nnu) + analmean_chunk(npt,nnu)) &
             & * coslat(npt) * adtime
        adlat = latsgrd(indxproc(nproc+1,npt)) &
             & - half * (ensmean_chunk(npt,nnv) + analmean_chunk(npt,nnv)) &
             & * adtime
        if(adlat > halfpi) then
           adlat = pi - adlat
           adlon = adlon + pi
        else if(adlat < -halfpi) then
           adlat = -pi - adlat
           adlon = adlon + pi
        end if
        if(adlon > pi2) then
           adlon = mod(adlon,pi2)
        else if(adlon < zero) then
           adlon = mod(adlon,pi2) + pi2
        end if
        adloc_chunk(1,nnpt) = cos(adlat)*cos(adlon)
        adloc_chunk(2,nnpt) = cos(adlat)*sin(adlon)
        adloc_chunk(3,nnpt) = sin(adlat)
     end do
  end do
!$omp end do
!$omp end parallel
  deallocate(coslat)
  ! kd-tree
  kdtree_grid => kdtree2_create(adloc_chunk,sort=.false.,rearrange=.true.)
  return
end subroutine loc_advection_efsoi

end module loc_advection
