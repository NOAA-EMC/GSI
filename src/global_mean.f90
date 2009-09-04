subroutine global_mean(psfc,psave,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    global_mean    calculate weighted global mean
!   prgmmr: kleist           org: np23                date: 2009-07-07
!
! abstract: compute weighted global mean of a two dimensional field
!
! program history log:
!   2009-07-07  kleist
!
!   input argument list:
!     psfc     - 2d field on subdomains
!     mype     - integer PE
!
!   output argument list:
!     psave    - (weighted) global mean of field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon,lat2,lon2,wgtlats
  use constants, only: zero,one,two
  implicit none

  real(r_kind),dimension(lat2,lon2),intent(in):: psfc
  real(r_kind),intent(out):: psave
  integer(i_kind),intent(in):: mype

  real(r_kind),dimension(nlat,nlon):: psg
  real(r_kind) r_npts
  integer(i_kind) i,j,wrkpe

  psave=zero
  r_npts=one/(two*float(nlon))
  wrkpe=0
 
  call gather_stuff2(psfc,psg,mype,wrkpe)

  if (mype==wrkpe) then
    do i=1,nlat
      do j=1,nlon
        psave = psave + psg(i,j)*wgtlats(i)
      end do
    end do
    psave=psave*r_npts
  end if

  return
end subroutine global_mean

subroutine global_mean_ad(psfc,psave,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    global_mean_ad   adjoint of global mean calculation
!   prgmmr: kleist           org: np23                date: 2009-07-07
!
! abstract: compute weighted global mean of a two dimensional field
!
! program history log:
!   2009-07-07  kleist
!
!   input argument list:
!     psfc     - 2d field on subdomains
!     psave    - (weighted) global mean of field
!     mype     - integer PE
!
!   output argument list:
!     psave    - (weighted) global mean of field
!     psfc     - 2d field on subdomains
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon,lat2,lon2,wgtlats
  use constants, only: zero,one,two
  implicit none

  real(r_kind),dimension(lat2,lon2),intent(inout):: psfc
  real(r_kind),intent(inout):: psave
  integer(i_kind),intent(in):: mype

  real(r_kind),dimension(nlat,nlon):: psg
  real(r_kind),dimension(lat2,lon2):: pstmp
  real(r_kind) r_npts
  integer(i_kind) i,j,wrkpe

  r_npts=one/(two*float(nlon))
  pstmp=zero
  wrkpe=0

  if (mype==wrkpe) then
    psave = psave*r_npts
    do j=1,nlon
      do i=1,nlat
        psg(i,j) = psave*wgtlats(i)
      end do
    end do
  end if

  call scatter_stuff2(psg,pstmp,mype,wrkpe)

  do j=2,lon2-1
    do i=2,lat2-1
      psfc(i,j)=psfc(i,j) + pstmp(i,j)
    end do
  end do

  return
end subroutine global_mean_ad
