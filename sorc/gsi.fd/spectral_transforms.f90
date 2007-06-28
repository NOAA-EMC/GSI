subroutine g2s0(spectral_out,grid_in)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g2s0        grid to spectral
!   prgmmr: kleist           org: np23                date: 2006-07-15
!
! abstract: transform scalar from gaussian grid to spherical harmonic coefficients
!
! program history log:
!   2006-07-15  kleist
!
!   input argument list:
!     grid_in  - input grid field on gaussian grid
!
!   output argument list:
!     spectral_out - output spherical harmonic coefficients
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use specmod, only: nc,factsml
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: nlat,nlon
  implicit none

  real(r_kind),intent(out)::spectral_out(nc)
  real(r_kind),intent(in)::grid_in(nlat,nlon)

  real(r_kind) work(nlon,nlat-2),spec_work(nc)
  integer(i_kind) i,j,jj

!  Transfer contents of input grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
  do j=2,nlat-1
    jj=nlat-j
    do i=1,nlon
      work(i,jj)=grid_in(j,i)
    end do
  end do
  call sptez_s(spec_work,work,-1)

  do i=1,nc
    spectral_out(i)=factsml(i)*spec_work(i)
  end do
 
  return
end subroutine g2s0

subroutine g2s0_ad(spectral_in,grid_out)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g2s0_ad     adjoint of g2s0
!   prgmmr: kleist           org: np23                date: 2006-07-15
!
! abstract: adjoint of g2s0
!
! program history log:
!   2006-07-15  kleist
!
!   input argument list:
!     spectral_in  - input spherical harmonic coefficients
!
!   output argument list:
!     grid_out - output grid field on gaussian grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use specmod, only: jcap,nc,factsml,wlat,jb,je
  use kinds, only: r_kind,i_kind
  use constants, only: zero,half
  use gridmod, only: nlat,nlon
  implicit none

  integer(i_kind) norm
  real(r_kind),intent(in)::spectral_in(nc)
  real(r_kind),intent(out)::grid_out(nlat,nlon)

  real(r_kind) work(nlon,nlat-2),spec_work(nc)
  integer(i_kind) i,j,jj

  do i=1,nc
    spec_work(i)=factsml(i)*spectral_in(i)/float(nlon)
  end do
  do i=2*jcap+3,nc
    spec_work(i)=half*spec_work(i)
  end do
 
  call sptez_s(spec_work,work,1)

  do j=jb,je
    do i=1,nlon
      work(i,j)=work(i,j)*wlat(j)
      work(i,nlat-1-j)=work(i,nlat-1-j)*wlat(j)
    end do
  end do

!  Transfer contents of output grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
  do j=2,nlat-1
    jj=nlat-j
    do i=1,nlon
      grid_out(j,i)=work(i,jj)
    end do
  end do

!  Load zero into north-south rows
  do i=1,nlon
    grid_out(1,i)   =zero
    grid_out(nlat,i)=zero
  end do

  return
end subroutine g2s0_ad

subroutine s2g0(spectral_in,grid_out)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    s2g0        inverse of g2s0
!   prgmmr: kleist           org: np23                date: 2006-07-15
!
! abstract: inverse of g2s0
!
! program history log:
!   2006-07-15  kleist
!
!   input argument list:
!     spectral_in  - input spherical harmonic coefficients
!
!   output argument list:
!     grid_out - output grid field on gaussian grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use specmod, only: nc,factsml,wlat,jb,je
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: nlat,nlon
  implicit none

  real(r_kind),intent(in)::spectral_in(nc)
  real(r_kind),intent(out)::grid_out(nlat,nlon)

  real(r_kind) work(nlon,nlat-2),spec_work(nc)
  integer(i_kind) i,ix,j,jj,ny
  real(r_kind) poln,pols
  real(r_kind),dimension(nlon):: grid3n,grid3s
  real(r_kind),dimension(nlat-2,nlon):: grid3

  ny=nlat-2

  do i=1,nc
    spec_work(i)=factsml(i)*spectral_in(i)
  end do
 
  call sptez_s(spec_work,work,1)

!  Reverse ordering in j direction from n-->s to s-->n
  do j=2,nlat-1
    jj=nlat-j
    do i=1,nlon
      grid3(j-1,i)=work(i,jj)
    end do
  end do

!  fill in pole points

  poln=zero
  pols=zero
  do ix=1,nlon
    poln=poln+grid3(ny,ix)
    pols=pols+grid3(1 ,ix)
  end do
  poln=poln/float(nlon)
  pols=pols/float(nlon)
  do ix=1,nlon
    grid3n(ix)=poln
    grid3s(ix)=pols
  end do
  do j=1,nlon
    grid_out(1,j)=grid3s(j)
    grid_out(nlat,j)=grid3n(j)
    do i=1,ny
      grid_out(i+1,j)=grid3(i,j)
    end do
  end do

  return
end subroutine s2g0

subroutine s2g0_ad(spectral_out,grid_in)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    s2g0_ad     adjoint of s2g0
!   prgmmr: kleist           org: np23                date: 2006-07-15
!
! abstract: adjoint of s2g0
!
! program history log:
!   2006-07-15  kleist
!
!   input argument list:
!     grid_in  - input spherical harmonic coefficients
!
!   output argument list:
!     spectral_out - output grid field on gaussian grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use specmod, only: jcap,nc,factsml,wlat,jb,je
  use kinds, only: r_kind,i_kind
  use constants, only: zero,two
  use gridmod, only: nlat,nlon
  implicit none

  real(r_kind),intent(out)::spectral_out(nc)
  real(r_kind),intent(in)::grid_in(nlat,nlon)

  real(r_kind) work(nlon,nlat-2),spec_work(nc)
  integer(i_kind) i,ix,j,jj,ny
  real(r_kind) poln,pols
  real(r_kind),dimension(nlon):: grid3n,grid3s
  real(r_kind),dimension(nlat-2,nlon):: grid3

  ny=nlat-2
  do j=1,nlon
    grid3s(j)=grid_in(1,j)
    grid3n(j)=grid_in(nlat,j)
    do i=1,ny
      grid3(i,j)=grid_in(i+1,j)
    end do
  end do
  poln=zero
  pols=zero
  do ix=1,nlon
    poln=poln+grid3n(ix)
    pols=pols+grid3s(ix)
  end do
  poln=poln/float(nlon)
  pols=pols/float(nlon)
  do ix=1,nlon
    grid3(ny,ix)=grid3(ny,ix)+poln
    grid3(1 ,ix)=grid3(1 ,ix)+pols
  end do

!  Transfer contents of input grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
  do j=2,nlat-1
    jj=nlat-j
    do i=1,nlon
      work(i,jj)=grid3(j-1,i)
    end do
  end do

  do j=jb,je
    do i=1,nlon
      work(i,j)=work(i,j)/wlat(j)
      work(i,nlat-1-j)=work(i,nlat-1-j)/wlat(j)
    end do
  end do

  call sptez_s(spec_work,work,-1)

  do i=1,nc
    spec_work(i)=spec_work(i)*float(nlon)
  end do
  do i=2*jcap+3,nc
    spec_work(i)=two*spec_work(i)
  end do

  do i=1,nc
    spectral_out(i)=factsml(i)*spec_work(i)
  end do

  return
end subroutine s2g0_ad


subroutine uvg2zds(zsp,dsp,ugrd,vgrd)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    uvg2zds     grid u,v to spectral vort, div
!   prgmmr: kleist           org: np23                date: 2006-07-15
!
! abstract: transform vector u,v from gaussian grid to spherical harmonic
!           coefficients of vorticity and divergence.
!
! program history log:
!   2006-07-15  kleist
!
!   input argument list:
!     ugrd  - input u on gaussian grid
!     vgrd  - input v on gaussian grid
!
!   output argument list:
!     zsp   - output spherical harmonic coefficients of vorticity
!     dsp   - output spherical harmonic coefficients of divergence
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use specmod, only: nc,factvml
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: nlat,nlon
  implicit none

! Passed variables
  real(r_kind),dimension(nlat,nlon),intent(in) :: ugrd,vgrd
  real(r_kind),dimension(nc),intent(out) :: zsp,dsp

! Local variables
  real(r_kind),dimension(nlon,nlat-2):: grdwrk1,grdwrk2 
  real(r_kind),dimension(nc):: spcwrk1,spcwrk2
  integer(i_kind) i,j,jj

! Transfer contents of input grid to local work array
! Reverse ordering in j direction from n-->s to s-->n
  do j=2,nlat-1
    jj=nlat-j
    do i=1,nlon
      grdwrk1(i,jj)=ugrd(j,i)
      grdwrk2(i,jj)=vgrd(j,i)
    end do
  end do

  call sptez_v(spcwrk1,spcwrk2,grdwrk1,grdwrk2,-1)

  do i=1,nc
    zsp(i)=factvml(i)*spcwrk2(i)
    dsp(i)=factvml(i)*spcwrk1(i)
  end do

  return
end subroutine uvg2zds


subroutine uvg2zds_ad(zsp,dsp,ugrd,vgrd)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    uvg2zds_ad  adjoint of uvg2zds
!   prgmmr: kleist           org: np23                date: 2006-07-15
!
! abstract: adjoint of uvg2zds
!
! program history log:
!   2006-07-15  kleist
!
!   input argument list:
!     ugrd  - input u on gaussian grid
!     vgrd  - input v on gaussian grid
!     zsp   - input spherical harmonic coefficients of vorticity
!     dsp   - input spherical harmonic coefficients of divergence
!
!   output argument list:
!     ugrd  - output u on gaussian grid
!     vgrd  - output v on gaussian grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use specmod, only: nc,factvml,wlat,slat,clat,jb,je,jcap,ncd2,enn1
  use kinds, only: r_kind,i_kind
  use constants, only: zero,half,two
  use gridmod, only: nlat,nlon
  implicit none

! Passed variables
  real(r_kind),dimension(nlat,nlon),intent(inout) :: ugrd,vgrd
  real(r_kind),dimension(nc),intent(in) :: zsp,dsp

! Local variables
  real(r_kind),dimension(nlon,nlat-2):: grdwrk1,grdwrk2
  real(r_kind),dimension(nc):: spcwrk1,spcwrk2
  integer(i_kind) i,j,jj

  do i=1,nc
    spcwrk1(i)=factvml(i)*dsp(i)/float(nlon)
    spcwrk2(i)=factvml(i)*zsp(i)/float(nlon)
  end do

  do i=2*jcap+3,nc
     spcwrk1(i)=half*spcwrk1(i)
     spcwrk2(i)=half*spcwrk2(i)
  end do

  do i=2,ncd2
     spcwrk1(2*i-1)=spcwrk1(2*i-1)*enn1(i)
     spcwrk1(2*i)=spcwrk1(2*i)*enn1(i)
     spcwrk2(2*i-1)=spcwrk2(2*i-1)*enn1(i)
     spcwrk2(2*i)=spcwrk2(2*i)*enn1(i)
  end do

  call sptez_v(spcwrk1,spcwrk2,grdwrk1,grdwrk2,1)

  do j=jb,je
    do i=1,nlon
      grdwrk1(i,j)=grdwrk1(i,j)*wlat(j)
      grdwrk1(i,nlat-1-j)=grdwrk1(i,nlat-1-j)*wlat(j)
      grdwrk2(i,j)=grdwrk2(i,j)*wlat(j)
      grdwrk2(i,nlat-1-j)=grdwrk2(i,nlat-1-j)*wlat(j)
    end do
  end do

! Transfer contents of input grid to local work array
! Reverse ordering in j direction from n-->s to s-->n
  do j=2,nlat-1
    jj=nlat-j
    do i=1,nlon
      ugrd(j,i)=grdwrk1(i,jj)
      vgrd(j,i)=grdwrk2(i,jj)
    end do
  end do

  do i=1,nlon
    ugrd(1,i)    = zero
    ugrd(nlat,i) = zero
    vgrd(1,i)    = zero
    vgrd(nlat,i) = zero
  end do

  return
end subroutine uvg2zds_ad


subroutine zds2uvg(zsp,dsp,ugrd,vgrd)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    zds2uvg     inverse of uvg2zds
!   prgmmr: kleist           org: np23                date: 2006-07-15
!
! abstract: inverse of uvg2zds
!
! program history log:
!   2006-07-15  kleist
!
!   input argument list:
!     zsp   - input spherical harmonic coefficients of vorticity
!     dsp   - input spherical harmonic coefficients of divergence
!
!   output argument list:
!     ugrd  - output u on gaussian grid
!     vgrd  - output v on gaussian grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use specmod, only: nc,factvml
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: nlat,nlon,sinlon,coslon
  implicit none

! Passed variables
  real(r_kind),dimension(nc),intent(in):: zsp,dsp
  real(r_kind),dimension(nlat,nlon),intent(out) :: ugrd,vgrd

! Local variables
  real(r_kind),dimension(nlon,nlat-2):: grdwrk1,grdwrk2
  real(r_kind),dimension(nc):: spcwrk1,spcwrk2
  integer(i_kind) i,ix,j,jj,ny
  real(r_kind) polnu,polnv,polsu,polsv
  real(r_kind),dimension(nlon):: grid3n,grid3s,grid1n,grid1s
  real(r_kind),dimension(nlat-2,nlon):: grid1,grid3           ! u = grid3, v = grid1

  ny=nlat-2
  do i=1,nc
    spcwrk1(i)=factvml(i)*dsp(i)
    spcwrk2(i)=factvml(i)*zsp(i)
  end do

  call sptez_v(spcwrk1,spcwrk2,grdwrk1,grdwrk2,1)

! Reverse ordering in j direction from n-->s to s-->n
  do j=2,nlat-1
    jj=nlat-j
    do i=1,nlon
      grid3(j-1,i)=grdwrk1(i,jj)
      grid1(j-1,i)=grdwrk2(i,jj)
    end do
  end do

!  fill in pole points

  polnu=zero
  polnv=zero
  polsu=zero
  polsv=zero
  do ix=1,nlon
     polnu=polnu+grid3(ny,ix)*coslon(ix)-grid1(ny,ix)*sinlon(ix)
     polnv=polnv+grid3(ny,ix)*sinlon(ix)+grid1(ny,ix)*coslon(ix)
     polsu=polsu+grid3(1 ,ix)*coslon(ix)+grid1(1 ,ix)*sinlon(ix)
     polsv=polsv+grid3(1 ,ix)*sinlon(ix)+grid1(1 ,ix)*coslon(ix)
  end do
  polnu=polnu/float(nlon)
  polnv=polnv/float(nlon)
  polsu=polsu/float(nlon)
  polsv=polsv/float(nlon)
  do ix=1,nlon
     grid3n(ix)= polnu*coslon(ix)+polnv*sinlon(ix)
     grid1n(ix)=-polnu*sinlon(ix)+polnv*coslon(ix)
     grid3s(ix)= polsu*coslon(ix)+polsv*sinlon(ix)
     grid1s(ix)= polsu*sinlon(ix)+polsv*coslon(ix)
  end do
! work 1 is u, work2 is v
  do j=1,nlon
    ugrd(1,j)=grid3s(j)
    vgrd(1,j)=grid1s(j)
    ugrd(nlat,j)=grid3n(j)
    vgrd(nlat,j)=grid1n(j)
    do i=1,ny
        ugrd(i+1,j)=grid3(i,j)
        vgrd(i+1,j)=grid1(i,j)
    end do
  end do

  return
end subroutine zds2uvg

subroutine zds2uvg_ad(zsp,dsp,ugrd,vgrd)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    zds2uvg_ad  adjoint of zds2uvg
!   prgmmr: kleist           org: np23                date: 2006-07-15
!
! abstract: adjoint of zds2uvg
!
! program history log:
!   2006-07-15  kleist
!
!   input argument list:
!     zsp   - input spherical harmonic coefficients of vorticity
!     dsp   - input spherical harmonic coefficients of divergence
!     ugrd  - input u on gaussian grid
!     vgrd  - input v on gaussian grid
!
!   output argument list:
!     zsp   - output spherical harmonic coefficients of vorticity
!     dsp   - output spherical harmonic coefficients of divergence
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use specmod, only: nc,factvml,wlat,slat,clat,jb,je,jcap,ncd2,enn1
  use kinds, only: r_kind,i_kind
  use constants, only: zero,half,two
  use gridmod, only: nlat,nlon,sinlon,coslon
  implicit none

! Passed variables
  real(r_kind),dimension(nlat,nlon),intent(in) :: ugrd,vgrd
  real(r_kind),dimension(nc),intent(inout) :: zsp,dsp

! Local variables
  real(r_kind),dimension(nlon,nlat-2):: grdwrk1,grdwrk2
  real(r_kind),dimension(nc):: spcwrk1,spcwrk2
  integer(i_kind) i,ix,j,jj,ny
  real(r_kind) polsu,polsv,polnu,polnv
  real(r_kind),dimension(nlon):: grid3n,grid3s,grid1n,grid1s
  real(r_kind),dimension(nlat-2,nlon):: grid3,grid1

  ny=nlat-2
  do j=1,nlon
    grid3s(j)=ugrd(1,j)
    grid1s(j)=vgrd(1,j)
    grid3n(j)=ugrd(nlat,j)
    grid1n(j)=vgrd(nlat,j)
    do i=1,ny
        grid3(i,j)=ugrd(i+1,j)
        grid1(i,j)=vgrd(i+1,j)
    end do
  end do
  polnu=zero
  polsu=zero
  polnv=zero
  polsv=zero
  do ix=1,nlon
     polnu=polnu+grid3n(ix)*coslon(ix)-sinlon(ix)*grid1n(ix)
     polsu=polsu+grid3s(ix)*coslon(ix)+sinlon(ix)*grid1s(ix)
     polnv=polnv+grid3n(ix)*sinlon(ix)+coslon(ix)*grid1n(ix)
     polsv=polsv+grid3s(ix)*sinlon(ix)+coslon(ix)*grid1s(ix)
  end do
  polnu=polnu/float(nlon)
  polsu=polsu/float(nlon)
  polnv=polnv/float(nlon)
  polsv=polsv/float(nlon)

  do ix=1,nlon
     grid3(ny,ix)=grid3(ny,ix)+polnu*coslon(ix)+polnv*sinlon(ix)
     grid3(1,ix) =grid3(1,ix) +polsu*coslon(ix)+polsv*sinlon(ix)
     grid1(ny,ix)=grid1(ny,ix)-polnu*sinlon(ix)+polnv*coslon(ix)
     grid1(1,ix) =grid1(1,ix) +polsu*sinlon(ix)+polsv*coslon(ix)
  end do
! Transfer contents of input grid to local work array
! Reverse ordering in j direction from n-->s to s-->n
  do j=2,nlat-1
    jj=nlat-j
    do i=1,nlon
      grdwrk1(i,jj)=grid3(j-1,i)
      grdwrk2(i,jj)=grid1(j-1,i)
    end do
  end do

  do j=jb,je
    do i=1,nlon
      grdwrk1(i,j)=grdwrk1(i,j)/wlat(j)
      grdwrk1(i,nlat-1-j)=grdwrk1(i,nlat-1-j)/wlat(j)
      grdwrk2(i,j)=grdwrk2(i,j)/wlat(j)
      grdwrk2(i,nlat-1-j)=grdwrk2(i,nlat-1-j)/wlat(j)

    end do
  end do

  call sptez_v(spcwrk1,spcwrk2,grdwrk1,grdwrk2,-1)

  do i=2,ncd2
     spcwrk1(2*i-1)=spcwrk1(2*i-1)/enn1(i)
     spcwrk1(2*i)=spcwrk1(2*i)/enn1(i)
     spcwrk2(2*i-1)=spcwrk2(2*i-1)/enn1(i)
     spcwrk2(2*i)=spcwrk2(2*i)/enn1(i)
  end do

  do i=1,nc
     spcwrk1(i)=spcwrk1(i)*float(nlon)
     spcwrk2(i)=spcwrk2(i)*float(nlon)
  end do

  do i=2*jcap+3,nc
     spcwrk1(i)=two*spcwrk1(i)
     spcwrk2(i)=two*spcwrk2(i)
  end do

  do i=1,nc
    zsp(i)=factvml(i)*spcwrk2(i)
    dsp(i)=factvml(i)*spcwrk1(i)
  end do

  return
end subroutine zds2uvg_ad
