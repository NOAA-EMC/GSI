subroutine intrp3(f,g,dx,dy,dz,n,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intrp3      linear interpolation in 3 dimensions
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: linear interpolate in 3 dims
!
! program history log:
!   1990-10-11  parrish
!   1998-04-05  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-05-18  kleist, documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!
!   input argument list:
!     f        - input interpolator
!     dx,dy,dz - input x,y,z-coords of interpolation points (grid units)
!     n        - number of interpolatees
!     mype     - mpi task id
!
!   output argument list:
!     g        - output interpolatees
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: istart,jstart,lon1,lon2,lat2,nlat,nsig,nlon
  use constants, only: zero,one
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: n,mype
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: f
  real(r_kind),dimension(n),intent(in):: dx,dy,dz
  real(r_kind),dimension(n),intent(out):: g

! Declare local variables
  integer(i_kind) mm1,i,ix1,iy1,iz,ix,iy,ixp,iyp,izp
  real(r_kind) delx,dely,delz,delzp,delyp,delxp


  mm1=mype+1
  do i=1,n
    ix1=int(dx(i))
    iy1=int(dy(i))
    iz=int(dz(i))
    ix1=max(1,min(ix1,nlat)); iz=max(1,min(iz,nsig))  
    delx=dx(i)-float(ix1)
    dely=dy(i)-float(iy1)
    delz=dz(i)-float(iz)
    delx=max(zero,min(delx,one)); delz=max(zero,min(delz,one))
    ix=ix1-istart(mm1)+2
    iy=iy1-jstart(mm1)+2
    if(iy<1) then
      iy1=iy1+nlon
      iy=iy1-jstart(mm1)+2
    end if
    if(iy>lon1+1) then
      iy1=iy1-nlon
      iy=iy1-jstart(mm1)+2
    end if
    ixp=ix+1; iyp=iy+1
    izp=min(iz+1,nsig)
    if(ix1==nlat) then
      ixp=ix
    end if
    delxp=one-delx; delyp=one-dely
    delzp=one-delz
    g(i)=f(ix,iy,iz)*delxp*delyp*delzp+f(ixp,iy,iz)*delx*delyp*delzp&
        +f(ix,iyp,iz)*delxp*dely*delzp+f(ixp,iyp,iz)*delx*dely*delzp&
        +f(ix,iy,izp)*delxp*delyp*delz+f(ixp,iy,izp)*delx*delyp*delz&
        +f(ix,iyp,izp)*delxp*dely*delz+f(ixp,iyp,izp)*delx*dely*delz
  end do

  return
end subroutine intrp3
