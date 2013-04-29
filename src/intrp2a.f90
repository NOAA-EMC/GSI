subroutine intrp2a(f,g,dx,dy,n,nlevs,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intrp2a
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: linear interpolate in 2 dimensions over n levels
!
! program history log:
!   1990-10-11  parrish
!   1998-04-05  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2003-12-22  kleist, modified to perform 2-d interpolation over a 
!                       specified number of vertical levels
!   2004-05-17  kleist, documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2006-04-03  derber  - optimize
!   2008-05-31  safford - rm unused use
!
!   input argument list:
!     f        - input interpolator
!     dx,dy    - input x,y -coords of interpolation points (grid units)
!     n        - number of interpolatees in each pe subdomain
!     nlevs    - number of vertical levels over which to perform the
!                2-d intrpolation
!     mype     - mpi task id
!
!   output argument list:
!     g        - output interpolatees
!
! remarks: see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: istart,jstart,lon1,lat2,lon2,nlat,nlon
  use constants, only: ione,zero,one
  implicit none

! Declare passed variables
  integer(i_kind)                        ,intent(in   ) :: mype,n,nlevs
  real(r_kind),dimension(n)              ,intent(in   ) :: dx,dy
  real(r_kind),dimension(lat2,lon2,nlevs),intent(in   ) :: f
  real(r_kind),dimension(nlevs,n)        ,intent(  out) :: g

! Declare local variables
  integer(i_kind) mm1,k,i,ix1,iy1,ix,iy,ixp,iyp
  real(r_kind) delx,dely,delxp,delyp

  mm1=mype+ione
 
  do i=1,n
     ix1=int(dx(i))
     iy1=int(dy(i))
     ix1=max(ione,min(ix1,nlat))
     delx=dx(i)-float(ix1)
     dely=dy(i)-float(iy1)
     delx=max(zero,min(delx,one))
     ix=ix1-istart(mm1)+2_i_kind
     iy=iy1-jstart(mm1)+2_i_kind
     if(iy<ione) then
        iy1=iy1+nlon
        iy=iy1-jstart(mm1)+2_i_kind
     end if
     if(iy>lon1+ione) then
        iy1=iy1-nlon
        iy=iy1-jstart(mm1)+2_i_kind
     end if
     ixp=ix+ione; iyp=iy+ione
     if(ix1==nlat) then
        ixp=ix
     end if
     delxp=one-delx; delyp=one-dely
 
     do k=1,nlevs
        g(k,i)=f(ix,iy,k)*delxp*delyp+f(ixp,iy,k)*delx*delyp&
              +f(ix,iyp,k)*delxp*dely+f(ixp,iyp,k)*delx*dely

     end do
  end do

  return
end subroutine intrp2a
