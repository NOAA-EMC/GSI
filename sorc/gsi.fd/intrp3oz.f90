subroutine intrp3oz(f,g,dx,dy,dz,obstime,n,nlevs,mype,obstype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intrp3oz    space-time linear interpolation for ozone
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract:  This routine linearly interpolates the guess ozone fields 
!            horizontally in space and temporally in time.  The 
!            horizontal interpolation is bilinear.  Guess ozone values
!            are interpolated for both the layered ozone and total 
!            column ozone observations.
!
! program history log:
!   1990-10-11  parrish
!   1999-03-01  wu - port cray90 code to ibm-sp (mpi version)
!   2004-06-16  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2005-05-18  wu - add obstype for use of OMI total ozone
!   2005-09-23  derber - modify to handle total column cleaner
!   2005-12-23  treadon - remove unused nix* and niy* variables
!
!   input argument list:
!     f        - input interpolator (gridded guess ozone fields)
!     dx,dy,dz - input x,y,z-coords of interpolation points (grid units)
!     obstime  - observation times
!     n        - number of interpolatees
!     nlevs    - number of observational layers + 1 (the total column)
!     mype     - mpi task id
!
!   output argument list:
!     g        - output interpolatees (guess ozone at observation location)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!--------
  use kinds, only: r_kind,i_kind
  use guess_grids, only: nfldsig,hrdifsig
  use gridmod, only: lat2,lon2,nlat,nlon,nsig,lon1,istart,jstart
  use constants, only: zero, one
  implicit none

! Declare passed variables
  character(10), intent(in)        :: obstype  ! type of ozone obs
  integer(i_kind),intent(in):: n,mype,nlevs
  real(r_kind),dimension(n),intent(in):: dx,dy,obstime
  real(r_kind),dimension(nlevs-1,n),intent(in):: dz
  real(r_kind),dimension(lat2,lon2,nsig,nfldsig),intent(in):: f
  real(r_kind),dimension(nlevs,n),intent(out):: g

! Declare local variables
  integer(i_kind) i,j,k,ix,ix1,iy,iy1,kk,itsig,itsigp,iz1,iz2
  integer(i_kind) ixp,iyp,mm1
  real(r_kind) w00,w01,w10,w11,delx,dely,delx1,dely1
  real(r_kind) delz,dz1,dtsig,dtsigp,pob


!*************************************************************************
! Initialize variables
  g=zero
  mm1=mype+1


! Loop over number of observations.
  do i=1,n

!    Get horizontal interpolation information.  This information includes
!    the (i,j) indices of the grid points surrounding the observation,
!    plus the corresponding interpolation weights between these points
!    and the observation.

     ix1=dx(i); iy1=dy(i)
     ix1=max(1,min(ix1,nlat))
     delx=dx(i)-ix1; dely=dy(i)-iy1; delx=max(zero,min(delx,one))
     ix=ix1-istart(mm1)+2; iy=iy1-jstart(mm1)+2
     if(iy<1) then
        iy1=iy1+nlon
        iy=iy1-jstart(mm1)+2
     end if
     if(iy>lon1+1) then
        iy1=iy1-nlon
        iy=iy1-jstart(mm1)+2
     end if
     ixp=ix+1; iyp=iy+1
     if(ix1==nlat) then
        ixp=ix
     end if
     delx1=one-delx; dely1=one-dely
     w00=delx1*dely1; w10=delx*dely1; w01=delx1*dely; w11=delx*dely
        
!    Set the weights for linear time iterpolation from the guess to 
!    the observation time.

     if(obstime(i) > hrdifsig(1) .and. obstime(i) < hrdifsig(nfldsig))then
        do j=1,nfldsig-1
           if(obstime(i) > hrdifsig(j) .and. obstime(i) <= hrdifsig(j+1))then
              itsig=j
              itsigp=j+1
              dtsig=((hrdifsig(j+1)-obstime(i))/(hrdifsig(j+1)-hrdifsig(j)))
           end if
        end do
     else if(obstime(i) <=hrdifsig(1))then
        itsig=1
        itsigp=1
        dtsig=one
     else
        itsig=nfldsig
        itsigp=nfldsig
        dtsig=one
     end if
     dtsigp=one-dtsig
     

!    Given horizontal (spatial) and temporal interpolate weights, loop 
!    over the number of layered ozone observations at the given location

     dz1=nsig+1
     do k=1,nlevs-1
        pob = dz(k,i)
        iz1 = dz1
        if (iz1>nsig) iz1=nsig
        iz2 = pob
        do kk=iz1,iz2,-1
           delz=one
           if (kk==iz1) delz=dz1-iz1
           if (kk==iz2) delz=delz-pob+iz2
           g(k,i)=g(k,i) + &
                ((f(ix ,iy ,kk,itsig )*w00 &
                + f(ixp,iy ,kk,itsig )*w10 &
                + f(ix ,iyp,kk,itsig )*w01 &
                + f(ixp,iyp,kk,itsig )*w11)*delz)*dtsig + &
                ((f(ix ,iy ,kk,itsigp)*w00 &
                + f(ixp,iy ,kk,itsigp)*w10 &
                + f(ix ,iyp,kk,itsigp)*w01 &
                + f(ixp,iyp,kk,itsigp)*w11)*delz)*dtsigp  
        enddo
        dz1=pob
     enddo
!
!    Perform spatial and temporal interpolation for the total column 
!    ozone observation
     do kk=1,nsig
        g(nlevs,i)=g(nlevs,i) + &
             (f(ix ,iy ,kk,itsig )*w00 & 
             + f(ixp,iy ,kk,itsig )*w10 &
             + f(ix ,iyp,kk,itsig )*w01 &
             + f(ixp,iyp,kk,itsig )*w11)*dtsig + &
             (f(ix ,iy ,kk,itsigp)*w00 &
             + f(ixp,iy ,kk,itsigp)*w10 &
             + f(ix ,iyp,kk,itsigp)*w01 &
             + f(ixp,iyp,kk,itsigp)*w11)*dtsigp
     enddo

! End of loop over observations
  end do

! End of routine
  return
end subroutine intrp3oz
