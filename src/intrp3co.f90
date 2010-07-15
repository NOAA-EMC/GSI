subroutine intrp3co(f,ga,dx,dy,dz,obstime,n,nlevs,ap,ak,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intrp3co    space-time linear interpolation for ozone
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract:  This routine linearly interpolates the guess co fields 
!            horizontally in space and temporally in time.  The 
!            horizontal interpolation is bilinear. This version
!            for MOPITT v4 CO interpolates vertically to the averaging
!            kernel pressure levels as well. 
!            
!            The averaging kernel is also applied here, but it may be 
!            better to move it elsewhere later
!            
!
! program history log:
!   1990-10-11  parrish
!   1999-03-01  wu - port cray90 code to ibm-sp (mpi version)
!   2004-06-16  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2005-05-18  wu - add obstype for use of OMI total ozone
!   2005-09-23  derber - modify to handle total column cleaner
!   2005-12-23  treadon - remove unused nix* and niy* variables
!   2007-05-30  h.liu - include unit conversion with interpolation weights
!   2010-04-06  tangborn - copied from interp3oz to handle MOPITT CO with ave. kernel
!
!   input argument list:
!     f        - input interpolator (gridded guess ozone fields)
!     dx,dy,dz - input x,y,z-coords of interpolation points (grid units)
!     obstime  - observation times
!     n        - number of interpolatees
!     nlevs    - number of observational layers + 1 (the total column)
!     ap       - apriori profile
!     ak(j,k)  - averaging kernel: First index is the profile level. 
!     mype     - mpi task id
!
!   output argument list:
!     ga        - output guess co profile at observation location with averaging kernel applied)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!--------
  use kinds, only: r_kind,i_kind
  use guess_grids, only: nfldsig,hrdifsig,ges_prsi
  use gridmod, only: lat2,lon2,nlat,nlon,nsig,lon1,istart,jstart
  use constants, only: ione, zero, one, cocon 
  implicit none

! Declare passed variables
  integer(i_kind)                               ,intent(in   ) :: n,mype,nlevs
  real(r_kind),dimension(n)                     ,intent(in   ) :: dx,dy,obstime
  real(r_kind),dimension(nlevs-ione,n)          ,intent(in   ) :: dz
  real(r_kind),dimension(lat2,lon2,nsig,nfldsig),intent(in   ) :: f
  real(r_kind),dimension(nlevs)                 ,intent(in   ) :: ap
  real(r_kind),dimension(nlevs,nlevs)           ,intent(in   ) :: ak
  real(r_kind),dimension(nlevs,n)               ,intent(  out) :: ga

! Declare local variables
  integer(i_kind) i,j,k,ix,ix1,iy,iy1,kk,itsig,itsigp,iz1,iz2
  integer(i_kind) ixp,iyp,mm1
  integer(i_kind) k1,k2
  real(r_kind) w00,w01,w10,w11,delx,dely,delx1,dely1
  real(r_kind) delz,dz1,dtsig,dtsigp,pob
  real(r_kind) delp1,delp2,delp3,delp4,delp5,delp6,delp7,delp8
  real(r_kind) wk1,wk2
  real(r_kind) rsum
  real(r_kind),dimension(nlevs) :: g
  logical,parameter::debug=.false.


!*************************************************************************
! Initialize variables
  g=zero
  mm1=mype+ione


! Loop over number of observations.
  do i=1,n

!    Get horizontal interpolation information.  This information includes
!    the (i,j) indices of the grid points surrounding the observation,
!    plus the corresponding interpolation weights between these points
!    and the observation.

     ix1=dx(i); iy1=dy(i)
     ix1=max(ione,min(ix1,nlat))
     delx=dx(i)-ix1; dely=dy(i)-iy1; delx=max(zero,min(delx,one))
     ix=ix1-istart(mm1)+2_i_kind; iy=iy1-jstart(mm1)+2_i_kind
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
     delx1=one-delx; dely1=one-dely
     w00=delx1*dely1; w10=delx*dely1; w01=delx1*dely; w11=delx*dely
        
!    Set the weights for linear time iterpolation from the guess to 
!    the observation time.

     if(obstime(i) > hrdifsig(1) .and. obstime(i) < hrdifsig(nfldsig))then
        do j=1,nfldsig-ione
           if(obstime(i) > hrdifsig(j) .and. obstime(i) <= hrdifsig(j+ione))then
              itsig=j
              itsigp=j+ione
              dtsig=((hrdifsig(j+ione)-obstime(i))/(hrdifsig(j+ione)-hrdifsig(j)))
           end if
        end do
     else if(obstime(i) <=hrdifsig(1))then
        itsig=ione
        itsigp=ione
        dtsig=one
     else
        itsig=nfldsig
        itsigp=nfldsig
        dtsig=one
     end if
     dtsigp=one-dtsig
     
!    Given horizontal (spatial) and temporal interpolate weights,  and
!    vertical weights defined below, interpolate to the obs lat,lon,press
!    and time.

     dz1=nsig+ione
     do k=1,nlevs
        pob = dz(k,i)
        k1=int(pob)
        k2=min(k1+1,nsig)
        wk1=one-(pob-real(k1))/(real(k2)-real(k1))
        wk2=    (pob-real(k1))/(real(k2)-real(k1))
        
           g(k)=&
                ((f(ix ,iy ,k1,itsig )*w00*cocon &
                + f(ixp,iy ,k1,itsig )*w10*cocon &
                + f(ix ,iyp,k1,itsig )*w01*cocon &
                + f(ixp,iyp,k1,itsig )*w11*cocon)*wk1 &
                +(f(ix ,iy ,k2,itsig )*w00*cocon & 
                + f(ixp,iy ,k2,itsig )*w10*cocon &
                + f(ix ,iyp,k2,itsig )*w01*cocon & 
                + f(ixp,iyp,k2,itsig )*w11*cocon)*wk2)*dtsig + &
                ((f(ix ,iy ,k1,itsigp)*w00*cocon &
                + f(ixp,iy ,k1,itsigp)*w10*cocon &
                + f(ix ,iyp,k1,itsigp)*w01*cocon &
                + f(ixp,iyp,k1,itsigp)*w11*cocon)*wk1 &
                +(f(ix, iy ,k2,itsigp)*w00*cocon &
                + f(ixp,iy ,k2,itsigp)*w10*cocon & 
                + f(ix ,iyp,k2,itsigp)*w01*cocon & 
                + f(ixp,iyp,k2,itsigp)*w11*cocon)*wk2)*dtsigp  
     enddo


!  Apply averaging kernel 

    do k=1,nlevs 
       rsum=zero
       if(debug) print*,'k=',k
       do j=1,nlevs 
          rsum=rsum+ak(k,j)*(log10(g(j))-log10(ap(j)))
       enddo 
       if(debug) print*,'rsum=',rsum
       rsum=rsum+log10(ap(k))
       ga(k,i)=10.0**rsum   
       if(debug) print*,'ga=',ga(k,i)
    enddo 
          

! End of loop over observations
  end do

! End of routine
  return
end subroutine intrp3co
