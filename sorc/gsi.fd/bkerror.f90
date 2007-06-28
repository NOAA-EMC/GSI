subroutine bkerror(gradx,grady)
!-----------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bkerror  apply background error covariance            
!   prgmmr: wu               org: np22                date: 1999-12-07
!
! abstract: grid transform, apply recursive filters on conformal/Cartesian 
!            grids back to Gaussian grid.
!
! program history log:
!   2003-12-18  derber, j. bring hoper and htoper into background error calcs.
!   2004-05-15  treadon - move slndt,sst,sicet=0 up from htoper to this routine
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-08-27  kleist - background error covariance placed in single routine
!   2004-10-26  kleist - u,v removed from vector
!   2005-01-22  parrish - add module balmod to access balance and tbalance
!   2005-03-30  treadon - add more comments to periodic block
!
!   input argument list:
!     gradx    - input field  
!
!   output
!     grady    - background structure * gradx 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use berror, only: varprd
  use balmod, only: balance,tbalance
  use mpimod, only: levs_id,nvar_id
  use gridmod, only: lat2,lon2,nlat,nlon,nsig1o,periodic
  use jfunc, only: nvp,nst,np,nt,ncw,nsst,noz,nq,&
       nclen,nclen1,nrclen
  use constants, only:  zero,half,one,two,four
  implicit none

! Declare passed variables
  real(r_kind),dimension(nclen),intent(inout):: gradx
  real(r_kind),dimension(nclen),intent(out):: grady

! Declare local variables
  integer(i_kind) k,i,j,nnn,iflg
  real(r_kind),dimension(nlat,nlon,nsig1o):: work
  real(r_kind),dimension(lat2,lon2):: sst,slndt,sicet

!
!  nvar_id = 1 vorticity
!            2 divergence
!            3 surface pressure
!            4 temperature
!            5 specific humidity
!            6 ozone
!            7 sea surface temperature
!            8 cw
!            9 land skin temperature
!           10 ice temperature

! Determine how many vertical levels each mpi task will
! handle in the horizontal smoothing  
  nnn=0
  do k=1,nsig1o
     if (levs_id(k)/=0) nnn=nnn+1
  end do


! If dealing with periodic (sub)domain, gather full domain grids,
! account for periodicity, and redistribute to subdomains.  This
! only needs to be done when running with a single mpi task and
! then only for array gradx.
  if (periodic) then
     iflg=2
     do j=1,lon2
        do i=1,lat2
           slndt(i,j)=zero
           sicet(i,j)=zero
        end do
     end do
     call sub2grid(work,gradx(nt),gradx(np),gradx(nq),gradx(noz),gradx(nsst),&
          slndt,sicet,gradx(ncw),gradx(nst),gradx(nvp),iflg)
     call grid2sub(work,gradx(nt),gradx(np),gradx(nq),gradx(noz),gradx(nsst),&
          slndt,sicet,gradx(ncw),gradx(nst),gradx(nvp))
  endif

! Put things in grady first since operations change input variables
  do i=1,nclen
     grady(i)=gradx(i)
  end do

! Zero arrays for land, ocean, ice skin (surface) temperature.
  do j=1,lon2
     do i=1,lat2
        slndt(i,j)=zero
        sst(i,j)  =zero
        sicet(i,j)=zero
     end do
  end do

! Transpose of balance equation
  call tbalance(grady(nt),grady(np),grady(nq),grady(nst),grady(nvp))

! Apply variances, as well as vertical & horizontal parts of background error
  call bkgcov(grady(nst),grady(nvp),grady(nt),grady(np),grady(nq),&
       grady(noz),grady(nsst),sst,slndt,sicet,grady(ncw),nnn)

! Balance equation
  call balance(grady(nt),grady(np),grady(nq),grady(nst),grady(nvp))

! Take care of background error for bias correction terms
  do i=1,nrclen
     grady(i+nclen1)=grady(i+nclen1)*varprd(i)
  end do

  return
end subroutine bkerror
