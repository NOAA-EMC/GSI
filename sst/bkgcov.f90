subroutine bkgcov(st,vp,t,p,q,oz,skint,cwmr,nlevs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bkgcov    perform hor & vert of background error 
!   prgmmr: kleist         org: np22                date: 2004-07-22
!
! abstract: perform horizontal and vertical parts of background error
!
! program history log:
!   2004-07-22  kleist
!   2004-10-26  kleist - remove u,v
!   2004-11-03  treadon - move definition of horizontal scale weighting 
!                         factors to namelist
!   2004-11-22  derber - add openMP
!   2008-06-05  safford - rm unused vars
!
!   input argument list:
!     t        - t on subdomain
!     p        - p surface pressure on subdomain
!     q        - q on subdomain
!     oz       - ozone on subdomain
!     skint    - skin temperature on subdomain
!     cwmr     - cloud water mixing ratio on subdomain
!     st       - streamfunction on subdomain
!     vp       - velocity potential on subdomain
!     nlevs    - number of vertical levels for smoothing
!
!   output argument list:
!                 all after smoothing, combining scales
!     t        - t on subdomain
!     p        - p surface pressure on subdomain
!     q        - q on subdomain
!     oz       - ozone on subdomain
!     skint    - skin temperature on subdomain
!     cwmr     - cloud water mixing ratio on subdomain
!     st       - streamfunction on subdomain
!     vp       - velocity potential on subdomain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: nlat,nlon,lat2,lon2,nsig,nnnn1o
  implicit none

! Passed Variables
  integer(i_kind),intent(in):: nlevs
  real(r_kind),dimension(lat2,lon2),intent(inout):: p,skint
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: t,q,cwmr,oz,st,vp

! Local Variables
  integer(i_kind) i,j,k,nsloop,iflg
  real(r_kind),dimension(lat2,lon2):: sst,slndt,sicet
  real(r_kind),dimension(nlat,nlon,nnnn1o):: hwork

  nsloop=3
  iflg=1

  do j=1,lon2
    do i=1,lat2
      sst(i,j)=zero
      slndt(i,j)=zero
      sicet(i,j)=zero
    end do
  end do

! Multiply by background error variances, and break up skin temp
! into components
  call bkgvar(t,p,q,oz,skint,cwmr,st,vp,sst,slndt,sicet,0)

! Apply vertical smoother
!$omp parallel do  schedule(dynamic,1) private(k)
  do k=1,6
   if(k == 1)call frfhvo(st,k)
   if(k == 2)call frfhvo(vp,k)
   if(k == 3)call frfhvo(t,k)
   if(k == 4)call frfhvo(q,k)
   if(k == 5)call frfhvo(oz,k)
   if(k == 6)call frfhvo(cwmr,k)
  end do

! Convert from subdomain to full horizontal field distributed among processors
  call sub2grid(hwork,t,p,q,oz,sst,slndt,sicet,cwmr,st,vp,iflg)

! Apply horizontal smoother for number of horizontal scales
  call smoothrf(hwork,nsloop,nlevs)

! Put back onto subdomains
  call grid2sub(hwork,t,p,q,oz,sst,slndt,sicet,cwmr,st,vp)

! Apply vertical smoother
!$omp parallel do  schedule(dynamic,1) private(k)
  do k=1,6
   if(k == 1)call frfhvo(st,k)
   if(k == 2)call frfhvo(vp,k)
   if(k == 3)call frfhvo(t,k)
   if(k == 4)call frfhvo(q,k)
   if(k == 5)call frfhvo(oz,k)
   if(k == 6)call frfhvo(cwmr,k)
  end do

! Multiply by background error variances, and combine sst,sldnt, and sicet
! into skin temperature field
  call bkgvar(t,p,q,oz,skint,cwmr,st,vp,sst,slndt,sicet,1)

  return
end subroutine bkgcov
! -----------------------------------------------------------------------------
subroutine ckgcov(z,st,vp,t,p,q,oz,skint,cwmr,nlevs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ckgcov   sqrt of bkgcov
!   prgmmr: kleist         org: np22                date: 2004-07-22
!
! abstract: perform horizontal and vertical parts of background error
!
! program history log:
!   2007-04-24  parrish
!   2008-12-04  todling - turn sst,slndt,sicet to locals per GSI May08 
!                         update to bkgcov above.
!
!   input argument list:
!     t        - t on subdomain
!     p        - p surface pressure on subdomain
!     q        - q on subdomain
!     oz       - ozone on subdomain
!     skint    - skin temperature on subdomain
!     cwmr     - cloud water mixing ratio on subdomain
!     st       - streamfunction on subdomain
!     vp       - velocity potential on subdomain
!     nlevs    - number of vertical levels for smoothing
!
!   output argument list:
!                 all after smoothing, combining scales
!     t        - t on subdomain
!     p        - p surface pressure on subdomain
!     q        - q on subdomain
!     oz       - ozone on subdomain
!     skint    - skin temperature on subdomain
!     cwmr     - cloud water mixing ratio on subdomain
!     st       - streamfunction on subdomain
!     vp       - velocity potential on subdomain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero, one, three
  use gridmod, only: nlat,nlon,lat2,lon2,nsig,nnnn1o
  use jfunc,only: nval_lenz
  implicit none

! Passed Variables
  integer(i_kind),intent(in):: nlevs
  real(r_kind),dimension(nval_lenz),intent(in):: z
  real(r_kind),dimension(lat2,lon2),intent(inout):: p,skint
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: t,q,cwmr,oz,st,vp

! Local Variables
  integer(i_kind) i,j,k,nsloop
  real(r_kind),dimension(lat2,lon2):: sst,slndt,sicet
  real(r_kind),dimension(nlat,nlon,nnnn1o):: hwork

  nsloop=3

  do j=1,lon2
    do i=1,lat2
      sst(i,j)=zero
      slndt(i,j)=zero
      sicet(i,j)=zero
    end do
  end do

! Apply horizontal smoother for number of horizontal scales
  call sqrt_smoothrf(z,hwork,nsloop,nlevs)

! Put back onto subdomains
  call grid2sub(hwork,t,p,q,oz,sst,slndt,sicet,cwmr,st,vp)

! Apply vertical smoother
!$omp parallel do  schedule(dynamic,1) private(k)
  do k=1,6
   if(k == 1)call frfhvo(st,k)
   if(k == 2)call frfhvo(vp,k)
   if(k == 3)call frfhvo(t,k)
   if(k == 4)call frfhvo(q,k)
   if(k == 5)call frfhvo(oz,k)
   if(k == 6)call frfhvo(cwmr,k)
  end do

! Multiply by background error variances, and combine sst,sldnt, and sicet
! into skin temperature field
  call bkgvar(t,p,q,oz,skint,cwmr,st,vp,sst,slndt,sicet,1)

  return
end subroutine ckgcov
! -----------------------------------------------------------------------------
subroutine ckgcov_ad(z,st,vp,t,p,q,oz,skint,cwmr,nlevs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ckgcov_ad  adjoint of ckgcov
!   prgmmr: kleist         org: np22                date: 2004-07-22
!
! abstract: perform horizontal and vertical parts of background error
!
! program history log:
!   2007-04-24  parrish
!   2008-12-04  todling - turn sst,slndt,sicet to locals per GSI May08 
!                         update to bkgcov above.
!
!   input argument list:
!     t        - t on subdomain
!     p        - p surface pressure on subdomain
!     q        - q on subdomain
!     oz       - ozone on subdomain
!     skint    - skin temperature on subdomain
!     cwmr     - cloud water mixing ratio on subdomain
!     st       - streamfunction on subdomain
!     vp       - velocity potential on subdomain
!     nlevs    - number of vertical levels for smoothing
!
!   output argument list:
!                 all after smoothing, combining scales
!     t        - t on subdomain
!     p        - p surface pressure on subdomain
!     q        - q on subdomain
!     oz       - ozone on subdomain
!     skint    - skin temperature on subdomain
!     cwmr     - cloud water mixing ratio on subdomain
!     st       - streamfunction on subdomain
!     vp       - velocity potential on subdomain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero, one, three
  use gridmod, only: nlat,nlon,lat2,lon2,nsig,nnnn1o
  use jfunc, only: nval_lenz
  implicit none

! Passed Variables
  integer(i_kind),intent(in):: nlevs
  real(r_kind),dimension(lat2,lon2),intent(inout):: p,skint
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: t,q,cwmr,oz,st,vp
  real(r_kind),dimension(nval_lenz),intent(inout):: z

! Local Variables
  integer(i_kind) i,j,k,nsloop,iflg
  real(r_kind),dimension(lat2,lon2):: sst,slndt,sicet
  real(r_kind),dimension(nlat,nlon,nnnn1o):: hwork

  nsloop=3
  iflg=1

  do j=1,lon2
    do i=1,lat2
      sst(i,j)=zero
      slndt(i,j)=zero
      sicet(i,j)=zero
    end do
  end do

! Multiply by background error variances, and break up skin temp
! into components
  call bkgvar(t,p,q,oz,skint,cwmr,st,vp,sst,slndt,sicet,0)

! Apply vertical smoother
!$omp parallel do  schedule(dynamic,1) private(k)
  do k=1,6
   if(k == 1)call frfhvo(st,k)
   if(k == 2)call frfhvo(vp,k)
   if(k == 3)call frfhvo(t,k)
   if(k == 4)call frfhvo(q,k)
   if(k == 5)call frfhvo(oz,k)
   if(k == 6)call frfhvo(cwmr,k)
  end do

! Convert from subdomain to full horizontal field distributed among processors
  call sub2grid(hwork,t,p,q,oz,sst,slndt,sicet,cwmr,st,vp,iflg)

! Apply horizontal smoother for number of horizontal scales
  call sqrt_smoothrf_ad(z,hwork,nsloop,nlevs)

  return
end subroutine ckgcov_ad
