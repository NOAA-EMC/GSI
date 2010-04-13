subroutine bkgcov(cstate,nlevs)
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
!   2010-03-01  zhu     - make changes for generalizing control vectors
!                       - replace explicit use of each control variable 
!                         by a control_state 'cstate'
!                       - use nrf* for generalized control variables
!                       - make changes to interfaces of sub2grid and grid2sub
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
  use constants, only: izero,ione,zero
  use gridmod, only: nlat,nlon,lat2,lon2,nsig,nnnn1o,latlon11
  use control_vectors, only: nrf3,nrf3_loc,control_state,nrf_levb
  implicit none

! Passed Variables
  integer(i_kind),intent(in) :: nlevs
  type(control_state),intent(inout) :: cstate

! Local Variables
  integer(i_kind) i,j,k,nsloop,iflg,nk,loc
  real(r_kind),dimension(lat2,lon2):: sst,slndt,sicet
  real(r_kind),dimension(nlat,nlon,nnnn1o):: hwork

  nsloop=3_i_kind
  iflg=ione

  do j=1,lon2
     do i=1,lat2
        sst(i,j)=zero
        slndt(i,j)=zero
        sicet(i,j)=zero
     end do
  end do

! Multiply by background error variances, and break up skin temp
! into components
  call bkgvar(cstate%values,sst,slndt,sicet,izero)

! Apply vertical smoother
!$omp parallel do  schedule(dynamic,1) private(k)
  do k=1,nrf3
     loc=nrf3_loc(k)
     nk=(nrf_levb(loc)-ione)*latlon11+ione
     call frfhvo(cstate%values(nk),k)
  end do

! Convert from subdomain to full horizontal field distributed among processors
  call sub2grid(hwork,cstate,sst,slndt,sicet,iflg)

! Apply horizontal smoother for number of horizontal scales
  call smoothrf(hwork,nsloop,nlevs)

! Put back onto subdomains
  call grid2sub(hwork,cstate,sst,slndt,sicet)

! Apply vertical smoother
!$omp parallel do  schedule(dynamic,1) private(k)
  do k=1,nrf3
     loc=nrf3_loc(k)
     nk=(nrf_levb(loc)-ione)*latlon11+ione
     call frfhvo(cstate%values(nk),k)
  end do

! Multiply by background error variances, and combine sst,sldnt, and sicet
! into skin temperature field
  call bkgvar(cstate%values,sst,slndt,sicet,ione)

  return
end subroutine bkgcov
! -----------------------------------------------------------------------------
subroutine ckgcov(z,cstate,nlevs)
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
!   2010-03-15  zhu - use nrf* and cstate for generalized control variable
!                   - make changes to interface of grid2sub
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
  use constants, only: ione,zero
  use gridmod, only: nlat,nlon,lat2,lon2,nsig,nnnn1o,latlon11
  use jfunc,only: nval_lenz,nval_levs
  use control_vectors, only: control_state,nrf3,nrf3_loc,nrf_levb
  implicit none

! Passed Variables
  integer(i_kind)    ,intent(in   ) :: nlevs
  type(control_state),intent(inout) :: cstate
  real(r_kind),dimension(nval_lenz),intent(in   ) :: z

! Local Variables
  integer(i_kind) i,j,k,nsloop,loc,nk
  real(r_kind),dimension(lat2,lon2):: sst,slndt,sicet
  real(r_kind),dimension(nlat,nlon,nnnn1o):: hwork

  nsloop=3_i_kind

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
  call grid2sub(hwork,cstate,sst,slndt,sicet)

! Apply vertical smoother
!$omp parallel do  schedule(dynamic,1) private(k,loc,nk)
  do k=1,nrf3
     loc=nrf3_loc(k)
     nk=(nrf_levb(loc)-ione)*latlon11+ione
     call frfhvo(cstate%values(nk),k)
  end do

! Multiply by background error variances, and combine sst,sldnt, and sicet
! into skin temperature field
  call bkgvar(cstate%values,sst,slndt,sicet,ione)

  return
end subroutine ckgcov
! -----------------------------------------------------------------------------
subroutine ckgcov_ad(z,cstate,nlevs)
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
!   2010-03-15  zhu - use nrf* and cstate for generalized control variable
!                   - make changes to interface of sub2grid 
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
  use constants, only: izero,ione,zero
  use gridmod, only: nlat,nlon,lat2,lon2,nsig,nnnn1o,latlon11
  use jfunc, only: nval_lenz
  use control_vectors, only: control_state,nrf3,nrf3_loc,nrf_levb
  implicit none

! Passed Variables
  integer(i_kind)    ,intent(in   ) :: nlevs
  type(control_state),intent(inout) :: cstate
  real(r_kind),dimension(nval_lenz),intent(inout) :: z

! Local Variables
  integer(i_kind) i,j,k,nsloop,iflg,loc,nk
  real(r_kind),dimension(lat2,lon2):: sst,slndt,sicet
  real(r_kind),dimension(nlat,nlon,nnnn1o):: hwork

  nsloop=3_i_kind
  iflg=ione

  do j=1,lon2
     do i=1,lat2
        sst(i,j)=zero
        slndt(i,j)=zero
        sicet(i,j)=zero
     end do
  end do

! Multiply by background error variances, and break up skin temp
! into components
  call bkgvar(cstate,sst,slndt,sicet,izero)

! Apply vertical smoother
!$omp parallel do  schedule(dynamic,1) private(k,loc,nk)
  do k=1,nrf3
     loc=nrf3_loc(k)
     nk=(nrf_levb(loc)-ione)*latlon11+ione
     call frfhvo(cstate%values(nk),k)
  end do

! Convert from subdomain to full horizontal field distributed among processors
  call sub2grid(hwork,cstate,sst,slndt,sicet,iflg)

! Apply horizontal smoother for number of horizontal scales
  call sqrt_smoothrf_ad(z,hwork,nsloop,nlevs)

  return
end subroutine ckgcov_ad
