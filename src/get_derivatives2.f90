subroutine get_derivatives2(st,vp,t,p3d,u,v, &
                 u_x,v_x,t_x,p3d_x, &
                 u_y,v_y,t_y,p3d_y)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_derivatives2  compute horizontal derivatives
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: get horizontal derivatives of state vector
!
! program history log:
!   2005-06-06  parrish
!   2005=07-10  kleist, clean up and fix skint
!   2009-04-21  derber - modify from get_derivatives to minimize data movement 
!               and work for mass and momentum only and calculate uv
!   2009-11-27  parrish - add uv_hyb_ens:  uv_hyb_ens=T, then st=u, vp=v
!   2010-01-04  safford - comment out $omp directives that produce inconsistent results 
!
!   input argument list:
!     u        - longitude velocity component
!     v        - latitude velocity component
!     t        - virtual temperature
!     p        - ln(psfc)
!
!   output argument list:
!     u_x      - longitude derivative of u  (note: in global mode, undefined at pole points)
!     v_x      - longitude derivative of v  (note: in global mode, undefined at pole points)
!     t_x      - longitude derivative of t
!     p_x      - longitude derivative of ln(psfc)
!     u_y      - latitude derivative of u  (note: in global mode, undefined at pole points)
!     v_y      - latitude derivative of v  (note: in global mode, undefined at pole points)
!     t_y      - latitude derivative of t
!     p_y      - latitude derivative of ln(psfc)
!
!   note:  u_x,v_x,u_y,v_y are not evaluated at the poles
!     all other derivatives are
!
!   for u and v, derivatives are following:
!
!     u_x:  (du/dlon)/(a*cos(lat))
!     u_y:  (d(u*cos(lat))/dlat)/(a*cos(lat))
!
!     v_x:  (dv/dlon)/(a*cos(lat))
!     v_y:  (d(v*cos(lat))/dlat)/(a*cos(lat))
!
!  for all other variables, derivatives are:
!
!     f_x:  (df/dlon)/(a*cos(lat))
!     f_y:  (df/dlat)/a
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use gridmod, only: regional,nlat,nlon,lat2,lon2,nsig
  use compact_diffs, only: compact_dlat,compact_dlon,stvp2uv
  use mpimod, only: nvarbal_id,nlevsbal,nnnvsbal
  use hybrid_ensemble_parameters, only: uv_hyb_ens

  implicit none

! Passed variables
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in   ) :: p3d
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(in   ) :: t,st,vp
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(  out) :: p3d_x,p3d_y
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(  out) :: t_x,u_x,v_x,u,v
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(  out) :: t_y,u_y,v_y

! Local Variables
  integer(i_kind) iflg,k,i,j
  real(r_kind),dimension(nlat,nlon,nlevsbal):: hwork,hwork_x,hwork_y
  real(r_kind),dimension(nlat,nlon):: stx,vpx
  logical vector


  iflg=1

! substitute u for q, v for oz, t for cwmr and p for tskin
  call sub2grid2(hwork,st,vp,p3d,t,iflg)
! x derivative
  if(regional)then
     do k=1,nnnvsbal
        if(nvarbal_id(k) ==1.and..not.uv_hyb_ens)then
           do j=1,nlon
              do i=1,nlat
                 stx(i,j)=hwork(i,j,k)
                 vpx(i,j)=hwork(i,j,k+1)
              end do
           end do
           call psichi2uv_reg(stx,vpx,hwork(1,1,k),hwork(1,1,k+1))
        end if
     end do
!$omp parallel do private (k,vector)
     do k=1,nnnvsbal
        vector=.false.
        if(nvarbal_id(k) <= 2)vector=.true.
        call delx_reg(hwork(1,1,k),hwork_x(1,1,k),vector)
        call dely_reg(hwork(1,1,k),hwork_y(1,1,k),vector)
     end do
!$omp end parallel do
  else
     do k=1,nnnvsbal
        if(nvarbal_id(k) == 1 .and..not.uv_hyb_ens)then
           call stvp2uv(hwork(1,1,k),hwork(1,1,k+1))
        end if
     end do
!$omp parallel do private(k,vector)
     do k=1,nnnvsbal
        vector=.false.
        if(nvarbal_id(k) <= 2)vector=.true.
        call compact_dlon(hwork(1,1,k),hwork_x(1,1,k),vector)
        call compact_dlat(hwork(1,1,k),hwork_y(1,1,k),vector)
     end do
!$omp end parallel do
  end if
! p3d_x and t_x are dummy arrays in first call
  call grid2sub2(hwork,u,v,p3d_x,t_x)
  call grid2sub2(hwork_x,u_x,v_x,p3d_x,t_x)
  call grid2sub2(hwork_y,u_y,v_y,p3d_y,t_y)


  return
end subroutine get_derivatives2

subroutine tget_derivatives2(st,vp,t,p3d,u,v,&
                 u_x,v_x,t_x,p3d_x, &
                 u_y,v_y,t_y,p3d_y)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tget_derivatives2  adjoint of get_derivatives
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: adjoint of get_derivatives 
!
! program history log:
!   2005-06-06  parrish
!   2005-07-10  kleist, clean up
!   2009-04-21  derber - modify from get_derivatives to minimize data movement 
!               and work for mass and momentum only and calculate uv
!   2009-11-27  parrish - add uv_hyb_ens:  uv_hyb_ens=T, then st=u, vp=v
!
!   input argument list:
!     u_x      - longitude derivative of u  (note: in global mode, undefined at pole points)
!     v_x      - longitude derivative of v  (note: in global mode, undefined at pole points)
!     t_x      - longitude derivative of t
!     p_x      - longitude derivative of ln(psfc)
!     u_y      - latitude derivative of u  (note: in global mode, undefined at pole points)
!     v_x      - latitude derivative of v  (note: in global mode, undefined at pole points)
!     t_y      - latitude derivative of t
!     p_y      - latitude derivative of ln(psfc)
!
!   output argument list:
!     u        - longitude velocity component
!     v        - latitude velocity component
!     t        - virtual temperature
!     p        - ln(psfc)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!    adjoint of get_derivatives

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional,nlat,nlon,lat2,lon2,nsig
  use compact_diffs, only: tcompact_dlat,tcompact_dlon,tstvp2uv
  use mpimod, only: nvarbal_id,nnnvsbal
  use hybrid_ensemble_parameters, only: uv_hyb_ens
  implicit none

! Passed variables
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout) :: p3d
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(inout) :: t,st,vp
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout) :: p3d_x
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(inout) :: t_x,u_x,v_x,u,v
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout) :: p3d_y
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(inout) :: t_y,u_y,v_y

! Local Variables
  integer(i_kind) iflg,k,i,j
  real(r_kind),dimension(nlat,nlon,nnnvsbal):: hwork,hwork_x,hwork_y
  real(r_kind),dimension(nlat,nlon):: ux,vx
  logical vector

  iflg=1

!             initialize hwork to zero, so can accumulate contribution from
!             all derivatives
  hwork=zero

  call sub2grid2(hwork_x,u_x,v_x,p3d_x,t_x,iflg)
  call sub2grid2(hwork_y,u_y,v_y,p3d_y,t_y,iflg)
  p3d_x=zero
  t_x=zero
  call sub2grid2(hwork,u,v,p3d_x,t_x,iflg)
  if(regional)then
     do k=nnnvsbal,1,-1
        vector=.false.
        if(nvarbal_id(k) <= 2)vector=.true.
        call tdelx_reg(hwork_x(1,1,k),hwork(1,1,k),vector)
        call tdely_reg(hwork_y(1,1,k),hwork(1,1,k),vector)
        if(nvarbal_id(k) == 1 .and..not.uv_hyb_ens)then
           do j=1,nlon
              do i=1,nlat
                 ux(i,j)=hwork(i,j,k)
                 vx(i,j)=hwork(i,j,k+1)
                 hwork(i,j,k)=zero
                 hwork(i,j,k+1)=zero
              end do
           end do
           call psichi2uvt_reg(ux,vx,hwork(1,1,k),hwork(1,1,k+1))

        end if
     end do
  else
     do k=nnnvsbal,1,-1
        vector=.false.
        if(nvarbal_id(k) <= 2)vector=.true.
        call tcompact_dlon(hwork(1,1,k),hwork_x(1,1,k),vector)
        call tcompact_dlat(hwork(1,1,k),hwork_y(1,1,k),vector)
        if(nvarbal_id(k) == 1 .and..not.uv_hyb_ens)then
           call tstvp2uv(hwork(1,1,k),hwork(1,1,k+1))
        end if
     end do
  end if

!     use t_x,etc since don't need to save contents
  call grid2sub2(hwork,u_x,v_x,p3d_x,t_x)

! accumulate to contents of t,p,etc (except st,vp, which are zero on input
!$omp parallel do private(i,j,k)
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           st(i,j,k)=st(i,j,k)+u_x(i,j,k)
           vp(i,j,k)=vp(i,j,k)+v_x(i,j,k)
           t(i,j,k)=t(i,j,k)+t_x(i,j,k)
        end do
     end do
  end do
!$omp end parallel do
  do k=1,nsig+1
     do j=1,lon2
        do i=1,lat2
           p3d(i,j,k)=p3d(i,j,k)+p3d_x(i,j,k)
        end do
     end do
  end do

end subroutine tget_derivatives2
