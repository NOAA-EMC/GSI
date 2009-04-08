subroutine get_derivatives2(u,v,t,p3d, &
                 u_x,v_x,t_x,p3d_x, &
                 u_y,v_y,t_y,p3d_y, &
                 nlevs,mype)
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
!   2008-10-11  derber - modify from get_derivatives to minimize data movement 
!               and work for mass and momentum only
!
!   input argument list:
!     u        - longitude velocity component
!     v        - latitude velocity component
!     t        - virtual temperature
!     p        - ln(psfc)
!     nlevs    - number of levs on current processor in horizontal slab mode
!     mype     - current processor number
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
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!   note:  u_x,v_x,u_y,v_y are not evaluated at the poles
!     all other derivatives are

!   for u and v, derivatives are following:

!     u_x:  (du/dlon)/(a*cos(lat))
!     u_y:  (d(u*cos(lat))/dlat)/(a*cos(lat))

!     v_x:  (dv/dlon)/(a*cos(lat))
!     v_y:  (d(v*cos(lat))/dlat)/(a*cos(lat))

!  for all other variables, derivatives are:

!     f_x:  (df/dlon)/(a*cos(lat))
!     f_y:  (df/dlat)/a

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional,nlat,nlon,lat2,lon2,nsig,nnnn1o,wrf_nmm_regional,eta2_ll
  use compact_diffs, only: compact_dlat,compact_dlon
  use mpimod, only: npe,nvar_id

  implicit none

! Passed variables
  integer(i_kind) mype
  integer(i_kind) nlevs
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in):: p3d
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: t,u,v
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(out):: p3d_x
  real(r_kind),dimension(lat2,lon2,nsig),intent(out):: t_x,u_x,v_x
  real(r_kind),dimension(lat2,lon2,nsig),intent(out):: p3d_y
  real(r_kind),dimension(lat2,lon2,nsig),intent(out):: t_y,u_y,v_y

! Local Variables
  integer(i_kind) iflg,k,i,j
  real(r_kind),dimension(lat2,lon2):: dum1,dum2,ps,ps_x,ps_y
  real(r_kind),dimension(nlat,nlon,nnnn1o):: hwork,hworkd
  logical vector

  ps=zero

  if(regional)then
    do i=1,nlon
      do j=1,nlat
        ps(i,j)=p3d(i,j,1)
      end do
    end do
  end if
    iflg=1
    dum1=zero
    dum2=zero

!  substitute u for q, v for oz, t for cwmr and p for tskin
    call sub2grid(hwork,t,ps,u,v,ps,dum1,dum2,t,u,v,iflg)
!   x derivative
!$omp parallel do private(vector)
    do k=1,nlevs
     if(nvar_id(k) <= 4)then
      if(regional) then
        call get_dlon_reg(hwork(1,1,k),hworkd(1,1,k))
      else
        vector = nvar_id(k) == 1 .or. nvar_id(k) == 2
        call compact_dlon(hwork(1,1,k),hworkd(1,1,k),vector)
      end if
     else
      if(regional) then
        call get_dlat_reg(hwork(1,1,k),hworkd(1,1,k))
      else
        vector = nvar_id(k) == 5 .or. nvar_id(k) == 6
        call compact_dlat(hwork(1,1,k),hworkd(1,1,k),vector)
      end if
     end if
    end do
!$omp end parallel do
    call grid2sub(hworkd,t_x,ps_x,u_y, &
                  v_y,ps_y,dum1,dum2, &
                  t_y,u_x,v_x)


  if(regional)then
    if(wrf_nmm_regional) then
       do k=2,nsig+1
         do j=1,lon2
           do i=1,lat2
               p3d_x(i,j,k)=eta2_ll(k)*ps_x(i,j)
               p3d_y(i,j,k)=eta2_ll(k)*ps_y(i,j)
           end do
         end do
       end do
    else
      p3d_x=zero
      p3d_y=zero
    end if
  else
!   call mp_compact_dlon1(t,t_x,.false.,nsig,mype)
!   call mp_compact_dlat1(t,t_y,.false.,nsig,mype)
!   call mp_compact_dlon1(u,u_x,.true.,nsig,mype)
!   call mp_compact_dlat1(u,v_y,.true.,nsig,mype)
!   call mp_compact_dlon1(v,v_x,.true.,nsig,mype)
!   call mp_compact_dlat1(v,v_y,.true.,nsig,mype)
    call mp_compact_dlon1(p3d,p3d_x,.false.,nsig+1,mype)
    call mp_compact_dlat1(p3d,p3d_y,.false.,nsig+1,mype)
  
  end if

  return
end subroutine get_derivatives2

subroutine tget_derivatives2(u,v,t,p3d, &
                 u_x,v_x,t_x,p3d_x, &
                 u_y,v_y,t_y,p3d_y,nlevs,mype)
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
!   2008-10-11  derber - modify from get_derivatives to minimize data movement 
!               and work for mass and momentum only
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
!     nlevs    - number of levs on current processor in horizontal slab mode
!     mype     - current processor number
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
  use gridmod, only: regional,nlat,nlon,lat2,lon2,nsig,nnnn1o,wrf_nmm_regional,eta2_ll
  use compact_diffs, only: tcompact_dlat,tcompact_dlon
  use mpimod, only: npe,nvar_id
  implicit none

! Passed variables
  integer(i_kind) mype
  integer(i_kind) nlevs
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout):: p3d
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: t,u,v
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout):: p3d_x
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: t_x,u_x,v_x
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout):: p3d_y
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: t_y,u_y,v_y

! Local Variables
  integer(i_kind) iflg,k,i,j
  real(r_kind),dimension(lat2,lon2):: dum1,dum2,ps,ps_x,ps_y
  real(r_kind),dimension(nlat,nlon,nnnn1o):: hwork,hworkd
  logical vector

  ps=zero
! Adjoint of horizontal derivatives
  if (regional) then
    if(wrf_nmm_regional) then
      do k=1,nsig+1
        do j=1,lon2
          do i=1,lat2
            ps(i,j) = ps(i,j) + eta2_ll(k)*p3d_y(i,j,k)
            ps(i,j) = ps(i,j) + eta2_ll(k)*p3d_x(i,j,k)
          end do
        end do
      end do
    end if
  else
    call mp_compact_dlon1_ad(p3d,p3d_x,.false.,nsig+1,mype)
    call mp_compact_dlat1_ad(p3d,p3d_y,.false.,nsig+1,mype)
!   call mp_compact_dlon1_ad(t,t_x,.false.,nsig,mype)
!   call mp_compact_dlat1_ad(t,t_y,.false.,nsig,mype)
!   call mp_compact_dlon1_ad(u,u_x,.true.,nsig,mype)
!   call mp_compact_dlat1_ad(u,u_y,.true.,nsig,mype)
!   call mp_compact_dlon1_ad(v,v_x,.true.,nsig,mype)
!   call mp_compact_dlat1_ad(v,v_y,.true.,nsig,mype)
  end if

    iflg=1

!             initialize hwork to zero, so can accumulate contribution from
!             all derivatives
    hwork=zero
!             for now zero out slndt,sicet
    dum1=zero
    dum2=zero

!   adjoint of y derivative

    call sub2grid(hworkd,t_y,ps_y,u_x,v_x,ps_x,dum1,dum2,t_x, &
                u_y,v_y,iflg)
!$omp parallel do private(vector)
    do k=1,nlevs
     if(nvar_id(k) <= 4)then
      if(regional) then
        call tget_dlat_reg(hworkd(1,1,k),hwork(1,1,k))
      else
        vector = nvar_id(k) == 1 .or. nvar_id(k) == 2
        call tcompact_dlat(hwork(1,1,k),hworkd(1,1,k),vector)
      end if
     else
      if(regional) then
        call tget_dlon_reg(hworkd(1,1,k),hwork(1,1,k))
      else
        vector = nvar_id(k) == 5 .or. nvar_id(k) == 6
        call tcompact_dlon(hwork(1,1,k),hworkd(1,1,k),vector)
      end if
     end if
    end do
!$omp end parallel do


!       use t_x,etc since don't need to save contents
    call grid2sub(hwork,t_y,ps_y,u_x,v_x,ps_x,dum1,dum2,t_x,u_y,v_y)

!   accumulate to contents of t,p,etc (except st,vp, which are zero on input
!$omp parallel do
    do k=1,nsig
      do j=1,lon2
        do i=1,lat2
          t(i,j,k)=t(i,j,k)+t_x(i,j,k)+t_y(i,j,k)
          u(i,j,k)=u(i,j,k)+u_x(i,j,k)+u_y(i,j,k)
          v(i,j,k)=v(i,j,k)+v_x(i,j,k)+v_y(i,j,k)
        end do
      end do
    end do
!$omp end parallel do
  if(regional)then
    do j=1,lon2
     do i=1,lat2
         p3d(i,j,1)=p3d(i,j,1)+ps_x(i,j)+ps_y(i,j)
     end do
    end do
  end if

end subroutine tget_derivatives2
