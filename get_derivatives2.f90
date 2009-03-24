subroutine get_derivatives2(u,v,t,p, &
                 u_x,v_x,t_x,p_x, &
                 u_y,v_y,t_y,p_y, &
                 nlevs,mype,nfldsig)
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
!     nfldsig  - number of time levels
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
  use gridmod, only: regional,nlat,nlon,lat2,lon2,nsig,nsig1o
  use compact_diffs, only: compact_dlat,compact_dlon
  use mpimod, only: npe,nvar_id

  implicit none

! Passed variables
  integer(i_kind) mype
  integer(i_kind) nlevs,nfldsig
  real(r_kind),dimension(lat2,lon2,nfldsig),intent(in):: p
  real(r_kind),dimension(lat2,lon2,nsig,nfldsig),intent(in):: t,u,v
  real(r_kind),dimension(lat2,lon2,nfldsig),intent(out):: p_x
  real(r_kind),dimension(lat2,lon2,nsig,nfldsig),intent(out):: t_x,u_x,v_x
  real(r_kind),dimension(lat2,lon2,nfldsig),intent(out):: p_y
  real(r_kind),dimension(lat2,lon2,nsig,nfldsig),intent(out):: t_y,u_y,v_y

! Local Variables
  integer(i_kind) iflg,k,i,j,it
  real(r_kind),dimension(lat2,lon2):: dum1,dum2
  real(r_kind),dimension(nlat,nlon,nsig1o):: hwork,hworkd
  logical vector

  iflg=1
  dum1=zero
  dum2=zero

  if(nsig1o > nlevs)then
    do k=nlevs+1,nsig1o
      do j=1,nlon
        do i=1,nlat
          hworkd(i,j,k) = zero
        end do
      end do
    end do
  end if

!  substitute u for q, v for oz, t for cwmr and p for tskin
  do it=1,nfldsig
    call sub2grid(hwork,t(1,1,1,it),p(1,1,it),u(1,1,1,it),v(1,1,1,it), &
                  p(1,1,it),dum1,dum2,t(1,1,1,it),u(1,1,1,it), &
                  v(1,1,1,it),iflg)
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
    call grid2sub(hworkd,t_x(1,1,1,it),p_x(1,1,it),u_y(1,1,1,it), &
                  v_y(1,1,1,it),p_y(1,1,it),dum1,dum2, &
                  t_y(1,1,1,it),u_x(1,1,1,it),v_x(1,1,1,it))

  end do  ! end do it

  return
end subroutine get_derivatives2

subroutine tget_derivatives2(u,v,t,p, &
                 u_x,v_x,t_x,p_x, &
                 u_y,v_y,t_y,p_y,nlevs,mype)
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
  use gridmod, only: regional,nlat,nlon,lat2,lon2,nsig,nsig1o
  use compact_diffs, only: tcompact_dlat,tcompact_dlon
  use mpimod, only: npe,nvar_id
  implicit none

! Passed variables
  integer(i_kind) mype
  integer(i_kind) nlevs
  real(r_kind),dimension(lat2,lon2),intent(inout):: p
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: t,u,v
  real(r_kind),dimension(lat2,lon2),intent(inout):: p_x
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: t_x,u_x,v_x
  real(r_kind),dimension(lat2,lon2),intent(inout):: p_y
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: t_y,u_y,v_y

! Local Variables
  integer(i_kind) iflg,k,i,j
  real(r_kind),dimension(lat2,lon2):: dum1,dum2
  real(r_kind),dimension(nlat,nlon,nsig1o):: hwork,hworkd
  logical vector

  iflg=1
!             initialize hwork to zero, so can accumulate contribution from
!             all derivatives
  hwork=zero
!             for now zero out slndt,sicet
  dum1=zero
  dum2=zero

!   adjoint of y derivative

  call sub2grid(hworkd,t_y,p_y,u_x,v_x,p_x,dum1,dum2,t_x, &
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
  call grid2sub(hwork,t_y,p_y,u_x,v_x,p_x,dum1,dum2,t_x,u_y,v_y)

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
  do j=1,lon2
    do i=1,lat2
      p(i,j)=p(i,j)+p_x(i,j)+p_y(i,j)
    end do
  end do

end subroutine tget_derivatives2
