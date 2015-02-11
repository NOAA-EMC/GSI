subroutine get_derivatives2(st,vp,t,p3d,u,v, &
                 u_x,v_x,t_x,p3d_x, &
                 u_y,v_y,t_y,p3d_y,uvflag)
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
!   2010-05-23  todling - trying to unwire index assumptions for sf and vp 
!   2012-02-08  kleist  - add uvflag to input arguments, remove ref to uv_hyb_ens parameter.
!   2012-06-12  parrish - significant reorganization to replace sub2grid2/grid2sub2 with
!                         general_sub2grid/general_grid2sub.
!   2014-12-03  derber - changes to reduce data movement (including call to
!                        stvp2uv and tstvp2uv
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
  use constants, only: zero
  use gridmod, only: regional,nlat,nlon,lat2,lon2,nsig
  use compact_diffs, only: compact_dlat,compact_dlon,stvp2uv
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use general_commvars_mod, only: s2g4

  implicit none

! Passed variables
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in   ) :: p3d
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(in   ) :: t,st,vp
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(  out) :: p3d_x,p3d_y
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(  out) :: t_x,u_x,v_x,u,v
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(  out) :: t_y,u_y,v_y
  logical                                 ,intent(in   ) :: uvflag

! Local Variables
  integer(i_kind) k,i,j,kk,k2
  real(r_kind),allocatable,dimension(:,:,:,:) :: hwork_sub,hwork,hwork_x,hwork_y
  real(r_kind),dimension(nlat,nlon):: stx,vpx
  logical vector

  allocate(hwork_sub(2,s2g4%lat2,s2g4%lon2,s2g4%num_fields))
  do kk=1,s2g4%num_fields
     k=s2g4%lnames(1,kk)
     k2=s2g4%lnames(2,kk)
     if(trim(s2g4%names(1,kk))=='sf'.and.trim(s2g4%names(2,kk))=='vp') then
        do j=1,s2g4%lon2
           do i=1,s2g4%lat2
              hwork_sub(1,i,j,kk)=st(i,j,k)
              hwork_sub(2,i,j,kk)=vp(i,j,k)
           end do
        end do
     else
        if(k2==0) then          !  p3d level nsig+1 where there is no corresponding t value
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 hwork_sub(1,i,j,kk)=p3d(i,j,k)
                 hwork_sub(2,i,j,kk)=zero
              end do
           end do
        else
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 hwork_sub(1,i,j,kk)=p3d(i,j,k)
                 hwork_sub(2,i,j,kk)=t(i,j,k)
              end do
           end do
        end if
     end if
  end do
  allocate(hwork(s2g4%inner_vars,s2g4%nlat,s2g4%nlon,s2g4%kbegin_loc:s2g4%kend_alloc))
  call general_sub2grid(s2g4,hwork_sub,hwork)
  allocate(hwork_x(s2g4%inner_vars,s2g4%nlat,s2g4%nlon,s2g4%kbegin_loc:s2g4%kend_alloc))
  allocate(hwork_y(s2g4%inner_vars,s2g4%nlat,s2g4%nlon,s2g4%kbegin_loc:s2g4%kend_alloc))

! x derivative
  if(regional)then
     if(.not.uvflag) then
        do k=s2g4%kbegin_loc,s2g4%kend_loc
           if(trim(s2g4%names(1,k))=='sf'.and.trim(s2g4%names(2,k))=='vp') then
              do j=1,nlon
                 do i=1,nlat
                    stx(i,j)=hwork(1,i,j,k)
                    vpx(i,j)=hwork(2,i,j,k)
                 end do
              end do
              call psichi2uv_reg(stx,vpx,hwork(1,:,:,k),hwork(2,:,:,k))
           end if
        end do
     end if
!       !$omp parallel do private (k,vector)     ! ??????????fix this later
     do k=s2g4%kbegin_loc,s2g4%kend_loc
        vector=trim(s2g4%names(1,k))=='sf'.and.trim(s2g4%names(2,k))=='vp'
        do j=1,nlon
           do i=1,nlat
              stx(i,j)=hwork(1,i,j,k)
              vpx(i,j)=hwork(2,i,j,k)
           end do
        end do
        call delx_reg(stx,hwork_x(1,:,:,k),vector)
        call dely_reg(stx,hwork_y(1,:,:,k),vector)
        call delx_reg(vpx,hwork_x(2,:,:,k),vector)
        call dely_reg(vpx,hwork_y(2,:,:,k),vector)
     end do
!        !$omp end parallel do                     ! ?????fix later

  else
     if(.not. uvflag)then
        do k=s2g4%kbegin_loc,s2g4%kend_loc
           if(trim(s2g4%names(1,k))=='sf'.and.trim(s2g4%names(2,k))=='vp') then
              call stvp2uv(hwork(1,1,1,k),s2g4%inner_vars)
           end if
        end do
     end if
!       !$omp parallel do private(k,vector)      ! ????????????fix this later
     do k=s2g4%kbegin_loc,s2g4%kend_loc
        vector=trim(s2g4%names(1,k))=='sf'.and.trim(s2g4%names(2,k))=='vp'
        do j=1,nlon
           do i=1,nlat
              stx(i,j)=hwork(1,i,j,k)
              vpx(i,j)=hwork(2,i,j,k)
           end do
        end do
        call compact_dlon(stx,hwork_x(1,:,:,k),vector)
        call compact_dlat(stx,hwork_y(1,:,:,k),vector)
        call compact_dlon(vpx,hwork_x(2,:,:,k),vector)
        call compact_dlat(vpx,hwork_y(2,:,:,k),vector)
     end do
!       !$omp end parallel do                        ! ???fix later
  end if

  call general_grid2sub(s2g4,hwork,hwork_sub)
  deallocate(hwork)
  do kk=1,s2g4%num_fields
     k=s2g4%lnames(1,kk)
     if(trim(s2g4%names(1,kk))=='sf'.and.trim(s2g4%names(2,kk))=='vp') then
        do j=1,s2g4%lon2
           do i=1,s2g4%lat2
              u(i,j,k)=hwork_sub(1,i,j,kk)
              v(i,j,k)=hwork_sub(2,i,j,kk)
           end do
        end do
     end if
  end do
  call general_grid2sub(s2g4,hwork_x,hwork_sub)
  deallocate(hwork_x)
  do kk=1,s2g4%num_fields
     k=s2g4%lnames(1,kk)
     k2=s2g4%lnames(2,kk)
     if(trim(s2g4%names(1,kk))=='sf'.and.trim(s2g4%names(2,kk))=='vp') then
        do j=1,s2g4%lon2
           do i=1,s2g4%lat2
              u_x(i,j,k)=hwork_sub(1,i,j,kk)
              v_x(i,j,k)=hwork_sub(2,i,j,kk)
           end do
        end do
     else
        if(k2==0) then          !  p3d level nsig+1 where there is no corresponding t value
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 p3d_x(i,j,k)=hwork_sub(1,i,j,kk)
              end do
           end do
        else
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 p3d_x(i,j,k)=hwork_sub(1,i,j,kk)
                 t_x(i,j,k)=hwork_sub(2,i,j,kk)
              end do
           end do
        end if
     end if
  end do
  call general_grid2sub(s2g4,hwork_y,hwork_sub)
  deallocate(hwork_y)
  do kk=1,s2g4%num_fields
     k=s2g4%lnames(1,kk)
     k2=s2g4%lnames(2,kk)
     if(trim(s2g4%names(1,kk))=='sf'.and.trim(s2g4%names(2,kk))=='vp') then
        do j=1,s2g4%lon2
           do i=1,s2g4%lat2
              u_y(i,j,k)=hwork_sub(1,i,j,kk)
              v_y(i,j,k)=hwork_sub(2,i,j,kk)
           end do
        end do
     else
        if(k2==0) then          !  p3d level nsig+1 where there is no corresponding t value
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 p3d_y(i,j,k)=hwork_sub(1,i,j,kk)
              end do
           end do
        else
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 p3d_y(i,j,k)=hwork_sub(1,i,j,kk)
                 t_y(i,j,k)=hwork_sub(2,i,j,kk)
              end do
           end do
        end if
     end if
  end do
  deallocate(hwork_sub)


  return
end subroutine get_derivatives2

subroutine tget_derivatives2(st,vp,t,p3d,u,v,&
                 u_x,v_x,t_x,p3d_x, &
                 u_y,v_y,t_y,p3d_y,uvflag)
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
!   2010-05-23  todling - trying to unwire index assumptions for sf and vp 
!   2012-02-08  kleist  - add uvflag to input arguments, remove ref to uv_hyb_ens parameter.
!   2012-06-12  parrish - significant reorganization to replace sub2grid2/grid2sub2 with
!                         general_sub2grid/general_grid2sub.
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
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use general_commvars_mod, only: s2g4
  implicit none

! Passed variables
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout) :: p3d
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(inout) :: t,st,vp
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout) :: p3d_x
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(inout) :: t_x,u_x,v_x,u,v
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout) :: p3d_y
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(inout) :: t_y,u_y,v_y
  logical                                 ,intent(in   ) :: uvflag

! Local Variables
  integer(i_kind) k,i,j,kk,k2
  real(r_kind),allocatable,dimension(:,:,:,:) :: hwork_sub,hwork,hwork_x,hwork_y
  real(r_kind),dimension(nlat,nlon):: ux,vx
  logical vector

  allocate(hwork_sub(2,s2g4%lat2,s2g4%lon2,s2g4%num_fields))

  do kk=1,s2g4%num_fields
     k=s2g4%lnames(1,kk)
     k2=s2g4%lnames(2,kk)
     if(trim(s2g4%names(1,kk))=='sf'.and.trim(s2g4%names(2,kk))=='vp') then
        do j=1,s2g4%lon2
           do i=1,s2g4%lat2
              hwork_sub(1,i,j,kk)=u_x(i,j,k)
              hwork_sub(2,i,j,kk)=v_x(i,j,k)
           end do
        end do
     else
        if(k2==0) then          !  p3d level nsig+1 where there is no corresponding t value
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 hwork_sub(1,i,j,kk)=p3d_x(i,j,k)
                 hwork_sub(2,i,j,kk)=zero
              end do
           end do
        else
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 hwork_sub(1,i,j,kk)=p3d_x(i,j,k)
                 hwork_sub(2,i,j,kk)=t_x(i,j,k)
              end do
           end do
        end if
     end if
  end do
  allocate(hwork_x(s2g4%inner_vars,s2g4%nlat,s2g4%nlon,s2g4%kbegin_loc:s2g4%kend_alloc))
  call general_sub2grid(s2g4,hwork_sub,hwork_x)
  p3d_x=zero
  t_x=zero

  do kk=1,s2g4%num_fields
     k=s2g4%lnames(1,kk)
     k2=s2g4%lnames(2,kk)
     if(trim(s2g4%names(1,kk))=='sf'.and.trim(s2g4%names(2,kk))=='vp') then
        do j=1,s2g4%lon2
           do i=1,s2g4%lat2
              hwork_sub(1,i,j,kk)=u_y(i,j,k)
              hwork_sub(2,i,j,kk)=v_y(i,j,k)
           end do
        end do
     else
        if(k2==0) then          !  p3d level nsig+1 where there is no corresponding t value
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 hwork_sub(1,i,j,kk)=p3d_y(i,j,k)
                 hwork_sub(2,i,j,kk)=zero
              end do
           end do
        else
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 hwork_sub(1,i,j,kk)=p3d_y(i,j,k)
                 hwork_sub(2,i,j,kk)=t_y(i,j,k)
              end do
           end do
        end if
     end if
  end do
  allocate(hwork_y(s2g4%inner_vars,s2g4%nlat,s2g4%nlon,s2g4%kbegin_loc:s2g4%kend_alloc))
  call general_sub2grid(s2g4,hwork_sub,hwork_y)


  do kk=1,s2g4%num_fields
     k=s2g4%lnames(1,kk)
     k2=s2g4%lnames(2,kk)
     if(trim(s2g4%names(1,kk))=='sf'.and.trim(s2g4%names(2,kk))=='vp') then
        do j=1,s2g4%lon2
           do i=1,s2g4%lat2
              hwork_sub(1,i,j,kk)=u(i,j,k)
              hwork_sub(2,i,j,kk)=v(i,j,k)
           end do
        end do
     else
        if(k2==0) then          !  p3d level nsig+1 where there is no corresponding t value
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 hwork_sub(1,i,j,kk)=p3d_x(i,j,k)
                 hwork_sub(2,i,j,kk)=zero
              end do
           end do
        else
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 hwork_sub(1,i,j,kk)=p3d_x(i,j,k)
                 hwork_sub(2,i,j,kk)=t_x(i,j,k)
              end do
           end do
        end if
     end if
  end do
!             initialize hwork to zero, so can accumulate contribution from
!             all derivatives
  allocate(hwork(s2g4%inner_vars,s2g4%nlat,s2g4%nlon,s2g4%kbegin_loc:s2g4%kend_alloc))
  hwork=zero      ! ??? do I need this ???
  call general_sub2grid(s2g4,hwork_sub,hwork)

  if(regional)then
!       !$omp parallel do private (k,vector)     ! ??????????fix this later
     do k=s2g4%kbegin_loc,s2g4%kend_loc
        vector=trim(s2g4%names(1,k))=='sf'.and.trim(s2g4%names(2,k))=='vp'
        if(vector) then
           ux=zero
           vx=zero
           call tdelx_reg(hwork_x(1,:,:,k),ux,vector)
           call tdely_reg(hwork_y(1,:,:,k),ux,vector)
           call tdelx_reg(hwork_x(2,:,:,k),vx,vector)
           call tdely_reg(hwork_y(2,:,:,k),vx,vector)
           call psichi2uvt_reg(ux,vx,hwork(1,:,:,k),hwork(2,:,:,k))
        else
           call tdelx_reg(hwork_x(1,:,:,k),hwork(1,:,:,k),vector)
           call tdely_reg(hwork_y(1,:,:,k),hwork(1,:,:,k),vector)
           call tdelx_reg(hwork_x(2,:,:,k),hwork(2,:,:,k),vector)
           call tdely_reg(hwork_y(2,:,:,k),hwork(2,:,:,k),vector)
        end if
     end do
  else
!       !$omp parallel do private(k,vector)      ! ????????????fix this later
     do k=s2g4%kbegin_loc,s2g4%kend_loc
        vector=trim(s2g4%names(1,k))=='sf'.and.trim(s2g4%names(2,k))=='vp'
        call tcompact_dlon(hwork(1,:,:,k),hwork_x(1,:,:,k),vector)
        call tcompact_dlat(hwork(1,:,:,k),hwork_y(1,:,:,k),vector)
        call tcompact_dlon(hwork(2,:,:,k),hwork_x(2,:,:,k),vector)
        call tcompact_dlat(hwork(2,:,:,k),hwork_y(2,:,:,k),vector)
     end do
!       !$omp end parallel do                        ! ???fix later
     if(.not. uvflag)then
        do k=s2g4%kbegin_loc,s2g4%kend_loc
           if(trim(s2g4%names(1,k))=='sf'.and.trim(s2g4%names(2,k))=='vp') then
              call tstvp2uv(hwork(1,1,1,k),s2g4%inner_vars)
           end if
        end do
     end if
  end if
  deallocate(hwork_x,hwork_y)

!     use t_x,etc since don't need to save contents
  call general_grid2sub(s2g4,hwork,hwork_sub)
  deallocate(hwork)
  do kk=1,s2g4%num_fields
     k=s2g4%lnames(1,kk)
     k2=s2g4%lnames(2,kk)
     if(trim(s2g4%names(1,kk))=='sf'.and.trim(s2g4%names(2,kk))=='vp') then
        do j=1,s2g4%lon2
           do i=1,s2g4%lat2
              st(i,j,k)=st(i,j,k)+hwork_sub(1,i,j,kk)
              vp(i,j,k)=vp(i,j,k)+hwork_sub(2,i,j,kk)
           end do
        end do
     else
        if(k2==0) then          !  p3d level nsig+1 where there is no corresponding t value
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 p3d(i,j,k)=p3d(i,j,k)+hwork_sub(1,i,j,kk)
              end do
           end do
        else
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 p3d(i,j,k)=p3d(i,j,k)+hwork_sub(1,i,j,kk)
                 t(i,j,k)=t(i,j,k)+hwork_sub(2,i,j,kk)
              end do
           end do
        end if
     end if
  end do
  deallocate(hwork_sub)

end subroutine tget_derivatives2
