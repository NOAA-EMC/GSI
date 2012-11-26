subroutine get_derivatives(u,v,t,p,q,oz,skint,cwmr, &
                 u_x,v_x,t_x,p_x,q_x,oz_x,skint_x,cwmr_x, &
                 u_y,v_y,t_y,p_y,q_y,oz_y,skint_y,cwmr_y, &
                 nlevs,nfldsig)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_derivatives  compute horizontal derivatives
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: get horizontal derivatives of state vector
!
! program history log:
!   2005-06-06  parrish
!   2005=07-10  kleist, clean up and fix skint
!   2008-06-04  safford - rm unused var nbad and uses
!   2010-03-25  zhu     - made changes to interface of sub2grid and grid2sub
!   2010-04-29  todling - update to use gsi_bundle
!   2010-05-22  todling - remove implicit assumption in ordering of nvar_id
!   2010-05-31  todling - no need to do pointer checking
!   2011-07-04  todling - allow run either single or double precision
!   2012-06-12  parrish - make changes to replace sub2grid/grid2sub with general_sub2grid/general_grid2sub.
!                         Remove arrays slndt, sicet, slndt_x, sicet_x, slndt_y, sicet_y,
!                         and variable nsig1o.
!
!   input argument list:
!     u        - longitude velocity component
!     v        - latitude velocity component
!     t        - virtual temperature
!     p        - ln(psfc)
!     q        - moisture
!     oz       - ozone
!     skint    - skin temperature
!     cwmr     - cloud water mixing ratio
!     nlevs    - number of levs on current processor in horizontal slab mode
!     nfldsig  - number of time levels
!
!   output argument list:
!     u_x      - longitude derivative of u  (note: in global mode, undefined at pole points)
!     v_x      - longitude derivative of v  (note: in global mode, undefined at pole points)
!     t_x      - longitude derivative of t
!     p_x      - longitude derivative of ln(psfc)
!     q_x      - longitude derivative of moisture
!     oz_x     - longitude derivative of ozone
!     skint_x  - longitude derivative of skin temperature
!     cwmr_x   - longitude derivative of cwmr
!     u_y      - latitude derivative of u  (note: in global mode, undefined at pole points)
!     v_y      - latitude derivative of v  (note: in global mode, undefined at pole points)
!     t_y      - latitude derivative of t
!     p_y      - latitude derivative of ln(psfc)
!     q_y      - latitude derivative of moisture
!     oz_y     - latitude derivative of ozone
!     skint_y  - latitude derivative of skin temperature
!     cwmr_y   - latitude derivative of cwmr
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
  use compact_diffs, only: compact_dlat,compact_dlon
  use control_vectors, only: cvars2d
  use control_vectors, only: cvars3d
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetvar
  use gsi_bundlemod, only: gsi_bundleputvar
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_gridcreate
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use general_commvars_mod, only: s2g_d

  implicit none

! Passed variables
  integer(i_kind)                          ,intent(in   ) :: nlevs,nfldsig
  real(r_kind),dimension(lat2,lon2,nfldsig),intent(in   ) :: p
  real(r_kind),dimension(lat2,lon2)        ,intent(in   ) :: skint
  real(r_kind),dimension(lat2,lon2,nsig)   ,intent(in   ) :: t,q,cwmr,oz,u,v
  real(r_kind),dimension(lat2,lon2,nfldsig),intent(  out) :: p_x
  real(r_kind),dimension(lat2,lon2)        ,intent(  out) :: skint_x
  real(r_kind),dimension(lat2,lon2,nsig)   ,intent(  out) :: t_x,q_x,cwmr_x,oz_x,u_x,v_x
  real(r_kind),dimension(lat2,lon2,nfldsig),intent(  out) :: p_y
  real(r_kind),dimension(lat2,lon2)        ,intent(  out) :: skint_y
  real(r_kind),dimension(lat2,lon2,nsig)   ,intent(  out) :: t_y,q_y,cwmr_y,oz_y,u_y,v_y

! Local Variables
  character(len=*),parameter::myname='get_derivatives'
  integer(i_kind) k,i,j,it,ii,rc,ier
  real(r_kind),allocatable,dimension(:,:,:,:):: hwork,hworkd
  type(gsi_bundle):: cstate
  type(gsi_grid) :: grid

  allocate(hwork (s2g_d%inner_vars,s2g_d%nlat,s2g_d%nlon,s2g_d%kbegin_loc:s2g_d%kend_alloc))
  allocate(hworkd(s2g_d%inner_vars,s2g_d%nlat,s2g_d%nlon,s2g_d%kbegin_loc:s2g_d%kend_alloc))

!        use s2g_d%kend_alloc instead of s2g_d%kend_loc to force hworkd=0 even if not used on this pe

  ier=0
  call gsi_gridcreate(grid,lat2,lon2,nsig)
  do it=1,nfldsig
     call gsi_bundlecreate (cstate,grid,'derivatives work',ier, &
                            names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
     if(ier/=0) then
        write(6,*) trim(myname), ': trouble creating work bundle'
        call stop2(999)
     endif
     call gsi_bundleputvar ( cstate, 'ps' , p(:,:,it), rc )
     call gsi_bundleputvar ( cstate, 'sf' , u,         rc )
     call gsi_bundleputvar ( cstate, 'vp' , v,         rc )
     call gsi_bundleputvar ( cstate, 't'  , t,         rc )
     call gsi_bundleputvar ( cstate, 'q ' , q,         rc )
     call gsi_bundleputvar ( cstate, 'oz' , oz,        rc )
     call gsi_bundleputvar ( cstate, 'cw' , cwmr,      rc )
     call gsi_bundleputvar ( cstate, 'sst', skint,     rc )

     call general_sub2grid(s2g_d,cstate%values,hwork)


!    x derivative
!              !$omp parallel do private(k,vector)     !  fix later
     do k=s2g_d%kbegin_loc,s2g_d%kend_loc
        if(regional) then
           call delx_reg(hwork(1,:,:,k),hworkd(1,:,:,k),s2g_d%vector(k))
        else
           call compact_dlon(hwork(1,:,:,k),hworkd(1,:,:,k),s2g_d%vector(k))
        end if
     end do
!                !$omp end parallel do                   !  fix later
     call general_grid2sub(s2g_d,hworkd,cstate%values)
     call gsi_bundlegetvar ( cstate, 'ps' , p_x(:,:,it), rc )
     call gsi_bundlegetvar ( cstate, 'sf' , u_x,         rc )
     call gsi_bundlegetvar ( cstate, 'vp' , v_x,         rc )
     call gsi_bundlegetvar ( cstate, 't'  , t_x,         rc )
     call gsi_bundlegetvar ( cstate, 'q ' , q_x,         rc )
     call gsi_bundlegetvar ( cstate, 'oz' , oz_x,        rc )
     call gsi_bundlegetvar ( cstate, 'cw' , cwmr_x,      rc )
     call gsi_bundlegetvar ( cstate, 'sst', skint_x,     rc )

!    y derivative
!                  !$omp parallel do private(k,vector)    !  fix later ?????????
     do k=s2g_d%kbegin_loc,s2g_d%kend_loc
        if(regional) then
           call dely_reg(hwork(1,:,:,k),hworkd(1,:,:,k),s2g_d%vector(k))
        else
           call compact_dlat(hwork(1,:,:,k),hworkd(1,:,:,k),s2g_d%vector(k))
        end if
     end do
!                   !$omp end parallel do                   !  fix later ?????????
     call general_grid2sub(s2g_d,hworkd,cstate%values)
     call gsi_bundlegetvar ( cstate, 'ps' , p_y(:,:,it), rc )
     call gsi_bundlegetvar ( cstate, 'sf' , u_y,         rc )
     call gsi_bundlegetvar ( cstate, 'vp' , v_y,         rc )
     call gsi_bundlegetvar ( cstate, 't'  , t_y,         rc )
     call gsi_bundlegetvar ( cstate, 'q ' , q_y,         rc )
     call gsi_bundlegetvar ( cstate, 'oz' , oz_y,        rc )
     call gsi_bundlegetvar ( cstate, 'cw' , cwmr_y,      rc )
     call gsi_bundlegetvar ( cstate, 'sst', skint_y,     rc )

!    clean work space
     call gsi_bundledestroy(cstate,ier)
     if(ier/=0) then
        write(6,*) trim(myname), ': trouble destroying work bundle'
        call stop2(999)
     endif
  end do  ! end do it

  deallocate(hwork,hworkd)

  return
end subroutine get_derivatives

subroutine tget_derivatives(u,v,t,p,q,oz,skint,cwmr, &
                 u_x,v_x,t_x,p_x,q_x,oz_x,skint_x,cwmr_x, &
                 u_y,v_y,t_y,p_y,q_y,oz_y,skint_y,cwmr_y,nlevs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tget_derivatives  adjoint of get_derivatives
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: adjoint of get_derivatives 
!
! program history log:
!   2005-06-06  parrish
!   2005-07-10  kleist, clean up
!   2008-06-04  safford - rm unused vars and uses
!   2010-04-29  todling - update to use gsi_bundle
!   2010-05-22  todling - remove implicit assumption in ordering of nvar_id
!   2010-05-31  todling - no need to do pointer checking
!   2012-06-12  parrish - make changes to replace sub2grid/grid2sub with general_sub2grid/general_grid2sub.
!                         Remove arrays slndt, sicet, slndt_x, sicet_x, slndt_y, sicet_y,
!                         and variable nsig1o.
!
!   input argument list:
!     u_x      - longitude derivative of u  (note: in global mode, undefined at pole points)
!     v_x      - longitude derivative of v  (note: in global mode, undefined at pole points)
!     t_x      - longitude derivative of t
!     p_x      - longitude derivative of ln(psfc)
!     q_x      - longitude derivative of moisture
!     oz_x     - longitude derivative of ozone
!     skint_x  - longitude derivative of skin temperature
!     cwmr_x   - longitude derivative of cwmr
!     u_y      - latitude derivative of u  (note: in global mode, undefined at pole points)
!     v_x      - latitude derivative of v  (note: in global mode, undefined at pole points)
!     t_y      - latitude derivative of t
!     p_y      - latitude derivative of ln(psfc)
!     q_y      - latitude derivative of moisture
!     oz_y     - latitude derivative of ozone
!     skint_y  - latitude derivative of skin temperature
!     cwmr_y   - latitude derivative of cwmr
!     nlevs    - number of levs on current processor in horizontal slab mode
!
!   output argument list:
!     u        - longitude velocity component
!     v        - latitude velocity component
!     t        - virtual temperature
!     p        - ln(psfc)
!     q        - moisture
!     oz       - ozone
!     skint    - skin temperature
!     cwmr     - cloud water mixing ratio
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional,nlat,nlon,lat2,lon2,nsig
  use compact_diffs, only: tcompact_dlat,tcompact_dlon
  use control_vectors, only: cvars2d
  use control_vectors, only: cvars3d
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetvar
  use gsi_bundlemod, only: gsi_bundleputvar
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_gridcreate
  use gsi_bundlemod, only: gsi_gridcreate
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use general_commvars_mod, only: s2g_d
  implicit none

! Passed variables
  integer(i_kind)                       ,intent(in   ) :: nlevs
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: p,skint
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: t,q,cwmr,oz,u,v
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: p_x,skint_x
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: t_x,q_x,cwmr_x,oz_x,u_x,v_x
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: p_y,skint_y
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: t_y,q_y,cwmr_y,oz_y,u_y,v_y

! Local Variables
  character(len=*),parameter::myname='tget_derivatives'
  integer(i_kind) k,i,j,ii,rc,ier
  real(r_kind),allocatable,dimension(:,:,:,:):: hwork,hworkd
  type(gsi_bundle):: cstate
  type(gsi_grid):: grid

  allocate(hwork (s2g_d%inner_vars,s2g_d%nlat,s2g_d%nlon,s2g_d%kbegin_loc:s2g_d%kend_alloc))
  allocate(hworkd(s2g_d%inner_vars,s2g_d%nlat,s2g_d%nlon,s2g_d%kbegin_loc:s2g_d%kend_alloc))

!        use s2g_d%kend_alloc instead of s2g_d%kend_loc to force hworkd=0 even if not used on this pe
  ier=0
!             initialize hwork to zero, so can accumulate contribution from
!             all derivatives
  hwork=zero

!   adjoint of y derivative

  call gsi_gridcreate(grid,lat2,lon2,nsig)
  call gsi_bundlecreate (cstate,grid,'ad derivatives work',ier, &
                         names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
  if(ier/=0) then
     write(6,*) trim(myname), ': trouble creating work bundle'
     call stop2(999)
  endif
  call gsi_bundleputvar ( cstate, 'ps', p_y,    rc )
  call gsi_bundleputvar ( cstate, 'sf', u_y,    rc )
  call gsi_bundleputvar ( cstate, 'vp', v_y,    rc )
  call gsi_bundleputvar ( cstate, 't' , t_y,    rc )
  call gsi_bundleputvar ( cstate, 'q ', q_y,    rc )
  call gsi_bundleputvar ( cstate, 'oz', oz_y,   rc )
  call gsi_bundleputvar ( cstate, 'cw', cwmr_y, rc )
  call gsi_bundleputvar ( cstate, 'sst', skint, rc )

  call general_sub2grid(s2g_d,cstate%values,hworkd)
!     !$omp parallel do private(k,vector)   !  fix later ???????????
  do k=s2g_d%kbegin_loc,s2g_d%kend_loc
     if(regional) then
        call dely_reg(hworkd(1,:,:,k),hwork(1,:,:,k),s2g_d%vector(k))
     else
        call tcompact_dlat(hwork(1,:,:,k),hworkd(1,:,:,k),s2g_d%vector(k))
     end if
  end do
!     !$omp end parallel do   !  fix later ???????????

!   adjoint of x derivative

  call gsi_bundleputvar ( cstate, 'ps', p_x,    rc )
  call gsi_bundleputvar ( cstate, 'sf', u_x,    rc )
  call gsi_bundleputvar ( cstate, 'vp', v_x,    rc )
  call gsi_bundleputvar ( cstate, 't' , t_x,    rc )
  call gsi_bundleputvar ( cstate, 'q ', q_x,    rc )
  call gsi_bundleputvar ( cstate, 'oz', oz_x,   rc )
  call gsi_bundleputvar ( cstate, 'cw', cwmr_x, rc )
  call gsi_bundleputvar ( cstate, 'sst', skint, rc )

  call general_sub2grid(s2g_d,cstate%values,hworkd)
!     !$omp parallel do private(k,vector)   ! fix later ?????
  do k=s2g_d%kbegin_loc,s2g_d%kend_loc
     if(regional) then
        call delx_reg(hworkd(1,:,:,k),hwork(1,:,:,k),s2g_d%vector(k))
     else
        call tcompact_dlon(hwork(1,:,:,k),hworkd(1,:,:,k),s2g_d%vector(k))
     end if
  end do
!     !$omp end parallel do                 ! fix later ??????

!       use t_x,etc since don't need to save contents
  call general_grid2sub(s2g_d,hwork,cstate%values)
  call gsi_bundlegetvar ( cstate, 'ps', p_x,    rc )
  call gsi_bundlegetvar ( cstate, 'sf', u_x,    rc )
  call gsi_bundlegetvar ( cstate, 'vp', v_x,    rc )
  call gsi_bundlegetvar ( cstate, 't' , t_x,    rc )
  call gsi_bundlegetvar ( cstate, 'q ', q_x,    rc )
  call gsi_bundlegetvar ( cstate, 'oz', oz_x,   rc )
  call gsi_bundlegetvar ( cstate, 'cw', cwmr_x, rc )
  call gsi_bundlegetvar ( cstate, 'sst', skint, rc )

  call gsi_bundledestroy(cstate,ier)
  if(ier/=0) then
     write(6,*) trim(myname), ': trouble destroying work bundle'
     call stop2(999)
  endif

!   accumulate to contents of t,p,etc (except st,vp, which are zero on input
!     !$omp parallel do private(k,j,i)   ! fix later ????
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           t(i,j,k)=t(i,j,k)+t_x(i,j,k)
           q(i,j,k)=q(i,j,k)+q_x(i,j,k)
           u(i,j,k)=u(i,j,k)+u_x(i,j,k)
           v(i,j,k)=v(i,j,k)+v_x(i,j,k)
           oz(i,j,k)=oz(i,j,k)+oz_x(i,j,k)
           cwmr(i,j,k)=cwmr(i,j,k)+cwmr_x(i,j,k)
        end do
     end do
  end do
!       !$omp end parallel do      ! fix later ????
  do j=1,lon2
     do i=1,lat2
        p(i,j)=p(i,j)+p_x(i,j)
        skint(i,j)=skint(i,j)+skint_x(i,j)
     end do
  end do

  deallocate(hwork,hworkd)

end subroutine tget_derivatives


subroutine get_zderivs(z,z_x,z_y,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_zderivs    get derivatives of terrain
!   prgmmr: parrish          org: np22                date: 2005-09-29
!
! abstract: get derivatives od terrain field
!
! program history log:
!   2005-09-29  parrish
!   2005-12-05  todling - reorder passed variable declarations
!   2007-07-02  derber - modify for single time level and optimization
!   2008-06-04  safford - complete doc block
!   2010-04-01  treadon - move strip to gridmod
!
!   input argument list:
!     z         - terrain grid
!     mype      - integer task id
!     nfldsig   - number of time periods in terrain grid array
!
!   output argument list:
!     z_x       - zonal derivative of terrain field
!     z_y       - meridional derivative of terrain field
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional,nlat,nlon,lat2,lon2
  use compact_diffs, only: compact_dlat,compact_dlon
  use general_sub2grid_mod, only: general_gather2grid,general_scatter2sub
  use general_commvars_mod, only: g1
  implicit none

! Passed variables
  integer(i_kind)                  ,intent(in   ) :: mype
  real(r_kind),dimension(lat2,lon2),intent(in   ) :: z
  real(r_kind),dimension(lat2,lon2),intent(  out) :: z_x,z_y

! Local variables
  real(r_kind),dimension(:,:,:),allocatable:: workh,workd1,workd2
  real(r_kind),dimension(:),allocatable:: z1,zx1,zy1
  integer(i_kind) i,ii,j
  integer(i_kind) workpe

  workpe=0

  allocate(workh (g1%inner_vars,g1%nlat,g1%nlon))
  allocate(workd1(g1%inner_vars,g1%nlat,g1%nlon))
  allocate(workd2(g1%inner_vars,g1%nlat,g1%nlon))
  allocate(z1(g1%inner_vars*g1%nlat*g1%nlon))
  allocate(zx1(g1%inner_vars*g1%nlat*g1%nlon))
  allocate(zy1(g1%inner_vars*g1%nlat*g1%nlon))

  ii=0
  do j=1,lon2
     do i=1,lat2
        ii=ii+1
        z1(ii)=z(i,j)
     end do
  end do
  call general_gather2grid(g1,z1,workh,workpe)
  deallocate(z1)

  if(mype==workpe) then
     if(regional) then
        call delx_reg(workh,workd1,(.false.))
        call dely_reg(workh,workd2,(.false.))
     else
        call compact_dlon(workh,workd1,(.false.))
        call compact_dlat(workh,workd2,(.false.))
     end if
  end if
  deallocate(workh)

  call general_scatter2sub(g1,workd1,zx1,workpe)
  call general_scatter2sub(g1,workd2,zy1,workpe)
  deallocate(workd1,workd2)
  ii=0
  do j=1,lon2
     do i=1,lat2
        ii=ii+1
        z_x(i,j)=zx1(ii)
        z_y(i,j)=zy1(ii)
     end do
  end do

  deallocate(zx1,zy1)


  return
end subroutine get_zderivs
