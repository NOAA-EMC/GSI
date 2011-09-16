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
  use gridmod, only: regional,nlat,nlon,lat2,lon2,nsig,nsig1o
  use compact_diffs, only: compact_dlat,compact_dlon
  use mpimod, only: nvar_id
  use control_vectors, only: nrf_var
  use control_vectors, only: cvars2d
  use control_vectors, only: cvars3d
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetvar
  use gsi_bundlemod, only: gsi_bundleputvar
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_gridcreate

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
  integer(i_kind) iflg,k,i,j,it,ii,rc,ier
  real(r_kind),dimension(lat2,lon2):: slndt,sicet
  real(r_kind),dimension(lat2,lon2):: slndt_x,sicet_x
  real(r_kind),dimension(lat2,lon2):: slndt_y,sicet_y
  real(r_kind),dimension(nlat,nlon,nsig1o):: hwork,hworkd
  type(gsi_bundle):: cstate
  type(gsi_grid) :: grid
  logical vector

  iflg=1
  ier=0
  slndt=zero
  sicet=zero

  if(nsig1o > nlevs)then
     do k=nlevs+1,nsig1o
        do j=1,nlon
           do i=1,nlat
              hworkd(i,j,k) = zero
           end do
        end do
     end do
  end if

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

     call sub2grid(hwork,cstate,skint,slndt,sicet,iflg)


!    x derivative
!$omp parallel do private(k,vector)
     do k=1,nlevs
        vector = nrf_var(nvar_id(k)) == 'sf' .or. nrf_var(nvar_id(k)) == 'vp'
        if(regional) then
           call delx_reg(hwork(1,1,k),hworkd(1,1,k),vector)
        else
           call compact_dlon(hwork(1,1,k),hworkd(1,1,k),vector)
        end if
     end do
!$omp end parallel do
     call grid2sub(hworkd,cstate,skint_x,slndt_x,sicet_x)
     call gsi_bundlegetvar ( cstate, 'ps' , p_x(:,:,it), rc )
     call gsi_bundlegetvar ( cstate, 'sf' , u_x,         rc )
     call gsi_bundlegetvar ( cstate, 'vp' , v_x,         rc )
     call gsi_bundlegetvar ( cstate, 't'  , t_x,         rc )
     call gsi_bundlegetvar ( cstate, 'q ' , q_x,         rc )
     call gsi_bundlegetvar ( cstate, 'oz' , oz_x,        rc )
     call gsi_bundlegetvar ( cstate, 'cw' , cwmr_x,      rc )

!    y derivative
!$omp parallel do private(k,vector)
     do k=1,nlevs
        vector = nrf_var(nvar_id(k)) == 'sf' .or. nrf_var(nvar_id(k)) == 'vp'
        if(regional) then
           call dely_reg(hwork(1,1,k),hworkd(1,1,k),vector)
        else
           call compact_dlat(hwork(1,1,k),hworkd(1,1,k),vector)
        end if
     end do
!$omp end parallel do
     call grid2sub(hworkd,cstate,skint_y,slndt_y,sicet_y)
     call gsi_bundlegetvar ( cstate, 'ps' , p_y(:,:,it), rc )
     call gsi_bundlegetvar ( cstate, 'sf' , u_y,         rc )
     call gsi_bundlegetvar ( cstate, 'vp' , v_y,         rc )
     call gsi_bundlegetvar ( cstate, 't'  , t_y,         rc )
     call gsi_bundlegetvar ( cstate, 'q ' , q_y,         rc )
     call gsi_bundlegetvar ( cstate, 'oz' , oz_y,        rc )
     call gsi_bundlegetvar ( cstate, 'cw' , cwmr_y,      rc )

!    clean work space
     call gsi_bundledestroy(cstate,ier)
     if(ier/=0) then
        write(6,*) trim(myname), ': trouble destroying work bundle'
        call stop2(999)
     endif
  end do  ! end do it

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
  use gridmod, only: regional,nlat,nlon,lat2,lon2,nsig,nsig1o
  use compact_diffs, only: tcompact_dlat,tcompact_dlon
  use mpimod, only: nvar_id
  use control_vectors, only: nrf_var
  use control_vectors, only: cvars2d
  use control_vectors, only: cvars3d
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetvar
  use gsi_bundlemod, only: gsi_bundleputvar
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_gridcreate
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
  integer(i_kind) iflg,k,i,j,ii,rc,ier
  real(r_kind),dimension(lat2,lon2):: slndt_x,sicet_x
  real(r_kind),dimension(lat2,lon2):: slndt_y,sicet_y
  real(r_kind),dimension(nlat,nlon,nsig1o):: hwork,hworkd
  type(gsi_bundle):: cstate
  type(gsi_grid):: grid
  logical vector

  iflg=1
  ier=0
!             initialize hwork to zero, so can accumulate contribution from
!             all derivatives
  hwork=zero
!             for now zero out slndt,sicet
  slndt_x=zero
  sicet_x=zero
  slndt_y=zero
  sicet_y=zero

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

  call sub2grid(hworkd,cstate,skint_y,slndt_y,sicet_y,iflg)
!$omp parallel do private(k,vector)
  do k=1,nlevs
     vector = nrf_var(nvar_id(k)) == 'sf' .or. nrf_var(nvar_id(k)) == 'vp'
     if(regional) then
        call dely_reg(hworkd(1,1,k),hwork(1,1,k),vector)
     else
        call tcompact_dlat(hwork(1,1,k),hworkd(1,1,k),vector)
     end if
  end do
!$omp end parallel do

!   adjoint of x derivative

  call gsi_bundleputvar ( cstate, 'ps', p_x,    rc )
  call gsi_bundleputvar ( cstate, 'sf', u_x,    rc )
  call gsi_bundleputvar ( cstate, 'vp', v_x,    rc )
  call gsi_bundleputvar ( cstate, 't' , t_x,    rc )
  call gsi_bundleputvar ( cstate, 'q ', q_x,    rc )
  call gsi_bundleputvar ( cstate, 'oz', oz_x,   rc )
  call gsi_bundleputvar ( cstate, 'cw', cwmr_x, rc )

  call sub2grid(hworkd,cstate,skint_x,slndt_x,sicet_x,iflg)
!$omp parallel do private(k,vector)
  do k=1,nlevs
     vector = nrf_var(nvar_id(k)) == 'sf' .or. nrf_var(nvar_id(k)) == 'vp'
     if(regional) then
        call delx_reg(hworkd(1,1,k),hwork(1,1,k),vector)
     else
        call tcompact_dlon(hwork(1,1,k),hworkd(1,1,k),vector)
     end if
  end do
!$omp end parallel do

!       use t_x,etc since don't need to save contents
  call grid2sub(hwork,cstate,skint_x,slndt_x,sicet_x)
  call gsi_bundlegetvar ( cstate, 'ps', p_x,    rc )
  call gsi_bundlegetvar ( cstate, 'sf', u_x,    rc )
  call gsi_bundlegetvar ( cstate, 'vp', v_x,    rc )
  call gsi_bundlegetvar ( cstate, 't' , t_x,    rc )
  call gsi_bundlegetvar ( cstate, 'q ', q_x,    rc )
  call gsi_bundlegetvar ( cstate, 'oz', oz_x,   rc )
  call gsi_bundlegetvar ( cstate, 'cw', cwmr_x, rc )

  call gsi_bundledestroy(cstate,ier)
  if(ier/=0) then
     write(6,*) trim(myname), ': trouble destroying work bundle'
     call stop2(999)
  endif

!   accumulate to contents of t,p,etc (except st,vp, which are zero on input
!$omp parallel do private(k,j,i)
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
!$omp end parallel do
  do j=1,lon2
     do i=1,lat2
        p(i,j)=p(i,j)+p_x(i,j)
        skint(i,j)=skint(i,j)+skint_x(i,j)
     end do
  end do

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
  use gridmod, only: regional,nlat,nlon,lat2,lon2,lat1,lon1,&
     displs_s,ltosj_s,ijn_s,ltosi,ltosj,iglobal,ltosi_s,itotsub,&
     ijn,displs_g,strip
  use compact_diffs, only: compact_dlat,compact_dlon
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none

! Passed variables
  integer(i_kind)                  ,intent(in   ) :: mype
  real(r_kind),dimension(lat2,lon2),intent(in   ) :: z
  real(r_kind),dimension(lat2,lon2),intent(  out) :: z_x,z_y

! Local variables
  real(r_kind),dimension(lat1*lon1):: zsm
  real(r_kind),dimension(itotsub):: work1,work2
  real(r_kind),dimension(nlat,nlon):: workh,workd1,workd2
  integer(i_kind) mm1,i,j,k

  mm1=mype+1

  do j=1,lon1*lat1
     zsm(j)=zero
  end do
 
  call strip(z,zsm,1)
  call mpi_gatherv(zsm,ijn(mm1),mpi_rtype,&
     work1,ijn,displs_g,mpi_rtype,&
     0,mpi_comm_world,ierror)

  if (mype==0) then
     do k=1,iglobal
        i=ltosi(k) ; j=ltosj(k)
        workh(i,j)=work1(k)
     end do
     if(regional) then
        call delx_reg(workh,workd1,(.false.))
        call dely_reg(workh,workd2,(.false.))
     else
        call compact_dlon(workh,workd1,(.false.))
        call compact_dlat(workh,workd2,(.false.))
     end if
     do k=1,itotsub
        i=ltosi_s(k) ; j=ltosj_s(k)
        work1(k)=workd1(i,j)
        work2(k)=workd2(i,j)
     end do
  end if
  call mpi_scatterv(work1,ijn_s,displs_s,mpi_rtype,&
       z_x,ijn_s(mm1),mpi_rtype,0,mpi_comm_world,ierror)

  call mpi_scatterv(work2,ijn_s,displs_s,mpi_rtype,&
       z_y,ijn_s(mm1),mpi_rtype,0,mpi_comm_world,ierror)


  return
end subroutine get_zderivs
