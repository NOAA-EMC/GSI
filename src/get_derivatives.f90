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
  use constants, only: ione,zero
  use gridmod, only: regional,nlat,nlon,lat2,lon2,nsig,nsig1o
  use compact_diffs, only: compact_dlat,compact_dlon
  use mpimod, only: nvar_id
  use control_vectors, only: control_state,allocate_cs,deallocate_cs, &
                             assign_array2cs,assign_cs2array

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
  integer(i_kind) iflg,k,i,j,it
  real(r_kind),dimension(lat2,lon2):: slndt,sicet
  real(r_kind),dimension(lat2,lon2):: slndt_x,sicet_x
  real(r_kind),dimension(lat2,lon2):: slndt_y,sicet_y
  real(r_kind),dimension(nlat,nlon,nsig1o):: hwork,hworkd
  type(control_state):: cstate
  logical vector

  iflg=ione
  slndt=zero
  sicet=zero

  if(nsig1o > nlevs)then
     do k=nlevs+ione,nsig1o
        do j=1,nlon
           do i=1,nlat
              hworkd(i,j,k) = zero
           end do
        end do
     end do
  end if

  do it=1,nfldsig
     call allocate_cs(cstate)
     call assign_array2cs(cstate,u,v,t,q,oz,cwmr,p(1,1,it))
     call sub2grid(hwork,cstate,skint,slndt,sicet,iflg)


!    x derivative
!$omp parallel do private(vector)
     do k=1,nlevs
        vector = nvar_id(k) == ione .or. nvar_id(k) == 2_i_kind
        if(regional) then
           call delx_reg(hwork(1,1,k),hworkd(1,1,k),vector)
        else
           call compact_dlon(hwork(1,1,k),hworkd(1,1,k),vector)
        end if
     end do
!$omp end parallel do
     call grid2sub(hworkd,cstate,skint_x,slndt_x,sicet_x)
     call assign_cs2array(cstate,u_x,v_x,t_x,q_x,oz_x,cwmr_x,p_x(1,1,it))

!    y derivative
!$omp parallel do private(vector)
     do k=1,nlevs
        vector = nvar_id(k) == ione .or. nvar_id(k) == 2_i_kind
        if(regional) then
           call dely_reg(hwork(1,1,k),hworkd(1,1,k),vector)
        else
           call compact_dlat(hwork(1,1,k),hworkd(1,1,k),vector)
        end if
     end do
!$omp end parallel do
     call grid2sub(hworkd,cstate,skint_y,slndt_y,sicet_y)
     call assign_cs2array(cstate,u_y,v_y,t_y,q_y,oz_y,cwmr_y,p_y(1,1,it))
     call deallocate_cs(cstate)
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
  use constants, only: ione,zero
  use gridmod, only: regional,nlat,nlon,lat2,lon2,nsig,nsig1o
  use compact_diffs, only: tcompact_dlat,tcompact_dlon
  use mpimod, only: nvar_id
  use control_vectors, only: control_state,allocate_cs,deallocate_cs, &
            assign_array2cs,assign_cs2array
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
  integer(i_kind) iflg,k,i,j
  real(r_kind),dimension(lat2,lon2):: slndt_x,sicet_x
  real(r_kind),dimension(lat2,lon2):: slndt_y,sicet_y
  real(r_kind),dimension(nlat,nlon,nsig1o):: hwork,hworkd
  type(control_state):: cstate
  logical vector

  iflg=ione
!             initialize hwork to zero, so can accumulate contribution from
!             all derivatives
  hwork=zero
!             for now zero out slndt,sicet
  slndt_x=zero
  sicet_x=zero
  slndt_y=zero
  sicet_y=zero

!   adjoint of y derivative

  call allocate_cs(cstate)
  call assign_array2cs(cstate,u_y,v_y,t_y,q_y,oz_y,cwmr_y,p_y)
  call sub2grid(hworkd,cstate,skint_y,slndt_y,sicet_y,iflg)
!$omp parallel do private(vector)
  do k=1,nlevs
     vector = nvar_id(k) == ione .or. nvar_id(k) == 2_i_kind
     if(regional) then
        call dely_reg(hworkd(1,1,k),hwork(1,1,k),vector)
     else
        call tcompact_dlat(hwork(1,1,k),hworkd(1,1,k),vector)
     end if
  end do
!$omp end parallel do

!   adjoint of x derivative

  call assign_array2cs(cstate,u_x,v_x,t_x,q_x,oz_x,cwmr_x,p_x)
  call sub2grid(hworkd,cstate,skint_x,slndt_x,sicet_x,iflg)
!$omp parallel do private(vector)
  do k=1,nlevs
     vector = nvar_id(k) == ione .or. nvar_id(k) == 2_i_kind
     if(regional) then
        call delx_reg(hworkd(1,1,k),hwork(1,1,k),vector)
     else
        call tcompact_dlon(hwork(1,1,k),hworkd(1,1,k),vector)
     end if
  end do
!$omp end parallel do

!       use t_x,etc since don't need to save contents
  call grid2sub(hwork,cstate,skint_x,slndt_x,sicet_x)
  call assign_cs2array(cstate,u_x,v_x,t_x,q_x,oz_x,cwmr_x,p_x)
  call deallocate_cs(cstate)

!   accumulate to contents of t,p,etc (except st,vp, which are zero on input
!$omp parallel do
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
  use constants, only: izero,ione,zero
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

  mm1=mype+ione

  do j=1,lon1*lat1
     zsm(j)=zero
  end do
 
  call strip(z,zsm,ione)
  call mpi_gatherv(zsm,ijn(mm1),mpi_rtype,&
     work1,ijn,displs_g,mpi_rtype,&
     izero,mpi_comm_world,ierror)

  if (mype==izero) then
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
       z_x,ijn_s(mm1),mpi_rtype,izero,mpi_comm_world,ierror)

  call mpi_scatterv(work2,ijn_s,displs_s,mpi_rtype,&
       z_y,ijn_s(mm1),mpi_rtype,izero,mpi_comm_world,ierror)


  return
end subroutine get_zderivs
