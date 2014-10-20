subroutine gsi_model_tlmtest(u,v,tv,q,oz,cw,ps,z,mype)

!$$$ subprogram documentation block
!               .      .    .                                      .
! subprogram:   gsi_model_nl      run perturbation model for 4dvar 
!   prgmmr: rancic                          date: 2010-02-24
!
! abstract: Run perturbation model using Adams-Bashforth 2nd, 3rd or 4th order
!           scheme for dynamics and implicit Crank-Nicolson scheme for pbl
!           and save produced fields needed for tlm and adm
! 
! program history log:
!   2010-02-24  rancic 
!   2011-05-19  todling - use FORTRAN random number generator
!
! usage: 
!   input argument list:
!     u     - zonal wind on subdomain
!     v     - meridional wind on subdomain  
!     tv    - virtual temperature  on subdomain
!     q     - moisture on subdomain
!     oz    - ozone on subdomain
!     cw    - cloud water on subdomain
!     ps    - surface presure on subdomain
!     z     - sfc terrain height  on subdomain
!     mype     - task id
!
!   output argument list:
! 
! $$$$    
  use kinds, only: r_kind,i_kind
  use constants, only: zero,one,two,half
  use gridmod, only: lat2,lon2,nsig,nsig1o
  use tends4pertmod, only: time_step,time_step_half,itime,itime_out,itime_max,ab_par
  use mpimod, only: levs_id
  use mpimod, only: ierror,mpi_rtype,mpi_sum,mpi_comm_world
  use dynamics_adams_bashforth, only: dynam_ab
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in) :: mype
  real(r_kind),dimension(lat2,lon2,nsig),intent(in) :: u,v,tv,q,oz,cw
  real(r_kind),dimension(lat2,lon2)     ,intent(in) :: ps,z

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig):: u_NL,v_NL,tv_NL,q_NL,oz_NL,cw_NL
  real(r_kind),dimension(lat2,lon2):: ps_NL

  real(r_kind),dimension(lat2,lon2,nsig):: u_NL2,v_NL2,tv_NL2,q_NL2,oz_NL2,cw_NL2
  real(r_kind),dimension(lat2,lon2):: ps_NL2

  real(r_kind),dimension(lat2,lon2,nsig):: u_TL,v_TL,tv_TL,q_TL,oz_TL,cw_TL
  real(r_kind),dimension(lat2,lon2):: ps_TL,z_TL

  real(r_kind),dimension(lat2,lon2,nsig):: u0,v0,tv0,q0,oz0,cw0
  real(r_kind),dimension(lat2,lon2):: ps0,z0

  integer(i_kind) i,j,jj,k,it,nnn,nout,nltl_mask,nit,iseed
  integer, allocatable :: nseed(:) ! Intentionaly default integer
  real(r_kind) seed,alpha,sumln,sumnl,sumln_all,sumnl_all
  real(r_kind), allocatable :: zz(:,:,:)


! Set mask that controls choice of model (nonlinear = 1 ; tangent linear = 2)

   nltl_mask=1_i_kind       
   nout=itime_out

! Used throughout
    nnn=0
    do k=1,nsig1o
      if (levs_id(k)/=0) nnn=nnn+1
    end do

! Copy arrays
   
   u_NL=u ; v_NL=v ; tv_NL=tv 
   q_NL=q ; oz_NL=oz ; cw_NL=cw ; ps_NL=ps 

! Initialize perturbation variables

   iseed=123
   call random_seed(size=jj)
   allocate(nseed(jj))
!  The following because we don't want all procs to get
!  exactly the same sequence (which would be repeated in
!  the then not so random vector) but it makes the test
!  not reproducible if the number of procs is changed.
   nseed(1:jj)=iseed + mype
   call random_seed(put=nseed)
   deallocate(nseed)
 
   allocate(zz(lat2,lon2,nsig))

   call random_number(zz)
   u0 (:,:,:)=0.1*(-0.5+zz(:,:,:))*u_NL (:,:,:)
   call random_number(zz)
   v0 (:,:,:)=0.1*(-0.5+zz(:,:,:))*v_NL (:,:,:)
   call random_number(zz)
   tv0(:,:,:)=0.1*(-0.5+zz(:,:,:))*tv_NL(:,:,:)
   call random_number(zz)
   q0 (:,:,:)=    (-0.5+zz(:,:,:))*q_NL (:,:,:)
   call random_number(zz)
   oz0(:,:,:)=    (-0.5+zz(:,:,:))*oz_NL(:,:,:)
   call random_number(zz)
   cw0(:,:,:)=    (-0.5+zz(:,:,:))*cw_NL(:,:,:)
   call random_number(zz)
   ps0(:,:)=0.1*(-0.5+zz(:,:,1))*ps_NL(:,:)
   z0=zero

   deallocate(zz)

!#############################################################################
    if(mype==0) write(6,*) &
 '--------- START NONLINEAR INTEGRATION 1  -----------------------------------'
!#############################################################################

        itime=0

      call write_bkgvars_grid_mod(u_NL,v_NL,tv_NL,ps_NL,itime,mype,'bkgvar_')

      call write_nonlinear(u_NL,v_NL,tv_NL,q_NL,oz_NL,cw_NL,ps_NL,itime,mype)    

    TIME_NL: do itime=1,itime_max

   if(mype==0) write(6,*) 'itime=',itime

    call dynam_ab(u_NL,v_NL,tv_NL,q_NL,oz_NL,cw_NL,ps_NL,z,nnn,mype,nltl_mask)

!-----------------------------------------------------------------
    if(mype==0) write(6,*) 'AFTER dynam_ab: itime=',itime
!-----------------------------------------------------------------

    call pbl(u_NL,v_NL,tv_NL,ps_NL,1,lon2)

!-----------------------------------------------------------------
    if(mype==0) write(6,*) 'AFTER pbl: itime=',itime
!-----------------------------------------------------------------

    call write_nonlinear(u_NL,v_NL,tv_NL,q_NL,oz_NL,cw_NL,ps_NL,itime,mype)    

!!     if(mod(itime,nout)==0) then
!!       call write_bkgvars_grid_mod(u_NL,v_NL,tv_NL,ps_NL,itime,mype,'bkgvar_')
!!     end if

  end do TIME_NL
!#############################################################################
    if(mype==0) write(6,*) &
 '--------- FINISHED NONLINEAR INTEGRATION 1 ---------------------------------'
!#############################################################################

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
        alpha=10._r_kind
      ITER_LOOP: do nit=1,12
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
        alpha=alpha*0.1_r_kind

!#############################################################################
    if(mype==0) write(6,*) 'ALPHA=',alpha 
    if(mype==0) write(6,*) &
 '--------- START TANGENT LINEAR -----------------------------------'
!#############################################################################

   nltl_mask=2

    do k=1,nsig
      do j=1,lon2
        do i=1,lat2
          u_TL(i,j,k)=u0(i,j,k)    *alpha
          v_TL(i,j,k)=v0(i,j,k)    *alpha
          tv_TL(i,j,k)=tv0(i,j,k)  *alpha
          q_TL(i,j,k)=q0(i,j,k)    *alpha
          oz_TL(i,j,k)=oz0(i,j,k)  *alpha
          cw_TL(i,j,k)=cw0(i,j,k)  *alpha
        end do
      end do
    end do
      do j=1,lon2
        do i=1,lat2
          ps_TL(i,j)=ps0(i,j)      *alpha
          z_TL (i,j)=zero
        end do
      end do

      call gsi_model_tl(u_TL,v_TL,tv_TL,q_TL,oz_TL,cw_TL,ps_TL,z_TL,mype,itime_max)

!#############################################################################
    if(mype==0) write(6,*) &
 '--------- FINISHED TANGENT LINEAR -----------------------------------'
!#############################################################################

!#############################################################################
    if(mype==0) write(6,*) &
 '--------- START NONLINEAR INTEGRATION 2  -----------------------------------'
!#############################################################################

   nltl_mask=1

  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        u_NL2(i,j,k)=u(i,j,k)+u0(i,j,k)*alpha
        v_NL2(i,j,k)=v(i,j,k)+v0(i,j,k)*alpha
        tv_NL2(i,j,k)=tv(i,j,k)+tv0(i,j,k)*alpha
        q_NL2(i,j,k)=q(i,j,k)+q0(i,j,k)*alpha
        oz_NL2(i,j,k)=oz(i,j,k)+oz0(i,j,k)*alpha
        cw_NL2(i,j,k)=cw(i,j,k)+cw0(i,j,k)*alpha
      end do
    end do
  end do
    do j=1,lon2
      do i=1,lat2
        ps_NL2(i,j)=ps(i,j)+ps0(i,j)*alpha
      end do
    end do


    TIME_NL2: do itime=1,itime_max

! Call dynamics

   call dynam_ab(u_NL2,v_NL2,tv_NL2,q_NL2,oz_NL2,cw_NL2,ps_NL2,z,nnn,mype,nltl_mask)

! Add contribution from vertical mixing

        call pbl(u_NL2,v_NL2,tv_NL2,ps_NL2,1,lon2)

  end do TIME_NL2

!#############################################################################
    if(mype==0) write(6,*) &
 '--------- FINSH NONLINEAR INTEGRATION 2  -----------------------------------'
!#############################################################################

! Diagnostics

    sumnl=zero ; sumln=zero

    do k=1,nsig
      do j=1,lon2
        do i=1,lat2
          sumnl = sumnl  &
                + sqrt((u_NL2(i,j,k)-u_NL(i,j,k))**two) &
                + sqrt((v_NL2(i,j,k)-v_NL(i,j,k))**two) &
                + sqrt((tv_NL2(i,j,k)-tv_NL(i,j,k))**two) &
                + sqrt((q_NL2(i,j,k)-q_NL(i,j,k))**two)   &
                + sqrt((oz_NL2(i,j,k)-oz_NL(i,j,k))**two) &
                + sqrt((cw_NL2(i,j,k)-cw_NL(i,j,k))**two)

          sumln = sumln  &
                + sqrt(u_TL(i,j,k)**two)  &
                + sqrt(v_TL(i,j,k)**two)  &
                + sqrt(tv_TL(i,j,k)**two) &
                + sqrt(q_TL(i,j,k)**two)  &
                + sqrt(oz_TL(i,j,k)**two) &
                + sqrt(cw_TL(i,j,k)**two)
        end do
      end do
    end do
      do j=1,lon2
        do i=1,lat2
          sumnl = sumnl  &
                + sqrt((ps_NL2(i,j)-ps_NL(i,j))**two)
         sumln = sumln  &
                + sqrt(ps_TL(i,j)**two)
       end do
     end do


!m=============================================================================
  call mpi_reduce(sumnl,sumnl_all,1,mpi_rtype,mpi_sum,0,mpi_comm_world,ierror)
  call mpi_reduce(sumln,sumln_all,1,mpi_rtype,mpi_sum,0,mpi_comm_world,ierror)

   if(mype==0)  write(300,*) 'ALPHA = ', alpha
   if(mype==0)  write(300,*) 'SUM_nl, SUM_lin = ',sumnl_all,sumln_all
   if(mype==0)  write(300,*) 'Ratio = ', sumnl_all/sumln_all
!!   if(mype==0)  write(300,*) 'time_step = ', time_step
!!   if(mype==0)  write(300,*) 'itime_max  = ', itime_max

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         end do ITER_LOOP
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

! End of routine
  return

end subroutine gsi_model_tlmtest
