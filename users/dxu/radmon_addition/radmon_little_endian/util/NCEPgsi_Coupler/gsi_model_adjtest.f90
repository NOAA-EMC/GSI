subroutine gsi_model_adjtest(u,v,tv,q,oz,cw,ps,z,mype)

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
  use tends4pertmod, only: time_step,time_step_half,itime,itime_max,itime_out,ab_par
  use mpimod, only: levs_id
  use mpimod, only: ierror,mpi_rtype,mpi_sum,mpi_comm_world
  use dynamics_adams_bashforth, only: dynam_ab,dynam_ab_ad
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in) :: mype
  real(r_kind),dimension(lat2,lon2,nsig),intent(in) :: u,v,tv,q,oz,cw
  real(r_kind),dimension(lat2,lon2)     ,intent(in) :: ps,z

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig):: u_NL,v_NL,tv_NL,q_NL,oz_NL,cw_NL
  real(r_kind),dimension(lat2,lon2):: ps_NL

  real(r_kind),dimension(lat2,lon2,nsig):: u_AD,v_AD,tv_AD,q_AD,oz_AD,cw_AD
  real(r_kind),dimension(lat2,lon2):: ps_AD,z_AD

  real(r_kind),dimension(lat2,lon2,nsig):: u_TL,v_TL,tv_TL,q_TL,oz_TL,cw_TL
  real(r_kind),dimension(lat2,lon2):: ps_TL,z_TL

  real(r_kind),dimension(lat2,lon2,nsig):: u0_TL,v0_TL,tv0_TL,q0_TL,oz0_TL,cw0_TL
  real(r_kind),dimension(lat2,lon2):: ps0_TL,z0_TL

  integer(i_kind) i,j,jj,k,it,nnn,nout,nltl_mask,nit,iseed
  integer, allocatable :: nseed(:) ! Intentionaly default integer
  real(r_kind) seed,alpha,beta,alpha_all,beta_all 
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




!#############################################################################
    if(mype==0) write(6,*) &
 '--------- START NONLINEAR INTEGRATION    -----------------------------------'
!#############################################################################

        itime=0_i_kind

      call write_nonlinear(u_NL,v_NL,tv_NL,q_NL,oz_NL,cw_NL,ps_NL,itime,mype)    

    TIME_NL: do itime=1,itime_max

   if(mype==0) write(6,*) 'TIME_NL:    itime=',itime

    call dynam_ab(u_NL,v_NL,tv_NL,q_NL,oz_NL,cw_NL,ps_NL,z,nnn,mype,nltl_mask)
    call pbl(u_NL,v_NL,tv_NL,ps_NL,1,lon2)

    call write_nonlinear(u_NL,v_NL,tv_NL,q_NL,oz_NL,cw_NL,ps_NL,itime,mype)    

    end do TIME_NL
!#############################################################################
    if(mype==0) write(6,*) &
 '--------- FINISHED NONLINEAR INTEGRATION  ---------------------------------'
!#############################################################################


!#############################################################################
    if(mype==0) write(6,*) &
 '--------- START TANGENT LINEAR -----------------------------------'
!#############################################################################
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
   u0_TL (:,:,:)=0.01*(-0.5+zz(:,:,:))*u (:,:,:)
   call random_number(zz)
   v0_TL (:,:,:)=0.01*(-0.5+zz(:,:,:))*v (:,:,:)
   call random_number(zz)
   tv0_TL(:,:,:)=0.01*(-0.5+zz(:,:,:))*tv(:,:,:)
   call random_number(zz)
   q0_TL (:,:,:)=0.01*(-0.5+zz(:,:,:))*q (:,:,:)
   call random_number(zz)
   oz0_TL(:,:,:)=0.01*(-0.5+zz(:,:,:))*oz(:,:,:)
   call random_number(zz)
   cw0_TL(:,:,:)=0.01*(-0.5+zz(:,:,:))*cw(:,:,:)
   
   call random_number(zz)
   ps0_TL(:,:)=0.1*(-0.5+zz(:,:,1))*ps(:,:)
   z0_TL=zero

   deallocate(zz)

   nltl_mask=2

    do k=1,nsig
      do j=1,lon2
        do i=1,lat2
          u_TL(i,j,k)=u0_TL(i,j,k)   
          v_TL(i,j,k)=v0_TL(i,j,k)  
          tv_TL(i,j,k)=tv0_TL(i,j,k)
          q_TL(i,j,k)=q0_TL(i,j,k)    
          oz_TL(i,j,k)=oz0_TL(i,j,k)  
          cw_TL(i,j,k)=cw0_TL(i,j,k)  
        end do
      end do
    end do
      do j=1,lon2
        do i=1,lat2
          ps_TL(i,j)=ps0_TL(i,j)     
          z_TL (i,j)=zero
        end do
      end do

     call gsi_model_tl(u_TL,v_TL,tv_TL,q_TL,oz_TL,cw_TL,ps_TL,z_TL,mype,itime_max)

! Diagnostics

       alpha=zero
    do k=1,nsig
      do j=2,lon2-1
        do i=2,lat2-1
          alpha = alpha &
                      + u_TL(i,j,k)**2+v_TL(i,j,k)**2 &
                      + tv_TL(i,j,k)**2  &
                      + q_TL(i,j,k)**2+oz_TL(i,j,k)**2+cw_TL(i,j,k)**2 
        end do
      end do
    end do
      do j=2,lon2-1
        do i=2,lat2-1
          alpha = alpha & 
                + ps_TL(i,j)**2
        end do
      end do

!#############################################################################
    if(mype==0) write(6,*) &
 '--------- FINISHED TANGENT LINEAR -----------------------------------'
!#############################################################################

!#############################################################################
    if(mype==0) write(6,*) &
 '--------- START ADJOINT                  -----------------------------------'
!#############################################################################

    u_AD=u_TL;   v_AD=v_TL;   tv_AD=tv_TL
    q_AD=q_TL;   oz_AD=oz_TL;   cw_AD=cw_TL;   z_AD=z_TL
    ps_AD=ps_TL


    call gsi_model_ad(u_AD,v_AD,tv_AD,q_AD,oz_AD,cw_AD,ps_AD,z_AD,mype,itime_max)


!#############################################################################
    if(mype==0) write(6,*) &
 '--------- FINSHED ADJOINT                -----------------------------------'
!#############################################################################

! Diagnostics

          beta=zero
    do k=1,nsig
      do j=2,lon2-1
        do i=2,lat2-1
          beta = beta  &
               + u_AD(i,j,k)*u0_TL(i,j,k)+v_AD(i,j,k)*v0_TL(i,j,k) &
               + tv_AD(i,j,k)*tv0_TL(i,j,k)  &
               + q_AD(i,j,k)*q0_TL(i,j,k)+oz_AD(i,j,k)*oz0_TL(i,j,k) &
               + cw_AD(i,j,k)*cw0_TL(i,j,k)
        end do
      end do
    end do

     do j=2,lon2-1
       do i=2,lat2-1
         beta = beta + ps_AD(i,j)*ps0_TL(i,j)
       end do
     end do

   call mpi_reduce(alpha,alpha_all,1,mpi_rtype,mpi_sum,0,mpi_comm_world,ierror)
   if(mype==0)  write(300,*) 'ALPHA = ', alpha_all

   call mpi_reduce(beta,beta_all,1,mpi_rtype,mpi_sum,0,mpi_comm_world,ierror)
   if(mype==0)  write(300,*) 'BETA  = ', beta_all


! End of routine
  return

end subroutine gsi_model_adjtest
