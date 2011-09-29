subroutine gsi_model_tl(u_TL,v_TL,tv_TL,q_TL,oz_TL,cw_TL,ps_TL,z_TL,mype,indx_min,indx_max)

!$$$ subprogram documentation block
!               .      .    .                                      .
! subprogram:   gsi_model_tl      run tl of perturbation model for 4dvar 
!   prgmmr: rancic                          date: 2011-01-20
!
! abstract: Run tangent linear of perturbation model using, Adams-Bashforth 2nd, 3rd or 4th order
!           scheme for dynamics and implicit Crank-Nicolson scheme for pbl.
!           Read fields produced by nonlinear perturbation model
! 
! program history log:
!   2010-02-24  rancic started development
!   2011-01-20  rancic include in 4dvar interface
!
! usage: 
!   input argument list:
!     u_TL     - zonal wind on subdomain
!     v_TL     - meridional wind on subdomain  
!     tv_TL    - virtual temperature  on subdomain
!     q_TL     - moisture on subdomain
!     oz_TL    - ozone on subdomain
!     cw_TL    - cloud water on subdomain
!     ps_TL    - surface presure on subdomain
!     z_TL     - sfc terrain height  on subdomain
!     mype     - task id
!
!   output argument list:
! 
! $$$$    
  use kinds, only: r_kind,i_kind
  use constants, only: zero,one,two,half
  use gridmod, only: lat2,lon2,nsig,nsig1o
  use tends4pertmod, only: time_step,time_step_half,itime,ab_par
  use mpimod, only: levs_id
  use dynamics_adams_bashforth, only: dynam_ab
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: mype,indx_min,indx_max
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: u_TL,v_TL,tv_TL,q_TL,oz_TL,cw_TL
  real(r_kind),dimension(lat2,lon2)     ,intent(inout):: ps_TL,z_TL

! Declare local variables
  integer(i_kind) i,j,k,nnn,nltl_mask


! Set mask that controls choice of model (nonlinear = 1 ; tangent linear = 2)

   nltl_mask=2

! Used throughout

    nnn=0
    do k=1,nsig1o
      if (levs_id(k)/=0) nnn=nnn+1
    end do


    TIME_TL: do itime=indx_min,indx_max


! Read base fields produced by nonlinear model

      call read_nonlinear(itime,mype)

! Call dynamics

     call dynam_ab(u_TL,v_TL,tv_TL,q_TL,oz_TL,cw_TL,ps_TL,z_TL,nnn,mype,nltl_mask)

! Add contribution from vertical mixing

     call pbl_tl(u_TL,v_TL,tv_TL,ps_TL,1,lon2)

    end do TIME_TL


! End of routine
  return

end subroutine gsi_model_tl
