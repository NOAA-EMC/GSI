subroutine gsi_model_ad(u_AD,v_AD,tv_AD,q_AD,oz_AD,cw_AD,ps_AD,z_AD,mype,indx_min,indx_max) 

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
!
!     u_AD     - zonal wind on subdomain
!     v_AD     - meridional wind on subdomain  
!     tv_AD    - virtual temperature  on subdomain
!     q_AD     - moisture on subdomain
!     oz_AD    - ozone on subdomain
!     cw_AD    - cloud water on subdomain
!     ps_AD    - surface presure on subdomain
!     z_AD     - sfc terrain height  on subdomain
!     mype     - task id
!
!   output argument list:
! 
! $$$$    
  use kinds, only: r_kind,i_kind
  use constants, only: zero,one,two,half
  use gridmod, only: lat2,lon2,nsig,nsig1o
  use tends4pertmod, only: itime
  use mpimod, only: levs_id
  use dynamics_adams_bashforth, only: dynam_ab_ad
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in) :: mype,indx_min,indx_max
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: u_AD,v_AD,tv_AD,q_AD,oz_AD,cw_AD
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: ps_AD,z_AD

! Declare local variables
  real(r_kind),dimension(lat2,lon2):: ps1_AD

  integer(i_kind) i,j,k,it,nnn


! Used throughout
    nnn=0
    do k=1,nsig1o
      if (levs_id(k)/=0) nnn=nnn+1
    end do

    ps1_AD=zero


    TIME_ADJ: do itime=indx_max,indx_min,-1

        call read_nonlinear(itime,mype)


! Add contribution from vertical mixing

    call pbl_ad(u_AD,v_AD,tv_AD,ps1_AD,1,lon2)

    ps_AD=ps_AD+ps1_AD

! Call dynamics

    call dynam_ab_ad(u_AD,v_AD,tv_AD,q_AD,oz_AD,cw_AD,ps_AD,z_AD,nnn,mype)

   end do TIME_ADJ


! End of routine
  return

end subroutine gsi_model_ad
