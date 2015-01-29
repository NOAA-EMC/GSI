!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  GSI_EnsCouplerMod ---
!
! !DESCRIPTION: This stub provides the default interfaces to the 
!               ensemble capability in GSI.
!
! !REVISION HISTORY:
!
!  19Sep2011 Todling - Initial code
!  01Dec2011 Todling - Add put_gsi_ens to allow write out of internal members
!
!EOP
!-------------------------------------------------------------------------

subroutine non_gaussian_ens_grid_ (elats,elons)
use kinds, only: i_kind,r_kind
use hybrid_ensemble_parameters, only: sp_ens
implicit none
real(r_kind),intent(out) :: elats(size(sp_ens%rlats)),elons(size(sp_ens%rlons))
elats=sp_ens%rlats
elons=sp_ens%rlons
end subroutine non_gaussian_ens_grid_

subroutine get_user_ens_(grd,member,g_z,g_ps,g_vor,g_div,g_u,g_v,g_tv,g_q,g_cwmr,g_oz,iret)
use kinds, only: i_kind,r_kind
use general_sub2grid_mod, only: sub2grid_info
implicit none
!  Declare passed variables
   type(sub2grid_info)                   ,intent(in   ) :: grd
   integer(i_kind)                       ,intent(in   ) :: member
   integer(i_kind)                       ,intent(  out) :: iret
   real(r_kind),dimension(grd%lat2,grd%lon2),intent(  out) :: g_z,g_ps
   real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out) :: g_u,g_v,&
        g_vor,g_div,g_cwmr,g_q,g_oz,g_tv
end subroutine get_user_ens_

subroutine put_gsi_ens_(member,nt,grd,pert,iret)
   use kinds, only: i_kind,r_kind
   use general_sub2grid_mod, only: sub2grid_info
   use gsi_bundlemod, only: gsi_bundle
   implicit none
!  Declare passed variables
      integer(i_kind),    intent(in   ) :: member ! member number
      integer(i_kind),    intent(in   ) :: nt     ! time index (mainly for 4d purposes)
      type(sub2grid_info),intent(in   ) :: grd    ! grid
      type(gsi_bundle),   intent(inout) :: pert   ! actual perturbation
      integer(i_kind),    intent(  out) :: iret   ! error return code
end subroutine put_gsi_ens_
