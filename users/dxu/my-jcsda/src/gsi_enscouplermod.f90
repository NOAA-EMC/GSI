!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  GSI_EnsCouplerMod ---
!
! !INTERFACE:

module GSI_EnsCouplerMod

! !USES:

use gsi_bundlemod, only: gsi_bundle
implicit none
private

!
! !PUBLIC MEMBER FUNCTIONS:
!
public GSI_EnsCoupler_localization_grid
public GSI_EnsCoupler_get_user_ens
public GSI_EnsCoupler_put_gsi_ens


interface gsi_enscoupler_localization_grid
   subroutine non_gaussian_ens_grid_ (elats,elons)
   use kinds, only: i_kind,r_kind
   use gridmod, only: rlats,rlons
   implicit none
   real(r_kind),intent(out) :: elats(size(rlats)),elons(size(rlons)) ! worse hack ever
   end subroutine non_gaussian_ens_grid_
end interface

interface gsi_enscoupler_get_user_ens
   subroutine get_user_ens_(grd,member,ntindex,g_z,g_ps,g_vor,g_div,g_u,g_v,g_tv,g_q,g_cwmr,g_oz,iret)
   use kinds, only: i_kind,r_kind
   use general_sub2grid_mod, only: sub2grid_info
   implicit none
!  Declare passed variables
      type(sub2grid_info)                   ,intent(in   ) :: grd
      integer(i_kind)                       ,intent(in   ) :: member  ! member index
      integer(i_kind)                       ,intent(in   ) :: ntindex ! time index
      integer(i_kind)                       ,intent(  out) :: iret
      real(r_kind),dimension(grd%lat2,grd%lon2),intent(  out) :: g_z,g_ps
      real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out) :: g_u,g_v,&
           g_vor,g_div,g_cwmr,g_q,g_oz,g_tv
   end subroutine get_user_ens_
end interface

interface gsi_enscoupler_put_gsi_ens
   subroutine put_gsi_ens_(member,nt,grd,pert,iret)
   use kinds, only: i_kind,r_kind
   use general_sub2grid_mod, only: sub2grid_info
   use gsi_bundlemod, only: gsi_bundle
   implicit none
!  Declare passed variables
      integer(i_kind),    intent(in   ) :: member
      integer(i_kind),    intent(in   ) :: nt
      type(sub2grid_info),intent(in   ) :: grd
      type(gsi_bundle),   intent(inout) :: pert
      integer(i_kind),    intent(  out) :: iret
   end subroutine put_gsi_ens_
end interface



! !DESCRIPTION: This module provides general interface for
!               ensemble capability
!
! !REVISION HISTORY:
!
!  19Sep2011 Todling - Initial code
!
!EOP
!-------------------------------------------------------------------------
end module GSI_EnsCouplerMod
