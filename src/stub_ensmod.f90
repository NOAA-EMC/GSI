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
!  30Nov2014 Todling - Update interface to get (bundle passed in)
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

subroutine get_user_ens_(grd,member,en_read,iret)
use kinds, only: i_kind,r_kind
use general_sub2grid_mod, only: sub2grid_info
use gsi_bundlemod, only: gsi_bundle
implicit none
!  Declare passed variables
   type(sub2grid_info)                   ,intent(in   ) :: grd
   integer(i_kind)                       ,intent(in   ) :: member
   integer(i_kind)                       ,intent(  out) :: iret
   type(gsi_bundle)                      ,intent(inout) :: en_read
   iret=0
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
   iret=0
end subroutine put_gsi_ens_
