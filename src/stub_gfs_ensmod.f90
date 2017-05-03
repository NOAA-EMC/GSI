!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  GSI_EnsCouplerMod ---
!
! !DESCRIPTION: This stub provides the default interfaces to read an 
!               ensemble in GSI.
!
! !REVISION HISTORY:
!
!  19Sep2011 Todling - Initial code
!  01Dec2011 Todling - Add put_gsi_ens to allow write out of internal members
!  30Nov2014 Todling - Update interface to get (bundle passed in)
!
!EOP
!-------------------------------------------------------------------------

module get_gfs_ensmod_mod
use abstract_get_gfs_ensmod_mod
  type, extends(abstract_get_gfs_ensmod_class) :: get_gfs_ensmod_class
  contains
  procedure, pass(this) :: get_user_ens_ => get_user_ens_dummy
  procedure, pass(this) :: put_gsi_ens_ => put_gsi_ens_dummy
  procedure, pass(this) :: non_gaussian_ens_grid_ => non_gaussian_ens_grid_dummy
  end type get_gfs_ensmod_class
contains
  
  subroutine get_user_ens_dummy(this,grd,member,ntindex,atm_bundle,iret)
     use kinds, only: i_kind
     use general_sub2grid_mod, only: sub2grid_info
     use gsi_bundlemod, only: gsi_bundle
     implicit none
     ! Declare passed variables
     class(get_gfs_ensmod_class), intent(inout) :: this
     type(sub2grid_info), intent(in   ) :: grd
     integer(i_kind),     intent(in   ) :: member
     integer(i_kind),     intent(in   ) :: ntindex
     type(gsi_bundle),    intent(inout) :: atm_bundle
     integer(i_kind),     intent(  out) :: iret
     iret = 0
  end subroutine get_user_ens_dummy
  
  subroutine put_gsi_ens_dummy(this,grd,member,ntindex,atm_bundle,iret)
     use kinds, only: i_kind
     use gsi_bundlemod, only: gsi_bundle
     use general_sub2grid_mod, only: sub2grid_info
     implicit none
     ! Declare passed variables
     class(get_gfs_ensmod_class), intent(inout) :: this
     type(sub2grid_info), intent(in   ) :: grd
     integer(i_kind),     intent(in   ) :: member
     integer(i_kind),     intent(in   ) :: ntindex
     type(gsi_bundle),    intent(inout) :: atm_bundle
     integer(i_kind),     intent(  out) :: iret
     iret = 0
  end subroutine put_gsi_ens_dummy

  subroutine non_gaussian_ens_grid_dummy(this,elats,elons)
     use kinds, only: r_kind
     use hybrid_ensemble_parameters, only: sp_ens
     implicit none
     ! Declare passed variables
     class(get_gfs_ensmod_class), intent(inout) :: this
     real(r_kind), intent(out) :: elats(size(sp_ens%rlats)),elons(size(sp_ens%rlons))
     elats=sp_ens%rlats
     elons=sp_ens%rlons
  end subroutine non_gaussian_ens_grid_dummy

end module get_gfs_ensmod_mod
