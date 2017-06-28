module abstract_get_gfs_ensmod_mod
  type, abstract :: abstract_get_gfs_ensmod_class
  contains
    procedure(get_user_ens_), deferred, pass(this) :: get_user_ens_
    procedure(put_gsi_ens_), deferred, pass(this) :: put_gsi_ens_
    procedure(non_gaussian_ens_grid_), deferred, pass(this) :: non_gaussian_ens_grid_
  end type abstract_get_gfs_ensmod_class

  abstract interface
  subroutine get_user_ens_(this,grd,member,ntindex,atm_bundle,iret)
    use kinds, only: i_kind
    use general_sub2grid_mod, only: sub2grid_info
    use gsi_bundlemod, only: gsi_bundle
    import abstract_get_gfs_ensmod_class
    implicit none
    class(abstract_get_gfs_ensmod_class), intent(inout) :: this
    type(sub2grid_info), intent(in   ) :: grd
    integer(i_kind),     intent(in   ) :: member
    integer(i_kind),     intent(in   ) :: ntindex
    type(gsi_bundle),    intent(inout) :: atm_bundle                      
    integer(i_kind),     intent(  out) :: iret
  end subroutine get_user_ens_
  end interface

  abstract interface
  subroutine put_gsi_ens_(this,grd,member,ntindex,atm_bundle,iret)
    use kinds, only: i_kind
    use gsi_bundlemod, only: gsi_bundle
    use general_sub2grid_mod, only: sub2grid_info
    import abstract_get_gfs_ensmod_class
    implicit none
    class(abstract_get_gfs_ensmod_class), intent(inout) :: this
    type(sub2grid_info), intent(in   ) :: grd
    integer(i_kind),     intent(in   ) :: member
    integer(i_kind),     intent(in   ) :: ntindex
    type(gsi_bundle),    intent(inout) :: atm_bundle
    integer(i_kind),     intent(  out) :: iret

  end subroutine put_gsi_ens_
  end interface

  abstract interface
  subroutine non_gaussian_ens_grid_(this,elats,elons)
    use kinds, only: r_kind
    use hybrid_ensemble_parameters, only: sp_ens
    import abstract_get_gfs_ensmod_class
    implicit none
    class(abstract_get_gfs_ensmod_class), intent(inout) :: this
    real(r_kind), intent(out) :: elats(size(sp_ens%rlats)),elons(size(sp_ens%rlons))
  end subroutine non_gaussian_ens_grid_
  end interface

end module abstract_get_gfs_ensmod_mod
