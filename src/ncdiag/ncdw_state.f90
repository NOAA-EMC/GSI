module ncdw_state
    use ncd_kinds, only: i_long
    use ncdw_types, only: diag_chaninfo, diag_metadata, &
        diag_data2d, diag_varattr
    
    implicit none
    
    integer(i_long) :: ncid
    logical         :: init_done = .FALSE.
    logical         :: append_only = .FALSE.
    
    logical         :: enable_trim = .FALSE.
    
    character(len=200) :: cur_nc_file
    
    type(diag_chaninfo), allocatable :: diag_chaninfo_store
    type(diag_metadata), allocatable :: diag_metadata_store
    type(diag_data2d),   allocatable :: diag_data2d_store
    type(diag_varattr),  allocatable :: diag_varattr_store
end module ncdw_state
