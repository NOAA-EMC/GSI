module ncdc_state
    use ncd_kinds, only: i_long
    use ncdc_types, only: nc_diag_cat_dim_names, data_blob
    
    implicit none
    
#ifdef USE_MPI
    integer(i_long)                    :: cur_proc, num_procs, ierr
#endif
    
    character(len=10000000) :: prgm_name, dummy_arg, output_file, input_file
    integer(i_long)         :: cli_arg_count, input_count
    
    integer(i_long)                    :: ncid_output, ncid_input
    
    ! Dimension storage
    character(len=100), dimension(:), allocatable     :: dim_names
    integer(i_long),    dimension(:), allocatable     :: dim_sizes
    integer(i_long),    dimension(:), allocatable     :: dim_output_ids
    integer(i_long),    dimension(:), allocatable     :: dim_counters
    integer(i_long),    dimension(:), allocatable     :: dim_unlim_sizes
    
    ! Array storage info for dimension storage
    integer(i_long)                                   :: dim_arr_total = 0
    integer(i_long)                                   :: dim_arr_size = 0
    
    integer(i_long)                                   :: num_unlims
    
    ! dim_sizes(i) of -1 designates an unlimited dimension
    
    ! Variable dimensions storage
    ! See ncdc_realloc for nc_diag_cat_dim_names derived type def
    
    ! Variable storage
    character(len=100),    dimension(:), allocatable  :: var_names
    integer(i_long),       dimension(:), allocatable  :: var_types
    type(nc_diag_cat_dim_names), dimension(:), allocatable  :: var_dim_names
    integer(i_long),       dimension(:), allocatable  :: var_output_ids
    integer(i_long),       dimension(:), allocatable  :: var_counters
    logical,               dimension(:), allocatable  :: var_hasunlim
    
    ! Array storage info for variable storage
    integer(i_long)                                   :: var_arr_total = 0
    integer(i_long)                                   :: var_arr_size = 0
    
    ! Data blob stores entire variable's data!
    ! Indexing uses the metadata indexing system.
    type(data_blob),    dimension(:), allocatable     :: data_blobs
end module ncdc_state
