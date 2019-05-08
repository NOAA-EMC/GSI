module ncdr_state
    use ncd_kinds, only: i_long, i_short
    use ncdr_types, only: ncdr_file
    
    implicit none
    
    integer(i_long) :: current_ncdr_id = -1
    integer(i_long), dimension(:), allocatable :: ncdr_id_stack
    integer(i_long) :: ncdr_id_stack_size = 0, ncdr_id_stack_count = 0
    logical :: init_done = .FALSE.
    
    character(len=200) :: cur_nc_file
    
    type(ncdr_file), dimension(:), allocatable :: ncdr_files
    integer(i_long)                            :: ncdr_file_count = 0
    integer(i_long)                            :: ncdr_file_total = 0
    integer(i_long)                            :: ncdr_file_highest = 0
    
    ! Default number of starting entries
    integer(i_short), parameter             :: NCDR_DEFAULT_ENT = 1024
    
    ! NetCDF chunking size
    integer(i_long), parameter              :: NCDR_CHUNKING = 16384
end module ncdr_state
