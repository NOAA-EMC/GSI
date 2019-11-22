module ncdr_vars_fetch
    use ncd_kinds, only: i_byte, i_short, i_long, r_single, r_double
    use ncdr_state, only: ncdr_files, current_ncdr_id
    use ncdr_check, only: ncdr_nc_check, ncdr_check_ncdr_id, &
        ncdr_check_current_ncdr_id, ncdr_check_ncid
    use ncdr_alloc_assert, only: nc_diag_read_id_assert_var, &
        nc_diag_read_assert_var_type, nc_diag_read_assert_var_ndims, &
        nc_diag_read_assert_dims
    use netcdf, only: nf90_get_var, NF90_BYTE, NF90_SHORT, NF90_INT, &
        NF90_FLOAT, NF90_DOUBLE, NF90_CHAR
    
    implicit none
    
    interface nc_diag_read_get_var
        module procedure &
            nc_diag_read_id_get_var_1d_byte, &
            nc_diag_read_id_get_var_1d_short, &
            nc_diag_read_id_get_var_1d_long, &
            nc_diag_read_id_get_var_1d_float, &
            nc_diag_read_id_get_var_1d_double, &
            nc_diag_read_id_get_var_1d_string, &
            nc_diag_read_noid_get_var_1d_byte, &
            nc_diag_read_noid_get_var_1d_short, &
            nc_diag_read_noid_get_var_1d_long, &
            nc_diag_read_noid_get_var_1d_float, &
            nc_diag_read_noid_get_var_1d_double, &
            nc_diag_read_noid_get_var_1d_string, &
            nc_diag_read_id_get_var_2d_byte, &
            nc_diag_read_id_get_var_2d_short, &
            nc_diag_read_id_get_var_2d_long, &
            nc_diag_read_id_get_var_2d_float, &
            nc_diag_read_id_get_var_2d_double, &
            nc_diag_read_id_get_var_2d_string, &
            nc_diag_read_noid_get_var_2d_byte, &
            nc_diag_read_noid_get_var_2d_short, &
            nc_diag_read_noid_get_var_2d_long, &
            nc_diag_read_noid_get_var_2d_float, &
            nc_diag_read_noid_get_var_2d_double, &
            nc_diag_read_noid_get_var_2d_string
    end interface nc_diag_read_get_var
    
    contains
        subroutine nc_diag_read_id_get_var_1d_byte(file_ncdr_id, var_name, var_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            integer(i_byte), dimension(:), allocatable, intent(inout) :: var_stor
            
            integer(i_long)                            :: var_index, var_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            var_type  = ncdr_files(file_ncdr_id)%vars(var_index)%var_type
                        
            call nc_diag_read_assert_var_type(var_type, NF90_BYTE)
            call nc_diag_read_assert_var_ndims(size( &
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes), 1)
            
            call nc_diag_read_assert_dims(var_stor, & 
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes)
            
            call ncdr_nc_check(nf90_get_var(file_ncid, var_index, &
                    var_stor, &
                    start = (/ 1 /), &
                    count = (/ ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(1) /) ))
        end subroutine nc_diag_read_id_get_var_1d_byte
        
        subroutine nc_diag_read_noid_get_var_1d_byte(var_name, var_stor)
            character(len=*), intent(in)             :: var_name
            integer(i_byte), dimension(:), allocatable, intent(inout) :: var_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_var_1d_byte(current_ncdr_id, var_name, var_stor)
        end subroutine nc_diag_read_noid_get_var_1d_byte
        
        subroutine nc_diag_read_id_get_var_1d_short(file_ncdr_id, var_name, var_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            integer(i_short), dimension(:), allocatable, intent(inout) :: var_stor
            
            integer(i_long)                            :: var_index, var_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            var_type  = ncdr_files(file_ncdr_id)%vars(var_index)%var_type
                        
            call nc_diag_read_assert_var_type(var_type, NF90_SHORT)
            call nc_diag_read_assert_var_ndims(size( &
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes), 1)
            
            call nc_diag_read_assert_dims(var_stor, & 
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes)
            
            call ncdr_nc_check(nf90_get_var(file_ncid, var_index, &
                    var_stor, &
                    start = (/ 1 /), &
                    count = (/ ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(1) /) ))
        end subroutine nc_diag_read_id_get_var_1d_short
        
        subroutine nc_diag_read_noid_get_var_1d_short(var_name, var_stor)
            character(len=*), intent(in)             :: var_name
            integer(i_short), dimension(:), allocatable, intent(inout) :: var_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_var_1d_short(current_ncdr_id, var_name, var_stor)
        end subroutine nc_diag_read_noid_get_var_1d_short
        
        subroutine nc_diag_read_id_get_var_1d_long(file_ncdr_id, var_name, var_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            integer(i_long), dimension(:), allocatable, intent(inout) :: var_stor
            
            integer(i_long)                            :: var_index, var_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            var_type  = ncdr_files(file_ncdr_id)%vars(var_index)%var_type
                        
            call nc_diag_read_assert_var_type(var_type, NF90_INT)
            call nc_diag_read_assert_var_ndims(size( &
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes), 1)
            
            call nc_diag_read_assert_dims(var_stor, & 
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes)
            
            call ncdr_nc_check(nf90_get_var(file_ncid, var_index, &
                    var_stor, &
                    start = (/ 1 /), &
                    count = (/ ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(1) /) ))
        end subroutine nc_diag_read_id_get_var_1d_long
        
        subroutine nc_diag_read_noid_get_var_1d_long(var_name, var_stor)
            character(len=*), intent(in)             :: var_name
            integer(i_long), dimension(:), allocatable, intent(inout) :: var_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_var_1d_long(current_ncdr_id, var_name, var_stor)
        end subroutine nc_diag_read_noid_get_var_1d_long
        
        subroutine nc_diag_read_id_get_var_1d_float(file_ncdr_id, var_name, var_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            real(r_single), dimension(:), allocatable, intent(inout) :: var_stor
            
            integer(i_long)                            :: var_index, var_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            var_type  = ncdr_files(file_ncdr_id)%vars(var_index)%var_type
                        
            call nc_diag_read_assert_var_type(var_type, NF90_FLOAT)
            call nc_diag_read_assert_var_ndims(size( &
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes), 1)
            
            call nc_diag_read_assert_dims(var_stor, & 
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes)
            
            call ncdr_nc_check(nf90_get_var(file_ncid, var_index, &
                    var_stor, &
                    start = (/ 1 /), &
                    count = (/ ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(1) /) ))
        end subroutine nc_diag_read_id_get_var_1d_float
        
        subroutine nc_diag_read_noid_get_var_1d_float(var_name, var_stor)
            character(len=*), intent(in)             :: var_name
            real(r_single), dimension(:), allocatable, intent(inout) :: var_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_var_1d_float(current_ncdr_id, var_name, var_stor)
        end subroutine nc_diag_read_noid_get_var_1d_float
        
        subroutine nc_diag_read_id_get_var_1d_double(file_ncdr_id, var_name, var_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            real(r_double), dimension(:), allocatable, intent(inout) :: var_stor
            
            integer(i_long)                            :: var_index, var_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            var_type  = ncdr_files(file_ncdr_id)%vars(var_index)%var_type
                        
            call nc_diag_read_assert_var_type(var_type, NF90_DOUBLE)
            call nc_diag_read_assert_var_ndims(size( &
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes), 1)
            
            call nc_diag_read_assert_dims(var_stor, & 
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes)
            
            call ncdr_nc_check(nf90_get_var(file_ncid, var_index, &
                    var_stor, &
                    start = (/ 1 /), &
                    count = (/ ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(1) /) ))
        end subroutine nc_diag_read_id_get_var_1d_double
        
        subroutine nc_diag_read_noid_get_var_1d_double(var_name, var_stor)
            character(len=*), intent(in)             :: var_name
            real(r_double), dimension(:), allocatable, intent(inout) :: var_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_var_1d_double(current_ncdr_id, var_name, var_stor)
        end subroutine nc_diag_read_noid_get_var_1d_double
        
        subroutine nc_diag_read_id_get_var_1d_string(file_ncdr_id, var_name, var_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            character(len=:), dimension(:), allocatable, intent(inout) :: var_stor
            
            integer(i_long)                            :: var_index, var_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            var_type  = ncdr_files(file_ncdr_id)%vars(var_index)%var_type
                        
            call nc_diag_read_assert_var_type(var_type, NF90_CHAR)
            call nc_diag_read_assert_var_ndims(size( &
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes), 2)
            
            call nc_diag_read_assert_dims(var_stor, & 
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes)
            
            call ncdr_nc_check(nf90_get_var(file_ncid, var_index, &
                    var_stor, &
                    start = (/ 1, 1 /), &
                    count = (/ ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(1), &
                        ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(2) /) ))
        end subroutine nc_diag_read_id_get_var_1d_string
        
        subroutine nc_diag_read_noid_get_var_1d_string(var_name, var_stor)
            character(len=*), intent(in)             :: var_name
            character(len=:), dimension(:), allocatable, intent(inout) :: var_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_var_1d_string(current_ncdr_id, var_name, var_stor)
        end subroutine nc_diag_read_noid_get_var_1d_string
        
        subroutine nc_diag_read_id_get_var_2d_byte(file_ncdr_id, var_name, var_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            integer(i_byte), dimension(:,:),allocatable,intent(inout) :: var_stor
            
            integer(i_long)                            :: var_index, var_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            var_type  = ncdr_files(file_ncdr_id)%vars(var_index)%var_type
                        
            call nc_diag_read_assert_var_type(var_type, NF90_BYTE)
            call nc_diag_read_assert_var_ndims(size( &
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes), 2)
            
            call nc_diag_read_assert_dims(var_stor, & 
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes)
            
            call ncdr_nc_check(nf90_get_var(file_ncid, var_index, &
                    var_stor, &
                    start = (/ 1 /), &
                    count = (/ ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(1), &
                        ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(2) /) ))
        end subroutine nc_diag_read_id_get_var_2d_byte
        
        subroutine nc_diag_read_noid_get_var_2d_byte(var_name, var_stor)
            character(len=*), intent(in)             :: var_name
            integer(i_byte), dimension(:,:),allocatable,intent(inout) :: var_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_var_2d_byte(current_ncdr_id, var_name, var_stor)
        end subroutine nc_diag_read_noid_get_var_2d_byte
        
        subroutine nc_diag_read_id_get_var_2d_short(file_ncdr_id, var_name, var_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            integer(i_short), dimension(:,:),allocatable,intent(inout) :: var_stor
            
            integer(i_long)                            :: var_index, var_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            var_type  = ncdr_files(file_ncdr_id)%vars(var_index)%var_type
                        
            call nc_diag_read_assert_var_type(var_type, NF90_SHORT)
            call nc_diag_read_assert_var_ndims(size( &
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes), 2)
            
            call nc_diag_read_assert_dims(var_stor, & 
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes)
            
            call ncdr_nc_check(nf90_get_var(file_ncid, var_index, &
                    var_stor, &
                    start = (/ 1 /), &
                    count = (/ ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(1), &
                        ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(2) /) ))
        end subroutine nc_diag_read_id_get_var_2d_short
        
        subroutine nc_diag_read_noid_get_var_2d_short(var_name, var_stor)
            character(len=*), intent(in)             :: var_name
            integer(i_short), dimension(:,:),allocatable,intent(inout) :: var_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_var_2d_short(current_ncdr_id, var_name, var_stor)
        end subroutine nc_diag_read_noid_get_var_2d_short
        
        subroutine nc_diag_read_id_get_var_2d_long(file_ncdr_id, var_name, var_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            integer(i_long), dimension(:,:),allocatable,intent(inout) :: var_stor
            
            integer(i_long)                            :: var_index, var_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            var_type  = ncdr_files(file_ncdr_id)%vars(var_index)%var_type
                        
            call nc_diag_read_assert_var_type(var_type, NF90_INT)
            call nc_diag_read_assert_var_ndims(size( &
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes), 2)
            
            call nc_diag_read_assert_dims(var_stor, & 
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes)
            
            call ncdr_nc_check(nf90_get_var(file_ncid, var_index, &
                    var_stor, &
                    start = (/ 1 /), &
                    count = (/ ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(1), &
                        ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(2) /) ))
        end subroutine nc_diag_read_id_get_var_2d_long
        
        subroutine nc_diag_read_noid_get_var_2d_long(var_name, var_stor)
            character(len=*), intent(in)             :: var_name
            integer(i_long), dimension(:,:),allocatable,intent(inout) :: var_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_var_2d_long(current_ncdr_id, var_name, var_stor)
        end subroutine nc_diag_read_noid_get_var_2d_long
        
        subroutine nc_diag_read_id_get_var_2d_float(file_ncdr_id, var_name, var_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            real(r_single), dimension(:,:),allocatable,intent(inout) :: var_stor
            
            integer(i_long)                            :: var_index, var_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            var_type  = ncdr_files(file_ncdr_id)%vars(var_index)%var_type
                        
            call nc_diag_read_assert_var_type(var_type, NF90_FLOAT)
            call nc_diag_read_assert_var_ndims(size( &
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes), 2)
            
            call nc_diag_read_assert_dims(var_stor, & 
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes)
            
            call ncdr_nc_check(nf90_get_var(file_ncid, var_index, &
                    var_stor, &
                    start = (/ 1 /), &
                    count = (/ ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(1), &
                        ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(2) /) ))
        end subroutine nc_diag_read_id_get_var_2d_float
        
        subroutine nc_diag_read_noid_get_var_2d_float(var_name, var_stor)
            character(len=*), intent(in)             :: var_name
            real(r_single), dimension(:,:),allocatable,intent(inout) :: var_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_var_2d_float(current_ncdr_id, var_name, var_stor)
        end subroutine nc_diag_read_noid_get_var_2d_float
        
        subroutine nc_diag_read_id_get_var_2d_double(file_ncdr_id, var_name, var_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            real(r_double), dimension(:,:),allocatable,intent(inout) :: var_stor
            
            integer(i_long)                            :: var_index, var_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            var_type  = ncdr_files(file_ncdr_id)%vars(var_index)%var_type
                        
            call nc_diag_read_assert_var_type(var_type, NF90_DOUBLE)
            call nc_diag_read_assert_var_ndims(size( &
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes), 2)
            
            call nc_diag_read_assert_dims(var_stor, & 
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes)
            
            call ncdr_nc_check(nf90_get_var(file_ncid, var_index, &
                    var_stor, &
                    start = (/ 1 /), &
                    count = (/ ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(1), &
                        ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(2) /) ))
        end subroutine nc_diag_read_id_get_var_2d_double
        
        subroutine nc_diag_read_noid_get_var_2d_double(var_name, var_stor)
            character(len=*), intent(in)             :: var_name
            real(r_double), dimension(:,:),allocatable,intent(inout) :: var_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_var_2d_double(current_ncdr_id, var_name, var_stor)
        end subroutine nc_diag_read_noid_get_var_2d_double
        
        subroutine nc_diag_read_id_get_var_2d_string(file_ncdr_id, var_name, var_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            character(len=:), dimension(:,:),allocatable,intent(inout) :: var_stor
            
            integer(i_long)                            :: var_index, var_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            var_type  = ncdr_files(file_ncdr_id)%vars(var_index)%var_type
            
            call nc_diag_read_assert_var_type(var_type, NF90_CHAR)
            call nc_diag_read_assert_var_ndims(size( &
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes), 3)
            
            call nc_diag_read_assert_dims(var_stor, & 
                ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes)
            
            call ncdr_nc_check(nf90_get_var(file_ncid, var_index, &
                    var_stor, &
                    start = (/ 1, 1, 1 /), &
                    count = (/ ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(1), &
                        ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(2), &
                        ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_sizes(3) /) ))
        end subroutine nc_diag_read_id_get_var_2d_string
        
        subroutine nc_diag_read_noid_get_var_2d_string(var_name, var_stor)
            character(len=*), intent(in)             :: var_name
            character(len=:), dimension(:,:),allocatable,intent(inout) :: var_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_var_2d_string(current_ncdr_id, var_name, var_stor)
        end subroutine nc_diag_read_noid_get_var_2d_string
end module ncdr_vars_fetch
