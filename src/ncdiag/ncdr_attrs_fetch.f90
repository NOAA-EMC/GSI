module ncdr_attrs_fetch
    use ncd_kinds, only: i_byte, i_short, i_long, r_single, r_double
    use ncdr_state, only: ncdr_files, current_ncdr_id
    use ncdr_check, only: ncdr_nc_check, ncdr_check_ncdr_id, &
        ncdr_check_current_ncdr_id, ncdr_check_ncid
    use ncdr_alloc_assert, only: nc_diag_read_id_assert_var, &
        nc_diag_read_id_assert_attr, nc_diag_read_assert_attr_type, &
        nc_diag_read_assert_dims, nc_diag_read_assert_dims_alloc_string
    use netcdf, only: nf90_get_att, NF90_BYTE, NF90_SHORT, NF90_INT, &
        NF90_FLOAT, NF90_DOUBLE, NF90_CHAR
    
    implicit none
    
    interface nc_diag_read_get_attr
        ! Note that nc_diag_read_(no)id_get_attr_1d_string is not
        ! included due to rare use + conflict with single_string.
        module procedure &
            nc_diag_read_id_get_attr_single_byte, &
            nc_diag_read_id_get_attr_single_short, &
            nc_diag_read_id_get_attr_single_long, &
            nc_diag_read_id_get_attr_single_float, &
            nc_diag_read_id_get_attr_single_double, &
            nc_diag_read_id_get_attr_single_string, &
            nc_diag_read_noid_get_attr_single_byte, &
            nc_diag_read_noid_get_attr_single_short, &
            nc_diag_read_noid_get_attr_single_long, &
            nc_diag_read_noid_get_attr_single_float, &
            nc_diag_read_noid_get_attr_single_double, &
            nc_diag_read_noid_get_attr_single_string, &
            nc_diag_read_id_get_attr_1d_byte, &
            nc_diag_read_id_get_attr_1d_short, &
            nc_diag_read_id_get_attr_1d_long, &
            nc_diag_read_id_get_attr_1d_float, &
            nc_diag_read_id_get_attr_1d_double, &
            nc_diag_read_noid_get_attr_1d_byte, &
            nc_diag_read_noid_get_attr_1d_short, &
            nc_diag_read_noid_get_attr_1d_long, &
            nc_diag_read_noid_get_attr_1d_float, &
            nc_diag_read_noid_get_attr_1d_double
    end interface nc_diag_read_get_attr
    
    contains
        subroutine nc_diag_read_id_get_attr_single_byte(file_ncdr_id, var_name, attr_name, attr_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            integer(i_byte), intent(out) :: attr_stor
            
            integer(i_long)                            :: var_index, attr_len, attr_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            call nc_diag_read_id_assert_attr(file_ncdr_id, var_name, attr_name, attr_type, attr_len)
            
            call nc_diag_read_assert_attr_type(attr_type, NF90_BYTE)
            
            call nc_diag_read_assert_dims(attr_stor, (/ attr_len /))
            
            call ncdr_nc_check(nf90_get_att(file_ncid, &
                    ncdr_files(file_ncdr_id)%vars(var_index)%var_id, &
                    attr_name, &
                    attr_stor))
        end subroutine nc_diag_read_id_get_attr_single_byte
        
        subroutine nc_diag_read_noid_get_attr_single_byte(var_name, attr_name, attr_stor)
            character(len=*), intent(in)             :: var_name
            character(len=*), intent(in)             :: attr_name
            integer(i_byte), intent(out)             :: attr_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_attr_single_byte(current_ncdr_id, var_name, attr_name, attr_stor)
        end subroutine nc_diag_read_noid_get_attr_single_byte
        
        subroutine nc_diag_read_id_get_attr_single_short(file_ncdr_id, var_name, attr_name, attr_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            integer(i_short), intent(out)              :: attr_stor
            
            integer(i_long)                            :: var_index, attr_len, attr_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            call nc_diag_read_id_assert_attr(file_ncdr_id, var_name, attr_name, attr_type, attr_len)
            
            call nc_diag_read_assert_attr_type(attr_type, NF90_SHORT)
            
            call nc_diag_read_assert_dims(attr_stor, (/ attr_len /))
            
            call ncdr_nc_check(nf90_get_att(file_ncid, &
                    ncdr_files(file_ncdr_id)%vars(var_index)%var_id, &
                    attr_name, &
                    attr_stor))
        end subroutine nc_diag_read_id_get_attr_single_short
        
        subroutine nc_diag_read_noid_get_attr_single_short(var_name, attr_name, attr_stor)
            character(len=*), intent(in)             :: var_name
            character(len=*), intent(in)             :: attr_name
            integer(i_short), intent(out)            :: attr_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_attr_single_short(current_ncdr_id, var_name, attr_name, attr_stor)
        end subroutine nc_diag_read_noid_get_attr_single_short
        
        subroutine nc_diag_read_id_get_attr_single_long(file_ncdr_id, var_name, attr_name, attr_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            integer(i_long), intent(out)               :: attr_stor
            
            integer(i_long)                            :: var_index, attr_len, attr_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            call nc_diag_read_id_assert_attr(file_ncdr_id, var_name, attr_name, attr_type, attr_len)
            
            call nc_diag_read_assert_attr_type(attr_type, NF90_INT)
            
            call nc_diag_read_assert_dims(attr_stor, (/ attr_len /))
            
            call ncdr_nc_check(nf90_get_att(file_ncid, &
                    ncdr_files(file_ncdr_id)%vars(var_index)%var_id, &
                    attr_name, &
                    attr_stor))
        end subroutine nc_diag_read_id_get_attr_single_long
        
        subroutine nc_diag_read_noid_get_attr_single_long(var_name, attr_name, attr_stor)
            character(len=*), intent(in)             :: var_name
            character(len=*), intent(in)             :: attr_name
            integer(i_long), intent(out)             :: attr_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_attr_single_long(current_ncdr_id, var_name, attr_name, attr_stor)
        end subroutine nc_diag_read_noid_get_attr_single_long
        
        subroutine nc_diag_read_id_get_attr_single_float(file_ncdr_id, var_name, attr_name, attr_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            real(r_single), intent(out)                :: attr_stor
            
            integer(i_long)                            :: var_index, attr_len, attr_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            call nc_diag_read_id_assert_attr(file_ncdr_id, var_name, attr_name, attr_type, attr_len)
            
            call nc_diag_read_assert_attr_type(attr_type, NF90_FLOAT)
            
            call nc_diag_read_assert_dims(attr_stor, (/ attr_len /))
            
            call ncdr_nc_check(nf90_get_att(file_ncid, &
                    ncdr_files(file_ncdr_id)%vars(var_index)%var_id, &
                    attr_name, &
                    attr_stor))
        end subroutine nc_diag_read_id_get_attr_single_float
        
        subroutine nc_diag_read_noid_get_attr_single_float(var_name, attr_name, attr_stor)
            character(len=*), intent(in)             :: var_name
            character(len=*), intent(in)             :: attr_name
            real(r_single), intent(out)              :: attr_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_attr_single_float(current_ncdr_id, var_name, attr_name, attr_stor)
        end subroutine nc_diag_read_noid_get_attr_single_float
        
        subroutine nc_diag_read_id_get_attr_single_double(file_ncdr_id, var_name, attr_name, attr_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            real(r_double), intent(out)                :: attr_stor
            
            integer(i_long)                            :: var_index, attr_len, attr_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            call nc_diag_read_id_assert_attr(file_ncdr_id, var_name, attr_name, attr_type, attr_len)
            
            call nc_diag_read_assert_attr_type(attr_type, NF90_DOUBLE)
            
            call nc_diag_read_assert_dims(attr_stor, (/ attr_len /))
            
            call ncdr_nc_check(nf90_get_att(file_ncid, &
                    ncdr_files(file_ncdr_id)%vars(var_index)%var_id, &
                    attr_name, &
                    attr_stor))
        end subroutine nc_diag_read_id_get_attr_single_double
        
        subroutine nc_diag_read_noid_get_attr_single_double(var_name, attr_name, attr_stor)
            character(len=*), intent(in)             :: var_name
            character(len=*), intent(in)             :: attr_name
            real(r_double), intent(out)              :: attr_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_attr_single_double(current_ncdr_id, var_name, attr_name, attr_stor)
        end subroutine nc_diag_read_noid_get_attr_single_double
        
        subroutine nc_diag_read_id_get_attr_single_string(file_ncdr_id, var_name, attr_name, attr_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            character(len=*), intent(out)              :: attr_stor
            
            integer(i_long)                            :: var_index, attr_len, attr_type, file_ncid
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            call nc_diag_read_id_assert_attr(file_ncdr_id, var_name, attr_name, attr_type, attr_len)
            
            call nc_diag_read_assert_attr_type(attr_type, NF90_CHAR)
            
            call nc_diag_read_assert_dims(attr_stor, (/ attr_len /))
            
            call ncdr_nc_check(nf90_get_att(file_ncid, &
                    ncdr_files(file_ncdr_id)%vars(var_index)%var_id, &
                    attr_name, &
                    attr_stor))
        end subroutine nc_diag_read_id_get_attr_single_string
        
        subroutine nc_diag_read_noid_get_attr_single_string(var_name, attr_name, attr_stor)
            character(len=*), intent(in)             :: var_name
            character(len=*), intent(in)             :: attr_name
            character(len=*), intent(out)            :: attr_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_attr_single_string(current_ncdr_id, var_name, attr_name, attr_stor)
        end subroutine nc_diag_read_noid_get_attr_single_string
        
        subroutine nc_diag_read_id_get_attr_1d_byte(file_ncdr_id, var_name, attr_name, attr_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            integer(i_byte), dimension(:), allocatable, intent(inout) :: attr_stor
            
            integer(i_long)                            :: var_index, attr_len, attr_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            call nc_diag_read_id_assert_attr(file_ncdr_id, var_name, attr_name, attr_type, attr_len)
            
            call nc_diag_read_assert_attr_type(attr_type, NF90_BYTE)
            
            call nc_diag_read_assert_dims(attr_stor, (/ attr_len /))
            
            call ncdr_nc_check(nf90_get_att(file_ncid, &
                    ncdr_files(file_ncdr_id)%vars(var_index)%var_id, &
                    attr_name, &
                    attr_stor))
        end subroutine nc_diag_read_id_get_attr_1d_byte
        
        subroutine nc_diag_read_noid_get_attr_1d_byte(var_name, attr_name, attr_stor)
            character(len=*), intent(in)             :: var_name
            character(len=*), intent(in)             :: attr_name
            integer(i_byte), dimension(:), allocatable, intent(inout) :: attr_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_attr_1d_byte(current_ncdr_id, var_name, attr_name, attr_stor)
        end subroutine nc_diag_read_noid_get_attr_1d_byte
        
        subroutine nc_diag_read_id_get_attr_1d_short(file_ncdr_id, var_name, attr_name, attr_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            integer(i_short), dimension(:), allocatable, intent(inout) :: attr_stor
            
            integer(i_long)                            :: var_index, attr_len, attr_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            call nc_diag_read_id_assert_attr(file_ncdr_id, var_name, attr_name, attr_type, attr_len)
            
            call nc_diag_read_assert_attr_type(attr_type, NF90_SHORT)
            
            call nc_diag_read_assert_dims(attr_stor, (/ attr_len /))
            
            call ncdr_nc_check(nf90_get_att(file_ncid, &
                    ncdr_files(file_ncdr_id)%vars(var_index)%var_id, &
                    attr_name, &
                    attr_stor))
        end subroutine nc_diag_read_id_get_attr_1d_short
        
        subroutine nc_diag_read_noid_get_attr_1d_short(var_name, attr_name, attr_stor)
            character(len=*), intent(in)             :: var_name
            character(len=*), intent(in)             :: attr_name
            integer(i_short), dimension(:), allocatable, intent(inout) :: attr_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_attr_1d_short(current_ncdr_id, var_name, attr_name, attr_stor)
        end subroutine nc_diag_read_noid_get_attr_1d_short
        
        subroutine nc_diag_read_id_get_attr_1d_long(file_ncdr_id, var_name, attr_name, attr_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            integer(i_long), dimension(:), allocatable, intent(inout) :: attr_stor
            
            integer(i_long)                            :: var_index, attr_len, attr_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            call nc_diag_read_id_assert_attr(file_ncdr_id, var_name, attr_name, attr_type, attr_len)
            
            call nc_diag_read_assert_attr_type(attr_type, NF90_INT)
            
            call nc_diag_read_assert_dims(attr_stor, (/ attr_len /))
            
            call ncdr_nc_check(nf90_get_att(file_ncid, &
                    ncdr_files(file_ncdr_id)%vars(var_index)%var_id, &
                    attr_name, &
                    attr_stor))
        end subroutine nc_diag_read_id_get_attr_1d_long
        
        subroutine nc_diag_read_noid_get_attr_1d_long(var_name, attr_name, attr_stor)
            character(len=*), intent(in)             :: var_name
            character(len=*), intent(in)             :: attr_name
            integer(i_long), dimension(:), allocatable, intent(inout) :: attr_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_attr_1d_long(current_ncdr_id, var_name, attr_name, attr_stor)
        end subroutine nc_diag_read_noid_get_attr_1d_long
        
        subroutine nc_diag_read_id_get_attr_1d_float(file_ncdr_id, var_name, attr_name, attr_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            real(r_single), dimension(:), allocatable, intent(inout) :: attr_stor
            
            integer(i_long)                            :: var_index, attr_len, attr_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            call nc_diag_read_id_assert_attr(file_ncdr_id, var_name, attr_name, attr_type, attr_len)
            
            call nc_diag_read_assert_attr_type(attr_type, NF90_FLOAT)
            
            call nc_diag_read_assert_dims(attr_stor, (/ attr_len /))
            
            call ncdr_nc_check(nf90_get_att(file_ncid, &
                    ncdr_files(file_ncdr_id)%vars(var_index)%var_id, &
                    attr_name, &
                    attr_stor))
        end subroutine nc_diag_read_id_get_attr_1d_float
        
        subroutine nc_diag_read_noid_get_attr_1d_float(var_name, attr_name, attr_stor)
            character(len=*), intent(in)             :: var_name
            character(len=*), intent(in)             :: attr_name
            real(r_single), dimension(:), allocatable, intent(inout) :: attr_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_attr_1d_float(current_ncdr_id, var_name, attr_name, attr_stor)
        end subroutine nc_diag_read_noid_get_attr_1d_float
        
        subroutine nc_diag_read_id_get_attr_1d_double(file_ncdr_id, var_name, attr_name, attr_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            real(r_double), dimension(:), allocatable, intent(inout) :: attr_stor
            
            integer(i_long)                            :: var_index, attr_len, attr_type, file_ncid
                        
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            call nc_diag_read_id_assert_attr(file_ncdr_id, var_name, attr_name, attr_type, attr_len)
            
            call nc_diag_read_assert_attr_type(attr_type, NF90_DOUBLE)
            
            call nc_diag_read_assert_dims(attr_stor, (/ attr_len /))
            
            call ncdr_nc_check(nf90_get_att(file_ncid, &
                    ncdr_files(file_ncdr_id)%vars(var_index)%var_id, &
                    attr_name, &
                    attr_stor))
        end subroutine nc_diag_read_id_get_attr_1d_double
        
        subroutine nc_diag_read_noid_get_attr_1d_double(var_name, attr_name, attr_stor)
            character(len=*), intent(in)             :: var_name
            character(len=*), intent(in)             :: attr_name
            real(r_double), dimension(:), allocatable, intent(inout) :: attr_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_attr_1d_double(current_ncdr_id, var_name, attr_name, attr_stor)
        end subroutine nc_diag_read_noid_get_attr_1d_double
        
        subroutine nc_diag_read_id_get_attr_1d_string(file_ncdr_id, var_name, attr_name, attr_stor)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            character(len=:),allocatable,intent(inout) :: attr_stor
            
            integer(i_long)                            :: var_index, attr_len, attr_type, file_ncid
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            file_ncid = ncdr_files(file_ncdr_id)%ncid
            call ncdr_check_ncid(file_ncid)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            call nc_diag_read_id_assert_attr(file_ncdr_id, var_name, attr_name, attr_type, attr_len)
            
            call nc_diag_read_assert_attr_type(attr_type, NF90_CHAR)
            
            call nc_diag_read_assert_dims_alloc_string(attr_stor, (/ attr_len /))
            
            call ncdr_nc_check(nf90_get_att(file_ncid, &
                    ncdr_files(file_ncdr_id)%vars(var_index)%var_id, &
                    attr_name, &
                    attr_stor))
        end subroutine nc_diag_read_id_get_attr_1d_string
        
        subroutine nc_diag_read_noid_get_attr_1d_string(var_name, attr_name, attr_stor)
            character(len=*), intent(in)             :: var_name
            character(len=*), intent(in)             :: attr_name
            character(len=:),allocatable,intent(inout) :: attr_stor
            
            call ncdr_check_current_ncdr_id
            call nc_diag_read_id_get_attr_1d_string(current_ncdr_id, var_name, attr_name, attr_stor)
        end subroutine nc_diag_read_noid_get_attr_1d_string
end module ncdr_attrs_fetch
