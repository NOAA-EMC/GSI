module ncdr_vars
    use ncd_kinds, only: i_long
    use ncdr_state, only: ncdr_files, current_ncdr_id
    use ncdr_climsg, only: ncdr_error
    use ncdr_check, only: ncdr_nc_check, ncdr_check_ncdr_id, &
        ncdr_check_current_ncdr_id
    use ncdr_alloc_assert, only: nc_diag_read_id_assert_var
    use netcdf, only: nf90_inquire_variable, NF90_MAX_NAME
    
    implicit none
    
    interface nc_diag_read_lookup_var
        module procedure nc_diag_read_id_lookup_var, &
            nc_diag_read_noid_lookup_var
    end interface nc_diag_read_lookup_var
    
    interface nc_diag_read_check_var
        module procedure nc_diag_read_id_check_var, &
            nc_diag_read_noid_check_var
    end interface nc_diag_read_check_var
    
    interface nc_diag_read_get_var_ndims
        module procedure nc_diag_read_id_get_var_ndims, &
            nc_diag_read_noid_get_var_ndims
    end interface nc_diag_read_get_var_ndims
    
    interface nc_diag_read_get_var_type
        module procedure nc_diag_read_id_get_var_type, &
            nc_diag_read_noid_get_var_type
    end interface nc_diag_read_get_var_type
    
    interface nc_diag_read_ret_var_dims
        module procedure nc_diag_read_id_ret_var_dims, &
            nc_diag_read_noid_ret_var_dims
    end interface nc_diag_read_ret_var_dims
    
    interface nc_diag_read_get_var_dims
        module procedure nc_diag_read_id_get_var_dims, &
            nc_diag_read_noid_get_var_dims
    end interface nc_diag_read_get_var_dims
    
    interface nc_diag_read_get_var_names
        module procedure nc_diag_read_id_get_var_names
    end interface nc_diag_read_get_var_names
    
    interface nc_diag_read_noid_get_var_names
        module procedure nc_diag_read_noid_get_var_names
    end interface nc_diag_read_noid_get_var_names

    contains
        subroutine nc_diag_read_parse_file_vars(file_ncid, file_index, num_vars)
            integer(i_long), intent(in)                :: file_ncid
            integer(i_long), intent(in)                :: file_index
            integer(i_long), intent(in)                :: num_vars
            
            integer(i_long)                            :: i, j
            
            character(len=NF90_MAX_NAME)               :: var_name
            
            ncdr_files(file_index)%nvars = num_vars
            allocate(ncdr_files(file_index)%vars(num_vars))
            
            do i = 1, num_vars
                ncdr_files(file_index)%vars(i)%var_id = i
                
                call ncdr_nc_check(nf90_inquire_variable(file_ncid, i, &
                    name = var_name, &
                    ndims = ncdr_files(file_index)%vars(i)%var_ndims, &
                    xtype = ncdr_files(file_index)%vars(i)%var_type))
                
                ncdr_files(file_index)%vars(i)%var_name = trim(var_name)
                
                allocate(ncdr_files(file_index)%vars(i)%var_dim_inds( &
                    ncdr_files(file_index)%vars(i)%var_ndims))
                
                call ncdr_nc_check(nf90_inquire_variable(file_ncid, i, &
                    dimids = ncdr_files(file_index)%vars(i)%var_dim_inds))
                
                ! Since the dimensions indicies are aligned to NetCDF's
                ! indicies, we don't need to do any more analysis.
                ! We're done with indices!
                
                ! Now, let's actually use them:
                allocate(ncdr_files(file_index)%vars(i)%var_dim_sizes( &
                    ncdr_files(file_index)%vars(i)%var_ndims))
                
                do j = 1, ncdr_files(file_index)%vars(i)%var_ndims
                    ncdr_files(file_index)%vars(i)%var_dim_sizes(j) = &
                        ncdr_files(file_index)%dims( &
                            ncdr_files(file_index)%vars(i)%var_dim_inds(j) &
                            )%dim_size
                end do
            end do
        end subroutine nc_diag_read_parse_file_vars
        
        function nc_diag_read_id_lookup_var(file_ncdr_id, var_name) result(var_index)
            integer(i_long), intent(in)    :: file_ncdr_id
            character(len=*), intent(in)   :: var_name
            
            integer(i_long)                :: var_index
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            do var_index = 1, ncdr_files(file_ncdr_id)%nvars
                if (ncdr_files(file_ncdr_id)%vars(var_index)%var_name == var_name) &
                    return
            end do
            
            ! Otherwise, return -1!
            var_index = -1
        end function nc_diag_read_id_lookup_var
        
        function nc_diag_read_noid_lookup_var(var_name) result(var_index)
            character(len=*), intent(in)   :: var_name
            
            integer(i_long)                :: var_index
            
            call ncdr_check_current_ncdr_id
            
            var_index = nc_diag_read_id_lookup_var(current_ncdr_id, var_name)
        end function nc_diag_read_noid_lookup_var
        
        function nc_diag_read_id_check_var(file_ncdr_id, var_name) result(var_exists)
            integer(i_long), intent(in)    :: file_ncdr_id
            character(len=*), intent(in)   :: var_name
            
            logical                        :: var_exists
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            if (nc_diag_read_id_lookup_var(file_ncdr_id, var_name) == -1) then
                var_exists = .FALSE.
                return
            end if
            
            var_exists = .TRUE.
        end function nc_diag_read_id_check_var
        
        function nc_diag_read_noid_check_var(var_name) result(var_exists)
            character(len=*), intent(in)   :: var_name
            
            logical                        :: var_exists
            
            call ncdr_check_current_ncdr_id
            
            if (nc_diag_read_lookup_var(var_name) == -1) then
                var_exists = .FALSE.
                return
            end if
            
            var_exists = .TRUE.
        end function nc_diag_read_noid_check_var
        
        function nc_diag_read_id_get_var_ndims(file_ncdr_id, var_name) result(var_ndims)
            integer(i_long), intent(in)    :: file_ncdr_id
            character(len=*), intent(in)   :: var_name
            
            integer(i_long)                :: var_index, var_ndims
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            
            var_ndims = ncdr_files(file_ncdr_id)%vars(var_index)%var_ndims
        end function nc_diag_read_id_get_var_ndims
        
        function nc_diag_read_noid_get_var_ndims(var_name) result(var_ndims)
            character(len=*), intent(in)   :: var_name
            
            integer(i_long)                :: var_ndims
            
            call ncdr_check_current_ncdr_id
            
            var_ndims = nc_diag_read_id_get_var_ndims(current_ncdr_id, var_name)
        end function nc_diag_read_noid_get_var_ndims
        
        function nc_diag_read_id_get_var_type(file_ncdr_id, var_name) result(var_type)
            integer(i_long), intent(in)    :: file_ncdr_id
            character(len=*), intent(in)   :: var_name
            
            integer(i_long)                :: var_index, var_type
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            
            var_type = ncdr_files(file_ncdr_id)%vars(var_index)%var_type
        end function nc_diag_read_id_get_var_type
        
        function nc_diag_read_noid_get_var_type(var_name) result(var_type)
            character(len=*), intent(in)   :: var_name
            
            integer(i_long)                :: var_type
            
            call ncdr_check_current_ncdr_id
            
            var_type = nc_diag_read_id_get_var_type(current_ncdr_id, var_name)
        end function nc_diag_read_noid_get_var_type
        
        function nc_diag_read_id_ret_var_dims(file_ncdr_id, var_name) result(var_dims)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: var_name
            
            integer(i_long)                            :: var_index, var_ndims, i
            integer(i_long), dimension(:), allocatable :: var_dims
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            
            var_ndims = nc_diag_read_id_get_var_ndims(file_ncdr_id, var_name)
            
            allocate(var_dims(var_ndims))
            
            do i = 1, var_ndims
                var_dims(i) = &
                    ncdr_files(file_ncdr_id)%dims( &
                        ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_inds(i) &
                        )%dim_size
            end do
        end function nc_diag_read_id_ret_var_dims
        
        function nc_diag_read_noid_ret_var_dims(var_name) result(var_dims)
            character(len=*), intent(in)                :: var_name
            integer(i_long), dimension(:), allocatable  :: var_dims
            
            integer(i_long)                             :: var_ndims
            
            call ncdr_check_current_ncdr_id
            
            var_ndims = nc_diag_read_id_get_var_ndims(current_ncdr_id, var_name)
            
            allocate(var_dims(var_ndims))
            
            var_dims = nc_diag_read_id_ret_var_dims(current_ncdr_id, var_name)
        end function nc_diag_read_noid_ret_var_dims
        
        subroutine nc_diag_read_id_get_var_dims(file_ncdr_id, var_name, var_ndims, var_dims)
            integer(i_long), intent(in)              :: file_ncdr_id
            character(len=*), intent(in)             :: var_name
            integer(i_long), intent(inout), optional :: var_ndims
            integer(i_long), intent(inout), dimension(:), allocatable, optional :: var_dims
            
            integer(i_long)                :: var_index, v_ndims, i
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            var_index = nc_diag_read_id_assert_var(file_ncdr_id, var_name)
            
            v_ndims = nc_diag_read_id_get_var_ndims(file_ncdr_id, var_name)
            
            if (present(var_ndims)) &
                var_ndims = v_ndims
            
            if (present(var_dims)) then
                if (.NOT. allocated(var_dims)) then
                    allocate(var_dims(v_ndims))
                else
                    if (size(var_dims) /= v_ndims) &
                        call ncdr_error("Invalid allocated array size for variable dimensions size storage!")
                end if
                
                do i = 1, v_ndims
                    var_dims(i) = &
                        ncdr_files(file_ncdr_id)%dims( &
                            ncdr_files(file_ncdr_id)%vars(var_index)%var_dim_inds(i) &
                            )%dim_size
                end do
            end if
        end subroutine nc_diag_read_id_get_var_dims
        
        subroutine nc_diag_read_noid_get_var_dims(var_name, var_ndims, var_dims)
            character(len=*), intent(in)             :: var_name
            integer(i_long), intent(inout), optional :: var_ndims
            integer(i_long), intent(inout), dimension(:), allocatable, optional :: var_dims
            
            call ncdr_check_current_ncdr_id
            
            if (present(var_ndims)) then
                if (present(var_dims)) then
                    call nc_diag_read_id_get_var_dims(current_ncdr_id, var_name, var_ndims, var_dims)
                else
                    call nc_diag_read_id_get_var_dims(current_ncdr_id, var_name, var_ndims)
                end if
            else
                if (present(var_dims)) then
                    call nc_diag_read_id_get_var_dims(current_ncdr_id, var_name, var_dims = var_dims)
                else
                    ! Why you want to do this, I dunno...
                    call nc_diag_read_id_get_var_dims(current_ncdr_id, var_name)
                end if
            end if
        end subroutine nc_diag_read_noid_get_var_dims
        
        subroutine nc_diag_read_id_get_var_names(file_ncdr_id, num_vars, var_name_mlen, var_names)
            integer(i_long), intent(in)              :: file_ncdr_id
            integer(i_long), intent(out), optional   :: num_vars
            integer(i_long), intent(out), optional   :: var_name_mlen
            character(len=:), intent(inout), dimension(:), allocatable, optional:: var_names
            
            integer(i_long)                :: var_index, nvars, max_var_name_len
            
            max_var_name_len = 0
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            nvars = ncdr_files(file_ncdr_id)%nvars
            
            if (present(num_vars)) &
                num_vars = nvars
            
            ! Figure out character max length
            do var_index = 1, nvars
                if (len(ncdr_files(file_ncdr_id)%vars(var_index)%var_name) > max_var_name_len) &
                    max_var_name_len = len(ncdr_files(file_ncdr_id)%vars(var_index)%var_name)
            end do
            
            if (present(var_name_mlen)) &
                var_name_mlen = max_var_name_len
            
            if (present(var_names)) then
                if (.NOT. allocated(var_names)) then
                    allocate(character(max_var_name_len) :: var_names(nvars))
                else
                    if (size(var_names) /= nvars) &
                        call ncdr_error("Invalid allocated array size for variable names storage!")
                    if (len(var_names) < max_var_name_len) &
                        call ncdr_error("Invalid allocated array size for variable names storage! (String length does not match!)")
                end if
                
                do var_index = 1, nvars
                    var_names(var_index) = ncdr_files(file_ncdr_id)%vars(var_index)%var_name
                end do
            end if
        end subroutine nc_diag_read_id_get_var_names
        
        subroutine nc_diag_read_noid_get_var_names(num_vars, var_name_mlen, var_names)
            integer(i_long), intent(out), optional   :: num_vars
            integer(i_long), intent(out), optional   :: var_name_mlen
            character(len=:), intent(inout), dimension(:), allocatable, optional:: var_names
            
            call ncdr_check_current_ncdr_id
            
            if (present(num_vars)) then
                if (present(var_name_mlen)) then
                    if (present(var_names)) then
                        call nc_diag_read_id_get_var_names(current_ncdr_id, num_vars, var_name_mlen, var_names)
                    else
                        call nc_diag_read_id_get_var_names(current_ncdr_id, num_vars, var_name_mlen)
                    end if
                else
                    if (present(var_names)) then
                        call nc_diag_read_id_get_var_names(current_ncdr_id, num_vars, var_names = var_names)
                    else
                        call nc_diag_read_id_get_var_names(current_ncdr_id, num_vars)
                    end if
                end if
            else
                if (present(var_name_mlen)) then
                    if (present(var_names)) then
                        call nc_diag_read_id_get_var_names(current_ncdr_id, var_name_mlen = var_name_mlen, &
                            var_names = var_names)
                    else
                        call nc_diag_read_id_get_var_names(current_ncdr_id, var_name_mlen = var_name_mlen)
                    end if
                else
                    if (present(var_names)) then
                        call nc_diag_read_id_get_var_names(current_ncdr_id, var_names = var_names)
                    else
                        ! Why would you do this?
                        call nc_diag_read_id_get_var_names(current_ncdr_id)
                    end if
                end if
            end if
        end subroutine nc_diag_read_noid_get_var_names
end module ncdr_vars
