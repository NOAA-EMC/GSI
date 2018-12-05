module ncdr_dims
    use ncd_kinds, only: i_long
    use ncdr_state, only: ncdr_files, current_ncdr_id
    use ncdr_climsg, only: ncdr_error
    use ncdr_check, only: ncdr_nc_check, ncdr_check_ncdr_id, &
        ncdr_check_current_ncdr_id
    use netcdf, only: nf90_inquire_dimension, NF90_MAX_NAME
    use netcdf_unlimdims, only: pf_nf90_inq_unlimdims
    
    implicit none
    
    interface nc_diag_read_lookup_dim
        module procedure nc_diag_read_id_lookup_dim, &
            nc_diag_read_noid_lookup_dim
    end interface nc_diag_read_lookup_dim
    
    interface nc_diag_read_assert_dim
        module procedure nc_diag_read_id_assert_dim, &
            nc_diag_read_noid_assert_dim
    end interface nc_diag_read_assert_dim
    
    interface nc_diag_read_check_dim
        module procedure nc_diag_read_id_check_dim, &
            nc_diag_read_noid_check_dim
    end interface nc_diag_read_check_dim
    
    interface nc_diag_read_get_dim
        module procedure nc_diag_read_id_get_dim, &
            nc_diag_read_noid_get_dim
    end interface nc_diag_read_get_dim
    
    interface nc_diag_read_check_dim_unlim
        module procedure nc_diag_read_id_check_dim_unlim, &
            nc_diag_read_noid_check_dim_unlim
    end interface nc_diag_read_check_dim_unlim
    
    interface nc_diag_read_get_dim_names
        module procedure nc_diag_read_id_get_dim_names
    end interface nc_diag_read_get_dim_names
    
    interface nc_diag_read_noid_get_dim_names
        module procedure nc_diag_read_noid_get_dim_names
    end interface nc_diag_read_noid_get_dim_names

    contains
        subroutine nc_diag_read_parse_file_dims(file_ncid, file_index, num_dims)
            integer(i_long), intent(in)                :: file_ncid
            integer(i_long), intent(in)                :: file_index
            integer(i_long), intent(in)                :: num_dims
            
            integer(i_long), dimension(:), allocatable :: unlim_dims
            integer(i_long)                            :: num_unlims
            integer(i_long)                            :: i, j
            
            character(len=NF90_MAX_NAME)               :: dim_name
            
            ncdr_files(file_index)%ndims = num_dims
            allocate(ncdr_files(file_index)%dims(num_dims))
            
            ! Get unlimited dimension information
            call ncdr_nc_check(pf_nf90_inq_unlimdims(file_ncid, num_unlims))
            
            allocate(unlim_dims(num_unlims))
            
            call ncdr_nc_check(pf_nf90_inq_unlimdims(file_ncid, num_unlims, unlim_dims))
            
            do i = 1, num_dims
                ncdr_files(file_index)%dims(i)%dim_id = i
                
                call ncdr_nc_check(nf90_inquire_dimension(file_ncid, i, &
                        dim_name, &
                        ncdr_files(file_index)%dims(i)%dim_size))
                
                ncdr_files(file_index)%dims(i)%dim_name = trim(dim_name)
                ncdr_files(file_index)%dims(i)%dim_unlim = .FALSE.
                
                do j = 1, num_unlims
                    if (i == unlim_dims(j)) then
                        ncdr_files(file_index)%dims(i)%dim_unlim = .TRUE.
                        exit
                    end if
                end do
            end do
        end subroutine nc_diag_read_parse_file_dims
        
        function nc_diag_read_id_lookup_dim(file_ncdr_id, dim_name) result(dim_index)
            integer(i_long), intent(in)    :: file_ncdr_id
            character(len=*), intent(in)   :: dim_name
            
            integer(i_long)                :: dim_index
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            do dim_index = 1, ncdr_files(file_ncdr_id)%ndims
                if (ncdr_files(file_ncdr_id)%dims(dim_index)%dim_name == dim_name) &
                    return
            end do
            
            ! Otherwise, return -1!
            dim_index = -1
        end function nc_diag_read_id_lookup_dim
        
        function nc_diag_read_noid_lookup_dim(dim_name) result(dim_index)
            character(len=*), intent(in)   :: dim_name
            
            integer(i_long)                :: dim_index
            
            call ncdr_check_current_ncdr_id
            
            dim_index = nc_diag_read_id_lookup_dim(current_ncdr_id, dim_name)
        end function nc_diag_read_noid_lookup_dim
        
        function nc_diag_read_id_assert_dim(file_ncdr_id, dim_name) result(dim_index)
            integer(i_long), intent(in)    :: file_ncdr_id
            character(len=*), intent(in)   :: dim_name
            
            integer(i_long)                :: dim_index
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            ! Otherwise, return -1!
            dim_index = nc_diag_read_id_lookup_dim(file_ncdr_id, dim_name)
            
            ! ...except don't, since we're asserting!
            if (dim_index == -1) &
                call ncdr_error("The specified dimension '" // dim_name // "' does not exist!")
        end function nc_diag_read_id_assert_dim
        
        function nc_diag_read_noid_assert_dim(dim_name) result(dim_index)
            character(len=*), intent(in)   :: dim_name
            
            integer(i_long)                :: dim_index
            
            call ncdr_check_current_ncdr_id
            
            dim_index = nc_diag_read_id_assert_dim(current_ncdr_id, dim_name)
        end function nc_diag_read_noid_assert_dim
        
        function nc_diag_read_id_check_dim(file_ncdr_id, dim_name) result(dim_exists)
            integer(i_long), intent(in)    :: file_ncdr_id
            character(len=*), intent(in)   :: dim_name
            
            logical                        :: dim_exists
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            if (nc_diag_read_id_lookup_dim(file_ncdr_id, dim_name) == -1) then
                dim_exists = .FALSE.
                return
            end if
            
            dim_exists = .TRUE.
        end function nc_diag_read_id_check_dim
        
        function nc_diag_read_noid_check_dim(dim_name) result(dim_exists)
            character(len=*), intent(in)   :: dim_name
            
            logical                        :: dim_exists
            
            call ncdr_check_current_ncdr_id
            
            if (nc_diag_read_lookup_dim(dim_name) == -1) then
                dim_exists = .FALSE.
                return
            end if
            
            dim_exists = .TRUE.
        end function nc_diag_read_noid_check_dim
        
        function nc_diag_read_id_get_dim(file_ncdr_id, dim_name) result(dim_size)
            integer(i_long), intent(in)    :: file_ncdr_id
            character(len=*), intent(in)   :: dim_name
            
            integer(i_long)                :: dim_index, dim_size
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            dim_index = nc_diag_read_id_assert_dim(file_ncdr_id, dim_name)
            
            dim_size = ncdr_files(file_ncdr_id)%dims(dim_index)%dim_size
        end function nc_diag_read_id_get_dim
        
        function nc_diag_read_noid_get_dim(dim_name) result(dim_size)
            character(len=*), intent(in)   :: dim_name
            
            integer(i_long)                :: dim_size
            
            call ncdr_check_current_ncdr_id
            
            dim_size = nc_diag_read_id_get_dim(current_ncdr_id, dim_name)
        end function nc_diag_read_noid_get_dim
        
        function nc_diag_read_id_check_dim_unlim(file_ncdr_id, dim_name) result(dim_isunlim)
            integer(i_long), intent(in)    :: file_ncdr_id
            character(len=*), intent(in)   :: dim_name
            
            integer(i_long)                :: dim_index
            logical                        :: dim_isunlim
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            dim_index = nc_diag_read_id_assert_dim(file_ncdr_id, dim_name)
            
            dim_isunlim = ncdr_files(file_ncdr_id)%dims(dim_index)%dim_unlim
        end function nc_diag_read_id_check_dim_unlim
        
        function nc_diag_read_noid_check_dim_unlim(dim_name) result(dim_isunlim)
            character(len=*), intent(in)   :: dim_name
            
            logical                        :: dim_isunlim
            
            call ncdr_check_current_ncdr_id
            
            dim_isunlim = nc_diag_read_id_check_dim_unlim(current_ncdr_id, dim_name)
        end function nc_diag_read_noid_check_dim_unlim
        
        subroutine nc_diag_read_id_get_dim_names(file_ncdr_id, num_dims, dim_name_mlen, dim_names)
            integer(i_long), intent(in)              :: file_ncdr_id
            integer(i_long), intent(out), optional   :: num_dims
            integer(i_long), intent(out), optional   :: dim_name_mlen
            character(len=:), intent(inout), dimension(:), allocatable, optional:: dim_names
            
            integer(i_long)                :: dim_index, ndims, max_dim_name_len
            
            max_dim_name_len = 0
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            ndims = ncdr_files(file_ncdr_id)%ndims
            
            if (present(num_dims)) &
                num_dims = ndims
            
            ! Figure out character max length
            do dim_index = 1, ndims
                if (len(ncdr_files(file_ncdr_id)%dims(dim_index)%dim_name) > max_dim_name_len) &
                    max_dim_name_len = len(ncdr_files(file_ncdr_id)%dims(dim_index)%dim_name)
            end do
            
            if (present(dim_name_mlen)) &
                dim_name_mlen = max_dim_name_len
            
            if (present(dim_names)) then
                if (.NOT. allocated(dim_names)) then
                    allocate(character(max_dim_name_len) :: dim_names(ndims))
                else
                    if (size(dim_names) /= ndims) &
                        call ncdr_error("Invalid allocated array size for dimension names storage!")
                    if (len(dim_names) < max_dim_name_len) &
                        call ncdr_error("Invalid allocated array size for dimension names storage! (String length does not match!)")
                end if
                
                do dim_index = 1, ndims
                    dim_names(dim_index) = ncdr_files(file_ncdr_id)%dims(dim_index)%dim_name
                end do
            end if
        end subroutine nc_diag_read_id_get_dim_names
        
        subroutine nc_diag_read_noid_get_dim_names(num_dims, dim_name_mlen, dim_names)
            integer(i_long), intent(out), optional   :: num_dims
            integer(i_long), intent(out), optional   :: dim_name_mlen
            character(len=:), intent(inout), dimension(:), allocatable, optional:: dim_names
            
            call ncdr_check_current_ncdr_id
            
            if (present(num_dims)) then
                if (present(dim_name_mlen)) then
                    if (present(dim_names)) then
                        call nc_diag_read_id_get_dim_names(current_ncdr_id, num_dims, dim_name_mlen, dim_names)
                    else
                        call nc_diag_read_id_get_dim_names(current_ncdr_id, num_dims, dim_name_mlen)
                    end if
                else
                    if (present(dim_names)) then
                        call nc_diag_read_id_get_dim_names(current_ncdr_id, num_dims, dim_names = dim_names)
                    else
                        call nc_diag_read_id_get_dim_names(current_ncdr_id, num_dims)
                    end if
                end if
            else
                if (present(dim_name_mlen)) then
                    if (present(dim_names)) then
                        call nc_diag_read_id_get_dim_names(current_ncdr_id, dim_name_mlen = dim_name_mlen, &
                            dim_names = dim_names)
                    else
                        call nc_diag_read_id_get_dim_names(current_ncdr_id, dim_name_mlen = dim_name_mlen)
                    end if
                else
                    if (present(dim_names)) then
                        call nc_diag_read_id_get_dim_names(current_ncdr_id, dim_names = dim_names)
                    else
                        ! Why would you do this?
                        call nc_diag_read_id_get_dim_names(current_ncdr_id)
                    end if
                end if
            end if
        end subroutine nc_diag_read_noid_get_dim_names
end module ncdr_dims
