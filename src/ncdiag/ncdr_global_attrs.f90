module ncdr_global_attrs
    use ncd_kinds, only: i_long
    use ncdr_state, only: ncdr_files, current_ncdr_id
    use ncdr_climsg, only: ncdr_error
    use ncdr_check, only: ncdr_nc_check, ncdr_check_ncdr_id, &
        ncdr_check_current_ncdr_id
    use netcdf, only: nf90_inquire_attribute, nf90_inquire, &
        nf90_inq_attname, NF90_GLOBAL, NF90_MAX_NAME, NF90_ENOTATT, &
        NF90_NOERR
    
    implicit none
    
    interface nc_diag_read_check_global_attr
        module procedure nc_diag_read_id_check_global_attr, &
            nc_diag_read_noid_check_global_attr
    end interface nc_diag_read_check_global_attr
    
    interface nc_diag_read_get_global_attr_type
        module procedure nc_diag_read_id_get_global_attr_type, &
            nc_diag_read_noid_get_global_attr_type
    end interface nc_diag_read_get_global_attr_type
    
    interface nc_diag_read_ret_global_attr_len
        module procedure nc_diag_read_id_ret_global_attr_len, &
            nc_diag_read_noid_ret_global_attr_len
    end interface nc_diag_read_ret_global_attr_len
    
    interface nc_diag_read_get_global_attr_len
        module procedure nc_diag_read_id_get_global_attr_len, &
            nc_diag_read_noid_get_global_attr_len
    end interface nc_diag_read_get_global_attr_len
    
    interface nc_diag_read_get_global_attr_names
        module procedure nc_diag_read_id_get_global_attr_names
    end interface nc_diag_read_get_global_attr_names
    
    interface nc_diag_read_noid_get_global_attr_names
        module procedure nc_diag_read_noid_get_global_attr_names
    end interface nc_diag_read_noid_get_global_attr_names

    contains
        function nc_diag_read_id_check_global_attr(file_ncdr_id, attr_name) result(attr_exists)
            integer(i_long), intent(in)    :: file_ncdr_id
            character(len=*), intent(in)   :: attr_name
            
            integer(i_long)                :: nc_err
            
            logical                        :: attr_exists
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            nc_err = nf90_inquire_attribute(ncdr_files(file_ncdr_id)%ncid, &
                NF90_GLOBAL, attr_name)
            
            ! If attribute doesn't exist, return false.
            if (nc_err == NF90_ENOTATT) then
                attr_exists = .FALSE.
                return
            end if
            
            ! Sanity check - could be another error!
            if (nc_err /= NF90_NOERR) then
                call ncdr_nc_check(nc_err)
            end if
            
            attr_exists = .TRUE.
        end function nc_diag_read_id_check_global_attr
        
        function nc_diag_read_noid_check_global_attr(attr_name) result(attr_exists)
            character(len=*), intent(in)   :: attr_name
            
            logical                        :: attr_exists
            
            call ncdr_check_current_ncdr_id
            
            attr_exists = nc_diag_read_id_check_global_attr(current_ncdr_id, attr_name)
        end function nc_diag_read_noid_check_global_attr
        
        function nc_diag_read_id_get_global_attr_type(file_ncdr_id, attr_name) result(attr_type)
            integer(i_long), intent(in)    :: file_ncdr_id
            character(len=*), intent(in)   :: attr_name
            
            integer(i_long)                :: attr_type
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            call ncdr_nc_check(nf90_inquire_attribute(ncdr_files(file_ncdr_id)%ncid, &
                NF90_GLOBAL, attr_name, attr_type))
        end function nc_diag_read_id_get_global_attr_type
        
        function nc_diag_read_noid_get_global_attr_type(attr_name) result(attr_type)
            character(len=*), intent(in)   :: attr_name
            
            integer(i_long)                :: attr_type
            
            call ncdr_check_current_ncdr_id
            
            attr_type = nc_diag_read_id_get_global_attr_type(current_ncdr_id, attr_name)
        end function nc_diag_read_noid_get_global_attr_type
        
        function nc_diag_read_id_ret_global_attr_len(file_ncdr_id, attr_name) result(attr_len)
            integer(i_long), intent(in)                :: file_ncdr_id
            character(len=*), intent(in)               :: attr_name
            
            integer(i_long)                            :: attr_len
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            call ncdr_nc_check(nf90_inquire_attribute(ncdr_files(file_ncdr_id)%ncid, &
                NF90_GLOBAL, attr_name, len = attr_len))
        end function nc_diag_read_id_ret_global_attr_len
        
        function nc_diag_read_noid_ret_global_attr_len(attr_name) result(attr_len)
            character(len=*), intent(in)                :: attr_name
            integer(i_long)                             :: attr_len
            
            call ncdr_check_current_ncdr_id
            
            attr_len = nc_diag_read_id_ret_global_attr_len(current_ncdr_id, attr_name)
        end function nc_diag_read_noid_ret_global_attr_len
        
        subroutine nc_diag_read_id_get_global_attr_len(file_ncdr_id, attr_name, attr_len)
            integer(i_long), intent(in)              :: file_ncdr_id
            character(len=*), intent(in)             :: attr_name
            integer(i_long), intent(out)             :: attr_len
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            call ncdr_nc_check(nf90_inquire_attribute(ncdr_files(file_ncdr_id)%ncid, &
                NF90_GLOBAL, attr_name, len = attr_len))
        end subroutine nc_diag_read_id_get_global_attr_len
        
        subroutine nc_diag_read_noid_get_global_attr_len(attr_name, attr_len)
            character(len=*), intent(in)             :: attr_name
            integer(i_long), intent(out)             :: attr_len
            
            call ncdr_check_current_ncdr_id
            
            call nc_diag_read_id_get_global_attr_len(current_ncdr_id, attr_name, attr_len)
        end subroutine nc_diag_read_noid_get_global_attr_len
        
        subroutine nc_diag_read_id_get_global_attr_names(file_ncdr_id, num_global_attrs, attr_name_mlen, attr_names)
            integer(i_long), intent(in)              :: file_ncdr_id
            integer(i_long), intent(out), optional   :: num_global_attrs
            integer(i_long), intent(out), optional   :: attr_name_mlen
            character(len=:), intent(inout), dimension(:), allocatable, optional:: attr_names
            
            integer(i_long)                :: nattrs, attr_index, max_global_attr_name_len
            
            character(len=NF90_MAX_NAME)             :: attr_name
            
            max_global_attr_name_len = 0
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            call ncdr_nc_check(nf90_inquire(ncdr_files(file_ncdr_id)%ncid, nAttributes = nattrs))
            
            if (present(num_global_attrs)) &
                num_global_attrs = nattrs
            
            ! Figure out character max length
            do attr_index = 1, nattrs
                call ncdr_nc_check(nf90_inq_attname(ncdr_files(file_ncdr_id)%ncid, &
                    NF90_GLOBAL, &
                    attr_index, &
                    attr_name))
                
                if (len_trim(attr_name) > max_global_attr_name_len) &
                    max_global_attr_name_len = len_trim(attr_name)
            end do
            
            if (present(attr_name_mlen)) &
                attr_name_mlen = max_global_attr_name_len
            
            if (present(attr_names)) then
                if (.NOT. allocated(attr_names)) then
                    allocate(character(max_global_attr_name_len) :: attr_names(nattrs))
                else
                    if (size(attr_names) /= nattrs) &
                        call ncdr_error("Invalid allocated array size for attribute names storage!")
                    if (len(attr_names) < max_global_attr_name_len) &
                        call ncdr_error("Invalid allocated array size for attribute names storage! (String length does not match!)")
                end if
                
                do attr_index = 1, nattrs
                    call ncdr_nc_check(nf90_inq_attname(ncdr_files(file_ncdr_id)%ncid, &
                        NF90_GLOBAL, &
                        attr_index, &
                        attr_names(attr_index)))
                end do
            end if
        end subroutine nc_diag_read_id_get_global_attr_names
        
        subroutine nc_diag_read_noid_get_global_attr_names(num_global_attrs, attr_name_mlen, attr_names)
            integer(i_long), intent(out), optional   :: num_global_attrs
            integer(i_long), intent(out), optional   :: attr_name_mlen
            character(len=:), intent(inout), dimension(:), allocatable, optional:: attr_names
            
            call ncdr_check_current_ncdr_id
            
            call nc_diag_read_id_get_global_attr_names(current_ncdr_id, num_global_attrs, attr_name_mlen, attr_names)
        end subroutine nc_diag_read_noid_get_global_attr_names
end module ncdr_global_attrs
