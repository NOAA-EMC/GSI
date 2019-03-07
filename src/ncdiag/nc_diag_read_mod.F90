module nc_diag_read_mod
    use ncd_kinds, only: i_long
    use ncdr_state, only: ncdr_files, ncdr_file_count, &
        ncdr_file_total, ncdr_file_highest, ncdr_id_stack, &
        current_ncdr_id, ncdr_id_stack_count, ncdr_id_stack_size, &
        NCDR_DEFAULT_ENT
    use ncdr_check, only: nc_diag_read_get_index_from_filename, &
        ncdr_check_ncdr_id, ncdr_check_ncid, ncdr_nc_check
    use ncdr_climsg, only: ncdr_error
    use ncdr_realloc_mod, only: ncdr_realloc
    use netcdf, only: nf90_open, nf90_close, nf90_inquire, &
        nf90_inq_libvers, NF90_NOWRITE
    
    !------------------------------------------------------------------
    ! API imports to expose API from this module
    !------------------------------------------------------------------
    use ncdr_alloc_assert, only: &
        nc_diag_read_assert_var, &
        nc_diag_read_assert_attr, &
        nc_diag_read_assert_global_attr, &
        nc_diag_read_get_type_str
    
    use ncdr_attrs, only: &
        nc_diag_read_check_attr, &
        nc_diag_read_get_attr_type, &
        nc_diag_read_ret_attr_len, &
        nc_diag_read_get_attr_len, &
        nc_diag_read_get_attr_names
    
    use ncdr_attrs_fetch, only: &
        nc_diag_read_get_attr, &
        nc_diag_read_id_get_attr_1d_string, &
        nc_diag_read_noid_get_attr_1d_string
    
    use ncdr_dims, only: &
        nc_diag_read_lookup_dim, &
        nc_diag_read_assert_dim, &
        nc_diag_read_check_dim, &
        nc_diag_read_get_dim, &
        nc_diag_read_check_dim_unlim, &
        nc_diag_read_get_dim_names, &
        nc_diag_read_parse_file_dims
    
    use ncdr_global_attrs, only: &
        nc_diag_read_check_global_attr, &
        nc_diag_read_get_global_attr_type, &
        nc_diag_read_ret_global_attr_len, &
        nc_diag_read_get_global_attr_len, &
        nc_diag_read_get_global_attr_names
    
    use ncdr_global_attrs_fetch, only: &
        nc_diag_read_get_global_attr, &
        nc_diag_read_id_get_global_attr_1d_string, &
        nc_diag_read_noid_get_global_attr_1d_string
    
    use ncdr_vars, only: &
        nc_diag_read_lookup_var, &
        nc_diag_read_check_var, &
        nc_diag_read_get_var_ndims, &
        nc_diag_read_get_var_type, &
        nc_diag_read_ret_var_dims, &
        nc_diag_read_get_var_dims, &
        nc_diag_read_get_var_names, &
        nc_diag_read_parse_file_vars
    
    use ncdr_vars_fetch, only: nc_diag_read_get_var
    
    implicit none
    
#define INITIAL_SIZE 1024
#define NCDR_MULTI_BASE 1
    
    contains
        ! NCID = NetCDF ID
        ! NCDR_ID = NetCDF Diag Reader ID (relative indexing)
        
        ! NCID = NetCDF ID
        ! NCDR_ID = NetCDF Diag Reader ID (relative indexing)
        
        ! Parses a given file for metadata, dimensions, and variables.
        ! 
        ! Given the NetCDF file name and its NCID, create an entry in
        ! the internal nc_diag_read file table and populate it with
        ! file information and variable/dimension structure.
        ! 
        ! This subroutine is meant to be called internally by
        ! nc_diag_read_id_init, and is NOT meant for calling from
        ! anywhere else.
        ! 
        ! Args:
        !     filename (character(len=*): NetCDF file name to store in
        !         internal file table.
        !     file_ncid (integer(i_long)): the corresponding NetCDF ID
        !         (NCID) of the opened NetCDF file to store in the
        !         internal file table and use for file reading.
        !     file_ncdr_id (integer(i_long)): internal nc_diag_read ID
        !         for use in other subroutines and functions. This is
        !         essentially the index of the internal file table that
        !         nc_diag_read uses for referencing the specified file.
        ! 
        ! Returns:
        !     file_ncdr_id (integer(i_long)): internal nc_diag_read ID
        !         for use in other subroutines and functions. This is
        !         essentially the index of the internal file table that
        !         nc_diag_read uses for referencing the specified file.
        ! 
        subroutine nc_diag_read_parse_file(filename, file_ncid, file_ncdr_id)
            character(len=*),intent(in)                :: filename
            integer(i_long), intent(in)                :: file_ncid
            integer(i_long), intent(out)               :: file_ncdr_id
            
            integer(i_long)                            :: input_ndims
            integer(i_long)                            :: input_nvars
            integer(i_long)                            :: input_nattrs
            
            ncdr_file_count = ncdr_file_count + 1
            
            if (allocated(ncdr_files)) then
                if (ncdr_file_count > ncdr_file_total) then
                    call ncdr_realloc(ncdr_files, ncdr_file_total * NCDR_MULTI_BASE)
                end if
            else
                allocate(ncdr_files(NCDR_DEFAULT_ENT))
            end if
            
            ncdr_files(ncdr_file_count)%filename = filename
            ncdr_files(ncdr_file_count)%ncid     = file_ncid
            
            ! Get top level info about the file!
            call ncdr_nc_check(nf90_inquire(file_ncid, nDimensions = input_ndims, &
                nVariables = input_nvars, nAttributes = input_nattrs))
            
            call nc_diag_read_parse_file_dims(file_ncid, ncdr_file_count, input_ndims)
            call nc_diag_read_parse_file_vars(file_ncid, ncdr_file_count, input_nvars)
            
            ! Make sure file is now open!
            ncdr_files(ncdr_file_count)%file_open = .TRUE.
            
            ! Update highest record - this will let us keep track and
            ! help us clear memory when we can!
            if (ncdr_file_count > ncdr_file_highest) then
                ncdr_file_highest = ncdr_file_count
            end if
            
            ! Set the NCDR ID - relative index!
            file_ncdr_id = ncdr_file_count
        end subroutine nc_diag_read_parse_file
        
        ! Opens a given file for reading.
        ! 
        ! Given the NetCDF file name, open the file and set everything
        ! up for reading the file. 
        ! 
        ! Args:
        !     filename (character(len=*): NetCDF file name to store in
        !         internal file table.
        ! 
        ! Returns:
        !     file_ncdr_id (integer(i_long)): internal nc_diag_read ID
        !         for use in other subroutines and functions. 
        ! 
        function nc_diag_read_id_init(filename) result(file_ncdr_id)
            character(len=*),intent(in)    :: filename
            integer(i_long)                :: file_ncid
            integer(i_long)                :: file_ncdr_id
            
            if (nc_diag_read_get_index_from_filename(filename) /= -1) &
                call ncdr_error("Can't open the same file more than once! (Opening, closing, and then opening again is allowed.)")
            
            call ncdr_nc_check( nf90_open(filename, NF90_NOWRITE, file_ncid) )
            
            call nc_diag_read_parse_file(filename, file_ncid, file_ncdr_id)
        end function nc_diag_read_id_init
        
        subroutine nc_diag_read_init(filename, file_ncdr_id, from_push)
            character(len=*),intent(in)            :: filename
            integer(i_long), intent(out), optional :: file_ncdr_id
            logical,         intent(in),  optional :: from_push
            integer(i_long)                        :: f_ncdr_id
            
            if (ncdr_id_stack_count > 0) then
                if (.NOT. (present(from_push) .AND. (from_push))) &
                    call ncdr_error("Can not initialize due to push/pop queue use! If you want to init without the stack, you must use nc_diag_read_id_init or clear the queue first!")
            end if
            
            f_ncdr_id = nc_diag_read_id_init(filename)
            
            if (present(file_ncdr_id)) &
                file_ncdr_id = f_ncdr_id
            
            ! Set current ncid
            current_ncdr_id = f_ncdr_id
        end subroutine nc_diag_read_init
        
        subroutine nc_diag_read_push(filename, file_ncdr_id)
            character(len=*),intent(in)            :: filename
            integer(i_long), intent(out), optional :: file_ncdr_id
            
            if ((ncdr_id_stack_count == 0) .AND. (current_ncdr_id /= -1)) &
                call ncdr_error("Can not initialize due to normal caching use! If you want to init with the stack, you must close the cached file first, then use nc_diag_read_push()!")
            
            ncdr_id_stack_count = ncdr_id_stack_count + 1
            
            if (allocated(ncdr_id_stack)) then
                if (ncdr_id_stack_count >= ncdr_id_stack_size) then
                    call ncdr_realloc(ncdr_id_stack, size(ncdr_id_stack))
                    ncdr_id_stack_size = size(ncdr_id_stack)
                end if
            else
                allocate(ncdr_id_stack(INITIAL_SIZE))
                ncdr_id_stack_size = size(ncdr_id_stack)
            end if
            
            if (present(file_ncdr_id)) then
                call nc_diag_read_init(filename, file_ncdr_id, .TRUE.)
            else
                call nc_diag_read_init(filename, from_push = .TRUE.)
            end if
            
            ! Push new NCID to stack
            ncdr_id_stack(ncdr_id_stack_count) = current_ncdr_id
        end subroutine nc_diag_read_push
        
        subroutine nc_diag_read_close(filename, file_ncdr_id, from_pop)
            character(len=*),intent(in), optional  :: filename
            integer(i_long), intent(in), optional  :: file_ncdr_id
            logical,         intent(in), optional  :: from_pop
            
            integer(i_long)                        :: f_ncdr_id, f_ncid, i
            logical                                :: range_closed
            
            f_ncid = -1
            
            if (ncdr_file_count == 0) &
                call ncdr_error("No files are currently open!")
            
            if (ncdr_id_stack_count > 0) then
                if ((any(ncdr_id_stack == file_ncdr_id)) .AND. (.NOT. (present(from_pop) .AND. (from_pop)))) &
                    call ncdr_error("Can not close due to push/pop queue use! If you want to use this without the stack, you must use nc_diag_read_id_init or clear the queue first!")
            end if
            
            if (present(filename)) then
                f_ncdr_id = nc_diag_read_get_index_from_filename(filename)
                
                if (f_ncdr_id == -1) &
                    call ncdr_error("The NetCDF file specified, " // filename // ", is not open and can't be closed.")
            else if (present(file_ncdr_id)) then
                ! Do... nothing. Just store the ncid.
                f_ncdr_id = file_ncdr_id
            else
                ! Try to see if current_ncid is defined
                if (current_ncdr_id == -1) &
                    call ncdr_error("No arguments specified for closing a file! (Also, no current NCIDs were found!)")
                f_ncdr_id = current_ncdr_id
            end if
            
            ! Sanity check
            call ncdr_check_ncdr_id(f_ncdr_id)
            
            ! Fetch NCID
            f_ncid = ncdr_files(f_ncdr_id)%ncid
            
            ! Sanity check for the NCID...
            call ncdr_check_ncid(f_ncid)
            
            ! Close it!
            call ncdr_nc_check(nf90_close(f_ncid))
            
            ! Deactivate entry...
            ncdr_files(f_ncdr_id)%file_open = .FALSE.
            
            ! Deallocate as much as possible!
            deallocate(ncdr_files(f_ncdr_id)%dims)
            deallocate(ncdr_files(f_ncdr_id)%vars)
            
            ! Set current_ncid to -1, as necessary:
            if (current_ncdr_id == f_ncdr_id) then
                current_ncdr_id = -1
            end if
            
            ! Update highest record - this will let us keep track and
            ! help us clear memory when we can!
            range_closed = .TRUE.
            
            if (f_ncdr_id < ncdr_file_highest) then
                do i = f_ncdr_id, ncdr_file_highest
                    if (ncdr_files(i)%file_open) then
                        range_closed = .FALSE.
                        exit
                    end if
                end do
                
                if (range_closed) then
                    ncdr_file_highest = f_ncdr_id
                    ncdr_file_count   = f_ncdr_id
                end if
            else if (f_ncdr_id == ncdr_file_highest) then
                ncdr_file_highest = f_ncdr_id - 1
                ncdr_file_count   = f_ncdr_id - 1
                
                do i = 1, ncdr_file_highest
                    if (ncdr_files(i)%file_open) then
                        range_closed = .FALSE.
                        exit
                    end if
                end do
                
                if (range_closed) then
                    ncdr_file_highest = 0
                    ncdr_file_count = 0
                end if
            end if
        end subroutine nc_diag_read_close
        
        ! Pop - we return the thing we just deleted, and push things up!
        subroutine nc_diag_read_pop(filename, file_ncdr_id)
            character(len=*),intent(out), optional :: filename
            integer(i_long), intent(out), optional :: file_ncdr_id
            
            if (ncdr_id_stack_count == 0) &
                call ncdr_error("No NetCDF files to pop!")
            
            if (current_ncdr_id /= ncdr_id_stack(ncdr_id_stack_count)) &
                call ncdr_error("BUG - current NCID differs from the current queued NCID!")
            
            if (present(filename)) then
                filename = ncdr_files(ncdr_id_stack(ncdr_id_stack_count))%filename
            end if
            
            if (present(file_ncdr_id)) then
                file_ncdr_id = ncdr_id_stack(ncdr_id_stack_count)
            end if
            
            ! Close the file
            call nc_diag_read_close(file_ncdr_id = ncdr_id_stack(ncdr_id_stack_count), from_pop = .TRUE.)
            
            ! Set the stack spot to -1...
            ncdr_id_stack(ncdr_id_stack_count) = -1
            
            ! ...and decrease the count, effectively "popping" it!
            ncdr_id_stack_count = ncdr_id_stack_count - 1
            
            ! If everything is gone, set current to -1.
            if (ncdr_id_stack_count /= 0) then
                current_ncdr_id = ncdr_id_stack(ncdr_id_stack_count)
            else
                current_ncdr_id = -1
            end if
        end subroutine nc_diag_read_pop
        
        ! Get current file in queue
        subroutine nc_diag_read_get_current_queue(filename, file_ncdr_id)
            character(len=*),intent(out), optional :: filename
            integer(i_long), intent(out), optional :: file_ncdr_id
            
            if (present(filename)) then
                if (ncdr_id_stack_count > 0) then
                    filename = ncdr_files(ncdr_id_stack(ncdr_id_stack_count))%filename
                else
                    filename = "(no file in queue at the moment)"
                end if
            end if
            
            if (present(file_ncdr_id)) then
                if (ncdr_id_stack_count > 0) then
                    file_ncdr_id = ncdr_id_stack(ncdr_id_stack_count)
                else
                    file_ncdr_id = -1
                end if
            end if
        end subroutine nc_diag_read_get_current_queue
        
        ! Get current file, disregarding queue
        subroutine nc_diag_read_get_current(filename, file_ncdr_id)
            character(len=*),intent(out), optional :: filename
            integer(i_long), intent(out), optional :: file_ncdr_id
            
            if (present(filename)) then
                if (current_ncdr_id /= -1) then
                    filename = ncdr_files(current_ncdr_id)%filename
                else
                    filename = "(no file open at the moment)"
                end if
            end if
            
            if (present(file_ncdr_id)) then
                if (current_ncdr_id /= -1) then
                    file_ncdr_id = current_ncdr_id
                else
                    file_ncdr_id = -1
                end if
            end if
        end subroutine nc_diag_read_get_current
end module nc_diag_read_mod
