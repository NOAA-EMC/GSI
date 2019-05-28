module ncdc_vars
    use ncd_kinds, only: i_long
    use ncdc_state, only: var_names, var_types, var_output_ids, &
        var_counters, var_hasunlim, var_dim_names, var_arr_total, &
        var_arr_size
    use ncdc_dims, only: dim_sizes, nc_diag_cat_lookup_dim
    use ncdc_realloc, only: nc_diag_realloc
    use ncdc_climsg, only: ncdc_error
    use netcdf, only: NF90_BYTE, NF90_SHORT, NF90_INT, NF90_FLOAT, &
        NF90_DOUBLE, NF90_CHAR
    
    implicit none
    
    integer(i_long), parameter                 :: VAR_START_SIZE = 1024
    
    contains
        function nc_diag_cat_lookup_var(var_name) result(ind)
            character(len=*), intent(in)    :: var_name
            integer(i_long)                 :: i, ind
            
            ind = -1
            
            if (allocated(var_names)) then
                do i = 1, var_arr_total
                    if (var_names(i) == var_name) then
                        ind = i
                        exit
                    end if
                end do
            end if
        end function nc_diag_cat_lookup_var
        
        subroutine nc_diag_cat_metadata_add_var(var_name, var_type, var_ndims, var_dims)
            character(len=*), intent(in)    :: var_name
            integer(i_long) , intent(in)    :: var_type
            integer(i_long) , intent(in)    :: var_ndims
            character(len=*), intent(in)    :: var_dims(:)
            
            integer(i_long)                 :: var_index, i
            character(len=1000)             :: err_string
            
            var_index = nc_diag_cat_lookup_var(trim(var_name))
            
            ! If we can't find it, it's new! Make sure we have enough
            ! space for it...
            if (var_index == -1) then
#ifdef DEBUG
                print *, "NEW VAR! Var = " // trim(var_name)
#endif
                
                var_arr_total = var_arr_total + 1
                
                if (var_arr_total >= var_arr_size) then
                    if (allocated(var_names)) then
                        call nc_diag_realloc(var_names, VAR_START_SIZE)
                        call nc_diag_realloc(var_types, VAR_START_SIZE)
                        call nc_diag_realloc(var_dim_names, VAR_START_SIZE)
                        call nc_diag_realloc(var_output_ids, VAR_START_SIZE)
                        call nc_diag_realloc(var_counters, VAR_START_SIZE)
                        call nc_diag_realloc(var_hasunlim, VAR_START_SIZE)
                    else
                        allocate(var_names(VAR_START_SIZE))
                        allocate(var_types(VAR_START_SIZE))
                        allocate(var_dim_names(VAR_START_SIZE))
                        allocate(var_output_ids(VAR_START_SIZE))
                        allocate(var_counters(VAR_START_SIZE))
                        allocate(var_hasunlim(VAR_START_SIZE))
                        var_arr_size = VAR_START_SIZE
                    end if
                end if
                
#ifdef DEBUG
                write (*, "(A)", advance="NO") "DEBUG DUMP:"
                
                do i = 1, var_arr_total - 1
                    if (i /= 1) write (*, "(A)", advance="NO") ", "
                    write (*, "(A)", advance="NO") var_names(i)
                end do
                
                print *, "NEW var_index: ", var_arr_total
#endif
                
                var_index = var_arr_total
                
                ! Add name
                var_names(var_index) = var_name
                var_types(var_index) = var_type
                var_counters(var_index) = 0
            end if
            
            if (allocated(var_dim_names(var_index)%dim_names)) then
                ! Just do a sanity check!
                if (var_types(var_index) /= var_type) &
                    call ncdc_error("Variable type changed!" // &
                        CHAR(10) // "             " // &
                        "(Type of variable '" // var_name // "' changed from " // &
                        trim(nc_diag_cat_metadata_type_to_str(var_types(var_index))) // &
                        CHAR(10) // "             " // &
                        "to " // &
                        trim(nc_diag_cat_metadata_type_to_str(var_type)) // &
                        "!)")
                
                if (var_dim_names(var_index)%num_names /= var_ndims) then
                    write (err_string, "(A, I0, A, I0, A)") &
                        "Variable ndims changed!" // &
                        CHAR(10) // "             " // &
                        "(Variable '" // var_name // "' changed ndims from ", &
                        var_dim_names(var_index)%num_names, &
                        CHAR(10) // "             " // &
                        "to ", &
                        var_ndims, &
                        "!)"
                    call ncdc_error(trim(err_string))
                end if
                
                do i = 1, var_ndims
                    if (var_dim_names(var_index)%dim_names(i) /= var_dims(i)) &
                        call ncdc_error("Variable dimensions changed!" // &
                        CHAR(10) // "             " // &
                        "(Variable '" // var_name // "' changed dimension from " // &
                        trim(var_dim_names(var_index)%dim_names(i)) // &
                        CHAR(10) // "             " // &
                        "to " // &
                        trim(var_dims(i)) // &
                        "!)")
                end do
            else
                var_dim_names(var_index)%num_names = var_ndims
                allocate(var_dim_names(var_index)%dim_names(var_ndims))
                allocate(var_dim_names(var_index)%output_dim_ids(var_ndims))
                var_dim_names(var_index)%dim_names(1:var_ndims) = var_dims(1:var_ndims)
                var_hasunlim(var_index) = .FALSE.
                
                do i = 1, var_ndims
                    if (dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(var_index)%dim_names(i))) == -1) then
                        var_hasunlim(var_index) = .TRUE.
                        exit
                    end if
                end do
                
            end if
        end subroutine nc_diag_cat_metadata_add_var
        
        function nc_diag_cat_metadata_type_to_str(var_type) result(nc_str)
            integer(i_long)   :: var_type
            character(len=11) :: nc_str
            
            nc_str = "(invalid)"
            
            if (var_type == NF90_BYTE)   nc_str = "NF90_BYTE"
            if (var_type == NF90_SHORT)  nc_str = "NF90_SHORT"
            if (var_type == NF90_INT)    nc_str = "NF90_INT (LONG)"
            if (var_type == NF90_FLOAT)  nc_str = "NF90_FLOAT"
            if (var_type == NF90_DOUBLE) nc_str = "NF90_DOUBLE"
            if (var_type == NF90_CHAR)   nc_str = "NF90_CHAR"
        end function nc_diag_cat_metadata_type_to_str
end module ncdc_vars
