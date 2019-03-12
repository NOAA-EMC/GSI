module ncdc_dims
    use ncd_kinds, only: i_long
    use ncdc_state, only: dim_names, dim_sizes, dim_unlim_sizes, &
        dim_counters, dim_output_ids, dim_arr_total, dim_arr_size
    use ncdc_realloc, only: nc_diag_realloc
    use ncdc_climsg, only: ncdc_error, ncdc_warning
    
    implicit none
    
    integer(i_long), parameter                 :: DIM_START_SIZE = 256
    
    contains
        function nc_diag_cat_lookup_dim(dim_name) result(ind)
            character(len=*), intent(in)    :: dim_name
            integer(i_long)                 :: i, ind
            
            ind = -1
            
            if (allocated(dim_names)) then
                do i = 1, dim_arr_total
                    if (dim_names(i) == dim_name) then
                        ind = i
                        exit
                    end if
                end do
            end if
        end function nc_diag_cat_lookup_dim
        
        subroutine nc_diag_cat_metadata_add_dim(dim_name, dim_size, dim_ul_size)
            character(len=*),         intent(in) :: dim_name
            integer(i_long) ,         intent(in) :: dim_size
            integer(i_long),optional, intent(in) :: dim_ul_size
            
            integer(i_long)                 :: dim_index
            character(len=1000)             :: err_string
            
            dim_index = nc_diag_cat_lookup_dim(dim_name)
            
            ! If we can't find it, it's new! Make sure we have enough
            ! space for it...
            if (dim_index == -1) then
#ifdef DEBUG
                print *, "NEW DIM!"
#endif
                dim_arr_total = dim_arr_total + 1
                
                if (dim_arr_total >= dim_arr_size) then
                    if (allocated(dim_names)) then
                        call nc_diag_realloc(dim_names, DIM_START_SIZE)
                        call nc_diag_realloc(dim_sizes, DIM_START_SIZE)
                        call nc_diag_realloc(dim_counters, DIM_START_SIZE)
                        call nc_diag_realloc(dim_output_ids, DIM_START_SIZE)
                        call nc_diag_realloc(dim_unlim_sizes, DIM_START_SIZE)
                        dim_arr_size = dim_arr_size + DIM_START_SIZE
                    else
                        allocate(dim_names(DIM_START_SIZE))
                        allocate(dim_sizes(DIM_START_SIZE))
                        allocate(dim_counters(DIM_START_SIZE))
                        allocate(dim_output_ids(DIM_START_SIZE))
                        allocate(dim_unlim_sizes(DIM_START_SIZE))
                        dim_arr_size = DIM_START_SIZE
                    end if
                end if
                
                dim_index = dim_arr_total
                
                ! Add name
                dim_names(dim_index) = dim_name
                dim_sizes(dim_index) = 0
                dim_unlim_sizes(dim_index) = 0
                
                ! Set counter to 0
                dim_counters(dim_index) = 0
                dim_output_ids(dim_index) = -1
            end if
            
            if (dim_size /= -1) then
                ! Add/update size
                if ((index(dim_name, "_maxstrlen") /= 0) .OR. (index(dim_name, "_str_dim") /= 0)) then
                    ! Use the maximum as the new size... and skip the check.
                    if (dim_size > dim_sizes(dim_index)) dim_sizes(dim_index) = dim_size
                else
                    if ((dim_sizes(dim_index) /= 0) .AND. (dim_size /= dim_sizes(dim_index))) then
                        write (err_string, "(A, I0, A, I0, A)") &
                            "Fixed dimension length changed between files!" // &
                            CHAR(10) // "             " // &
                            "(Fixed dimension '" // dim_name // "' changed from length ", &
                            dim_sizes(dim_index), &
                            CHAR(10) // "             " // &
                            "to ", &
                            dim_size, &
                            "!)"
                        call ncdc_error(trim(err_string))
                    end if
                    dim_sizes(dim_index) = dim_size
                end if
            else
                if ((dim_sizes(dim_index) /= -1) .AND. (dim_sizes(dim_index) /= 0)) then
                    write (err_string, "(A, I0, A)") &
                        "Changed from a fixed dimension length to unlimited" // &
                        CHAR(10) // "             " // &
                        "dimension length. (Fixed dimension '" // &
                        trim(dim_name) // &
                        "' had a fixed" // &
                        CHAR(10) // "             " // &
                        "length of ", &
                        dim_sizes(dim_index), &
                        "!)"
                    call ncdc_error(trim(err_string))
                end if
                dim_sizes(dim_index) = -1
                
                if (present(dim_ul_size)) then
                    dim_unlim_sizes(dim_index) = dim_unlim_sizes(dim_index) + dim_ul_size
                else
                    call ncdc_warning("Call made for unlimited dimension without specifying unlimited size!")
                end if
            end if
        end subroutine nc_diag_cat_metadata_add_dim
end module ncdc_dims
