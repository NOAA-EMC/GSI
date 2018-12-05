module ncdw_varattr
    use ncd_kinds, only: i_byte, i_short, i_long, i_llong, r_single, &
        r_double
    use ncdw_state, only: init_done, append_only, ncid, &
        diag_varattr_store
    use ncdw_types, only: NLAYER_DEFAULT_ENT
    use ncdw_climsg, only: nclayer_error, nclayer_warning, &
        nclayer_check
    use ncdw_realloc, only: nc_diag_realloc
    use netcdf, only: nf90_inq_dimid, nf90_def_dim, nf90_put_att, &
        NF90_UNLIMITED
    
    implicit none
    
    interface nc_diag_varattr
        module procedure nc_diag_varattr_byte, &
            nc_diag_varattr_short, nc_diag_varattr_long, &
            nc_diag_varattr_rsingle, nc_diag_varattr_rdouble, &
            nc_diag_varattr_string, &
            nc_diag_varattr_byte_v, nc_diag_varattr_short_v, &
            nc_diag_varattr_long_v, nc_diag_varattr_rsingle_v, &
            nc_diag_varattr_rdouble_v
    end interface nc_diag_varattr
    
    contains
        function nc_diag_varattr_check_var(var_name) result(found)
            character(len=*), intent(in)    :: var_name
            integer :: i
            logical :: found
            found = .FALSE.
            
            if (init_done .AND. allocated(diag_varattr_store)) then
                do i = 1, diag_varattr_store%total
                    if (diag_varattr_store%names(i) == var_name) then
                        found = .TRUE.
                        exit
                    end if
                end do
            end if
        end function nc_diag_varattr_check_var
        
        function nc_diag_varattr_lookup_var(var_name) result(ind)
            character(len=*), intent(in)    :: var_name
            integer :: i, ind
            
            ind = -1
            
            if (init_done .AND. allocated(diag_varattr_store)) then
                do i = 1, diag_varattr_store%total
                    if (diag_varattr_store%names(i) == var_name) then
                        ind = i
                        exit
                    end if
                end do
            end if
        end function nc_diag_varattr_lookup_var
        
        subroutine nc_diag_varattr_make_nobs_dim
            if (init_done .AND. allocated(diag_varattr_store)) then
                if (diag_varattr_store%nobs_dim_id == -1) then
                    if (append_only) then
                        ! Fetch the nobs dimension ID instead!
                        call nclayer_check(nf90_inq_dimid(ncid, "nobs", diag_varattr_store%nobs_dim_id))
                    else
                        call nclayer_check(nf90_def_dim(ncid, "nobs", NF90_UNLIMITED, diag_varattr_store%nobs_dim_id))
                    end if
                end if
            else
                call nclayer_error("NetCDF4 layer not initialized yet!")
            end if
        end subroutine nc_diag_varattr_make_nobs_dim
        
        subroutine nc_diag_varattr_expand(addl_fields)
            integer(i_llong), intent(in) :: addl_fields
            integer(i_llong)             :: size_add
            
            if (init_done .AND. allocated(diag_varattr_store)) then
                if (allocated(diag_varattr_store%names)) then
                    if (diag_varattr_store%total >= size(diag_varattr_store%names)) then
                        size_add = (size(diag_varattr_store%names) * 0.5) + addl_fields
                        call nc_diag_realloc(diag_varattr_store%names, addl_fields)
                    end if
                else
                    allocate(diag_varattr_store%names(NLAYER_DEFAULT_ENT))
                end if
                
                if (allocated(diag_varattr_store%types)) then
                    if (diag_varattr_store%total >= size(diag_varattr_store%types)) then
                        size_add = (size(diag_varattr_store%types) * 0.5) + addl_fields
                        call nc_diag_realloc(diag_varattr_store%types, size_add)
                    end if
                else
                    allocate(diag_varattr_store%types(NLAYER_DEFAULT_ENT))
                    diag_varattr_store%types = -1
                end if
                
                if (allocated(diag_varattr_store%var_ids)) then
                    if (diag_varattr_store%total >= size(diag_varattr_store%var_ids)) then
                        size_add = (size(diag_varattr_store%var_ids) * 0.5) + addl_fields
                        call nc_diag_realloc(diag_varattr_store%var_ids, size_add)
                    end if
                else
                    allocate(diag_varattr_store%var_ids(NLAYER_DEFAULT_ENT))
                    diag_varattr_store%var_ids = -1
                end if
                
            else
                call nclayer_error("NetCDF4 layer not initialized yet!")
            endif
            
        end subroutine nc_diag_varattr_expand
        
        subroutine nc_diag_varattr_add_var(var_name, var_type, var_id)
            character(len=*), intent(in)    :: var_name
            integer(i_byte),  intent(in)    :: var_type
            integer(i_long)                 :: var_id
            
            if (nc_diag_varattr_check_var(var_name)) then
                call nclayer_error("Variable already exists for variable attributes!")
            else
#ifdef _DEBUG_MEM_
                print *, "adding var!"
#endif
                call nc_diag_varattr_expand(1_i_llong)
                diag_varattr_store%total = diag_varattr_store%total + 1
                diag_varattr_store%names(diag_varattr_store%total) = var_name
                diag_varattr_store%types(diag_varattr_store%total) = var_type
                diag_varattr_store%var_ids(diag_varattr_store%total) = var_id
#ifdef _DEBUG_MEM_
                print *, "done adding var!"
#endif
            end if
        end subroutine nc_diag_varattr_add_var
        
        ! nc_diag_varattr - input integer(i_byte)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_varattr_byte(var_name, attr_name, attr_value)
            character(len=*), intent(in)    :: var_name
            character(len=*), intent(in)    :: attr_name
            integer(i_byte), intent(in)     :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call nclayer_error("Bug! Variable exists but could not lookup index for attr!")
                call nclayer_check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call nclayer_error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_byte
        
        ! nc_diag_varattr - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_varattr_short(var_name, attr_name, attr_value)
            character(len=*), intent(in)    :: var_name
            character(len=*), intent(in)    :: attr_name
            integer(i_short), intent(in)    :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call nclayer_error("Bug! Variable exists but could not lookup index for attr!")
                call nclayer_check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call nclayer_error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_short
        
        ! nc_diag_varattr - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_varattr_long(var_name, attr_name, attr_value)
            character(len=*), intent(in)    :: var_name
            character(len=*), intent(in)    :: attr_name
            integer(i_long), intent(in)     :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call nclayer_error("Bug! Variable exists but could not lookup index for attr!")
                call nclayer_check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call nclayer_error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_long
        
        ! nc_diag_varattr - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_varattr_rsingle(var_name, attr_name, attr_value)
            character(len=*), intent(in)    :: var_name
            character(len=*), intent(in)    :: attr_name
            real(r_single), intent(in)      :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call nclayer_error("Bug! Variable exists but could not lookup index for attr!")
                call nclayer_check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call nclayer_error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_rsingle
        
        ! nc_diag_varattr - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_varattr_rdouble(var_name, attr_name, attr_value)
            character(len=*), intent(in)    :: var_name
            character(len=*), intent(in)    :: attr_name
            real(r_double), intent(in)      :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call nclayer_error("Bug! Variable exists but could not lookup index for attr!")
                call nclayer_check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call nclayer_error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_rdouble

        ! nc_diag_varattr - input character(len=*)
        ! Corresponding NetCDF4 type: string? char?
        subroutine nc_diag_varattr_string(var_name, attr_name, attr_value)
            character(len=*), intent(in)    :: var_name
            character(len=*), intent(in)    :: attr_name
            character(len=*), intent(in)    :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call nclayer_error("Bug! Variable exists but could not lookup index for attr!")
                call nclayer_check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call nclayer_error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_string
        
        !=============================================================
        ! VECTOR TYPES
        !=============================================================
        
        ! nc_diag_varattr - input integer(i_byte), dimension(:)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_varattr_byte_v(var_name, attr_name, attr_value)
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            integer(i_byte), dimension(:), intent(in)  :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call nclayer_error("Bug! Variable exists but could not lookup index for attr!")
                call nclayer_check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call nclayer_error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_byte_v
        
        ! nc_diag_varattr - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_varattr_short_v(var_name, attr_name, attr_value)
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            integer(i_short), dimension(:), intent(in) :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call nclayer_error("Bug! Variable exists but could not lookup index for attr!")
                call nclayer_check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call nclayer_error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_short_v
        
        ! nc_diag_varattr - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_varattr_long_v(var_name, attr_name, attr_value)
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            integer(i_long), dimension(:), intent(in)  :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call nclayer_error("Bug! Variable exists but could not lookup index for attr!")
                call nclayer_check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call nclayer_error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_long_v
        
        ! nc_diag_varattr - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_varattr_rsingle_v(var_name, attr_name, attr_value)
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            real(r_single), dimension(:), intent(in)   :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call nclayer_error("Bug! Variable exists but could not lookup index for attr!")
                call nclayer_check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call nclayer_error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_rsingle_v
        
        ! nc_diag_varattr - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_varattr_rdouble_v(var_name, attr_name, attr_value)
            character(len=*), intent(in)               :: var_name
            character(len=*), intent(in)               :: attr_name
            real(r_double), dimension(:), intent(in)   :: attr_value
            
            integer(i_long)                 :: var_index
            
            if (nc_diag_varattr_check_var(var_name)) then
                var_index = nc_diag_varattr_lookup_var(var_name)
                if (var_index == -1) call nclayer_error("Bug! Variable exists but could not lookup index for attr!")
                call nclayer_check(nf90_put_att(ncid, diag_varattr_store%var_ids(var_index), attr_name, attr_value))
            else
                call nclayer_error("Can't set attribute for a non-existent variable!" &
                    // char(10) &
                    // "             (If you did add the variable, make sure you lock" &
                    // char(10) &
                    // "             the definitions before calling varattr!) ")
            end if
        end subroutine nc_diag_varattr_rdouble_v
end module ncdw_varattr
