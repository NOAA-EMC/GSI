module ncdw_lheader
    use ncd_kinds, only: i_byte, i_short, i_long, r_single, r_double
    use ncdw_state, only: ncid, init_done
    use ncdw_climsg, only: nclayer_error, nclayer_check
    use netcdf, only: nf90_put_att, NF90_GLOBAL
    
    implicit none
    
    interface nc_diag_header
        module procedure nc_diag_header_byte, &
            nc_diag_header_short, nc_diag_header_long, &
            nc_diag_header_rsingle, nc_diag_header_rdouble, &
            nc_diag_header_string, nc_diag_header_byte_v, &
            nc_diag_header_short_v, nc_diag_header_long_v, &
            nc_diag_header_rsingle_v, nc_diag_header_rdouble_v
    end interface nc_diag_header
    
    contains
        ! nc_diag_header - input integer(i_byte)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_header_byte(header_name, header_value)
            character(len=*), intent(in)    :: header_name
            integer(i_byte), intent(in)     :: header_value
            
            if (.NOT. init_done) &
                call nclayer_error("Can't write definitions - NetCDF4 layer not initialized yet!")
            call nclayer_check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_byte
        
        ! nc_diag_header - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_header_short(header_name, header_value)
            character(len=*), intent(in)    :: header_name
            integer(i_short), intent(in)    :: header_value
            
            if (.NOT. init_done) &
                call nclayer_error("Can't write definitions - NetCDF4 layer not initialized yet!")
            call nclayer_check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_short
        
        ! nc_diag_header - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_header_long(header_name, header_value)
            character(len=*), intent(in)    :: header_name
            integer(i_long), intent(in)     :: header_value
            
            if (.NOT. init_done) &
                call nclayer_error("Can't write definitions - NetCDF4 layer not initialized yet!")
            call nclayer_check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_long
        
        ! nc_diag_header - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_header_rsingle(header_name, header_value)
            character(len=*), intent(in)    :: header_name
            real(r_single), intent(in)      :: header_value
            
            if (.NOT. init_done) &
                call nclayer_error("Can't write definitions - NetCDF4 layer not initialized yet!")
            call nclayer_check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_rsingle
        
        ! nc_diag_header - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_header_rdouble(header_name, header_value)
            character(len=*), intent(in)    :: header_name
            real(r_double), intent(in)      :: header_value
            
            if (.NOT. init_done) &
                call nclayer_error("Can't write definitions - NetCDF4 layer not initialized yet!")
            call nclayer_check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_rdouble

        ! nc_diag_header - input character(len=*)
        ! Corresponding NetCDF4 type: string? char?
        subroutine nc_diag_header_string(header_name, header_value)
            character(len=*), intent(in)    :: header_name
            character(len=*), intent(in)    :: header_value
            
            if (.NOT. init_done) &
                call nclayer_error("Can't write definitions - NetCDF4 layer not initialized yet!")
            ! Note: using F95 trim here!
            call nclayer_check(nf90_put_att(ncid, NF90_GLOBAL, header_name, trim(header_value)))
        end subroutine nc_diag_header_string
        
        !=============================================================
        ! VECTOR TYPES
        !=============================================================
        
        ! nc_diag_header - input integer(i_byte), dimension(:)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_header_byte_v(header_name, header_value)
            character(len=*), intent(in)               :: header_name
            integer(i_byte), dimension(:), intent(in)  :: header_value
            
            if (.NOT. init_done) &
                call nclayer_error("Can't write definitions - NetCDF4 layer not initialized yet!")
            call nclayer_check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_byte_v
        
        ! nc_diag_header - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_header_short_v(header_name, header_value)
            character(len=*), intent(in)               :: header_name
            integer(i_short), dimension(:), intent(in) :: header_value
            
            if (.NOT. init_done) &
                call nclayer_error("Can't write definitions - NetCDF4 layer not initialized yet!")
            call nclayer_check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_short_v
        
        ! nc_diag_header - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_header_long_v(header_name, header_value)
            character(len=*), intent(in)               :: header_name
            integer(i_long), dimension(:), intent(in)  :: header_value
            
            if (.NOT. init_done) &
                call nclayer_error("Can't write definitions - NetCDF4 layer not initialized yet!")
            call nclayer_check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_long_v
        
        ! nc_diag_header - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_header_rsingle_v(header_name, header_value)
            character(len=*), intent(in)               :: header_name
            real(r_single), dimension(:), intent(in)   :: header_value
            
            if (.NOT. init_done) &
                call nclayer_error("Can't write definitions - NetCDF4 layer not initialized yet!")
            call nclayer_check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_rsingle_v
        
        ! nc_diag_header - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_header_rdouble_v(header_name, header_value)
            character(len=*), intent(in)               :: header_name
            real(r_double), dimension(:), intent(in)   :: header_value
            
            if (.NOT. init_done) &
                call nclayer_error("Can't write definitions - NetCDF4 layer not initialized yet!")
            call nclayer_check(nf90_put_att(ncid, NF90_GLOBAL, header_name, header_value))
        end subroutine nc_diag_header_rdouble_v
end module ncdw_lheader
