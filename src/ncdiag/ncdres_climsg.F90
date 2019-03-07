module ncdres_climsg
    ! NetCDF Diag Reader - CLI Message portion
    implicit none
    
    logical :: ncdres_enable_info = .FALSE.
    
    contains
        ! NetCDF Diag Reader - CLI Message portion
        ! (Subroutine/Function implementation)
        
        subroutine ncdres_error(err)
            character(len=*), intent(in) :: err
#ifdef ERROR_TRACEBACK
            integer                      :: div0
#endif
            write(*, "(A)") " **   ERROR: " // err
#ifdef ERROR_TRACEBACK
            write(*, "(A)") " ** Failed to read NetCDF4."
            write(*, "(A)") "    (Traceback requested, triggering div0 error...)"
            div0 = 1 / 0
            write(*, "(A)") "    Couldn't trigger traceback, ending gracefully."
            write(*, "(A)") "    (Ensure floating point exceptions are enabled,"
            write(*, "(A)") "    and that you have debugging (-g) and tracebacks"
            write(*, "(A)") "    compiler flags enabled!)"
            stop 1
#else
            write (*, "(A)") " ** Failed to read NetCDF4."
            stop 1
#endif
        end subroutine ncdres_error
        
        subroutine ncdres_warning(warn)
            character(len=*), intent(in) :: warn
            write(*, "(A)") " ** WARNING: " // warn
        end subroutine ncdres_warning
        
        subroutine ncdres_set_info_display(info_on_off)
            logical :: info_on_off
            ncdres_enable_info = info_on_off
        end subroutine ncdres_set_info_display
        
        subroutine ncdres_info(ifo)
            character(len=*), intent(in) :: ifo
            if (ncdres_enable_info) &
                write(*, "(A)") " **    INFO: " // ifo
        end subroutine ncdres_info
        
#ifdef _DEBUG_MEM_
        subroutine ncdres_debug(dbg)
            character(len=*), intent(in) :: dbg
            write(*, "(A, A)") "D: ", dbg
        end subroutine ncdres_debug
#endif

end module ncdres_climsg
