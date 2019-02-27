module ncdr_climsg
    implicit none
    
    ! NetCDF Diag Reader - CLI Message portion
    ! (Declarations)
    logical :: ncdr_enable_info = .FALSE.
    
    contains
        ! NetCDF Diag Reader - CLI Message portion
        ! (Subroutine/Function implementation)
        
        subroutine ncdr_error(err)
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
        end subroutine ncdr_error
        
        subroutine ncdr_warning(warn)
            character(len=*), intent(in) :: warn
            write(*, "(A)") " ** WARNING: " // warn
        end subroutine ncdr_warning
        
        subroutine ncdr_set_info_display(info_on_off)
            logical :: info_on_off
            ncdr_enable_info = info_on_off
        end subroutine ncdr_set_info_display
        
        subroutine ncdr_info(ifo)
            character(len=*), intent(in) :: ifo
            if (ncdr_enable_info) &
                write(*, "(A)") " **    INFO: " // ifo
        end subroutine ncdr_info
        
#ifdef _DEBUG_MEM_
        subroutine ncdr_debug(dbg)
            character(len=*), intent(in) :: dbg
            write(*, "(A, A)") "D: ", dbg
        end subroutine ncdr_debug
#endif

end module ncdr_climsg
