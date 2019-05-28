module ncdc_climsg
    use ncd_kinds, only: i_long
    use netcdf, only: nf90_noerr, nf90_strerror
    
#ifdef USE_MPI
    use ncdc_state, only: cur_proc
#endif
    
    implicit none
    
#ifdef QUIET
    logical                            :: ncdc_enable_info = .FALSE.
    logical                            :: ncdc_enable_warn = .FALSE.
#else
    logical                            :: ncdc_enable_info = .TRUE.
    logical                            :: ncdc_enable_warn = .TRUE.
#endif
    
    contains
        subroutine ncdc_check(status)
          integer(i_long), intent(in) :: status
          
          if(status /= nf90_noerr) then 
            call ncdc_error(trim(nf90_strerror(status)))
          end if
        end subroutine ncdc_check
        
        subroutine ncdc_error(err)
            character(len=*), intent(in) :: err
#ifdef ERROR_TRACEBACK
            integer(i_long)              :: div0
#endif
#ifdef USE_MPI
                write(*, "(A, I0, A)") &
#else
                write(*, "(A)") &
#endif
#ifdef USE_MPI
                            "[PROC ", cur_proc, "]" // &
#endif
                            " **   ERROR: " // err 
#ifdef ERROR_TRACEBACK
            write(*, "(A)") " ** Failed to concatenate NetCDF4."
            write(*, "(A)") "    (Traceback requested, triggering div0 error...)"
            div0 = 1 / 0
            write(*, "(A)") "    Couldn't trigger traceback, ending gracefully."
            write(*, "(A)") "    (Ensure floating point exceptions are enabled,"
            write(*, "(A)") "    and that you have debugging (-g) and tracebacks"
            write(*, "(A)") "    compiler flags enabled!)"
            stop 1
#else
            write(*,"(A)")  " ** Failed to concatenate NetCDF4."
            stop " ** Failed to concatenate NetCDF4."
#endif
        end subroutine ncdc_error
        
        subroutine ncdc_warning(warn)
            character(len=*), intent(in) :: warn
            if (ncdc_enable_warn) &
#ifdef USE_MPI
                write(*, "(A, I0, A)") &
#else
                write(*, "(A)") &
#endif
#ifdef USE_MPI
                            "[PROC ", cur_proc, "]" // &
#endif
                            " ** WARNING: " // warn 
        end subroutine ncdc_warning
        
        subroutine ncdc_info(ifo)
            character(len=*), intent(in) :: ifo
            if (ncdc_enable_info) &
#ifdef USE_MPI
                write(*, "(A, I0, A)") &
#else
                write(*, "(A)") &
#endif
#ifdef USE_MPI
                                "[PROC ", cur_proc, "]" // &
#endif
                                " **    INFO: " // ifo  
        end subroutine ncdc_info
        
#ifdef _DEBUG_MEM_
        subroutine ncdc_debug(dbg)
            character(len=*), intent(in) :: dbg
            write(*, "(A, A)") "D: ", dbg
        end subroutine ncdc_debug
#endif
end module ncdc_climsg
