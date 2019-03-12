! nc_diag_write - NetCDF Layer Diag Writing Module
! Copyright 2015 Albert Huang - SSAI/NASA for NASA GSFC GMAO (610.1).
! 
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
! 
!   http://www.apache.org/licenses/LICENSE-2.0
! 
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
! implied. See the License for the specific language governing
! permissions and limitations under the License.
! 
! command line message printing module - ncdw_climsg
!
module ncdw_climsg
    ! Module that provides command line message printing support.
    ! 
    ! This module has all of the subroutines needed to print various
    ! types of command line messages.
    ! 
    ! Message types include:
    !   -> Errors - errors that occur within the application. Errors
    !      will always result in the program exiting (via stop). If
    !      ANSI colors are enabled, this will show up in all red.
    !      
    !   -> Warnings - warnings that occur within the application. This
    !      will show a warning, but allow the program to continue. If
    !      ANSI colors are enabled, this will show up in yellow (or
    !      orange, depending on your terminal colors).
    !      
    !   -> Info - information about the application's progress. These
    !      tend to be verbose, hence the option to toggle them on and
    !      off. By default, they are turned off.
    !      
    !   -> Action - debug information that displays key subroutines and
    !      their arguments at the start of the subroutine. These are 
    !      very verbose, hence the option to toggle them on and off.
    !      
    !      In addition, since these are placed in front of subroutines,
    !      they require a compile time flag to turn on, since they take
    !      processing time.
    !      
    !      By default, due to the high verbosity, they are off.
    !      
    !   -> Debug - debug information about the application in general.
    !      These are extremely verbose, and can only be turned on with
    !      a compile time flag.
    ! 
    
    ! Load our numerical types from kinds - we just need our standard
    ! integer type, i_long
    use ncd_kinds, only: i_long
    
    use netcdf, only: nf90_noerr, nf90_strerror
    
    implicit none
    
    ! Whether to enable info message printing or not.
    ! By default, this is set to FALSE.
    logical :: nclayer_enable_info = .FALSE.
    
    ! Whether to enable action message printing or not.
    ! By default, this is set to FALSE.
    ! 
    ! Note that even if this is set to TRUE, action message support
    ! must be enabled at compile time for messages to be printed.
    logical :: nclayer_enable_action = .FALSE.
    
    contains
        ! Display a given error message, and exit.
        ! 
        ! Display a specified error message, and exit.
        ! 
        ! If ANSI colors are enabled at compile time, the entire message
        ! will be printed in red.
        ! 
        ! If error tracebacks are enabled, this will attempt to generate
        ! a traceback of the error before terminating.
        ! 
        ! Args:
        !     err (character(len=*)): the error to display.
        !     
        ! Raises:
        !     This is the error subroutine that exits, so it is
        !     basically... an error itself. So indeed, this WILL result
        !     in an error, no matter what!
        !     
        !     Although unlikely, other errors may indirectly occur.
        !     They may be general storage errors, or even a bug.
        !     These errors are likely to crash the program in unexpected
        !     ways...
        ! 
        subroutine nclayer_error(err)
            character(len=*), intent(in) :: err
#ifdef ERROR_TRACEBACK
            integer(i_long)              :: div0
#endif
            write(*, "(A)") " **   ERROR: " // err
#ifdef ERROR_TRACEBACK
            write(*, "(A)") " ** Failed to process data/write NetCDF4."
            write(*, "(A)") "    (Traceback requested, triggering div0 error...)"
            div0 = 1 / 0
#else
            stop " ** Failed to process data/write NetCDF4."
#endif
        end subroutine nclayer_error
        
        ! Display a given warning message.
        ! 
        ! Display a specified warning message.
        ! 
        ! If ANSI colors are enabled at compile time, the entire message
        ! will be printed in yellow or orange, depending on how your
        ! terminal displays colors.
        ! 
        ! Args:
        !     warn (character(len=*)): the warning to display.
        !     
        ! Raises:
        !     Although unlikely, other errors may indirectly occur.
        !     They may be general storage errors, or even a bug.
        !     These errors are likely to crash the program in unexpected
        !     ways...
        ! 
        subroutine nclayer_warning(warn)
            character(len=*), intent(in) :: warn
            write(*, "(A)") " ** WARNING: " // warn
        end subroutine nclayer_warning
        
        ! Set whether to display action messages or not.
        ! 
        ! This sets the flag on whether to display action messages or
        ! not.
        ! 
        ! If the provided argument is TRUE, action messages will be
        ! displayed. Otherwise, they will be hidden, even if action
        ! message calls are made.
        ! 
        ! Args:
        !     action_on_off (logical): boolean indicating whether to
        !         display action messages or not. If TRUE, action
        !         messages will be displayed. Otherwise, they will be
        !         hidden.
        !     
        ! Raises:
        !     Although unlikely, other errors may indirectly occur.
        !     They may be general storage errors, or even a bug.
        !     These errors are likely to crash the program in unexpected
        !     ways...
        ! 
        subroutine nc_set_action_display(action_on_off)
            logical :: action_on_off
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, L, A)") "nc_set_action_display(action_on_off = ", action_on_off, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            nclayer_enable_action = action_on_off
        end subroutine nc_set_action_display
        
#ifdef ENABLE_ACTION_MSGS
        ! Display a given action message.
        ! 
        ! Display a specified action message.
        ! 
        ! The messages displayed here are intended to be debug messages
        ! indicating the subroutine that was called, along with the
        ! arguments provided for the subroutine, if any.
        ! (Hence, the "action" message.)
        ! 
        ! An example of such a message:
        !   nc_set_action_display(action_on_off = T)
        ! 
        ! Although other kinds of messages can be printed via action
        ! messages, it's strongly recommended to only print subroutine
        ! and/or function calls here.
        ! 
        ! If ANSI colors are enabled at compile time, the entire message
        ! will be printed in cyan (light blue).
        ! 
        ! Args:
        !     act (character(len=*)): the action message to display.
        !     
        ! Raises:
        !     Although unlikely, other errors may indirectly occur.
        !     They may be general storage errors, or even a bug.
        !     These errors are likely to crash the program in unexpected
        !     ways...
        ! 
        subroutine nclayer_actionm(act)
            character(len=*), intent(in) :: act
            if (nclayer_enable_action) &
                write(*, "(A)") " **  ACTION: " // act
        end subroutine nclayer_actionm
#endif
        
        ! Set whether to display informational messages or not.
        ! 
        ! This sets the flag on whether to display information messages
        ! or not.
        ! 
        ! If the provided argument is TRUE, informational messages will
        ! be displayed. Otherwise, they will be hidden, even if info
        ! message calls are made.
        ! 
        ! Args:
        !     info_on_off (logical): boolean indicating whether to
        !         display informational messages or not. If TRUE,
        !         informational messages will be displayed. Otherwise,
        !         they will be hidden.
        !     
        ! Raises:
        !     Although unlikely, other errors may indirectly occur.
        !     They may be general storage errors, or even a bug.
        !     These errors are likely to crash the program in unexpected
        !     ways...
        ! 
        subroutine nc_set_info_display(info_on_off)
            logical :: info_on_off
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, L, A)") "nc_set_info_display(info_on_off = ", info_on_off, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            nclayer_enable_info = info_on_off
        end subroutine nc_set_info_display
        
        ! Display a given information message.
        ! 
        ! Display a specified information message.
        ! 
        ! If ANSI colors are enabled at compile time, the entire message
        ! will be printed in blue.
        ! 
        ! Args:
        !     ifo (character(len=*)): the information message to
        !         display.
        !     
        ! Raises:
        !     Although unlikely, other errors may indirectly occur.
        !     They may be general storage errors, or even a bug.
        !     These errors are likely to crash the program in unexpected
        !     ways...
        ! 
        subroutine nclayer_info(ifo)
            character(len=*), intent(in) :: ifo
            if (nclayer_enable_info) &
                write(*, "(A)") " **    INFO: " // ifo
        end subroutine nclayer_info
        
#ifdef _DEBUG_MEM_
        ! Display a given debug message.
        ! 
        ! Display a specified debug message. This subroutine is only
        ! enabled when _DEBUG_MEM_ is defined at compile time.
        ! Otherwise, this subroutine will not exist.
        ! 
        ! Therefore, any calls to this subroutine must have the
        ! '#ifdef _DEBUG_MEM_' and #endif surrounding it.
        ! 
        ! Args:
        !     dbg (character(len=*)): the debug message to display.
        !     
        ! Raises:
        !     Although unlikely, other errors may indirectly occur.
        !     They may be general storage errors, or even a bug.
        !     These errors are likely to crash the program in unexpected
        !     ways...
        ! 
        subroutine nclayer_debug(dbg)
            character(len=*), intent(in) :: dbg
            write(*, "(A, A)") "D: ", dbg
        end subroutine nclayer_debug
#endif
        
        ! Check whether a NetCDF operation completed successfully or
        ! not, and if not, display the corresponding error message.
        ! 
        ! Given the NetCDF error code integer, determine whether the
        ! corresponding NetCDF operation succeeded or not. If it failed,
        ! display the corresponding error message and exit.
        ! 
        ! Args:
        !     status (integer(i_long)): NetCDF error code to check.
        !     
        ! Raises:
        !     Although unlikely, other errors may indirectly occur.
        !     They may be general storage errors, NetCDF errors, or even
        !     a bug. See the called subroutines' documentation for
        !     details.
        ! 
        subroutine nclayer_check(status)
            integer(i_long), intent(in) :: status
            
            if(status /= nf90_noerr) then 
                call nclayer_error(trim(nf90_strerror(status)))
            end if
        end subroutine nclayer_check
end module ncdw_climsg
