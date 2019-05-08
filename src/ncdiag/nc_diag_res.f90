! NetCDF Diag Resource file library

module nc_diag_res
    ! Library to read a resource file and check if a variable is
    ! enabled within the resource file.
    ! 
    ! This library reads a JSON resource file with the following format:
    !   {
    !     "variables" : {
    !       "some_var" : true,
    !       "more_var" : false
    !     }
    !   }
    ! 
    ! Based on this sample file, we can check whether a certain variable
    ! is enabled or not using this library:
    ! 
    !     call nc_diag_load_resource_file("resource.json")
    !     ! This will return true:
    !     if (nc_diag_load_check_variable("some_var")) then
    !         print *, "This variable exists!"
    !         ! Do some other things here
    !     end if
    !     ! This will return false:
    !     if (nc_diag_load_check_variable("more_var")) then
    !         print *, "This variable exists!"
    !         ! Do some other things here
    !     end if
    !     ! Note that we can specify non-existent variables - these
    !     ! will also return false.
    !     if (nc_diag_load_check_variable("hmmm_var")) then
    !         print *, "This variable exists!"
    !         ! Do some other things here
    !     end if
    !     call nc_diag_close_resource_file
    
    use ncdres_climsg, only: ncdres_error
    use nc_diag_fson, only: ncdf_value, ncdf_parse, &
        ncdf_get, ncdf_destroy
    
    implicit none
    
    type(ncdf_value), pointer :: nc_diag_json => null()
    
    contains
        ! Opens a given resource file for reading.
        ! 
        ! Given the resource file name, open the file and set everything
        ! up for reading the file. This includes any internal memory
        ! allocation required for reading the resource file.
        ! 
        ! In order for memory allocation to be freed, the
        ! subroutine nc_diag_close_resource_file MUST be called.
        ! 
        ! If a resource file is already open, this will raise an error
        ! and the program will terminate.
        ! 
        ! Args:
        !     filename (character(len=*)): resource file name to load.
        ! 
        ! Raises:
        !     Resource file already open error if there is already a
        !     resource file currently open.
        ! 
        subroutine nc_diag_load_resource_file(filename)
            character(len=*), intent(in) :: filename
            
            if (associated(nc_diag_json)) &
                call ncdres_error("Resource file already open!")
            
            nc_diag_json => ncdf_parse(filename)
        end subroutine nc_diag_load_resource_file
        
        ! Lookup a variable and check its status.
        ! 
        ! Given the variable name, lookup its status within the JSON
        ! resource file.
        ! 
        ! If the variable is present in the JSON file, and it is
        ! enabled, this will return true. Otherwise, if the variable
        ! doesn't exist in the resource file, or it is disabled,
        ! this will return false.
        ! 
        ! Args:
        !     var_name (character(len=*)): variable name to lookup
        !         within the resource file.
        ! 
        ! Returns:
        !     var_enabled (logical): whether the variable is enabled or
        !         not within the resource file.
        ! 
        function nc_diag_load_check_variable(var_name) result(var_enabled)
            character(len=*), intent(in)  :: var_name
            logical                       :: var_enabled
            
            character(len=1024)           :: var_str
            
            write (var_str, "(A)") "variables." // var_name
            
            var_enabled = .FALSE.
            
            call ncdf_get(nc_diag_json, trim(var_str), var_enabled)
        end function nc_diag_load_check_variable
        
        ! Closes the current resource file.
        ! 
        ! Closes a previously opened resource file. This will free any
        ! resources allocated towards the previous resource file, and
        ! allow for opening a new resource file.
        ! 
        ! If no file has been opened previously, or if the file is
        ! already closed, this will raise an error and the program will
        ! terminate.
        ! 
        ! Raises:
        !     No resource file open error will occur if there is no
        !     resource file currently open.
        ! 
        subroutine nc_diag_close_resource_file
            if (associated(nc_diag_json)) then
                call ncdf_destroy(nc_diag_json)
                nullify(nc_diag_json)
            else
                call ncdres_error("No resource file open!")
            end if
        end subroutine nc_diag_close_resource_file
end module nc_diag_res
