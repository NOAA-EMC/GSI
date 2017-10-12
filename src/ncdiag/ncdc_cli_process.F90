module ncdc_cli_process
    use ncdc_state, only: input_file, output_file, prgm_name, &
        cli_arg_count, dummy_arg
    
    implicit none
    
    contains
        subroutine ncdc_usage(err)
            character(len=*), intent(in), optional :: err
            
            if (present(err)) then
                write(*, "(A)") " ** ERROR: " // err
            end if
            
            call get_command_argument(0, prgm_name)
            write (*, "(A)") " nc_diag_cat v1.0"
            write (*, "(A)") " NetCDF Diag File Concatenator"
            write (*, "(A)") " Usage: " // trim(prgm_name) // " -o OUTPUT_FILE FILES..."
            write (*, "(A)") "     Concatenate the NetCDF files listed in FILES into OUTPUT_FILE."
            write (*, "(A)") "     At least 2 input files must be specified in order for this tool"
            write (*, "(A)") "     to run. The resulting file will be compressed."
            stop
        end subroutine ncdc_usage
        
        subroutine nc_diag_cat_process_args
            cli_arg_count = command_argument_count()
            
            if (cli_arg_count < 4) then
                call ncdc_usage
            end if
            
            ! Check for -o.
            ! We enforce this so that people really know what they're putting
            ! into this program!
            call get_command_argument(1, dummy_arg)
            
            if (trim(dummy_arg) /= "-o") then
                call ncdc_usage("Invalid option - '-o' must be specified in the 1st argument.")
            end if
            
            ! Grab output file argument
            call get_command_argument(2, output_file)
            
            if (len_trim(output_file) <= 0) then
                call ncdc_usage("Invalid output file name.")
            end if
            
            ! Grab first input file argument
            call get_command_argument(3, input_file)
            
            if (len_trim(input_file) <= 0) then
                call ncdc_usage("Invalid first input file name.")
            end if
            
            ! Grab second input file argument
            call get_command_argument(4, input_file)
            
            if (len_trim(input_file) <= 0) then
                call ncdc_usage("Invalid second input file name.")
            end if
            
            ! Sanity checks done!
        end subroutine nc_diag_cat_process_args
end module ncdc_cli_process
