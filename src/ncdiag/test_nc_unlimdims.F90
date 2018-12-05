program test_nc_unlimdims
    use ncd_kinds, only: i_long
    use ncdc_state, only: prgm_name, cli_arg_count, input_file, &
        ncid_input, num_unlims
    use ncdc_climsg, only: ncdc_error, ncdc_info, ncdc_check
    use netcdf, only: nf90_open, nf90_inquire_dimension, nf90_close, &
        NF90_MAX_NAME, NF90_NOWRITE
    use netcdf_unlimdims, only: pf_nf90_inq_unlimdims
    
    implicit none
    
#ifdef USE_MPI
! We don't use this option here, and setting it will cause problems
! with ncdc_util.F90, so let's unset it.
#undef USE_MPI
#endif
    
    integer(i_long) :: tmp_dim_size, i
    character(len=NF90_MAX_NAME) :: tmp_dim_name
    integer(i_long), dimension(:), allocatable        :: unlim_dims
    
    call get_command_argument(0, prgm_name)
    cli_arg_count = command_argument_count()
    
    if (cli_arg_count /= 1) &
        call ncdc_error("Usage: " // trim(prgm_name) // " [input NetCDF4 file]")
    
    call get_command_argument(1, input_file)
    
    call ncdc_info("Opening NetCDF4 file: " // trim(input_file))
    
    call ncdc_check(nf90_open(input_file, NF90_NOWRITE, ncid_input))
    
    call ncdc_check(pf_nf90_inq_unlimdims(ncid_input, num_unlims))
    
    write (*, "(A, I0)") "Number of unlimited dimensions: ", num_unlims
    allocate(unlim_dims(num_unlims))
    
    call ncdc_check(pf_nf90_inq_unlimdims(ncid_input, num_unlims, unlim_dims))
    
    do i = 1, num_unlims
        call ncdc_check(nf90_inquire_dimension(ncid_input, int(unlim_dims(i)), &
                            tmp_dim_name, tmp_dim_size))
        write (*, "(A, I0, A, I0, A)") " => Unlimited dimension | ID: ", unlim_dims(i), " | Size: ", tmp_dim_size, &
            " | Name = " // trim(tmp_dim_name)
    end do
    
    deallocate(unlim_dims)
    call ncdc_check(nf90_close(ncid_input))
end program test_nc_unlimdims
