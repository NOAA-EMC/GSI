program nc_diag_cat
    use ncd_kinds, only: r_double
    use ncdc_climsg, only: ncdc_info, ncdc_warning, ncdc_error, &
        ncdc_check
    use ncdc_cli_process, only: nc_diag_cat_process_args
    
#ifdef USE_MPI
    use ncdc_state, only: output_file, ncid_output, ierr, cur_proc, num_procs
    use ncdc_data_mpi, only: nc_diag_cat_data_pass, nc_diag_cat_data_commit
#else
    use ncdc_state, only: output_file, ncid_output
    use ncdc_data, only: nc_diag_cat_data_pass, nc_diag_cat_data_commit
#endif
    
    use ncdc_metadata, only: nc_diag_cat_metadata_pass, &
        nc_diag_cat_metadata_define, nc_diag_cat_metadata_alloc
    
    use netcdf, only: nf90_inq_libvers, nf90_create, nf90_close, &
        NF90_NETCDF4, NF90_CLOBBER
    
    implicit none
    
#ifdef USE_MPI
    include "mpif.h"
#endif
    
    ! NCDC = Net CDF Diag Concatenation
    character(len=300)                 :: info_str
    
    real(r_double) :: start_time, stop_time
    
#ifdef USE_MPI
    ! MPI is essentially a smarter fork()... but remember, we're still
    ! forking! That means that there WILL be multiple processes!
    
    ! Do MPI things:
    ! First, initialize it!
    call MPI_INIT(ierr)
    
    ! Get the current processor (or really, the "PC") number
    call MPI_COMM_RANK(MPI_COMM_WORLD, cur_proc, ierr)
    
    ! Get the total number of processors / PCs
    call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)
    
    if (num_procs < 2) &
        call ncdc_error("At least 2 processors are required to use MPI features.")
    
    if (num_procs < 3) &
        call ncdc_warning("3 processors or more is needed to best use MPI features.")
    
    if (cur_proc == 0) &
        call ncdc_info("Using MPI for faster concatenation.")
#endif
    
    call ncdc_info('Initializing netcdf layer library, version ' // trim(nf90_inq_libvers()) // '...')
    
    ! nc_diag_cat steps:
    !   1) Do a quick pass to read metadata, then allocate space as
    !      necessary.
    !   2) Define variables with metadata. Do NOT store attributes.
    !   3) Read all the files, and add data to the output file.
    
    call nc_diag_cat_process_args
    
#ifdef USE_MPI
    if (cur_proc == 0) then
#endif
        call ncdc_info("Creating new NetCDF file: " // trim(output_file))
        call ncdc_check( nf90_create(output_file, OR(NF90_NETCDF4, NF90_CLOBBER), ncid_output, &
            0) )
#ifdef USE_MPI
    end if
#endif
    
    call cpu_time(start_time)
    call nc_diag_cat_metadata_pass
    call cpu_time(stop_time)
    
    write (info_str, "(A, F0.3, A)") "Metadata read took ", stop_time - start_time, " seconds!"
    call ncdc_info(trim(info_str))
    
#ifdef USE_MPI
    if (cur_proc == 0) then
#endif
        call nc_diag_cat_metadata_define
        
#ifdef DEBUG
        print *, "MAIN: trigger data pass!"
#endif
    
        call cpu_time(start_time)
        call nc_diag_cat_metadata_alloc
        call cpu_time(stop_time)
        
        write (info_str, "(A, F0.3, A)") "Data preallocation took ", stop_time - start_time, " seconds!"
        call ncdc_info(trim(info_str))
#ifdef USE_MPI
    end if
#endif
    
    call cpu_time(start_time)
    call nc_diag_cat_data_pass
    call cpu_time(stop_time)
    
    write (info_str, "(A, F0.3, A)") "Data read took ", stop_time - start_time, " seconds!"
    call ncdc_info(trim(info_str))
    
#ifdef USE_MPI
    if (cur_proc == 0) then
#endif
        call cpu_time(start_time)
        call nc_diag_cat_data_commit
        call cpu_time(stop_time)
        
        write (info_str, "(A, F0.3, A)") "Data commit took ", stop_time - start_time, " seconds!"
        call ncdc_info(trim(info_str))
    
#ifdef DEBUG
        print *, "ALL DONE!"
#endif
    
        call ncdc_info("All data queued, letting NetCDF take over (and actually write)!")
        
        call cpu_time(start_time)
        call ncdc_check(nf90_close(ncid_output))
        call cpu_time(stop_time)
        
        write (info_str, "(A, F0.3, A)") "Final data write took ", stop_time - start_time, " seconds!"
        call ncdc_info(trim(info_str))
#ifdef USE_MPI
    endif
    
    call MPI_FINALIZE(ierr)
#endif
    
    call ncdc_info("All done!")
end program nc_diag_cat
