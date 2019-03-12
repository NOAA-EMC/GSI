module ncdc_data_MPI
    use ncd_kinds, only: i_byte, i_short, i_long, r_single, r_double
    use ncdc_state, only: prgm_name, cli_arg_count, input_count, &
        input_file, output_file,  ncid_input, &
        ncid_input, ncid_output, &
        var_arr_total, var_names, var_dim_names, var_output_ids, &
        var_types, var_counters, var_hasunlim, &
        dim_sizes, dim_names, dim_output_ids, dim_arr_total, &
        dim_counters, dim_unlim_sizes, &
#ifdef USE_MPI
        data_blobs, cur_proc, num_procs, ierr
#else
        data_blobs
#endif
    use ncdc_dims, only: nc_diag_cat_lookup_dim
    use ncdc_vars, only: nc_diag_cat_lookup_var
    
    use ncdc_climsg, only: ncdc_error, ncdc_warning, ncdc_info, &
        ncdc_check
    use ncdc_cli_process, only: ncdc_usage
    
    use netcdf, only: nf90_open, nf90_close, nf90_inquire, &
        nf90_inquire_dimension, nf90_inquire_variable, nf90_get_var, &
        nf90_put_var, nf90_inq_dimid, nf90_inq_varid, &
        NF90_NOWRITE, NF90_BYTE, NF90_SHORT, NF90_INT, NF90_FLOAT, &
        NF90_DOUBLE, NF90_CHAR, NF90_FILL_BYTE, NF90_FILL_SHORT, &
        NF90_FILL_INT, NF90_FILL_FLOAT, NF90_FILL_DOUBLE, &
        NF90_FILL_CHAR, NF90_MAX_NAME, &
        NF90_EBADDIM, NF90_NOERR, NF90_ENOTVAR
    
    implicit none
    
#ifdef USE_MPI
    include "mpif.h"
#endif
    
    contains
#ifdef USE_MPI
        subroutine nc_diag_cat_data_pass
            integer(i_long) :: cur_dim_id, cur_dim_len
            integer(i_long) :: cur_out_var_id, cur_out_var_ndims, cur_out_var_counter
            integer(i_long) :: cur_out_dim_ind, cur_out_var_ind, cur_out_var_type
            integer(i_long) :: var_index, arg_index, local_var_index
            integer(i_long), dimension(:), allocatable :: cur_out_dim_ids, cur_dim_ids
            integer(i_long), dimension(:), allocatable :: cur_out_dim_sizes
            integer(i_long), dimension(:), allocatable :: cur_dim_sizes
            
            ! Error tracking, temporary dimension ID storage to fetch
            ! the actual dimension value.
            integer(i_long) :: nc_err, tmp_dim_id
            
            ! Did we find a blank? If true, send a blank variable...
            ! and don't attempt to fetch any data!
            logical         :: found_blank
            
            integer(i_long)     :: tmp_dim_index
            integer(i_long)     :: input_ndims
            integer(i_long)     :: input_nvars
            integer(i_long)     :: input_nattrs
            
            character(len=NF90_MAX_NAME)               :: tmp_var_name
            integer(i_long)                            :: tmp_var_type, tmp_var_ndims
            integer(i_long), dimension(:), allocatable :: tmp_var_dimids
            character(len=NF90_MAX_NAME) , allocatable :: tmp_var_dim_names(:)
            
            integer(i_long), dimension(:), allocatable :: tmp_input_varids
            
            integer(i_byte),    dimension(:), allocatable     :: byte_buffer
            integer(i_short),   dimension(:), allocatable     :: short_buffer
            integer(i_long),    dimension(:), allocatable     :: long_buffer
            
            real(r_single),     dimension(:), allocatable     :: rsingle_buffer
            real(r_double),     dimension(:), allocatable     :: rdouble_buffer
            
            !character(len=1000),dimension(:), allocatable     :: string_buffer
            character(1)     ,dimension(:,:), allocatable     :: string_buffer
            
            integer(i_byte),  dimension(:,:), allocatable     :: byte_2d_buffer
            integer(i_short), dimension(:,:), allocatable     :: short_2d_buffer
            integer(i_long),  dimension(:,:), allocatable     :: long_2d_buffer
            
            real(r_single),   dimension(:,:), allocatable     :: rsingle_2d_buffer
            real(r_double),   dimension(:,:), allocatable     :: rdouble_2d_buffer
            
            character(1),   dimension(:,:,:), allocatable     :: string_2d_buffer
            
            type temp_storage
                integer(i_byte),    dimension(:), allocatable     :: byte_buffer
                integer(i_short),   dimension(:), allocatable     :: short_buffer
                integer(i_long),    dimension(:), allocatable     :: long_buffer
                
                real(r_single),     dimension(:), allocatable     :: rsingle_buffer
                real(r_double),     dimension(:), allocatable     :: rdouble_buffer
                
                !character(len=1000),dimension(:), allocatable     :: string_buffer
                character(1)     ,dimension(:,:), allocatable     :: string_buffer
                character(1)     ,dimension(:,:), allocatable     :: string_expanded_buffer
                character(1)     ,dimension(:),   allocatable     :: string_1d_buffer
                
                integer(i_byte),  dimension(:,:), allocatable     :: byte_2d_buffer
                integer(i_short), dimension(:,:), allocatable     :: short_2d_buffer
                integer(i_long),  dimension(:,:), allocatable     :: long_2d_buffer
                
                real(r_single),   dimension(:,:), allocatable     :: rsingle_2d_buffer
                real(r_double),   dimension(:,:), allocatable     :: rdouble_2d_buffer
                
                character(1),   dimension(:,:,:), allocatable     :: string_2d_buffer
                character(1),   dimension(:,:,:), allocatable     :: string_2d_expanded_buffer
            end type temp_storage
            
            type(temp_storage), dimension(:), allocatable         :: temp_storage_arr
            
            integer(i_long) :: i, i_proc, procs_done = 0, base_proc = 1
            integer(i_long) :: num_count, file_count = 0
            
            integer(i_long),  dimension(:),   allocatable     :: procs_done_arr
            
            logical :: mpi_read_flag = .FALSE.
            
            integer(i_long),    dimension(:), allocatable     :: read_var_count
            
            integer(i_long) :: mpi_status(MPI_STATUS_SIZE)
            integer(i_long),    dimension(:), allocatable     :: mpi_requests
            integer(i_long)                                   :: mpi_requests_total = 0
            
            integer(i_long)                                   :: mpi_request_EOF
            integer(i_long)                                   :: mpi_request_EOP
            
            character(len=NF90_MAX_NAME) , allocatable :: tmp_in_dim_names(:)
            
            character(len=1000)                :: err_string
            
            character(:), allocatable :: input_file_cut
            
            if (.NOT. allocated(var_names)) then
                call ncdc_warning("No variables found to concatenate.")
                return
            end if
            
#ifdef DEBUG
            print *, " !!! BEGINNING DATA PASS!!"
#endif
            
            input_count = cli_arg_count - 2
            
            if (cur_proc /= 0) then
                call ncdc_info("Reading in data from all files...")
                
                ! Allocate the correct amount of requests needed for the
                ! files and variables!
                ! 
                ! We need (num of files * num of vars) space.
                ! 
                ! Number of files is a bit tricky to determine, but not too bad!
                !  -> If the total number of files divides evenly into the number
                !     of processors handling files (num_procs - 1), then we just
                !     divide and multiply.
                !  -> If we have a remainder, and the current process is less
                !     than or equal to (input_count % (num_procs - 1)), do the
                !     same, but add 1 extra after dividing, THEN multiply.
                !  -> If we have a remainder, but we are greater than that,
                !     just do simple division and multiplication without adding
                !     anything.
                
                if (mod(input_count, num_procs - 1) == 0) then
                    allocate(mpi_requests((input_count / (num_procs - 1)) * (var_arr_total + 1) + 1))
                    allocate(temp_storage_arr((input_count / (num_procs - 1)) * (var_arr_total + 1) + 1))
                else
                    if (cur_proc <= mod(input_count, num_procs - 1)) then
                        allocate(mpi_requests(((input_count / (num_procs - 1)) + 1) * (var_arr_total + 1) + 1))
                        allocate(temp_storage_arr(((input_count / (num_procs - 1)) + 1) * (var_arr_total + 1) + 1))
                    else
                        allocate(mpi_requests((input_count / (num_procs - 1)) * (var_arr_total + 1) + 1))
                        allocate(temp_storage_arr((input_count / (num_procs - 1)) * (var_arr_total + 1) + 1))
                    end if
                end if
                
                mpi_request_EOF = var_arr_total + 1000
                mpi_request_EOP = var_arr_total + 2000
                
                ! For each processor 1 ... n, do every (n - proc + 1) task.
                ! Example:
                !     Total # of tasks:      20
                !     Total # of processors: 5 (so 0 root, 1-4)
                !     Formula: (task # - 1) mod (# procs - 1) == (proc # - 1)
                !                                            Zero Indexed     | Fortran Indexed
                !     processor 0: is collecting flowers and not doing anything
                !     processor 1: (task # - 1) mod 4 == 0 | 0, 4, 8,  12, 16 | 1, 5, 9,  13, 17
                !     processor 2: (task # - 1) mod 4 == 1 | 1, 5, 9,  13, 17 | 2, 6, 10, 14, 18
                !     processor 3: (task # - 1) mod 4 == 2 | 2, 6, 10, 14, 18 | 3, 7, 11, 15, 19
                !     processor 4: (task # - 1) mod 4 == 3 | 3, 7, 11, 15, 19 | 4, 8, 12, 16, 20
                ! We could do an if statement using mod... but can we do better? YES!
                ! Looking at the Fortran indexed tasks for each processor, we can set
                ! our initial to (proc #), and then just add (num_procs - 1) after.
                do arg_index = cur_proc, input_count, num_procs - 1
                    call get_command_argument(2 + arg_index, input_file)
                    
                    input_file_cut = trim(input_file)
                    
                    if (len(input_file_cut) <= 0) then
                        call ncdc_usage("Invalid input file name - likely blank!")
                    end if
                    
                    if (input_file_cut == output_file) then
                        ! No warning here - we've already shown it in metadata.
                        call ncdc_info(" -> Skipping " // input_file_cut // " since it is the output file...")
                    else
#ifndef QUIET
                        call ncdc_info(" -> Reading data from " // input_file_cut // "...")
#endif
                        call ncdc_check(nf90_open(input_file, NF90_NOWRITE, ncid_input, &
                            cache_size = 2147483647))
                        
                        ! Get top level info about the file!
                        call ncdc_check(nf90_inquire(ncid_input, nDimensions = input_ndims, &
                            nVariables = input_nvars, nAttributes = input_nattrs))
                        
                        ! Dimensions
                        allocate(tmp_in_dim_names(input_ndims))
                        do tmp_dim_index = 1, input_ndims
                            call ncdc_check(nf90_inquire_dimension(ncid_input, tmp_dim_index, &
                                tmp_in_dim_names(tmp_dim_index)))
                        end do
                        
                        ! Variables
                        allocate(tmp_input_varids(input_nvars))
                        
                        ! Loop through each variable!
                        do local_var_index = 1, var_arr_total
                            var_index = -1
                            
                            ! We iterate through our local variable storage
                            ! to keep things consistent, especially with the order
                            ! with which we send out variables to rank 0.
                            ! We also do this to detect "blank" variables, or
                            ! variables that don't exist in our input
                            ! file. Once we find out which files don't
                            ! exist, we set a blank flag to trigger a
                            ! few things to make this all work out!
                            nc_err = nf90_inq_varid(ncid_input, var_names(local_var_index), var_index)
                            
                            if (nc_err == NF90_ENOTVAR) then
                                ! We need to make a blank!
                                found_blank = .TRUE.
                            else if (nc_err /= NF90_NOERR) then
                                call ncdc_check(nc_err)
                            else
                                found_blank = .FALSE.
                            end if
                            
                        !do var_index = 1, input_nvars
                            
                            if (found_blank) then
                                tmp_var_ndims = var_dim_names(local_var_index)%num_names
                                tmp_var_name = var_names(local_var_index)
                            else
                                ! Grab number of dimensions and attributes first
                                call ncdc_check(nf90_inquire_variable(ncid_input, var_index, name = tmp_var_name, &
                                    ndims = tmp_var_ndims))
                            end if
                            
                            ! Allocate temporary variable dimids storage!
                            allocate(tmp_var_dimids(tmp_var_ndims))
                            allocate(tmp_var_dim_names(tmp_var_ndims))
                            allocate(cur_dim_ids(tmp_var_ndims))
                            allocate(cur_dim_sizes(tmp_var_ndims))
                            allocate(cur_out_dim_ids(tmp_var_ndims))
                            allocate(cur_out_dim_sizes(tmp_var_ndims))
                            
#ifdef DEBUG
                            write (*, "(A)") "*************************************"
                            write (*, "(A)") "NR0 DEBUG: file is " // input_file_cut
                            write (*, "(A)") "NR0 DEBUG: var is " // trim(var_names(local_var_index))
#endif
                            
                            if (found_blank) then
#ifdef DEBUG
                                write (*, "(A)") "NR0 DEBUG: (in found_blank state)"
#endif
                                do i = 1, tmp_var_ndims
                                    cur_out_dim_ind = nc_diag_cat_lookup_dim(var_dim_names(local_var_index)%dim_names(i))
                                    cur_out_dim_ids(i)   = dim_output_ids(cur_out_dim_ind)
                                    cur_out_dim_sizes(i) = dim_sizes(cur_out_dim_ind)
                                    
                                    nc_err = nf90_inq_dimid(ncid_input, var_dim_names(local_var_index)%dim_names(i), &
                                            tmp_dim_id)
                                    
#ifdef DEBUG
                                    write (*, "(A, I0, A)") "NR0 DEBUG: dim is " // trim(var_dim_names(local_var_index)%dim_names(i)) // " (ID = ", i, ")"
#endif
                                    
                                    if (nc_err /= NF90_EBADDIM) then
                                        if (nc_err /= NF90_NOERR) then
                                            call ncdc_check(nc_err)
                                        else
                                            call ncdc_check( &
                                                nf90_inquire_dimension(ncid_input, &
                                                tmp_dim_id, len = cur_dim_sizes(i)) )
#ifdef DEBUG
                                            write (*, "(A)") "NR0 DEBUG: using file cur_dim_sizes"
#endif
                                        end if
                                    else
                                        cur_dim_sizes(i) = cur_out_dim_sizes(i)
#ifdef DEBUG
                                        write (*, "(A)") "NR0 DEBUG: using cur_dim_sizes = cur_out_dim_sizes"
#endif
                                    end if
#ifdef DEBUG
                                    write (*, "(A, I0)") "NR0 DEBUG: final cur_dim_sizes(i) = ", cur_dim_sizes(i)
#endif
                                end do
                                
                                ! Make sure to set the var type for the "blank" var!
                                tmp_var_type = var_types(local_var_index)
                            else
                                ! Grab the actual dimension IDs and attributes
                                call ncdc_check(nf90_inquire_variable(ncid_input, var_index, dimids = tmp_var_dimids, &
                                    xtype = tmp_var_type))
                                
                                do i = 1, tmp_var_ndims
                                    call ncdc_check(nf90_inquire_dimension(ncid_input, tmp_var_dimids(i), &
                                        tmp_var_dim_names(i), cur_dim_sizes(i)))
                                    cur_out_dim_ind = nc_diag_cat_lookup_dim(tmp_var_dim_names(i))
                                    cur_out_dim_ids(i)   = dim_output_ids(cur_out_dim_ind)
                                    cur_out_dim_sizes(i) = dim_sizes(cur_out_dim_ind)
#ifdef DEBUG
                                    write (*, "(A, I0)") "NR0 DEBUG: final cur_dim_sizes(i) = ", cur_dim_sizes(i)
#endif
                                end do
                            end if
                            
                            ! Now, let's lookup everything and translate the result to our file.
                            cur_out_var_ind = nc_diag_cat_lookup_var(tmp_var_name)
                            cur_out_var_id = var_output_ids(cur_out_var_ind)
                            cur_out_var_ndims = var_dim_names(cur_out_var_ind)%num_names
                            cur_out_var_counter = var_counters(cur_out_var_ind)
                            
                            ! One-time only vars - if we are blank, do NOTHING.
                            if ((.NOT. any(cur_out_dim_sizes == -1)) .AND. (cur_out_var_counter == 0) &
                                .AND. (found_blank)) then
                                cycle
                            end if
                            
#ifdef DEBUG
                            write (*, "(A, I0)") "NR0 DEBUG: cur_out_var_ndims = ", cur_out_var_ndims
#endif
                            
                            ! Check for one-time only vars...
                            if (((.NOT. any(cur_out_dim_sizes == -1)) .AND. (cur_out_var_counter == 0)) &
                                .OR. (any(cur_out_dim_sizes == -1))) then
                                if ((cur_out_var_ndims == 1) .OR. &
                                    ((cur_out_var_ndims == 2) .AND. (tmp_var_type == NF90_CHAR))) then
                                    mpi_requests_total = mpi_requests_total + 1
                                    
                                    if (tmp_var_type == NF90_BYTE) then
                                        allocate(temp_storage_arr(mpi_requests_total)%byte_buffer   (cur_dim_sizes(1)))
                                        ! EMPTY FILL GOES HERE
                                        temp_storage_arr(mpi_requests_total)%byte_buffer = NF90_FILL_BYTE
                                        
                                        if (.NOT. found_blank) &
                                            call ncdc_check(nf90_get_var(ncid_input, var_index, &
                                                temp_storage_arr(mpi_requests_total)%byte_buffer))
                                        
                                        ! Args: the variable, number of elements to send,
                                        ! data type (in MPI land), destination process #,
                                        ! numeric tag for extra info, and communicator.
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%byte_buffer, &
                                            cur_dim_sizes(1), MPI_BYTE, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                    else if (tmp_var_type == NF90_SHORT) then
                                        allocate(temp_storage_arr(mpi_requests_total)%short_buffer  (cur_dim_sizes(1)))
                                        temp_storage_arr(mpi_requests_total)%short_buffer = NF90_FILL_SHORT
                                        
                                        if (.NOT. found_blank) &
                                            call ncdc_check(nf90_get_var(ncid_input, var_index, &
                                                temp_storage_arr(mpi_requests_total)%short_buffer))
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%short_buffer, &
                                            cur_dim_sizes(1), MPI_SHORT, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                    else if (tmp_var_type == NF90_INT) then
                                        allocate(temp_storage_arr(mpi_requests_total)%long_buffer   (cur_dim_sizes(1)))
                                        temp_storage_arr(mpi_requests_total)%long_buffer = NF90_FILL_INT
                                        
                                        if (.NOT. found_blank) &
                                            call ncdc_check(nf90_get_var(ncid_input, var_index, &
                                                temp_storage_arr(mpi_requests_total)%long_buffer))
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%long_buffer, &
                                            cur_dim_sizes(1), MPI_INT, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                    else if (tmp_var_type == NF90_FLOAT) then
                                        allocate(temp_storage_arr(mpi_requests_total)%rsingle_buffer(cur_dim_sizes(1)))
                                        temp_storage_arr(mpi_requests_total)%rsingle_buffer = NF90_FILL_FLOAT
                                        
                                        if (.NOT. found_blank) &
                                            call ncdc_check(nf90_get_var(ncid_input, var_index, &
                                                temp_storage_arr(mpi_requests_total)%rsingle_buffer, &
                                                start = (/ 1 /), &
                                                count = (/ cur_dim_sizes(1) /) ))
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%rsingle_buffer, &
                                            cur_dim_sizes(1), MPI_FLOAT, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                    else if (tmp_var_type == NF90_DOUBLE) then
                                        allocate(temp_storage_arr(mpi_requests_total)%rdouble_buffer(cur_dim_sizes(1)))
                                        temp_storage_arr(mpi_requests_total)%rdouble_buffer = NF90_FILL_DOUBLE
                                        !print *, cur_dim_sizes(1)
                                        
                                        if (.NOT. found_blank) &
                                            call ncdc_check(nf90_get_var(ncid_input, var_index, &
                                                temp_storage_arr(mpi_requests_total)%rdouble_buffer, &
                                                start = (/ 1 /), &
                                                count = (/ cur_dim_sizes(1) /) ))
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%rdouble_buffer, &
                                            cur_dim_sizes(1), MPI_DOUBLE, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                    else if (tmp_var_type == NF90_CHAR) then
                                        allocate(string_buffer   (cur_dim_sizes(1), cur_dim_sizes(2)))
                                        
                                        ! NOTE: the 2nd dim is nobs, so this is the actual file size.
                                        ! Other fields are the final sizes (maximum).
                                        allocate(temp_storage_arr(mpi_requests_total)%string_expanded_buffer (cur_out_dim_sizes(1), cur_dim_sizes(2)))
                                        
                                        string_buffer = NF90_FILL_CHAR
                                        temp_storage_arr(mpi_requests_total)%string_expanded_buffer = NF90_FILL_CHAR
                                        
                                        if (.NOT. found_blank) &
                                            call ncdc_check(nf90_get_var(ncid_input, var_index, string_buffer, &
                                                start = (/ 1, 1 /), &
                                                count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                                        
                                        temp_storage_arr(mpi_requests_total)%string_expanded_buffer(1:cur_dim_sizes(1), 1:cur_dim_sizes(2)) = &
                                            string_buffer
                                        
#ifdef DEBUG
                                        write (*, "(A, I0)") "NR0 DEBUG: cur_out_dim_sizes(1)* cur_dim_sizes(2) = ", cur_out_dim_sizes(1)* cur_dim_sizes(2)
#endif
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%string_expanded_buffer, &
                                            cur_out_dim_sizes(1)* cur_dim_sizes(2), MPI_BYTE, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                        
                                        deallocate(string_buffer)
                                    else
                                        write (err_string, "(A, I0, A)") &
                                            "Invalid type detected during write." // &
                                            CHAR(10) // "             " // &
                                            "(Variable '" // trim(tmp_var_name) // "' has an type of ", &
                                            tmp_var_type, "," // &
                                            CHAR(10) // "             " // &
                                            "which is invalid!)"
                                        call ncdc_error(trim(err_string))
                                    end if
                                else if ((cur_out_var_ndims == 2) .OR. &
                                    ((cur_out_var_ndims == 3) .AND. (tmp_var_type == NF90_CHAR))) then
                                    
                                    mpi_requests_total = mpi_requests_total + 1
                                    
                                    if (tmp_var_type == NF90_BYTE) then
                                        allocate(temp_storage_arr(mpi_requests_total)%byte_2d_buffer   (cur_dim_sizes(1), cur_dim_sizes(2)))
                                        
                                        temp_storage_arr(mpi_requests_total)%byte_2d_buffer = NF90_FILL_BYTE
                                        
                                        if (.NOT. found_blank) &
                                            call ncdc_check(nf90_get_var(ncid_input, var_index, &
                                                temp_storage_arr(mpi_requests_total)%byte_2d_buffer))
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%byte_2d_buffer, &
                                            cur_dim_sizes(1)* cur_dim_sizes(2), MPI_BYTE, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                    else if (tmp_var_type == NF90_SHORT) then
                                        allocate(temp_storage_arr(mpi_requests_total)%short_2d_buffer  (cur_dim_sizes(1), cur_dim_sizes(2)))
                                        
                                        temp_storage_arr(mpi_requests_total)%short_2d_buffer = NF90_FILL_SHORT
                                        
                                        if (.NOT. found_blank) &
                                            call ncdc_check(nf90_get_var(ncid_input, var_index, &
                                                temp_storage_arr(mpi_requests_total)%short_2d_buffer))
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%short_2d_buffer, &
                                            cur_dim_sizes(1)* cur_dim_sizes(2), MPI_SHORT, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                    else if (tmp_var_type == NF90_INT) then
                                        allocate(temp_storage_arr(mpi_requests_total)%long_2d_buffer   (cur_dim_sizes(1), cur_dim_sizes(2)))
                                        
                                        temp_storage_arr(mpi_requests_total)%long_2d_buffer = NF90_FILL_INT
                                        
                                        if (.NOT. found_blank) &
                                            call ncdc_check(nf90_get_var(ncid_input, var_index, &
                                                temp_storage_arr(mpi_requests_total)%long_2d_buffer))
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%long_2d_buffer, &
                                            cur_dim_sizes(1)* cur_dim_sizes(2), MPI_INT, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                    else if (tmp_var_type == NF90_FLOAT) then
                                        allocate(temp_storage_arr(mpi_requests_total)%rsingle_2d_buffer(cur_dim_sizes(1), cur_dim_sizes(2)))
                                        
                                        temp_storage_arr(mpi_requests_total)%rsingle_2d_buffer = NF90_FILL_FLOAT
                                        
                                        if (.NOT. found_blank) &
                                            call ncdc_check(nf90_get_var(ncid_input, var_index, &
                                                temp_storage_arr(mpi_requests_total)%rsingle_2d_buffer, &
                                                start = (/ 1, 1 /), &
                                                count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%rsingle_2d_buffer, &
                                            cur_dim_sizes(1)* cur_dim_sizes(2), MPI_FLOAT, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                    else if (tmp_var_type == NF90_DOUBLE) then
                                        allocate(temp_storage_arr(mpi_requests_total)%rdouble_2d_buffer(cur_dim_sizes(1), cur_dim_sizes(2)))
                                        
                                        temp_storage_arr(mpi_requests_total)%rdouble_2d_buffer = NF90_FILL_DOUBLE
                                        
                                        if (.NOT. found_blank) &
                                            call ncdc_check(nf90_get_var(ncid_input, var_index, &
                                                temp_storage_arr(mpi_requests_total)%rdouble_2d_buffer, &
                                                start = (/ 1, 1 /), &
                                                count = (/ cur_dim_sizes(1), cur_dim_sizes(2) /) ))
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%rdouble_2d_buffer, &
                                            cur_dim_sizes(1)* cur_dim_sizes(2), MPI_DOUBLE, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                    else if (tmp_var_type == NF90_CHAR) then
                                        allocate(string_2d_buffer (cur_dim_sizes(1), cur_dim_sizes(2), cur_dim_sizes(3)))
                                        
                                        ! NOTE: the 3rd dim is nobs, so this is the actual file size.
                                        ! Other fields are the final sizes (maximum).
                                        allocate(temp_storage_arr(mpi_requests_total)%string_2d_expanded_buffer &
                                            (cur_out_dim_sizes(1), cur_out_dim_sizes(2), cur_dim_sizes(3)))
                                        
                                        ! Same again, this time just multiplying...
                                        string_2d_buffer = NF90_FILL_CHAR
                                        temp_storage_arr(mpi_requests_total)%string_2d_expanded_buffer = NF90_FILL_CHAR
                                        
                                        if (.NOT. found_blank) &
                                            call ncdc_check(nf90_get_var(ncid_input, var_index, string_2d_buffer, &
                                                start = (/ 1, 1, 1 /), &
                                                count = (/ cur_dim_sizes(1), cur_dim_sizes(2), cur_dim_sizes(3) /) ))
                                        
                                        temp_storage_arr(mpi_requests_total)%string_2d_expanded_buffer &
                                            (1:cur_dim_sizes(1), 1:cur_dim_sizes(2), 1:cur_dim_sizes(3)) = &
                                                string_2d_buffer
                                        
                                        call MPI_ISend(temp_storage_arr(mpi_requests_total)%string_2d_expanded_buffer, &
                                            cur_out_dim_sizes(1)* cur_out_dim_sizes(2)* cur_dim_sizes(3), MPI_BYTE, &
                                            0, cur_out_var_ind, MPI_COMM_WORLD, &
                                            mpi_requests(mpi_requests_total), ierr)
                                        
                                        deallocate(string_2d_buffer)
                                    else
                                        write (err_string, "(A, I0, A)") &
                                            "Invalid type detected during write." // &
                                            CHAR(10) // "             " // &
                                            "(Variable '" // trim(tmp_var_name) // "' has an type of ", &
                                            tmp_var_type, "," // &
                                            CHAR(10) // "             " // &
                                            "which is invalid!)"
                                        call ncdc_error(trim(err_string))
                                    end if
                                end if
                                
                                var_counters(cur_out_var_ind) = &
                                        var_counters(cur_out_var_ind) + 1
                            end if
                            
                            ! Deallocate
                            deallocate(tmp_var_dimids)
                            deallocate(tmp_var_dim_names)
                            deallocate(cur_dim_ids)
                            deallocate(cur_dim_sizes)
                            deallocate(cur_out_dim_ids)
                            deallocate(cur_out_dim_sizes)
                        end do
                        
                        ! Update any unlimited counters
                        if (any(dim_sizes == -1)) then
                            do i = 1, dim_arr_total
                                ! Check for -1 - unlimited indicator
                                if ((dim_sizes(i) == -1) .AND. (any(tmp_in_dim_names == dim_names(i)))) then
                                    ! We got one! But... we need to find this dimension in the file.
                                    ! First, lookup dimension name to get dimension ID.
                                    call ncdc_check(nf90_inq_dimid(ncid_input, dim_names(i), cur_dim_id))
                                    
                                    ! Then, grab the current unlimited dimension length!
                                    call ncdc_check(nf90_inquire_dimension(ncid_input, cur_dim_id, len = cur_dim_len))
                                    
                                    ! Add the length to the counter!
                                    dim_counters(i) = dim_counters(i) + cur_dim_len
                                end if
                            end do
                        end if
                        
                        call ncdc_check(nf90_close(ncid_input))
                        
                        deallocate(tmp_input_varids)
                        deallocate(tmp_in_dim_names)
                    end if
                    
                    ! Send EOF notification
                    mpi_requests_total = mpi_requests_total + 1
                    call MPI_ISend(0, 1, MPI_INT, 0, mpi_request_EOF, MPI_COMM_WORLD, &
                        mpi_requests(mpi_requests_total), ierr)
                end do
                
                ! Send process competion notification
                mpi_requests_total = mpi_requests_total + 1
                call MPI_ISend(0, 1, MPI_INT, 0, mpi_request_EOP, MPI_COMM_WORLD, &
                    mpi_requests(mpi_requests_total), ierr)
                
                ! Flush all MPI communications!
                ! (Deallocate everything while we're at it!)
                call ncdc_info(" -> Flushing all data...")
                do i = 1, mpi_requests_total
                    call MPI_Wait(mpi_requests(i), mpi_status, ierr)
                end do
                
                ! This will deallocate everything, including internal stuff
                deallocate(temp_storage_arr)
            else
                ! Do collection!
                ! We know how much we need to receive - but we don't know
                ! when we will get our data. So we'll keep a tally of
                ! what data we got, and once we reach our limit, we'll be
                ! done!
                
                allocate(read_var_count(var_arr_total))
                read_var_count = 0
                
                allocate(procs_done_arr(num_procs - 1))
                procs_done_arr = -1
                
                procs_done = 0
                
                call ncdc_info("Receiving data from other processes...")
                
                base_proc = 1
                
                do while (file_count /= input_count)
                    do i_proc = 1, num_procs - 1
                        ! Make sure this process isn't already done
                        if (any(procs_done_arr == i_proc)) &
                            cycle
                        
                        call MPI_Probe(i_proc, MPI_ANY_TAG, MPI_COMM_WORLD, mpi_status, ierr)
                        
                        if (ierr /= 0) &
                            call ncdc_error("MPI ERROR OCCURRED!")
                        
                        ! Within mpi_status, we get the following:
                        !   MPI_SOURCE - the source process #
                        !   MPI_TAG    - the tag # for the index
                        !   MPI_ERROR  - error code of the probe operation
                        ! 
                        ! The above are also indexes to fetch said value, e.g.
                        ! mpi_status(MPI_SOURCE) will get the source process #,
                        ! mpi_status(MPI_TAG) will get the tag #, etc.
                        ! 
                        ! We also have a hidden value that needs to be "decoded"
                        ! via MPI_GET_COUNT. This gives us the number of
                        ! elements we'll be fetching.
                        ! 
                        ! Now we gotta decode all of this into something we can use!
                        ! Our MPI_TAG is a big helper for us. MPI_TAG contains the
                        ! relative variable index that we use in our database.
                        ! As a result, we can instantly figure out the dimensions
                        ! and type for the variable we are about to store.
                        ! 
                        ! With this information, we can take the count and divide it
                        ! by the fixed dimensions we just got to get our unlimited
                        ! dimensions (if applicable).
                        ! 
                        ! Now, with the fixed and unlimited dimensions, the types,
                        ! and the variable index from the tag, we can now reconstruct
                        ! the data.
                        ! 
                        ! First and foremost, we need to allocate a temporary variable
                        ! to store all of our data. Once we've allocated, we'll fetch
                        ! the data given all the parameters we got, plus the allocated
                        ! temporary variable.
                        ! 
                        ! Once everything's done, we'll go ahead and add it to the final
                        ! array, with the correct position pre-calculated!
                        ! 
                        ! Note - strings WILL be expanded to the pre-computed size.
                        
                        ! Finished file tag
                        if (mpi_status(MPI_TAG) == var_arr_total + 1000) then
                            file_count = file_count + 1
                            
                            call MPI_Recv(i, 1, MPI_INT, &
                                          i_proc, var_arr_total + 1000, MPI_COMM_WORLD, mpi_status, ierr)
                            
                            cycle
                        end if
                        
                        ! Finished process tag
                        if (mpi_status(MPI_TAG) == var_arr_total + 2000) then
                            call MPI_Recv(i, 1, MPI_INT, &
                                          i_proc, var_arr_total + 2000, MPI_COMM_WORLD, mpi_status, ierr)
                            procs_done = procs_done + 1
                            procs_done_arr(procs_done) = i_proc
                            cycle
                        end if
                        
                        cur_out_var_ind = mpi_status(MPI_TAG)
                        cur_out_var_ndims = var_dim_names(cur_out_var_ind)%num_names
                        
                        allocate(cur_out_dim_sizes(cur_out_var_ndims))
                        
                        cur_out_var_counter = var_counters(cur_out_var_ind)
                        cur_out_var_type = var_types(cur_out_var_ind)
                        
                        do i = 1, cur_out_var_ndims
                            cur_out_dim_ind = nc_diag_cat_lookup_dim(var_dim_names(cur_out_var_ind)%dim_names(i))
                            cur_out_dim_sizes(i) = dim_sizes(cur_out_dim_ind)
                        end do
                        
                        if (cur_out_var_type == NF90_BYTE)   call MPI_GET_COUNT(mpi_status, MPI_BYTE, num_count, ierr)
                        if (cur_out_var_type == NF90_SHORT)  call MPI_GET_COUNT(mpi_status, MPI_SHORT, num_count, ierr)
                        if (cur_out_var_type == NF90_INT)    call MPI_GET_COUNT(mpi_status, MPI_INT, num_count, ierr)
                        if (cur_out_var_type == NF90_FLOAT)  call MPI_GET_COUNT(mpi_status, MPI_FLOAT, num_count, ierr)
                        if (cur_out_var_type == NF90_DOUBLE) call MPI_GET_COUNT(mpi_status, MPI_DOUBLE, num_count, ierr)
                        if (cur_out_var_type == NF90_CHAR)   call MPI_GET_COUNT(mpi_status, MPI_BYTE, num_count, ierr)
                        
                        if (ierr /= 0) &
                            call ncdc_error("MPI ERROR OCCURRED!")
                        
                        ! Check for one-time only vars...
                        if (((.NOT. any(cur_out_dim_sizes == -1)) .AND. (cur_out_var_counter == 0)) &
                            .OR. (any(cur_out_dim_sizes == -1))) then
                            
                            if ((cur_out_var_ndims == 1) .OR. &
                                ((cur_out_var_ndims == 2) .AND. (cur_out_var_type == NF90_CHAR))) then
                                if (cur_out_var_type == NF90_BYTE) then
                                    allocate(byte_buffer   (num_count))
                                    byte_buffer = NF90_FILL_BYTE
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(byte_buffer, num_count, MPI_BYTE, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    data_blobs(cur_out_var_ind)%byte_buffer &
                                        (data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + num_count - 1) &
                                        = byte_buffer(:)
                                    
                                    deallocate(byte_buffer)
                                else if (cur_out_var_type == NF90_SHORT) then
                                    allocate(short_buffer   (num_count))
                                    short_buffer = NF90_FILL_SHORT
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(short_buffer, num_count, MPI_SHORT, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    data_blobs(cur_out_var_ind)%short_buffer &
                                        (data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + num_count - 1) &
                                        = short_buffer(:)
                                    
                                    deallocate(short_buffer)
                                else if (cur_out_var_type == NF90_INT) then
                                    allocate(long_buffer   (num_count))
                                    long_buffer = NF90_FILL_INT
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(long_buffer, num_count, MPI_INT, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    data_blobs(cur_out_var_ind)%long_buffer &
                                        (data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + num_count - 1) &
                                        = long_buffer(:)
                                    
                                    deallocate(long_buffer)
                                else if (cur_out_var_type == NF90_FLOAT) then
                                    allocate(rsingle_buffer   (num_count))
                                    rsingle_buffer = NF90_FILL_FLOAT
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(rsingle_buffer, num_count, MPI_FLOAT, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    data_blobs(cur_out_var_ind)%rsingle_buffer &
                                        (data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + num_count - 1) &
                                        = rsingle_buffer(:)
                                    
                                    deallocate(rsingle_buffer)
                                else if (cur_out_var_type == NF90_DOUBLE) then
                                    allocate(rdouble_buffer(num_count))
                                    rdouble_buffer = NF90_FILL_DOUBLE
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(rdouble_buffer, num_count, MPI_DOUBLE, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    data_blobs(cur_out_var_ind)%rdouble_buffer &
                                        (data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + num_count - 1) &
                                        = rdouble_buffer(:)
                                    
                                    deallocate(rdouble_buffer)
                                else if (cur_out_var_type == NF90_CHAR) then
                                    allocate(string_buffer   (cur_out_dim_sizes(1), num_count / cur_out_dim_sizes(1)))
                                    
                                    string_buffer = NF90_FILL_CHAR
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(string_buffer, num_count, MPI_BYTE, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
#ifdef DEBUG
                                    write (*, "(A)") "*****************************************************"
                                    write (*, "(A)") "DEBUG: var_name = [" // trim(var_names(cur_out_var_ind)) // "]"
                                    write (*, "(A, I0)") "DEBUG: num_count = ", num_count
                                    if (num_count > 0) &
                                        write (*, "(A)") "DEBUG: string_buffer = [" // string_buffer(1:cur_out_dim_sizes(1), 1) // "]"
                                    write (*, "(A, I0)") "DEBUG: cur_out_dim_sizes(1) = ", cur_out_dim_sizes(1)
                                    write (*, "(A, I0)") "DEBUG: (num_count / cur_out_dim_sizes(1)) = ", (num_count / cur_out_dim_sizes(1))
                                    write (*, "(A, I0)") "DEBUG: data_blobs(cur_out_var_ind)%cur_pos = ", data_blobs(cur_out_var_ind)%cur_pos
#endif
                                    data_blobs(cur_out_var_ind)%string_buffer &
                                        (1 : cur_out_dim_sizes(1), &
                                            data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + (num_count / cur_out_dim_sizes(1)) - 1) &
                                        = string_buffer(:,:)
                                    
                                    deallocate(string_buffer)
                                else
                                    write (err_string, "(A, I0, A)") &
                                        "Invalid type detected during write." // &
                                        CHAR(10) // "             " // &
                                        "(Variable '" // trim(tmp_var_name) // "' has an type of ", &
                                        cur_out_var_type, "," // &
                                        CHAR(10) // "             " // &
                                        "which is invalid!)"
                                    call ncdc_error(trim(err_string))
                                end if
                                
                                if (any(cur_out_dim_sizes == -1)) then
                                    if (cur_out_var_type == NF90_CHAR) then
                                        data_blobs(cur_out_var_ind)%cur_pos = &
                                            data_blobs(cur_out_var_ind)%cur_pos + &
                                            (num_count / cur_out_dim_sizes(1))
                                    else
                                        data_blobs(cur_out_var_ind)%cur_pos = &
                                            data_blobs(cur_out_var_ind)%cur_pos + &
                                            num_count
                                    end if
                                end if
                            else if (((cur_out_var_ndims == 2) .AND. (cur_out_var_type /= NF90_CHAR)) .OR. &
                                ((cur_out_var_ndims == 3) .AND. (cur_out_var_type == NF90_CHAR))) then
                                
                                if (cur_out_var_type == NF90_BYTE) then
                                    allocate(byte_2d_buffer   (cur_out_dim_sizes(1), num_count / cur_out_dim_sizes(1)))
                                    
                                    byte_2d_buffer = NF90_FILL_BYTE
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(byte_2d_buffer, num_count, MPI_BYTE, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    data_blobs(cur_out_var_ind)%byte_2d_buffer &
                                        (1 : cur_out_dim_sizes(1), &
                                            data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + (num_count / cur_out_dim_sizes(1)) - 1) &
                                        = byte_2d_buffer(:,:)
                                    
                                    deallocate(byte_2d_buffer)
                                else if (cur_out_var_type == NF90_SHORT) then
                                    allocate(short_2d_buffer  (cur_out_dim_sizes(1), num_count / cur_out_dim_sizes(1)))
                                    
                                    short_2d_buffer = NF90_FILL_SHORT
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(short_2d_buffer, num_count, MPI_SHORT, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    data_blobs(cur_out_var_ind)%short_2d_buffer &
                                        (1 : cur_out_dim_sizes(1), &
                                            data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + (num_count / cur_out_dim_sizes(1)) - 1) &
                                        = short_2d_buffer(:,:)
                                    
                                    deallocate(short_2d_buffer)
                                else if (cur_out_var_type == NF90_INT) then
                                    allocate(long_2d_buffer   (cur_out_dim_sizes(1), num_count / cur_out_dim_sizes(1)))
                                    
                                    long_2d_buffer = NF90_FILL_INT
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(long_2d_buffer, num_count, MPI_INT, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    data_blobs(cur_out_var_ind)%long_2d_buffer &
                                        (1 : cur_out_dim_sizes(1), &
                                            data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + (num_count / cur_out_dim_sizes(1)) - 1) &
                                        = long_2d_buffer(:,:)
                                    
                                    deallocate(long_2d_buffer)
                                else if (cur_out_var_type == NF90_FLOAT) then
                                    allocate(rsingle_2d_buffer(cur_out_dim_sizes(1), num_count / cur_out_dim_sizes(1)))
                                    
                                    rsingle_2d_buffer = NF90_FILL_FLOAT
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(rsingle_2d_buffer, num_count, MPI_FLOAT, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    data_blobs(cur_out_var_ind)%rsingle_2d_buffer &
                                        (1 : cur_out_dim_sizes(1), &
                                            data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + (num_count / cur_out_dim_sizes(1)) - 1) &
                                        = rsingle_2d_buffer(:,:)
                                    
                                    deallocate(rsingle_2d_buffer)
                                else if (cur_out_var_type == NF90_DOUBLE) then
                                    allocate(rdouble_2d_buffer(cur_out_dim_sizes(1), num_count / cur_out_dim_sizes(1)))
                                    
                                    rdouble_2d_buffer = NF90_FILL_DOUBLE
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(rdouble_2d_buffer, num_count, MPI_DOUBLE, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    data_blobs(cur_out_var_ind)%rdouble_2d_buffer &
                                        (1 : cur_out_dim_sizes(1), &
                                            data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + (num_count / cur_out_dim_sizes(1)) - 1) &
                                        = rdouble_2d_buffer(:,:)
                                    
                                    deallocate(rdouble_2d_buffer)
                                else if (cur_out_var_type == NF90_CHAR) then
                                    allocate(string_2d_buffer (cur_out_dim_sizes(1), cur_out_dim_sizes(2), &
                                        num_count / (cur_out_dim_sizes(1) * cur_out_dim_sizes(2))))
                                    
                                    string_2d_buffer = NF90_FILL_CHAR
                                    
                                    ! Args: the target variable, number of elements to recv,
                                    ! data type (in MPI land), source process #,
                                    ! numeric tag for extra info, and communicator.
                                    call MPI_Recv(string_2d_buffer, num_count, MPI_BYTE, &
                                        i_proc, cur_out_var_ind, MPI_COMM_WORLD, mpi_status, ierr)
                                    
                                    data_blobs(cur_out_var_ind)%string_2d_buffer &
                                        (1 : cur_out_dim_sizes(1), &
                                            1 : cur_out_dim_sizes(2), &
                                            data_blobs(cur_out_var_ind)%cur_pos : &
                                            data_blobs(cur_out_var_ind)%cur_pos + &
                                                (num_count / (cur_out_dim_sizes(1) * cur_out_dim_sizes(2))) - 1) &
                                        = string_2d_buffer(:,:,:)
                                    
                                    deallocate(string_2d_buffer)
                                else
                                    write (err_string, "(A, I0, A)") &
                                        "Invalid type detected during write." // &
                                        CHAR(10) // "             " // &
                                        "(Variable '" // trim(tmp_var_name) // "' has an type of ", &
                                        cur_out_var_type, "," // &
                                        CHAR(10) // "             " // &
                                        "which is invalid!)"
                                    call ncdc_error(trim(err_string))
                                end if
                                
                                if (any(cur_out_dim_sizes == -1)) then
                                    if (cur_out_var_type == NF90_CHAR) then
                                        data_blobs(cur_out_var_ind)%cur_pos = &
                                            data_blobs(cur_out_var_ind)%cur_pos + &
                                            (num_count / (cur_out_dim_sizes(1) * cur_out_dim_sizes(2)))
                                    else
                                        data_blobs(cur_out_var_ind)%cur_pos = &
                                            data_blobs(cur_out_var_ind)%cur_pos + &
                                            (num_count / cur_out_dim_sizes(1))
                                    end if
                                end if
                            else
                                write (err_string, "(A, I0, A, I0, A)") &
                                    "Invalid dimensions detected during write." // &
                                    CHAR(10) // "             " // &
                                    "(Variable '" // trim(tmp_var_name) // "' has an type of ", &
                                    cur_out_var_type, &
                                    ", with ", &
                                    cur_out_var_ndims, &
                                    " dimensions," // &
                                    CHAR(10) // "             " // &
                                    "which is invalid!)"
                                call ncdc_error(trim(err_string))
                            end if
                        end if
                        
                        ! Don't increment until we read the first round of files
                        if (((.NOT. any(cur_out_dim_sizes == -1)) .AND. &
                                (read_var_count(cur_out_var_ind) > (num_procs - 1))) &
                            .OR. (any(cur_out_dim_sizes == -1))) &
                            var_counters(cur_out_var_ind) = &
                                var_counters(cur_out_var_ind) + 1
                        
                        read_var_count(cur_out_var_ind) = read_var_count(cur_out_var_ind) + 1
                        
                        deallocate(cur_out_dim_sizes)
                    end do
                    ! End of process loop!
                end do
                
                ! Attempt to flush the MPI queue!
                ! We need to make sure everything exited properly...
                do i = 1, 1000
                    if (procs_done == (num_procs - 1)) then
                        exit
                    end if
                    do i_proc = 1, num_procs - 1
                        call MPI_Iprobe(i_proc, MPI_ANY_TAG, MPI_COMM_WORLD, mpi_read_flag, mpi_status, ierr)
                        
                        ! Skip the rest if we can't do anything!
                        if (.NOT. mpi_read_flag) &
                            cycle
                        
                        if (ierr /= 0) &
                            call ncdc_error("MPI ERROR OCCURRED!")
                        
                        ! Finished file tag
                        if (mpi_status(MPI_TAG) == var_arr_total + 1000) then
                            call ncdc_error("Inconsistency error - getting file completion after" &
                                // char(10) &
                                // "             main data loop end. BUG!")
                        else if (mpi_status(MPI_TAG) == var_arr_total + 2000) then
                            ! Finished process tag
                            call MPI_Recv(i, 1, MPI_INT, &
                                          i_proc, var_arr_total + 2000, MPI_COMM_WORLD, mpi_status, ierr)
                            
                            procs_done = procs_done + 1
                            procs_done_arr(procs_done) = i_proc
                            
                            cycle
                        else
                            ! We got data... that's really bad!
                            ! This is a bug and we need to exit, ASAP.
                            call ncdc_error("Inconsistency error - getting variable data after" &
                                // char(10) &
                                // "             main data loop end. BUG!")
                        end if
                    end do
                end do
                
                if (procs_done /= (num_procs - 1)) then
                    call ncdc_error("Inconsistency error - not all processes completed" &
                        // char(10) &
                        // "             before main data loop end. BUG!")
                end if
                
                deallocate(read_var_count)
            end if
        end subroutine nc_diag_cat_data_pass
        
        subroutine nc_diag_cat_data_commit
            integer(i_long) :: var_index
            
#ifndef QUIET
            call ncdc_info("Doing final data commit...")
#endif
            
            do var_index = 1, var_arr_total
#ifndef QUIET
                call ncdc_info(" => Writing variable " // trim(var_names(var_index)) // "...")
#endif
                if ((var_dim_names(var_index)%num_names == 1) .OR. &
                    ((var_dim_names(var_index)%num_names == 2) .AND. (var_types(var_index) == NF90_CHAR)) ) then
                    if (var_types(var_index) == NF90_BYTE) &
                        call ncdc_check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%byte_buffer, &
                            start = (/ 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1) /) ))
                    if (var_types(var_index) == NF90_SHORT) &
                        call ncdc_check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%short_buffer, &
                            start = (/ 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1) /) ))
                    if (var_types(var_index) == NF90_INT) &
                        call ncdc_check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%long_buffer, &
                            start = (/ 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1) /) ))
                    if (var_types(var_index) == NF90_FLOAT) &
                        call ncdc_check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%rsingle_buffer, &
                            start = (/ 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1) /) ))
                    
                    if (var_types(var_index) == NF90_DOUBLE) &
                        call ncdc_check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%rdouble_buffer, &
                            start = (/ 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1) /) ))
                    if (var_types(var_index) == NF90_CHAR) &
                        call ncdc_check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%string_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2) /) ))
                else if ((var_dim_names(var_index)%num_names == 2) .OR. &
                    ((var_dim_names(var_index)%num_names == 3) .AND. (var_types(var_index) == NF90_CHAR)) ) then
                    if (var_types(var_index) == NF90_BYTE) &
                        call ncdc_check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%byte_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2) /) ))
                    if (var_types(var_index) == NF90_SHORT) &
                        call ncdc_check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%short_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2) /) ))
                    if (var_types(var_index) == NF90_INT) &
                        call ncdc_check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%long_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2) /) ))
                    if (var_types(var_index) == NF90_FLOAT) &
                        call ncdc_check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%rsingle_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2) /) ))
                    if (var_types(var_index) == NF90_DOUBLE) &
                        call ncdc_check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%rdouble_2d_buffer, &
                            start = (/ 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2) /) ))
                    if (var_types(var_index) == NF90_CHAR) &
                        call ncdc_check(nf90_put_var(ncid_output, var_output_ids(var_index), &
                            data_blobs(var_index)%string_2d_buffer, &
                            start = (/ 1, 1, 1 /), &
                            count = (/ data_blobs(var_index)%alloc_size(1), &
                                data_blobs(var_index)%alloc_size(2), &
                                data_blobs(var_index)%alloc_size(3) /) ))
                end if
            end do
        end subroutine nc_diag_cat_data_commit
#endif
end module ncdc_data_MPI
