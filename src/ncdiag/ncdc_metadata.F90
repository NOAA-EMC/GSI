module ncdc_metadata
    use ncd_kinds, only: i_byte, i_short, i_long, r_single, r_double
    use ncdc_state, only: ncid_input, input_count, input_file, &
        ncid_output, output_file, &
        num_unlims, &
        dim_arr_total, dim_sizes, dim_names, dim_output_ids, &
        dim_unlim_sizes, &
        var_arr_total, var_dim_names, var_names, var_types, &
        var_output_ids, var_hasunlim, &
        cli_arg_count, &
#ifdef USE_MPI
        data_blobs, &
        cur_proc
#else
        data_blobs
#endif
    use ncdc_dims, only: nc_diag_cat_lookup_dim, &
        nc_diag_cat_metadata_add_dim
    use ncdc_vars, only: nc_diag_cat_metadata_add_var
    use ncdc_types, only: NC_DIAG_CAT_CHUNK_SIZE, &
        NC_DIAG_CAT_GZIP_COMPRESS
    use ncdc_climsg, only: ncdc_error, ncdc_warning, ncdc_info, &
        ncdc_check
    use ncdc_cli_process, only: ncdc_usage
    use netcdf, only: nf90_inquire_attribute, nf90_get_att, &
        nf90_put_att, nf90_open, nf90_close, nf90_inquire, &
        nf90_inq_attname, nf90_inquire_dimension, &
        nf90_inquire_variable, nf90_def_dim, nf90_def_var, &
        nf90_def_var_chunking, nf90_def_var_deflate, &
        NF90_BYTE, NF90_SHORT, NF90_INT, NF90_FLOAT, NF90_DOUBLE, &
        NF90_CHAR, NF90_FILL_BYTE, NF90_FILL_SHORT, NF90_FILL_INT, &
        NF90_FILL_FLOAT, NF90_FILL_DOUBLE, NF90_FILL_CHAR, &
        NF90_GLOBAL, NF90_NOWRITE, NF90_ENOTATT, &
        NF90_NOERR, NF90_MAX_NAME, NF90_UNLIMITED, NF90_CHUNKED
    use netcdf_unlimdims, only: pf_nf90_inq_unlimdims
    
    implicit none
    
    contains
        subroutine nc_diag_cat_copy_attr(attr_name, var_id_in, var_id_out)
            character(len=*), intent(in)               :: attr_name
            integer(i_long), intent(in)                :: var_id_in
            integer(i_long), intent(in), optional      :: var_id_out
            
            integer(i_byte), dimension(:), allocatable :: byte_arr
            integer(i_short),dimension(:), allocatable :: short_arr
            integer(i_long), dimension(:), allocatable :: long_arr
            real(r_single),dimension(:),   allocatable :: rsingle_arr
            real(r_double),dimension(:),   allocatable :: rdouble_arr
            character(len=:),              allocatable :: string_arr
            
            integer(i_long)                            :: attr_type, attr_len, final_var_id_out
            
            call ncdc_check(nf90_inquire_attribute(ncid_input, var_id_in, attr_name, &
                xtype = attr_type, len = attr_len))
            
            if (.NOT. present(var_id_out)) then
                if (var_id_in /= NF90_GLOBAL) &
                    call ncdc_error("BUG! var_id_out not specified even when var_id_in is var-specific!")
                final_var_id_out = var_id_in
            else
                final_var_id_out = var_id_out
            end if
            
            if (attr_type == NF90_BYTE) then
                allocate(byte_arr(attr_len))
                call ncdc_check(nf90_get_att(ncid_input, var_id_in, attr_name, byte_arr))
                call ncdc_check(nf90_put_att(ncid_output, final_var_id_out, attr_name, byte_arr))
                deallocate(byte_arr)
            else if (attr_type == NF90_SHORT) then
                allocate(short_arr(attr_len))
                call ncdc_check(nf90_get_att(ncid_input, var_id_in, attr_name, short_arr))
                call ncdc_check(nf90_put_att(ncid_output, final_var_id_out, attr_name, short_arr))
                deallocate(short_arr)
            else if (attr_type == NF90_INT) then
                allocate(long_arr(attr_len))
                call ncdc_check(nf90_get_att(ncid_input, var_id_in, attr_name, long_arr))
                call ncdc_check(nf90_put_att(ncid_output, final_var_id_out, attr_name, long_arr))
                deallocate(long_arr)
            else if (attr_type == NF90_FLOAT) then
                allocate(rsingle_arr(attr_len))
                call ncdc_check(nf90_get_att(ncid_input, var_id_in, attr_name, rsingle_arr))
                call ncdc_check(nf90_put_att(ncid_output, final_var_id_out, attr_name, rsingle_arr))
                deallocate(rsingle_arr)
            else if (attr_type == NF90_DOUBLE) then
                allocate(rdouble_arr(attr_len))
                call ncdc_check(nf90_get_att(ncid_input, var_id_in, attr_name, rdouble_arr))
                call ncdc_check(nf90_put_att(ncid_output, final_var_id_out, attr_name, rdouble_arr))
                deallocate(rdouble_arr)
            else if (attr_type == NF90_CHAR) then
                allocate(character(len=attr_len) :: string_arr)
                call ncdc_check(nf90_get_att(ncid_input, var_id_in, attr_name, string_arr))
                call ncdc_check(nf90_put_att(ncid_output, final_var_id_out, attr_name, string_arr))
                deallocate(string_arr)
            else
                call ncdc_error("Unable to copy attribute for unknown type!")
            end if
        end subroutine nc_diag_cat_copy_attr
        
        subroutine nc_diag_cat_metadata_pass
            character(len=1000) :: err_string
            integer(i_long)     :: old_dim_arr_total = 0, old_var_arr_total = 0
            
            integer(i_long)     :: tmp_dim_index, tmp_attr_index
            integer(i_long)     :: input_ndims, cached_ndims = -1
            integer(i_long)     :: input_nvars, cached_nvars = -1
            integer(i_long)     :: input_nattrs
            
            character(len=NF90_MAX_NAME)               :: tmp_var_name
            integer(i_long)                            :: tmp_var_type, tmp_var_ndims
            integer(i_long), dimension(:), allocatable :: tmp_var_dimids
            character(len=NF90_MAX_NAME) , allocatable :: tmp_var_dim_names(:)
            
            integer(i_long), dimension(:), allocatable :: unlim_dims
            logical                                    :: is_unlim = .FALSE.
            
            character(len=NF90_MAX_NAME)               :: tmp_dim_name, tmp_attr_name
            integer(i_long)                            :: tmp_dim_size
            
            integer(i_long)                            :: arg_index, var_index, i
            
            integer(i_long)                            :: nc_err
            
            character(:), allocatable :: input_file_cut
            
            input_count = cli_arg_count - 2
            
#ifndef QUIET
#ifdef USE_MPI
            if (cur_proc == 0) &
#endif
                call ncdc_info("Scanning NetCDF files for dimensions and variables...")
#endif
            
            do arg_index = 1, input_count
                call get_command_argument(2 + arg_index, input_file)
                
                input_file_cut = trim(input_file)
                
                if (len(input_file_cut) <= 0) then
                    call ncdc_usage("Invalid input file name - likely blank!")
                end if
                
                if (input_file_cut == output_file) then
                    call ncdc_warning(" -> Ignoring output file in input file list.")
                    call ncdc_info(" -> Skipping " // input_file_cut // " since it is the output file...")
                else
#ifndef QUIET
#ifdef USE_MPI
                    if (cur_proc == 0) &
#endif
                        call ncdc_info(" -> Opening " // input_file_cut // " for reading...")
#endif
                    
                    call ncdc_check(nf90_open(input_file, NF90_NOWRITE, ncid_input))
                    
                    ! Get top level info about the file!
                    call ncdc_check(nf90_inquire(ncid_input, nDimensions = input_ndims, &
                        nVariables = input_nvars, nAttributes = input_nattrs))
                    
#ifdef USE_MPI
                    if (cur_proc == 0) then
#endif
                        ! Fetch attributes and only add if they are NOT in the final file
                        do tmp_attr_index = 1, input_nattrs
                            call ncdc_check(nf90_inq_attname(ncid_input, NF90_GLOBAL, tmp_attr_index, tmp_attr_name))
                            
                            nc_err = nf90_inquire_attribute(ncid_output, &
                                NF90_GLOBAL, trim(tmp_attr_name))
                            
                            ! If attribute doesn't exist, add it!
                            if (nc_err == NF90_ENOTATT) then
                                call nc_diag_cat_copy_attr(trim(tmp_attr_name), NF90_GLOBAL)
                            else if (nc_err /= NF90_NOERR) then
                                ! Sanity check - could be another error!
                                call ncdc_check(nc_err)
                            end if
                        end do
#ifdef USE_MPI
                    end if
#endif
#ifdef DEBUG
                    write (*, "(A, I0)") "Number of dimensions: ", input_ndims
#endif
                    
                    if (cached_ndims == -1) &
                        cached_ndims = input_ndims
                    

                    if (input_ndims == 0) then
#ifndef QUIET
                        call ncdc_warning("No dimensions found in file " // input_file_cut // "! Skipping file...")
#endif
                        call ncdc_check(nf90_close(ncid_input))
                        cycle
                    end if
                    
#ifndef QUIET
                    if (input_nvars == 0) &
                        call ncdc_warning("No variables found in file " // input_file_cut // "!")
                    
                    if (cached_ndims /= input_ndims) &
                        call ncdc_warning("Number of dimensions in " // trim(input_file) // " does not match first input file.")
#endif
                    
                    ! Get unlimited dimension information
                    call ncdc_check(pf_nf90_inq_unlimdims(ncid_input, num_unlims))
                    
#ifdef DEBUG
                    write (*, "(A, I0)") "Number of unlimited dimensions: ", num_unlims
#endif
                    
                    allocate(unlim_dims(num_unlims))
                    
                    call ncdc_check(pf_nf90_inq_unlimdims(ncid_input, num_unlims, unlim_dims))
                    
                    ! Loop through each dimension!
                    do tmp_dim_index = 1, input_ndims
                        call ncdc_check(nf90_inquire_dimension(ncid_input, tmp_dim_index, &
                            tmp_dim_name, tmp_dim_size))
                        
                        is_unlim = .FALSE.
                        
                        do i = 1, num_unlims
                            if (tmp_dim_index == unlim_dims(i)) then
                                is_unlim = .TRUE.
                                exit
                            end if
                        end do
                        
                        if (is_unlim) then
#ifdef DEBUG
                            write (*, "(A, I0, A, I0, A)") " => Dimension #", tmp_dim_index, ": " // &
                                trim(tmp_dim_name) // " (size: ", &
                                tmp_dim_size, &
                                " - UNLIMITED)"
#endif
                            call nc_diag_cat_metadata_add_dim(tmp_dim_name, -1, tmp_dim_size)
                        else
#ifdef DEBUG
                            write (*, "(A, I0, A, I0, A)") " => Dimension #", tmp_dim_index, ": " // &
                                trim(tmp_dim_name) // " (size: ", &
                                tmp_dim_size, &
                                ")"
#endif
                            call nc_diag_cat_metadata_add_dim(trim(tmp_dim_name), tmp_dim_size)
                        end if
                    end do
                    
                    deallocate(unlim_dims)
                    
                    ! Variables
#ifdef DEBUG
                    write (*, "(A, I0)") "Number of variables: ", input_nvars
#endif
                    
                    if (cached_nvars == -1) cached_nvars = input_nvars
#ifndef QUIET
                    if (cached_nvars /= input_nvars) &
                        call ncdc_warning("Number of variables in " // trim(input_file) // " does not match first input file.")
#endif
                    
                    if (input_nvars == 0) then
                        call ncdc_check(nf90_close(ncid_input))
                        cycle
                    end if
                    
                    ! Loop through each variable!
                    do var_index = 1, input_nvars
                        ! Grab number of dimensions and attributes first
                        call ncdc_check(nf90_inquire_variable(ncid_input, var_index, name = tmp_var_name, &
                            ndims = tmp_var_ndims, xtype = tmp_var_type))
                        
                        ! Allocate temporary variable dimids storage!
                        allocate(tmp_var_dimids(tmp_var_ndims))
                        allocate(tmp_var_dim_names(tmp_var_ndims))
                        
                        ! Grab the actual dimension IDs and attributes
                        
                        call ncdc_check(nf90_inquire_variable(ncid_input, var_index, dimids = tmp_var_dimids, &
                            xtype = tmp_var_type))
                        
                        if ((tmp_var_ndims <= 2) .OR. &
                            ((tmp_var_ndims == 3) .AND. (tmp_var_type == NF90_CHAR))) then
                            
#ifdef DEBUG
                            write (*, "(A, I0, A, I0)") " => Variable #", var_index, ": " // &
                                trim(tmp_var_name)
                            write (*, "(A)", advance = "NO") "    => Dimension IDs: "
                            
                            do i = 1, tmp_var_ndims
                                if (i /= 1) write (*, "(A)", advance = "NO") ", "
                                write (*, "(I0)", advance = "NO") tmp_var_dimids(i)
                            end do
                            
                            write (*, "(A)") ""
                            
                            write (*, "(A)", advance = "NO") "    => Dimensions: "
#endif
                            
                            do i = 1, tmp_var_ndims
#ifdef DEBUG
                                if (i /= 1) write (*, "(A)", advance = "NO") ", "
#endif
                                call ncdc_check(nf90_inquire_dimension(ncid_input, tmp_var_dimids(i), tmp_var_dim_names(i)))
#ifdef DEBUG
                                write (*, "(A)", advance = "NO") trim(tmp_var_dim_names(i))
#endif
                            end do
                            
#ifdef DEBUG
                            write (*, "(A)") ""
#endif
                            
                            call nc_diag_cat_metadata_add_var(trim(tmp_var_name), tmp_var_type, tmp_var_ndims, tmp_var_dim_names)
                        else
                            write (err_string, "(A, I0, A)") &
                                "Variables with >2 dimensions NOT supported." // &
                                CHAR(10) // "             " // &
                                "(Variable '" // trim(tmp_var_name) // "' has ", &
                                tmp_var_ndims, &
                                " dimensions!)"
                            call ncdc_error(trim(err_string))
                        end if
                        ! Deallocate
                        deallocate(tmp_var_dimids)
                        deallocate(tmp_var_dim_names)
                    end do
                    
#ifdef DEBUG
                    write (*, "(A)") " => For all variables, the order of dimensions are INVERTED!"
#endif
                    
                    call ncdc_check(nf90_close(ncid_input))
                    
                    old_dim_arr_total = dim_arr_total
                    old_var_arr_total = var_arr_total
                end if
            end do
        end subroutine nc_diag_cat_metadata_pass
        
        subroutine nc_diag_cat_metadata_define
            integer(i_long) :: i, j
            
            call ncdc_info("Creating new dimensions and variables for output file...")
            
            call ncdc_info(" -> Defining dimensions...")
            
            if (dim_arr_total == 0) &
                call ncdc_warning("No dimensions found in input files, so not defining anything.")
            
            do i = 1, dim_arr_total
                if (dim_sizes(i) == -1) then
                    call ncdc_check(nf90_def_dim(ncid_output, dim_names(i), &
                        NF90_UNLIMITED, dim_output_ids(i)))
                else
                    call ncdc_check(nf90_def_dim(ncid_output, dim_names(i), &
                        dim_sizes(i), dim_output_ids(i)))
                end if
#ifdef DEBUG
                write(*, "(A, I0, A, I0)") "STORED DIMID for dim " // trim(dim_names(i)) // ": ", &
                    dim_output_ids(i), " | size: ", dim_sizes(i)
#endif
            end do
            
            if (var_arr_total == 0) &
                call ncdc_warning("No variables found in input files, so not defining anything.")
            
            call ncdc_info(" -> Defining variables...")
            do i = 1, var_arr_total
                do j = 1, var_dim_names(i)%num_names
                    var_dim_names(i)%output_dim_ids(j) = &
                        dim_output_ids(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(j)))
#ifdef DEBUG
                    write(*, "(A, I0)") "Paired ID for dim " // trim(var_dim_names(i)%dim_names(j)) // ": ", &
                        var_dim_names(i)%output_dim_ids(j)
#endif
                end do
                
#ifdef DEBUG
                write (*, "(A, I0, A)") "Defining variable: " // trim(var_names(i)) // " (type = ", var_types(i), ")"
                
                print *, "var_dim_names(i)%output_dim_ids", var_dim_names(i)%output_dim_ids
                print *, "LEN var_dim_names(i)%output_dim_ids", size(var_dim_names(i)%output_dim_ids)
#endif
                
                call ncdc_check(nf90_def_var(ncid_output, var_names(i), var_types(i), &
                    var_dim_names(i)%output_dim_ids, &
                    var_output_ids(i)))
                
#ifdef DEBUG
                if (var_dim_names(i)%num_names == 1) print *, "DIM #1", var_dim_names(i)%dim_names(1)
                if (var_dim_names(i)%num_names == 2) print *, "DIM #2", var_dim_names(i)%dim_names(2)
                if (var_dim_names(i)%num_names == 3) print *, "DIM #3", var_dim_names(i)%dim_names(3)
#endif
                
                if (var_hasunlim(i)) then
                    if (var_dim_names(i)%num_names == 1) then
                        call ncdc_check(nf90_def_var_chunking(ncid_output, var_output_ids(i), &
                            NF90_CHUNKED, (/ NC_DIAG_CAT_CHUNK_SIZE /) ))
                    else if (var_dim_names(i)%num_names == 2) then
                        call ncdc_check(nf90_def_var_chunking(ncid_output, var_output_ids(i), &
                            NF90_CHUNKED, (/ dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(1))), &
                            NC_DIAG_CAT_CHUNK_SIZE /) ))
                    else if (var_dim_names(i)%num_names == 3) then
                        call ncdc_check(nf90_def_var_chunking(ncid_output, var_output_ids(i), &
                            NF90_CHUNKED, &
                            (/ dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(1))), &
                                dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(2))), &
                                NC_DIAG_CAT_CHUNK_SIZE /) ))
                    end if
                else
                    if (var_dim_names(i)%num_names == 1) then
                        call ncdc_check(nf90_def_var_chunking(ncid_output, var_output_ids(i), &
                            NF90_CHUNKED, (/ dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(1))) /) ))
                    else if (var_dim_names(i)%num_names == 2) then
                        call ncdc_check(nf90_def_var_chunking(ncid_output, var_output_ids(i), &
                            NF90_CHUNKED, (/ dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(1))), &
                                dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(2))) /) ))
                    else if (var_dim_names(i)%num_names == 3) then
                        call ncdc_check(nf90_def_var_chunking(ncid_output, var_output_ids(i), &
                            NF90_CHUNKED, &
                            (/ dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(1))), &
                                dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(2))), &
                                dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(3))) /) ))
                    end if
                end if
                
                call ncdc_check(nf90_def_var_deflate(ncid_output, var_output_ids(i), &
                    shuffle = 1, deflate = 1, deflate_level = NC_DIAG_CAT_GZIP_COMPRESS))
            end do
        end subroutine nc_diag_cat_metadata_define
        
        subroutine nc_diag_cat_metadata_alloc
            integer(i_long), dimension(3) :: alloc_dim_sizes = 0
            integer(i_long)               :: i
            
            ! Next portion depends on defines/vars in ncdc_data_decl.F90
            call ncdc_info(" -> Allocating data storage for variables...")
            
            allocate(data_blobs(var_arr_total))
            
            do i = 1, var_arr_total
                if (var_dim_names(i)%num_names == 1) then
                    alloc_dim_sizes = (/ &
                        dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(1))), &
                        0, &
                        0 /)
                    
                    ! Check for unlimited sizes and replace them!
                    if (alloc_dim_sizes(1) == -1) &
                        alloc_dim_sizes(1) = &
                            dim_unlim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(1)))
                    
                    if (var_types(i) == NF90_BYTE)   allocate(data_blobs(i)%byte_buffer(alloc_dim_sizes(1)))
                    if (var_types(i) == NF90_SHORT)  allocate(data_blobs(i)%short_buffer(alloc_dim_sizes(1)))
                    if (var_types(i) == NF90_INT)    allocate(data_blobs(i)%long_buffer(alloc_dim_sizes(1)))
                    if (var_types(i) == NF90_FLOAT)  allocate(data_blobs(i)%rsingle_buffer(alloc_dim_sizes(1)))
                    if (var_types(i) == NF90_DOUBLE) allocate(data_blobs(i)%rdouble_buffer(alloc_dim_sizes(1)))
                    if (var_types(i) == NF90_CHAR)   call ncdc_error("1D character variable type not supported!")
                    
                    if (var_types(i) == NF90_BYTE)   data_blobs(i)%byte_buffer    = NF90_FILL_BYTE
                    if (var_types(i) == NF90_SHORT)  data_blobs(i)%short_buffer   = NF90_FILL_SHORT
                    if (var_types(i) == NF90_INT)    data_blobs(i)%long_buffer    = NF90_FILL_INT
                    if (var_types(i) == NF90_FLOAT)  data_blobs(i)%rsingle_buffer = NF90_FILL_FLOAT
                    if (var_types(i) == NF90_DOUBLE) data_blobs(i)%rdouble_buffer = NF90_FILL_DOUBLE
                else if (var_dim_names(i)%num_names == 2) then
                    alloc_dim_sizes = (/ &
                        dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(1))), &
                        dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(2))), &
                        0 /)
                    
                    ! Check for unlimited sizes and replace them!
                    if (alloc_dim_sizes(2) == -1) &
                        alloc_dim_sizes(2) = &
                            dim_unlim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(2)))
                    
                    if (var_types(i) == NF90_BYTE)   allocate(data_blobs(i)%byte_2d_buffer(alloc_dim_sizes(1), alloc_dim_sizes(2)))
                    if (var_types(i) == NF90_SHORT)  allocate(data_blobs(i)%short_2d_buffer(alloc_dim_sizes(1), alloc_dim_sizes(2)))
                    if (var_types(i) == NF90_INT)    allocate(data_blobs(i)%long_2d_buffer(alloc_dim_sizes(1), alloc_dim_sizes(2)))
                    if (var_types(i) == NF90_FLOAT)  allocate(data_blobs(i)%rsingle_2d_buffer(alloc_dim_sizes(1), alloc_dim_sizes(2)))
                    if (var_types(i) == NF90_DOUBLE) allocate(data_blobs(i)%rdouble_2d_buffer(alloc_dim_sizes(1), alloc_dim_sizes(2)))
                    if (var_types(i) == NF90_CHAR)   allocate(data_blobs(i)%string_buffer(alloc_dim_sizes(1), alloc_dim_sizes(2)))
                    
                    if (var_types(i) == NF90_BYTE)   data_blobs(i)%byte_2d_buffer    = NF90_FILL_BYTE
                    if (var_types(i) == NF90_SHORT)  data_blobs(i)%short_2d_buffer   = NF90_FILL_SHORT
                    if (var_types(i) == NF90_INT)    data_blobs(i)%long_2d_buffer    = NF90_FILL_INT
                    if (var_types(i) == NF90_FLOAT)  data_blobs(i)%rsingle_2d_buffer = NF90_FILL_FLOAT
                    if (var_types(i) == NF90_DOUBLE) data_blobs(i)%rdouble_2d_buffer = NF90_FILL_DOUBLE
                    if (var_types(i) == NF90_CHAR)   data_blobs(i)%string_buffer     = NF90_FILL_CHAR
                    
                else if (var_dim_names(i)%num_names == 3) then
                    alloc_dim_sizes = (/ &
                        dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(1))), &
                        dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(2))), &
                        dim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(3))) /)
                    
                    ! Check for unlimited sizes and replace them!
                    ! (Though, this should always be the case...)
                    if (alloc_dim_sizes(3) == -1) &
                        alloc_dim_sizes(3) = &
                            dim_unlim_sizes(nc_diag_cat_lookup_dim(var_dim_names(i)%dim_names(3)))
                    
                    if (var_types(i) == NF90_CHAR) then
                        allocate(data_blobs(i)%string_2d_buffer(alloc_dim_sizes(1), alloc_dim_sizes(2), alloc_dim_sizes(3)))
                        data_blobs(i)%string_2d_buffer = NF90_FILL_CHAR
                    else
                        call ncdc_error("3D non-character variable type not supported!")
                    end if
                end if
                
                data_blobs(i)%alloc_size = alloc_dim_sizes
                !print *, trim(var_names(i)), data_blobs(i)%alloc_size
            end do
            
#ifdef DEBUG
            print *, "!! END DEFINITION PASS"
#endif
        end subroutine nc_diag_cat_metadata_alloc
end module ncdc_metadata
