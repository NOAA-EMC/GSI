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
! data2d module - ncdw_data2d
!
module ncdw_data2d
    ! Module that provides chaninfo variable storage support.
    ! 
    ! This module has all of the subroutines needed to store chaninfo
    ! data. It includes the chaninfo storing subroutine
    ! (nc_diag_chaninfo), subroutines for controlling chaninfo data
    ! (dimension setting, loading definitions, saving definitions,
    ! saving data, etc.), and preallocation subroutines.
    ! 
    ! Background:
    !   chaninfo is a fixed storage variable, with dimensions of
    !   1 x nchans, where nchans is a known integer.
    !   
    !   Because we can know nchans, we can constrain the dimensions and
    !   make a few assumptions:
    !   
    !   -> nchans won't change for the duration of the file being open;
    !   -> nchans will be the same for all chaninfo variables, for any
    !      type involved;
    !   -> because everything is fixed, we can store variables block
    !      by block!
    !  
    !  Because Fortran is a strongly typed language, we can't do silly
    !  tricks in C, like allocating some memory to a void pointer and
    !  just storing our byte, short, int, long, float, or double numeric
    !  data there, and later casting it back...
    !  
    !  (e.g. void **data_ref; data_ref = malloc(sizeof(void *) * 1000);
    !        float *f = malloc(sizeof(float)); *f = 1.2345;
    !        data_ref[0] = f; ...)
    !  
    !  No frets - we can work around this issue with some derived types
    !  and arrays! We create an array for each type we want to support.
    !  Since we're using kinds.F90, we support the following types:
    !    i_byte, i_short, i_long, r_single, r_double
    !  
    !  The derived type used, diag_chaninfo, has these variables to help
    !  us keep track of everything:
    !  
    !  -> ci_* - these arrays have the types listed above, plus string
    !     support! These arrays are simply arrays that we throw our data
    !     in. However, don't mistaken "throw in" with "disorganized" -
    !     chaninfo uses a very structured format for these variables!
    !     Keep reading to find out how we structure it...
    !     
    !  -> nchans - the number of channels to use. Remember that chaninfo
    !     variables have dimensions 1 x nchans - basically, we need to
    !     store nchans values. We'll need this a LOT to do consistency
    !     checks, and to keep track of everything!
    !     
    !  -> names - all of the chaninfo variable names! We'll be using
    !     this array to store and lookup chaninfo variables, as well as
    !     storing them!
    !     
    !  -> types - all of the chaninfo variable types! These are byte
    !     integers that get compared to our NLAYER_* type constants
    !     (see: ncdw_types.F90).
    !     
    !  -> var_usage - the amount of entries we've stored in our chaninfo
    !     variable! For instance, if we called
    !     nc_diag_chaninfo("myvar", 1) three times, for that particular
    !     var_usage(i), we would have an entry of 3.
    !     
    !  -> var_rel_pos - the star of the show! This is an abbreviation
    !     of "variable relative positioning". Recall that we store
    !     our variable data in ci_* specific type arrays. We know
    !     the nchans amount, and we know the type. This variable stores
    !     the "block" that our data is in within the type array.
    !     
    !     Therefore, we can use the equation to find our starting
    !     position: 1 + [(REL_VAL - 1) * nchans]
    !     
    !     For instance, if var_rel_pos(1) for variable names(1) = "bla"
    !     is set to 2, nchans is 10, and the type is NLAYER_FLOAT, we
    !     can deduce that in ci_rsingle, our data can be found starting
    !     at 1 + (1 * 10) = 11. This makes sense, as seen with our mini
    !     diagram below:
    !     
    !     ci_rsingle:
    !       /                    ci_rsingle index                   \
    !      1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20
    !     [ x, x, x, x, x, x, x, x, x, x, y, y, y, y, y, y, y, y, y, y ]
    !       \                    ci_rsingle array                   /
    !     
    !     Indeed, our second block does start at index 11!
    !     As a bonus, since our data is in blocks, things can be super
    !     fast since we're just cutting our big array into small ones!
    !     
    !  -> acount_v: Finally, we have dynamic allocation. We have in our
    !     type a variable called acount_v. This tells us how many
    !     variables are stored in each type. Using the same equation
    !     above, and combining with var_usage, we can figure out where
    !     to put our data!
    !     
    !     Assume var_usage(i) = 2, block starts at index 11 with the
    !     equation above.
    !     
    !     Again, with our fun little diagram:
    !     
    !     ci_rsingle:
    !       /                    ci_rsingle index                   \
    !      1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20
    !     [ x, x, x, x, x, x, x, x, x, x, y, y, Y, y, y, y, y, y, y, y ]
    !      [ BLOCK 1 SEEK = 1->10->11  ][var_u=2|---block 2 area 11->20]
    !       \                    ci_rsingle array                   /
    !     
    !     The capital Y marks the place we store our data!
    !   
    !   For the non-data variables (e.g. variable names, types, etc.),
    !   they are indexed by variable insertion order. This allows for
    !   easy lookup by looking up the variable name, and using the
    !   resulting index for fetching other information.
    !   
    !   Example:
    !     names:       [ 'asdf', 'ghjk', 'zxcv' ]
    !     types:       [   BYTE,  FLOAT,   BYTE ]
    !     var_rel_pos: [      1,      1,      2 ]
    !     
    !     Lookup: "ghjk", result index = 2
    !     
    !     Therefore, the "ghjk" variable type is types(2) = FLOAT, and
    !     the var_rel_pos for "ghjk" variable is var_rel_pos(2) = 1.
    !   
    !   These variables are allocated and reallocated, as needed.
    !   
    !   For the variable metadata fields (variable names, types,
    !   relative indicies, etc.), these are reallocated incrementally
    !   when a new variable is added.
    !   
    !   For the data storage fields, these are reallocated incrementally
    !   when new data is added.
    !   
    !   Initial allocation and subsequent reallocation is done by
    !   chunks. Allocating one element and/or reallocating and adding
    !   just one element is inefficient, since it's likely that much
    !   more data (and variables) will be added. Thus, allocation and
    !   reallocation is done by (re-)allocating exponentially increasing
    !   chunk sizes. See nc_diag_chaninfo_allocmulti help for more
    !   details.
    !   
    !   Because (re-)allocation is done in chunks, we keep a count of
    !   how much of the memory we're using so that we know when it's
    !   time to (re-)allocate. Once we need to (re-)allocate, we
    !   perform it, and then update our total memory counter to keep
    !   track of the memory already allocated.
    !   
    !   With all of these variables (and a few more state variables),
    !   we can reliably store our chaninfo data quickly and
    !   efficiently!
    ! 
    
    use ncd_kinds, only: i_byte, i_short, i_long, i_llong, r_single, &
        r_double
    use ncdw_state, only: init_done, append_only, ncid, &
        enable_trim, &
        diag_data2d_store, diag_varattr_store
    use ncdw_types, only: NLAYER_BYTE, NLAYER_SHORT, NLAYER_LONG, &
        NLAYER_FLOAT, NLAYER_DOUBLE, NLAYER_STRING, NLAYER_CHUNKING, &
        NLAYER_COMPRESSION, NLAYER_FILL_BYTE, NLAYER_FILL_SHORT, &
        NLAYER_FILL_LONG, NLAYER_FILL_FLOAT, NLAYER_FILL_DOUBLE, &
        NLAYER_FILL_CHAR, &
        NLAYER_DEFAULT_ENT, NLAYER_MULTI_BASE
    use ncdw_strarrutils, only: &
#ifdef _DEBUG_MEM_
        string_array_dump, &
#endif
        max_len_string_array, max_len_notrim_string_array
    use ncdw_varattr, only: nc_diag_varattr_make_nobs_dim, &
        nc_diag_varattr_add_var
    
    use ncdw_dresize, only: nc_diag_data2d_resize_byte, &
        nc_diag_data2d_resize_short, nc_diag_data2d_resize_long, &
        nc_diag_data2d_resize_rsingle, nc_diag_data2d_resize_rdouble, &
        nc_diag_data2d_resize_string, nc_diag_data2d_resize_iarr_type, &
        nc_diag_data2d_resize_iarr
    use ncdw_realloc, only: nc_diag_realloc
    
    use netcdf, only: nf90_inquire, nf90_inquire_variable, &
        nf90_inquire_dimension, nf90_def_dim, nf90_def_var, &
        nf90_def_var_deflate, nf90_def_var_chunking, nf90_put_var, &
        NF90_BYTE, NF90_SHORT, NF90_INT, NF90_FLOAT, NF90_DOUBLE, &
        NF90_CHAR, NF90_MAX_NAME, NF90_CHUNKED
    
    use ncdw_climsg, only: &
#ifdef ENABLE_ACTION_MSGS
        nclayer_enable_action, nclayer_actionm, &
#endif
#ifdef _DEBUG_MEM_
        nclayer_debug, &
#endif
        nclayer_error, nclayer_warning, nclayer_info, nclayer_check
    
    implicit none
    
    interface nc_diag_data2d
        module procedure nc_diag_data2d_byte, &
            nc_diag_data2d_short, nc_diag_data2d_long, &
            nc_diag_data2d_rsingle, nc_diag_data2d_rdouble, &
            nc_diag_data2d_string
    end interface nc_diag_data2d
    
    contains
        subroutine nc_diag_data2d_allocmulti(multiplier)
            integer(i_long), intent(in)    :: multiplier
            if (init_done) then
                ! # of times we needed to realloc simple data2d
                ! also the multiplier factor for allocation (2^x)
                diag_data2d_store%alloc_s_multi = multiplier
                
                ! # of times we needed to realloc data2d data storage
                ! also the multiplier factor for allocation (2^x)
                diag_data2d_store%alloc_m_multi = multiplier
                
                ! # of times we needed to realloc data2d INDEX data storage
                ! also the multiplier factor for allocation (2^x)
                diag_data2d_store%alloc_mi_multi = multiplier
            end if
        end subroutine nc_diag_data2d_allocmulti
        
        function nc_diag_data2d_max_len_var(var_index) result(max_len)
            integer(i_llong), intent(in)    :: var_index
            
            integer :: i, max_len
            
            character(len=1000)                        :: data_uneven_msg
            
            max_len = -1
            
            do i = 1, diag_data2d_store%stor_i_arr(var_index)%icount
                ! Only show a message if strict checking is enabled.
                ! Otherwise, show the message later in data writing.
                if (diag_data2d_store%strict_check .AND. &
                    (diag_data2d_store%stor_i_arr(var_index)%length_arr(i) /= max_len) .AND. &
                    (max_len /= -1)) then
                    ! Show message!
                    ! NOTE - I0 and TRIM are Fortran 95 specs
                    write (data_uneven_msg, "(A, I0, A, I0, A)") "Amount of data written in " // &
                        trim(diag_data2d_store%names(var_index)) // " (", &
                        diag_data2d_store%stor_i_arr(var_index)%length_arr(i), &
                        ")" // char(10) // &
                        "             does not match the variable length" // &
                        " (", max_len, ")!"
                    
                    ! Probably not needed, since this only triggers on a
                    ! strict check... but just in case...
                    if (diag_data2d_store%strict_check) then
                        call nclayer_error(trim(data_uneven_msg))
                    else
                        call nclayer_warning(trim(data_uneven_msg))
                    end if
                end if
                
                if (diag_data2d_store%stor_i_arr(var_index)%length_arr(i) > max_len) &
                    max_len = diag_data2d_store%stor_i_arr(var_index)%length_arr(i)
            end do
        end function nc_diag_data2d_max_len_var
        
        subroutine nc_diag_data2d_load_def
            integer(i_long) :: ndims, nvars, var_index, type_index
            integer(i_long) :: rel_index, i, nobs_size
            
            character(len=NF90_MAX_NAME)               :: tmp_var_name
            integer(i_long)                            :: tmp_var_type, tmp_var_ndims
            
            integer(i_long), dimension(:), allocatable :: tmp_var_dimids, tmp_var_dim_sizes
            character(len=NF90_MAX_NAME) , allocatable :: tmp_var_dim_names(:)
            
            logical                                    :: is_data2d_var
            
            ! Get top level info about the file!
            call nclayer_check(nf90_inquire(ncid, nDimensions = ndims, &
                nVariables = nvars))
            
            ! Now search for variables that use data2d storage!
            ! Loop through each variable!
            do var_index = 1, nvars
                ! Grab number of dimensions and attributes first
                call nclayer_check(nf90_inquire_variable(ncid, var_index, name = tmp_var_name, ndims = tmp_var_ndims))
                
                ! Allocate temporary variable dimids storage!
                allocate(tmp_var_dimids(tmp_var_ndims))
                allocate(tmp_var_dim_names(tmp_var_ndims))
                allocate(tmp_var_dim_sizes(tmp_var_ndims))
                
                ! Grab the actual dimension IDs and attributes
                
                call nclayer_check(nf90_inquire_variable(ncid, var_index, dimids = tmp_var_dimids, &
                    xtype = tmp_var_type))
                
                if ((tmp_var_ndims == 2) .OR. &
                    ((tmp_var_ndims == 3) .AND. (tmp_var_type == NF90_CHAR))) then
                    is_data2d_var = .FALSE.
                    
                    do i = 1, tmp_var_ndims
                        call nclayer_check(nf90_inquire_dimension(ncid, tmp_var_dimids(i), tmp_var_dim_names(i), &
                            tmp_var_dim_sizes(i)))
                        
                        if (tmp_var_dim_names(i) == "nobs") then
                            nobs_size = tmp_var_dim_sizes(i)
                            if (tmp_var_type /= NF90_CHAR) then
                                is_data2d_var = .TRUE.
                            else if (tmp_var_type == NF90_CHAR) then
                                if (index(tmp_var_dim_names(1), "_str_dim") /= 0) &
                                    is_data2d_var = .TRUE.
                            end if
                        end if
                    end do
                    
                    if (is_data2d_var) then
                        ! Expand things first!
                        call nc_diag_data2d_expand
                        
                        ! Add to the total!
                        diag_data2d_store%total = diag_data2d_store%total + 1
                        
                        ! Store name and type!
                        diag_data2d_store%names(diag_data2d_store%total) = trim(tmp_var_name)
                        
                        ! The relative index is the total nobs
                        rel_index = nobs_size
                        
                        if (tmp_var_type == NF90_BYTE) then
                            diag_data2d_store%types(diag_data2d_store%total) = NLAYER_BYTE
                            type_index = 1
                        else if (tmp_var_type == NF90_SHORT) then
                            diag_data2d_store%types(diag_data2d_store%total) = NLAYER_SHORT
                            type_index = 2
                        else if (tmp_var_type == NF90_INT) then
                            diag_data2d_store%types(diag_data2d_store%total) = NLAYER_LONG
                            type_index = 3
                        else if (tmp_var_type == NF90_FLOAT) then
                            diag_data2d_store%types(diag_data2d_store%total) = NLAYER_FLOAT
                            type_index = 4
                        else if (tmp_var_type == NF90_DOUBLE) then
                            diag_data2d_store%types(diag_data2d_store%total) = NLAYER_DOUBLE
                            type_index = 5
                        else if (tmp_var_type == NF90_CHAR) then
                            diag_data2d_store%max_str_lens(diag_data2d_store%total) = tmp_var_dim_sizes(1)
                            diag_data2d_store%types(diag_data2d_store%total) = NLAYER_STRING
                            type_index = 6
                        else
                            call nclayer_error("NetCDF4 type invalid!")
                        end if
                        
                        if (tmp_var_type == NF90_CHAR) then
                            diag_data2d_store%max_lens(diag_data2d_store%total) = tmp_var_dim_sizes(2)
                        else
                            diag_data2d_store%max_lens(diag_data2d_store%total) = tmp_var_dim_sizes(1)
                        end if
                        
!                       print *, trim(tmp_var_name), "rel index", rel_index
                        
                        ! Now add a relative position... based on the next position!
                        
                        ! Set relative index!
                        diag_data2d_store%rel_indexes(diag_data2d_store%total) = rel_index
                        
                        ! Set variable ID! Note that var_index here is the actual variable ID.
                        diag_data2d_store%var_ids(diag_data2d_store%total) = var_index
                    end if
                end if
                
                ! Deallocate
                deallocate(tmp_var_dimids)
                deallocate(tmp_var_dim_names)
                deallocate(tmp_var_dim_sizes)
            end do
            
            diag_data2d_store%def_lock = .TRUE.
        end subroutine nc_diag_data2d_load_def
        
        subroutine nc_diag_data2d_write_def(internal)
            logical, intent(in), optional         :: internal
            
            integer(i_byte)                       :: data_type
            character(len=100)                    :: data2d_name
            
            integer(i_llong)                      :: curdatindex, j
            integer(i_long)                       :: nc_data_type
            integer(i_long)                       :: tmp_dim_id
            character(len=120)                    :: data_dim_name
            character(len=120)                    :: data_dim_str_name
            integer(i_long)                       :: max_len
            integer(i_long)                       :: max_str_len, msl_tmp
            
            character(len=:),         allocatable :: string_arr(:)
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                if (present(internal)) then
                    write(action_str, "(A, L, A)") "nc_diag_data2d_write_def(internal = ", internal, ")"
                else
                    write(action_str, "(A)") "nc_diag_data2d_write_def(internal = (not specified))"
                end if
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            if (init_done) then
                if (.NOT. diag_data2d_store%def_lock) then
                    ! Use global nobs ID!
                    ! Call subroutine to ensure the nobs dim is created already...
                    call nc_diag_varattr_make_nobs_dim
                    
                    do curdatindex = 1, diag_data2d_store%total
                        data2d_name = diag_data2d_store%names(curdatindex)
                        data_type = diag_data2d_store%types(curdatindex)
                        
                        call nclayer_info("data2d: defining " // trim(data2d_name))
                        
                        if (data_type == NLAYER_BYTE)   nc_data_type = NF90_BYTE
                        if (data_type == NLAYER_SHORT)  nc_data_type = NF90_SHORT
                        if (data_type == NLAYER_LONG)   nc_data_type = NF90_INT
                        if (data_type == NLAYER_FLOAT)  nc_data_type = NF90_FLOAT
                        if (data_type == NLAYER_DOUBLE) nc_data_type = NF90_DOUBLE
                        if (data_type == NLAYER_STRING) nc_data_type = NF90_CHAR
                        
#ifdef _DEBUG_MEM_
                        print *, "data2d part 1"
#endif
                        
                        ! We need to create a new dimension...
                        write (data_dim_name, "(A, A)") trim(data2d_name), "_arr_dim"
                        
                        ! Find the maximum array length of this variable!
                        max_len = nc_diag_data2d_max_len_var(curdatindex)
                        
                        ! Create this maximum array length dimension for this variable
                        if (.NOT. append_only) &
                            call nclayer_check(nf90_def_dim(ncid, data_dim_name, max_len, diag_data2d_store%var_dim_ids(curdatindex)))
                        
                        ! Store maximum length
                        diag_data2d_store%max_lens(curdatindex) = max_len;
                        
                        if (data_type == NLAYER_STRING) then
                            max_str_len = 0
                            write (data_dim_name, "(A, A)") trim(data2d_name), "_maxstrlen"
                            
                            ! If trimming is enabled, we haven't found our max_str_len yet.
                            ! Go find it!
                            if (enable_trim) then
                                ! Dimension is # of chars by # of obs (unlimited)
                                do j = 1, diag_data2d_store%stor_i_arr(curdatindex)%icount
                                    allocate(character(10000) :: string_arr(diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j)))
                                    string_arr = &
                                        diag_data2d_store%m_string(diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) &
                                            : diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) + &
                                                diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j))
                                    
#ifdef _DEBUG_MEM_
                                    write(*, "(A, I0)") "DEBUG DATA2D: tmp array size is: ", diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j)
#endif
                                    
                                    ! Now we can calculate the length!
                                    msl_tmp = max_len_string_array(string_arr, &
                                        diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j))
                                    
                                    if (msl_tmp > max_str_len) max_str_len = msl_tmp
                                    
#ifdef _DEBUG_MEM_
                                    write (*, "(A, A, A, I0, A, I0)") "DEBUG DATA2D DEF WRITE: at data2d_name ", trim(data2d_name), ", msl_tmp computes to ", msl_tmp, ", max_str_len computes to ", max_str_len
                                    print *, "DEBUG DATA2D DEF WRITE: string array dump follows:"
                                    call string_array_dump(string_arr)
#endif
                                    
                                    ! Deallocate right after we're done!
                                    deallocate(string_arr)
                                end do
#ifdef _DEBUG_MEM_
                                write (*, "(A, A, A, I0, A, I0)") "DEBUG DATA2D DEF WRITE: ** at data2d_name ", trim(data2d_name), ", FINAL max_str_len computes to ", max_str_len, ", max_len computes to ", max_len
#endif
                                
                                ! Save the max string len
                                diag_data2d_store%max_str_lens(curdatindex) = max_str_len
                            end if
                            
                            ! Create dimension needed!
                            write (data_dim_str_name, "(A, A)") trim(data2d_name), "_str_dim"
                            if (.NOT. append_only) &
                                call nclayer_check(nf90_def_dim(ncid, data_dim_str_name, &
                                    diag_data2d_store%max_str_lens(curdatindex), tmp_dim_id))
                            
#ifdef _DEBUG_MEM_
                            print *, "Defining char var type..."
#endif
                            
                            if (.NOT. append_only) &
                                call nclayer_check(nf90_def_var(ncid, data2d_name, nc_data_type, &
                                    (/ tmp_dim_id, diag_data2d_store%var_dim_ids(curdatindex), &
                                    diag_varattr_store%nobs_dim_id /), &
                                    diag_data2d_store%var_ids(curdatindex)))
                            
#ifdef _DEBUG_MEM_
                            write (*, "(A, A, A, I0, A, I0)") "DEBUG DATA2D DEF WRITE: ** at data2d_name ", trim(data2d_name), ", result VID is ", diag_data2d_store%var_ids(curdatindex)
                            write (*, "(A, I0, A, I0)") "DEBUG DATA2D DEF WRITE: ** result dim is unlim x max_len = ", max_len, " x max_str_len = ", diag_data2d_store%max_str_lens(curdatindex)
                            print *, "data2d part 2"
#endif
                            
#ifdef _DEBUG_MEM_
                            print *, "Done defining char var type..."
#endif
                        else
#ifdef _DEBUG_MEM_
                            print *, "Definition for variable " // trim(data2d_name) // ":"
                            print *, diag_data2d_store%max_lens(curdatindex), "x unlimited (NetCDF order)"
#endif
                            if (.NOT. append_only) &
                                call nclayer_check(nf90_def_var(ncid, data2d_name, nc_data_type, &
                                    (/ diag_data2d_store%var_dim_ids(curdatindex), diag_varattr_store%nobs_dim_id /), &
                                    diag_data2d_store%var_ids(curdatindex)))
                        end if
                        
                        call nc_diag_varattr_add_var(diag_data2d_store%names(curdatindex), &
                                    diag_data2d_store%types(curdatindex), &
                                    diag_data2d_store%var_ids(curdatindex))
                        
                        ! Enable compression
                        ! Args: ncid, varid, enable_shuffle (yes), enable_deflate (yes), deflate_level
#ifdef _DEBUG_MEM_
                        print *, "Defining compression 1 (chunking)..."
#endif
                        
                        if (.NOT. append_only) then
                            if (data_type == NLAYER_STRING) then
                                call nclayer_check(nf90_def_var_chunking(ncid, diag_data2d_store%var_ids(curdatindex), &
                                    NF90_CHUNKED, (/ diag_data2d_store%max_str_lens(curdatindex), &
                                        diag_data2d_store%max_lens(curdatindex), NLAYER_CHUNKING /)))
                            else
                                call nclayer_check(nf90_def_var_chunking(ncid, diag_data2d_store%var_ids(curdatindex), &
                                    NF90_CHUNKED, (/ diag_data2d_store%max_lens(curdatindex), NLAYER_CHUNKING /)))
                            end if
                        end if
#ifdef _DEBUG_MEM_
                        print *, "Defining compression 2 (gzip)..."
#endif
                        if (.NOT. append_only) &
                            call nclayer_check(nf90_def_var_deflate(ncid, diag_data2d_store%var_ids(curdatindex), &
                                1, 1, int(NLAYER_COMPRESSION)))
                        
#ifdef _DEBUG_MEM_
                        print *, "Done defining compression..."
#endif
                        
                        ! Lock the definitions!
                        diag_data2d_store%def_lock = .TRUE.
                    end do
                else
                    if(.NOT. present(internal)) &
                        call nclayer_error("Can't write definitions - definitions have already been written and locked!")
                end if
            end if
        end subroutine nc_diag_data2d_write_def
        
        subroutine nc_diag_data2d_write_data(flush_data_only)
            ! Optional internal flag to only flush data - if this is
            ! true, data flushing will be performed, and the data will
            ! NOT be locked.
            logical, intent(in), optional         :: flush_data_only
            
            integer(i_byte)                       :: data_type
            character(len=100)                    :: data2d_name
            
            ! For some strange reason, curdatindex needs to be
            ! initialized here to 1, otherwise a runtime error of using
            ! an undefined variable occurs... even though it's set
            ! by the DO loop...
            integer(i_long)                       :: curdatindex = 1, j
#ifdef _DEBUG_MEM_
            ! Index counter for inner loop (intermediate) array debug
            integer(i_long)                       :: i
#endif
            
            integer(i_byte), dimension(:, :), allocatable :: byte_arr
            integer(i_short),dimension(:, :), allocatable :: short_arr
            integer(i_long), dimension(:, :), allocatable :: long_arr
            real(r_single),  dimension(:, :), allocatable :: rsingle_arr
            real(r_double),  dimension(:, :), allocatable :: rdouble_arr
            character(len=:),dimension(:, :), allocatable :: string_arr
            
            integer(i_long)                               :: max_str_len
            
            integer(i_llong)                              :: data_length_counter
            character(len=100)                            :: counter_data_name
            integer(i_llong)                              :: current_length_count
            character(len=1000)                           :: data_uneven_msg
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                if (present(flush_data_only)) then
                    write(action_str, "(A, L, A)") "nc_diag_data2d_write_data(flush_data_only = ", flush_data_only, ")"
                else
                    write(action_str, "(A)") "nc_diag_data2d_write_data(flush_data_only = (not specified))"
                end if
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            ! Initialization MUST occur here, not in decl...
            ! Otherwise, it'll initialize once, and never again...
            ! 
            ! This will cause scary issues in the future, where closing
            ! and opening a new file shows strange errors about a file
            ! opened in the past...
            max_str_len = -1
            data_length_counter = -1
            current_length_count = -1
            
            if (init_done .AND. allocated(diag_data2d_store)) then
                if (.NOT. diag_data2d_store%data_lock) then
                    do curdatindex = 1, diag_data2d_store%total
#ifdef _DEBUG_MEM_
                        print *, curdatindex
#endif
                        data2d_name = diag_data2d_store%names(curdatindex)
                        data_type = diag_data2d_store%types(curdatindex)
                        
                        call nclayer_info("data2d: writing " // trim(data2d_name))
                        
                        ! Warn about data inconsistencies
                        if (.NOT. (present(flush_data_only) .AND. flush_data_only)) then
                            current_length_count = diag_data2d_store%stor_i_arr(curdatindex)%icount + &
                                diag_data2d_store%rel_indexes(curdatindex)
                            
                            if (data_length_counter == -1) then
                                data_length_counter = current_length_count
                                counter_data_name = data2d_name
                            else
                                if (data_length_counter /= current_length_count) then
                                    ! Show message!
                                    ! NOTE - I0 and TRIM are Fortran 95 specs
                                    write (data_uneven_msg, "(A, I0, A, I0, A)") "Amount of data written in " // &
                                        trim(data2d_name) // " (", &
                                        current_length_count, &
                                        ")" // char(10) // &
                                        "             differs from variable " // trim(counter_data_name) // &
                                        " (", data_length_counter, ")!"
                                    
                                    if (diag_data2d_store%strict_check) then
                                        call nclayer_error(trim(data_uneven_msg))
                                    else
                                        call nclayer_warning(trim(data_uneven_msg))
                                    end if
                                end if
                            end if
                        end if
                        
                        ! Make sure we have data to write in the first place!
                        if (diag_data2d_store%stor_i_arr(curdatindex)%icount > 0) then
                            ! MAJOR GOTCHA:
                            ! Fortran is weird... and by weird, we mean Fortran's indexing
                            ! system! Fortran uses a column-major system, which means that
                            ! we MUST allocate and store in a column-major format! Each
                            ! column needs to store a single array of data. Before, with
                            ! single dimensions, this didn't matter since the data itself
                            ! was automatically stored into a column. With 2D data,
                            ! we MUST be aware of the reversed dimensions!
                            ! (NetCDF4 respects the Fortran way, and takes in a "row" of
                            ! data via columns!)
                            
                            if (data_type == NLAYER_BYTE) then
                                allocate(byte_arr(diag_data2d_store%max_lens(curdatindex), &
                                    diag_data2d_store%stor_i_arr(curdatindex)%icount))
                                
                                byte_arr = NLAYER_FILL_BYTE
                                
                                do j = 1, diag_data2d_store%stor_i_arr(curdatindex)%icount
                                    ! Just in case our definition checks failed...
                                    if (diag_data2d_store%max_lens(curdatindex) /= &
                                        diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j)) then
                                        ! Show message!
                                        ! NOTE - I0 and TRIM are Fortran 95 specs
                                        write (data_uneven_msg, "(A, I0, A, I0, A)") "Amount of data written in " // &
                                            trim(data2d_name) // " (", &
                                            diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j), &
                                            ")" // char(10) // &
                                            "             does not match the variable length" // &
                                            " (", diag_data2d_store%max_lens(curdatindex), ")!"
                                        
                                        if (diag_data2d_store%strict_check) then
                                            call nclayer_error(trim(data_uneven_msg))
                                        else
                                            call nclayer_warning(trim(data_uneven_msg))
                                        end if
                                    end if
                                    
                                    byte_arr(1 : diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j), j) = &
                                        diag_data2d_store%m_byte( &
                                            diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) : &
                                            diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) + &
                                                diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j) - 1)
                                end do
                                
                                call nclayer_check(nf90_put_var(&
                                    ncid, diag_data2d_store%var_ids(curdatindex), &
                                    byte_arr, &
                                    (/ 1, 1 + diag_data2d_store%rel_indexes(curdatindex) /), &
                                    (/ diag_data2d_store%max_lens(curdatindex), &
                                        diag_data2d_store%stor_i_arr(curdatindex)%icount /) &
                                    ))
                                
                                deallocate(byte_arr)
                            else if (data_type == NLAYER_SHORT) then
                                allocate(short_arr(diag_data2d_store%max_lens(curdatindex), &
                                    diag_data2d_store%stor_i_arr(curdatindex)%icount))
                                
                                short_arr = NLAYER_FILL_SHORT
                                
                                do j = 1, diag_data2d_store%stor_i_arr(curdatindex)%icount
                                    ! Just in case our definition checks failed...
                                    if (diag_data2d_store%max_lens(curdatindex) /= &
                                        diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j)) then
                                        ! Show message!
                                        ! NOTE - I0 and TRIM are Fortran 95 specs
                                        write (data_uneven_msg, "(A, I0, A, I0, A)") "Amount of data written in " // &
                                            trim(data2d_name) // " (", &
                                            diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j), &
                                            ")" // char(10) // &
                                            "             does not match the variable length" // &
                                            " (", diag_data2d_store%max_lens(curdatindex), ")!"
                                        
                                        if (diag_data2d_store%strict_check) then
                                            call nclayer_error(trim(data_uneven_msg))
                                        else
                                            call nclayer_warning(trim(data_uneven_msg))
                                        end if
                                    end if
                                    
                                    short_arr(1 : diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j), j) = &
                                        diag_data2d_store%m_short( &
                                            diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) : &
                                            diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) + &
                                                diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j) - 1)
                                end do
                                
                                call nclayer_check(nf90_put_var(&
                                    ncid, diag_data2d_store%var_ids(curdatindex), &
                                    short_arr, &
                                    (/ 1, 1 + diag_data2d_store%rel_indexes(curdatindex) /), &
                                    (/ diag_data2d_store%max_lens(curdatindex), &
                                        diag_data2d_store%stor_i_arr(curdatindex)%icount /) &
                                    ))
                                
                                deallocate(short_arr)
                            else if (data_type == NLAYER_LONG) then
                                !allocate(long_arr(diag_data2d_store%stor_i_arr(curdatindex)%icount, &
                                !    diag_data2d_store%max_lens(curdatindex)))
                                
                                allocate(long_arr(diag_data2d_store%max_lens(curdatindex), &
                                    diag_data2d_store%stor_i_arr(curdatindex)%icount))
                                
#ifdef _DEBUG_MEM_
                                write (*, "(A, I0)") "NLAYER_FILL_LONG = ", NLAYER_FILL_LONG
#endif
                                
                                long_arr = NLAYER_FILL_LONG
                                
#ifdef _DEBUG_MEM_
                                write (*, "(A)") "************ DEBUG: INITIAL var array for " // trim(data2d_name)
                                do j = 1, diag_data2d_store%stor_i_arr(curdatindex)%icount
                                    print *, long_arr(:, j)
                                end do
#endif
                                
                                do j = 1, diag_data2d_store%stor_i_arr(curdatindex)%icount
                                    ! Just in case our definition checks failed...
                                    if (diag_data2d_store%max_lens(curdatindex) /= &
                                        diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j)) then
                                        ! Show message!
                                        ! NOTE - I0 and TRIM are Fortran 95 specs
                                        write (data_uneven_msg, "(A, I0, A, I0, A)") "Amount of data written in " // &
                                            trim(data2d_name) // " (", &
                                            diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j), &
                                            ")" // char(10) // &
                                            "             does not match the variable length" // &
                                            " (", diag_data2d_store%max_lens(curdatindex), ")!"
                                        
                                        if (diag_data2d_store%strict_check) then
                                            call nclayer_error(trim(data_uneven_msg))
                                        else
                                            call nclayer_warning(trim(data_uneven_msg))
                                        end if
                                    end if
                                    
#ifdef _DEBUG_MEM_
                                    write (*, "(A, I0, A)") "Adding to long_arr, index ", j, ":"
                                    print *, diag_data2d_store%m_long( &
                                        diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) : &
                                        diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) + &
                                            diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j) - 1)
                                    write (*, "(A, I0)") " -> length of subarr: ", diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j)
#endif
                                    
                                    long_arr(1 : diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j), j) = &
                                        diag_data2d_store%m_long( &
                                            diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) : &
                                            diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) + &
                                                diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j) - 1)
                                    
#ifdef _DEBUG_MEM_
                                    write (*, "(A)") "************ DEBUG: INTERMEDIATE var array for " // trim(data2d_name)
                                    do i = 1, diag_data2d_store%stor_i_arr(curdatindex)%icount
                                        print *, long_arr(:, i)
                                    end do
#endif
                                end do
                                
#ifdef _DEBUG_MEM_
                                write (*, "(A, I0, A, I0, A, I0, A, I0, A)") &
                                    "Writing long with start = (", 1, ", ", &
                                    1 + diag_data2d_store%rel_indexes(curdatindex), &
                                    "), count = (", diag_data2d_store%stor_i_arr(curdatindex)%icount, &
                                    ", ", 1, ")"
                                
                                write (*, "(A, I0, A, I0)") "************ DEBUG: dim for " // trim(data2d_name) // ": ", &
                                    diag_data2d_store%stor_i_arr(curdatindex)%icount, " by ", &
                                    diag_data2d_store%max_lens(curdatindex)
                                write (*, "(A)") "************ DEBUG: var array for " // trim(data2d_name)
                                
                                do j = 1, diag_data2d_store%stor_i_arr(curdatindex)%icount
                                    print *, long_arr(:, j)
                                end do
#endif
                                
                                call nclayer_check(nf90_put_var(&
                                    ncid, diag_data2d_store%var_ids(curdatindex), &
                                    long_arr, &
                                    (/ 1, 1 + diag_data2d_store%rel_indexes(curdatindex) /), &
                                    (/ diag_data2d_store%max_lens(curdatindex), &
                                        diag_data2d_store%stor_i_arr(curdatindex)%icount /) &
                                    ))
                                
                                deallocate(long_arr)
                            else if (data_type == NLAYER_FLOAT) then
                                allocate(rsingle_arr(diag_data2d_store%max_lens(curdatindex), &
                                    diag_data2d_store%stor_i_arr(curdatindex)%icount))
                                
                                rsingle_arr = NLAYER_FILL_FLOAT
                                
                                do j = 1, diag_data2d_store%stor_i_arr(curdatindex)%icount
                                    ! Just in case our definition checks failed...
                                    if (diag_data2d_store%max_lens(curdatindex) /= &
                                        diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j)) then
                                        ! Show message!
                                        ! NOTE - I0 and TRIM are Fortran 95 specs
                                        write (data_uneven_msg, "(A, I0, A, I0, A)") "Amount of data written in " // &
                                            trim(data2d_name) // " (", &
                                            diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j), &
                                            ")" // char(10) // &
                                            "             does not match the variable length" // &
                                            " (", diag_data2d_store%max_lens(curdatindex), ")!"
                                        
                                        if (diag_data2d_store%strict_check) then
                                            call nclayer_error(trim(data_uneven_msg))
                                        else
                                            call nclayer_warning(trim(data_uneven_msg))
                                        end if
                                    end if
                                    
                                    rsingle_arr(1 : diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j), j) = &
                                        diag_data2d_store%m_rsingle( &
                                            diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) : &
                                            diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) + &
                                                diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j) - 1)
                                end do
                                
                                !print *, "end queue / start put"
                                call nclayer_check(nf90_put_var(&
                                    ncid, diag_data2d_store%var_ids(curdatindex), &
                                    rsingle_arr, &
                                    (/ 1, 1 + diag_data2d_store%rel_indexes(curdatindex) /), &
                                    (/ diag_data2d_store%max_lens(curdatindex), &
                                        diag_data2d_store%stor_i_arr(curdatindex)%icount /) &
                                    ))
                                !call nclayer_check(nf90_sync(ncid))
                                deallocate(rsingle_arr)
                                !print *, "end put"
                                
                            else if (data_type == NLAYER_DOUBLE) then
                                allocate(rdouble_arr(diag_data2d_store%max_lens(curdatindex), &
                                    diag_data2d_store%stor_i_arr(curdatindex)%icount))
                                
                                rdouble_arr = NLAYER_FILL_DOUBLE
                                
                                do j = 1, diag_data2d_store%stor_i_arr(curdatindex)%icount
                                    ! Just in case our definition checks failed...
                                    if (diag_data2d_store%max_lens(curdatindex) /= &
                                        diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j)) then
                                        ! Show message!
                                        ! NOTE - I0 and TRIM are Fortran 95 specs
                                        write (data_uneven_msg, "(A, I0, A, I0, A)") "Amount of data written in " // &
                                            trim(data2d_name) // " (", &
                                            diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j), &
                                            ")" // char(10) // &
                                            "             does not match the variable length" // &
                                            " (", diag_data2d_store%max_lens(curdatindex), ")!"
                                        
                                        if (diag_data2d_store%strict_check) then
                                            call nclayer_error(trim(data_uneven_msg))
                                        else
                                            call nclayer_warning(trim(data_uneven_msg))
                                        end if
                                    end if
                                    
                                    rdouble_arr(1 : diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j), j) = &
                                        diag_data2d_store%m_rdouble( &
                                            diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) : &
                                            diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) + &
                                                diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j) - 1)
                                end do
                                
                                call nclayer_check(nf90_put_var(&
                                    ncid, diag_data2d_store%var_ids(curdatindex), &
                                    rdouble_arr, &
                                    (/ 1, 1 + diag_data2d_store%rel_indexes(curdatindex) /), &
                                    (/ diag_data2d_store%max_lens(curdatindex), &
                                        diag_data2d_store%stor_i_arr(curdatindex)%icount /) &
                                    ))
                                deallocate(rdouble_arr)
                            else if (data_type == NLAYER_STRING) then
                                ! We need to seperate everything because the Intel Fortran compiler loves
                                ! to optimize... and then assume that I'll try to use an unallocated variable,
                                ! even with checks.
                                if (allocated(diag_data2d_store%max_str_lens)) then
                                    max_str_len = diag_data2d_store%max_str_lens(curdatindex)
                                else
                                    call nclayer_error("BUG: diag_data2d_store%max_str_lens not allocated yet!")
                                end if
                                
                                allocate(character(max_str_len) :: &
                                    string_arr(diag_data2d_store%max_lens(curdatindex), &
                                    diag_data2d_store%stor_i_arr(curdatindex)%icount &
                                    ))
                                
                                string_arr = NLAYER_FILL_CHAR
                                
                                do j = 1, diag_data2d_store%stor_i_arr(curdatindex)%icount
                                    ! Just in case our definition checks failed...
                                    if (diag_data2d_store%max_lens(curdatindex) /= &
                                        diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j)) then
                                        ! Show message!
                                        ! NOTE - I0 and TRIM are Fortran 95 specs
                                        write (data_uneven_msg, "(A, I0, A, I0, A)") "Amount of data written in " // &
                                            trim(data2d_name) // " (", &
                                            diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j), &
                                            ")" // char(10) // &
                                            "             does not match the variable length" // &
                                            " (", diag_data2d_store%max_lens(curdatindex), ")!"
                                        
                                        if (diag_data2d_store%strict_check) then
                                            call nclayer_error(trim(data_uneven_msg))
                                        else
                                            call nclayer_warning(trim(data_uneven_msg))
                                        end if
                                    end if
                                    
                                    string_arr(1 : diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j), j) = &
                                        diag_data2d_store%m_string( &
                                            diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) : &
                                            diag_data2d_store%stor_i_arr(curdatindex)%index_arr(j) + &
                                                diag_data2d_store%stor_i_arr(curdatindex)%length_arr(j) - 1)
                                end do
                                
                                if (allocated(diag_data2d_store%max_str_lens)) then
                                    call nclayer_check(nf90_put_var(&
                                        ncid, diag_data2d_store%var_ids(curdatindex), &
                                        string_arr, &
                                        (/ 1, 1, 1 + diag_data2d_store%rel_indexes(curdatindex) /), &
                                        (/ diag_data2d_store%max_str_lens(curdatindex), &
                                            diag_data2d_store%max_lens(curdatindex), &
                                            diag_data2d_store%stor_i_arr(curdatindex)%icount /) &
                                        ))
                                else
                                    call nclayer_error("BUG: diag_data2d_store%max_str_lens not allocated yet!")
                                end if
                                
                                deallocate(string_arr)
                            end if
                            
                            ! Check for data flushing, and if so, update the relative indexes
                            ! and set icount to 0.
                            if (present(flush_data_only) .AND. flush_data_only) then
                                diag_data2d_store%rel_indexes(curdatindex) = &
                                    diag_data2d_store%rel_indexes(curdatindex) + &
                                    diag_data2d_store%stor_i_arr(curdatindex)%icount
                                diag_data2d_store%stor_i_arr(curdatindex)%icount = 0
                                
#ifdef _DEBUG_MEM_
                                print *, "diag_data2d_store%rel_indexes(curdatindex) is now:"
                                print *, diag_data2d_store%rel_indexes(curdatindex)
#endif
                            end if
                            
                        end if
                    end do
                    
                    if (present(flush_data_only) .AND. flush_data_only) then
#ifdef _DEBUG_MEM_
                        print *, "In buffer flush mode!"
#endif
                        
                        ! We need to reset all array counts to zero!
                        diag_data2d_store%acount = 0
                    else
                        ! Lock data writing
                        diag_data2d_store%data_lock = .TRUE.
#ifdef _DEBUG_MEM_
                        print *, "In data lock mode!"
#endif
                    end if
                else
                    call nclayer_error("Can't write data - data have already been written and locked!")
                end if
            else
                call nclayer_error("Can't write data - NetCDF4 layer not initialized yet!")
            end if
            
#ifdef _DEBUG_MEM_
            print *, "All done writing data2d data"
#endif
        end subroutine nc_diag_data2d_write_data
        
        ! Set strict checking
        subroutine nc_diag_data2d_set_strict(enable_strict)
            logical, intent(in) :: enable_strict
            
            if (init_done .AND. allocated(diag_data2d_store)) then
                diag_data2d_store%strict_check = enable_strict
            else
                call nclayer_error("Can't set strictness level for data2d - NetCDF4 layer not initialized yet!")
            end if
        end subroutine nc_diag_data2d_set_strict
        
        ! Preallocate variable name/type/etc. storage.
        subroutine nc_diag_data2d_prealloc_vars(num_of_addl_vars)
            integer(i_llong), intent(in)          :: num_of_addl_vars
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_data2d_prealloc_vars(num_of_addl_vars = ", num_of_addl_vars, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            if (init_done .AND. allocated(diag_data2d_store)) then
                if (allocated(diag_data2d_store%names)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%names)) then
                        call nc_diag_realloc(diag_data2d_store%names, num_of_addl_vars)
                    end if
                else
                    allocate(diag_data2d_store%names(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                end if
                
                if (allocated(diag_data2d_store%types)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%types)) then
                        call nc_diag_realloc(diag_data2d_store%types, num_of_addl_vars)
                    end if
                else
                    allocate(diag_data2d_store%types(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                end if
                
                if (allocated(diag_data2d_store%stor_i_arr)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%stor_i_arr)) then
                        call nc_diag_data2d_resize_iarr_type(num_of_addl_vars)
                    end if
                else
                    allocate(diag_data2d_store%stor_i_arr(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                end if
                
                if (allocated(diag_data2d_store%var_ids)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%var_ids)) then
                        call nc_diag_realloc(diag_data2d_store%var_ids, num_of_addl_vars)
                    end if
                else
                    allocate(diag_data2d_store%var_ids(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_data2d_store%var_ids = -1
                end if
                
                if (allocated(diag_data2d_store%var_dim_ids)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%var_dim_ids)) then
                        call nc_diag_realloc(diag_data2d_store%var_dim_ids, num_of_addl_vars)
                    end if
                else
                    allocate(diag_data2d_store%var_dim_ids(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_data2d_store%var_dim_ids = -1
                end if
                
                if (allocated(diag_data2d_store%alloc_sia_multi)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%alloc_sia_multi)) then
                        call nc_diag_realloc(diag_data2d_store%alloc_sia_multi, num_of_addl_vars)
                    end if
                else
                    allocate(diag_data2d_store%alloc_sia_multi(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_data2d_store%alloc_sia_multi = 0
                end if
                
                if (allocated(diag_data2d_store%max_str_lens)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%max_str_lens)) then
                        call nc_diag_realloc(diag_data2d_store%max_str_lens, num_of_addl_vars)
                    end if
                else
                    allocate(diag_data2d_store%max_str_lens(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_data2d_store%max_str_lens = -1
                end if
                
                if (allocated(diag_data2d_store%rel_indexes)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%rel_indexes)) then
                        call nc_diag_realloc(diag_data2d_store%rel_indexes, num_of_addl_vars)
                    end if
                else
                    allocate(diag_data2d_store%rel_indexes(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_data2d_store%rel_indexes = 0
                end if
                
                if (allocated(diag_data2d_store%max_lens)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%max_lens)) then
                        call nc_diag_realloc(diag_data2d_store%max_lens, num_of_addl_vars)
                    end if
                else
                    allocate(diag_data2d_store%max_lens(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_data2d_store%max_lens = 0
                end if
                
                diag_data2d_store%prealloc_total = diag_data2d_store%prealloc_total + num_of_addl_vars
            else
                call nclayer_error("NetCDF4 layer not initialized yet!")
            endif
        end subroutine nc_diag_data2d_prealloc_vars
        
        ! Preallocate actual variable data storage
        subroutine nc_diag_data2d_prealloc_vars_storage(nclayer_type, num_of_addl_slots)
            integer(i_byte), intent(in)           :: nclayer_type
            integer(i_llong), intent(in)          :: num_of_addl_slots
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, I0, A, I0, A)") "nc_diag_data2d_prealloc_vars_storage(nclayer_type = ", nclayer_type, ", num_of_addl_slots = ", num_of_addl_slots, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif            
            
            if (nclayer_type == NLAYER_BYTE) then
                call nc_diag_data2d_resize_byte(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_SHORT) then
                call nc_diag_data2d_resize_short(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_LONG) then
                call nc_diag_data2d_resize_long(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_FLOAT) then
                call nc_diag_data2d_resize_rsingle(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_DOUBLE) then
                call nc_diag_data2d_resize_rdouble(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_STRING) then
                call nc_diag_data2d_resize_string(num_of_addl_slots, .FALSE.)
            else
                call nclayer_error("Invalid type specified for variable storage preallocation!")
            end if
        end subroutine nc_diag_data2d_prealloc_vars_storage
        
        ! Preallocate index storage
        subroutine nc_diag_data2d_prealloc_vars_storage_all(num_of_addl_slots)
            integer(i_llong), intent(in)          :: num_of_addl_slots
            integer(i_long)                       :: i
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_data2d_prealloc_vars_storage_all(num_of_addl_slots = ", num_of_addl_slots, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            do i = 1, diag_data2d_store%prealloc_total
                call nc_diag_data2d_resize_iarr(i, num_of_addl_slots, .FALSE.)
            end do
        end subroutine nc_diag_data2d_prealloc_vars_storage_all
        
        subroutine nc_diag_data2d_expand
            integer(i_llong) :: addl_fields
            
            ! Did we realloc at all?
            logical :: meta_realloc
            
            meta_realloc = .FALSE.
            
            if (init_done .AND. allocated(diag_data2d_store)) then
                addl_fields = 1 + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_data2d_store%alloc_s_multi))
                
#ifdef _DEBUG_MEM_
                call nclayer_debug("INITIAL value of diag_data2d_store%alloc_s_multi:")
                print *, diag_data2d_store%alloc_s_multi
#endif
                
                if (allocated(diag_data2d_store%names)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%names)) then
#ifdef _DEBUG_MEM_
                        call nclayer_debug("Reallocating diag_data2d_store%names...")
                        print *, (NLAYER_MULTI_BASE ** diag_data2d_store%alloc_s_multi)
                        print *, addl_fields
#endif
                        call nc_diag_realloc(diag_data2d_store%names, addl_fields)
#ifdef _DEBUG_MEM_
                    call nclayer_debug("Reallocated diag_data2d_store%names. Size:")
                    print *, size(diag_data2d_store%names)
#endif
                        meta_realloc = .TRUE.
                    end if
                else
#ifdef _DEBUG_MEM_
                    call nclayer_debug("Allocating diag_data2d_store%names for first time...")
                    print *, NLAYER_DEFAULT_ENT
#endif
                    
                    allocate(diag_data2d_store%names(NLAYER_DEFAULT_ENT))
                    
#ifdef _DEBUG_MEM_
                    call nclayer_debug("Allocated diag_data2d_store%names. Size:")
                    print *, size(diag_data2d_store%names)
#endif
                end if
                
                if (allocated(diag_data2d_store%types)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%types)) then
#ifdef _DEBUG_MEM_
                        call nclayer_debug("Reallocating diag_data2d_store%types...")
                        print *, (NLAYER_MULTI_BASE ** diag_data2d_store%alloc_s_multi)
                        print *, addl_fields
#endif
                        call nc_diag_realloc(diag_data2d_store%types, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%types(NLAYER_DEFAULT_ENT))
                end if
                
                if (allocated(diag_data2d_store%stor_i_arr)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%stor_i_arr)) then
#ifdef _DEBUG_MEM_
                        call nclayer_debug("Reallocating diag_data2d_store%stor_i_arr...")
                        print *, (NLAYER_MULTI_BASE ** diag_data2d_store%alloc_s_multi)
                        print *, (1 + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_data2d_store%alloc_s_multi)))
#endif
                        call nc_diag_data2d_resize_iarr_type(addl_fields)
                        
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%stor_i_arr(NLAYER_DEFAULT_ENT))
                end if
                
                if (allocated(diag_data2d_store%var_ids)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%var_ids)) then
                        call nc_diag_realloc(diag_data2d_store%var_ids, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%var_ids(NLAYER_DEFAULT_ENT))
                    diag_data2d_store%var_ids = -1
                end if
                
                if (allocated(diag_data2d_store%var_dim_ids)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%var_dim_ids)) then
                        call nc_diag_realloc(diag_data2d_store%var_dim_ids, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%var_dim_ids(NLAYER_DEFAULT_ENT))
                    diag_data2d_store%var_dim_ids = -1
                end if
                
                if (allocated(diag_data2d_store%alloc_sia_multi)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%alloc_sia_multi)) then
                        call nc_diag_realloc(diag_data2d_store%alloc_sia_multi, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%alloc_sia_multi(NLAYER_DEFAULT_ENT))
                    diag_data2d_store%alloc_sia_multi = 0
                end if
                
                if (allocated(diag_data2d_store%max_str_lens)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%max_str_lens)) then
                        call nc_diag_realloc(diag_data2d_store%max_str_lens, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%max_str_lens(NLAYER_DEFAULT_ENT))
                    diag_data2d_store%max_str_lens = -1
                end if
                
                if (allocated(diag_data2d_store%rel_indexes)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%rel_indexes)) then
                        call nc_diag_realloc(diag_data2d_store%rel_indexes, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%rel_indexes(NLAYER_DEFAULT_ENT))
                    diag_data2d_store%rel_indexes = 0
                end if
                
                if (allocated(diag_data2d_store%max_lens)) then
                    if (diag_data2d_store%total >= size(diag_data2d_store%max_lens)) then
                        call nc_diag_realloc(diag_data2d_store%max_lens, addl_fields)
                        meta_realloc = .TRUE.
                    end if
                else
                    allocate(diag_data2d_store%max_lens(NLAYER_DEFAULT_ENT))
                    diag_data2d_store%max_lens = 0
                end if
                
                if (meta_realloc) then
                    diag_data2d_store%alloc_s_multi = diag_data2d_store%alloc_s_multi + 1
#ifdef _DEBUG_MEM_
                    print *, "Incrementing alloc_s_multi... new value:"
                    print *, diag_data2d_store%alloc_s_multi
#endif
                endif
            else
                call nclayer_error("NetCDF4 layer not initialized yet!")
            endif
            
        end subroutine nc_diag_data2d_expand
        
        function nc_diag_data2d_lookup_var(data2d_name) result(ind)
            character(len=*), intent(in)    :: data2d_name
            integer :: i, ind
            
            ind = -1
            
            if (init_done .AND. allocated(diag_data2d_store)) then
                do i = 1, diag_data2d_store%total
                    if (diag_data2d_store%names(i) == data2d_name) then
                        ind = i
                        exit
                    end if
                end do
            end if
        end function nc_diag_data2d_lookup_var
        
        ! nc_diag_data2d - input integer(i_byte)
        ! Corresponding NetCDF4 type: byte
        subroutine nc_diag_data2d_byte(data2d_name, data2d_value)
            character(len=*), intent(in)              :: data2d_name
            integer(i_byte), dimension(:), intent(in) :: data2d_value
            
            integer(i_long)                 :: var_index
            integer(i_llong)                :: input_size
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            integer(i_llong)                      :: data_value_size
            
            if (nclayer_enable_action) then
                data_value_size = size(data2d_value)
                write(action_str, "(A, I0, A, I0, A, I0, A, I0, A)") &
                    "nc_diag_data2d_byte(data2d_name = " // data2d_name // &
                    ", data2d_value = array with length of ", &
                    data_value_size, &
                    " [", &
                    data2d_value(1), &
                    " ... ", &
                    data2d_value(data_value_size), &
                    "]"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            if (diag_data2d_store%data_lock) then
                call nclayer_error("Can't add new data - data have already been written and locked!")
            end if
            
            var_index = nc_diag_data2d_lookup_var(data2d_name)
            
            if (var_index == -1) then
                ! First, check to make sure we can still define new variables.
                if (diag_data2d_store%def_lock) then
                    call nclayer_error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_data2d_expand
                
                diag_data2d_store%total = diag_data2d_store%total + 1
                
                diag_data2d_store%names(diag_data2d_store%total) = data2d_name
                diag_data2d_store%types(diag_data2d_store%total) = NLAYER_BYTE
                
                var_index = diag_data2d_store%total
            end if
            
            ! Get input size and do size checks!
            input_size = size(data2d_value)
            
            if ((diag_data2d_store%def_lock) .AND. &
                (size(data2d_value) > diag_data2d_store%max_lens(var_index))) then
                call nclayer_error("Cannot expand variable size after locking variable definitions!")
            end if
            
            ! We just need to add one entry...
            call nc_diag_data2d_resize_iarr(var_index, 1_i_llong)
            call nc_diag_data2d_resize_byte(input_size)
            
            ! Now add the actual entry!
            diag_data2d_store%m_byte(diag_data2d_store%acount(1) - input_size + 1:diag_data2d_store%acount(1)) = &
                data2d_value
            diag_data2d_store%stor_i_arr(var_index)%index_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                diag_data2d_store%acount(1) - input_size + 1
            diag_data2d_store%stor_i_arr(var_index)%length_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                input_size
        end subroutine nc_diag_data2d_byte
        
        ! nc_diag_data2d - input integer(i_short)
        ! Corresponding NetCDF4 type: short
        subroutine nc_diag_data2d_short(data2d_name, data2d_value)
            character(len=*), intent(in)               :: data2d_name
            integer(i_short), dimension(:), intent(in) :: data2d_value
            
            integer(i_long)                 :: var_index
            integer(i_llong)                :: input_size
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            integer(i_llong)                      :: data_value_size
            
            if (nclayer_enable_action) then
                data_value_size = size(data2d_value)
                write(action_str, "(A, I0, A, I0, A, I0, A, I0, A)") &
                    "nc_diag_data2d_short(data2d_name = " // data2d_name // &
                    ", data2d_value = array with length of ", &
                    data_value_size, &
                    " [", &
                    data2d_value(1), &
                    " ... ", &
                    data2d_value(data_value_size), &
                    "]"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            if (diag_data2d_store%data_lock) then
                call nclayer_error("Can't add new data - data have already been written and locked!")
            end if
            
            var_index = nc_diag_data2d_lookup_var(data2d_name)
            
            if (var_index == -1) then
                ! First, check to make sure we can still define new variables.
                if (diag_data2d_store%def_lock) then
                    call nclayer_error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_data2d_expand
                
                diag_data2d_store%total = diag_data2d_store%total + 1
                
                diag_data2d_store%names(diag_data2d_store%total) = data2d_name
                diag_data2d_store%types(diag_data2d_store%total) = NLAYER_SHORT
                
                var_index = diag_data2d_store%total
            end if
            
            ! Get input size and do size checks!
            input_size = size(data2d_value)
            
            if ((diag_data2d_store%def_lock) .AND. &
                (size(data2d_value) > diag_data2d_store%max_lens(var_index))) then
                call nclayer_error("Cannot expand variable size after locking variable definitions!")
            end if
            
            ! We just need to add one entry...
            call nc_diag_data2d_resize_iarr(var_index, 1_i_llong)
            call nc_diag_data2d_resize_short(input_size)
            
            ! Now add the actual entry!
            diag_data2d_store%m_short(diag_data2d_store%acount(2) - input_size + 1:diag_data2d_store%acount(2)) = &
                data2d_value
            diag_data2d_store%stor_i_arr(var_index)%index_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                diag_data2d_store%acount(2) - input_size + 1
            diag_data2d_store%stor_i_arr(var_index)%length_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                input_size
        end subroutine nc_diag_data2d_short
        
        ! nc_diag_data2d - input integer(i_long)
        ! Corresponding NetCDF4 type: int (old: long)
        subroutine nc_diag_data2d_long(data2d_name, data2d_value)
            character(len=*), intent(in)              :: data2d_name
            integer(i_long), dimension(:), intent(in) :: data2d_value
            
            integer(i_long)                 :: var_index
            integer(i_llong)                :: input_size
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            integer(i_llong)                      :: data_value_size
            
            if (nclayer_enable_action) then
                data_value_size = size(data2d_value)
                write(action_str, "(A, I0, A, I0, A, I0, A, I0, A)") &
                    "nc_diag_data2d_long(data2d_name = " // data2d_name // &
                    ", data2d_value = array with length of ", &
                    data_value_size, &
                    " [", &
                    data2d_value(1), &
                    " ... ", &
                    data2d_value(data_value_size), &
                    "]"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            if (diag_data2d_store%data_lock) then
                call nclayer_error("Can't add new data - data have already been written and locked!")
            end if
            
            var_index = nc_diag_data2d_lookup_var(data2d_name)
            
            if (var_index == -1) then
                ! First, check to make sure we can still define new variables.
                if (diag_data2d_store%def_lock) then
                    call nclayer_error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_data2d_expand
                
                diag_data2d_store%total = diag_data2d_store%total + 1
                
                diag_data2d_store%names(diag_data2d_store%total) = data2d_name
                diag_data2d_store%types(diag_data2d_store%total) = NLAYER_LONG
                
                var_index = diag_data2d_store%total
            end if
            
#ifdef _DEBUG_MEM_
            call nclayer_debug("Current total:")
            print *, diag_data2d_store%total
#endif
            
            ! Get input size and do size checks!
            input_size = size(data2d_value)
            
            if ((diag_data2d_store%def_lock) .AND. &
                (size(data2d_value) > diag_data2d_store%max_lens(var_index))) then
                call nclayer_error("Cannot expand variable size after locking variable definitions!")
            end if
            
            ! We just need to add one entry...
            call nc_diag_data2d_resize_iarr(var_index, 1_i_llong)
            call nc_diag_data2d_resize_long(input_size)
            
            ! Now add the actual entry!
            diag_data2d_store%m_long(diag_data2d_store%acount(3) - input_size + 1:diag_data2d_store%acount(3)) = &
                data2d_value
            diag_data2d_store%stor_i_arr(var_index)%index_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                diag_data2d_store%acount(3) - input_size + 1
            diag_data2d_store%stor_i_arr(var_index)%length_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                input_size
        end subroutine nc_diag_data2d_long
        
        ! nc_diag_data2d - input real(r_single)
        ! Corresponding NetCDF4 type: float (or real)
        subroutine nc_diag_data2d_rsingle(data2d_name, data2d_value)
            character(len=*), intent(in)             :: data2d_name
            real(r_single), dimension(:), intent(in) :: data2d_value

            integer(i_long)                 :: var_index
            integer(i_llong)                :: input_size
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            integer(i_llong)                      :: data_value_size
            
            if (nclayer_enable_action) then
                data_value_size = size(data2d_value)
                write(action_str, "(A, I0, A, F0.5, A, F0.5, A)") &
                    "nc_diag_data2d_rsingle(data2d_name = " // data2d_name // &
                    ", data2d_value = array with length of ", &
                    data_value_size, &
                    " [", &
                    data2d_value(1), &
                    " ... ", &
                    data2d_value(data_value_size), &
                    "]"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            if (diag_data2d_store%data_lock) then
                call nclayer_error("Can't add new data - data have already been written and locked!")
            end if
            
            var_index = nc_diag_data2d_lookup_var(data2d_name)
            
            if (var_index == -1) then
                ! First, check to make sure we can still define new variables.
                if (diag_data2d_store%def_lock) then
                    call nclayer_error("Can't add new variable - definitions have already been written and locked!")
                end if
#ifdef _DEBUG_MEM_
                write (*, "(A, A, A, F)") "NEW data2d: ", data2d_name, " | First value: ", data2d_value
#endif
                call nc_diag_data2d_expand
                
                diag_data2d_store%total = diag_data2d_store%total + 1
                
                diag_data2d_store%names(diag_data2d_store%total) = data2d_name
                diag_data2d_store%types(diag_data2d_store%total) = NLAYER_FLOAT
                
                var_index = diag_data2d_store%total
            end if
            
            ! Get input size and do size checks!
            input_size = size(data2d_value)
            
            if ((diag_data2d_store%def_lock) .AND. &
                (size(data2d_value) > diag_data2d_store%max_lens(var_index))) then
                call nclayer_error("Cannot expand variable size after locking variable definitions!")
            end if
            
            ! We just need to add one entry...
            call nc_diag_data2d_resize_iarr(var_index, 1_i_llong)
            call nc_diag_data2d_resize_rsingle(input_size)
            
            ! Now add the actual entry!
            diag_data2d_store%m_rsingle(diag_data2d_store%acount(4) - input_size + 1:diag_data2d_store%acount(4)) = &
                data2d_value
            diag_data2d_store%stor_i_arr(var_index)%index_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                diag_data2d_store%acount(4) - input_size + 1
            diag_data2d_store%stor_i_arr(var_index)%length_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                input_size
        end subroutine nc_diag_data2d_rsingle
        
        ! nc_diag_data2d - input real(r_double)
        ! Corresponding NetCDF4 type: double
        subroutine nc_diag_data2d_rdouble(data2d_name, data2d_value)
            character(len=*), intent(in)             :: data2d_name
            real(r_double), dimension(:), intent(in) :: data2d_value

            integer(i_long)                 :: var_index
            integer(i_llong)                :: input_size
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            integer(i_llong)                      :: data_value_size
            
            if (nclayer_enable_action) then
                data_value_size = size(data2d_value)
                write(action_str, "(A, I0, A, F0.5, A, F0.5, A)") &
                    "nc_diag_data2d_rdouble(data2d_name = " // data2d_name // &
                    ", data2d_value = array with length of ", &
                    data_value_size, &
                    " [", &
                    data2d_value(1), &
                    " ... ", &
                    data2d_value(data_value_size), &
                    "]"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            if (diag_data2d_store%data_lock) then
                call nclayer_error("Can't add new data - data have already been written and locked!")
            end if
            
            var_index = nc_diag_data2d_lookup_var(data2d_name)
            
            if (var_index == -1) then
                ! First, check to make sure we can still define new variables.
                if (diag_data2d_store%def_lock) then
                    call nclayer_error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_data2d_expand
                
                diag_data2d_store%total = diag_data2d_store%total + 1
                
                diag_data2d_store%names(diag_data2d_store%total) = data2d_name
                diag_data2d_store%types(diag_data2d_store%total) = NLAYER_DOUBLE
                
                var_index = diag_data2d_store%total
            end if
            
            ! Get input size and do size checks!
            input_size = size(data2d_value)
            
            if ((diag_data2d_store%def_lock) .AND. &
                (size(data2d_value) > diag_data2d_store%max_lens(var_index))) then
                call nclayer_error("Cannot expand variable size after locking variable definitions!")
            end if
            
            ! We just need to add one entry...
            call nc_diag_data2d_resize_iarr(var_index, 1_i_llong)
            call nc_diag_data2d_resize_rdouble(input_size)
            
            ! Now add the actual entry!
            diag_data2d_store%m_rdouble(diag_data2d_store%acount(5) - input_size + 1:diag_data2d_store%acount(5)) = &
                data2d_value
            diag_data2d_store%stor_i_arr(var_index)%index_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                diag_data2d_store%acount(5) - input_size + 1
            diag_data2d_store%stor_i_arr(var_index)%length_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                input_size
        end subroutine nc_diag_data2d_rdouble

        ! nc_diag_data2d - input character(len=*)
        ! Corresponding NetCDF4 type: string? char?
        subroutine nc_diag_data2d_string(data2d_name, data2d_value)
            character(len=*), intent(in)               :: data2d_name
            character(len=*), dimension(:), intent(in) :: data2d_value
            
            integer(i_long)                 :: var_index
            integer(i_long)                 :: max_str_len
            integer(i_llong)                :: input_size
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            integer(i_llong)                      :: data_value_size
            
            if (nclayer_enable_action) then
                data_value_size = size(data2d_value)
                write(action_str, "(A, I0, A, A)") &
                    "nc_diag_data2d_string(data2d_name = " // data2d_name // &
                    ", data2d_value = array with length of ", &
                    data_value_size, &
                    " [" // &
                        trim(data2d_value(1)) // &
                        " ... " // &
                        trim(data2d_value(data_value_size)) // &
                        "]"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            if (diag_data2d_store%data_lock) then
                call nclayer_error("Can't add new data - data have already been written and locked!")
            end if
            
            var_index = nc_diag_data2d_lookup_var(data2d_name)
            
            if (var_index == -1) then
                ! First, check to make sure we can still define new variables.
                if (diag_data2d_store%def_lock) then
                    call nclayer_error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                call nc_diag_data2d_expand
                
                diag_data2d_store%total = diag_data2d_store%total + 1
                
                diag_data2d_store%names(diag_data2d_store%total) = data2d_name
                diag_data2d_store%types(diag_data2d_store%total) = NLAYER_STRING
                
                var_index = diag_data2d_store%total
            else
                ! Check max string length
#ifdef _DEBUG_MEM_
                print *, "len_trim(data2d_value) = ", len_trim(data2d_value)
                print *, "diag_data2d_store%max_str_lens(var_index) = ", diag_data2d_store%max_str_lens(var_index)
#endif
            end if
            
            ! Get input size and do size checks!
            input_size = size(data2d_value)
            
            if (diag_data2d_store%def_lock) then
                if (input_size > diag_data2d_store%max_lens(var_index)) &
                    call nclayer_error("Cannot expand variable size after locking variable definitions!")
                
                ! Check max string length
                max_str_len = max_len_string_array(data2d_value, &
                    int(input_size))
                
#ifdef _DEBUG_MEM_
                print *, "max_str_len: ", max_str_len
                print *, "diag_data2d_store%max_str_lens(var_index): ", diag_data2d_store%max_str_lens(var_index)
#endif
                
                if (max_str_len > diag_data2d_store%max_str_lens(var_index)) &
                    call nclayer_error("Cannot expand variable string length after locking variable definitions!")
            end if
            
            ! We just need to add one entry...
            call nc_diag_data2d_resize_iarr(var_index, 1_i_llong)
            call nc_diag_data2d_resize_string(input_size)
            
            ! If trim isn't enabled, set our maximum string length here!
            if (.NOT. enable_trim) then
                if (diag_data2d_store%max_str_lens(var_index) == -1) then
                    diag_data2d_store%max_str_lens(var_index) = len(data2d_value(1))
                else
                    ! Validate that our non-first value isn't different from
                    ! the initial string length
                    if (max_len_notrim_string_array(data2d_value, int(input_size)) /= &
                        diag_data2d_store%max_str_lens(var_index)) &
                        call nclayer_error("Cannot change string size when trimming is disabled!")
                end if
            end if
            
            ! Now add the actual entry!
            diag_data2d_store%m_string(diag_data2d_store%acount(6) - input_size + 1:diag_data2d_store%acount(6)) = &
                data2d_value
            diag_data2d_store%stor_i_arr(var_index)%index_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                diag_data2d_store%acount(6) - input_size + 1
            diag_data2d_store%stor_i_arr(var_index)%length_arr(diag_data2d_store%stor_i_arr(var_index)%icount) = &
                input_size
        end subroutine nc_diag_data2d_string
end module ncdw_data2d
