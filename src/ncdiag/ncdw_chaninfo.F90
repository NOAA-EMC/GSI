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
! chaninfo module - ncdw_chaninfo
!
module ncdw_chaninfo
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
    !   Because Fortran is a strongly typed language, we can't do silly
    !   tricks in C, like allocating some memory to a void pointer and
    !   just storing our byte, short, int, long, float, or double
    !   numeric data there, and later casting it back...
    !   
    !   (e.g. void **data_ref; data_ref = malloc(sizeof(void *) * 1000);
    !         float *f = malloc(sizeof(float)); *f = 1.2345;
    !         data_ref[0] = f; ...)
    !  
    !   No frets - we can work around this issue with some derived types
    !   and arrays! We create an array for each type we want to support.
    !   Since we're using kinds.F90, we support the following types:
    !     i_byte, i_short, i_long, r_single, r_double, character(len=*)
    !  
    !   The derived type used, diag_chaninfo, has these variables to
    !   help us keep track of everything:
    !  
    !   -> ci_* - these arrays have the types listed above, plus string
    !      support! These arrays are simply arrays that we throw our
    !      data in. However, don't mistaken "throw in" with
    !      "disorganized" - chaninfo uses a very structured format for
    !      these variables! Keep reading to find out how we structure
    !      it...
    !      
    !   -> nchans - the number of channels to use. Remember that
    !      chaninfo variables have dimensions 1 x nchans - basically, we
    !      need to store nchans values. We'll need this a LOT to do
    !      consistency checks, and to keep track of everything!
    !      
    !   -> names - all of the chaninfo variable names! We'll be using
    !      this array to store and lookup chaninfo variables, as well as
    !      storing them!
    !      
    !   -> types - all of the chaninfo variable types! These are byte
    !      integers that get compared to our NLAYER_* type constants
    !      (see: ncdw_types.F90).
    !      
    !   -> var_usage - the amount of entries we've stored in our 
    !      chaninfo variable! For instance, if we called
    !      nc_diag_chaninfo("myvar", 1) three times, for that particular
    !      var_usage(i), we would have an entry of 3.
    !      
    !   -> var_rel_pos - the star of the show! This is an abbreviation
    !      of "variable relative positioning". Recall that we store
    !      our variable data in ci_* specific type arrays. We know
    !      the nchans amount, and we know the type. This variable stores
    !      the "block" that our data is in within the type array.
    !      
    !      Therefore, we can use the equation to find our starting
    !      position: 1 + [(REL_VAL - 1) * nchans]
    !      
    !      For instance, if var_rel_pos(1) for variable names(1) = "bla"
    !      is set to 2, nchans is 10, and the type is NLAYER_FLOAT, we
    !      can deduce that in ci_rsingle, our data can be found starting
    !      at 1 + (1 * 10) = 11. This makes sense, as seen with our mini
    !      diagram below:
    !      
    !      ci_rsingle:
    !        /                    ci_rsingle index                   \
    !       1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20
    !      [ x, x, x, x, x, x, x, x, x, x, y, y, y, y, y, y, y, y, y, y]
    !        \                    ci_rsingle array                   /
    !      
    !      Indeed, our second block does start at index 11!
    !      As a bonus, since our data is in blocks, things can be super
    !      fast since we're just cutting our big array into small ones!
    !      
    !   -> acount_v: Finally, we have dynamic allocation. We have in our
    !      type a variable called acount_v. This tells us how many
    !      variables are stored in each type. Using the same equation
    !      above, and combining with var_usage, we can figure out where
    !      to put our data!
    !      
    !      Assume var_usage(i) = 2, block starts at index 11 with the
    !      equation above.
    !      
    !      Again, with our fun little diagram:
    !      
    !      ci_rsingle:
    !        /                    ci_rsingle index                   \
    !       1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20
    !      [ x, x, x, x, x, x, x, x, x, x, y, y, Y, y, y, y, y, y, y, y]
    !      [ BLOCK 1 SEEK = 1->10->11   ][var_u=2|--block 2 area 11->20]
    !        \                    ci_rsingle array                   /
    !      
    !      The capital Y marks the place we store our data!
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
    
    ! Load our numerical types from kinds
    ! Note that i_llong is not a type we store - it's just for keeping
    ! track of numeric indexes. (Maybe this is too excessive...)
    use ncd_kinds, only: i_byte, i_short, i_long, i_llong, r_single, &
        r_double
    
    ! Load state variables! We need to know:
    !   init_done           - ...whether a file is currently loaded or
    !                         not.
    !   ncid                - ...the current NCID of our file.
    !   append_only         - ...whether we are in append mode or not.
    !   enable_trim         - ...whether we need to automatically trim
    !                         our strings for chaninfo string storage or
    !                         not.
    !   diag_chaninfo_store - ...chaninfo variable information.
    !                         We pretty much do everything related to
    !                         chaninfo here, so we're using everything
    !                         inside this derived type!
    use ncdw_state, only: init_done, ncid, append_only, &
        enable_trim, &
        diag_chaninfo_store
    
    ! Load types! We need:
    !   NLAYER_*           - nc_diag types.
    !   NLAYER_FILL_*      - nc_diag type fill. This is pretty much
    !                        equivalent to NF90_FILL_*.
    !   NLAYER_COMPRESSION - zlib (a la gzip) compression level setting.
    !   NLAYER_DEFAULT_ENT - default starting number of element entries.
    !                        This is for the initial allocation of
    !                        space for data storage arrays, e.g.
    !                        the ci_* data arrays within diag_chaninfo.
    !   NLAYER_MULTI_BASE  - the base number to use when exponentiating
    !                        to allocate or reallocate data storage
    !                        arrays.
    use ncdw_types, only: NLAYER_BYTE, NLAYER_SHORT, NLAYER_LONG, &
        NLAYER_FLOAT, NLAYER_DOUBLE, NLAYER_STRING, &
        NLAYER_FILL_BYTE, NLAYER_FILL_SHORT, NLAYER_FILL_LONG, &
        NLAYER_FILL_FLOAT, NLAYER_FILL_DOUBLE, NLAYER_FILL_CHAR, &
        NLAYER_COMPRESSION, NLAYER_DEFAULT_ENT, NLAYER_MULTI_BASE
    
    ! Load our varattr adder! We need this to store our new shiny
    ! variable in the varattr database so we can add variable attributes
    ! to our variables.
    use ncdw_varattr, only: nc_diag_varattr_add_var
    
    ! Load our function - given an array of strings, find
    ! max(len_trim(str_array)) - aka the maximum for len_trim()s on each
    ! variable.
    use ncdw_strarrutils, only: max_len_string_array
    
    use ncdw_climsg, only: &
#ifdef ENABLE_ACTION_MSGS
        nclayer_enable_action, nclayer_actionm, &
#endif
        nclayer_error, nclayer_warning, nclayer_info, nclayer_check
    
    ! Load our fun reallocation subroutine - we need this to reallocate
    ! a few things in our preallocation subroutines:
    use ncdw_realloc, only: nc_diag_realloc
    
    ! Load our chaninfo resizing subroutines - these resize our data
    ! storage arrays automatically when needed! 
    use ncdw_ciresize, only: nc_diag_chaninfo_resize_byte, &
        nc_diag_chaninfo_resize_short, nc_diag_chaninfo_resize_long, &
        nc_diag_chaninfo_resize_rsingle, &
        nc_diag_chaninfo_resize_rdouble, nc_diag_chaninfo_resize_string
    
    use netcdf, only: nf90_inquire, nf90_inq_dimid, &
        nf90_inquire_dimension, nf90_inquire_variable, nf90_def_dim, &
        nf90_def_var, nf90_get_var, nf90_put_var, &
        nf90_def_var_deflate, nf90_def_var_chunking, &
        NF90_BYTE, NF90_SHORT, NF90_INT, NF90_FLOAT, NF90_DOUBLE, &
        NF90_CHAR, &
        NF90_EBADDIM, NF90_NOERR, NF90_MAX_NAME, NF90_CHUNKED
    
    implicit none
    
    ! Add a single chaninfo value to a new or existing chaninfo
    ! variable.
    ! 
    ! Given the chaninfo variable name and value, add or update the
    ! variable with the corresponding value.
    ! 
    ! If the variable doesn't already exist, this will automatically
    ! create it and store the value into it.
    ! 
    ! If the variable does exist, it will simply append to the
    ! variable's existing values.
    ! 
    ! Values are inserted in the order of the calls made. As such,
    ! this subroutine is best designed to be used in a loop, where
    ! for every channel iteration, a value is added using this
    ! subroutine.
    ! 
    ! chaninfo is stored element by element - no arrays are accepted,
    ! only scalar values. The best way to call chaninfo is in a loop,
    ! where each channel is being accessed and stored.
    ! 
    ! Once a value has been added, it may not be removed. Make sure you
    ! are certain that the value should be added!
    ! 
    ! The number of values may not exceed the number of channels
    ! (nchans). If more values are added and nchans is exceeded, an
    ! error will occur.
    ! 
    ! Data locking and definition locking will also affect adding
    ! chaninfo variables and value. If data locking is in effect, any
    ! variable or value adding will not work. If definition locking is
    ! in effect, adding variable values to existing variables will still
    ! work, but adding new variables will not.
    ! 
    ! For strings, if the length of the string changes when trimming is
    ! disabled, or when the definitions have been locked, an error will
    ! occur as well.
    ! 
    ! To see more details about what checks are made, see the
    ! corresponding called subroutine documentation for details.
    ! 
    ! Valid data types (represented below as data_types):
    ! integer(i_byte), integer(i_short), integer(i_long),
    ! real(r_single), real(r_double), character(len=*)
    ! 
    ! Args:
    !     name (character(len=*)): the name of the chaninfo variable to
    !         add or update.
    !     value (data_types): the value to add to chaninfo.
    !     
    ! Raises:
    !     If data writing is locked, this will result in an error.
    !     
    !     If the variable doesn't exist yet, and definitions are locked,
    !     this will result in an error.
    !     
    !     If the amount of data in the chaninfo variable is already at
    !     or exceeding nchans, this will result in an error.
    !     
    !     For string data, if the string length changes and the
    !     definitions have already been locked, this will result in an
    !     error.
    !     
    !     Also, for string data, if the string length changes and
    !     trimming is turned off, this will also result in an error.
    !     
    !     The following errors will trigger indirectly from other
    !     subroutines called here:
    !     
    !     If nchans has not been set yet, this will result in an error.
    !     
    !     If there is no file open (or the file is already closed),
    !     this will result in an error.
    !     
    !     Other errors may result from invalid data storage, NetCDF
    !     errors, or even a bug. See the called subroutines'
    !     documentation for details.
    ! 
    interface nc_diag_chaninfo
        module procedure nc_diag_chaninfo_byte, &
            nc_diag_chaninfo_short, nc_diag_chaninfo_long, &
            nc_diag_chaninfo_rsingle, nc_diag_chaninfo_rdouble, &
            nc_diag_chaninfo_string
    end interface nc_diag_chaninfo
    
    contains
        ! Set the number of channels (nchans) for chaninfo to use for
        ! variable storage and configuration.
        ! 
        ! This set the number of channels (nchans) for all of the future
        ! chaninfo variables that will be added. nchans will be used
        ! as the number of elements to use for every chaninfo variable
        ! added. It will also be used as a bounds check for variable
        ! data amounts.
        ! 
        ! Args:
        !     nchans (integer(i_long)): the number of channels to use
        !         for chaninfo.
        !     
        ! Raises:
        !     If nchans was already set, this will result in an error.
        !     (You can't change nchans arbitarily - otherwise, variable
        !     data amounts could become invalid!)
        !     
        !     If the nchans specified is invalid (<1), this will result
        !     in an error. If you have no chaninfo variables to write,
        !     don't call this subroutine at all. No chaninfo variables
        !     will be processed or written if you don't set anything!
        !     
        !     If there is no file open (or the file is already closed),
        !     this will result in an error.
        !     
        !     Although unlikely, other errors may indirectly occur.
        !     They may be general storage errors, or even a bug.
        !     See the called subroutines' documentation for details.
        ! 
        subroutine nc_diag_chaninfo_dim_set(nchans)
            integer(i_long), intent(in) :: nchans
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_chaninfo_dim_set(nchans = ", nchans, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            ! Check if everything is initialized / file is open
            if (init_done .AND. allocated(diag_chaninfo_store)) then
                ! nchans can't be less than 1! 
                if (nchans < 1) then
                    call nclayer_error("Critical error - specified a nchan < 1!")
                end if
                
                ! Is nchans already set?
                if (diag_chaninfo_store%nchans /= -1) &
                    call nclayer_error("nchans already set!")
                
                ! Set nchans
                diag_chaninfo_store%nchans = nchans
            else
                call nclayer_error("NetCDF4 layer not initialized yet!")
            end if
        end subroutine nc_diag_chaninfo_dim_set
        
        ! Set the allocation multiplier for chaninfo variable storage
        ! allocation and reallocation.
        ! 
        ! This sets the allocation multiplier (exponentiator?) for
        ! chaninfo variable storage allocation and reallocation.
        ! 
        ! Reallocation looks like this:
        !     new_size = old_size + addl_num_entries + 
        !       (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** 
        !                             diag_chaninfo_store%alloc_multi))
        ! 
        ! NLAYER_DEFAULT_ENT and NLAYER_MULTI_BASE are constants defined
        ! in ncdw_types. The alloc_multi part is set with this
        ! subroutine.
        ! 
        ! As reallocation occurs, the alloc_multi continues to increase
        ! by one, causing subsequent reallocations to allocate
        ! exponentially more memory.
        ! 
        ! You can use this subroutine to increase the initial amount of
        ! memory allocated/reallocated, or you can use it to prevent
        ! the reallocating counter from increasing by calling this
        ! every so often.
        ! 
        ! If this is not set, it will be initially set to 0 and will
        ! increase from there.
        ! 
        ! Args:
        !     multiplier (integer(i_long)): the multiplier to use when
        !         allocating or reallocating.
        !     
        ! Raises:
        !     If there is no file open (or the file is already closed),
        !     this will result in an error.
        !     
        !     Although unlikely, other errors may indirectly occur.
        !     They may be general storage errors, or even a bug.
        !     See the called subroutines' documentation for details.
        ! 
        subroutine nc_diag_chaninfo_allocmulti(multiplier)
            integer(i_long), intent(in)    :: multiplier
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_chaninfo_allocmulti(multiplier = ", multiplier, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            if (init_done) then
                ! # of times we needed to realloc simple metadata
                ! also the multiplier factor for allocation (2^x)
                diag_chaninfo_store%alloc_multi = multiplier
            end if
        end subroutine nc_diag_chaninfo_allocmulti
        
        ! Load chaninfo variable definitions from an existing, already
        ! open NetCDF file.
        ! 
        ! This will scan the currently open NetCDF file for chaninfo
        ! variables. If any exist, the metadata and last position will
        ! get loaded into the chaninfo variable data buffer.
        ! 
        ! Basically, this scans for the "nchans" dimension. If it
        ! exists, we set our internal nchans to that dimension's value.
        ! Then we fetch the dimension names for all variables, and try
        ! to match them to "nchans". (This is slow... see TODO.txt for
        ! a better idea!)
        ! 
        ! Once we find our chaninfo variable(s), we scan them for NetCDF
        ! fill bytes, starting at the end of the variable. When we find
        ! a spot that does NOT have a fill byte, we set our relative
        ! index at that spot, and set everything up to resume at that
        ! position.
        ! 
        ! For string data, we also our maximum string length constraint
        ! so that we still keep additional variable data within bounds.
        ! 
        ! This is an internal subroutine, and is NOT meant to be called
        ! outside of nc_diag_write. Calling this subroutine in your
        ! program may result in unexpected behavior and/or data
        ! corruption!
        ! 
        ! Args:
        !     None
        !     
        ! Raises:
        !     If the chaninfo variable uses an unsupported type (e.g.
        !     not one of the types listed above), this will result in
        !     an error.
        !     
        !     If there is no file open (or the file is already closed),
        !     this will result in an error. (NetCDF error here, since
        !     init_done is not being checked... see TODO.txt)
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_chaninfo_load_def
            integer(i_long) :: ndims, nvars, var_index, type_index
            integer(i_long) :: rel_index, i, j
            
            ! Temporary variables used when scanning variables and dimensions
            ! from our NetCDF file
            character(len=NF90_MAX_NAME)               :: tmp_var_name
            integer(i_long)                            :: tmp_var_type, tmp_var_ndims
            
            integer(i_long), dimension(:), allocatable :: tmp_var_dimids, tmp_var_dim_sizes
            character(len=NF90_MAX_NAME) , allocatable :: tmp_var_dim_names(:)
            
            ! Is this a nchans var?
            logical                                    :: is_nchans_var
            
            ! Data buffers - we need these to fetch our data and see where
            ! we left off...
            integer(i_byte),    dimension(:), allocatable     :: byte_buffer
            integer(i_short),   dimension(:), allocatable     :: short_buffer
            integer(i_long),    dimension(:), allocatable     :: long_buffer
            
            real(r_single),     dimension(:), allocatable     :: rsingle_buffer
            real(r_double),     dimension(:), allocatable     :: rdouble_buffer
            
            character(1),     dimension(:,:), allocatable     :: string_buffer
            
            ! Dimension checking NetCDF error storage
            integer(i_long)                                   :: dim_nc_err
            
            ! Get top level info about the file!
            call nclayer_check(nf90_inquire(ncid, nDimensions = ndims, &
                nVariables = nvars))
            
            ! Fetch nchans first!
            dim_nc_err = nf90_inq_dimid(ncid, "nchans", diag_chaninfo_store%nchans_dimid)
            
            ! Check if we found anything!
            ! If we got NF90_EBADDIM, then exit.
            if (dim_nc_err == NF90_EBADDIM) then
                return
            else if (dim_nc_err /= NF90_NOERR) then
                ! If an error besides not finding the dimension occurs,
                ! raise an exception.
                call nclayer_check(dim_nc_err)
            end if
            
            ! Then grab nchans value...
            call nclayer_check(nf90_inquire_dimension(ncid, diag_chaninfo_store%nchans_dimid, &
                len = diag_chaninfo_store%nchans))
            
            ! Now search for variables that use nchans!
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
                
                if ((tmp_var_ndims == 1) .OR. &
                    ((tmp_var_ndims == 2) .AND. (tmp_var_type == NF90_CHAR))) then
                    ! Reset our is_nchans_var switch to FALSE!
                    is_nchans_var = .FALSE.
                    
                    ! Fetch all dimension names for the dimensions in the
                    ! variable, and check if the variable is a nchans
                    ! variable. We do so by (slowly) checking all
                    ! dimension names and seeing if they match "nchans".
                    ! If they do, is_nchans_var is set to TRUE.
                    do i = 1, tmp_var_ndims
                        call nclayer_check(nf90_inquire_dimension(ncid, tmp_var_dimids(i), tmp_var_dim_names(i), &
                            tmp_var_dim_sizes(i)))
                        
                        if (tmp_var_dim_names(i) == "nchans") is_nchans_var = .TRUE.
                    end do
                    
                    if (is_nchans_var) then
                        ! Expand variable metadata first!
                        ! Make sure we have enough variable metadata storage
                        ! (and if not, reallocate!)
                        call nc_diag_chaninfo_expand
                        
                        ! Add to the total!
                        diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                        
                        ! Store name and type!
                        diag_chaninfo_store%names(diag_chaninfo_store%total) = trim(tmp_var_name)
                        
                        ! Reset relative index to zero...
                        rel_index = 0
                        
                        ! For the rest of the code, we basically do the following:
                        !  -> We allocate a temporary data storage variable.
                        !  -> We set the NLAYER variable type for the variable.
                        !  -> We fetch all of the data for the variable.
                        !  -> We search, starting at the end of the variable, for
                        !     fill bytes. We keep going if we see filler bytes, and
                        !     stop when we encounter a non-fill byte.
                        !  -> Since the place we stop is where we last stored a value,
                        !     we set our relative index to the stopped index variable.
                        !  -> We deallocate our temporary data storage variable.
                        !  -> We set our type_index to update our data storage array count.
                        
                        if (tmp_var_type == NF90_BYTE) then
                            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_BYTE
                            call nc_diag_chaninfo_resize_byte(int8(diag_chaninfo_store%nchans), .FALSE.)
                            allocate(byte_buffer(diag_chaninfo_store%nchans))
                            call nclayer_check(nf90_get_var(ncid, var_index, byte_buffer))
                            
                            do j = diag_chaninfo_store%nchans, 1, -1
                                if (byte_buffer(j) /= NLAYER_FILL_BYTE) then
                                    exit
                                end if
                            end do
                            
                            rel_index = j
                            
                            deallocate(byte_buffer)
                            
                            type_index = 1
                        else if (tmp_var_type == NF90_SHORT) then
                            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_SHORT
                            call nc_diag_chaninfo_resize_short(int8(diag_chaninfo_store%nchans), .FALSE.)
                            allocate(short_buffer(diag_chaninfo_store%nchans))
                            call nclayer_check(nf90_get_var(ncid, var_index, short_buffer))
                            
                            do j = diag_chaninfo_store%nchans, 1, -1
                                if (short_buffer(j) /= NLAYER_FILL_SHORT) then
                                    exit
                                end if
                            end do
                            
                            rel_index = j
                            
                            deallocate(short_buffer)
                            
                            type_index = 2
                        else if (tmp_var_type == NF90_INT) then
                            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_LONG
                            call nc_diag_chaninfo_resize_long(int8(diag_chaninfo_store%nchans), .FALSE.)
                            allocate(long_buffer(diag_chaninfo_store%nchans))
                            call nclayer_check(nf90_get_var(ncid, var_index, long_buffer))
                            
                            do j = diag_chaninfo_store%nchans, 1, -1
                                if (long_buffer(j) /= NLAYER_FILL_LONG) then
                                    exit
                                end if
                            end do
                            
                            rel_index = j
                            
                            deallocate(long_buffer)
                            
                            type_index = 3
                        else if (tmp_var_type == NF90_FLOAT) then
                            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_FLOAT
                            call nc_diag_chaninfo_resize_rsingle(int8(diag_chaninfo_store%nchans), .FALSE.)
                            allocate(rsingle_buffer(diag_chaninfo_store%nchans))
                            call nclayer_check(nf90_get_var(ncid, var_index, rsingle_buffer))
                            
                            do j = diag_chaninfo_store%nchans, 1, -1
                                if (rsingle_buffer(j) /= NLAYER_FILL_FLOAT) then
                                    exit
                                end if
                            end do
                            
                            rel_index = j
                            
                            deallocate(rsingle_buffer)
                            
                            type_index = 4
                        else if (tmp_var_type == NF90_DOUBLE) then
                            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_DOUBLE
                            call nc_diag_chaninfo_resize_rdouble(int8(diag_chaninfo_store%nchans), .FALSE.)
                            allocate(rdouble_buffer(diag_chaninfo_store%nchans))
                            call nclayer_check(nf90_get_var(ncid, var_index, rdouble_buffer))
                            
                            do j = diag_chaninfo_store%nchans, 1, -1
                                if (rdouble_buffer(j) /= NLAYER_FILL_DOUBLE) then
                                    exit
                                end if
                            end do
                            
                            rel_index = j
                            
                            deallocate(rdouble_buffer)
                            
                            type_index = 5
                        else if (tmp_var_type == NF90_CHAR) then
                            diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_STRING
                            call nc_diag_chaninfo_resize_string(int8(diag_chaninfo_store%nchans), .FALSE.)
                            allocate(string_buffer(diag_chaninfo_store%nchans, tmp_var_dim_sizes(1)))
                            call nclayer_check(nf90_get_var(ncid, var_index, string_buffer))
                            
                            do j = diag_chaninfo_store%nchans, 1, -1
                                if (string_buffer(j, 1) /= NLAYER_FILL_CHAR) then
                                    exit
                                end if
                            end do
                            
                            rel_index = j
                            
                            deallocate(string_buffer)
                            
                            ! Set max string length constraint
                            diag_chaninfo_store%max_str_lens(diag_chaninfo_store%total) = tmp_var_dim_sizes(1)
                            
                            type_index = 6
                        else
                            ! The type is not supported by chaninfo - error!
                            call nclayer_error("NetCDF4 type invalid!")
                        end if
                        
!                       print *, trim(tmp_var_name), "rel index", rel_index
                        
                        ! Now add a relative position... based on the next position!
                        
                        ! First, increment the number of variables stored for this type:
                        diag_chaninfo_store%acount_v(type_index) = diag_chaninfo_store%acount_v(type_index) + 1
                        
                        ! Then, set the next variable's relative positioning,
                        ! based on the number of variables stored for this type.
                        diag_chaninfo_store%var_rel_pos(diag_chaninfo_store%total) = diag_chaninfo_store%acount_v(type_index)
                        
                        ! Initialize the amount of memory used to 0.
                        diag_chaninfo_store%var_usage(diag_chaninfo_store%total) = 0
                        
                        ! Set relative index!
                        diag_chaninfo_store%rel_indexes(diag_chaninfo_store%total) = rel_index
                        
                        ! Set variable ID! Note that var_index here is the actual variable ID.
                        diag_chaninfo_store%var_ids(diag_chaninfo_store%total) = var_index
                    end if
                    
                    !call nc_diag_cat_metadata_add_var(trim(tmp_var_name), tmp_var_type, tmp_var_ndims, tmp_var_dim_names)
                end if
                
                ! Deallocate
                deallocate(tmp_var_dimids)
                deallocate(tmp_var_dim_names)
                deallocate(tmp_var_dim_sizes)
            end do
            
            ! Set our definition lock!
            diag_chaninfo_store%def_lock = .TRUE.
        end subroutine nc_diag_chaninfo_load_def
        
        ! Write out chaninfo variable dimensions and variable
        ! definitions to NetCDF via the NetCDF API.
        ! 
        ! Commit the current variables and make them known to NetCDF to
        ! allow chaninfo variable data writing. If successfully written,
        ! this will always set the definition lock flag to prevent any
        ! further changes.
        ! 
        ! Definitions are only written once for every file opened, and
        ! can not be modified or written again within the opened file.
        ! This is enforced with a definition lock (def_lock) that is
        ! set here and checked everywhere.
        ! 
        ! If definitions are already locked, no additional definitions
        ! will be created. Depending on conditions, the following may
        ! occur:
        ! 
        !   -> If the internal argument is defined and set to TRUE, no
        !      error will be triggered. This is used internally by
        !      nc_diag_write to prevent errors from occuring when the
        !      lock may have already been set elsewhere.
        !      
        !   -> Otherwise, an error will be triggered, since the
        !      definition write occurred when the definitions were
        !      already written and locked.
        ! 
        ! The inner workings:
        ! 
        !   -> First and foremost, it performs sanity checks to ensure
        !      that we have a file loaded. If the check fails, an error
        !      occurs.
        !      
        !   -> It then checks to make sure we have chaninfo variables to
        !      write in the first place. If we don't have any, we simply
        !      return.
        !      
        !   -> We then do another sanity check to ensure that nchans is
        !      defined. We probably shouldn't have any variables in the
        !      first place if nchans isn't defined, but it doesn't hurt
        !      to check! (If this check fails, we probably have a
        !      serious bug...)
        !      
        !   -> If necessary (aka not in append mode, where this might
        !      already exist), define the nchans dimension in NetCDF.
        !      
        !   -> For every variable, fetch the type and name of the
        !      variable. If the variable is a string type, we also
        !      figure out the maximum string length, and create an
        !      extra dimension for that as well. Finally, we can go and
        !      define the variable itself to NetCDF, with the variable's
        !      respective dimensions (and NetCDF dimension IDs).
        !      
        !   -> We then add the variable to the varattr list to allow
        !      variable attributes for the chaninfo variable.
        !      
        !   -> If we're not in append mode, we set the appropriate
        !      chunking and compression settings for the variable to
        !      make storing the data more efficient.
        !      
        !   -> After we've gone through all of the chaninfo variables,
        !      we lock the definitions. That's it!
        ! 
        ! This is an internal subroutine, and is NOT meant to be called
        ! outside of nc_diag_write. Calling this subroutine in your
        ! program may result in unexpected behavior and/or data
        ! corruption!
        ! 
        ! Args:
        !     internal (logical, optional): whether or not to disable
        !         triggering an error when a definition lock is
        !         detected. This flag is used internally for the final
        !         nc_diag_write, where this flag is purposely set to
        !         avoid any errors with definition locking, since the
        !         lock could have already been set earlier by
        !         nc_diag_lock_def or others.
        !     
        ! Raises:
        !     If definitions are already locked, and the internal
        !     argument is not set or is not TRUE, this will result in an
        !     error.
        !     
        !     If the nchans dimension hasn't been defined yet, this will
        !     result in an error.
        !     
        !     If there is no file open (or the file is already closed),
        !     this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_chaninfo_write_def(internal)
            logical, intent(in), optional :: internal
            
            ! Just write the definitions out!
            integer(i_llong)              :: curdatindex
            integer(i_byte)               :: data_type
            integer(i_long)               :: data_type_index
            character(len=100)            :: data_name
            integer(i_long)               :: nc_data_type
            
            integer(i_long)               :: tmp_dim_id
            character(len=120)            :: data_dim_name
            
            character(len=:), allocatable :: string_arr(:)
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                if (present(internal)) then
                    write(action_str, "(A, L, A)") "nc_diag_chaninfo_write_def(internal = ", internal, ")"
                else
                    write(action_str, "(A)") "nc_diag_chaninfo_write_def(internal = (not specified))"
                end if
                call nclayer_actionm(trim(action_str))
            end if
#endif
            ! Ensure that we have a file open and that things are loaded!
            if (init_done .AND. allocated(diag_chaninfo_store)) then
                ! Ensure that we have at least one variable to store!
                ! Otherwise, just return and do nothing.
                if (diag_chaninfo_store%total > 0) then
                    ! Make sure nchans is defined before doing anything!
                    if (diag_chaninfo_store%nchans /= -1) then
                        ! Finally, make sure definitions are not locked!
                        if (.NOT. diag_chaninfo_store%def_lock) then
                            ! First, set the dimensions... if necessary!
                            if (.NOT. append_only) &
                                call nclayer_check(nf90_def_dim(ncid, "nchans", diag_chaninfo_store%nchans, diag_chaninfo_store%nchans_dimid))
                            
                            ! Once we have the dimension, we can start writing
                            ! variable definitions!
                            do curdatindex = 1, diag_chaninfo_store%total
                                ! Fetch variable name and type:
                                data_name = diag_chaninfo_store%names(curdatindex)
                                data_type = diag_chaninfo_store%types(curdatindex)
                                
                                ! Figure out where our data is stored, given var_rel_pos
                                ! and nchans... (see equation/discussion above for more
                                ! details!)
                                data_type_index = 1 + &
                                    ((diag_chaninfo_store%var_rel_pos(curdatindex) - 1) * diag_chaninfo_store%nchans)
                                
                                call nclayer_info("chaninfo: defining " // trim(data_name))
                                
                                ! Map our NLAYER type to the NF90 NetCDF native type!
                                if (data_type == NLAYER_BYTE)   nc_data_type = NF90_BYTE
                                if (data_type == NLAYER_SHORT)  nc_data_type = NF90_SHORT
                                if (data_type == NLAYER_LONG)   nc_data_type = NF90_INT
                                if (data_type == NLAYER_FLOAT)  nc_data_type = NF90_FLOAT
                                if (data_type == NLAYER_DOUBLE) nc_data_type = NF90_DOUBLE
                                if (data_type == NLAYER_STRING) nc_data_type = NF90_CHAR
                                
#ifdef _DEBUG_MEM_
                                print *, "chaninfo part 1"
#endif
                                
                                ! If our variable type is a string, we need to compute the maximum
                                ! string length.
                                ! 
                                ! If we're trimming, we take the maximum of the length of strings
                                ! in the variable, and use that as our maximum string length.
                                ! 
                                ! Otherwise, we simply use the previously defined fixed length,
                                ! which is already stored as the maximum string length from the
                                ! initial string add.
                                ! 
                                ! Once we know our maximum string length, we add that as a
                                ! dimension, and use it (along with our nchans dimension) to
                                ! create our string chaninfo variable!
                                
                                if (data_type == NLAYER_STRING) then
                                    ! Figure out the dimension name for this chaninfo variable
                                    write (data_dim_name, "(A, A)") trim(data_name), "_maxstrlen"
                                    
                                    ! Assume that the maximum string length is 10000
                                    ! Allocate an array of 10000, with a size of the
                                    ! variable's var_usage
                                    allocate(character(10000) :: string_arr(diag_chaninfo_store%var_usage(curdatindex)))
                                    
                                    ! Fetch the strings from our variable storage
                                    string_arr = diag_chaninfo_store%ci_string(data_type_index:(data_type_index + &
                                            diag_chaninfo_store%var_usage(curdatindex) - 1))
                                    
                                    ! If trimming is enabled, we haven't found our max_str_len yet.
                                    ! Go find it!
                                    if (enable_trim) then
                                        ! Save the max string len
                                        diag_chaninfo_store%max_str_lens(curdatindex) = &
                                            max_len_string_array(string_arr, diag_chaninfo_store%var_usage(curdatindex))
                                    end if
                                    
                                    ! Add our custom string dimension to NetCDF, if necessary
                                    if (.NOT. append_only) &
                                        call nclayer_check(nf90_def_dim(ncid, data_dim_name, &
                                            diag_chaninfo_store%max_str_lens(curdatindex), &
                                            tmp_dim_id))
#ifdef _DEBUG_MEM_
                                    print *, "Defining char var type..."
#endif
                                    ! Add our string variable to NetCDF!
                                    if (.NOT. append_only) &
                                        call nclayer_check(nf90_def_var(ncid, diag_chaninfo_store%names(curdatindex), &
                                            nc_data_type, (/ tmp_dim_id, diag_chaninfo_store%nchans_dimid /), &
                                            diag_chaninfo_store%var_ids(curdatindex)))
#ifdef _DEBUG_MEM_
                                    print *, "Done defining char var type..."
#endif
                                    ! Deallocate temp string array
                                    deallocate(string_arr)
                                else
                                    ! Nothing fancy here!
                                    ! Just add our non-string variable to NetCDF!
                                    if (.NOT. append_only) &
                                        call nclayer_check(nf90_def_var(ncid, diag_chaninfo_store%names(curdatindex), &
                                            nc_data_type, diag_chaninfo_store%nchans_dimid, &
                                            diag_chaninfo_store%var_ids(curdatindex)))
                                end if
                                
#ifdef _DEBUG_MEM_
                                print *, "chaninfo part 2"
#endif
                                
                                ! Make our variable known to varattr - add it to the varattr database!
                                call nc_diag_varattr_add_var(diag_chaninfo_store%names(curdatindex), &
                                    diag_chaninfo_store%types(curdatindex), &
                                    diag_chaninfo_store%var_ids(curdatindex))
                                
                                ! If we are not appending, make sure to also set chunking and
                                ! compression for efficiency + optimization!
                                if (.NOT. append_only) then
                                    ! If we're storing a string, we need to specify both dimensions
                                    ! for our chunking parameters. Otherwise, we just need to
                                    ! specify nchans...
                                    if (data_type == NLAYER_STRING) then
                                        call nclayer_check(nf90_def_var_chunking(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                            NF90_CHUNKED, (/ diag_chaninfo_store%max_str_lens(curdatindex), diag_chaninfo_store%nchans /)))
                                    else
                                        call nclayer_check(nf90_def_var_chunking(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                            NF90_CHUNKED, (/ diag_chaninfo_store%nchans /)))
                                    end if
                                    
                                    ! Enable zlib (gzip-like) compression based on our level settings
                                    call nclayer_check(nf90_def_var_deflate(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                        1, 1, int(NLAYER_COMPRESSION)))
                                end if
                            end do
                            
                            ! Lock the definitions!
                            diag_chaninfo_store%def_lock = .TRUE.
                        else
                            ! Show an error message if we didn't suppress errors on purpose
                            if(.NOT. present(internal)) &
                                call nclayer_error("Can't write definitions - definitions have already been written and locked!")
                        end if
                    else
                        call nclayer_error("Can't write definitions - number of chans not set yet!")
                    end if
                    
                    ! End: if (diag_chaninfo_store%total > 0)
                end if
            else
                call nclayer_error("Can't write definitions - NetCDF4 layer not initialized yet!")
            end if
        end subroutine nc_diag_chaninfo_write_def
        
        ! Write all of the currently stored chaninfo data to NetCDF via
        ! the NetCDF APIs ("put").
        ! 
        ! This will go through all of the variables stored in chaninfo,
        ! and write their data to NetCDF.
        ! 
        ! Buffer flushing mode is enabled if flush_data_only is set and
        ! is TRUE. Otherwise, this will operate normally.
        ! 
        ! For buffer flushing mode, data locking will not be performed.
        ! Instead, it "flushes" the variable storage buffer. For all
        ! of the variables stored, it increments the relative index of
        ! the variable with the amount of data currently stored in the
        ! variable.
        ! 
        ! (Essentially, new_rel_index = old_rel_index + var_data_count)
        ! 
        ! Recall that the relative index stores the position of the last
        ! data entered for the variable. This is set by write_data, as
        ! well as load_def for the data append mode. In turn, write_data
        ! also uses it to store at the correct position.
        ! 
        ! We also reset the var_usage, or the variable memory usage
        ! counter, back to zero to allow data storage to start at the
        ! beginning again. We use var_usage in write_data and in the
        ! storage subroutines to keep track of how much data we're
        ! storing, and how much we need to "read" from the array to
        ! store the data in NetCDF4 efficiently and within bounds.
        ! 
        ! A quick example:
        !   -> If we have 2 elements, var_usage (variable memory usage)
        !      is initially 2, and rel_index (variable relative index,
        !      or our starting position) is initially 0.
        !      
        !   -> We flush the buffer. Since we flushed our buffer,
        !      var_usage is reset to 0, and rel_index is now 2 since
        !      we stored 2 elements.
        !      
        !   -> If we add 3 elements, we get a var_usage of 3 (for 3
        !      elements stored), and rel_index stays the same (2).
        !      
        !   -> When we finally flush or write, this time we know to
        !      start at element number 3 (rel_index), and we know to
        !      write 3 elements from there (var_usage).
        !      
        !   -> We now have a total of 5 elements! Indicies 1-2 were
        !      stored with the flush, and indicies 3-5 were stored
        !      afterwards - all thanks to buffer flushing!
        ! 
        ! Finally, if data flushing mode is enabled, the data_lock is
        ! not set to allow additional data to be written in the future.
        ! 
        ! However, if data flushing mode is not set, or it is disabled,
        ! we assume that we are writing only one more time (or once,
        ! depending on if buffer flushing was ever enabled or not).
        ! Therefore, we set the data_lock (data writing lock) to TRUE
        ! in this case, assuming data writing was successful.
        ! 
        ! If data writing has already been locked, this will error.
        ! 
        ! If data flushing mode is disabled, we will also check to see
        ! if each variable's data fills up the nchans dimension.
        ! 
        ! Depending on the strictness (strict_check), if the data is
        ! not filled to the nchans dimension, it could either result in
        ! an error (if strict_check is TRUE), or a warning (if
        ! strict_check is FALSE).
        ! 
        ! This is an internal subroutine, and is NOT meant to be called
        ! outside of nc_diag_write. Calling this subroutine in your
        ! program may result in unexpected behavior and/or data
        ! corruption!
        ! 
        ! Args:
        !     flush_data_only (logical, optional): whether to only flush
        !         the chaninfo data buffers or not. If we flush data,
        !         data locking will not be set.
        ! 
        ! Raises:
        !     If data writing has already been locked, and the data
        !     flushing argument is not set or is not TRUE, this will
        !     result in an error.
        !     
        !     If the nchans dimension hasn't been defined yet, this will
        !     result in an error.
        !     
        !     If strict checking (strict_check) is enabled, and a
        !     variable's data doesn't fill to the nchans dimension,
        !     this will result in an error.
        !     
        !     If there is no file open (or the file is already closed),
        !     this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_chaninfo_write_data(flush_data_only)
            ! Optional internal flag to only flush data - if this is
            ! true, data flushing will be performed, and the data will
            ! NOT be locked.
            logical, intent(in), optional         :: flush_data_only
            
            integer(i_byte)                       :: data_type
            integer(i_long)                       :: data_type_index
            character(len=100)                    :: data_name
            
            character(len=1000)                   :: nchan_empty_msg
            
            integer(i_llong)               :: curdatindex, j
            integer(i_long)                :: string_arr_maxlen
            
            character(len=:), allocatable :: string_arr(:)
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                if (present(flush_data_only)) then
                    write(action_str, "(A, L, A)") "nc_diag_chaninfo_write_data(flush_data_only = ", flush_data_only, ")"
                else
                    write(action_str, "(A)") "nc_diag_chaninfo_write_data(flush_data_only = (not specified))"
                end if
                call nclayer_actionm(trim(action_str))
            end if
#endif
            ! Check to make sure a file is open / things are loaded!
            if (init_done .AND. allocated(diag_chaninfo_store)) then
                ! Check to see if we have any variables to write in the
                ! first place!
                if (diag_chaninfo_store%total > 0) then
                    ! Check to make sure that we have nchans defined!
                    if (diag_chaninfo_store%nchans /= -1) then
                        ! Check if we can still write any data!
                        if (.NOT. diag_chaninfo_store%data_lock) then
                            ! Iterate through all of our variables!
                            do curdatindex = 1, diag_chaninfo_store%total
                                ! Fetch the variable's name and type!
                                data_name = diag_chaninfo_store%names(curdatindex)
                                data_type = diag_chaninfo_store%types(curdatindex)
                                
                                ! Figure out where our data is stored, given var_rel_pos
                                ! and nchans... (see equation/discussion above for more
                                ! details!)
                                data_type_index = 1 + &
                                    ((diag_chaninfo_store%var_rel_pos(curdatindex) - 1) * diag_chaninfo_store%nchans)
                                
                                call nclayer_info("chaninfo: writing " // trim(data_name))
                                
                                ! Warn about low data filling... but only if we are finishing
                                ! our data write (or writing once) - basically, we're NOT in
                                ! flushing data mode!
                                if ((.NOT. (present(flush_data_only) .AND. flush_data_only)) .AND. &
                                    ((diag_chaninfo_store%var_usage(curdatindex) + &
                                        diag_chaninfo_store%rel_indexes(curdatindex)) < diag_chaninfo_store%nchans)) then
                                    ! NOTE - I0 and TRIM are Fortran 95 specs
                                    write (nchan_empty_msg, "(A, A, A, I0, A, I0, A)") "Amount of data written in ", &
                                        trim(data_name), " (", &
                                        diag_chaninfo_store%var_usage(curdatindex) + &
                                            diag_chaninfo_store%rel_indexes(curdatindex), &
                                        ")"  // char(10) // &
                                        "             is less than nchans (", diag_chaninfo_store%nchans, ")!"
                                    
                                    ! If we are set to strict checking mode, error.
                                    ! Otherwise, just show a warning.
                                    if (diag_chaninfo_store%strict_check) then
                                        call nclayer_error(trim(nchan_empty_msg))
                                    else
                                        call nclayer_warning(trim(nchan_empty_msg))
                                    end if
                                end if
                                
#ifdef _DEBUG_MEM_
                                print *, "****** Processing ******"
                                print *, "data_name:"
                                print *, data_name
                                print *, "data_type:"
                                print *, data_type
                                print *, "data_type_index:"
                                print *, data_type_index
                                print *, "diag_chaninfo_store%var_ids(curdatindex):"
                                print *, diag_chaninfo_store%var_ids(curdatindex)
                                print *, "diag_chaninfo_store%var_usage(curdatindex):"
                                print *, diag_chaninfo_store%var_usage(curdatindex)
                                print *, "Upper range (data_type_index + &"
                                print *, "  diag_chaninfo_store%var_usage(curdatindex) - 1):"
                                print *, (data_type_index + &
                                            diag_chaninfo_store%var_usage(curdatindex) - 1)
#endif
                                ! Make sure we have variable data to write in the first place!
                                ! 
                                ! If we do, we essentially:
                                !      
                                !   -> Find the right type to save to.
                                !      
                                !   -> If we are NOT storing a string, we just store a subsection
                                !      of our variable storage array at (1 + rel_index) in the
                                !      NetCDF variable.
                                !      
                                !   -> If we are storing a string, we create our own array to
                                !      store all of our strings in to standardize the length
                                !      (e.g. a 3, 4, and 5 character string is expanded to
                                !      a 5, 5, and 5 character string array). This is needed
                                !      to store all strings at once and match the NetCDF bounds.
                                !      Once done, the array is sent through the NetCDF API for
                                !      data storage. We deallocate the array once we're done!
                                ! 
                                if (diag_chaninfo_store%var_usage(curdatindex) > 0) then
                                    if (data_type == NLAYER_BYTE) then
#ifdef _DEBUG_MEM_
                                        print *, "Resulting data to be stored:"
                                        print *, diag_chaninfo_store%ci_byte(data_type_index:(data_type_index + &
                                                    diag_chaninfo_store%var_usage(curdatindex) - 1))
#endif
                                        call nclayer_check(nf90_put_var(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                            diag_chaninfo_store%ci_byte(data_type_index:(data_type_index + &
                                                diag_chaninfo_store%var_usage(curdatindex) - 1)), &
                                            start = (/ 1 + diag_chaninfo_store%rel_indexes(curdatindex) /) &
                                            ))
                                    else if (data_type == NLAYER_SHORT) then
                                        call nclayer_check(nf90_put_var(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                            diag_chaninfo_store%ci_short(data_type_index:(data_type_index + &
                                                diag_chaninfo_store%var_usage(curdatindex) - 1)), &
                                            start = (/ 1 + diag_chaninfo_store%rel_indexes(curdatindex) /) &
                                            ))
                                    else if (data_type == NLAYER_LONG) then
#ifdef _DEBUG_MEM_
                                        print *, "Resulting data to be stored:"
                                        print *, diag_chaninfo_store%ci_long(data_type_index:(data_type_index + &
                                                    diag_chaninfo_store%var_usage(curdatindex) - 1))
                                        print *, "start index:"
                                        print *, 1 + diag_chaninfo_store%rel_indexes(curdatindex)
#endif
                                        call nclayer_check(nf90_put_var(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                            diag_chaninfo_store%ci_long(data_type_index:(data_type_index + &
                                                diag_chaninfo_store%var_usage(curdatindex) - 1)), &
                                            start = (/ 1 + diag_chaninfo_store%rel_indexes(curdatindex) /) &
                                            ))
                                    else if (data_type == NLAYER_FLOAT) then
                                        call nclayer_check(nf90_put_var(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                            diag_chaninfo_store%ci_rsingle(data_type_index:(data_type_index + &
                                                diag_chaninfo_store%var_usage(curdatindex) - 1)), &
                                            start = (/ 1 + diag_chaninfo_store%rel_indexes(curdatindex) /) &
                                            ))
                                    else if (data_type == NLAYER_DOUBLE) then
                                        call nclayer_check(nf90_put_var(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                            diag_chaninfo_store%ci_rdouble(data_type_index:(data_type_index + &
                                                diag_chaninfo_store%var_usage(curdatindex) - 1)), &
                                            start = (/ 1 + diag_chaninfo_store%rel_indexes(curdatindex) /) &
                                            ))
                                    else if (data_type == NLAYER_STRING) then
                                        ! Storing to another variable may seem silly, but it's necessary
                                        ! to avoid "undefined variable" errors, thanks to the compiler's
                                        ! super optimization insanity...
                                        string_arr_maxlen = diag_chaninfo_store%max_str_lens(curdatindex)
                                        allocate(character(string_arr_maxlen) :: &
                                                string_arr(diag_chaninfo_store%var_usage(curdatindex)))
                                        if (enable_trim) then
                                            do j = data_type_index, data_type_index + &
                                                    diag_chaninfo_store%var_usage(curdatindex) - 1
                                                string_arr(j - data_type_index + 1) = &
                                                    trim(diag_chaninfo_store%ci_string(j))
                                            end do
                                            
#ifdef _DEBUG_MEM_
                                            do j = 1, diag_chaninfo_store%var_usage(curdatindex)
                                                write (*, "(A, A, A)") "String: '", string_arr(j), "'"
                                            end do
                                            
                                            write (*, "(A, I0)") "string_arr_maxlen = ", string_arr_maxlen
                                            write (*, "(A, I0)") "diag_chaninfo_store%var_usage(curdatindex) = ", diag_chaninfo_store%var_usage(curdatindex)
#endif
                                        else
                                            do j = data_type_index, data_type_index + &
                                                    diag_chaninfo_store%var_usage(curdatindex) - 1
                                                string_arr(j - data_type_index + 1) = &
                                                    diag_chaninfo_store%ci_string(j)
                                            end do
                                        end if
                                        
                                        call nclayer_check(nf90_put_var(ncid, diag_chaninfo_store%var_ids(curdatindex), &
                                            string_arr, &
                                            start = (/ 1, 1 + diag_chaninfo_store%rel_indexes(curdatindex) /), &
                                            count = (/ string_arr_maxlen, &
                                                diag_chaninfo_store%var_usage(curdatindex) /) ))
                                        
                                        deallocate(string_arr)
                                    else
                                        call nclayer_error("Critical error - unknown variable type!")
                                    end if
                                    
                                    ! Check for data flushing, and if so, update the relative indexes
                                    ! and set var_usage to 0.
                                    if (present(flush_data_only) .AND. flush_data_only) then
                                        diag_chaninfo_store%rel_indexes(curdatindex) = &
                                            diag_chaninfo_store%rel_indexes(curdatindex) + &
                                            diag_chaninfo_store%var_usage(curdatindex)
                                        diag_chaninfo_store%var_usage(curdatindex) = 0
                                        
#ifdef _DEBUG_MEM_
                                        print *, "diag_chaninfo_store%rel_indexes(curdatindex) is now:"
                                        print *, diag_chaninfo_store%rel_indexes(curdatindex)
#endif
                                    end if
                                end if
                            end do
                            
                            ! If we're flushing data, don't do anything...
                            if (present(flush_data_only) .AND. flush_data_only) then
#ifdef _DEBUG_MEM_
                                print *, "In buffer flush mode!"
#endif
                            else
                                ! Otherwise, lock data writing! Note that we do this,
                                ! even if we have no data!
                                diag_chaninfo_store%data_lock = .TRUE.
#ifdef _DEBUG_MEM_
                                print *, "In data lock mode!"
#endif
                            end if
                        else
                            call nclayer_error("Can't write data - data have already been written and locked!")
                        end if
                    else
                        call nclayer_error("Can't write data - number of chans not set yet!")
                    end if
                end if
            else
                call nclayer_error("Can't write data - NetCDF4 layer not initialized yet!")
            end if
            
        end subroutine nc_diag_chaninfo_write_data
        
        ! Set the strict mode for chaninfo variables.
        ! 
        ! This sets the mode that determines how strict chaninfo's
        ! variable consistency checks will be.
        ! 
        ! During the final data write (nc_diag_chaninfo_write_data,
        ! without the buffering flag), chaninfo will check to see if all
        ! of the variables are filled, e.g. all variables have been
        ! stored up to nchans dimension.
        ! 
        ! If there are any variables that are not completely filled to
        ! the nchans dimension, one of the following may occur:
        ! 
        !   -> If strict mode is enabled, a consistency check error will
        !      occur and the program will exit.
        !      
        !   -> If strict mode is disabled, this will only result in a
        !      consistency check warning. After the warning is
        !      displayed, normal operation will occur, including data
        !      writing. For values that are not in the variable (up to
        !      the nchans dimension), missing values will be placed.
        ! 
        ! By default, strict mode is disabled.
        ! 
        ! Since the strict mode is bound to the chaninfo type, it can
        ! only be set when a file is open and when diag_chaninfo_store
        ! is initialized. (It should be initialized if a file is open!)
        ! 
        ! If there isn't a file open / diag_chaninfo_store isn't
        ! initialized, an error will occur.
        ! 
        ! This is an internal subroutine, and is NOT meant to be called
        ! outside of nc_diag_write. Calling this subroutine in your
        ! program may result in unexpected behavior and/or data
        ! corruption!
        ! 
        ! Args:
        !     enable_strict (logical): boolean indicating whether to
        !         enable strict mode or not. If set to TRUE, strict mode
        !         will be enabled. Otherwise, it will be disabled.
        !     
        ! Raises:
        !     If there is no file open (or the file is already closed),
        !     this will result in an error.
        !     
        !     Although unlikely, other errors may indirectly occur.
        !     They may be general storage errors, or even a bug.
        !     See the called subroutines' documentation for details.
        ! 
        subroutine nc_diag_chaninfo_set_strict(enable_strict)
            logical, intent(in) :: enable_strict
            
            if (init_done .AND. allocated(diag_chaninfo_store)) then
                diag_chaninfo_store%strict_check = enable_strict
            else
                call nclayer_error("Can't set strictness level for chaninfo - NetCDF4 layer not initialized yet!")
            end if
        end subroutine nc_diag_chaninfo_set_strict
        
        ! Preallocate variable metadata storage (names, types, etc.).
        ! 
        ! This preallocates variable metadata storage for a given number
        ! of variables.
        ! 
        ! If properly defined, this can speed up chaninfo variable
        ! creation since reallocation will (hopefully) not be necessary
        ! for variable metadata storage, since it was preallocated here.
        ! 
        ! Variable metadata includes storing the variables' names,
        ! types, indicies, usage counts, etc. The metadata pre-allocated
        ! here is essentially the variable indexed arrays within our
        ! specific storage type!
        ! 
        ! Args:
        !     num_of_addl_vars (integer(i_llong)): the number of
        !         additional variables to preallocate metadata storage
        !         for.
        !     
        ! Raises:
        !     If there is no file open (or the file is already closed),
        !     this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_chaninfo_prealloc_vars(num_of_addl_vars)
            integer(i_llong), intent(in)           :: num_of_addl_vars
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_chaninfo_prealloc_vars(num_of_addl_vars = ", num_of_addl_vars, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            if (init_done .AND. allocated(diag_chaninfo_store)) then
                ! For all variable metadata fields:
                !   -> Check if the field is allocated.
                !   -> If not, allocate it with the default initial
                !      size, plus the number of additional variables
                !      specified in the argument.
                !   -> If it's allocated, check to see if the total
                !      number of variables exceeds our field's allocated
                !      size.
                !   -> If the size is exceeded, reallocate the field
                !      with the number of additional variables specified
                !      in the argument.
                ! 
                if (allocated(diag_chaninfo_store%names)) then
                    if (diag_chaninfo_store%total >= size(diag_chaninfo_store%names)) then
                        call nc_diag_realloc(diag_chaninfo_store%names, num_of_addl_vars)
                    end if
                else
                    allocate(diag_chaninfo_store%names(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                end if
                
                if (allocated(diag_chaninfo_store%types)) then
                    if (diag_chaninfo_store%total >= size(diag_chaninfo_store%types)) then
                        call nc_diag_realloc(diag_chaninfo_store%types, num_of_addl_vars)
                    end if
                else
                    allocate(diag_chaninfo_store%types(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                end if
                
                if (allocated(diag_chaninfo_store%var_rel_pos)) then
                    if (diag_chaninfo_store%total >= size(diag_chaninfo_store%var_rel_pos)) then
                        call nc_diag_realloc(diag_chaninfo_store%var_rel_pos, num_of_addl_vars)
                    end if
                else
                    allocate(diag_chaninfo_store%var_rel_pos(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_chaninfo_store%var_rel_pos = -1
                end if
                
                if (allocated(diag_chaninfo_store%var_usage)) then
                    if (diag_chaninfo_store%total >= size(diag_chaninfo_store%var_usage)) then
                        call nc_diag_realloc(diag_chaninfo_store%var_usage, num_of_addl_vars)
                    end if
                else
                    allocate(diag_chaninfo_store%var_usage(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_chaninfo_store%var_usage = 0
                end if
                
                if (allocated(diag_chaninfo_store%var_ids)) then
                    if (diag_chaninfo_store%total >= size(diag_chaninfo_store%var_ids)) then
                        call nc_diag_realloc(diag_chaninfo_store%var_ids, num_of_addl_vars)
                    end if
                else
                    allocate(diag_chaninfo_store%var_ids(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_chaninfo_store%var_ids = -1
                end if
                
                if (allocated(diag_chaninfo_store%max_str_lens)) then
                    if (diag_chaninfo_store%total >= size(diag_chaninfo_store%max_str_lens)) then
                        call nc_diag_realloc(diag_chaninfo_store%max_str_lens, num_of_addl_vars)
                    end if
                else
                    allocate(diag_chaninfo_store%max_str_lens(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_chaninfo_store%max_str_lens = -1
                end if
                
                if (allocated(diag_chaninfo_store%rel_indexes)) then
                    if (diag_chaninfo_store%total >= size(diag_chaninfo_store%rel_indexes)) then
                        call nc_diag_realloc(diag_chaninfo_store%rel_indexes, num_of_addl_vars)
                    end if
                else
                    allocate(diag_chaninfo_store%rel_indexes(NLAYER_DEFAULT_ENT + num_of_addl_vars))
                    diag_chaninfo_store%rel_indexes = 0
                end if
            else
                call nclayer_error("NetCDF4 layer not initialized yet!")
            endif
        end subroutine nc_diag_chaninfo_prealloc_vars
        
        ! Preallocate actual variable data storage - the data itself.
        ! 
        ! This preallocates the variable data storage for a given
        ! variable type, and a given number of data elements or slots.
        ! 
        ! If properly defined, this can speed up chaninfo variable
        ! data insertion since reallocation will (hopefully) not be
        ! necessary for variable data storage, since it was preallocated
        ! here.
        ! 
        ! For example, if you have 10 float chaninfo variables, and
        ! nchans is 20, you can call:
        ! 
        !   nc_diag_chaninfo_prealloc_vars_storage(NLAYER_FLOAT, 200)
        ! 
        ! Assuming that no other float chaninfo variables get added,
        ! no reallocations should occur, therefore speeding up the
        ! variable data insertion process!
        ! 
        ! Note that this is a state-based subroutine call - by design,
        ! it preallocates the largest amount provided. For instance, if
        ! you attempted to preallocate 10 floats, then 9000 floats, then
        ! 5 floats, 20 floats will be preallocated.
        ! 
        ! Specifically, it looks like this:
        ! 
        !   -> Preallocate 10 floats - nothing allocated, so 10 floats
        !      allocated.
        !      
        !   -> Preallocate 9000 floats - only 10 floats allocated, so
        !      reallocating to 9000.
        !      
        !   -> Preallocate 20 floats - 9000 floats already allocated, so
        !      no need to do anything.
        ! 
        ! Args:
        !     nclayer_type (integer(i_byte)): the type of variable to
        !         preallocate data elements/slots for.
        !     num_of_addl_slots (integer(i_llong)): the number of
        !         additional variable data elements/slots to
        !         preallocate.
        !     
        ! Raises:
        !     If the variable type is invalid, this will result in an
        !     error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_chaninfo_prealloc_vars_storage(nclayer_type, num_of_addl_slots)
            integer(i_byte), intent(in)           :: nclayer_type
            integer(i_llong), intent(in)          :: num_of_addl_slots
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, I0, A, I0, A)") "nc_diag_chaninfo_prealloc_vars_storage(nclayer_type = ", nclayer_type, ", num_of_addl_slots = ", num_of_addl_slots, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            ! Find the type specified, and attempt to pre-allocate.
            ! Note that FALSE is specified as an argument to ensure that
            ! the actual variable data storage usage count isn't
            ! incremented, since we're just preallocating here.
            ! 
            if (nclayer_type == NLAYER_BYTE) then
                call nc_diag_chaninfo_resize_byte(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_SHORT) then
                call nc_diag_chaninfo_resize_short(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_LONG) then
                call nc_diag_chaninfo_resize_long(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_FLOAT) then
                call nc_diag_chaninfo_resize_rsingle(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_DOUBLE) then
                call nc_diag_chaninfo_resize_rdouble(num_of_addl_slots, .FALSE.)
            else if (nclayer_type == NLAYER_STRING) then
                call nc_diag_chaninfo_resize_string(num_of_addl_slots, .FALSE.)
            else
                call nclayer_error("Invalid type specified for variable storage preallocation!")
            end if
        end subroutine nc_diag_chaninfo_prealloc_vars_storage
        
        ! Expand variable metadata storage (names, types, etc.) for one
        ! single variable.
        ! 
        ! This ensures that there is enough variable metadata storage to
        ! add a single variable. If there isn't enough storage, it will
        ! reallocate as necessary. See this module's header for more
        ! information about how memory allocation works for variable
        ! metadata storage.
        ! 
        ! This is an internal subroutine, and is NOT meant to be called
        ! outside of nc_diag_write. Calling this subroutine in your
        ! program may result in unexpected behavior and/or data
        ! corruption!
        ! 
        ! Args:
        !     num_of_addl_vars (integer(i_llong)): the number of
        !         additional variables to preallocate metadata storage
        !         for.
        !     
        ! Raises:
        !     If nchans has not been set yet, this will result in an
        !     error.
        !     
        !     If there is no file open (or the file is already closed),
        !     this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_chaninfo_expand
            integer(i_llong) :: addl_fields
            ! Did we realloc at all?
            logical :: meta_realloc
            meta_realloc = .FALSE.
            
            if (init_done .AND. allocated(diag_chaninfo_store)) then
                addl_fields = 1 + (NLAYER_DEFAULT_ENT * (NLAYER_MULTI_BASE ** diag_chaninfo_store%alloc_multi))
                if (diag_chaninfo_store%nchans /= -1) then
                    
                    ! For all variable metadata fields:
                    !   -> Check if the field is allocated.
                    !   -> If not, allocate it with the default initial
                    !      size, and initialize it with blank values!
                    !   -> If it's allocated, check to see if the total
                    !      number of variables exceeds our field's
                    !      allocated size.
                    !   -> If the size is exceeded, reallocate the
                    !      field, and indicate that a reallocation has
                    !      occurred.
                    ! 
                    if (allocated(diag_chaninfo_store%names)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%names)) then
                            call nc_diag_realloc(diag_chaninfo_store%names, addl_fields)
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%names(NLAYER_DEFAULT_ENT))
                    end if
                    
                    if (allocated(diag_chaninfo_store%types)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%types)) then
                            call nc_diag_realloc(diag_chaninfo_store%types, addl_fields)
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%types(NLAYER_DEFAULT_ENT))
                    end if
                    
                    if (allocated(diag_chaninfo_store%var_rel_pos)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%var_rel_pos)) then
                            call nc_diag_realloc(diag_chaninfo_store%var_rel_pos, addl_fields)
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%var_rel_pos(NLAYER_DEFAULT_ENT))
                        diag_chaninfo_store%var_rel_pos = -1
                    end if
                    
                    if (allocated(diag_chaninfo_store%var_usage)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%var_usage)) then
                            call nc_diag_realloc(diag_chaninfo_store%var_usage, addl_fields)
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%var_usage(NLAYER_DEFAULT_ENT))
                        diag_chaninfo_store%var_usage = 0
                    end if
                    
                    if (allocated(diag_chaninfo_store%var_ids)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%var_ids)) then
                            call nc_diag_realloc(diag_chaninfo_store%var_ids, addl_fields)
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%var_ids(NLAYER_DEFAULT_ENT))
                        diag_chaninfo_store%var_ids = -1
                    end if
                    
                    if (allocated(diag_chaninfo_store%max_str_lens)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%max_str_lens)) then
                            call nc_diag_realloc(diag_chaninfo_store%max_str_lens, addl_fields)
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%max_str_lens(NLAYER_DEFAULT_ENT))
                        diag_chaninfo_store%max_str_lens = -1
                    end if
                    
                    if (allocated(diag_chaninfo_store%rel_indexes)) then
                        if (diag_chaninfo_store%total >= size(diag_chaninfo_store%rel_indexes)) then
                            call nc_diag_realloc(diag_chaninfo_store%rel_indexes, addl_fields)
                            meta_realloc = .TRUE.
                        end if
                    else
                        allocate(diag_chaninfo_store%rel_indexes(NLAYER_DEFAULT_ENT))
                        diag_chaninfo_store%rel_indexes = 0
                    end if
                    
                    ! If reallocation occurred, increment our multiplier
                    ! to allocate more and speed things up in the
                    ! future!
                    if (meta_realloc) then
                        diag_chaninfo_store%alloc_multi = diag_chaninfo_store%alloc_multi + 1
                    end if
                else
                    call nclayer_error("Number of chans not set yet!")
                end if
            else
                call nclayer_error("NetCDF4 layer not initialized yet!")
            end if
        end subroutine nc_diag_chaninfo_expand
        
        ! Add a single scalar byte integer to the given chaninfo
        ! variable.
        ! 
        ! This adds a single value to the specified chaninfo variable.
        ! 
        ! If the variable does not already exist, it will be created,
        ! and the value will be inserted as the variable's first
        ! element.
        ! 
        ! Otherwise, the value will be inserted to the next empty spot.
        ! 
        ! Values are inserted in the order of the calls made. As such,
        ! this subroutine is best designed to be used in a loop, where
        ! for every channel iteration, a value is added using this
        ! subroutine.
        ! 
        ! This is an internal subroutine, and is NOT meant to be called
        ! outside of nc_diag_write. Calling this subroutine in your
        ! program may result in unexpected behavior and/or data
        ! corruption! (You should use the generic nc_diag_chaninfo
        ! instead!)
        ! 
        ! Args:
        !     chaninfo_name (character(len=*)): the chaninfo variable
        !         to store to.
        !     chaninfo_value (integer(i_byte)): the value to store.
        !     
        ! Raises:
        !     If the data has already been locked, this will result in 
        !     an error.
        !     
        !     If definitions have already been locked, and a new
        !     variable is being created, this will result in an error.
        !     
        !     If the variable is already full (e.g. it has nchans number
        !     of elements), this will result in an error.
        !     
        !     The following errors will trigger indirectly from other
        !     subroutines called here:
        !     
        !     If nchans has not been set yet, this will result in an
        !     error.
        !     
        !     If there is no file open (or the file is already closed),
        !     this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_chaninfo_byte(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)    :: chaninfo_name
            integer(i_byte), intent(in)     :: chaninfo_value
            
            integer(i_long) :: i, var_index, var_rel_index, type_index
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_chaninfo_byte(chaninfo_name = " // chaninfo_name // ", chaninfo_value = ", chaninfo_value, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            ! Make sure that data hasn't been locked
            if (diag_chaninfo_store%data_lock) then
                call nclayer_error("Can't add new data - data have already been written and locked!")
            end if
            
            ! For byte, type index is 1
            type_index = 1
            
            ! Default to -1
            var_index = -1
            
            ! Attempt to match the variable name + fetch the variable
            ! index first!
            do i = 1, diag_chaninfo_store%total
                if (diag_chaninfo_store%names(i) == chaninfo_name) then
                    var_rel_index = diag_chaninfo_store%var_rel_pos(i)
                    var_index = i
                    exit
                end if
            end do
            
            if (var_index == -1) then
                ! Entry does not exist yet...
                
                ! First, check to make sure we can still define new variables.
                if (diag_chaninfo_store%def_lock) then
                    call nclayer_error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                ! Expand variable metadata first!
                ! Make sure we have enough variable metadata storage
                ! (and if not, reallocate!)
                call nc_diag_chaninfo_expand
                
                ! Add to the total!
                diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                
                ! Store name and type!
                diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
                diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_BYTE
                
                ! We just need to add one entry...
                ! Call resize subroutine to ensure we have enough space
                ! (and if not, realloc!)
                call nc_diag_chaninfo_resize_byte(int8(diag_chaninfo_store%nchans))
                
                ! Now add a relative position... based on the next position!
                
                ! First, increment the number of variables stored for this type:
                diag_chaninfo_store%acount_v(type_index) = diag_chaninfo_store%acount_v(type_index) + 1
                
                ! Then, set the next variable's relative positioning,
                ! based on the number of variables stored for this type.
                diag_chaninfo_store%var_rel_pos(diag_chaninfo_store%total) = diag_chaninfo_store%acount_v(type_index)
                
                ! Initialize the amount of memory used to 1.
                diag_chaninfo_store%var_usage(diag_chaninfo_store%total) = 1
                
                ! Set var_index to the total
                var_index = diag_chaninfo_store%total
            else
                ! Variable already exists!
                
                ! Check to make sure we can fit more data!
                ! (# data < nchans)
                if (diag_chaninfo_store%var_usage(var_index) + &
                    diag_chaninfo_store%rel_indexes(var_index) >= diag_chaninfo_store%nchans) then
                    call nclayer_error("Can't add new data - data added is exceeding nchan! Data must fit within nchan constraint.")
                endif
                
                ! Increment current variable count
                diag_chaninfo_store%var_usage(var_index) = &
                    diag_chaninfo_store%var_usage(var_index) + 1
            end if
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_byte(1 + &
                    ((diag_chaninfo_store%var_rel_pos(var_index) - 1) &
                        * diag_chaninfo_store%nchans) &
                    + (diag_chaninfo_store%var_usage(var_index) - 1)) = chaninfo_value
        end subroutine nc_diag_chaninfo_byte
        
        ! Add a single scalar short integer to the given chaninfo
        ! variable.
        ! 
        ! This adds a single value to the specified chaninfo variable.
        ! 
        ! If the variable does not already exist, it will be created,
        ! and the value will be inserted as the variable's first
        ! element.
        ! 
        ! Otherwise, the value will be inserted to the next empty spot.
        ! 
        ! Values are inserted in the order of the calls made. As such,
        ! this subroutine is best designed to be used in a loop, where
        ! for every channel iteration, a value is added using this
        ! subroutine.
        ! 
        ! This is an internal subroutine, and is NOT meant to be called
        ! outside of nc_diag_write. Calling this subroutine in your
        ! program may result in unexpected behavior and/or data
        ! corruption! (You should use the generic nc_diag_chaninfo
        ! instead!)
        ! 
        ! Args:
        !     chaninfo_name (character(len=*)): the chaninfo variable
        !         to store to.
        !     chaninfo_value (integer(i_short)): the value to store.
        !     
        ! Raises:
        !     If the data has already been locked, this will result in 
        !     an error.
        !     
        !     If definitions have already been locked, and a new
        !     variable is being created, this will result in an error.
        !     
        !     If the variable is already full (e.g. it has nchans number
        !     of elements), this will result in an error.
        !     
        !     The following errors will trigger indirectly from other
        !     subroutines called here:
        !     
        !     If nchans has not been set yet, this will result in an
        !     error.
        !     
        !     If there is no file open (or the file is already closed),
        !     this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_chaninfo_short(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)    :: chaninfo_name
            integer(i_short), intent(in)    :: chaninfo_value
            
            integer(i_long) :: i, var_index, var_rel_index, type_index
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_chaninfo_short(chaninfo_name = " // chaninfo_name // ", chaninfo_value = ", chaninfo_value, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            ! Make sure that data hasn't been locked
            if (diag_chaninfo_store%data_lock) then
                call nclayer_error("Can't add new data - data have already been written and locked!")
            end if
            
            ! For short, type index is 2
            type_index = 2
            
            ! Default to -1
            var_index = -1
            
            ! Attempt to match the variable name + fetch the variable
            ! index first!
            do i = 1, diag_chaninfo_store%total
                if (diag_chaninfo_store%names(i) == chaninfo_name) then
                    var_rel_index = diag_chaninfo_store%var_rel_pos(i)
                    var_index = i
                    exit
                end if
            end do
            
            if (var_index == -1) then
                ! Entry does not exist yet...
                
                ! First, check to make sure we can still define new variables.
                if (diag_chaninfo_store%def_lock) then
                    call nclayer_error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                ! Expand variable metadata first!
                ! Make sure we have enough variable metadata storage
                ! (and if not, reallocate!)
                call nc_diag_chaninfo_expand
                
                ! Add to the total!
                diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                
                ! Store name and type!
                diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
                diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_SHORT
                
                ! We just need to add one entry...
                ! Call resize subroutine to ensure we have enough space
                ! (and if not, realloc!)
                call nc_diag_chaninfo_resize_short(int8(diag_chaninfo_store%nchans))
                
                ! Now add a relative position... based on the next position!
                
                ! First, increment the number of variables stored for this type:
                diag_chaninfo_store%acount_v(type_index) = diag_chaninfo_store%acount_v(type_index) + 1
                
                ! Then, set the next variable's relative positioning,
                ! based on the number of variables stored for this type.
                diag_chaninfo_store%var_rel_pos(diag_chaninfo_store%total) = diag_chaninfo_store%acount_v(type_index)
                
                ! Initialize the amount of memory used to 1.
                diag_chaninfo_store%var_usage(diag_chaninfo_store%total) = 1
                
                ! Set var_index to the total
                var_index = diag_chaninfo_store%total
            else
                ! Variable already exists!
                
                ! Check to make sure we can fit more data!
                ! (# data < nchans)
                if (diag_chaninfo_store%var_usage(var_index) + &
                    diag_chaninfo_store%rel_indexes(var_index) >= diag_chaninfo_store%nchans) then
                    call nclayer_error("Can't add new data - data added is exceeding nchan! Data must fit within nchan constraint.")
                endif
                
                ! Increment current variable count
                diag_chaninfo_store%var_usage(var_index) = &
                    diag_chaninfo_store%var_usage(var_index) + 1
            end if
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_short(1 + &
                    ((diag_chaninfo_store%var_rel_pos(var_index) - 1) &
                        * diag_chaninfo_store%nchans) &
                    + (diag_chaninfo_store%var_usage(var_index) - 1)) = chaninfo_value
        end subroutine nc_diag_chaninfo_short
        
        ! Add a single scalar long integer to the given chaninfo
        ! variable. (This is NOT a NetCDF "long", just a NetCDF "int".)
        ! 
        ! This adds a single value to the specified chaninfo variable.
        ! 
        ! If the variable does not already exist, it will be created,
        ! and the value will be inserted as the variable's first
        ! element.
        ! 
        ! Otherwise, the value will be inserted to the next empty spot.
        ! 
        ! Values are inserted in the order of the calls made. As such,
        ! this subroutine is best designed to be used in a loop, where
        ! for every channel iteration, a value is added using this
        ! subroutine.
        ! 
        ! This is an internal subroutine, and is NOT meant to be called
        ! outside of nc_diag_write. Calling this subroutine in your
        ! program may result in unexpected behavior and/or data
        ! corruption! (You should use the generic nc_diag_chaninfo
        ! instead!)
        ! 
        ! Args:
        !     chaninfo_name (character(len=*)): the chaninfo variable
        !         to store to.
        !     chaninfo_value (integer(i_long)): the value to store.
        !     
        ! Raises:
        !     If the data has already been locked, this will result in 
        !     an error.
        !     
        !     If definitions have already been locked, and a new
        !     variable is being created, this will result in an error.
        !     
        !     If the variable is already full (e.g. it has nchans number
        !     of elements), this will result in an error.
        !     
        !     The following errors will trigger indirectly from other
        !     subroutines called here:
        !     
        !     If nchans has not been set yet, this will result in an
        !     error.
        !     
        !     If there is no file open (or the file is already closed),
        !     this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_chaninfo_long(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)    :: chaninfo_name
            integer(i_long), intent(in)     :: chaninfo_value
            
            integer(i_long) :: i, var_index, var_rel_index, type_index
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, I0, A)") "nc_diag_chaninfo_long(chaninfo_name = " // chaninfo_name // ", chaninfo_value = ", chaninfo_value, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            ! Make sure that data hasn't been locked
            if (diag_chaninfo_store%data_lock) then
                call nclayer_error("Can't add new data - data have already been written and locked!")
            end if
            
            ! For long, type index is 3
            type_index = 3
            
            ! Default to -1
            var_index = -1
            
            ! Attempt to match the variable name + fetch the variable
            ! index first!
            do i = 1, diag_chaninfo_store%total
                if (diag_chaninfo_store%names(i) == chaninfo_name) then
                    var_rel_index = diag_chaninfo_store%var_rel_pos(i)
                    var_index = i
                    exit
                end if
            end do
            
#ifdef _DEBUG_MEM_
            print *, " *** chaninfo_name"
            print *, chaninfo_name
            print *, " *** var_index is set to:"
            print *, var_index
#endif
            
            if (var_index == -1) then
                ! Entry does not exist yet...
                
                ! First, check to make sure we can still define new variables.
                if (diag_chaninfo_store%def_lock) then
                    call nclayer_error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                ! Expand variable metadata first!
                ! Make sure we have enough variable metadata storage
                ! (and if not, reallocate!)
                call nc_diag_chaninfo_expand
                
                ! Add to the total!
                diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                
                ! Store name and type!
                diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
                diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_LONG
                
                ! We just need to add one entry...
                ! Call resize subroutine to ensure we have enough space
                ! (and if not, realloc!)
                call nc_diag_chaninfo_resize_long(int8(diag_chaninfo_store%nchans))
                
                ! Now add a relative position... based on the next position!
                
                ! First, increment the number of variables stored for this type:
                diag_chaninfo_store%acount_v(type_index) = diag_chaninfo_store%acount_v(type_index) + 1
                
                ! Then, set the next variable's relative positioning,
                ! based on the number of variables stored for this type.
                diag_chaninfo_store%var_rel_pos(diag_chaninfo_store%total) = diag_chaninfo_store%acount_v(type_index)
                
                ! Initialize the amount of memory used to 1.
                diag_chaninfo_store%var_usage(diag_chaninfo_store%total) = 1
                
                ! Set var_index to the total
                var_index = diag_chaninfo_store%total
            else
                ! Variable already exists!
                
                ! Check to make sure we can fit more data!
                ! (# data < nchans)
                if (diag_chaninfo_store%var_usage(var_index) + &
                    diag_chaninfo_store%rel_indexes(var_index) >= diag_chaninfo_store%nchans) then
#ifdef _DEBUG_MEM_
                    print *, "!!!! diag_chaninfo_store%var_usage(var_index)"
                    print *, diag_chaninfo_store%var_usage(var_index)
#endif
                    call nclayer_error("Can't add new data - data added is exceeding nchan! Data must fit within nchan constraint.")
                endif
                
                ! Increment current variable count
                diag_chaninfo_store%var_usage(var_index) = &
                    diag_chaninfo_store%var_usage(var_index) + 1
            end if
            
            ! Now add the actual entry!
#ifdef _DEBUG_MEM_
            print *, "===================================="
            print *, "diag_chaninfo_store%total"
            print *, diag_chaninfo_store%total
            print *, "var_index"
            print *, var_index
            print *, "diag_chaninfo_store%var_rel_pos(var_index)"
            print *, diag_chaninfo_store%var_rel_pos(var_index)
            print *, "diag_chaninfo_store%nchans"
            print *, diag_chaninfo_store%nchans
            print *, "diag_chaninfo_store%var_usage(var_index)"
            print *, diag_chaninfo_store%var_usage(var_index)
            print *, "===================================="
#endif
            
            diag_chaninfo_store%ci_long(1 + &
                    ((diag_chaninfo_store%var_rel_pos(var_index) - 1) &
                        * diag_chaninfo_store%nchans) &
                    + (diag_chaninfo_store%var_usage(var_index) - 1)) = chaninfo_value
        end subroutine nc_diag_chaninfo_long
        
        ! Add a single scalar float to the given chaninfo variable.
        ! 
        ! This adds a single value to the specified chaninfo variable.
        ! 
        ! If the variable does not already exist, it will be created,
        ! and the value will be inserted as the variable's first
        ! element.
        ! 
        ! Otherwise, the value will be inserted to the next empty spot.
        ! 
        ! Values are inserted in the order of the calls made. As such,
        ! this subroutine is best designed to be used in a loop, where
        ! for every channel iteration, a value is added using this
        ! subroutine.
        ! 
        ! This is an internal subroutine, and is NOT meant to be called
        ! outside of nc_diag_write. Calling this subroutine in your
        ! program may result in unexpected behavior and/or data
        ! corruption! (You should use the generic nc_diag_chaninfo
        ! instead!)
        ! 
        ! Args:
        !     chaninfo_name (character(len=*)): the chaninfo variable
        !         to store to.
        !     chaninfo_value (real(r_single)): the value to store.
        !     
        ! Raises:
        !     If the data has already been locked, this will result in 
        !     an error.
        !     
        !     If definitions have already been locked, and a new
        !     variable is being created, this will result in an error.
        !     
        !     If the variable is already full (e.g. it has nchans number
        !     of elements), this will result in an error.
        !     
        !     The following errors will trigger indirectly from other
        !     subroutines called here:
        !     
        !     If nchans has not been set yet, this will result in an
        !     error.
        !     
        !     If there is no file open (or the file is already closed),
        !     this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_chaninfo_rsingle(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)    :: chaninfo_name
            real(r_single), intent(in)      :: chaninfo_value
            
            integer(i_long) :: i, var_index, var_rel_index, type_index
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, F0.5, A)") "nc_diag_chaninfo_rsingle(chaninfo_name = " // chaninfo_name // ", chaninfo_value = ", chaninfo_value, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            ! Make sure that data hasn't been locked
            if (diag_chaninfo_store%data_lock) then
                call nclayer_error("Can't add new data - data have already been written and locked!")
            end if
            
            ! For rsingle, type index is 4
            type_index = 4
            
            ! Default to -1
            var_index = -1
            
            ! Attempt to match the variable name + fetch the variable
            ! index first!
            do i = 1, diag_chaninfo_store%total
                if (diag_chaninfo_store%names(i) == chaninfo_name) then
                    var_rel_index = diag_chaninfo_store%var_rel_pos(i)
                    var_index = i
                    exit
                end if
            end do
            
#ifdef _DEBUG_MEM_
            print *, " *** chaninfo_name"
            print *, chaninfo_name
            print *, " *** var_index is set to:"
            print *, var_index
#endif
            
            if (var_index == -1) then
                ! Entry does not exist yet...
                
                ! First, check to make sure we can still define new variables.
                if (diag_chaninfo_store%def_lock) then
                    call nclayer_error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                ! Expand variable metadata first!
                ! Make sure we have enough variable metadata storage
                ! (and if not, reallocate!)
                call nc_diag_chaninfo_expand
                
                ! Add to the total!
                diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                
                ! Store name and type!
                diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
                diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_FLOAT
                
                ! We just need to add one entry...
                ! Call resize subroutine to ensure we have enough space
                ! (and if not, realloc!)
                call nc_diag_chaninfo_resize_rsingle(int8(diag_chaninfo_store%nchans))
                
                ! Now add a relative position... based on the next position!
                
                ! First, increment the number of variables stored for this type:
                diag_chaninfo_store%acount_v(type_index) = diag_chaninfo_store%acount_v(type_index) + 1
                
                ! Then, set the next variable's relative positioning,
                ! based on the number of variables stored for this type.
                diag_chaninfo_store%var_rel_pos(diag_chaninfo_store%total) = diag_chaninfo_store%acount_v(type_index)
                
                ! Initialize the amount of memory used to 1.
                diag_chaninfo_store%var_usage(diag_chaninfo_store%total) = 1
                
                ! Set var_index to the total
                var_index = diag_chaninfo_store%total
            else
                ! Variable already exists!
                
                ! Check to make sure we can fit more data!
                ! (# data < nchans)
                if (diag_chaninfo_store%var_usage(var_index) + &
                    diag_chaninfo_store%rel_indexes(var_index) >= diag_chaninfo_store%nchans) then
                    call nclayer_error("Can't add new data - data added is exceeding nchan! Data must fit within nchan constraint.")
                endif
                
                ! Increment current variable count
                diag_chaninfo_store%var_usage(var_index) = &
                    diag_chaninfo_store%var_usage(var_index) + 1
            end if
            
#ifdef _DEBUG_MEM_
            print *, "===================================="
            print *, "diag_chaninfo_store%total"
            print *, diag_chaninfo_store%total
            print *, "var_index"
            print *, var_index
            print *, "diag_chaninfo_store%var_rel_pos(var_index)"
            print *, diag_chaninfo_store%var_rel_pos(var_index)
            print *, "diag_chaninfo_store%nchans"
            print *, diag_chaninfo_store%nchans
            print *, "diag_chaninfo_store%var_usage(var_index)"
            print *, diag_chaninfo_store%var_usage(var_index)
            print *, "===================================="
#endif
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_rsingle(1 + &
                    ((diag_chaninfo_store%var_rel_pos(var_index) - 1) &
                        * diag_chaninfo_store%nchans) &
                    + (diag_chaninfo_store%var_usage(var_index) - 1)) = chaninfo_value
        end subroutine nc_diag_chaninfo_rsingle
        
        ! Add a single scalar double to the given chaninfo variable.
        ! 
        ! This adds a single value to the specified chaninfo variable.
        ! 
        ! If the variable does not already exist, it will be created,
        ! and the value will be inserted as the variable's first
        ! element.
        ! 
        ! Otherwise, the value will be inserted to the next empty spot.
        ! 
        ! Values are inserted in the order of the calls made. As such,
        ! this subroutine is best designed to be used in a loop, where
        ! for every channel iteration, a value is added using this
        ! subroutine.
        ! 
        ! This is an internal subroutine, and is NOT meant to be called
        ! outside of nc_diag_write. Calling this subroutine in your
        ! program may result in unexpected behavior and/or data
        ! corruption! (You should use the generic nc_diag_chaninfo
        ! instead!)
        ! 
        ! Args:
        !     chaninfo_name (character(len=*)): the chaninfo variable
        !         to store to.
        !     chaninfo_value (real(r_double)): the value to store.
        !     
        ! Raises:
        !     If the data has already been locked, this will result in 
        !     an error.
        !     
        !     If definitions have already been locked, and a new
        !     variable is being created, this will result in an error.
        !     
        !     If the variable is already full (e.g. it has nchans number
        !     of elements), this will result in an error.
        !     
        !     The following errors will trigger indirectly from other
        !     subroutines called here:
        !     
        !     If nchans has not been set yet, this will result in an
        !     error.
        !     
        !     If there is no file open (or the file is already closed),
        !     this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_chaninfo_rdouble(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)    :: chaninfo_name
            real(r_double), intent(in)      :: chaninfo_value
            
            integer(i_long) :: i, var_index, var_rel_index, type_index
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A, F0.5, A)") "nc_diag_chaninfo_rdouble(chaninfo_name = " // chaninfo_name // ", chaninfo_value = ", chaninfo_value, ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            ! Make sure that data hasn't been locked
            if (diag_chaninfo_store%data_lock) then
                call nclayer_error("Can't add new data - data have already been written and locked!")
            end if
            
            ! For rdouble, type index is 5
            type_index = 5
            
            ! Default to -1
            var_index = -1
            
            ! Attempt to match the variable name + fetch the variable
            ! index first!
            do i = 1, diag_chaninfo_store%total
                if (diag_chaninfo_store%names(i) == chaninfo_name) then
                    var_rel_index = diag_chaninfo_store%var_rel_pos(i)
                    var_index = i
                    exit
                end if
            end do
            
            if (var_index == -1) then
                ! Entry does not exist yet...
                
                ! First, check to make sure we can still define new variables.
                if (diag_chaninfo_store%def_lock) then
                    call nclayer_error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                ! Expand variable metadata first!
                ! Make sure we have enough variable metadata storage
                ! (and if not, reallocate!)
                call nc_diag_chaninfo_expand
                
                ! Add to the total!
                diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                
                ! Store name and type!
                diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
                diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_DOUBLE
                
                ! We just need to add one entry...
                ! Call resize subroutine to ensure we have enough space
                ! (and if not, realloc!)
                call nc_diag_chaninfo_resize_rdouble(int8(diag_chaninfo_store%nchans))
                
                ! Now add a relative position... based on the next position!
                
                ! First, increment the number of variables stored for this type:
                diag_chaninfo_store%acount_v(type_index) = diag_chaninfo_store%acount_v(type_index) + 1
                
                ! Then, set the next variable's relative positioning,
                ! based on the number of variables stored for this type.
                diag_chaninfo_store%var_rel_pos(diag_chaninfo_store%total) = diag_chaninfo_store%acount_v(type_index)
                
                ! Initialize the amount of memory used to 1.
                diag_chaninfo_store%var_usage(diag_chaninfo_store%total) = 1
                
                ! Set var_index to the total
                var_index = diag_chaninfo_store%total
            else
                ! Variable already exists!
                
                ! Check to make sure we can fit more data!
                ! (# data < nchans)
                if (diag_chaninfo_store%var_usage(var_index) + &
                    diag_chaninfo_store%rel_indexes(var_index) >= diag_chaninfo_store%nchans) then
                    call nclayer_error("Can't add new data - data added is exceeding nchan! Data must fit within nchan constraint.")
                endif
                
                ! Increment current variable count
                diag_chaninfo_store%var_usage(var_index) = &
                    diag_chaninfo_store%var_usage(var_index) + 1
            end if
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_rdouble(1 + &
                    ((diag_chaninfo_store%var_rel_pos(var_index) - 1) &
                        * diag_chaninfo_store%nchans) &
                    + (diag_chaninfo_store%var_usage(var_index) - 1)) = chaninfo_value
        end subroutine nc_diag_chaninfo_rdouble

        ! Add a single scalar string to the given chaninfo variable.
        ! (This uses the NetCDF char type, stored internally as a 2D
        ! array of characters.)
        ! 
        ! This adds a single value to the specified chaninfo variable.
        ! 
        ! If the variable does not already exist, it will be created,
        ! and the value will be inserted as the variable's first
        ! element.
        ! 
        ! Otherwise, the value will be inserted to the next empty spot.
        ! 
        ! Values are inserted in the order of the calls made. As such,
        ! this subroutine is best designed to be used in a loop, where
        ! for every channel iteration, a value is added using this
        ! subroutine.
        ! 
        ! This is an internal subroutine, and is NOT meant to be called
        ! outside of nc_diag_write. Calling this subroutine in your
        ! program may result in unexpected behavior and/or data
        ! corruption! (You should use the generic nc_diag_chaninfo
        ! instead!)
        ! 
        ! Args:
        !     chaninfo_name (character(len=*)): the chaninfo variable
        !         to store to.
        !     chaninfo_value (character(len=*)): the value to store.
        !     
        ! Raises:
        !     If the data has already been locked, this will result in 
        !     an error.
        !     
        !     If definitions have already been locked, and a new
        !     variable is being created, this will result in an error.
        !     
        !     If the variable is already full (e.g. it has nchans number
        !     of elements), this will result in an error.
        !     
        !     The following errors will trigger indirectly from other
        !     subroutines called here:
        !     
        !     If nchans has not been set yet, this will result in an
        !     error.
        !     
        !     If there is no file open (or the file is already closed),
        !     this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_chaninfo_string(chaninfo_name, chaninfo_value)
            character(len=*), intent(in)    :: chaninfo_name
            character(len=*), intent(in)    :: chaninfo_value
            
            integer(i_long) :: i, var_index, var_rel_index, type_index
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                write(action_str, "(A)") "nc_diag_chaninfo_string(chaninfo_name = " // chaninfo_name // ", chaninfo_value = " // trim(chaninfo_value) // ")"
                call nclayer_actionm(trim(action_str))
            end if
#endif
            ! Make sure that data hasn't been locked
            if (diag_chaninfo_store%data_lock) then
                call nclayer_error("Can't add new data - data have already been written and locked!")
            end if
            
            ! For string, type index is 6
            type_index = 6
            
            ! Default to -1
            var_index = -1
            
            ! Attempt to match the variable name + fetch the variable
            ! index first!
            do i = 1, diag_chaninfo_store%total
                if (diag_chaninfo_store%names(i) == chaninfo_name) then
                    var_rel_index = diag_chaninfo_store%var_rel_pos(i)
                    var_index = i
                    exit
                end if
            end do
            
            if (var_index == -1) then
                ! Entry does not exist yet...
                
                ! First, check to make sure we can still define new variables.
                if (diag_chaninfo_store%def_lock) then
                    call nclayer_error("Can't add new variable - definitions have already been written and locked!")
                end if
                
                ! Expand variable metadata first!
                ! Make sure we have enough variable metadata storage
                ! (and if not, reallocate!)
                call nc_diag_chaninfo_expand
                
                ! Add to the total!
                diag_chaninfo_store%total = diag_chaninfo_store%total + 1
                
                ! Store name and type!
                diag_chaninfo_store%names(diag_chaninfo_store%total) = chaninfo_name
                diag_chaninfo_store%types(diag_chaninfo_store%total) = NLAYER_STRING
                
                ! We just need to add one entry...
                ! Call resize subroutine to ensure we have enough space
                ! (and if not, realloc!)
                call nc_diag_chaninfo_resize_string(int8(diag_chaninfo_store%nchans))
                
                ! Now add a relative position... based on the next position!
                
                ! First, increment the number of variables stored for this type:
                diag_chaninfo_store%acount_v(type_index) = diag_chaninfo_store%acount_v(type_index) + 1
                
                ! Then, set the next variable's relative positioning,
                ! based on the number of variables stored for this type.
                diag_chaninfo_store%var_rel_pos(diag_chaninfo_store%total) = diag_chaninfo_store%acount_v(type_index)
                
                ! Initialize the amount of memory used to 1.
                diag_chaninfo_store%var_usage(diag_chaninfo_store%total) = 1
                
                ! Set var_index to the total
                var_index = diag_chaninfo_store%total
            else
                ! Variable already exists!
                
                ! Check to make sure we can fit more data!
                ! (# data < nchans)
                if (diag_chaninfo_store%var_usage(var_index) + &
                    diag_chaninfo_store%rel_indexes(var_index) >= diag_chaninfo_store%nchans) then
                    call nclayer_error("Can't add new data - data added is exceeding nchan! Data must fit within nchan constraint.")
                endif
                
                ! Check max string length
                if ((diag_chaninfo_store%def_lock) .AND. &
                    (len_trim(chaninfo_value) > diag_chaninfo_store%max_str_lens(var_index))) &
                    call nclayer_error("Cannot expand variable string length after locking variable definitions!")
                
                ! Increment current variable count
                diag_chaninfo_store%var_usage(var_index) = &
                    diag_chaninfo_store%var_usage(var_index) + 1
            end if
            
            ! If trim isn't enabled, set our maximum string length here!
            if (.NOT. enable_trim) then
                if (diag_chaninfo_store%max_str_lens(var_index) == -1) then
                    diag_chaninfo_store%max_str_lens(var_index) = len(chaninfo_value)
                else
                    ! Validate that our non-first value isn't different from
                    ! the initial string length
                    if (diag_chaninfo_store%max_str_lens(var_index) /= len(chaninfo_value)) &
                        call nclayer_error("Cannot change string size when trimming is disabled!")
                end if
            end if
            
            ! Now add the actual entry!
            diag_chaninfo_store%ci_string(1 + &
                    ((diag_chaninfo_store%var_rel_pos(var_index) - 1) &
                        * diag_chaninfo_store%nchans) &
                    + (diag_chaninfo_store%var_usage(var_index) - 1)) = chaninfo_value
        end subroutine nc_diag_chaninfo_string
end module ncdw_chaninfo
